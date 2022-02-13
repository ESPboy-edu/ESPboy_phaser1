#include <Wire.h>
#include <ESP8266WiFi.h>
#include <Adafruit_MCP23017.h>

Adafruit_MCP23017 mcp;

#define MCP23017address 0 // actually it's 0x20 but in <Adafruit_MCP23017.h> lib there is (x|0x20) :)
#define MCP4725dacresolution 8
#define MCP4725address 0

#define SOUNDPIN       D3

#define SAMPLE_RATE           (3500000/146*2)   //original Phaser1 runs at 23972 but uses sample interleaving, doubling the output rate
#define MUSIC_FRAME           2048              //now many samples in one tempo unit
#define TIMER_RATE            80000000/SAMPLE_RATE

//include music data generated from the binary Beepola output (without player) and bin2h.py
#include "music_data.h"

//include sample data
#include "drum_sample_data.h"



//general settings
#define COMPILE_ADR           40000             //address for compiled Beepola module, default is 40000

//note table, stored in the PROGMEM
//calculated at compile time depending from the sample rate, to make experimenting with it easier
//the magic numbers is the note frequencies of the 8th octave multiplied by 65536

#define NOTE_C(octave)  ((274334351/SAMPLE_RATE)>>(6-octave))
#define NOTEhC(octave)  ((290646917/SAMPLE_RATE)>>(6-octave))
#define NOTE_D(octave)  ((307929415/SAMPLE_RATE)>>(6-octave))
#define NOTEhD(octave)  ((326240174/SAMPLE_RATE)>>(6-octave))
#define NOTE_E(octave)  ((345639485/SAMPLE_RATE)>>(6-octave))
#define NOTE_F(octave)  ((366192230/SAMPLE_RATE)>>(6-octave))
#define NOTEhF(octave)  ((387967221/SAMPLE_RATE)>>(6-octave))
#define NOTE_G(octave)  ((411037204/SAMPLE_RATE)>>(6-octave))
#define NOTEhG(octave)  ((435478855/SAMPLE_RATE)>>(6-octave))
#define NOTE_A(octave)  ((461373440/SAMPLE_RATE)>>(6-octave))
#define NOTEhA(octave)  ((488808120/SAMPLE_RATE)>>(6-octave))
#define NOTE_B(octave)  ((517873991/SAMPLE_RATE)>>(6-octave))

const PROGMEM uint16_t note_table[5 * 12] = {
  NOTE_C(1), NOTEhC(1), NOTE_D(1), NOTEhD(1), NOTE_E(1), NOTE_F(1), NOTEhF(1), NOTE_G(1), NOTEhG(1), NOTE_A(1), NOTEhA(1), NOTE_B(1),
  NOTE_C(2), NOTEhC(2), NOTE_D(2), NOTEhD(2), NOTE_E(2), NOTE_F(2), NOTEhF(2), NOTE_G(2), NOTEhG(2), NOTE_A(2), NOTEhA(2), NOTE_B(2),
  NOTE_C(3), NOTEhC(3), NOTE_D(3), NOTEhD(3), NOTE_E(3), NOTE_F(3), NOTEhF(3), NOTE_G(3), NOTEhG(3), NOTE_A(3), NOTEhA(3), NOTE_B(3),
  NOTE_C(4), NOTEhC(4), NOTE_D(4), NOTEhD(4), NOTE_E(4), NOTE_F(4), NOTEhF(4), NOTE_G(4), NOTEhG(4), NOTE_A(4), NOTEhA(4), NOTE_B(4),
  NOTE_C(5), NOTEhC(5), NOTE_D(5), NOTEhD(5), NOTE_E(5), NOTE_F(5), NOTEhF(5), NOTE_G(5), NOTEhG(5), NOTE_A(5), NOTEhA(5), NOTE_B(5)
};

//accumulator and adders for tone channels, channel 1 has two oscillators

uint32_t channel_active;

uint32_t channel_1a_acc;
uint32_t channel_1a_add;
uint32_t channel_1b_acc;
uint32_t channel_1b_add;

uint32_t channel_2_acc;
uint32_t channel_2_add;

uint32_t channel_1_out;
uint32_t channel_2_out;

//previous speaker output state

uint32_t output_state;

//'drum' sample player variables

uint32_t drum_ptr;
uint32_t drum_sample;

//sync counter to syncronize the music parser with the synth code
//it is volatile because it the main thread reads it back

volatile uint32_t parser_sync;

//song parser variables

uint32_t order_list_off;
uint32_t order_pos;
uint32_t order_loop;
uint32_t order_length;

uint32_t pattern_ptr;

uint32_t instrument_ptr;



void IRAM_ATTR sound_ISR(){
  if (output_state){
    GPIO_REG_WRITE(GPIO_OUT_W1TS_ADDRESS, _BV(SOUNDPIN));   //set
  }
  else{
    GPIO_REG_WRITE(GPIO_OUT_W1TC_ADDRESS, _BV(SOUNDPIN));   //clear
  }
  //also decrement sync variable early, although this isn't that important, no noticeable jitter possible
  if (parser_sync) --parser_sync;
  if (!drum_sample){
    if (channel_active){
      channel_1a_acc += channel_1a_add;
      if (channel_1a_acc >= 0x10000){
        channel_1a_acc -= 0x10000;
        channel_1_out ^= 1;
      }
      channel_1b_acc += channel_1b_add;
      if (channel_1b_acc >= 0x10000){
        channel_1b_acc -= 0x10000;
        channel_1_out ^= 1;
      }
      output_state = channel_1_out;
    }
    else{
      channel_2_acc += channel_2_add;
      if (channel_2_acc >= 0x10000){
        channel_2_acc -= 0x10000;
        channel_2_out ^= 1;
      }
      output_state = channel_2_out;
    }
    channel_active ^= 1;
  }
  else{
    //play drum sample using the drum_sample variable as bit mask
    if (drum_sample_data[drum_ptr >> 1]&drum_sample) output_state = 1; else output_state = 0;
    ++drum_ptr;
    if (drum_ptr >= 1024 * 2) drum_sample = 0; //drum_ptr increments in half steps to compensate doubled sample rate
  }
}



void sound_init(void){
  //initialize song parser
  order_pos   = 0;
  order_loop  = music_data[0]; //Beepola stores order loop and length <<1
  order_length = music_data[1];
  order_list_off = 4 + music_data[2] + (music_data[3] << 8);
  instrument_ptr = 4;
  output_state = 0;
  parser_sync = 0;

  song_new_pattern();
  //initialize tone and drum synth variables
  channel_active = 0;
  channel_1a_acc = 0;
  channel_1b_acc = 0;
  channel_1a_add = 0;
  channel_1b_add = 0;
  channel_2_acc = 0;
  channel_2_add = 0;
  channel_1_out = 0;
  channel_2_out = 0;
  drum_ptr = 0;
  drum_sample = 0;    //this is not sample number, but bit mask, so 0 means no drum is playing

  pinMode(SOUNDPIN, OUTPUT);

  timer1_attachInterrupt(sound_ISR);
  timer1_enable(TIM_DIV1, TIM_EDGE, TIM_LOOP);
  timer1_write(TIMER_RATE);
}



void sound_stop(void){
  timer1_disable();
}



void song_new_pattern(void){
  pattern_ptr = (music_data[order_list_off + order_pos] + (music_data[order_list_off + order_pos + 1] << 8)) - COMPILE_ADR;
  order_pos += 2;
  if (order_pos >= order_length) order_pos = order_loop;
}



uint8_t song_data_read(void){
  return music_data[pattern_ptr++];
}



void setup() {
  Serial.begin(115200); //serial init
  delay (100);
  WiFi.mode(WIFI_OFF); // to safe some battery power
  //buttons on mcp23017 init
  mcp.begin(MCP23017address);
  delay (100);
  for (int i = 0; i < 8; i++){
    mcp.pinMode(i, INPUT);
    mcp.pullUp(i, HIGH);
  }
  sound_init();
}


void loop() {
  uint16_t tag, channel;
  uint32_t add, off;
  bool done;

  while (1){
    for (int i = 0; i < 8; i++){
      if (!mcp.digitalRead(i)){
        sound_stop();
      }
    }
    //update row, parse song data
    channel = 0;
    done = false;
    while (!done){
      tag = song_data_read();
//end of a pattern, move to the next one
      if (!tag){
        song_new_pattern();
        continue;
      }
//0=wait for next row, otherwise a parameter
      if (tag & 0x80){
        switch (tag & 0x3f){
          case 60:    //key off
            if (!channel){
              channel_1a_add = 0;
              channel_1b_add = 0;
              channel_1_out = 0;
              channel = 1;
            }
            else{
              channel_2_add = 0;
              channel_2_out = 0;
            }
            break;
          case 61:    //instrument change
            instrument_ptr = 4 + (song_data_read() << 1); //in the original data instument number stored <<1, one instrument takes 4 bytes
            break;

          case 62:    //second channel
            channel = 1;
            break;
          case 63:    //loop position, not implemented in Beepola version
            break;
          default:     //note
          
            add = note_table[tag & 0x3f];
            if (!channel){
              off = music_data[instrument_ptr + 1] + (music_data[instrument_ptr + 2] << 8);
              channel_1a_add = add;
              channel_1b_add = (add << music_data[instrument_ptr + 0]) + off; //multiple and detune
               //optional phase reset
              if (tag & 0x40){
                channel_1a_acc = 0;
                channel_1b_acc = music_data[instrument_ptr + 3] << 8; //phase
                channel_1_out = 0;
              }

              channel = 1;
            }
            //always reset phase on the channel 2, to hear note restart instead of continuous tone
            else{
              channel_2_acc = 0;
              channel_2_add = add;
              channel_2_out = 0;
            }
        }
      }
      else{
        //end of a row, set the parser sync value according to the wait value (in tempo units)
        if (tag < 118){
          parser_sync = MUSIC_FRAME * tag; //wait for given number of tempo units before playing next row
        }
       //118..127 is a drum
        else{
          drum_ptr = 0;
          drum_sample = 1 << (tag - 118); //bit mask for the drum sample
          parser_sync = MUSIC_FRAME * 1; //drum always take one tempo unit, so it normally followed by the wait command
        }

        done = true;
      }
    }
    //wait for the next row
    while (parser_sync > 0) delay(0);
  }
}
