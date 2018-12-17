#define F_CPU F_OSC

#include <avr/io.h>
#include <util/delay.h>
#include <stdlib.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include "I2C_slave.h"

#define I2C_ID 0x32

// sensor mapping to trianglular faces, in the right order
const int triples[] = {
   0,8,4,
   12,2,10,
   6,14,1,
   3,11,7,
   9,5,13
};

const int absent_thresh = 700; // not used
const int black_thresh = 200;

unsigned char receptor_donut    = 0x0; // 000
unsigned char receptor_triangle = 0x1; // 001
unsigned char receptor_circle   = 0x2; // 010
unsigned char receptor_square   = 0x3; // 011
unsigned char receptor_angle    = 0x4; // 100
unsigned char receptor_guitar   = 0x5; // 101
unsigned char receptor_tee      = 0x6; // 110
unsigned char receptor_none     = 99;

unsigned char num_receptors=10;

void adc_init(void) {
    // AREF = AVcc
    ADMUX = (1<<REFS0);
 
    // ADC Enable and prescaler of 128
    // 16000000/128 = 125000
    ADCSRA = (1<<ADEN)|(1<<ADPS2)|(1<<ADPS1)|(1<<ADPS0);
}

uint16_t adc_read(uint8_t ch) {
  // select the corresponding channel 0~7
  // ANDing with ’7′ will always keep the value
  // of ‘ch’ between 0 and 7
  ch &= 0b00000111;  // AND operation with 7
  ADMUX = (ADMUX & 0xF8)|ch; // clears the bottom 3 bits before ORing
 
  // start single convertion
  // write ’1′ to ADSC
  ADCSRA |= (1<<ADSC);
 
  // wait for conversion to complete
  // ADSC becomes ’0′ again
  // till then, run loop continuously
  while(ADCSRA & (1<<ADSC));
 
  return (ADC);
}

// address registers
// Port D 0-3
void set_addr(unsigned char addr) {
  PORTD = (PORTD&0xf0) | (addr&0x0f);
}

// which pcb to turn on (also switches LEDs on for this block)
void set_block(unsigned char block) {
  if (block==0) {
    PORTD&=~_BV(PD4);
    PORTD|=_BV(PD5);
  } else {
    PORTD|=_BV(PD4);
    PORTD&=~_BV(PD5);
  }
}

unsigned char read_photo(int block, int id) {
  // set enable for the PCB
  set_block(block);
  set_addr(id);
  // set the address
  _delay_ms(20);    

  int value = 0;
  if (block==0) {
    value = adc_read(1);   
    } else { 
    value = adc_read(0);  
  }
  
  if (value<black_thresh) {
    return 1;
  } else {
    return 0;
  }
}

unsigned char read_triangle(int index) {
  unsigned char triangle = index%5;
  unsigned char block = 1;
  if (index<5) block = 0;
  unsigned char ret=read_photo(block,triples[triangle*3]) |
    (read_photo(block,triples[triangle*3+1])<<1) |
    (read_photo(block,triples[triangle*3+2])<<2);
  return ret;
}

////////////////////////////////////////////////////////

int main(void) {
  I2C_init(I2C_ID<<1); // initalize as slave with address 0x32
  adc_init();
  sei();
  
  // use port d for the address and enable outputs
  DDRD = 0xff; // output
  // port C 0,1 are the analogue inputs
  // port C 4.5 is i2c

  for (int i=0; i<0xFF; i++) {
    i2cbuffer[i]=99;
  }

  unsigned int count=0;
  set_block(0);

  while(1) {    
    i2cbuffer[0]=count;
    count++;    
    unsigned int bufferpos=1;
    for (unsigned char triangle=0; triangle<10; triangle++) {
      i2cbuffer[bufferpos++]=read_triangle(triangle);
    } 
    
    //    _delay_ms(500);
    //set_led_state(1);
    //_delay_ms(20);
    //set_led_state(0);
    //    wdt_reset();
  }
}

