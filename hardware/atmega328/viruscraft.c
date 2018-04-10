#define F_CPU F_OSC

#include <avr/io.h>
#include <util/delay.h>
#include <stdlib.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include "I2C_slave.h"

//#define SINGLE_SENSOR
#define I2C_ID 0x32

#define STATUS_PORT_D DDRC
#define STATUS_PORT PORTC
#define PIN_STATUS PC3

unsigned char receptor_donut    = 0x0; // 000
unsigned char receptor_triangle = 0x1; // 001
unsigned char receptor_circle   = 0x2; // 010
unsigned char receptor_square   = 0x3; // 011
unsigned char receptor_angle    = 0x4; // 100
unsigned char receptor_guitar   = 0x5; // 101
unsigned char receptor_tee      = 0x6; // 110
unsigned char receptor_none     = 99;

unsigned char num_receptors=7;

unsigned char thresh[] = {1,2,2,3,2,2,2,99};

unsigned char filter_receptors(unsigned char a, unsigned char b, unsigned char c) {
  if (a==b==c) return a;
  if (thresh[a]<=1) return a;
  if (thresh[b]<=1) return b;
  if (thresh[c]<=1) return c;
  if ((a==b || a==c) && thresh[a]<=2) return a;
  if ((b==a || b==c) && thresh[b]<=2) return b;
  return receptor_none;
}

// address registers
// PB1 PB3 PB5 PB2
//         00 01 02 03 04 05 06 07
// PORT B: xx AA xx BB xx CC xx xx
// PORT C: xx xx DD xx xx xx xx xx

void set_addr(unsigned char addr) {
  PORTB = (addr&0x01)<<1 |
          (addr&0x02)<<2 |
          (addr&0x04)<<2;
  PORTC = (addr&0x08)>>1;
}

// receptor registers

//          00 01 02 03 04 05 06 07
// Port D: |A1 A2 A3|B3 B2 B1|C1 C2| 
// Port C:  xx xx xx|C3|xx xx xx xx

unsigned char read_a() {
  return PIND&0x07;
}

unsigned char read_b() {
  // flipped, sigh...
  return (PIND&0x20)>>5 |
         (PIND&0x10)>>3 |
         (PIND&0x08)>>1;
}

unsigned char read_c() {
  return ((PIND&0xC0)>>6) | ((PINC&0x08)>>1);
}

void set_led_state(unsigned char s) {
    if (s) STATUS_PORT|=_BV(PIN_STATUS);
    else STATUS_PORT&=~_BV(PIN_STATUS);
}

////////////////////////////////////////////////////////

int main(void) {
  I2C_init(I2C_ID<<1); // initalize as slave with address 0x32
  
  sei();
  //   MCUSR = 0;
  //wdt_disable();
  
  DDRC = 0x04;
  DDRD = 0x00; // input
  DDRB = 0x0f; // output

  for (int i=0; i<0xFF; i++) {
    i2cbuffer[i]=99;
  }

  unsigned char led=0;

  //  PORTC|=_BV(FACE_SELECT_PIN);
  //PORTC&=~_BV(FACE_SELECT_PIN);

  unsigned int count=0;

  while(1) {    
    i2cbuffer[0]=count;
    count++;

    unsigned int bufferpos=1;
    for (unsigned char addr=0; addr<2; addr++) {
      set_addr(addr);
      _delay_ms(250);
      i2cbuffer[bufferpos++]=read_a();
      i2cbuffer[bufferpos++]=read_b();
      i2cbuffer[bufferpos++]=read_c();
      _delay_ms(250);
    }

    //    _delay_ms(500);
    //set_led_state(1);
    //_delay_ms(20);
    //set_led_state(0);
    //    wdt_reset();
  }
}

