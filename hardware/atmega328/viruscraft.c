#define F_CPU F_OSC

#include <avr/io.h>
#include <util/delay.h>
#include <stdlib.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include "I2C_slave.h"

//#define SINGLE_SENSOR
#define I2C_ID 0x32

// physical pins on port D
#define SENSE_A_PORT PIND
#define PIN_SENSE_A_0 PD0
#define PIN_SENSE_A_1 PD1
#define PIN_SENSE_A_2 PD2

#define SENSE_B_PORT PIND
#define PIN_SENSE_B_0 PD3
#define PIN_SENSE_B_1 PD4
#define PIN_SENSE_B_2 PD5

// physical pins on port C
#define SENSE_C_PORT PINC
#define PIN_SENSE_C_0 PC0
#define PIN_SENSE_C_1 PC1
#define PIN_SENSE_C_2 PC2

#define STATUS_PORT_D DDRC
#define STATUS_PORT PORTC
#define PIN_STATUS PC3

unsigned char receptor_donut    = 0x0; // 000
unsigned char receptor_angle    = 0x1; // 001
unsigned char receptor_circle   = 0x2; // 010
unsigned char receptor_square   = 0x3; // 011
unsigned char receptor_triangle = 0x4; // 100
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

unsigned char read_a() {
  unsigned int mask = _BV(PIN_SENSE_A_0)|_BV(PIN_SENSE_A_1)|_BV(PIN_SENSE_A_2);
  return SENSE_A_PORT&0x7;
}

unsigned char read_b() {
  unsigned int mask = PIN_SENSE_B_0|PIN_SENSE_B_1|PIN_SENSE_B_2;
  return SENSE_B_PORT&mask;
}

unsigned char read_c() {
  unsigned int mask = PIN_SENSE_C_0|PIN_SENSE_C_1|PIN_SENSE_C_2;
  return (SENSE_C_PORT&mask)>>3;
}

void set_led_state(unsigned char s)
{
    if (s) STATUS_PORT|=_BV(PIN_STATUS);
    else STATUS_PORT&=~_BV(PIN_STATUS);
}

int main(void) {
  I2C_init(I2C_ID<<1); // initalize as slave with address 0x32
  
  sei();
  //   MCUSR = 0;
  wdt_disable();
  
  STATUS_PORT_D = 0xff;
  DDRD = 0x0;

  set_led_state(0);

  for (int i=0; i<0xFF; i++) {
    i2cbuffer[i]=99;
  }

  unsigned char led=0;

  while(1) {    
    if (read_a()==receptor_guitar) {
      set_led_state(1);
    }
    _delay_ms(500);
    set_led_state(1);
    _delay_ms(20);
    set_led_state(0);
    //    wdt_reset();
  }
}

