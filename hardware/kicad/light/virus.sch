EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:switches
LIBS:relays
LIBS:motors
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:virus-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Conn_01x04 J2
U 1 1 5B479A42
P 5450 1900
F 0 "J2" H 5450 2100 50  0000 C CNN
F 1 "Conn_01x04" H 5450 1600 50  0000 C CNN
F 2 "virus:TCRT1000" H 5450 1900 50  0001 C CNN
F 3 "" H 5450 1900 50  0001 C CNN
	1    5450 1900
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x04 J3
U 1 1 5B479AA0
P 5450 2600
F 0 "J3" H 5450 2800 50  0000 C CNN
F 1 "Conn_01x04" H 5450 2300 50  0000 C CNN
F 2 "virus:TCRT1000" H 5450 2600 50  0001 C CNN
F 3 "" H 5450 2600 50  0001 C CNN
	1    5450 2600
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x04 J4
U 1 1 5B479ACD
P 5450 3350
F 0 "J4" H 5450 3550 50  0000 C CNN
F 1 "Conn_01x04" H 5450 3050 50  0000 C CNN
F 2 "virus:TCRT1000" H 5450 3350 50  0001 C CNN
F 3 "" H 5450 3350 50  0001 C CNN
	1    5450 3350
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x06 J1
U 1 1 5B48C336
P 4050 2700
F 0 "J1" H 4050 3000 50  0000 C CNN
F 1 "Conn_01x06" H 4050 2300 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x06_Pitch2.54mm_SMD_Pin1Right" H 4050 2700 50  0001 C CNN
F 3 "" H 4050 2700 50  0001 C CNN
	1    4050 2700
	-1   0    0    1   
$EndComp
Wire Wire Line
	4250 2700 4500 2700
Wire Wire Line
	4500 1900 4500 3350
Wire Wire Line
	4500 2600 5250 2600
Wire Wire Line
	4500 3350 5250 3350
Connection ~ 4500 2700
Wire Wire Line
	4500 1900 5250 1900
Connection ~ 4500 2600
Wire Wire Line
	4250 2400 4250 1800
Wire Wire Line
	4250 1800 5250 1800
Wire Wire Line
	4250 2500 5250 2500
Wire Wire Line
	5250 3250 4350 3250
Wire Wire Line
	4350 3250 4350 2600
Wire Wire Line
	4350 2600 4250 2600
Wire Wire Line
	5250 3450 5250 2800
Wire Wire Line
	5250 2700 5250 2100
Wire Wire Line
	4250 2900 5150 2900
Wire Wire Line
	5150 2900 5150 2000
Wire Wire Line
	5150 2000 5250 2000
Wire Wire Line
	4250 2800 4250 3550
Wire Wire Line
	4250 3550 5250 3550
$EndSCHEMATC
