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
Wire Wire Line
	2300 2900 2300 4050
Wire Wire Line
	3650 1800 4200 1800
Wire Wire Line
	3650 1800 3650 2700
Wire Wire Line
	3650 2050 4200 2050
Wire Wire Line
	3650 2300 4200 2300
Connection ~ 3650 2050
Wire Wire Line
	3650 2650 4100 2650
Wire Wire Line
	3650 2650 3650 4050
Wire Wire Line
	3650 2900 4200 2900
Wire Wire Line
	3650 3150 4200 3150
Connection ~ 3650 2900
Wire Wire Line
	3650 3550 4200 3550
Wire Wire Line
	3650 3800 4200 3800
Wire Wire Line
	2300 4050 4200 4050
Connection ~ 3650 3800
Connection ~ 3650 4050
Connection ~ 3650 3150
Connection ~ 3650 3550
Connection ~ 3650 2300
Connection ~ 3650 2700
Wire Wire Line
	2800 1700 4200 1700
Wire Wire Line
	2850 3100 2750 3100
Wire Wire Line
	2750 3100 2750 1950
Wire Wire Line
	2750 1950 4200 1950
Wire Wire Line
	2650 2200 4200 2200
Wire Wire Line
	2650 2200 2650 3200
Wire Wire Line
	2650 3200 2850 3200
Wire Wire Line
	2850 3300 2550 3300
Wire Wire Line
	2550 3300 2550 2550
Wire Wire Line
	2550 2550 4200 2550
Wire Wire Line
	3600 2800 4200 2800
Wire Wire Line
	3600 2800 3600 3300
Wire Wire Line
	3600 3300 3350 3300
Wire Wire Line
	3350 3200 3500 3200
Wire Wire Line
	3500 3200 3500 3050
Wire Wire Line
	3500 3050 4200 3050
Wire Wire Line
	3350 3100 3350 3450
Wire Wire Line
	3350 3450 4200 3450
Wire Wire Line
	3350 3000 3400 3000
Wire Wire Line
	3400 3000 3400 3700
Wire Wire Line
	3400 3700 4200 3700
Wire Wire Line
	3350 2900 3450 2900
Wire Wire Line
	3450 2900 3450 3950
Wire Wire Line
	3450 3950 4200 3950
$Comp
L Conn_02x05_Counter_Clockwise J1
U 1 1 5A8599AE
P 3050 3100
F 0 "J1" H 3100 3400 50  0000 C CNN
F 1 "Conn_02x05_Counter_Clockwise" H 3100 2800 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x10_Pitch2.00mm_SMD_Pin1Left" H 3050 3100 50  0001 C CNN
F 3 "" H 3050 3100 50  0001 C CNN
	1    3050 3100
	1    0    0    -1  
$EndComp
Wire Wire Line
	2850 2900 2300 2900
Wire Wire Line
	2850 3000 2800 3000
Wire Wire Line
	2800 3000 2800 1700
$Comp
L Conn_01x06 J2
U 1 1 5A86AABC
P 4400 1950
F 0 "J2" H 4400 2250 50  0000 C CNN
F 1 "Conn_01x06" H 4400 1550 50  0000 C CNN
F 2 "virus:detector_pad_6" H 4400 1950 50  0001 C CNN
F 3 "" H 4400 1950 50  0001 C CNN
	1    4400 1950
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x06 J3
U 1 1 5A86AD7C
P 4400 2850
F 0 "J3" H 4400 3150 50  0000 C CNN
F 1 "Conn_01x06" H 4400 2450 50  0000 C CNN
F 2 "virus:detector_pad_6" H 4400 2850 50  0001 C CNN
F 3 "" H 4400 2850 50  0001 C CNN
	1    4400 2850
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x06 J4
U 1 1 5A86ADC5
P 4400 3700
F 0 "J4" H 4400 4000 50  0000 C CNN
F 1 "Conn_01x06" H 4400 3300 50  0000 C CNN
F 2 "virus:detector_pad_6" H 4400 3700 50  0001 C CNN
F 3 "" H 4400 3700 50  0001 C CNN
	1    4400 3700
	1    0    0    -1  
$EndComp
Wire Wire Line
	4200 1700 4200 1750
Wire Wire Line
	4200 1800 4200 1850
Wire Wire Line
	4200 2200 4200 2150
Wire Wire Line
	4200 2300 4200 2250
Wire Wire Line
	4200 2550 4200 2650
Wire Wire Line
	4100 2650 4100 2750
Wire Wire Line
	4100 2750 4200 2750
Wire Wire Line
	4200 2800 4200 2850
Wire Wire Line
	4200 2900 4200 2950
Wire Wire Line
	4200 3450 4200 3500
Wire Wire Line
	4200 3550 4200 3600
Wire Wire Line
	4200 3950 4200 3900
Wire Wire Line
	4200 4050 4200 4000
$EndSCHEMATC
