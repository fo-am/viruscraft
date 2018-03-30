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
LIBS:proto-cache
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
L ATMEGA328P-PU U1
U 1 1 5ABAAF99
P 4400 3150
F 0 "U1" H 3650 4400 50  0000 L BNN
F 1 "ATMEGA328P-PU" H 4800 1750 50  0000 L BNN
F 2 "Housings_DIP:DIP-28_W7.62mm" H 4400 3150 50  0001 C CIN
F 3 "" H 4400 3150 50  0001 C CNN
	1    4400 3150
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x04 J2
U 1 1 5ABAAF9A
P 2100 2250
F 0 "J2" H 2100 2450 50  0000 C CNN
F 1 "Power/i2c" H 2100 1950 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x04_Pitch2.54mm" H 2100 2250 50  0001 C CNN
F 3 "" H 2100 2250 50  0001 C CNN
	1    2100 2250
	-1   0    0    1   
$EndComp
$Comp
L Crystal Y1
U 1 1 5ABAAF9D
P 7050 2750
F 0 "Y1" H 7050 2900 50  0000 C CNN
F 1 "Crystal" H 7050 2600 50  0000 C CNN
F 2 "Crystals:Crystal_HC52-8mm_Vertical" H 7050 2750 50  0001 C CNN
F 3 "" H 7050 2750 50  0001 C CNN
	1    7050 2750
	1    0    0    -1  
$EndComp
$Comp
L C C2
U 1 1 5ABAAF9E
P 6900 3100
F 0 "C2" H 6925 3200 50  0000 L CNN
F 1 "C" H 6925 3000 50  0000 L CNN
F 2 "Capacitors_THT:C_Axial_L3.8mm_D2.6mm_P7.50mm_Horizontal" H 6938 2950 50  0001 C CNN
F 3 "" H 6900 3100 50  0001 C CNN
	1    6900 3100
	1    0    0    -1  
$EndComp
$Comp
L C C3
U 1 1 5ABAAF9F
P 7200 3100
F 0 "C3" H 7225 3200 50  0000 L CNN
F 1 "C" H 7225 3000 50  0000 L CNN
F 2 "Capacitors_THT:C_Axial_L3.8mm_D2.6mm_P7.50mm_Horizontal" H 7238 2950 50  0001 C CNN
F 3 "" H 7200 3100 50  0001 C CNN
	1    7200 3100
	1    0    0    -1  
$EndComp
$Comp
L Earth #PWR01
U 1 1 5ABAAFA0
P 7050 3250
F 0 "#PWR01" H 7050 3000 50  0001 C CNN
F 1 "Earth" H 7050 3100 50  0001 C CNN
F 2 "" H 7050 3250 50  0001 C CNN
F 3 "" H 7050 3250 50  0001 C CNN
	1    7050 3250
	1    0    0    -1  
$EndComp
$Comp
L Earth #PWR02
U 1 1 5ABAAFA2
P 2950 2300
F 0 "#PWR02" H 2950 2050 50  0001 C CNN
F 1 "Earth" H 2950 2150 50  0001 C CNN
F 2 "" H 2950 2300 50  0001 C CNN
F 3 "" H 2950 2300 50  0001 C CNN
	1    2950 2300
	1    0    0    -1  
$EndComp
$Comp
L Earth #PWR03
U 1 1 5ABAAFA3
P 2900 4400
F 0 "#PWR03" H 2900 4150 50  0001 C CNN
F 1 "Earth" H 2900 4250 50  0001 C CNN
F 2 "" H 2900 4400 50  0001 C CNN
F 3 "" H 2900 4400 50  0001 C CNN
	1    2900 4400
	1    0    0    -1  
$EndComp
$Comp
L Conn_02x03_Counter_Clockwise J1
U 1 1 5ABAAFA4
P 1850 3700
F 0 "J1" H 1900 3900 50  0000 C CNN
F 1 "ISP" H 1900 3500 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_2x03_Pitch2.54mm" H 1850 3700 50  0001 C CNN
F 3 "" H 1850 3700 50  0001 C CNN
	1    1850 3700
	1    0    0    -1  
$EndComp
$Comp
L Earth #PWR04
U 1 1 5ABAAFA5
P 2300 4150
F 0 "#PWR04" H 2300 3900 50  0001 C CNN
F 1 "Earth" H 2300 4000 50  0001 C CNN
F 2 "" H 2300 4150 50  0001 C CNN
F 3 "" H 2300 4150 50  0001 C CNN
	1    2300 4150
	1    0    0    -1  
$EndComp
$Comp
L R R1
U 1 1 5ABAAFA6
P 5700 1700
F 0 "R1" V 5780 1700 50  0000 C CNN
F 1 "R" V 5700 1700 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0204_L3.6mm_D1.6mm_P7.62mm_Horizontal" V 5630 1700 50  0001 C CNN
F 3 "" H 5700 1700 50  0001 C CNN
	1    5700 1700
	1    0    0    -1  
$EndComp
$Comp
L C C1
U 1 1 5ABAAFA8
P 3100 3900
F 0 "C1" H 3125 4000 50  0000 L CNN
F 1 "C" H 3125 3800 50  0000 L CNN
F 2 "Capacitors_THT:C_Axial_L3.8mm_D2.6mm_P7.50mm_Horizontal" H 3138 3750 50  0001 C CNN
F 3 "" H 3100 3900 50  0001 C CNN
	1    3100 3900
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR05
U 1 1 5ABAAFB3
P 2800 2300
F 0 "#PWR05" H 2800 2150 50  0001 C CNN
F 1 "+5V" H 2800 2440 50  0000 C CNN
F 2 "" H 2800 2300 50  0001 C CNN
F 3 "" H 2800 2300 50  0001 C CNN
	1    2800 2300
	-1   0    0    1   
$EndComp
$Comp
L +5V #PWR06
U 1 1 5ABAAFB4
P 5700 1450
F 0 "#PWR06" H 5700 1300 50  0001 C CNN
F 1 "+5V" H 5700 1590 50  0000 C CNN
F 2 "" H 5700 1450 50  0001 C CNN
F 3 "" H 5700 1450 50  0001 C CNN
	1    5700 1450
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR07
U 1 1 5ABAAFB5
P 3300 1750
F 0 "#PWR07" H 3300 1600 50  0001 C CNN
F 1 "+5V" H 3300 1890 50  0000 C CNN
F 2 "" H 3300 1750 50  0001 C CNN
F 3 "" H 3300 1750 50  0001 C CNN
	1    3300 1750
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR08
U 1 1 5ABAAFB6
P 3100 3400
F 0 "#PWR08" H 3100 3250 50  0001 C CNN
F 1 "+5V" H 3100 3540 50  0000 C CNN
F 2 "" H 3100 3400 50  0001 C CNN
F 3 "" H 3100 3400 50  0001 C CNN
	1    3100 3400
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR09
U 1 1 5ABAAFB7
P 2300 3500
F 0 "#PWR09" H 2300 3350 50  0001 C CNN
F 1 "+5V" H 2300 3640 50  0000 C CNN
F 2 "" H 2300 3500 50  0001 C CNN
F 3 "" H 2300 3500 50  0001 C CNN
	1    2300 3500
	1    0    0    -1  
$EndComp
Wire Wire Line
	2900 4250 3500 4250
Wire Wire Line
	2900 4350 3500 4350
Wire Wire Line
	3300 1750 3300 2350
Wire Wire Line
	3300 2050 3500 2050
Wire Wire Line
	5400 3300 6500 3300
Wire Wire Line
	5800 3300 5800 4650
Wire Wire Line
	5800 4650 2600 4650
Wire Wire Line
	2600 4650 2600 2250
Wire Wire Line
	2600 2250 2300 2250
Wire Wire Line
	2300 2350 2650 2350
Wire Wire Line
	2650 2350 2650 4600
Wire Wire Line
	2650 4600 5750 4600
Wire Wire Line
	5750 4600 5750 3400
Wire Wire Line
	5400 3400 6500 3400
Wire Wire Line
	5400 2050 6500 2050
Wire Wire Line
	5400 2150 6500 2150
Wire Wire Line
	5400 2250 6500 2250
Wire Wire Line
	5400 2350 6500 2350
Wire Wire Line
	5400 2450 6500 2450
Wire Wire Line
	5400 2550 6500 2550
Wire Wire Line
	5400 2650 6850 2650
Wire Wire Line
	5400 2750 6900 2750
Wire Wire Line
	5400 2900 6750 2900
Wire Wire Line
	5400 3000 6700 3000
Wire Wire Line
	5400 3100 6600 3100
Wire Wire Line
	5400 3200 6500 3200
Connection ~ 5800 3300
Connection ~ 5750 3400
Wire Wire Line
	5400 3500 6500 3500
Wire Wire Line
	5400 3650 6500 3650
Wire Wire Line
	5400 3750 6500 3750
Wire Wire Line
	5400 3850 6500 3850
Wire Wire Line
	5400 3950 6500 3950
Wire Wire Line
	5400 4050 6500 4050
Wire Wire Line
	5400 4150 7000 4150
Wire Wire Line
	6850 2650 6850 2550
Wire Wire Line
	6850 2550 7200 2550
Wire Wire Line
	7200 2550 7200 2950
Connection ~ 7200 2750
Wire Wire Line
	6900 2750 6900 2950
Connection ~ 6900 2750
Wire Wire Line
	6900 3250 7200 3250
Connection ~ 7050 3250
Wire Wire Line
	3300 2350 3500 2350
Wire Wire Line
	2300 2050 2950 2050
Wire Wire Line
	2950 2050 2950 2300
Wire Wire Line
	2900 4250 2900 4400
Connection ~ 2900 4350
Wire Wire Line
	2300 3800 2300 4150
Wire Wire Line
	5850 4700 1400 4700
Wire Wire Line
	1400 4700 1400 3800
Wire Wire Line
	1400 3800 1650 3800
Wire Wire Line
	5450 2350 5450 4750
Wire Wire Line
	5450 4750 2550 4750
Wire Wire Line
	2550 4750 2550 3700
Wire Wire Line
	2550 3700 2150 3700
Connection ~ 5450 2350
Wire Wire Line
	2150 3800 2300 3800
Wire Wire Line
	5500 2450 5500 4800
Wire Wire Line
	5500 4800 1300 4800
Wire Wire Line
	1300 4800 1300 3600
Connection ~ 5500 2450
Wire Wire Line
	1300 3600 1650 3600
Wire Wire Line
	5850 4700 5850 3500
Connection ~ 5850 3500
Wire Wire Line
	5550 2550 5550 4850
Wire Wire Line
	5550 4850 1500 4850
Wire Wire Line
	1500 4850 1500 3700
Wire Wire Line
	1500 3700 1650 3700
Connection ~ 5550 2550
Connection ~ 5700 3500
Wire Wire Line
	3100 4050 3100 4250
Connection ~ 3100 4250
Wire Wire Line
	2300 2150 2800 2150
Wire Wire Line
	2800 2150 2800 2300
Wire Wire Line
	5700 3500 5700 1850
Wire Wire Line
	5700 1550 5700 1450
Connection ~ 3300 2050
Wire Wire Line
	3100 3750 3100 3400
Wire Wire Line
	2150 3600 2300 3600
Wire Wire Line
	2300 3600 2300 3500
$Comp
L Conn_01x05 J3
U 1 1 5ABAD105
P 6700 3850
F 0 "J3" H 6700 4150 50  0000 C CNN
F 1 "Conn_01x05" H 6700 3550 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x05_Pitch2.54mm" H 6700 3850 50  0001 C CNN
F 3 "" H 6700 3850 50  0001 C CNN
	1    6700 3850
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x05 J4
U 1 1 5ABAD24F
P 7300 3650
F 0 "J4" H 7300 3950 50  0000 C CNN
F 1 "Conn_01x05" H 7300 3350 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x05_Pitch2.54mm" H 7300 3650 50  0001 C CNN
F 3 "" H 7300 3650 50  0001 C CNN
	1    7300 3650
	1    0    0    -1  
$EndComp
Wire Wire Line
	7000 4150 7000 3450
Wire Wire Line
	7000 3450 7100 3450
Wire Wire Line
	7100 3850 7100 4300
$Comp
L Earth #PWR010
U 1 1 5ABAD89C
P 7100 4300
F 0 "#PWR010" H 7100 4050 50  0001 C CNN
F 1 "Earth" H 7100 4150 50  0001 C CNN
F 2 "" H 7100 4300 50  0001 C CNN
F 3 "" H 7100 4300 50  0001 C CNN
	1    7100 4300
	1    0    0    -1  
$EndComp
Wire Wire Line
	6750 2900 6750 3550
Wire Wire Line
	6750 3550 7100 3550
Wire Wire Line
	6700 3000 6700 3500
Wire Wire Line
	6700 3500 7100 3500
Wire Wire Line
	7100 3500 7100 3650
Wire Wire Line
	6600 3150 6850 3150
Wire Wire Line
	6850 3150 6850 3750
Wire Wire Line
	6850 3750 7100 3750
Wire Wire Line
	6600 3100 6600 3150
$EndSCHEMATC
