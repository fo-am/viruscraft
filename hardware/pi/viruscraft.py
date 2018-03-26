import RPi.GPIO as GPIO
import time
import os

GPIO.setmode(GPIO.BOARD)

pins = [33,35,37, 11,13,15, 19,23,21]

for pin in pins:
    GPIO.setup(pin, GPIO.IN, pull_up_down = GPIO.PUD_OFF)


receptors = {
    "donut": [0,0,0],
    "angle": [0,0,1],
    "circle": [0,1,0],
    "square":[0,1,1],
    "triangle":[1,0,0],
    "tee":[1,1,0],
}

likely = {
    "donut": 1,
    "angle": 2,
    "circle": 2,
    "square": 3,
    "triangle": 2,
    "tee": 3,
    "none": 99,
}
    
def check_receptor(pins):
    for r,c in receptors.items():
        if c==pins: return r
    return "none"

def filter_receptors(rec):
    print(rec)
    if rec[0]==rec[1]==rec[2]: return rec[0]
    if likely[rec[0]]<=1: return rec[0]
    if likely[rec[1]]<=1: return rec[1]
    if likely[rec[2]]<=1: return rec[2]
    if rec[0]==rec[1] or rec[0]==rec[2]:
        if likely[rec[0]]<=2: return rec[0]
    if rec[1]==rec[0] or rec[1]==rec[2]:
        if likely[rec[1]]<=2: return rec[1]
    return "none"
         
last_one = "none"

while True:
    
    in_pins = []

    for pin in pins:
        in_pins.append(GPIO.input(pin))

#    print(in_pins)
    print(in_pins[:3],"-",in_pins[3:6],"-",in_pins[6:])
     
    r=filter_receptors([check_receptor(in_pins[:3]),
                        check_receptor(in_pins[3:6]),
                        check_receptor(in_pins[6:])])

    print(r)
    if r!="none": os.system("aplay "+r+".wav")
    
    time.sleep(0.5)
