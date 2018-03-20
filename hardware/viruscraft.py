import RPi.GPIO as GPIO
import time

GPIO.setmode(GPIO.BOARD)

for pin in [33,35,37,11,13,15]:
    GPIO.setup(pin, GPIO.IN, pull_up_down = GPIO.PUD_UP)


receptors = { "circle": [0,1,0],
              "guitar" : [1,0,1],
              "triangle" : [0,0,0],
              "tee": [1,1,0],
              "arrow": [1,0,0] }

def check_receptor(pins):
    for r,c in receptors.items():
        if c==pins: return r
    return "none"

while True:
    in_pins = [GPIO.input(33),
               GPIO.input(35),
               GPIO.input(37),
               GPIO.input(11),
               GPIO.input(13),
               GPIO.input(15)]

    
    
    r1=check_receptor(in_pins[:3])
    r2=check_receptor(in_pins[3:3])

    if r1==r2: print(r1+": high confidence")
    elif r1=="none":
        print(r2+": low confidence")
    elif r2=="none":
        print(r1+": low confidence")
        
    
    time.sleep(0.5)
