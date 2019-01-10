import smbus
import time
import os

bus=smbus.SMBus(1)

old_dat = [0,0,0,0,0,0,0,0,0,0]

os.system("aplay startup.wav")

while True:
    try:
        dat = bus.read_i2c_block_data(0x32,0)[2:12]
    except:
        print "i2c error"

    print(dat)
    changed=False        
    for i in range(0,len(dat)):
        if old_dat[i]!=dat[i]:
            changed=True
            if old_dat[i]!=0:
                os.system("aplay wang1.wav")
            if old_dat[i]==0:
                if dat[i]==1:
                    os.system("aplay square.wav")
                if dat[i]==2:
                    os.system("aplay guitar.wav")
                if dat[i]==3:
                    os.system("aplay triangle.wav")
                if dat[i]==5:
                    os.system("aplay circle.wav")
                if dat[i]==6:
                    os.system("aplay donut.wav")

    if changed:
        print dat
        f = open("../../web/client/htdocs/receptors.txt","w")
        for d in dat:
            f.write(str(d)+" ")
        f.close()
        old_dat=dat
    
        
    time.sleep(0.1)
