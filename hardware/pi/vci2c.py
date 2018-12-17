import smbus
import time

bus=smbus.SMBus(1)

old_dat=[0,99,99,99]

def dat_eq(a,b):
    return a[1]==b[1] and a[2]==b[2] and a[3]==b[3]

while True:
    try:
        dat = bus.read_i2c_block_data(0x32,0)[1:10]

        if not dat_eq(dat,old_dat):
            print dat
            f = open("../../web/client/htdocs/receptors.txt","w")
            f.write(str(dat[1])+" "+str(dat[2])+" "+str(dat[3])+"\n")
            f.close()
            old_dat[1]=dat[1]
            old_dat[2]=dat[2]
            old_dat[3]=dat[3]
            
    except:
        print "i2c error"
        
    time.sleep(0.5)

    
