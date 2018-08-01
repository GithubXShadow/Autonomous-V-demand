import numpy

freeway=[16,9952,9953,10166,10353,10380]
aterial=[16,9952,9953,10166,10353,10380]

path='X:\Zihan\AMS\Simulation\May24\OC2\OC2_ATM_ADM\\'
if __name__ == "__main__":
    linktypefile=open('linktype.csv','r')
    linkid=linktypefile.readlines()
    speedfile=open(path+'OutLinkSpeedAll.dat','r')
    speedout=open(path+'output_speed.csv','w')
    volfile=open(path+'OutLinkVeh.dat','r')
    volout=open(path+'output_vol.csv','w')
    lines=speedfile.readlines()
    lines2=volfile.readlines()
    i=7
    count=0
    volout.write('freeway,aterial\n')
    speedout.write('freeway,aterial\n')
    while (count<1440):
        La=[]
        Sa=[]
        Lf=[]
        Sf=[]
        speed = [map(float,line.split( )) for line in lines[i:i+37]]
        vol = [map(float,line.split( )) for line in lines[i:i+37]]
        for lid in freeway+aterial:
            m=lid/10
            n=lid-m*10-1
            if lid in freeway:
                Lf.append(vol[m][n])
                Sf.append(speed[m][n])
            else:
                La.append(vol[m][n])
                Sa.append(speed[m][n])
        volout.write(str(sum(Lf))+',')
        volout.write(str(sum(La))+'\n')
        speedout.write(str(sum(Sf)/len(Sf))+',')
        speedout.write(str(sum(Sa)/len(Sa))+'\n')
        count=count+1
        i=i+37
    volfile.close
    speedfile.close
    print ('over')