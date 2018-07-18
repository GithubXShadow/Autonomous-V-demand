numL=4805
import numpy as np
if __name__ == "__main__":
    speedfile=open('OutLinkSpeedAll.dat','r')
    volfile=open('Linkvolume.dat','r')
    ttfile=open('totaltt2.csv','w')
    #volout=open('outlinkvol.csv','w')
    lines=speedfile.readlines()
    lines2=volfile.readlines()
    AS=[]
    PS=[]
    PS2=[]
    i=7
    count=0
    listtt=[]
    listspeed=[]
    listvol=[]
    #ttfile.write('average,80%tt,95%tt')
    while (count<1440):
        speed = [map(float,line.split( )) for line in lines[i:i+37]]
        vol=[map(float,line.split( )) for line in lines2[i:i+37]]
        listspeed=speed[0]
        listvol=vol[0]
        for p in range(1,37):
            listspeed=listspeed+speed[p]
            listvol=listvol+vol[p]
        listtt=listspeed
        for s in range(len(listspeed)):
            listtt[s]=listspeed[s]*listvol[s]
        arraytt=np.asarray(listtt)
        stt=sum(arraytt)
        aves=sum(arraytt)/numL
        pers=np.percentile(arraytt, 80)
        pers2=np.percentile(arraytt, 95)
        AS.append(aves)
        PS.append(pers)
        PS2.append(pers2)
        ttfile.write(str(stt)+'\n')
        #ttfile.write(str(aves)+','+str(pers)+','+str(pers2)+'\n')
        i=i+40
        count=count+1
    speedfile.close
    print ('over')
    

