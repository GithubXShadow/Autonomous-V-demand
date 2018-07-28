numL=369
import numpy as np
if __name__ == "__main__":
    speedfile=open('OutLinkSpeedAll.dat','r')
    volfile=open('LinkVolume.dat','r')
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
        speed = [list(map(float,line.split( ))) for line in lines[i:i+37]]
        vol=[list(map(float,line.split( ))) for line in lines2[i:i+37]]
        listspeed=speed[0]
        listvol=vol[0]
       
        for p in range(1,37):
            listspeed=listspeed+speed[p]
            listvol=listvol+vol[p]
        listtt=listspeed
        # for s in range(len(listspeed)):
        #     listtt[s]=listspeed[s]*listvol[s]
        listtt_temp=[[i]*int(j) for i,j in zip(listspeed,listvol)]
        listtt=[x for sub_list in listtt_temp for x in sub_list]
        
        arraytt=np.asarray(listtt)
        temp1=np.asarray(listvol)
        stt=sum(arraytt)
        
        if stt==0:
            aves=0
            pers=0
            pers2=0
        else:
            aves=sum(temp1)/sum(arraytt)*60
            pers=60/np.percentile(arraytt, 80)
            pers2=60/np.percentile(arraytt, 95)
        
        
        AS.append(aves)
        PS.append(pers)
        PS2.append(pers2)
        # ttfile.write(str(stt)+'\n')
        ttfile.write(str(aves)+','+str(pers)+','+str(pers2)+'\n')
        i=i+40
        count=count+1
        # count=1440
    speedfile.close
    print ('over')
    

