
import numpy as np

import pandas as pd
import matplotlib as plt
import networkx as nx
import math
import random
import requests
import time
import os
def read_network(path):
    '''
        input: network.dat
        output:
            num_zones: number of zones within the network
            num_nodes: number of nodes in the network
            num_links: number of links in the network
            node_detail: node id and zone which the node belong to
            node_id: match the node id to 1~num_nodes to save memory
            link_detail:The information associated with a specific link
            link_id: a matrix to find a specific link based on origin node
                and destination node 
   '''
    global num_zones,num_nodes,num_links,node_detail,node_id,link_detail,link_id
    file=open(path,'r')
    i=0
    network_basic=list()
    node_list=list()
    node_id={}
    link_detail_list=list()
    for line in file:
        line_list_temp=line.split()
        if i==0:
            num_zones=int(line_list_temp[0])
            num_nodes=int(line_list_temp[1])
            num_links=int(line_list_temp[2])
            num_shortest_path=int(line_list_temp[3])
            zone_flag=int(line_list_temp[4])
            link_id=-np.ones((num_nodes,num_nodes))
        #Read the node information from network.dat
        elif len(line_list_temp)<3:
            node_list.append([int(line_list_temp[0]),int(line_list_temp[1])])
            node_id.update({int(line_list_temp[0]):i-1})
        #Read the link information from network.dat
        else:
            for j in line_list_temp:
                if '+' in j and len(j)>2:
                    check=j.split('+')
                    line_list_temp.pop(6) 
                    line_list_temp.insert(6,check[0])
                    line_list_temp.insert(7,check[1])
            line_list_temp=[float(j) for j in line_list_temp]
            link_detail_list.append(line_list_temp)
            link_id[node_id[line_list_temp[0]],node_id[line_list_temp[1]]]=int(i-num_nodes-1)
        i=i+1
        
    node_detail=np.matrix(node_list)
    link_detail=np.matrix(link_detail_list)
    
    return  num_zones,num_nodes,num_links,node_detail,node_id,link_detail,link_id
def read_flow(path):
    '''
        input: outflow.dat
        output: 
            link_volume - matrix
    '''
    global link_volume
    volume_time=[]
    file=open(path)
    i=1
    link_volume=[]
    link_volume_one_time_interval=[]
    for line in file:
        line_list=line.split()
        if i>6 : # skip the first 6 lines
            if len(line_list)==1: #find the line indicating time interval
                volume_time.append(float(line_list[0]))
                link_volume.append(link_volume_one_time_interval)
                link_volume_one_time_interval=[]
            else:
                line_list=[float(j) for j in line_list]
                link_volume_one_time_interval.extend(line_list)
        i=i+1
    link_volume.append(link_volume_one_time_interval)
    link_volume.pop(0)
    link_volume=np.array(link_volume)
    return link_volume

def read_xy(path):
    '''
        input: xy.dat
        output: nodexy: a dictionary storing the latitude and longtitude of 
            each node
    '''
    global nodexy
    nodexy={}
    file=open(path)
    for line in file:
        line_list_temp=line.split()
        line_list_temp=[float(j) for j in line_list_temp]
        nodexy.update({line_list_temp[0]:[line_list_temp[1]/1000000.0,line_list_temp[2]/1000000.0]})
    return nodexy
def read_origin_destion(origin_path,destination_path):
    global origins,destinations
    origin_links=[]
    destination_nodes=[]
    origin_nodes=[]
    origin_file=open(origin_path)
    zoneflag=1
    for line in origin_file:
        line_temp=line.split()
        if (line_temp!=[]):
            if zoneflag==1:
                #read the information as zone
                zone_id=int(line_temp[0])
                num_links=float(line_temp[1])
                zoneflag=0
                link_counter=0
            else:
                origin_links.append([zone_id,int(line_temp[0]),int(line_temp[1])])
                origin_nodes.append([zone_id,int(line_temp[1])])
                link_counter +=1
                if link_counter==num_links:
                    zoneflag=1
                
    destination_file=open(destination_path)
    for line in destination_file:
        line_temp=line.split()
        for i in range(int(line_temp[1])):
            destination_nodes.append([int(line_temp[0]),int(line_temp[i+2])])
    destinations=pd.DataFrame(destination_nodes,columns=['zone_id','nodes'])
    origins=pd.DataFrame(origin_nodes,columns=['zone_id','nodes'])
    origins_links=pd.DataFrame(origin_links,columns=['zone_id','u_node','d_node'])
    return origins, destinations,origins_links


#This cell contains functions read vehicle and transit skims
def read_vehicle_skim(skim_folder_path):
    '''
    This function reads in the skim file information and store them in a multiindex dataframe Vehicular_Skim
    '''
    Max_Num_Vot=5
    # Get the basic parameters for reading the vehicle skim
    NumSkimSuperZone=len(os.listdir(skim_folder_path))
    with open(skim_folder_path+'/1.dat') as f: 
        for line,i in zip(f,range(3)):
            if i==0 and 'TAZ' in line: 
                NumIteration=1
                NumSkimOriginZone=line.split()[0]
            elif i==0 and 'NTI' in line: 
                NumIteration=0
                NumSkimIntervals=line.split()[0]
            elif i==1 and 'NTI' in line: 
                NumSkimIntervals=line.split()[0]
            elif i==1 and 'TAZ' in line:
                NumSkimOriginZone=line.split()[0]
    NumSkimIntervals=int(NumSkimIntervals)
    NumSkimOriginZone=int(NumSkimOriginZone)
    print(NumSkimSuperZone,NumSkimOriginZone,NumSkimIntervals)
    #Create a list of Time and Origin. The list is determined by how the data were writen in the vehicularpnrskim
    #When interation larger than 0, the file iterate through time frist. In other words the loop should be writen as 
    #(do O=1,NumSkimOriginZone: do T=1, NumSkimIntervals). When iteration number is lager than 1, the order reverse
    if NumIteration>0:
        Tlist=list(list(range(1,NumSkimIntervals+1))*NumSkimOriginZone)
        Olist=list(np.repeat(range(1,NumSkimOriginZone+1),NumSkimIntervals))
    else:
        Tlist=list(np.repeat(range(1,NumSkimIntervals+1),NumSkimOriginZone))
        Olist=list(list(range(1,NumSkimOriginZone+1))*NumSkimIntervals)
    #Start to read the files 
    Os=[]
    Ds=[]
    Ts=[]
    VotNo=[]
    VotValue=[]
    Cost=[]
    Time=[]
    Dist=[]
    #Vehicular_Skim_Dict for fast speed
    Vehicular_Skim_Dict={}
    for O in range(1,NumSkimOriginZone+1):
        Vehicular_Skim_Dict[O]={}
        for D in range(1,NumSkimSuperZone+1):
            Vehicular_Skim_Dict[O][D]={}
            for T in range(1,NumSkimIntervals+1):
                Vehicular_Skim_Dict[O][D][T]={}

    for D in range(1,NumSkimSuperZone+1): #D is the destination 
#         print('read',D,datetime.datetime.now())
        with open(skim_folder_path+'/'+str(D)+'.dat') as f: 
            for _ in range(2):
                next(f)
            for line,O,T in zip(f,Olist,Tlist): 
                line_list=line.split()
                Num_VOT=int(line_list[0])
                for j in range(1,Num_VOT+1): 

                    Os.extend([int(O)])
                    Ds.extend([int(D)])
                    Ts.extend([int(T)])
                    VotNo.extend([j])
                    VotValue.extend([float(line_list[4*j-3])])
                    Cost.extend([float(line_list[4*j-2])])
                    Time.extend([float(line_list[4*j-1])])
                    Dist.extend([float(line_list[4*j])])

                    Vehicular_Skim_Dict[O][D][T][j]={}
                    Vehicular_Skim_Dict[O][D][T][j]['VOT']=float(line_list[4*j-3])
                    Vehicular_Skim_Dict[O][D][T][j]['Cost']=float(line_list[4*j-2])
                    Vehicular_Skim_Dict[O][D][T][j]['Time']=float(line_list[4*j-1])
                    Vehicular_Skim_Dict[O][D][T][j]['Dist']=float(line_list[4*j])


    Vehicular_Skim=pd.DataFrame(data={'O':Os,'D':Ds,'TI':Ts ,'VotIndex':VotNo,'Vot':VotValue
                                     ,'Cost':Cost,'Time':Time,'Dist':Dist})    


    Vehicular_Skim.set_index(['O', 'D','TI','VotIndex'], inplace=True)
    Vehicular_Skim.sort_index(inplace=True)
    return Vehicular_Skim,Vehicular_Skim_Dict
def read_transit_setting(transit_setting_filepath):
    f=open(transit_setting_filepath,'r')
    TransitMazTazFlag=int(next(f).split()[0])
    drivingweight=float(next(f).split()[0])
    walkingweight=float(next(f).split()[0])
    TransitSkimTimeIntervalLength=float(next(f).split()[0])
    MaxNumTransitSkimTimeInterval=int(int(next(f).split()[0]))
    WalkSpeed=float(next(f).split()[0])
    return TransitMazTazFlag,drivingweight,walkingweight,TransitSkimTimeIntervalLength,MaxNumTransitSkimTimeInterval,WalkSpeed
def read_pnr_zones(three_link_pnr):
    '''
    This function is called by function read_transitskim
    This function return the transit_zone_candidates where the first column is the zones in the 
    DYNSMART network and the second column is the transit zone in the three-link zone, which is
    indicated by the first column
    '''
    from collections import defaultdict
    transit_zone_candidates=defaultdict(list)
    for line in three_link_pnr:
        if 'tran' not in line:
            transit_zone_candidates[int(line.split()[0])].extend([int(line.split()[1])])
    return transit_zone_candidates
def read_threelink_walk(three_link_walk_file):
    three_link_walk_list=[]
    for line in three_link_walk_file:
        if 'tran' not in line: 
            three_link_walk_list.append([float(i) for i in line.split()])
    
    three_link_walk=pd.DataFrame(data=three_link_walk_list,columns=['three_link_zone','transit_zone','distance'])
    return three_link_walk
def read_transitskim(transit_skim_folderpath):
    '''
    This function call the read_transit_setting function and read_pnr_zones function
    This function returns all the NUTRANS realated parameters such as:TransitMazTazFlag,drivingweight,walkingweight,TransitSkimTimeIntervalLength,MaxNumTransitSkimTimeInterval,WalkSpeed
    This function returns the transit zone associated with each DYNSAMART zone in transit_zone_candidates
    This function also returns the transit AB cost skim which is the estimated travel time between zones at each time interval
    
    '''
    #First read the transit setting file
    transit_setting_filepath=transit_skim_folderpath+'TransitSetting.dat'
    TransitMazTazFlag,drivingweight,walkingweight,TransitSkimTimeIntervalLength,MaxNumTransitSkimTimeInterval,WalkSpeed=read_transit_setting(transit_setting_filepath)
    #Next depends on the setting file determine whether the file is maz based or taz based
    ab_time_f=open(transit_skim_folderpath+'TransitPNRSkim_AB_time.dat','r')
    if TransitMazTazFlag==0:
        ab_cost_f=open(transit_skim_folderpath+'TransitPNRSkim_AB_cost.dat','r')
        ba_time_f=open(transit_skim_folderpath+'TransitPNRSkim_BA_time.dat','r')
        ba_cost_f=open(transit_skim_folderpath+'TransitPNRSkim_BA_cost.dat','r')
        three_link_pnr=open(transit_skim_folderpath+'input_threelink_PNR.dat','r')
        three_link_walk_file=open(transit_skim_folderpath+'input_threelink_walk.dat','r')
    else:
        ab_cost_f=open(transit_skim_folderpath+'TransitPNRSkim_AB_cost_TAZ.dat','r')
        ba_time_f=open(transit_skim_folderpath+'TransitPNRSkim_BA_time_TAZ.dat','r')
        ba_cost_f=open(transit_skim_folderpath+'TransitPNRSkim_BA_cost_TAZ.dat','r')
        three_link_pnr=open(transit_skim_folderpath+'input_threelink_PNR_TAZ.dat','r')
        three_link_walk_file=open(transit_skim_folderpath+'input_threelink_walk_TAZ.dat','r')
    transit_zone_candidates=read_pnr_zones(three_link_pnr)
    three_link_walk=read_threelink_walk(three_link_walk_file)
    cost_temp=[]
    for line in ab_cost_f:
        cost_temp.append([float(i) for i in line.split()])
    time_temp=[]
    for line in ab_time_f:
        time_temp.append([float(i) for i in line.split()])
    column_name=['otap','dtap']
    column_name.extend([i for i in range(MaxNumTransitSkimTimeInterval)])
    Transit_AB_Cost_Skim=pd.DataFrame(data=cost_temp,columns=column_name)
    Transit_AB_Time_Skim=pd.DataFrame(data=time_temp,columns=column_name)
    return TransitMazTazFlag,drivingweight,walkingweight,TransitSkimTimeIntervalLength,MaxNumTransitSkimTimeInterval,WalkSpeed,transit_zone_candidates,Transit_AB_Cost_Skim,Transit_AB_Time_Skim,three_link_walk


def read_superzone_info(superzone_filepath):
    i=0
    origin_flag=0
    super_zone_flag=0
    origin_zone=[]
    superzone=[]
    with open(superzone_filepath) as f: 
        for line in f: 
            if i==2: 
                origin_flag=1
            if line.split()[0]=='Mapping':
                origin_flag=0
                super_zone_flag=1
            else:
                if origin_flag==1:
                    origin_zone.extend([int(j) for j in line.split()])
                elif super_zone_flag==1:
                    superzone.extend([int(j) for j in line.split()])
            i=i+1
    superzone_info=dict(zip(origin_zone,superzone))
    return superzone_info
def read_link_travel_time(link_travel_time_filepath):
    i=0
    with open(link_travel_time_filepath) as f:
        for line in f:
            i=i+1
            print(i,len(line),line,line==' ')
            if i ==6:
                z=line
    return z
def read_turn_penalty(turn_penalty_filepath):
    with open(turn_penalty_filepath) as f:
        for line in f:
            print(line)
    return 

# In[ ]:

def cluster_highlight(linklist):
    #This function will highlight the linklist in a given network
    
    Gnormal=nx.Graph()
    Gred=nx.Graph()
    for nodeinf in node_detail:
        node=nodeinf[0,0]
        Gnormal.add_node(node,pos=(nodexy[node][0],nodexy[node][1]))
        Gred.add_node(node,pos=(nodexy[node][0],nodexy[node][1]))
    counter=0
    for linkinf in link_detail: 
        if counter in linklist:
            Gred.add_edge(linkinf[0,0],linkinf[0,1])
        else:
            Gnormal.add_edge(linkinf[0,0],linkinf[0,1])
        counter=counter+1
            
    pos=nx.get_node_attributes(Gnormal,'pos')
    nx.draw(Gnormal,pos,node_size=1,node_color='black')
    nx.draw(Gred,pos,edge_color='r',node_color='black',width=4,node_size=4)
    #nx.draw_networkx_nodes(G,pos,node_size=50)
    return


# In[ ]:

def convert_travelerdat(input_path,output_path):
    file=open(input_path+'/traveler.dat')
    num_lines = sum(1 for line in file)
    file.close()
    file=open(input_path+'/traveler.dat')
    linecounter=0
    j=0
    traveler_flag=0
    print('Extract information from traveler.dat')
    for line in file:
        if linecounter%500==0: 
                print(linecounter,time.time())
        line_temp=line.split()
        if linecounter==0:
            num_traveler=int(line_temp[0])
            max_num_trips=line_temp[1]
            traveler_flag=1
            traveler_info=pd.DataFrame(index=range(num_lines-num_traveler-3),columns=('person_id','num_trips','value_of_time','trip_counter',
                                    'ActivityTime','tripmode','orig_purpose','dest_purpose', 
                                     'orig_maz',  'orig_taz','dest_maz','dest_taz','driver_passenger_flag','joint_trip_flag','park&ride_flag',
                                    'starttimeinterval','starttime'))
        elif linecounter!=1 and linecounter!=2:
            if traveler_flag==1:
                #read the traveler information
                traveler_id=line_temp[2]
                num_trips=int(line_temp[4])
                value_of_time=float(line_temp[9])
                traveler_counter=int(line_temp[0])-1
                trip_temp=[]
                trip_counter=1
                traveler_flag=0
            else: 
                #trip_temp=line_temp[1:13] #[line_temp[i] for i in [1,2,3,4,5,6,7,8,9,10,11,12,13,15]]
                trip_info=[traveler_id,num_trips,value_of_time,trip_counter]
                trip_info.extend(line_temp[1:14])
                traveler_info.loc[j] = trip_info
                j+=1
                if trip_counter==num_trips:
                    traveler_flag=1
                trip_counter+=1
        linecounter=linecounter+1
    #Add the household information to the dataframe
    print('Add household information')
    traveler_info.to_csv('input/traveler_info.dat',index=False)
    traveler_info=pd.read_csv('input/traveler_info.dat')
    personData=pd.read_csv('input/personData_1.csv')
    hh_temp=personData[['person_id','hh_id']]
    traveler_infos=pd.merge(traveler_info,hh_temp,how='left',on=['person_id'])
    print('Generate the origin and destination node for each trip')
    #read the origin, destination information and add them to the traveler_trip
    origins, destinations=read_origin_destion(input_path+'/origin.dat',input_path+'/destination.dat')
    traveler_trips=add_od_node_all_travelers(traveler_infos)
    traveler_trips.to_csv(input_path+'/traveler_trip_info.csv',index=False)
    return

#Find the origin and destination node of one trip
def find_orign_destination_node(origin_zone,destination_zone,origins,destinations):
    '''
        input: 
            the origin zone number and destination zone number
            the global variable destinations and origins which are includes
                the destination and origin nodes from each zone
        output:
            origin_node: the node where the trip starts
            destination_node: the node where the trip ends
    '''
    #Find the origin node where the trip starts
    num_or_candidates=len(origins[origins['zone_id']==origin_zone].index)
    if num_or_candidates<=1:
        print('Number of candidate is zero',num_or_candidates,origin_zone)
    origin_node=origins[origins['zone_id']==origin_zone].iloc[random.randint(0, num_or_candidates-1)]['nodes']
    #Find the destination node where the trip ends
    num_des_candidates=len(destinations[destinations['zone_id']==destination_zone].index)
    destination_node=destinations[destinations['zone_id']==destination_zone].iloc[random.randint(0, num_des_candidates-1)]['nodes']
    return origin_node,destination_node

def add_od_node_all_travelers(traveler_trips):
    '''
    This function add two columns origin_nodes and destination_nodes to the traveler_trip dataframe
    '''
    origin_node=[]
    destination_node=[]
    for index, row in traveler_trips.iterrows():
        # print(row['orig_taz'],row['dest_taz'])
        or_temp,dest_temp=find_orign_destination_node(row['orig_taz'],row['dest_taz'])
        origin_node.extend([or_temp])
        destination_node.extend([dest_temp])
    a= np.asarray(origin_node)
    b= np.asarray(destination_node)
    traveler_trips['origin_node']=a
    traveler_trips['destination_node']=b
    return traveler_trips

def distance_between_link(link_ID1,link_ID2,link_xy):

    distance=math.sqrt((link_xy[link_ID1][0]-link_xy[link_ID2][0])**2+(link_xy[link_ID1][1]-link_xy[link_ID2][1])**2)
    return distance
def c_link_xy (links):
    global link_xy
    link_xy=[]
    for link_ID1 in links: 
        link_xy.append([(nodexy[link_detail[link_ID1,0]][0]+nodexy[link_detail[link_ID1,1]][0])/2,
                       (nodexy[link_detail[link_ID1,0]][1]+nodexy[link_detail[link_ID1,1]][1])/2])
    return 

def distance_between_nodes(node_ID1,node_ID2,node_xy):
    lattomile=69
    longtomile=53
    distance=math.sqrt(((node_xy[node_ID1][0]-node_xy[node_ID2][0])*lattomile)**2+((node_xy[node_ID1][1]-node_xy[node_ID2][1])*longtomile)**2)
    if node_ID1==node_ID2: 
        distance=0.01
    return distance
def travel_time_between_nodes(node_ID1,node_ID2,node_xy):
    '''
    node_ID1: the node id of upstream node
    node_ID2: the node id of downstream node
    node_xy: the latitude and longtitude of node
    '''
    origin_lat_long=str(-nodexy[node_ID1][1])+","+str(nodexy[node_ID1][0])
    destination_lat_long=str(-nodexy[node_ID2][1])+","+str(nodexy[node_ID2][0])
    url="https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins="+origin_lat_long+"&destinations="+destination_lat_long+"&key=AIzaSyBw112JGqCiFB4jF_1Sc0iH7mXCIzRXlI8"
    z=requests.get(url)
    travel_time=z.json()['rows'][0]['elements'][0]['duration']['value']/60
    return travel_time
