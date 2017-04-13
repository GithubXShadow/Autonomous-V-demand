
import numpy as np

import pandas as pd
import matplotlib as plt
import networkx as nx

import math
import random

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
            link_id=np.zeros((num_nodes,num_nodes))
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
            link_id[node_id[line_list_temp[0]],node_id[line_list_temp[1]]]=i-num_nodes-1
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
    return origins, destinations


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
    normal_link=[]
    for linkinf in link_detail: 
        if counter in linklist:
            Gred.add_edge(linkinf[0,0],linkinf[0,1])
        else:
            normal_link.append((linkinf[0,0],linkinf[0,1]))
            Gnormal.add_edge(linkinf[0,0],linkinf[0,1])
        counter=counter+1
            
    pos=nx.get_node_attributes(Gnormal,'pos')
    nx.draw(Gnormal,pos,node_size=1,node_color='black')
    nx.draw(Gred,pos,edge_color='r',node_color='black',width=4,node_size=4)
    #nx.draw_networkx_nodes(G,pos,node_size=50)
    return


# In[ ]:

def convert_travelerdat(input_path,output_path):
    #global num_traveler,max_num_trips,traveler_info
    file=open(path)
    num_lines = sum(1 for line in file)
    file.close()
    file=open(path)
    linecounter=0
    j=0
    traveler_flag=0
    for line in file:
        line_temp=line.split()
        if linecounter==0:
            num_traveler=int(line_temp[0])
            max_num_trips=line_temp[1]
            traveler_flag=1
            traveler_info=pd.DataFrame(index=range(num_lines-num_traveler),columns=('person_id','num_trips','value_of_time','trip_counter',
                                    'ActivityTime','tripmode','orig_purpose','dest_purpose', 
                                     'orig_maz',  'orig_taz','dest_maz','dest_taz',
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
                trip_temp=[line_temp[i] for i in [1,2,3,4,5,6,7,8,12,13]]
                trip_info=[traveler_id,num_trips,value_of_time,trip_counter]
                trip_info.extend(trip_temp)
                traveler_info.loc[j] = trip_info
                j+=1
                if trip_counter==num_trips:
                    traveler_flag=1
                trip_counter+=1
        linecounter=linecounter+1
        traveler_info.to_csv(input_path,output_path)
    return

