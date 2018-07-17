
# coding: utf-8

# In[1]:

import numpy as np
import pandas as pd
import matplotlib as plt
import networkx as nx
# get_ipython().magic('matplotlib inline')
from Modules import *
from Modules import DYNASMART_Process as dy


# In[ ]:

#Find the origin and destination node of one trip
def find_orign_destination_node(origin_zone,destination_zone):
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
        print(num_or_candidates,origin_zone)
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
        or_temp,dest_temp=find_orign_destination_node(row['orig_taz'],row['dest_taz'])
        origin_node.extend([or_temp])
        destination_node.extend([dest_temp])
    a= np.asarray(origin_node)
    b= np.asarray(destination_node)
    traveler_trips['origin_node']=a
    traveler_trips['destination_node']=b
    return traveler_trips

def trip_chain_highlight(origin_nodes,destination_nodes,node_detail,link_detail,nodexy):
    '''
        input trip_chain_or: a dataframe contains the origin and destination nodes of all the trips in one trip chain
    '''
    plt.pyplot.figure()
    Gnormal=nx.Graph()
    Gred=nx.Graph()
    for nodeinf in node_detail:
        node=nodeinf[0,0]
        Gnormal.add_node(node,pos=(nodexy[node][0],nodexy[node][1]))
        Gred.add_node(node,pos=(nodexy[node][0],nodexy[node][1]))
    for linkinf in link_detail: 
        Gnormal.add_edge(linkinf[0,0],linkinf[0,1])

    for orig_node,dest_node in zip(origin_nodes,destination_nodes):
        Gred.add_edge(orig_node,dest_node)
    pos=nx.get_node_attributes(Gnormal,'pos')
    nx.draw(Gnormal,pos,node_size=1,node_color='black',dpi=900)             
    nx.draw(Gred,pos,edge_color='r',node_color='black',node_size=4,arrows=True,style='-',dpi=900,width=3)
    return  

def zone_node_highlight(target_node_list,node_detail,link_detail,nodexy,target_node_color):
    '''
        input trip_chain_or: a dataframe contains the origin and destination nodes of all the trips in one trip chain
    '''
    # target_zone_list.apply(lambda row: dy.find_orign_destination_node(row['origin_zone'],row[''])) 

    plt.pyplot.figure()
    Gnormal=nx.Graph()
    Gred=nx.Graph()
    for nodeinf in node_detail:
        node=nodeinf[0,0]
        Gnormal.add_node(node,pos=(nodexy[node][0],nodexy[node][1]))
        
    for linkinf in link_detail: 
        Gnormal.add_edge(linkinf[0,0],linkinf[0,1])
    for target_node in target_node_list:
        Gred.add_node(target_node,pos=(nodexy[target_node][0],nodexy[target_node][1]))
    for i in range(len(target_node_list)-1):
        Gred.add_edge(target_node_list[i],target_node_list[i+1])
    pos=nx.get_node_attributes(Gnormal,'pos')
    nx.draw(Gnormal,pos,node_size=1,node_color='black',dpi=900)             
    nx.draw(Gred,pos,edge_color='black',node_color=target_node_color,node_size=40,arrows=True,style='dotted',dpi=900)

    return  

def calculate_node_distance_matrix(visit_candidate,nodexy,C):
    for (orig_node,i) in zip(visit_candidate,range(len(visit_candidate))):
        for (dest_node,j) in zip(visit_candidate,range(len(visit_candidate))):
            if i!=j:
                C[i,j]=dy.distance_between_nodes(orig_node,dest_node,nodexy)
            else:
                C[i,j]=0
    return C

def calculate_node_travel_time_matrix(visit_candidate,nodexy):
    C=np.ones((len(visit_candidate),len(visit_candidate)))
    for (orig_node,i) in zip(visit_candidate,range(len(visit_candidate))):
        for (dest_node,j) in zip(visit_candidate,range(len(visit_candidate))):
            if i<j:
                C[i,j]=dy.travel_time_between_nodes(orig_node,dest_node,nodexy)
                C[j,i]=C[i,j]
            #elif i==j:
             #   C[i,j]=2000
    return C
    
