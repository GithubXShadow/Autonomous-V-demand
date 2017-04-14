
# coding: utf-8

# In[1]:

import numpy as np
import pandas as pd
import matplotlib as plt
import networkx as nx
get_ipython().magic('matplotlib inline')
from DYNASMART_Process import *
import DYNASMART_Process as dy


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

def trip_chain_highlight(trip_chain_or,node_detail,link_detail,nodexy):
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
    for index,row in trip_chain_or.iterrows():
        Gred.add_edge(row['origin_node'],row['destination_node'])
    pos=nx.get_node_attributes(Gnormal,'pos')
    nx.draw(Gnormal,pos,node_size=1,node_color='black',dpi=900)
    nx.draw(Gred,pos,edge_color='r',node_color='black',node_size=4,arrows=True,style='dotted',dpi=900)
    return   
