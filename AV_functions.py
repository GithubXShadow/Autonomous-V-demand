
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

