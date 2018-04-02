import numpy as np
import pandas as pd
import math
import matplotlib as plt
from Modules import DYNASMART_Process as dy
# import preprocessing_read_skim_file as rs
from Modules import AV_functions as av
from Modules import Preprocess_DARP as prd

from gurobipy import *
import datetime

def analysis_result(darp_solution,sorted_trips,Vehicular_Skim,superzone_map):
    route_info=darp_solution['route_info']
    schedule_deviation =darp_solution['schedule_deviation']
    drivingcost_per_mile=darp_solution['drivingcost_per_mile']
    num_cav=darp_solution['num_cav']
    darp_analyzed_result={}
    darp_analyzed_result['num_cav_trips']=len(route_info)
    darp_analyzed_result['num_occupied_trips']=len(route_info.loc[route_info.person_id>0])
    darp_analyzed_result['num_unoccupied_trips']=len(route_info.loc[route_info.person_id==0])
    darp_analyzed_result['num_pickup_trips']=len(route_info.loc[route_info.orig_node_index<=len(sorted_trips)])
    darp_analyzed_result['num_shared_trips']=len(route_info.loc[(route_info.orig_node_index<=len(sorted_trips)) &(route_info.dest_node_index<=len(sorted_trips))])
    darp_analyzed_result['num_convention car trips']=len(sorted_trips.loc[sorted_trips.tripmode<=6])
    darp_analyzed_result['total_convention_vehicle_driving_time']=sorted_trips.loc[sorted_trips.tripmode<=6].apply(lambda row: prd.estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,Vehicular_Skim,2,superzone_map,drivingcost_per_mile),axis=1).sum()/60
    darp_analyzed_result['total_AV_driving_time']=route_info.apply(lambda row: prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim,2,superzone_map,drivingcost_per_mile),axis=1).sum()/60
    darp_analyzed_result['total_AV_unoccupied_driving_time']=route_info.loc[route_info.person_id==0].apply(lambda row: prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim,2,superzone_map,drivingcost_per_mile),axis=1).sum()/60
    darp_analyzed_result['total_convention_vehicle_driving_distance']=sorted_trips.loc[sorted_trips.tripmode<=6].apply(lambda row: prd.estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,Vehicular_Skim,1,superzone_map,drivingcost_per_mile),axis=1).sum()
    darp_analyzed_result['total_AV_driving_distance']=route_info.apply(lambda row:prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim,1,superzone_map,drivingcost_per_mile) ,axis=1).sum()
    darp_analyzed_result['total_AV_unoccupied_driving_distance']=route_info.loc[route_info.person_id==0].apply(lambda row:prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim,1,superzone_map,drivingcost_per_mile),axis=1).sum()
    darp_analyzed_result['num_delayed_trips']=sum(1 for i in schedule_deviation if i >1)
    darp_analyzed_result['num_early_trips']=sum(1 for i in schedule_deviation if i <-1)
    darp_analyzed_result['total_delayed_time']=round(sum(i for i in schedule_deviation if i >0),3)
    darp_analyzed_result['Total_early_time']=round(sum(i for i in schedule_deviation if i <0),3)
    darp_analyzed_result['total_reward']=darp_solution['total_reward']
    darp_analyzed_result['total_schedule_penalty']=darp_solution['total_schedule_penalty']
    darp_analyzed_result['total_travel_cost']=darp_solution['total_travel_cost']
    darp_analyzed_result['reward_mode']=darp_solution['reward_mode']
    darp_analyzed_result['drivingcost_per_mile']=darp_solution['drivingcost_per_mile']
    darp_analyzed_result['share_ride_factor']=darp_solution['share_ride_factor']
    darp_analyzed_result['run_mode']=darp_solution['run_mode']
    darp_analyzed_result['num_cav']=darp_solution['num_cav']
    darp_analyzed_result['cav_use_mode']=darp_solution['cav_use_mode']
    darp_analyzed_result['time_window_flag']=darp_solution['time_window_flag']

    # print('Number of CAV Trips',darp_analyzed_result['num_cav_trips'])
    # print('Number of Occupied trips',darp_analyzed_result['num_occupied_trips'])
    # print('Number of unoccupied trips',darp_analyzed_result['num_unoccupied_trips'])
    # print('Number of pickup trips',darp_analyzed_result['num_pickup_trips'])
    # print('Number of shared trips',darp_analyzed_result['num_shared_trips'])
    # print('Number of convention car trips',darp_analyzed_result['num_convention car trips'])
    # print('Convention car trips VHT: \t',darp_analyzed_result['total_convention_vehicle_driving_time'],
    #       '\nAV driving time: \t',darp_analyzed_result['total_AV_driving_time'],
    #       '\nAV unoccupied driving_time: \t',darp_analyzed_result['total_AV_unoccupied_driving_time'])
    # # Compare the VMT between convention vehicle and CAV
    
    # print('Convention VMT: \t',darp_analyzed_result['total_convention_vehicle_driving_distance'],
    #       '\nAV driving distance: \t',darp_analyzed_result['total_AV_driving_distance'],
    #       '\nAV unoccupied driving_distance: \t',darp_analyzed_result['total_AV_unoccupied_driving_distance'])
    # print('Number of delayed trips\t',darp_analyzed_result['num_delayed_trips'])
    # print('Number of early trips\t',darp_analyzed_result['num_early_trips'])
    # print('Total delayed time\t',darp_analyzed_result['total_delayed_time'])
    # print('Total_early_time\t',darp_analyzed_result['Total_early_time'])
    # plt.pyplot.figure(1,figsize=[2.3,10])
    plot_route_info_schedule(route_info,sorted_trips,num_cav)
    return darp_analyzed_result

def plot_route_info_schedule(route_info,sorted_trips,num_cav):
#     z=traveler_trips[traveler_trips.hh_id==route_info.hh_id[0]]
    plt.pyplot.figure(1,figsize=[2.3,10])
    sorted_trips.loc[:,'p_id']=sorted_trips.groupby(['person_id']).ngroup()
    hh_num_trips=len(sorted_trips)
    person_id_and_inhouse_p_id_map=dict(zip(sorted_trips.person_id,sorted_trips.p_id))
    person_id_and_inhouse_p_id_map[0]=-1
    route_info['p_id']=route_info.person_id.apply(lambda x: person_id_and_inhouse_p_id_map[x])
    
    plt.pyplot.scatter(route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips),'p_id'],
                route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips),'origin_arrival_time'])
    # color=iter(plt.pyplot.cm.rainbow(np.linspace(0,1,num_cav)))

    for ve in range(num_cav):
        # c=next(color)
        # print(ve)
        # print(route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) & (route_info.hh_vehicle_id==ve),'origin_arrival_time'])
        plt.pyplot.plot(route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) & (route_info.hh_vehicle_id==ve),'p_id'],
                        route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) & (route_info.hh_vehicle_id==ve),'origin_arrival_time'])
#     plt.pyplot.scatter(route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) & (route_info.dest_node_index<1+hh_num_trips),'p_id'],
#                     route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) &(route_info.dest_node_index<1+hh_num_trips),'origin_arrival_time'])
    plt.pyplot.xlabel('Traveler Index')
    plt.pyplot.ylabel('Time')
    plt.pyplot.grid()
    plt.pyplot.xticks(np.arange(min(sorted_trips.p_id), max(sorted_trips.p_id)+1, 1))
    plt.pyplot.yticks(np.arange(0,1441,30))
#     plt.pyplot.title(title)
    return 

