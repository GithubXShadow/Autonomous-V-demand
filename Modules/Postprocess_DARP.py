import numpy as np
import pandas as pd
import math
import matplotlib as plt
import pickle
from Modules import DYNASMART_Process as dy
# import preprocessing_read_skim_file as rs
from Modules import AV_functions as av
from Modules import Preprocess_DARP as prd

from gurobipy import *
import datetime

def analysis_result(darp_solution,sorted_trips,Vehicular_Skim_Dict,superzone_map):
    print(len(sorted_trips))

    route_info=darp_solution['route_info']
    schedule_deviation =darp_solution['schedule_deviation']
    drivingcost_per_mile=darp_solution['drivingcost_per_mile']
    num_cav=darp_solution['num_cav']
    darp_analyzed_result={}
    darp_analyzed_result['num_cav_trips']=len(route_info)
    darp_analyzed_result['num_occupied_trips']=len(route_info.loc[route_info.person_id>0])
    darp_analyzed_result['num_unoccupied_trips']=len(route_info.loc[route_info.person_id==0])
    darp_analyzed_result['num_pickup_trips']=len(route_info.loc[route_info.orig_node_index<=len(sorted_trips)])

    darp_analyzed_result['num_shared_trips']=\
    len(route_info.loc[(route_info.orig_node_index<=len(sorted_trips)) &(route_info.dest_node_index<=len(sorted_trips))])

    darp_analyzed_result['num_convention car trips']=len(sorted_trips.loc[sorted_trips.tripmode<=6])

    darp_analyzed_result['total_convention_vehicle_driving_time']=\
    sorted_trips.loc[sorted_trips.tripmode<=6].apply(lambda row: 
        prd.estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,
            Vehicular_Skim_Dict,2,superzone_map,drivingcost_per_mile),axis=1).sum()/60

    darp_analyzed_result['total_AV_driving_time']=route_info.apply(lambda row: 
        prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,
            Vehicular_Skim_Dict,2,superzone_map,drivingcost_per_mile),axis=1).sum()/60

    darp_analyzed_result['total_AV_unoccupied_driving_time']=route_info.loc[route_info.person_id==0].apply(
        lambda row: prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim_Dict,2,
            superzone_map,drivingcost_per_mile),axis=1).sum()/60

    darp_analyzed_result['total_convention_vehicle_driving_distance']=sorted_trips.loc[sorted_trips.tripmode<=6].apply(
        lambda row: prd.estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,Vehicular_Skim_Dict,1,
            superzone_map,drivingcost_per_mile),axis=1).sum()

    darp_analyzed_result['total_AV_driving_distance']=route_info.apply(
        lambda row:prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim_Dict,1,
            superzone_map,drivingcost_per_mile) ,axis=1).sum()

    darp_analyzed_result['total_AV_unoccupied_driving_distance']=route_info.loc[route_info.person_id==0].apply(
        lambda row:prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim_Dict,1,
            superzone_map,drivingcost_per_mile),axis=1).sum()

    # darp_analyzed_result['num_delayed_trips']=sum(1 for i in schedule_deviation if i >1)
    # darp_analyzed_result['num_early_trips']=sum(1 for i in schedule_deviation if i <-1)
    # darp_analyzed_result['total_delayed_time']=round(sum(i for i in schedule_deviation if i >0),3)
    # darp_analyzed_result['Total_early_time']=round(sum(i for i in schedule_deviation if i <0),3)

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
    darp_analyzed_result['total_travel_cost']=darp_solution['total_travel_cost']
    # darp_analyzed_result['total_delayed_time']=darp_solution['total_delayed_time']
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
    # plot_route_info_schedule(route_info,sorted_trips,num_cav)
    return darp_analyzed_result

def plot_route_info_schedule(route_info,sorted_trips,num_cav):
#     z=traveler_trips[traveler_trips.hh_id==route_info.hh_id[0]]
    plt.pyplot.figure(1,figsize=[2.3,5])
    sorted_trips.loc[:,'p_id']=sorted_trips.groupby(['person_id']).ngroup()
    # print(sorted_trips.loc[:,'p_id'].unique())
    hh_num_trips=len(sorted_trips)
    person_id_and_inhouse_p_id_map=dict(zip(sorted_trips.person_id,sorted_trips.p_id))
    person_id_and_inhouse_p_id_map[0]=-1
    route_info['p_id']=route_info.person_id.apply(lambda x: person_id_and_inhouse_p_id_map[x])
    route_info.sort_values(by=['origin_arrival_time'],inplace=True)
    plt.pyplot.scatter(route_info.loc[(route_info.p_id!=-1) & 
                    (route_info.orig_node_index<1+hh_num_trips),'p_id'],
                    route_info.loc[(route_info.p_id!=-1) & 
                    (route_info.orig_node_index<1+hh_num_trips),'origin_arrival_time'],label='_nolegend')
    # color=iter(plt.pyplot.cm.rainbow(np.linspace(0,1,num_cav)))
    for ve in range(num_cav):
    # ve=1
        line_label='AV '+str(ve+1)
        plt.pyplot.plot(route_info.loc[(route_info.p_id!=-1) &
                    (route_info.orig_node_index<1+hh_num_trips) &
                    (route_info.hh_vehicle_id==ve),'p_id'],
                    route_info.loc[(route_info.p_id!=-1) & 
                    (route_info.orig_node_index<1+hh_num_trips) &
                    (route_info.hh_vehicle_id==ve),'origin_arrival_time'],
                    label=line_label)
#     plt.pyplot.scatter(route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) & (route_info.dest_node_index<1+hh_num_trips),'p_id'],
#                     route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) &(route_info.dest_node_index<1+hh_num_trips),'origin_arrival_time'])
    plt.pyplot.xlabel('Traveler Index')
    plt.pyplot.ylabel('Time')
    plt.pyplot.legend()
    plt.pyplot.grid()
    plt.pyplot.xticks(np.arange(min(sorted_trips.p_id), max(sorted_trips.p_id)+1, 1))
    plt.pyplot.yticks(np.arange(0,1441,120))
#     plt.pyplot.title(title)
    return 

def plot_household_schedule(sorted_trips):
#     z=traveler_trips[traveler_trips.hh_id==route_info.hh_id[0]]
    plt.pyplot.figure(1,figsize=[2.3,5])
    sorted_trips.loc[:,'p_id']=sorted_trips.groupby(['person_id']).ngroup()
    # print(sorted_trips.loc[:,'p_id'].unique())
    hh_num_trips=len(sorted_trips)
    # person_id_and_inhouse_p_id_map=dict(zip(sorted_trips.person_id,sorted_trips.p_id))
    # person_id_and_inhouse_p_id_map[0]=-1
    # route_info['p_id']=route_info.person_id.apply(lambda x: person_id_and_inhouse_p_id_map[x])
    # route_info.sort_values(by=['origin_arrival_time'],inplace=True)
    # plt.pyplot.scatter(route_info.loc[(route_info.p_id!=-1) & 
    #                 (route_info.orig_node_index<1+hh_num_trips),'p_id'],
    #                 route_info.loc[(route_info.p_id!=-1) & 
    #                 (route_info.orig_node_index<1+hh_num_trips),'origin_arrival_time'],label='_nolegend')
    plt.pyplot.scatter(sorted_trips['p_id'],sorted_trips['starttime'])
    # color=iter(plt.pyplot.cm.rainbow(np.linspace(0,1,num_cav)))
    
    # for ve in range(num_cav):
    # # ve=1
    #     line_label='AV '+str(ve+1)
    #     plt.pyplot.plot(route_info.loc[(route_info.p_id!=-1) &
    #                 (route_info.orig_node_index<1+hh_num_trips) &
    #                 (route_info.hh_vehicle_id==ve),'p_id'],
    #                 route_info.loc[(route_info.p_id!=-1) & 
    #                 (route_info.orig_node_index<1+hh_num_trips) &
    #                 (route_info.hh_vehicle_id==ve),'origin_arrival_time'],
    #                 label=line_label)

#     plt.pyplot.scatter(route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) & (route_info.dest_node_index<1+hh_num_trips),'p_id'],
#                     route_info.loc[(route_info.p_id!=-1) & (route_info.orig_node_index<1+hh_num_trips) &(route_info.dest_node_index<1+hh_num_trips),'origin_arrival_time'])
    plt.pyplot.xlabel('Traveler Index')
    plt.pyplot.ylabel('Time')
    # plt.pyplot.legend()
    plt.pyplot.grid()
    plt.pyplot.xticks(np.arange(min(sorted_trips.p_id), max(sorted_trips.p_id)+1, 1))
    plt.pyplot.yticks(np.arange(0,1441,120))
#     plt.pyplot.title(title)
    return 

def save_obj(obj, name,file_path ):
    with open(file_path+ name + '.pkl', 'wb') as f:
        pickle.dump(obj, f, pickle.HIGHEST_PROTOCOL)

def load_obj(name,file_path ):
    with open(file_path + name + '.pkl', 'rb') as f:
        return pickle.load(f)

def analysis_one_hh_result(target_hh_id,darp_solutions,target_trips,num_cav,Vehicular_Skim_Dict,
    superzone_map):
    for solution in darp_solutions:
        # print(solution['route_info'])
        if not solution['route_info'].hh_id.empty:
            temp=solution['route_info'].hh_id.iloc[0]
            if (temp)==(target_hh_id):
                target_solution=solution
    plot_route_info_schedule(target_solution['route_info'],
                             target_trips[target_trips.hh_id==target_hh_id],num_cav)
    # plot_route_info_schedule(target_solution['route_info'],
    #                          target_trips,num_cav)
    zz=analysis_result(target_solution,
                        target_trips[target_trips.hh_id==target_hh_id],Vehicular_Skim_Dict,superzone_map)
    # zz=analysis_result(target_solution,
    #                     target_trips,Vehicular_Skim_Dict,superzone_map)
    for key,value in zz.items():
        print(key,value)
    return

def analysis_one_hh_result_new_darpsolution(target_hh_id,darp_solutions,target_trips,num_cav,
    Vehicular_Skim_Dict,superzone_map):
    
    target_solution=darp_solutions[target_hh_id]
    plot_route_info_schedule(target_solution['route_info'],
                             target_trips[target_trips.hh_id==target_hh_id],num_cav)
    # plot_route_info_schedule(target_solution['route_info'],
    #                          target_trips,num_cav)
    zz=analysis_result(target_solution,
                        target_trips[target_trips.hh_id==target_hh_id],Vehicular_Skim_Dict,superzone_map)
    # zz=analysis_result(target_solution,
    #                     target_trips,Vehicular_Skim_Dict,superzone_map)
    for key,value in zz.items():
        print(key,value)
    return

def analysis_network_level_results(route_info,darp_solutions,target_trips,Vehicular_Skim_Dict,superzone_map,
    drivingcost_per_mile):
    print('Number of AV Trips: \t',len(route_info))
    print('Number of Shared Rides: \t',route_info.shared_ride_flag.sum())
   
    print('Total VMT: \t',route_info.apply(
        lambda row: prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,
            row.origin_arrival_time,row.value_of_time,Vehicular_Skim_Dict,1,
            superzone_map,drivingcost_per_mile),axis=1).sum())
    print('Number of occupied trips: \t',len(route_info.loc[route_info.person_id>0]))
    print('Number of unoccupied trips: \t',len(route_info.loc[route_info.person_id==0]))
    print('Number of transit trips: \t',len(target_trips)-route_info.pickup_trip_flag.sum())
    # print('Share of transit change',(len(target_trips)-route_info.pickup_trip_flag.sum())/len(target_trips))
    print('total_convention_vehicle_driving_time: \t',
    target_trips.loc[target_trips.tripmode<=6].apply(lambda row: 
        prd.estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,
            Vehicular_Skim_Dict,2,superzone_map,drivingcost_per_mile),axis=1).sum()/60)
    print('total_convention_vehicle_driving_distance: \t',target_trips.loc[target_trips.tripmode<=6].apply(
        lambda row: prd.estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,
            row.value_of_time,Vehicular_Skim_Dict,1,
            superzone_map,drivingcost_per_mile),axis=1).sum())

    print('total_AV_driving_time: \t',route_info.apply(lambda row: 
        prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.origin_arrival_time,
            row.value_of_time,Vehicular_Skim_Dict,2,superzone_map,drivingcost_per_mile),axis=1).sum()/60)

    print('total_AV_unoccupied_driving_time: \t',route_info.loc[route_info.person_id==0].apply(
        lambda row: prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,
            row.value_of_time,Vehicular_Skim_Dict,2,
            superzone_map,drivingcost_per_mile),axis=1).sum()/60)

    print('total_AV_driving_distance: \t',route_info.apply(
        lambda row:prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,
            row.value_of_time,Vehicular_Skim_Dict,1,
            superzone_map,drivingcost_per_mile) ,axis=1).sum())

    print('total_AV_unoccupied_driving_distance: \t',route_info.loc[route_info.person_id==0].apply(
        lambda row:prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,
            row.value_of_time,Vehicular_Skim_Dict,1,
            superzone_map,drivingcost_per_mile),axis=1).sum())
    print('Number of pickup trips: \t',route_info.pickup_trip_flag.sum())
    return

def determin_number_car(darp_solution1,darp_solution2,route_info1,route_info2,car_operating_cost,target_trips):
    household_car_select_list=[[],[]]
    new_darp_solution={}
    z=[[],[],[]]
    for hh_id in target_trips.hh_id.unique():
        # print(darp_solution1[hh_id]['objective_value'],darp_solution1[hh_id]['num_cav'],darp_solution2[hh_id]['objective_value'],darp_solution2[hh_id]['num_cav'])
        z[2].extend([darp_solution1[hh_id]['objective_value']-darp_solution2[hh_id]['objective_value']])
        if(abs(darp_solution1[hh_id]['total_reward'] -darp_solution2[hh_id]['total_reward'])>2):
            print(hh_id)
        if (darp_solution1[hh_id]['objective_value']+darp_solution1[hh_id]['num_cav']*car_operating_cost\
            <(darp_solution2[hh_id]['objective_value']+darp_solution2[hh_id]['num_cav']*car_operating_cost)):

            z[0].extend([hh_id])
            new_darp_solution[hh_id]=darp_solution1[hh_id]
        else: 
            z[1].extend([hh_id])
            new_darp_solution[hh_id]=darp_solution2[hh_id]

    new_route_info=route_info1[route_info1.hh_id.isin(z[0])]
    new_route_info=new_route_info.append(route_info2[route_info2.hh_id.isin(z[1])])


    return new_route_info,new_darp_solution,z

def plot_numtrips_numcav_relation(target_trips,hh_car_list):
    temp1=target_trips[target_trips.hh_id.isin(hh_car_list[1])]
    temp2=target_trips[target_trips.hh_id.isin(hh_car_list[0])]
    fig1, (ax1, ax2) = plt.pyplot.subplots(nrows=1, ncols=2,figsize=(12, 4))
    ax1.set_xlabel('Number of Trips')
    ax1.set_ylabel('Count')
    ax1.set_title('Households choose 1 AV')
    ax2.set_xlabel('Number of Trips')
    ax2.set_ylabel('Count')
    ax2.set_title('Households choose 2 AV')
    temp2.groupby('hh_id').count().person_id.hist(ax=ax1)
    temp1.groupby('hh_id').count().person_id.hist(ax=ax2)
    return

def transit_trip_inconsistency_analysis(darp_solutions,target_trips):
    hh_id_list=[]
    difference=[]
    num_trips=[]
    num_shared_trips=[]
    hh_size=[]

    for hh_id,group in target_trips.groupby('hh_id'):
        hh_id_list.extend([hh_id])
        difference.extend([len(group[group.actual_mode=='Car'])-darp_solutions[hh_id]['num_pickup_trips']])
        num_trips.extend([len(group)])
        hh_size.extend([len(group.person_id.unique())])
        num_shared_trips.extend([darp_solutions[hh_id]['num_shared_trips']])

    transit_trip_inconsistency=pd.DataFrame({'hh_id':hh_id_list,'difference':difference,
        'num_trips':num_trips,'num_shared_trips':num_shared_trips,'hh_size':hh_size})
    return transit_trip_inconsistency
