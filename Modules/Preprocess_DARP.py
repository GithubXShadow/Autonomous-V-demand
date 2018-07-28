
# coding: utf-8

# In[1]:

import numpy as np
import pandas as pd
import math
import datetime
import random
def get_travel_cost_matrix(sorted_trips,Vehicular_Skim_Dict,superzone_map,drivingcost_per_mile,num_time_interval):
    '''
    This function returns two matrix: 
        C: the cost matrix. Each element is the travel cost between two nodes in the dial-n-ride problem network at a time interval
        TT: the travel time matrix. Each element is the travel time between two nodes in the dial-n-ride problem network at a time interval
    Input:
        Vehicular_Skim_Dict: the skim file from dynasmart
        superzone_map: store the relationship between taz and superzone
        num_time_interval: number of time interval used in the dial-and-ride problem. 
    '''
    if num_time_interval>1:
        num_skim_interval=max(Vehicular_Skim_Dict[1][1])
        # print('check3',datetime.datetime.now())
        hh_num_trips=sorted_trips.shape[0]
        C=np.ones((2*hh_num_trips+2,2*hh_num_trips+2,num_time_interval))
        TT=np.ones((2*hh_num_trips+2,2*hh_num_trips+2,num_time_interval))
        visit_candidate_zone=[sorted_trips['orig_taz'].iloc[0]]
        visit_candidate_zone.extend(sorted_trips['orig_taz'].tolist())
        visit_candidate_zone.extend(sorted_trips['dest_taz'].tolist())
        visit_candidate_zone.extend([sorted_trips['orig_taz'].iloc[0]])
        # print('check4',datetime.datetime.now())
        if num_time_interval==1:
            correlated_skim_time_interval=[math.ceil(sorted_trips.starttime.iloc[0].item()/(1440/num_skim_interval))]
            correlated_skim_time_interval.extend(sorted_trips.starttime.apply(lambda x: math.ceil(x/(1440/num_skim_interval))))
            correlated_skim_time_interval.extend(sorted_trips.starttime.apply(lambda x: math.ceil(x/(1440/num_skim_interval))))
            correlated_skim_time_interval.extend([math.ceil(sorted_trips.starttime.iloc[-1].item()/(1440/num_skim_interval))])
        correlated_vot=[0.16]
        correlated_vot.extend(sorted_trips.value_of_time)
    #     correlated_vot.extend(sorted_trips.value_of_time)
    #     correlated_vot.extend([0.16])
        correlated_vot.extend([0.01]*(hh_num_trips+1))
        # print('check5',datetime.datetime.now())
        for ti in range(num_time_interval):
            # print('check1',ti,datetime.datetime.now())
            
            current_skim_time_interval=math.ceil(1440/num_time_interval*(ti+0.5)/(1440/num_skim_interval))
            # print(ti,num_skim_interval,current_skim_time_interval,1440/num_time_interval*(ti+0.5))
            for i,ozone in zip(range(len(visit_candidate_zone)),visit_candidate_zone):
                
                for j,dzone in zip(range(len(visit_candidate_zone)),visit_candidate_zone):
                    if i!=j:
                        if num_time_interval==1:
                            current_skim_time_interval=correlated_skim_time_interval[i]

                        time_temp=Vehicular_Skim_Dict[ozone][superzone_map[dzone]][current_skim_time_interval][1]['Time']
                        cost_temp=Vehicular_Skim_Dict[ozone][superzone_map[dzone]][current_skim_time_interval][1]['Cost']
                        dist_temp=Vehicular_Skim_Dict[ozone][superzone_map[dzone]][current_skim_time_interval][1]['Dist']
                        C[i,j,ti]=time_temp*correlated_vot[i]+cost_temp+dist_temp*drivingcost_per_mile
                        TT[i,j,ti]=time_temp
                        #The cost and travel time between the destination of ith trip and the origin of i+1th trip of same traveler is zero
                        if j==i-hh_num_trips+1 and i <2*hh_num_trips: 
                            if (sorted_trips.iloc[i-hh_num_trips-1].person_id == sorted_trips.iloc[j-1].person_id):
                                C[i,j,ti]=0
                                TT[i,j,ti]=0
                    else:   
                        C[i,j,ti]=0
                        TT[i,j,ti]=0
                    # print('check2',j,datetime.datetime.now())
        C[0,:,:]=0.01
        TT[0,:,:]=0.01
        C[:,2*hh_num_trips+1,:]=0.01
        TT[:,2*hh_num_trips+1,:]=0.01
    #     for i in range(hh_num_trips): 
    #         TT[i+1,i+hh_num_trips+1]=sorted_trips['travel_time'].iloc[i]
    else:
        num_skim_interval=max(Vehicular_Skim_Dict[1][1])
        hh_num_trips=sorted_trips.shape[0]
        C=np.ones((2*hh_num_trips+2,2*hh_num_trips+2))
        TT=np.ones((2*hh_num_trips+2,2*hh_num_trips+2))
        visit_candidate_zone=[sorted_trips['orig_taz'].iloc[0]]
        visit_candidate_zone.extend(sorted_trips['orig_taz'].tolist())
        visit_candidate_zone.extend(sorted_trips['dest_taz'].tolist())
        visit_candidate_zone.extend([sorted_trips['orig_taz'].iloc[0]])

        correlated_skim_time_interval=[math.ceil(sorted_trips.starttime.iloc[0].item()/num_skim_interval)]
        correlated_skim_time_interval.extend(sorted_trips.starttime.apply(lambda x: math.ceil(x/num_skim_interval)))
        correlated_skim_time_interval.extend(sorted_trips.starttime.apply(lambda x: math.ceil(x/num_skim_interval)))
        correlated_skim_time_interval.extend([math.ceil(sorted_trips.starttime.iloc[-1].item()/num_skim_interval)])
        correlated_vot=[0.16]
        correlated_vot.extend(sorted_trips.value_of_time)
    #     correlated_vot.extend(sorted_trips.value_of_time)
    #     correlated_vot.extend([0.16])
        correlated_vot.extend([0.16]*(hh_num_trips+1))
        for i,ozone in zip(range(len(visit_candidate_zone)),visit_candidate_zone):
            for j,dzone in zip(range(len(visit_candidate_zone)),visit_candidate_zone):
                if i!=j:
                    time_temp=Vehicular_Skim_Dict[ozone][superzone_map[dzone]][correlated_skim_time_interval[i]][1]['Time']
                    cost_temp=Vehicular_Skim_Dict[ozone][superzone_map[dzone]][correlated_skim_time_interval[i]][1]['Cost']
                    dist_temp=Vehicular_Skim_Dict[ozone][superzone_map[dzone]][correlated_skim_time_interval[i]][1]['Dist']

                    C[i,j]=time_temp*correlated_vot[i]+cost_temp+dist_temp*drivingcost_per_mile
                    TT[i,j]=time_temp
                
                    #The cost and travel time between the destination of ith trip and the origin of i+1th trip of same traveler is zero
                    if j==i-hh_num_trips+1 and i <2*hh_num_trips: 
                        if (sorted_trips.iloc[i-hh_num_trips-1].person_id == sorted_trips.iloc[j-1].person_id):
                            C[i,j]=0
                            TT[i,j]=0
                else:   
                    C[i,j]=0
                    TT[i,j]=0

        C[0,:]=0.01
        TT[0,:]=0.01
        C[:,2*hh_num_trips+1]=0.01
        TT[:,2*hh_num_trips+1]=0.01
#     for i in range(hh_num_trips): 
#         TT[i+1,i+hh_num_trips+1]=sorted_trips['travel_time'].iloc[i]
    return C,TT

def estimate_transit_cost(sorted_trips,TransitMazTazFlag,TransitSkimTimeIntervalLength,
    Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,transit_zone_candidates,three_link_walk_dict,transit_zone_dict):
    if TransitMazTazFlag==0:
        orig_column_name='orig_maz'
        dest_column_name='dest_maz'
    else: 
        orig_column_name='orig_taz'
        dest_column_name='dest_taz'
    R=[0]
    random.seed(233)
    for index,row in sorted_trips.iterrows():
        min_transit_gc= estimate_single_transit_trip_cost(row[orig_column_name],row[dest_column_name],
            row.starttime,row.value_of_time,TransitMazTazFlag,three_link_walk_dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,
            TransitSkimTimeIntervalLength,transit_zone_dict,0)
        R.extend([min_transit_gc])
    return R

def estimate_single_transit_trip_cost(origin_zone,dest_zone,trip_start_time,vot,TransitMazTazFlag,three_link_walk_dict,
    Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,TransitSkimTimeIntervalLength,transit_zone_dict,output_flag):
    min_transit_gc=1440
    opt_walk_time=1440
    opt_transit_time=1440
    transit_start_time_interval=math.ceil(trip_start_time/TransitSkimTimeIntervalLength)-1
    
    transit_fare=1.75
    transit_asc=0.5
    WalkSpeed=4.55672
    for otap in  transit_zone_dict[origin_zone]:
        for dtap in transit_zone_dict[dest_zone]: 
            # if (Transit_AB_Time_Skim_Dict.loc[(Transit_AB_Time_Skim_Dict.otap==otap) & (Transit_AB_Time_Skim_Dict.dtap==dtap)].empty) or \
            # (Transit_AB_Time_Skim_Dict.loc[(Transit_AB_Time_Skim_Dict.otap==otap) & (Transit_AB_Time_Skim_Dict.dtap==dtap),int(transit_start_time_interval)].item()==0):
            transit_gc_temp=1440
            opt_walk_time_temp=1440
            opt_transit_time_temp=1440
            
            if (otap in Transit_AB_Time_Skim_Dict):
                if (dtap in Transit_AB_Time_Skim_Dict[otap]):
                    # print(otap,dtap,int(transit_start_time_interval))
                    # print(Transit_AB_Time_Skim_Dict[4][4][0])
                    if (Transit_AB_Time_Skim_Dict[otap][dtap][transit_start_time_interval]!=0):
                        opt_transit_time_temp=Transit_AB_Time_Skim_Dict[otap][dtap][transit_start_time_interval]
                        opt_walk_time_temp=2*three_link_walk_dict[origin_zone][otap]/WalkSpeed/60

                        transit_gc_temp=opt_transit_time_temp*vot*0.9+1.5*vot*opt_walk_time_temp
                        
                        # transit_gc_temp=Transit_AB_Time_Skim_Dict.loc[(Transit_AB_Time_Skim_Dict.otap==otap) 
                        #                                    &(Transit_AB_Time_Skim_Dict.dtap==dtap)
                        #                                   ,int(transit_start_time_interval)].item()*vot +2*three_link_walk_dict.loc[(three_link_walk_dict.three_link_zone==origin_zone) &(three_link_walk_dict.transit_zone==otap),'distance'].item()*1.5*vot/WalkSpeed/60
                
                        if transit_gc_temp==0:
                            transit_gc_temp=1440
                            opt_walk_time_temp=1440
                            opt_transit_time_temp=1440
            if (transit_gc_temp<min_transit_gc):
                min_transit_gc=transit_gc_temp
                otap_candidate=otap
                dtap_candidate=dtap
                opt_walk_time=opt_walk_time_temp
                opt_transit_time=opt_transit_time_temp
                # opt_transit_time=Transit_AB_Time_Skim_Dict[otap][dtap][transit_start_time_interval]
                # opt_walk_time=2*three_link_walk_dict[origin_zone][otap]/WalkSpeed/60
    if output_flag==0:
        return min_transit_gc+transit_fare+transit_asc+np.random.logistic()/0.6
    if output_flag==1:
        return opt_transit_time
    if output_flag==2:
        return opt_walk_time
    if output_flag==4:
        return min_transit_gc+transit_fare+transit_asc,opt_transit_time,opt_walk_time
# def estimate_single_transit_trip_cost(origin_zone,dest_zone,trip_start_time,vot,TransitMazTazFlag,three_link_walk_dict,
# Transit_AB_Cost_Skim_Dict,TransitSkimTimeIntervalLength,output_flag):
#     min_transit_gc=1440
#     transit_gc_temp=1440
#     transit_start_time_interval=math.ceil(trip_start_time/TransitSkimTimeIntervalLength)-1
#     opt_transit_time=0
    
#     for otap in three_link_walk_dict.loc[three_link_walk_dict.three_link_zone==origin_zone].transit_zone:
#         for dtap in three_link_walk_dict.loc[three_link_walk_dict.three_link_zone==dest_zone].transit_zone:
#             if (Transit_AB_Time_Skim_Dict.loc[(Transit_AB_Time_Skim_Dict.otap==otap) & 
#                                         (Transit_AB_Time_Skim_Dict.dtap==dtap)].empty) or (Transit_AB_Time_Skim_Dict.loc[(Transit_AB_Time_Skim_Dict.otap==otap) & (Transit_AB_Time_Skim_Dict.dtap==dtap),int(transit_start_time_interval)].item()==0):
#                 transit_gc_temp=1440
                
#             else:
#                 transit_gc_temp=Transit_AB_Time_Skim_Dict.loc[(Transit_AB_Time_Skim_Dict.otap==otap) 
#                                                            &(Transit_AB_Time_Skim_Dict.dtap==dtap)
#                                                           ,int(transit_start_time_interval)].item()*vot +2*three_link_walk_dict.loc[(three_link_walk_dict.three_link_zone==origin_zone) &(three_link_walk_dict.transit_zone==otap),'distance'].item()*1.5*vot/WalkSpeed/60
                
#                 if transit_gc_temp==0:
#                     transit_gc_temp=1440
#             if (transit_gc_temp<min_transit_gc):
#                 min_transit_gc=transit_gc_temp
#                 otap_candidate=otap
#                 dtap_candidate=dtap
#                 opt_transit_time=Transit_AB_Time_Skim_Dict.loc[(Transit_AB_Cost_Skim_Dict.otap==otap) 
#                                                            &(Transit_AB_Cost_Skim_Dict.dtap==dtap)
#                                                           ,int(transit_start_time_interval)].item()
#                 opt_walk_time=2*three_link_walk_dict.loc[(three_link_walk_dict.three_link_zone==origin_zone) &(three_link_walk_dict.transit_zone==otap),'distance'].item()/WalkSpeed/60
#     if output_flag==0:
#         return min_transit_gc+transit_fare+transit_asc+np.random.logistic()/0.6
#     if output_flag==1:
#         return opt_transit_time
#     if output_flag==2:
#         return opt_walk_time
def estimate_single_car_trip_cost(origin_zone,dest_zone,trip_start_time,
    vot,Vehicular_Skim_Dict,return_flag,superzone_map,drivingcost_per_mile):
    
    num_skim_interval=max(Vehicular_Skim_Dict[1][1])
    correlated_skim_time_interval=math.ceil(trip_start_time/num_skim_interval)

    time_temp=Vehicular_Skim_Dict[origin_zone][superzone_map[dest_zone]][correlated_skim_time_interval][1]['Time']
    cost_temp=Vehicular_Skim_Dict[origin_zone][superzone_map[dest_zone]][correlated_skim_time_interval][1]['Cost']
    dist_temp=Vehicular_Skim_Dict[origin_zone][superzone_map[dest_zone]][correlated_skim_time_interval][1]['Dist']
    car_trip_cost=time_temp*(vot)+cost_temp+dist_temp*drivingcost_per_mile
    if return_flag==0:
        return car_trip_cost
    elif return_flag==1:
        return dist_temp
    elif return_flag==2:
        return time_temp
    elif return_flag==3:
        return cost_temp
    
def compare_mode_utlity(sorted_trips,TransitMazTazFlag,three_link_walk_dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,TransitSkimTimeIntervalLength,
    Vehicular_Skim_Dict,superzone_map,drivingcost_per_mile,transit_zone_dict):
    np.random.seed(123)
    # sorted_trips['transit_time']=sorted_trips.apply(lambda row: 
    #     estimate_single_transit_trip_cost(row.orig_maz,row.dest_maz,row.starttime,row.value_of_time,
    #     TransitMazTazFlag,three_link_walk_dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,TransitSkimTimeIntervalLength
    #     ,transit_zone_dict,1),axis=1)

    # sorted_trips['transit_walk_time']=sorted_trips.apply(lambda row: 
    #     estimate_single_transit_trip_cost(row.orig_maz,row.dest_maz,row.starttime,row.value_of_time,
    #     TransitMazTazFlag,three_link_walk_dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,TransitSkimTimeIntervalLength
    #     ,transit_zone_dict,2),axis=1)
    
    # sorted_trips[['transit_utility','transit_time','transit_walk_time']]
    # sorted_trips['transit_utility'],sorted_trips['transit_time'],sorted_trips['transit_walk_time']
    sorted_trips ['transit_utility']=sorted_trips.apply(lambda row: 
        estimate_single_transit_trip_cost(row.orig_maz,row.dest_maz,row.starttime,row.value_of_time,
                        TransitMazTazFlag,three_link_walk_dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,TransitSkimTimeIntervalLength,
                        transit_zone_dict,0),axis=1)

    # sorted_trips['transit_utility']=R[1:]
    # sorted_trips['car_time']=sorted_trips.apply(lambda row: 
    #     estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,
    #     Vehicular_Skim_Dict,2,superzone_map,drivingcost_per_mile),axis=1)

    # sorted_trips['car_dist']=sorted_trips.apply(lambda row: 
    #     estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,
    #     Vehicular_Skim_Dict,1,superzone_map,drivingcost_per_mile),axis=1)

    # sorted_trips['toll_cost']=sorted_trips.apply(lambda row:
    #  estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,
    #     Vehicular_Skim_Dict,3,superzone_map,drivingcost_per_mile),axis=1)

    sorted_trips['car_utility']=sorted_trips.apply(lambda row: 
        (estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,
        Vehicular_Skim_Dict,0,superzone_map,drivingcost_per_mile)), axis=1)

    # sorted_trips['expected_mode']=sorted_trips.apply(lambda row:
    #  'NonCar' if row.transit_utility<row.car_utility else 'Car',axis=1)
    sorted_trips['predicted_mode']=sorted_trips.apply(lambda row:
        'NonCar' if row.transit_utility<row.car_utility else 'Car',axis=1)
    sorted_trips['actual_mode']=sorted_trips.apply(lambda row: 
        'NonCar' if row.tripmode>6 else 'Car',axis=1 )

    # sorted_trips['probablity_car']=sorted_trips.apply(lambda row: 
    #     round(math.exp(-row.car_utility)/(math.exp(-row.car_utility)+math.exp(-row.transit_utility)),5),axis=1)
    return sorted_trips

def estimate_trip_reward(hh_num_trips,sorted_trips,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,three_link_walk_dict,
    reward_mode,superzone_map,drivingcost_per_mile,transit_zone_dict,transit_zone_candidates,TransitMazTazFlag,TransitSkimTimeIntervalLength):
#     R=[0]
#     R.extend([6*TT[i+1,i+1+hh_num_trips] for i in range(hh_num_trips) ])
#     R=10*np.ones(1+hh_num_trips)
#     R=[0]
#     print('**********************************')
#     print('Estimate the reward from function')
#     for index,row in sorted_trips.iterrows():
#       
    if reward_mode==0: #Set reward to a large number so that all trips are served by cav
        R=[0]
        for index,row in sorted_trips.iterrows():
            R.extend([10*estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,Vehicular_Skim_Dict,2,superzone_map,drivingcost_per_mile)])
    elif reward_mode==1: #Set the reward to the utility of transit trips
        # R=estimate_transit_cost(sorted_trips,TransitMazTazFlag,TransitSkimTimeIntervalLength,
        #     Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,transit_zone_candidates,three_link_walk_dict,transit_zone_dict)
        R=[0]
        R.extend(list(sorted_trips['transit_utility']))
    elif reward_mode==2:
        R=150*np.ones(1+hh_num_trips)
        R[0]=0
    return R

def extract_hh_information(sorted_trips,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,superzone_map,
    drivingcost_per_mile,num_time_interval):
    '''
    Get the household related information. Those information will be used as input
    for the optimization model
    '''
    
    
    hh_num_trips=sorted_trips.shape[0]
    num_hh_member=sorted_trips['person_id'].unique().size
    
    
    #Then only keep the 
#     sorted_trips=sorted_trips.drop_duplicates(['orig_purpose','dest_purpose','orig_maz','dest_maz','starttime'],keep='first')
#     C=np.ones((2*hh_num_trips+2,2*hh_num_trips+2))
    visit_candidate=[sorted_trips['origin_node'].iloc[0]]
    visit_candidate.extend(sorted_trips['origin_node'].tolist())
    visit_candidate.extend(sorted_trips['destination_node'].tolist())
    visit_candidate.extend([sorted_trips['destination_node'].iloc[-1]])

    C,TT=get_travel_cost_matrix(sorted_trips,Vehicular_Skim_Dict,superzone_map,drivingcost_per_mile,num_time_interval)
    visit_candidate_zone=[sorted_trips['orig_taz'].iloc[0]]
    visit_candidate_zone.extend(sorted_trips['orig_taz'].tolist())
    visit_candidate_zone.extend(sorted_trips['dest_taz'].tolist())
    visit_candidate_zone.extend([sorted_trips['orig_taz'].iloc[0]])
    
    expected_arrival_time=np.ones(2*hh_num_trips+2)
    expected_arrival_time[0]=sorted_trips['starttime'].min()-1
    expected_arrival_time[1:hh_num_trips+1]=sorted_trips.starttime #Expected arrival time at origin node is the original trip start time
    expected_leave_time=np.ones(2*hh_num_trips+2)
    expected_leave_time[0:hh_num_trips+1]=expected_arrival_time[0:hh_num_trips+1] #Expected leave time at origin node is the original trip start time
    #Update the early/late penalty for each trip
    early_penalty=100*np.ones(2*hh_num_trips+2)
    late_penalty=100*np.ones(2*hh_num_trips+2)
    early_penalty[1:hh_num_trips+1]=sorted_trips.earlyarr_penalty
    late_penalty[1:hh_num_trips+1]=sorted_trips.latearr_penalty
    early_penalty[hh_num_trips+1:2*hh_num_trips+1]=sorted_trips.earlyarr_penalty
    late_penalty[hh_num_trips+1:2*hh_num_trips+1]=sorted_trips.latearr_penalty
    
    #Update the earl/late penalty threshold
    early_penalty_threshold=100*np.ones(2*hh_num_trips+2)
    late_penalty_threshold=100*np.ones(2*hh_num_trips+2)
    early_penalty_threshold[1:hh_num_trips+1]=sorted_trips.early_penalty_threshold
    late_penalty_threshold[1:hh_num_trips+1]=sorted_trips.late_penalty_threshold
    early_penalty_threshold[hh_num_trips+1:2*hh_num_trips+1]=sorted_trips.early_penalty_threshold
    late_penalty_threshold[hh_num_trips+1:2*hh_num_trips+1]=sorted_trips.late_penalty_threshold
    for i in range(1+hh_num_trips,2*hh_num_trips+1):
        #Expected arrival time at the destination node is the trip starttime + expected travel time
        if num_time_interval>1:
            expected_arrival_time_interval=math.floor(expected_arrival_time[i-hh_num_trips]/(1440/num_time_interval))
            expected_arrival_time[i]=expected_arrival_time[i-hh_num_trips]+TT[i-hh_num_trips,i,expected_arrival_time_interval] 
        else:
            expected_arrival_time[i]=expected_arrival_time[i-hh_num_trips]+TT[i-hh_num_trips,i] 
        #Expected leav time at the destination node is the trip start time of the same person's next trip
        expected_leave_time[i]=expected_arrival_time[i-hh_num_trips+1]
        
        #         print(i,i-hh_num_trips,hh_num_trips,expected_arrival_time[i-hh_num_trips],expected_arrival_time[i],C[i-hh_num_trips,i])
    
    expected_arrival_time[2*hh_num_trips+1]=1440
    expected_leave_time[2*hh_num_trips+1]=1440
    expected_leave_time[2*hh_num_trips]=1440
    return num_hh_member,hh_num_trips,C,TT,expected_arrival_time,expected_leave_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,visit_candidate_zone


def extract_transit_utility_skim(TransitMazTazFlag,traveler_trips,three_link_walk_dict,
    Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,TransitSkimTimeIntervalLength,transit_zone_dict):
    transit_skim_dict={}
    if TransitMazTazFlag==0:
        orig_column_name='orig_maz'
        dest_column_name='dest_maz'
    else: 
        orig_column_name='orig_taz'
        dest_column_name='dest_taz'
    for origin_zone in traveler_trips[orig_column_name].unique():
        transit_skim_dict[origin_zone]={}
        for dest_zone in traveler_trips[dest_column_name].unique():
            transit_skim_dict[origin_zone][dest_zone]={}
            for ti in range(int(1440/TransitSkimTimeIntervalLength)):
                transit_skim_dict[origin_zone][dest_zone][ti]={}
                trip_start_time=ti*TransitSkimTimeIntervalLength+1
                vot=0.1807 # The median of all transit travelers
                transit_skim_dict[origin_zone][dest_zone][ti]['utility'],transit_skim_dict[origin_zone][dest_zone][ti]['transit_time'],transit_skim_dict[origin_zone][dest_zone][ti]['walk_time']\
                =estimate_single_transit_trip_cost(origin_zone,dest_zone,trip_start_time,vot,TransitMazTazFlag,three_link_walk_dict,
                    Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,TransitSkimTimeIntervalLength,transit_zone_dict,4)

    return transit_skim_dict