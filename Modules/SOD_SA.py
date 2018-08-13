import numpy as np
import pandas as pd
import math

from Modules import DYNASMART_Process as dy
# import preprocessing_read_skim_file as rs
from Modules import AV_functions as av
from Modules import Preprocess_DARP as prd
from Modules import Solve_DARP as sod
from gurobipy import *
import datetime
import random
def dial_n_ride_model_schedule_adjustment_previous_solution(num_hh_member,hh_num_trips,x_sol,T_sol,C,TT,TL,TU,sorted_trips,
                    expected_arrival_time,early_penalty,late_penalty,
                    early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim_Dict,
                    share_ride_factor,output_flag,run_mode,reward_mode,
                    num_cav,num_time_interval,cav_use_mode,time_window_flag,single_model_runtime):
    '''
    This function is the mixed integer programing for the dial and ride problem. Serveral run mode are defined
    run_mode: 
    0 basic formulation soft time window
    1 mode choice extension
    2 schedule rescheduling
    cav_use_mode:
        0 if all num_cav must be used
        1 if num_cav is the upper limit
    '''
    m4=Model("AVSchedule_seq_adjust")
    # x=m4.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,vtype=GRB.BINARY,name="x")
    x=m4.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,num_time_interval,vtype=GRB.BINARY,name="x")
    y=m4.addVars(hh_num_trips+1,hh_num_trips+1,vtype=GRB.BINARY,name='y') # 0 when demand i is served before demandk other wise 1
    T=m4.addVars(2*hh_num_trips+2,name="T") #T represent the expected arrivial time at a node
    S=m4.addVars(2*hh_num_trips+2,name="S")
    # Preprocess
    #First identify the requests that can be moved around. We can only reschedule shop trips for now
    target_request=list(sorted_trips[(sorted_trips.orig_purpose=='Shop')|(sorted_trips.dest_purpose=='Shop')].hh_index+1)
    target_nodes=target_request
    target_nodes.extend([j+hh_num_trips for j in target_request]) #Find the correspoding pickup/delivery node
    #Then remove all the trips that goes from shop to home as it is not necessary anymore
    shop_to_home_requests=list(sorted_trips[(sorted_trips.orig_purpose=='Shop') & (sorted_trips.dest_purpose=='Home')].hh_index+1)
    shop_to_home_nodes=shop_to_home_requests
    shop_to_home_nodes.extend([j+hh_num_trips for j in shop_to_home_requests])
    
    for i in range(num_cav):
        for node in shop_to_home_nodes:
            x_sol=sod.remove_request_from_route(x_sol,node,i,hh_num_trips)
    
    #reset the penalty for the shop trips
    early_penalty[0]=0
    late_penalty[0]=0
    for node in target_nodes:
        early_penalty[node]=0
        late_penalty[node]=0
    
    for ve in range(num_cav):
        for ti in range(num_time_interval):
            for i in range(1,hh_num_trips+1):
                x[0,i+hh_num_trips,ve,ti].ub=0      #No trip go directly from depot to a delivery node
                x[i,2*hh_num_trips+1,ve,ti].ub=0    #No trip go directly from a pickup node to depot
                x[hh_num_trips+i,i,ve,ti].ub=0      #No trip go directly from a delivery node back to associated pickup node
                x[i,i,ve,ti].ub=0                   #No trip between the same node
                for j in range(2*hh_num_trips+2):
                    if (TT[i,j,ti]+TT[j,i+hh_num_trips,ti]>share_ride_factor*TT[i,i+hh_num_trips,ti]):
                        x[i,j,ve,ti].ub=0
                        x[j,i+hh_num_trips,ve,ti].ub=0
            for i in range(hh_num_trips+1,2*hh_num_trips+2):         
                x[i,i,ve,ti].ub=0
            for i in range(2*hh_num_trips+1):
                for j in range(2*hh_num_trips+1):
                    if (x_sol[i,j,ve]==0) & (i not in target_nodes) & (j not in target_nodes):
                            # x[i,j,ve,ti].ub=0
                            downstream_node1=np.where(x_sol[i,:,ve]==1)
                            upstream_node1=np.where(x_sol[:,j,ve]==1)
                            if (downstream_node1 in target_nodes) and (upstream_node1 in target_nodes):
                                x[i,j,ve,ti].ub=1
    
    # The shopping trips
    purpose_list=[1 if (i!='Shop' and j!='Shop') \
              else 0 for i,j in zip(sorted_trips.orig_purpose,sorted_trips.dest_purpose) ]
    purpose_list=[1]+purpose_list
    person_id_list=[0]+list(sorted_trips.person_id)
    '''
    if trip i is a shopping trip or trip i and j does not have the same traveler
        y_temp=3 
    elif trip i is planned before j
        y_temp=0 
    elif:
        y_temp=1
    '''
    y_temp=[[3 if (((purpose_list[i]==0) | (purpose_list[j]==0)) or (person_id_list[i]!=person_id_list[j] ) )\
             else 0 if i<j  \
             else 1 for j in range(len(sorted_trips)+1)] for i in range(len(sorted_trips)+1) ]
    m4.addConstrs((y[i,j]==y_temp[i][j] 
        for i in range(hh_num_trips+1) for j in range(hh_num_trips+1) if y_temp[i][j]!=3 ),'precedencet_relation')
    
    # B=traveler_trips[traveler_trips['hh_id']==household]['starttime'].max()-traveler_trips[traveler_trips['hh_id']==household]['starttime'].min()
    B=1440+max(Vehicular_Skim_Dict[1][1])
    #Add constraints
    # m4.addConstrs((x.sum(0,i,0,'*')==1 for i in [1]),'testconstraint')
    ###################################
    #Basic deliver and pickup constraints
#     m4.addConstrs((x.sum(i,'*')==1 for i in range(1,hh_num_trips+1)),"forcepickupalldemand")
    if share_ride_factor<=1:
        m4.addConstrs((x.sum('*',i,ve,'*')==x.sum(i,i+hh_num_trips,ve,'*') 
            for i in range(1,hh_num_trips+1) for ve in range(num_cav)),'bansharedride')
    # # if num_cav==1:
    if cav_use_mode==0: #All vehicles must be used
        m4.addConstrs((x.sum(i,'*',ve,'*')==1 for i in [0] for ve in range(num_cav)),"FromDepot2")
        m4.addConstrs((x.sum('*',i,ve,'*')==1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3") 
    else: #Not all vehicles must be used
        m4.addConstrs((x.sum(i,'*',ve,'*')<=1 for i in [0] for ve in range(num_cav)),"FromDepot2")
        m4.addConstrs((x.sum('*',i,ve,'*')<=1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3")
    m4.addConstrs((x.sum(i,'*',"*",'*')>=1 for i in [0] ),"oneFromDepot2")
    m4.addConstrs((x.sum('*',i,"*",'*')>=1 for i in [2*hh_num_trips+1] ),"oneToDepot3") 
    m4.addConstrs((x.sum(i,"*",ve,"*")==x.sum("*",i+hh_num_trips,ve,"*") 
        for i in range(1,hh_num_trips+1) for ve in range(num_cav)),"DemandbeDelivered11")

    # # if reward_mode==0 and time_window_flag !=1: #if reward mode is zero then force the cav to pick up all target trips
    # #     m4.addConstrs((x.sum(i,"*","*",'*')==1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
    # #     m4.addConstrs((x.sum("*",j,"*",'*')==1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")
    # # else: #Otherwise pickup is optional
    # # Do not need to serve the shop to home trips
    m4.addConstrs((x.sum(i,"*","*",'*')==1 for i in range(1,2*hh_num_trips+1) if i not in shop_to_home_nodes ),"PickupOnce12")
    m4.addConstrs((x.sum("*",j,"*",'*')==1 for j in range(1,2*hh_num_trips+1) if j not in shop_to_home_nodes),"DeliverOnce13")
    m4.addConstrs((x.sum(i,"*","*",'*')==0 for i in range(1,2*hh_num_trips+1) if i in shop_to_home_nodes ),"DontPickShop2HomeTrips")
    m4.addConstrs((x.sum("*",i,"*",'*')==0 for i in range(1,2*hh_num_trips+1) if i in shop_to_home_nodes ),"DontDeliverShop2HomeTrips")
    m4.addConstrs((x.sum("*",i,ve,'*')==x.sum(i,"*",ve,'*') 
        for i in range(1,2*hh_num_trips+1) for ve in range(num_cav)),"FlowConvervative14")
    m4.addConstrs((x.sum(i,i,ve,ti)==0 for i in range(2*hh_num_trips+2) for ve in range(num_cav) for ti in range(num_time_interval)),"NoSamePointCircleVisit")
    ###################################
    # Time constratins
    if time_window_flag==1: #exact arrvial time
        m4.addConstrs((T[i]==expected_arrival_time[i] for i in range(1,2*hh_num_trips+2)),'ExactStartTime')
#     if force_serve_factor==1: #Force visit all nodes
#         m4.addConstrs((x.sum(i,"*")==1 for i in range(2*hh_num_trips+1)),"allnodemustbeserved")
    m4.addConstrs((T[j]-T[i]-B*x[i,j,ve,ti]>=TT[i,j,ti]-B 
        for i in range(2*hh_num_trips+2) 
        for j in range(2*hh_num_trips+2) 
        for ve in range(num_cav) 
        for ti in range(num_time_interval)),"precedencet15")
    # m4.addConstrs((T[i+hh_num_trips]-T[i]-B*x.sum(i,"*","*")>=(TT[i,i+hh_num_trips]-B) for i in range(1,hh_num_trips+1)),"deliverafterpickup16")
    m4.addConstrs((T[i+hh_num_trips]-T[i]>=(TT[i,i+hh_num_trips,ti]*x.sum(i,'*','*',ti)) 
        for i in range(1,hh_num_trips+1) 
        for ti in range(num_time_interval)),"deliverafterpickup16")
    m4.addConstrs((T[i+hh_num_trips]-T[i]-B<=share_ride_factor*TT[i,i+hh_num_trips,ti]-B*x.sum(i,'*','*',ti) 
        for i in range(1,hh_num_trips+1) 
        for ti in range(num_time_interval)),"triptimecannotexcceed1.5expectedtraveltime")
    m4.addConstrs((x[i,i,ve,ti]==0 for i in range(2*hh_num_trips+2) for ve in range(num_cav) 
        for ti in range(num_time_interval)),"NoSamePointCircleVisit")
    # ####################################
    # #Late/Early Arrival penalty
    m4.addConstrs((S[i]>=early_penalty[i]*(expected_arrival_time[i]-T[i]) for i in range(2*hh_num_trips+2)),'earlyarrivalpenalty')
    m4.addConstrs((S[i]>=late_penalty[i]*(T[i]-expected_arrival_time[i]) for i in range(2*hh_num_trips+2)),'latearrivalpenalty')
    m4.addConstrs((S[i]>=2*early_penalty[i]*(expected_arrival_time[i]-T[i])-early_penalty[i]*early_penalty_threshold[i] 
        for i in range(2*hh_num_trips+2)),'earlyarrivaloverthrespenalty')
    m4.addConstrs((S[i]>=2*late_penalty[i]*(T[i]-expected_arrival_time[i])-late_penalty[i]*late_penalty_threshold[i] 
        for i in range(2*hh_num_trips+2)),'latearrivaloverthrespenalty')
    # ####################################
    m4.addConstrs((T[i]+B*x.sum(i,'*','*',ti)<=TU[ti]+B 
        for i in range(2*hh_num_trips+2) for ti in range(num_time_interval)),'get_time_interval1')
    m4.addConstrs((T[i]-TL[ti]*x.sum(i,'*','*',ti)>=0 
        for i in range(2*hh_num_trips+2) for ti in range(num_time_interval)),'get_time_interval2')

    m4.addConstrs((T[k]-T[i+hh_num_trips]-(x.sum(k,'*','*','*')+x.sum(i,'*','*','*')-1)*B
        >=TT[i+hh_num_trips,k,ti]*x.sum(i,'*','*',ti)-B-B*y[i,k] 
        for i in range(1,hh_num_trips+1) for k in range(1,hh_num_trips+1) for ti in range(num_time_interval)
         if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[k-1]['person_id'])
        ,'activity_sequence'
    )
    m4.addConstrs((T[j]-T[i]>=0 
        for i in range(1,hh_num_trips+1) 
        for j in range(1,hh_num_trips+1) 
        if y_temp[i][j]==0),'precedencet_time')
    m4.addConstrs((y[i,j]==1-y[j,i]) for i in range(1,hh_num_trips+1) for j in range(i+1,hh_num_trips+1))
    # #####################################
    # Objective Function
    obj1=sum(x.sum(i,'*',"*")*R[i] for i in range(hh_num_trips+1))
    obj2=S.sum()
    obj3=sum(x.sum(i,j,"*",ti)*C[i,j,ti] for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) for ti in range(num_time_interval))
    if reward_mode==0 and time_window_flag !=1 :
        m4.setObjective(-obj2-obj3, GRB.MAXIMIZE)
    else:
        m4.setObjective(-obj2-obj3, GRB.MAXIMIZE)

    m4.setParam(GRB.Param.OutputFlag,output_flag)
    m4.Params.TIME_LIMIT=single_model_runtime
    # m4.Params.MIPGap=0.01
    ###############################################################
    #warm start with heuristic
    x_sol_temp=np.zeros([2*hh_num_trips+2,2*hh_num_trips+2,num_cav,num_time_interval])*3
    for i in range(2*hh_num_trips+2):
        T[i].start=T_sol[i]
        expected_arrival_time_interval=sod.get_time_interval(T_sol[i],TL,TU,num_time_interval)
        for j in range(2*hh_num_trips+2):
            for ve in range(num_cav):
                for ti in range(num_time_interval):
                    # x[i,j,ve,ti].start=x_sol[i,j,ve,ti]
                    x[i,j,ve,ti].start=0
                    x_sol_temp[i,j,ve,ti]=0
                if x_sol[i,j,ve]==1:
                    x[i,j,ve,expected_arrival_time_interval].start=1
                    x_sol_temp[i,j,ve,expected_arrival_time_interval]=1
                   
        # print('total_temp',total_temp1) 
    # (x.sum("*",j,"*",'*')==1 for j in range(1,2*hh_num_trips+1) if j not in shop_to_home_nodes)
    
    ###############################################################
    m4.optimize()
    # print(m4.Status)
    # print(m4.SolCount)
    # print(m4.)
    if m4.Status==3 or m4.SolCount==0:
        return m4,0,0,0,0,0,0
    else:
        if output_flag>0:
            print('################################\nThe total reward is',obj1.getValue(),'\nThe delay cost is ',obj2.getValue(),'\nThe total travel cost is ', obj3.getValue())
        return m4,x,y,T,obj1.getValue(),obj2.getValue(),obj3.getValue()

def solve_darpsa_with_darp_solution(target_hh_id,sorted_trips,run_mode,num_time_interval,TL,TU,pre_route_info,
    Vehicular_Skim_Dict,Transit_AB_Time_Skim_Dict,Transit_AB_Cost_Skim_Dict,three_link_walk_dict,transit_zone_dict,
    transit_zone_candidates,TransitMazTazFlag,TransitSkimTimeIntervalLength,superzone_map,drivingcost_per_mile,
    single_model_runtime,reward_mode,cav_use_mode,time_window_flag,num_cav,share_ride_factor,output_flag):
    run_mode=2
    num_hh_member,hh_num_trips,C,TT,expected_arrival_time,expected_leave_time,\
    early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,visit_candidate_zone\
        =prd.extract_hh_information(sorted_trips,Vehicular_Skim_Dict,\
                                    Transit_AB_Cost_Skim_Dict,superzone_map,drivingcost_per_mile,num_time_interval)
    x_sol,T_sol=sod.extract_solution_from_route_info(pre_route_info,hh_num_trips,num_cav,sorted_trips)
    R=prd.estimate_trip_reward(hh_num_trips,sorted_trips,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,
                           Transit_AB_Time_Skim_Dict,three_link_walk_dict,reward_mode,superzone_map
                           ,drivingcost_per_mile,transit_zone_dict,transit_zone_candidates,TransitMazTazFlag,
                           TransitSkimTimeIntervalLength)
    m4,x,y,T,obj1,obj2,obj3=dial_n_ride_model_schedule_adjustment_previous_solution(num_hh_member,hh_num_trips
                        ,x_sol,T_sol,C,TT,TL,TU,sorted_trips,
                        expected_arrival_time,early_penalty,late_penalty,
                        early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim_Dict,
                        share_ride_factor,output_flag,run_mode,reward_mode,
                        num_cav,num_time_interval,cav_use_mode,time_window_flag,single_model_runtime)
    if obj1+obj2+obj3 !=0:
        route_info=sod.extract_route_from_model_solution(x,T,R,C,TT,sorted_trips,
                                                         visit_candidate_zone,hh_num_trips,expected_arrival_time,
                                                         expected_leave_time,superzone_map,num_cav,num_time_interval,run_mode)
        
        T_sol=np.ones(2*hh_num_trips+2)
        for i in range(2*hh_num_trips+2):
            T_sol[i]=T[i].x
        schedule_deviation=T_sol-expected_arrival_time
        if not route_info.empty:
            route_info=sod.break_route_to_seg(route_info,superzone_map)
        route_info.orig_zone=route_info.orig_zone.apply(lambda x: int(x))
        route_info.dest_zone=route_info.dest_zone.apply(lambda x: int(x))
        darp_solution={}
        darp_solution['route_info']=route_info
        darp_solution['schedule_deviation']=sum(list(map(abs,schedule_deviation)))
        darp_solution['schedule_deviation_list']=list(map(abs,schedule_deviation))
        darp_solution['total_reward']=obj1
        darp_solution['total_schedule_penalty']=obj2
        darp_solution['total_travel_cost']=obj3
        darp_solution['reward_mode']=reward_mode
        darp_solution['drivingcost_per_mile']=drivingcost_per_mile
        darp_solution['share_ride_factor']=share_ride_factor
        darp_solution['run_mode']=run_mode
        darp_solution['num_cav']=num_cav
        darp_solution['cav_use_mode']=cav_use_mode
        darp_solution['time_window_flag']=time_window_flag
        darp_solution['hh_num_trips']=len(sorted_trips)
        darp_solution['num_cav_trips']=len(route_info)
        darp_solution['num_occupied_trips']=len(route_info.loc[route_info.person_id>0])
        darp_solution['num_unoccupied_trips']=darp_solution['num_cav_trips']-darp_solution['num_occupied_trips']
        darp_solution['num_pickup_trips']=len(route_info.loc[route_info.orig_node_index<=len(sorted_trips)])
        darp_solution['num_shared_trips']=\
        len(route_info.loc[(route_info.orig_node_index<=len(sorted_trips)) &(route_info.dest_node_index<=len(sorted_trips))])
        darp_solution['num_convention car trips']=len(sorted_trips.loc[sorted_trips.tripmode<=6])
        darp_solution['total_convention_vehicle_driving_distance']=sorted_trips.loc[sorted_trips.tripmode<=6].apply(
            lambda row: prd.estimate_single_car_trip_cost(row.orig_taz,row.dest_taz,row.starttime,row.value_of_time,Vehicular_Skim_Dict,1,
                superzone_map,drivingcost_per_mile),axis=1).sum()
        darp_solution['total_AV_driving_distance']=route_info.apply(
            lambda row:prd.estimate_single_car_trip_cost(row.orig_zone,row.dest_zone,row.start_time,row.value_of_time,Vehicular_Skim_Dict,1,
                superzone_map,drivingcost_per_mile) ,axis=1).sum()
        darp_solution['objective_value']=-obj2-obj3
        darp_solution['hh_id']=sorted_trips.hh_id.iloc[0]
    else: 
        darp_solution={}
        darp_solution['route_info']=pd.DataFrame()

    return darp_solution

def convert_gurobi_solution_to_list(x,T,hh_num_trips):
    x_sol=np.ones([2*hh_num_trips+2,2*hh_num_trips+2,num_cav])
    T_sol=np.ones(2*hh_num_trips+2)
    for i in range(2*hh_num_trips+2):
        T_sol[i]=T[i].x
        for j in range(2*hh_num_trips+2):
            for ve in range(num_cav):
                for ti in range(num_time_interval):
                    x_sol[i,j,ve]=x[i,j,ve].x
    return x_sol,T_sol

def get_route_info_allhh_sa(traveler_trips,initial_darp_solutions,output_flag,min_length,max_length,single_model_runtime,drivingcost_per_mile,
    reward_mode,run_mode,cav_use_mode,num_time_interval,num_cav,share_ride_factor,time_window_flag,
    Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,three_link_walk_dict,superzone_map,TL,TU,transit_zone_dict,transit_zone_candidates,
    TransitMazTazFlag,TransitSkimTimeIntervalLength):
    '''
    This function loops over all the household and find the optimal path for all of them. Return route_infos that 
    stores all the route related information for all households
    '''
    counter=0
    route_infos=pd.DataFrame()
    darp_solutions={}
    # row_number=0
    for target_hh_id,target_hh_temp in traveler_trips.groupby('hh_id'):
        # row_number=row_number+len(traveler_trips[traveler_trips.hh_id==household_id])
        if counter%1000==0: 
            print('Estimate Route for the ',counter,'th household ',datetime.datetime.now())
        counter=counter+1
        if counter>0:
            
            target_hh=target_hh_temp.drop_duplicates(subset=['orig_maz','dest_maz','orig_purpose','dest_purpose',
                                     'starttime','joint_trip_flag'])
            #Sort all trips based on start time. This step could reduce the solving time and make it easier to track
            sorted_trips=target_hh.sort_values("starttime")
            hh_num_trips=sorted_trips.shape[0]
            print(datetime.datetime.now(),'\t',counter,'\t',hh_num_trips,'\t',target_hh_id)
            sorted_trips["hh_index"]=(range(hh_num_trips))
            darp_solutions[target_hh_id]=solve_darpsa_with_darp_solution(target_hh_id,sorted_trips,run_mode,num_time_interval,
                TL,TU,initial_darp_solutions[target_hh_id]['route_info'],
                Vehicular_Skim_Dict,Transit_AB_Time_Skim_Dict,Transit_AB_Cost_Skim_Dict,three_link_walk_dict,transit_zone_dict,
                transit_zone_candidates,TransitMazTazFlag,TransitSkimTimeIntervalLength,superzone_map,drivingcost_per_mile,
                single_model_runtime,reward_mode,cav_use_mode,time_window_flag,num_cav,share_ride_factor,output_flag)
            # sorted_trips=sorted_trips.loc[sorted_trips.tripmode<=6]
            # print(counter,hh_num_trips,datetime.datetime.now())
            route_info=darp_solutions[target_hh_id]['route_info']
            if not route_info.empty:
                route_infos=route_infos.append(route_info)
       
    return  route_infos,darp_solutions