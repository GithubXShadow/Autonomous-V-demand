import numpy as np
import pandas as pd
import math

from Modules import DYNASMART_Process as dy
# import preprocessing_read_skim_file as rs
from Modules import AV_functions as av
from Modules import Preprocess_DARP as prd
from gurobipy import *
import datetime


def dial_n_ride_model_schedule_adjustment(num_hh_member,hh_num_trips,C,TT,TL,TU,sorted_trips,
                    expected_arrival_time,early_penalty,late_penalty,
                    early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim,
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
        1 if num_cav is the upper limite
    '''
    m1=Model("AVSchedule")
    # x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,vtype=GRB.BINARY,name="x")
    x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,num_time_interval,vtype=GRB.BINARY,name="x")
    y=m1.addVars(hh_num_trips+1,hh_num_trips+1,vtype=GRB.BINARY,name='y')
    T=m1.addVars(2*hh_num_trips+2,name="T") #T represent the expected arrivial time at a node
    S=m1.addVars(2*hh_num_trips+2,name="S")
    # B=traveler_trips[traveler_trips['hh_id']==household]['starttime'].max()-traveler_trips[traveler_trips['hh_id']==household]['starttime'].min()
    B=1440+Vehicular_Skim.Time.max()
    #Add constraints
    m1.addConstrs((x.sum(0,i,0,'*')==1 for i in [1]),'testconstraint')
    ###################################
    #Basic deliver and pickup constraints
#     m1.addConstrs((x.sum(i,'*')==1 for i in range(1,hh_num_trips+1)),"forcepickupalldemand")
    if share_ride_factor<=1:
        m1.addConstrs((x.sum('*',i,ve,'*')==x[i,i+hh_num_trips,ve,'*'] for i in range(1,hh_num_trips+1) for ve in range(num_cav)),'bansharedride')
    # if num_cav==1:
    if cav_use_mode==0: #All vehicles must be used
        m1.addConstrs((x.sum(i,'*',ve,'*')==1 for i in [0] for ve in range(num_cav)),"FromDepot2")
        m1.addConstrs((x.sum('*',i,ve,'*')==1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3") 
    else: #Not all vehicles must be used
        m1.addConstrs((x.sum(i,'*',ve,'*')<=1 for i in [0] for ve in range(num_cav)),"FromDepot2")
        m1.addConstrs((x.sum('*',i,ve,'*')<=1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3")

    m1.addConstrs((x.sum(i,'*',"*",'*')>=1 for i in [0] ),"oneFromDepot2")
    m1.addConstrs((x.sum('*',i,"*",'*')>=1 for i in [2*hh_num_trips+1] ),"oneToDepot3") 

    
    m1.addConstrs((x.sum(i,"*",ve,"*")==x.sum("*",i+hh_num_trips,ve,"*") for i in range(1,hh_num_trips+1) for ve in range(num_cav)),"DemandbeDelivered11")
    if reward_mode==0 and time_window_flag !=1: #if reward mode is zero then force the cav to pick up all target trips
        m1.addConstrs((x.sum(i,"*","*",'*')==1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
        m1.addConstrs((x.sum("*",j,"*",'*')==1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")
    else: #Otherwise pickup is optional
        m1.addConstrs((x.sum(i,"*","*",'*')<=1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
        m1.addConstrs((x.sum("*",j,"*",'*')<=1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")

    m1.addConstrs((x.sum("*",i,ve,'*')==x.sum(i,"*",ve,'*') for i in range(1,2*hh_num_trips+1) for ve in range(num_cav)),"FlowConvervative14")
    m1.addConstrs((x.sum(i,i,ve,ti)==0 for i in range(2*hh_num_trips+2) for ve in range(num_cav) for ti in range(num_time_interval)),"NoSamePointCircleVisit")
    # ###################################
    #Time constratins
    if time_window_flag==1: #exact arrvial time
        m1.addConstrs((T[i]==expected_arrival_time[i] for i in range(1,2*hh_num_trips+2)),'ExactStartTime')
#     if force_serve_factor==1: #Force visit all nodes
#         m1.addConstrs((x.sum(i,"*")==1 for i in range(2*hh_num_trips+1)),"allnodemustbeserved")
    m1.addConstrs((T[j]-T[i]-B*x[i,j,ve,ti]>=TT[i,j,ti]-B 
        for i in range(2*hh_num_trips+2) 
        for j in range(2*hh_num_trips+2) 
        for ve in range(num_cav) 
        for ti in range(num_time_interval)),"precedencet15")
    # m1.addConstrs((T[i+hh_num_trips]-T[i]-B*x.sum(i,"*","*")>=(TT[i,i+hh_num_trips]-B) for i in range(1,hh_num_trips+1)),"deliverafterpickup16")
    m1.addConstrs((T[i+hh_num_trips]-T[i]>=(TT[i,i+hh_num_trips,ti]*x.sum(i,'*','*',ti)) 
        for i in range(1,hh_num_trips+1) 
        for ti in range(num_time_interval)),"deliverafterpickup16")
    m1.addConstrs((T[i+hh_num_trips]-T[i]-B<=share_ride_factor*TT[i,i+hh_num_trips,ti]-B*x.sum(i,'*','*',ti) 
        for i in range(1,hh_num_trips+1) 
        for ti in range(num_time_interval)),"triptimecannotexcceed1.5expectedtraveltime")
    
    # ####################################
    # #Late/Early Arrival penalty
    m1.addConstrs((S[i]>=early_penalty[i]*(expected_arrival_time[i]-T[i]) for i in range(2*hh_num_trips+2)),'earlyarrivalpenalty')
    m1.addConstrs((S[i]>=late_penalty[i]*(T[i]-expected_arrival_time[i]) for i in range(2*hh_num_trips+2)),'latearrivalpenalty')
    m1.addConstrs((S[i]>=2*early_penalty[i]*(expected_arrival_time[i]-T[i])-early_penalty[i]*early_penalty_threshold[i] 
        for i in range(2*hh_num_trips+2)),'earlyarrivaloverthrespenalty')
    m1.addConstrs((S[i]>=2*late_penalty[i]*(T[i]-expected_arrival_time[i])-late_penalty[i]*late_penalty_threshold[i] 
        for i in range(2*hh_num_trips+2)),'latearrivaloverthrespenalty')
    # ####################################
    # Special Constraints for this problem
    if run_mode<2: 
        m1.addConstrs((T[j]-T[i+hh_num_trips]-B*(x.sum("*",i,"*")+x.sum("*",j,"*")-1)>=-B 
                            for i in range(1,hh_num_trips+1) for j in range(i+1,hh_num_trips+1)
                            if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[j-1]['person_id']),
                            'personal_sequence_consistent'
                     ) # For the same person, the sequence of activity can not be violated
        
        m1.addConstrs((x[i,j,ve]==0 
                    for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) for ve in range(num_cav)
                    if expected_arrival_time[j]-expected_arrival_time[i]<-30)
                     ,'TripIearlierthanTripj') #
    else:
        m1.addConstrs((T[i]+B*x.sum(i,'*','*',ti)<=TU[ti]+B 
            for i in range(2*hh_num_trips+2) for ti in range(num_time_interval)),'get_time_interval1')
        m1.addConstrs((T[i]-TL[ti]*x.sum(i,'*','*',ti)>=0 
            for i in range(2*hh_num_trips+2) for ti in range(num_time_interval)),'get_time_interval2')
        m1.addConstrs((T[k]-T[i+hh_num_trips]-(x.sum(k,'*','*','*')+x.sum(i,'*','*','*')-1)*B
            >=TT[i,k,ti]*x.sum(i,'*','*',ti)-B-B*y[i,k] 
            for i in range(1,hh_num_trips+1) for k in range(1,hh_num_trips+1) for ti in range(num_time_interval))
            ,if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[k-1]['person_id']
            ,'activity_sequence'
        )
        m1.addConstrs((y[i,j]==1-y[j,i]) for i in range(1,hh_num_trips+1) for j in range(i+1,hh_num_trips+1))


    # #####################################
    # Objective Function
    obj1=sum(x.sum(i,'*',"*")*R[i] for i in range(hh_num_trips+1))
    obj2=S.sum()
    obj3=sum(x.sum(i,j,"*",ti)*C[i,j,ti] for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) for ti in range(num_time_interval))
    if reward_mode==0 and time_window_flag !=1 :
        m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)
    else:
        m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)

    m1.setParam(GRB.Param.OutputFlag,output_flag)
    m1.Params.TIME_LIMIT=single_model_runtime
    
    ###############################################################
    #warm start with heuristic
    for i in range(2*hh_num_trips+2):
        for j in range(2*hh_num_trips+2):
            for ve in range(num_cav):
                for ti in range(num_time_interval):
                    x[i,j,ve,ti].start=0
    # x["*","*","*"].start=0
    expected_arrival_time_interval=math.floor(expected_arrival_time[0]/(1440/num_time_interval))
    x[0,1,0,expected_arrival_time_interval].start=1
    expected_arrival_time_interval=math.floor(expected_arrival_time[1+hh_num_trips]/(1440/num_time_interval))
    x[1,1+hh_num_trips,0,expected_arrival_time_interval].start=1
    last_node=1+hh_num_trips
    for i in range(2*hh_num_trips+2):
        T[i].start=expected_arrival_time[i]
        S[i].start=0
    for i in range(2,1+hh_num_trips):
        expected_arrival_time_interval=math.floor(expected_arrival_time[last_node]/(1440/num_time_interval))
        print(i,expected_arrival_time[i],expected_arrival_time_interval)
        if expected_arrival_time[last_node]+TT[last_node,i,expected_arrival_time_interval]<expected_arrival_time[i]:
            x[last_node,i,0,expected_arrival_time_interval].start=1
            expected_arrival_time_interval=math.floor(expected_arrival_time[i]/(1440/num_time_interval))
            x[i,i+hh_num_trips,0,expected_arrival_time_interval].start=1
            last_node=i+hh_num_trips
        else: 
            for ti in range(num_time_interval):
                x[last_node,i,0,ti].start=0
                x[i,i+hh_num_trips,0,ti].start=0
    x[last_node,2*hh_num_trips+1,0,num_time_interval-1].start=1

#     print('The heuristic solution: ',obj1.getValue(),obj2.getValue(),obj3.getValue())
    
    #Check if the warm start is an feasible solution
#     x_temp=np.zeros((2*hh_num_trips+2,2*hh_num_trips+2))
#     x_temp[0,1]=1
#     x_temp[1,1+hh_num_trips]=1
#     last_node=1+hh_num_trips
#     T_temp=expected_arrival_time
#     for i in range(2,1+hh_num_trips):
#         if expected_arrival_time[last_node]+TT[last_node,i]<expected_arrival_time[i]:
#             x_temp[last_node,i]=1
#             x_temp[i,i+hh_num_trips]=1
#             last_node=i+hh_num_trips
#         else: 
#             x_temp[last_node,i]=0
#             x_temp[i,i+hh_num_trips]=0
#     for i in range(2*hh_num_trips+2):
#         for j in range(2*hh_num_trips+2):
#             if expected_arrival_time[j]-expected_arrival_time[i]-B*x_temp[i,j]<TT[i,j]-B:
#                 print('****************************')
#                 print(i,j,expected_arrival_time[j]-expected_arrival_time[i]-B*x_temp[i,j]>=TT[i,j]-B,x_temp[i,j])
#                 print('****************************')
#     for i in range(1,hh_num_trips+1):
#         for j in range(i+1,hh_num_trips+1):
#             if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[j-1]['person_id']:
#                 if 
    ###############################################################
    m1.optimize()
    if output_flag>0:
        print('################################\nThe total reward is',obj1.getValue(),'\nThe delay cost is ',obj2.getValue(),'\nThe total travel cost is ', obj3.getValue())
        print(S[2*hh_num_trips].x)
       
    return m1,x,y,T,obj1.getValue(),obj2.getValue(),obj3.getValue()



# def dial_n_ride_model(num_hh_member,hh_num_trips,C,TT,sorted_trips,
#                     expected_arrival_time,early_penalty,late_penalty,
#                     early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim,
#                     share_ride_factor,output_flag,run_mode,reward_mode,
#                     num_cav,num_time_interval,cav_use_mode,time_window_flag,single_model_runtime):
#     '''
#     This function is the mixed integer programing for the dial and ride problem. Serveral run mode are defined
#     run_mode: 
#     0 basic formulation soft time window
#     1 mode choice extension
#     2 schedule rescheduling
#     cav_use_mode:
#         0 if all num_cav must be used
#         1 if num_cav is the upper limite
#     '''
#     m1=Model("AVSchedule")
#     # x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,vtype=GRB.BINARY,name="x")
#     x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,num_time_interval,vtype=GRB.BINARY,name="x")
#     T=m1.addVars(2*hh_num_trips+2,name="T") #T represent the expected arrivial time at a node
#     S=m1.addVars(2*hh_num_trips+2,name="S")
#     # B=traveler_trips[traveler_trips['hh_id']==household]['starttime'].max()-traveler_trips[traveler_trips['hh_id']==household]['starttime'].min()
#     B=1440+Vehicular_Skim.Time.max()
#     #Add constraints
#     ###################################
#     #Basic deliver and pickup constraints
# #     m1.addConstrs((x.sum(i,'*')==1 for i in range(1,hh_num_trips+1)),"forcepickupalldemand")
#     if share_ride_factor<=1:
#         m1.addConstrs((x.sum('*',i,ve,'*')==x[i,i+hh_num_trips,ve,'*'] for i in range(1,hh_num_trips+1) for ve in range(num_cav)),'bansharedride')
#     # if num_cav==1:
#     if cav_use_mode==0: #All vehicles must be used
#         m1.addConstrs((x.sum(i,'*',ve,'*')==1 for i in [0] for ve in range(num_cav)),"FromDepot2")
#         m1.addConstrs((x.sum('*',i,ve,'*')==1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3") 
#     else: #Not all vehicles must be used
#         m1.addConstrs((x.sum(i,'*',ve,'*')<=1 for i in [0] for ve in range(num_cav)),"FromDepot2")
#         m1.addConstrs((x.sum('*',i,ve,'*')<=1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3")

#     m1.addConstrs((x.sum(i,'*',"*",'*')>=1 for i in [0] ),"oneFromDepot2")
#     m1.addConstrs((x.sum('*',i,"*",'*')>=1 for i in [2*hh_num_trips+1] ),"oneToDepot3") 
#     # else:
#     #     m1.addConstrs((x.sum(i,'*')==num_cav for i in [0]),"FromDepotlessthannumpoav2")
#     #     m1.addConstrs((x.sum('*',i)==num_cav for i in [2*hh_num_trips+1]),"ToDepotlessthannumpoav3") 
#         # m1.addConstrs((x.sum(i,'*')>=1 for i in [0]),"FromDepot2")
#         # m1.addConstrs((x.sum('*',i)>=1 for i in [2*hh_num_trips+1]),"ToDepot3") 
    
#     m1.addConstrs((x.sum(i,"*",ve)==x.sum("*",i+hh_num_trips,ve) for i in range(1,hh_num_trips+1) for ve in range(num_cav)),"DemandbeDelivered11")
#     if reward_mode==0 and time_window_flag !=1: #if reward mode is zero then force the cav to pick up all target trips
#         m1.addConstrs((x.sum(i,"*","*")<=1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
#         m1.addConstrs((x.sum("*",j,"*")<=1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")
#     else: 
#         m1.addConstrs((x.sum(i,"*","*")<=1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
#         m1.addConstrs((x.sum("*",j,"*")<=1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")

#     m1.addConstrs((x.sum("*",i,ve)==x.sum(i,"*",ve) for i in range(1,2*hh_num_trips+1) for ve in range(num_cav)),"FlowConvervative14")
#     m1.addConstrs((x[i,i,ve]==0 for i in range(2*hh_num_trips+2) for ve in range(num_cav)),"NoSamePointCircleVisit")
#     # ###################################
#     #Time constratins
#     if time_window_flag==1: #exact arrvial time
#         m1.addConstrs((T[i]==expected_arrival_time[i] for i in range(1,2*hh_num_trips+2)),'ExactStartTime')
# #     if force_serve_factor==1: #Force visit all nodes
# #         m1.addConstrs((x.sum(i,"*")==1 for i in range(2*hh_num_trips+1)),"allnodemustbeserved")
#     m1.addConstrs((T[j]-T[i]-B*x[i,j,ve]>=TT[i,j]-B for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) for ve in range(num_cav)),"precedencet15")
#     # m1.addConstrs((T[i+hh_num_trips]-T[i]-B*x.sum(i,"*","*")>=(TT[i,i+hh_num_trips]-B) for i in range(1,hh_num_trips+1)),"deliverafterpickup16")
#     m1.addConstrs((T[i+hh_num_trips]-T[i]>=(TT[i,i+hh_num_trips]) for i in range(1,hh_num_trips+1)),"deliverafterpickup16")
#     m1.addConstrs((T[i+hh_num_trips]-T[i]<=share_ride_factor*TT[i,i+hh_num_trips] for i in range(1,hh_num_trips+1)),"triptimecannotexcceed1.5expectedtraveltime")
    
#     # ####################################
#     # #Late/Early Arrival penalty
#     m1.addConstrs((S[i]>=early_penalty[i]*(expected_arrival_time[i]-T[i]) for i in range(2*hh_num_trips+2)),'earlyarrivalpenalty')
#     m1.addConstrs((S[i]>=late_penalty[i]*(T[i]-expected_arrival_time[i]) for i in range(2*hh_num_trips+2)),'latearrivalpenalty')
#     m1.addConstrs((S[i]>=2*early_penalty[i]*(expected_arrival_time[i]-T[i])-early_penalty[i]*early_penalty_threshold[i] for i in range(2*hh_num_trips+2)),'earlyarrivaloverthrespenalty')
#     m1.addConstrs((S[i]>=2*late_penalty[i]*(T[i]-expected_arrival_time[i])-late_penalty[i]*late_penalty_threshold[i] for i in range(2*hh_num_trips+2)),'latearrivaloverthrespenalty')
# #     m1.addConstrs((T[i]==expected_arrival_time[i] for i in [2*hh_num_trips]),'test123')
#     # ####################################
#     # Special Constraints for this problem

#     m1.addConstrs((T[j]-T[i+hh_num_trips]-B*(x.sum("*",i,"*")+x.sum("*",j,"*")-1)>=-B 
#                         for i in range(1,hh_num_trips+1) for j in range(i+1,hh_num_trips+1)
#                         if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[j-1]['person_id']),
#                         'personal_sequence_consistent'
#                  ) # For the same person, the sequence of activity can not be violated
    
#     m1.addConstrs((x[i,j,ve]==0 
#                 for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) for ve in range(num_cav)
#                 if expected_arrival_time[j]-expected_arrival_time[i]<-30)
#                  ,'TripIearlerthanTripj') #
#     # #####################################
#     # Objective Function
#     obj1=sum(x.sum(i,'*',"*")*R[i] for i in range(hh_num_trips+1))
#     obj2=S.sum()
#     obj3=sum(x.sum(i,j,"*")*C[i,j] for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2))
#     if reward_mode==0 and time_window_flag !=1 :
#         m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)
#     else:
#         m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)

#     m1.setParam(GRB.Param.OutputFlag,output_flag)
#     m1.Params.TIME_LIMIT=single_model_runtime
    
#     ###############################################################
#     #warm start with heuristic
#     for i in range(2*hh_num_trips+2):
#         for j in range(2*hh_num_trips+2):
#             for ve in range(num_cav):
#                 x[i,j,ve].start=0
#     # x["*","*","*"].start=0

#     x[0,1,0].start=1
#     x[1,1+hh_num_trips,0].start=1
#     last_node=1+hh_num_trips
#     for i in range(2*hh_num_trips+2):
#         T[i].start=expected_arrival_time[i]
#         S[i].start=0
#     for i in range(2,1+hh_num_trips):
#         if expected_arrival_time[last_node]+TT[last_node,i]<expected_arrival_time[i]:
#             x[last_node,i,0].start=1
#             x[i,i+hh_num_trips,0].start=1
#             last_node=i+hh_num_trips
#         else: 
#             x[last_node,i,0].start=0
#             x[i,i+hh_num_trips,0].start=0
#     x[last_node,2*hh_num_trips+1,0].start=1

# #     print('The heuristic solution: ',obj1.getValue(),obj2.getValue(),obj3.getValue())
    
#     #Check if the warm start is an feasible solution
# #     x_temp=np.zeros((2*hh_num_trips+2,2*hh_num_trips+2))
# #     x_temp[0,1]=1
# #     x_temp[1,1+hh_num_trips]=1
# #     last_node=1+hh_num_trips
# #     T_temp=expected_arrival_time
# #     for i in range(2,1+hh_num_trips):
# #         if expected_arrival_time[last_node]+TT[last_node,i]<expected_arrival_time[i]:
# #             x_temp[last_node,i]=1
# #             x_temp[i,i+hh_num_trips]=1
# #             last_node=i+hh_num_trips
# #         else: 
# #             x_temp[last_node,i]=0
# #             x_temp[i,i+hh_num_trips]=0
# #     for i in range(2*hh_num_trips+2):
# #         for j in range(2*hh_num_trips+2):
# #             if expected_arrival_time[j]-expected_arrival_time[i]-B*x_temp[i,j]<TT[i,j]-B:
# #                 print('****************************')
# #                 print(i,j,expected_arrival_time[j]-expected_arrival_time[i]-B*x_temp[i,j]>=TT[i,j]-B,x_temp[i,j])
# #                 print('****************************')
# #     for i in range(1,hh_num_trips+1):
# #         for j in range(i+1,hh_num_trips+1):
# #             if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[j-1]['person_id']:
# #                 if 
#     ###############################################################
#     m1.optimize()
#     if output_flag>0:
#         print('################################\nThe total reward is',obj1.getValue(),'\nThe delay cost is ',obj2.getValue(),'\nThe total travel cost is ', obj3.getValue())
#         print(S[2*hh_num_trips].x)
       
#     return m1,x,T,obj1.getValue(),obj2.getValue(),obj3.getValue()





def break_route_to_seg(route_info,superzone_map,vehicle_id):
    '''
    This function break the route of an av into segements for DYNASMART depends on the travelers. 
    The function is called by extract_route_from_model_solution
    '''
    
    seg_index=[0]
    seg_temp=0
    i=1
    intrasuperzone_flag=[0]*len(route_info)
    
    for index, row in route_info[1:].iterrows():    
        if(row.orig_zone==row.dest_zone or check_intrasuperzone(row.orig_zone,row.dest_zone,superzone_map)):
            seg_temp=seg_temp+1
        elif((row.person_id != route_info.iloc[i-1].person_id) 
           or (route_info.iloc[i-1].orig_zone==route_info.iloc[i-1].dest_zone)
            or (check_intrasuperzone(route_info.iloc[i-1].orig_zone,route_info.iloc[i-1].dest_zone,superzone_map))):
            seg_temp=seg_temp+1
        if(check_intrasuperzone(row.orig_zone,row.dest_zone,superzone_map) and row.orig_zone!=row.dest_zone ):
            intrasuperzone_flag[i]=1
        seg_index.extend([seg_temp])
        i=i+1
    route_info['seg_index']=seg_index
    route_info['intrasuperzone_flag']=intrasuperzone_flag
    route_info['hh_vehicle_id']=vehicle_id
    route_info['veh_seg_index']=route_info.hh_id.astype(str)+"_"+route_info.seg_index.astype(str)+"_"+str(vehicle_id) #route_info[['hh_id','seg_index']].apply(lambda x: veh_seg_index_creator(x), axis=1)
    return route_info

def check_intrasuperzone(orig_taz,dest_taz,superzone_map):
    '''
    This fucntion check if the trip between orig_taz and dest_taz is within the same superzone
    '''
#     print(superzone_map.loc[superzone_map.Original_Zones==orig_taz]['SuperZone'].item,superzone_map.loc[superzone_map.Original_Zones==dest_taz]['SuperZone'][0])
    return superzone_map[orig_taz]==superzone_map[dest_taz]
    
def veh_seg_index_creator(x):
    return str(int(x[0]))+'_'+str(int(x[1]))


def find_av_schedule_exact_method(target_hh_id,traveler_trips,Vehicular_Skim,superzone_map,TransitMazTazFlag,WalkSpeed,TransitSkimTimeIntervalLength,Transit_AB_Cost_Skim,Transit_AB_Time_Skim,transit_zone_candidates,three_link_walk,output_flag,drivingcost_per_mile,num_time_interval):
    target_hh=traveler_trips[traveler_trips['hh_id']==target_hh_id].drop_duplicates(subset=['orig_maz','dest_maz','orig_purpose','dest_purpose','starttime','joint_trip_flag'])
    #Sort all trips based on start time. This step could reduce the solving time and make it easier to track
    sorted_trips=target_hh.sort_values("starttime")
    #hh_index give an index to all trips within the household for tracking purpose
    sorted_trips["hh_index"]=(range(hh_num_trips))
    
    num_hh_member,hh_num_trips,C,TT,expected_arrival_time,expected_leave_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,visit_candidate_zone=prd.extract_hh_information(sorted_trips,Vehicular_Skim,Transit_AB_Cost_Skim,superzone_map,drivingcost_per_mile,num_time_interval)
#     R=estimate_transit_cost(sorted_trips,TransitMazTazFlag,WalkSpeed,TransitSkimTimeIntervalLength,Transit_AB_Cost_Skim,Transit_AB_Time_Skim,transit_zone_candidates,three_link_walk)
    R=prd.estimate_trip_reward(hh_num_trips,sorted_trip,Vehicular_Skim,reward_mode,superzone_map)
    m1,x,T,obj1_value,obj2_value,obj3_value=dial_n_ride_model(num_hh_member,hh_num_trips,C,TT,sorted_trips,expected_arrival_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,
                            R,Vehicular_Skim,share_ride_factor,output_flag,run_mode,reward_mode,num_cav,cav_use_mode,time_window_flag,single_model_runtime)
    route_info=extract_route_from_model_solution(x,T,sorted_trips,visit_candidate_zone,hh_num_trips,expected_arrival_time,expected_leave_time,superzone_map,num_cav,num_time_interval)
    return route_info

def get_route_info_allhh(traveler_trips,output_flag):
    '''
    This function loops over all the household and find the optimal path for all of them. Return route_infos that 
    stores all the route related information for all households
    
    '''
    counter=0
    route_infos=pd.DataFrame()
    row_number=0
    for household_id in traveler_trips.hh_id.unique():
        row_number=row_number+len(traveler_trips[traveler_trips.hh_id==household_id])
        if counter%100==0: 
            print('Estimate Route for the ',counter,'th household ',datetime.datetime.now())
        counter=counter+1
        if len(traveler_trips[traveler_trips.hh_id==household_id])<40:
#             print(household_id,len(traveler_trips[traveler_trips.hh_id==household_id]),datetime.datetime.now())
            route_info=find_av_schedule_exact_method(household_id,traveler_trips,Vehicular_Skim,superzone_map,TransitMazTazFlag,WalkSpeed,TransitSkimTimeIntervalLength,Transit_AB_Cost_Skim,Transit_AB_Time_Skim,transit_zone_candidates,three_link_walk,output_flag)
            if not route_info.empty:
                route_infos=route_infos.append(route_info)
    return  route_infos
def extract_route_from_model_solution(x,T,sorted_trips,visit_candidate_zone,hh_num_trips,expected_arrival_time,
    expected_leave_time,superzone_map,num_cav,num_time_interval,run_mode):
    '''
    This function extract route information from the MIP solution and convert the x, T in to trip list.
    '''
    #Check the feasibility of the answer
    #route_dic store the optimiztion model solution as a dictionary. The keys are the upstream node index and the answer is
    #the corresponding downstream node index
    hh_id=sorted_trips['hh_id'].iloc[0]    
    route_info=pd.DataFrame()
    for ve in range(num_cav):
        route_node=[0]
        travelers=[]
        vot=[]
        activity_time=[]
        start_time=[]
        dest_expected_arrival_time=[]
        origin_arrival_time=[]
        dest_arrival_time=[]
        orig_node=[]
        dest_node=[]
        potential_next_node=set(range(1,2*hh_num_trips+1))
        while potential_next_node != set():
            for node in potential_next_node: 
                if run_mode<2: 
                    x_variable_temp=x[route_node[-1],node,ve].x
                else:
                    x_variable_temp=x.sum(route_node[-1],node,ve,'*').getValue()
                    print(x_variable_temp,route_node[-1],node,ve)
                if x_variable_temp>0.91:
                    print(route_node[-1],node,ve)
                    if (route_node[-1]==0): #Initialize the traveler list when it is the depot
                        traveler_set=set()
                        travelers.extend([0])
                        vot.extend([0.01])
                        activity_time.extend([0])
                    elif (route_node[-1]<=hh_num_trips): # If the trip starts from an origin node
    #                     print(traveler_set,sorted_trips.iloc[route_node[-1]-1]['person_id'])
                        traveler_set.add(sorted_trips.iloc[route_node[-1]-1]['person_id'])
                        travelers.extend([sorted_trips.iloc[route_node[-1]-1]['person_id']])
    #                     print(route_node[-1],node,sorted_trips.iloc[route_node[-1]-1]['person_id'],traveler_set)
                    else: #if the trip starts from a destination node
                        traveler_set.remove(sorted_trips.iloc[route_node[-1]-1-hh_num_trips]['person_id'])
                        if traveler_set ==set():
                            travelers.extend([0])
                            vot.extend([0.01])
                            activity_time.extend([0])
                        else:
                            travelers.extend([list(traveler_set)[0]])
    #                     print(route_node[-1],node,sorted_trips.iloc[route_node[-1]-1-hh_num_trips]['person_id'],traveler_set)
    #                 print(traveler_set,travelers)
        
                    if traveler_set !=set():
                        vot.extend([sorted_trips[sorted_trips.person_id==travelers[-1]]['value_of_time'].iloc[0]])
                        activity_time.extend([max(0,expected_leave_time[node]-expected_arrival_time[node])])
    #                     vot.extend([sorted_trips.iloc[route_node[-1]-1]['value_of_time']])
                    start_time.extend([expected_leave_time[route_node[-1]]])
                    origin_arrival_time.extend([T[route_node[-1]].x])
                    dest_arrival_time.extend([T[node].x])
                    dest_expected_arrival_time.extend([expected_arrival_time[node]])
                    orig_node.extend([sorted_trips])
                    route_node.extend([node])
                    break
            if route_node[-1] in potential_next_node:
                potential_next_node.remove(route_node[-1])
            else:
                potential_next_node=set()
                # print('*******************************')
               
                # print(sum([x[i,j].x for i in potential_next_node for j in range(1,2*hh_num_trips+1)]))
                # print('*******************************')
        route=[visit_candidate_zone[x] for x in route_node]
        route_info_temp=pd.DataFrame({'orig_zone':route[1:-1],'dest_zone':route[2:],'person_id':travelers[1:],
                                 'orig_node_index':route_node[1:-1],'dest_node_index':route_node[2:],
                                 'origin_arrival_time':origin_arrival_time[1:],'dest_arrival_time':dest_arrival_time[1:],
                                 'dest_expected_arrival_time':dest_expected_arrival_time[1:],'value_of_time':vot[1:],
                                 'start_time':start_time[1:],'Activity_Time':activity_time[1:],
                                 'hh_id':np.ones(len(route[1:-1]))*hh_id},
                                columns=['orig_zone','dest_zone','orig_node_index','dest_node_index',
                                         'person_id','origin_arrival_time','dest_arrival_time','dest_expected_arrival_time','value_of_time'
                                         ,'start_time','Activity_Time','hh_id'])
        # Drop the trip between a person's current destination and next trips's origin, as those are the same node
        route_info_temp=route_info_temp.loc[((route_info_temp.orig_node_index-route_info_temp.dest_node_index!=hh_num_trips-1)
                                   | (route_info_temp.orig_zone!=route_info_temp.dest_zone)
                                  |(route_info_temp.start_time!=route_info_temp.dest_expected_arrival_time)) ] 
#     sorted_trips.iloc[max(i for i in [route_info_temp.orig_node_index,route_info_temp.orig_node_index-hh_num_trips] if i >0)].person_id !=
# sorted_trips.iloc[max(i for i in [route_info_temp.dest_node_index,route_info_temp.dest_node_index-hh_num_trips] if i >0)].person_id
        if not route_info_temp.empty:
            route_info_temp=break_route_to_seg(route_info_temp,superzone_map,ve)
        route_info=route_info.append(route_info_temp)
    
    return route_info


###############################
#This section contains functions associated with the schedule partition heuristic
def find_break_point(sorted_trips,min_length,max_length,Vehicular_Skim,superzone_map):
    min_length=20 #The minimal length of the schedule
    max_length=30 #The maximal length of the schedule 
    #Let i and i+1 be two consective trips in the sorted_trip list. 
    # gap_ratio=((arrival time at origin of i+1)-(arrival time at destination of i))/(travel time from destination of i to origin of i+1)
    gap_ratio=(sorted_trips.iloc[1:].starttime.values-sorted_trips.iloc[0:-1].starttime.values-sorted_trips.iloc[0:-1].travel_time.values)/[Vehicular_Skim.loc[i,superzone_map[j],1,1].Time.item() for (i,j) in zip(sorted_trips.dest_taz.values[0:-1], sorted_trips.orig_taz.values[1:])]

    if len(gap_ratio)>2*min_length:
        break_point_1=np.argmax(gap_ratio[min_length:max_length])+min_length
        break_point_2=len(gap_ratio)-max_length+np.argmax(gap_ratio[-max_length:-min_length])
        if gap_ratio[break_point_1]>gap_ratio[break_point_2]:
            break_point=break_point_1
        else:
            break_point=break_point_2
    else: 
        break_point=np.argmax(gap_ratio)
#     print(gap_ratio[break_point])
    return break_point

def schedule_partition(sorted_trips,Vehicular_Skim,min_length,max_length,superzone_map):
    break_point=find_break_point(sorted_trips,min_length,max_length,Vehicular_Skim,superzone_map)
    left_sub_trips=sorted_trips[0:break_point+1]
    right_sub_trips=sorted_trips[break_point+1:]
#     print(break_point,len(left_sub_trips),len(right_sub_trips))
    #Split the trips to left_sub_trips and right_subtrips
    if max(len(left_sub_trips),len(right_sub_trips))>max_length:
        if len(left_sub_trips)>len(right_sub_trips): #Furthur split the longer subtrips
            return [schedule_partition(left_sub_trips,Vehicular_Skim,min_length,max_length,superzone_map),[right_sub_trips]]
        else: 
            return [[left_sub_trips],schedule_partition(right_sub_trips,Vehicular_Skim,min_length,max_length,superzone_map)]
    else:
        return [left_sub_trips,right_sub_trips]
    
def flatten(sub_sorted_trips):
    if isinstance(sub_sorted_trips, pd.DataFrame):
        return [sub_sorted_trips]
    else:
        return [a for i in sub_sorted_trips for a in flatten(i)]
    
def solve_with_schedule_partition(sorted_trips,Vehicular_Skim,Transit_AB_Cost_Skim,superzone_map,min_length,max_length,
                                    reward_mode,drivingcost_per_mile,share_ride_factor,output_flag,run_mode,num_cav,
                                    cav_use_mode,time_window_flag,single_model_runtime,num_time_interval,TL,TU):
#     sub_sorted_trips=[item for sublist in schedule_partition(sorted_trips,Vehicular_Skim,min_length,max_length) for item in sublist]
    sub_sorted_trips=flatten(schedule_partition(sorted_trips,Vehicular_Skim,min_length,max_length,superzone_map))
    route_info=pd.DataFrame()
   
    total_previous_sub_trips_length=0
    total_tailing_sub_trips_length=0
    schedule_deviation=[]
    total_reward=0
    total_schedule_penalty=0 #The total penalty for early/late arrival
    total_travel_cost=0
    for sub_sorted_trip in sub_sorted_trips:
        num_hh_member,hh_num_trips,C,TT,expected_arrival_time,expected_leave_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,visit_candidate_zone=prd.extract_hh_information(sub_sorted_trip,Vehicular_Skim,Transit_AB_Cost_Skim,superzone_map,drivingcost_per_mile,num_time_interval)
        # R=estimate_transit_cost(sorted_trips,TransitMazTazFlag,WalkSpeed,TransitSkimTimeIntervalLength,Transit_AB_Cost_Skim,Transit_AB_Time_Skim,transit_zone_candidates,three_link_walk)
        R=prd.estimate_trip_reward(hh_num_trips,sub_sorted_trip,Vehicular_Skim,reward_mode,superzone_map,drivingcost_per_mile)
    
        print('start sovling problem at ',datetime.datetime.now())
        if run_mode<2:
            m1,x,T,obj1_value,obj2_value,obj3_value=dial_n_ride_model(num_hh_member,hh_num_trips,C,TT,sub_sorted_trip,expected_arrival_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,
                                    R,Vehicular_Skim,share_ride_factor,output_flag,run_mode,reward_mode,num_cav,cav_use_mode,time_window_flag,single_model_runtime)
        else:
            m1,x,T,obj1_value,obj2_value,obj3_value=dial_n_ride_model_schedule_adjustment(num_hh_member,hh_num_trips,C,TT,TL,TU,sub_sorted_trip,
                expected_arrival_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim,
                share_ride_factor,output_flag,run_mode,reward_mode,num_cav,num_time_interval,cav_use_mode,time_window_flag,single_model_runtime)
                        
        print('finish solving problem at ',datetime.datetime.now())
        sub_route_info=extract_route_from_model_solution(x,T,sub_sorted_trip,visit_candidate_zone,hh_num_trips,expected_arrival_time,
            expected_leave_time,superzone_map,num_cav,num_time_interval)
        total_tailing_sub_trips_length=len(sorted_trips)-total_previous_sub_trips_length-len(sub_sorted_trip)
        sub_route_info['orig_node_index']=sub_route_info.orig_node_index.apply(
            lambda x: x+total_previous_sub_trips_length if x<=hh_num_trips else x+2*total_previous_sub_trips_length+total_tailing_sub_trips_length) 
        sub_route_info['dest_node_index']=sub_route_info.dest_node_index.apply(
            lambda x: x+total_previous_sub_trips_length if x<=hh_num_trips else x+2*total_previous_sub_trips_length+total_tailing_sub_trips_length) 
        total_previous_sub_trips_length=total_previous_sub_trips_length+len(sub_sorted_trip)
        route_info=route_info.append(sub_route_info)
        total_reward+=obj1_value
        total_schedule_penalty+=obj2_value
        total_travel_cost+=obj3_value
#         for index, row in route_info.iterrows():
#             print(route_info.dest_expected_arrival_time,'\t',row.dest_arrival_time,'\t',row.start_time,'\t',T[row.dest_node_index].x)
#         #Estimate the delay and early arrival
    route_info.orig_zone=route_info.orig_zone.apply(lambda x: int(x))
    route_info.dest_zone=route_info.dest_zone.apply(lambda x: int(x))
    T_sol=np.ones(2*hh_num_trips+2)
    for i in range(2*hh_num_trips+2):
    #     print(int(T[i].x),'\t',expected_arrival_time[i],'\t',T[i].x-expected_arrival_time[i])
        T_sol[i]=T[i].x
        
    schedule_deviation.extend(T_sol-expected_arrival_time)
    darp_solution={}
    darp_solution['route_info']=route_info
    darp_solution['schedule_deviation']=schedule_deviation
    darp_solution['total_reward']=total_reward
    darp_solution['total_schedule_penalty']=total_schedule_penalty
    darp_solution['total_travel_cost']=total_travel_cost
    darp_solution['reward_mode']=reward_mode
    darp_solution['drivingcost_per_mile']=drivingcost_per_mile
    darp_solution['share_ride_factor']=share_ride_factor
    darp_solution['run_mode']=run_mode
    darp_solution['num_cav']=num_cav
    darp_solution['cav_use_mode']=cav_use_mode
    darp_solution['time_window_flag']=time_window_flag
    return darp_solution
################################################
# One vehicle DNRP
# def dial_n_ride_model(num_hh_member,hh_num_trips,C,TT,sorted_trips,
#                     expected_arrival_time,early_penalty,late_penalty,
#                     early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim,
#                     share_ride_factor,output_flag,run_mode,
#                     num_cav,time_window_flag,single_model_runtime):
#     '''
#     This function is the mixed integer programing for the dial and ride problem. Serveral run mode are defined
#     run_mode: 
#     0 basic formulation soft time window
#     1 mode choice extension
    
    
#     '''
#     m1=Model("AVSchedule")
#     x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,vtype=GRB.BINARY,name="x")
#     T=m1.addVars(2*hh_num_trips+2,name="T") #T represent the expected arrivial time at a node
#     S=m1.addVars(2*hh_num_trips+2,name="S")
   
#     # B=traveler_trips[traveler_trips['hh_id']==household]['starttime'].max()-traveler_trips[traveler_trips['hh_id']==household]['starttime'].min()
#     B=1440+Vehicular_Skim.Time.max()
#     #Add constraints
#     ###################################
#     #Basic deliver and pickup constraints
# #     m1.addConstrs((x.sum(i,'*')==1 for i in range(1,hh_num_trips+1)),"forcepickupalldemand")
# #     m1.addConstrs((x.sum('*',i)==x[i,i+hh_num_trips] for i in range(1,hh_num_trips+1)),'bansharedride')
#     if num_cav==1:
#         m1.addConstrs((x.sum(i,'*')==num_cav for i in [0]),"FromDepot2")
#         m1.addConstrs((x.sum('*',i)==num_cav for i in [2*hh_num_trips+1]),"ToDepot3") 
#     else:
#         m1.addConstrs((x.sum(i,'*')==num_cav for i in [0]),"FromDepotlessthannumpoav2")
#         m1.addConstrs((x.sum('*',i)==num_cav for i in [2*hh_num_trips+1]),"ToDepotlessthannumpoav3") 
#         m1.addConstrs((x.sum(i,'*')>=1 for i in [0]),"FromDepot2")
#         m1.addConstrs((x.sum('*',i)>=1 for i in [2*hh_num_trips+1]),"ToDepot3") 
    
#     m1.addConstrs((x.sum(i,"*")==x.sum("*",i+hh_num_trips) for i in range(1,hh_num_trips+1)),"DemandbeDelivered11")
#     m1.addConstrs((x.sum(i,"*")<=1 for i in range(1,2*hh_num_trips+1)),"PickupOnce12")
#     m1.addConstrs((x.sum("*",j)<=1 for j in range(1,2*hh_num_trips+1)),"DeliverOnce13")
#     m1.addConstrs((x.sum("*",i)==x.sum(i,"*") for i in range(1,2*hh_num_trips+1)),"FlowConvervative14")
#     m1.addConstrs((x[i,i]==0 for i in range(2*hh_num_trips+2)),"NoSamePointCircleVisit")
#     # ###################################
#     #Time constratins
#     if time_window_flag==1: #exact arrvial time
#         m1.addConstrs((T[i]==expected_arrival_time[i] for i in range(1,2*hh_num_trips+2)),'ExactStartTime')
# #     if force_serve_factor==1: #Force visit all nodes
# #         m1.addConstrs((x.sum(i,"*")==1 for i in range(2*hh_num_trips+1)),"allnodemustbeserved")
#     m1.addConstrs((T[j]-T[i]-B*x[i,j]>=TT[i,j]-B for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2)),"precedencet15")
#     m1.addConstrs((T[i+hh_num_trips]-T[i]-B*x.sum(i,"*")>=(TT[i,i+hh_num_trips]-B) for i in range(1,hh_num_trips+1)),"deliverafterpickup16")

#     m1.addConstrs((T[i+hh_num_trips]-T[i]<=share_ride_factor*TT[i,i+hh_num_trips] for i in range(1,hh_num_trips+1)),"triptimecannotexcceed1.5expectedtraveltime")
    
#     # ####################################
#     # #Late/Early Arrival penalty
#     m1.addConstrs((S[i]>=early_penalty[i]*(expected_arrival_time[i]-T[i]) for i in range(2*hh_num_trips+2)),'earlyarrivalpenalty')
#     m1.addConstrs((S[i]>=late_penalty[i]*(T[i]-expected_arrival_time[i]) for i in range(2*hh_num_trips+2)),'latearrivalpenalty')
#     m1.addConstrs((S[i]>=2*early_penalty[i]*(expected_arrival_time[i]-T[i])-early_penalty[i]*early_penalty_threshold[i] for i in range(2*hh_num_trips+2)),'earlyarrivaloverthrespenalty')
#     m1.addConstrs((S[i]>=2*late_penalty[i]*(T[i]-expected_arrival_time[i])-late_penalty[i]*late_penalty_threshold[i] for i in range(2*hh_num_trips+2)),'latearrivaloverthrespenalty')
# #     m1.addConstrs((T[i]==expected_arrival_time[i] for i in [2*hh_num_trips]),'test123')
#     # ####################################
#     # Special Constraints for this problem

#     m1.addConstrs((T[j]-T[i+hh_num_trips]-B*(x.sum("*",i)+x.sum("*",j)-1)>=-B for i in range(1,hh_num_trips+1)
#                         for j in range(i+1,hh_num_trips+1)
#                         if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[j-1]['person_id']),
#                         'personal_sequence_consistent'
#                  ) # For the same person, the sequence of activity can not be violated
    
#     m1.addConstrs((x[i,j]==0 for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) 
#                   if expected_arrival_time[j]-expected_arrival_time[i]<-30)
#                  ,'TripIearlerthanTripj') #
#     # #####################################
#     # Objective Function
#     obj1=sum(x.sum(i,'*')*R[i] for i in range(hh_num_trips+1))
#     obj2=S.sum()
#     obj3=sum(x[i,j]*C[i,j] for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2))
#     if run_mode==0 or run_mode==2:
#         m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)
#     elif run_mode==1:
#         m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)

#     m1.setParam(GRB.Param.OutputFlag,output_flag)
#     m1.Params.TIME_LIMIT=single_model_runtime
    
#     ###############################################################
#     #warm start with heuristic
#     for i in range(2*hh_num_trips+2):
#         for j in range(2*hh_num_trips+2):
#             x[i,j].start=0
    
#     x[0,1].start=1
#     x[1,1+hh_num_trips].start=1
#     last_node=1+hh_num_trips
#     for i in range(2*hh_num_trips+2):
#         T[i].start=expected_arrival_time[i]
#         S[i].start=0
#     for i in range(2,1+hh_num_trips):
#         if expected_arrival_time[last_node]+TT[last_node,i]<expected_arrival_time[i]:
#             x[last_node,i].start=1
#             x[i,i+hh_num_trips].start=1
#             last_node=i+hh_num_trips
#         else: 
#             x[last_node,i].start=0
#             x[i,i+hh_num_trips].start=0
#     x[last_node,2*hh_num_trips+1].start=1
# #     print('The heuristic solution: ',obj1.getValue(),obj2.getValue(),obj3.getValue())
    
#     #Check if the warm start is an feasible solution
# #     x_temp=np.zeros((2*hh_num_trips+2,2*hh_num_trips+2))
# #     x_temp[0,1]=1
# #     x_temp[1,1+hh_num_trips]=1
# #     last_node=1+hh_num_trips
# #     T_temp=expected_arrival_time
# #     for i in range(2,1+hh_num_trips):
# #         if expected_arrival_time[last_node]+TT[last_node,i]<expected_arrival_time[i]:
# #             x_temp[last_node,i]=1
# #             x_temp[i,i+hh_num_trips]=1
# #             last_node=i+hh_num_trips
# #         else: 
# #             x_temp[last_node,i]=0
# #             x_temp[i,i+hh_num_trips]=0
# #     for i in range(2*hh_num_trips+2):
# #         for j in range(2*hh_num_trips+2):
# #             if expected_arrival_time[j]-expected_arrival_time[i]-B*x_temp[i,j]<TT[i,j]-B:
# #                 print('****************************')
# #                 print(i,j,expected_arrival_time[j]-expected_arrival_time[i]-B*x_temp[i,j]>=TT[i,j]-B,x_temp[i,j])
# #                 print('****************************')
# #     for i in range(1,hh_num_trips+1):
# #         for j in range(i+1,hh_num_trips+1):
# #             if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[j-1]['person_id']:
# #                 if 
#     ###############################################################
#     m1.optimize()
#     if output_flag>0:
#         print('################################\nThe total reward is',obj1.getValue(),'\nThe delay cost is ',obj2.getValue(),'\nThe total travel cost is ', obj3.getValue())
#         print(S[2*hh_num_trips].x)
        
#     return m1,x,T
