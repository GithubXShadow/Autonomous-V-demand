import numpy as np
import pandas as pd
import math

from Modules import DYNASMART_Process as dy
# import preprocessing_read_skim_file as rs
from Modules import AV_functions as av
from Modules import Preprocess_DARP as prd
from gurobipy import *
import datetime
import random
def dial_n_ride_model_schedule_adjustment(num_hh_member,hh_num_trips,C,TT,TL,TU,sorted_trips,
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
        1 if num_cav is the upper limite
    '''
    m1=Model("AVSchedule_seq_adjust")
    # x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,vtype=GRB.BINARY,name="x")
    x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,num_time_interval,vtype=GRB.BINARY,name="x")
    y=m1.addVars(hh_num_trips+1,hh_num_trips+1,vtype=GRB.BINARY,name='y')
    T=m1.addVars(2*hh_num_trips+2,name="T") #T represent the expected arrivial time at a node
    S=m1.addVars(2*hh_num_trips+2,name="S")
    # Preprocess
   
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
    
    # B=traveler_trips[traveler_trips['hh_id']==household]['starttime'].max()-traveler_trips[traveler_trips['hh_id']==household]['starttime'].min()
    B=1440+max(Vehicular_Skim_Dict[1][1])
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

    
    m1.addConstrs((x.sum(i,"*",ve,"*")==x.sum("*",i+hh_num_trips,ve,"*") 
        for i in range(1,hh_num_trips+1) for ve in range(num_cav)),"DemandbeDelivered11")
    if reward_mode==0 and time_window_flag !=1: #if reward mode is zero then force the cav to pick up all target trips
        m1.addConstrs((x.sum(i,"*","*",'*')==1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
        m1.addConstrs((x.sum("*",j,"*",'*')==1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")
    else: #Otherwise pickup is optional
        m1.addConstrs((x.sum(i,"*","*",'*')<=1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
        m1.addConstrs((x.sum("*",j,"*",'*')<=1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")

    m1.addConstrs((x.sum("*",i,ve,'*')==x.sum(i,"*",ve,'*') 
        for i in range(1,2*hh_num_trips+1) for ve in range(num_cav)),"FlowConvervative14")
    # m1.addConstrs((x.sum(i,i,ve,ti)==0 for i in range(2*hh_num_trips+2) for ve in range(num_cav) for ti in range(num_time_interval)),"NoSamePointCircleVisit")
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
    m1.addConstrs((x[i,i,ve,ti]==0 for i in range(2*hh_num_trips+2) for ve in range(num_cav) for ti in range(num_time_interval)),"NoSamePointCircleVisit")
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
        m1.addConstrs((T[k]-T[i+hh_num_trips]-(x.sum(k,'*','*',ti)+x.sum(i,'*','*',ti)-1)*B
            >=TT[i+hh_num_trips,k,ti]*x.sum(i,'*','*',ti)-B-B*y[i,k] 
            for i in range(1,hh_num_trips+1) for k in range(1,hh_num_trips+1) for ti in range(num_time_interval)
             if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[k-1]['person_id'])
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
    m1.Params.MITGap=0.01
    ###############################################################
    #warm start with heuristic
    for i in range(2*hh_num_trips+2):
        for j in range(2*hh_num_trips+2):
            for ve in range(num_cav):
                for ti in range(num_time_interval):
                    x[i,j,ve,ti].start=0
    x_temp=np.zeros((2*hh_num_trips+2,2*hh_num_trips+2,num_cav,num_time_interval))
    
    # x["*","*","*"].start=0
    expected_arrival_time_interval=math.floor(expected_arrival_time[0]/(1440/num_time_interval))
    x[0,1,0,expected_arrival_time_interval].start=1
    x_temp[0,1,0,expected_arrival_time_interval]=1
    expected_arrival_time_interval=math.floor(expected_arrival_time[1+hh_num_trips]/(1440/num_time_interval))
    x[1,1+hh_num_trips,0,expected_arrival_time_interval].start=1
    x_temp[1,1+hh_num_trips,0,expected_arrival_time_interval]=1
    last_node=1+hh_num_trips
    for i in range(2*hh_num_trips+2):
        T[i].start=expected_arrival_time[i]
        S[i].start=0
    for i in range(2,1+hh_num_trips):
        expected_arrival_time_interval=math.floor(expected_arrival_time[last_node]/(1440/num_time_interval))
        
        if expected_arrival_time[last_node]+TT[last_node,i,expected_arrival_time_interval]<expected_arrival_time[i]:
            x[last_node,i,0,expected_arrival_time_interval].start=1
            x_temp[last_node,i,0,expected_arrival_time_interval]=1

           
            expected_arrival_time_interval=math.floor(expected_arrival_time[i]/(1440/num_time_interval))
            x[i,i+hh_num_trips,0,expected_arrival_time_interval].start=1
            x_temp[i,i+hh_num_trips,0,expected_arrival_time_interval]=1
            last_node=i+hh_num_trips
        else: 
            for ti in range(num_time_interval):
                x[last_node,i,0,ti].start=0
                x[i,i+hh_num_trips,0,ti].start=0
    x[last_node,2*hh_num_trips+1,0,num_time_interval-1].start=1
    x_temp[last_node,2*hh_num_trips+1,0,num_time_interval-1]=1

     
    # y_temp=np.ones((hh_num_trips+1,hh_num_trips+1))
    # for i in range(1,hh_num_trips+1):
    #     for j in range(1,hh_num_trips+1):
    #         if i<j:
    #             y_temp[i,j]=0

    # for i in range(1,hh_num_trips+1):
    #     for k in range(1,hh_num_trips+1):
    #         for ti in range(num_time_interval):
    #             if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[k-1]['person_id']:
    #                 if i!=k:
    #                     if (expected_arrival_time[k]-expected_arrival_time[i+hh_num_trips]-
    #                     (x_temp.sum(axis=1).sum(axis=1)[k,ti]+x_temp.sum(axis=1).sum(axis=1)[i,ti]-1)*B
    #                     <TT[i+hh_num_trips,k,ti]*x_temp.sum(axis=1).sum(axis=1)[i,ti]-B-B*y_temp[i,k] ):
    #                         print(i,k,ti,expected_arrival_time[k],expected_arrival_time[i+hh_num_trips]
    #                             ,x_temp.sum(axis=1).sum(axis=1)[k,ti],x_temp.sum(axis=1).sum(axis=1)[i,ti]
    #                             ,TT[i,k,ti],x_temp.sum(axis=1).sum(axis=1)[i,ti],y_temp[i,k])

    # print(x[last_node,2*hh_num_trips+1,0,num_time_interval-1].ub)
    
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



def dial_n_ride_model(num_hh_member,hh_num_trips,C,TT,sorted_trips,
                    expected_arrival_time,early_penalty,late_penalty,
                    early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim_Dict,
                    share_ride_factor,output_flag,run_mode,reward_mode,
                    num_cav,cav_use_mode,time_window_flag,single_model_runtime):
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
    x=m1.addVars(2*hh_num_trips+2,2*hh_num_trips+2,num_cav,vtype=GRB.BINARY,name="x")
    T=m1.addVars(2*hh_num_trips+2,name="T") #T represent the expected arrivial time at a node
    S=m1.addVars(2*hh_num_trips+2,name="S")
    # B=traveler_trips[traveler_trips['hh_id']==household]['starttime'].max()-traveler_trips[traveler_trips['hh_id']==household]['starttime'].min()
    B=1440+max(Vehicular_Skim_Dict[1][1])
    
    #Add constraints
    ###################################
    #Basic deliver and pickup constraints
#     m1.addConstrs((x.sum(i,'*')==1 for i in range(1,hh_num_trips+1)),"forcepickupalldemand")
    if share_ride_factor<=1:
        m1.addConstrs((x.sum('*',i,ve)==x[i,i+hh_num_trips,ve] for i in range(1,hh_num_trips+1) for ve in range(num_cav)),'bansharedride')
    # if num_cav==1:
    if cav_use_mode==0: #All vehicles must be used
        m1.addConstrs((x.sum(i,'*',ve)==1 for i in [0] for ve in range(num_cav)),"FromDepot2")
        m1.addConstrs((x.sum('*',i,ve)==1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3") 
    else: #Not all vehicles must be used
        m1.addConstrs((x.sum(i,'*',ve)<=1 for i in [0] for ve in range(num_cav)),"FromDepot2")
        m1.addConstrs((x.sum('*',i,ve)<=1 for i in [2*hh_num_trips+1] for ve in range(num_cav)),"ToDepot3")

    m1.addConstrs((x.sum(i,'*',"*")>=1 for i in [0] ),"oneFromDepot2")
    m1.addConstrs((x.sum('*',i,"*")>=1 for i in [2*hh_num_trips+1] ),"oneToDepot3") 
    
    
    m1.addConstrs((x.sum(i,"*",ve)==x.sum("*",i+hh_num_trips,ve) for i in range(1,hh_num_trips+1) for ve in range(num_cav)),"DemandbeDelivered11")
    if reward_mode==0 and time_window_flag !=1: #if reward mode is zero then force the cav to pick up all target trips
        m1.addConstrs((x.sum(i,"*","*")==1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
        m1.addConstrs((x.sum("*",j,"*")==1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")
    else: 
        m1.addConstrs((x.sum(i,"*","*")<=1 for i in range(1,2*hh_num_trips+1) ),"PickupOnce12")
        m1.addConstrs((x.sum("*",j,"*")<=1 for j in range(1,2*hh_num_trips+1) ),"DeliverOnce13")

    m1.addConstrs((x.sum("*",i,ve)==x.sum(i,"*",ve) for i in range(1,2*hh_num_trips+1) for ve in range(num_cav)),"FlowConvervative14")
    m1.addConstrs((x[i,i,ve]==0 for i in range(2*hh_num_trips+2) for ve in range(num_cav)),"NoSamePointCircleVisit")
    # ###################################
    #Time constratins
    if time_window_flag==1: #exact arrvial time
        m1.addConstrs((T[i]==expected_arrival_time[i] for i in range(1,2*hh_num_trips+2)),'ExactStartTime')
#     if force_serve_factor==1: #Force visit all nodes
#         m1.addConstrs((x.sum(i,"*")==1 for i in range(2*hh_num_trips+1)),"allnodemustbeserved")
    m1.addConstrs((T[j]-T[i]-B*x[i,j,ve]>=TT[i,j]-B 
        for i in range(2*hh_num_trips+2) 
        for j in range(2*hh_num_trips+2) 
        for ve in range(num_cav)),"precedencet15")
    # m1.addConstrs((T[i+hh_num_trips]-T[i]-B*x.sum(i,"*","*")>=(TT[i,i+hh_num_trips]-B) for i in range(1,hh_num_trips+1)),"deliverafterpickup16")
    m1.addConstrs((T[i+hh_num_trips]-T[i]>=(TT[i,i+hh_num_trips]) for i in range(1,hh_num_trips+1)),"deliverafterpickup16")
    m1.addConstrs((T[i+hh_num_trips]-T[i]<=share_ride_factor*TT[i,i+hh_num_trips] for i in range(1,hh_num_trips+1)),"triptimecannotexcceed1.5expectedtraveltime")
    
    # ####################################
    # #Late/Early Arrival penalty
    m1.addConstrs((S[i]>=early_penalty[i]*(expected_arrival_time[i]-T[i]) for i in range(2*hh_num_trips+2)),'earlyarrivalpenalty')
    m1.addConstrs((S[i]>=late_penalty[i]*(T[i]-expected_arrival_time[i]) for i in range(2*hh_num_trips+2)),'latearrivalpenalty')
    m1.addConstrs((S[i]>=2*early_penalty[i]*(expected_arrival_time[i]-T[i])-early_penalty[i]*early_penalty_threshold[i] for i in range(2*hh_num_trips+2)),'earlyarrivaloverthrespenalty')
    m1.addConstrs((S[i]>=2*late_penalty[i]*(T[i]-expected_arrival_time[i])-late_penalty[i]*late_penalty_threshold[i] for i in range(2*hh_num_trips+2)),'latearrivaloverthrespenalty')
#     m1.addConstrs((T[i]==expected_arrival_time[i] for i in [2*hh_num_trips]),'test123')
    # ####################################
    # Special Constraints for this problem

    m1.addConstrs((T[j]-T[i+hh_num_trips]-B*(x.sum("*",i,"*")+x.sum("*",j,"*")-1)>=-B 
                        for i in range(1,hh_num_trips+1) for j in range(i+1,hh_num_trips+1)
                        if sorted_trips.iloc[i-1]['person_id']==sorted_trips.iloc[j-1]['person_id']),
                        'personal_sequence_consistent'
                 ) # For the same person, the sequence of activity can not be violated
    
    m1.addConstrs((x[i,j,ve]==0 
                for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) for ve in range(num_cav)
                if expected_arrival_time[j]-expected_arrival_time[i]<-30)
                 ,'TripIearlerthanTripj') #
    # #####################################
    # Objective Function
    obj1=sum(x.sum(i,'*',"*")*R[i] for i in range(hh_num_trips+1))
    obj2=S.sum()
    obj3=sum(x.sum(i,j,"*")*C[i,j] for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2))
    obj4= sum(x.sum(i,'*','*')*30 for i in [0])
    if reward_mode==0 and time_window_flag !=1 :
        m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)
    elif num_cav>1:
        m1.setObjective(obj1-obj2-obj3,GRB.MAXIMIZE)
    else:
        m1.setObjective(obj1-obj2-obj3, GRB.MAXIMIZE)

    m1.setParam(GRB.Param.OutputFlag,output_flag)
    m1.Params.TIME_LIMIT=single_model_runtime
    m1.setParam(GRB.Param.MIPGap,0.01)
    # print('***********************')
    # print(R[9],C[9,20],C[8,19],C[10,21],C[19,10])
    # m1.addConstrs((x.sum(9,'*',ve)==0 for ve in range(num_cav)),'tempconstraints')
    # print('***********************')
    ##############################################################
    # Preprocessing
    for ve in range(num_cav):
        for i in range(1,hh_num_trips+1):
            m1.remove(x[0,i+hh_num_trips,ve])     #No trip go directly from depot to a delivery node
            m1.remove(x[i,2*hh_num_trips+1,ve])  #No trip go directly from a pickup node to depot
            m1.remove(x[hh_num_trips+i,i,ve])      #No trip go directly from a delivery node back to associated pickup node
            m1.remove(x[i,i,ve])                  #No trip between the same node
            for j in range(2*hh_num_trips+2):
                if (TT[i,j]+TT[j,i+hh_num_trips]>share_ride_factor*TT[i,i+hh_num_trips]):
                    m1.remove(x[i,j,ve])
                    m1.remove(x[j,i+hh_num_trips,ve])
        for i in range(hh_num_trips+1,2*hh_num_trips+2):         
            m1.remove(x[i,i,ve])
    ###############################################################
    #warm start with heuristic
    for i in range(2*hh_num_trips+2):
        for j in range(2*hh_num_trips+2):
            for ve in range(num_cav):
                x[i,j,ve].start=0
    # x["*","*","*"].start=0

    x[0,1,0].start=1
    x[1,1+hh_num_trips,0].start=1
    last_node=1+hh_num_trips
    for i in range(2*hh_num_trips+2):
        T[i].start=expected_arrival_time[i]
        S[i].start=0
    for i in range(2,1+hh_num_trips):
        if expected_arrival_time[last_node]+TT[last_node,i]<expected_arrival_time[i]:
            x[last_node,i,0].start=1
            x[i,i+hh_num_trips,0].start=1
            last_node=i+hh_num_trips
        else: 
            x[last_node,i,0].start=0
            x[i,i+hh_num_trips,0].start=0
    x[last_node,2*hh_num_trips+1,0].start=1

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
       
    return m1,x,T,obj1.getValue(),obj2.getValue(),obj3.getValue()





def break_route_to_seg(route_info,superzone_map):
    '''
    This function break the route of an av into segements for DYNASMART depends on the travelers. 
    The function is called by extract_route_from_model_solution
    '''
    route_info=route_info.sort_values(by=['hh_id','hh_vehicle_id','origin_arrival_time'])
    seg_index=[0]
    seg_temp=0
    last_hh_id=route_info.iloc[0]['hh_id']
    last_vehicle_id=0
    i=1
    intrasuperzone_flag=[0]*len(route_info)
    for index, row in route_info[1:].iterrows():   
        if row.hh_id != last_hh_id or row.hh_vehicle_id !=last_vehicle_id:
            seg_temp=0
            
            last_hh_id=row.hh_id
            last_vehicle_id=row.hh_vehicle_id
            last_hh_id=row.hh_id
        else:
            if(row.orig_zone==row.dest_zone or check_intrasuperzone(row.orig_zone,row.dest_zone,superzone_map)):
                seg_temp=seg_temp+1
            elif((row.person_id != route_info.iloc[i-1].person_id) 
               or (route_info.iloc[i-1].orig_zone==route_info.iloc[i-1].dest_zone)
                or (check_intrasuperzone(route_info.iloc[i-1].orig_zone,route_info.iloc[i-1].dest_zone,superzone_map))
                or (row.dest_zone==route_info.iloc[i-1].dest_zone)):
                seg_temp=seg_temp+1
            if(check_intrasuperzone(row.orig_zone,row.dest_zone,superzone_map) and row.orig_zone!=row.dest_zone ):
                intrasuperzone_flag[i]=1
        i+=1
        seg_index.extend([seg_temp])
        
        

    route_info=route_info.assign(seg_index=seg_index, intrasuperzone_flag=intrasuperzone_flag)
    route_info=route_info.assign(veh_seg_index=route_info.hh_id.astype(str)+
        "_"+route_info.seg_index.astype(str)+
        "_"+route_info.hh_vehicle_id.astype(str))
    return route_info

def check_intrasuperzone(orig_taz,dest_taz,superzone_map):
    '''
    This fucntion check if the trip between orig_taz and dest_taz is within the same superzone
    '''
#     print(superzone_map.loc[superzone_map.Original_Zones==orig_taz]['SuperZone'].item,superzone_map.loc[superzone_map.Original_Zones==dest_taz]['SuperZone'][0])
    return superzone_map[orig_taz]==superzone_map[dest_taz]
    
def veh_seg_index_creator(x):
    return str(int(x[0]))+'_'+str(int(x[1]))


def find_av_schedule_exact_method(target_hh_id,traveler_trips,output_flag,min_length,max_length,single_model_runtime,drivingcost_per_mile,
                         reward_mode,run_mode,cav_use_mode,num_time_interval,num_cav,share_ride_factor
                         ,time_window_flag,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,three_link_walk_dict,superzone_map,TL,TU,transit_zone_dict):
    target_hh=traveler_trips[traveler_trips['hh_id']==target_hh_id].drop_duplicates(subset=['orig_maz','dest_maz','orig_purpose',
        'dest_purpose','starttime','joint_trip_flag'])
    #Sort all trips based on start time. This step could reduce the solving time and make it easier to track
    sorted_trips=target_hh.sort_values("starttime")
    #hh_index give an index to all trips within the household for tracking purpose
    hh_num_trips=sorted_trips.shape[0]
    sorted_trips["hh_index"]=(range(hh_num_trips))
    num_hh_member,hh_num_trips,C,TT,expected_arrival_time,expected_leave_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,visit_candidate_zone\
    =prd.extract_hh_information(sorted_trips,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,superzone_map,drivingcost_per_mile,num_time_interval)
#     R=estimate_transit_cost(sorted_trips,TransitMazTazFlag,TransitSkimTimeIntervalLength,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,transit_zone_candidates,three_link_walk_dict)
    R=prd.estimate_trip_reward(hh_num_trips,sorted_trips,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,three_link_walk_dict,
        reward_mode,superzone_map,drivingcost_per_mile,transit_zone_dict,transit_zone_candidates,TransitMazTazFlag,TransitSkimTimeIntervalLength)
    m1,x,T,obj1_value,obj2_value,obj3_value=dial_n_ride_model(num_hh_member,hh_num_trips,C,TT,sorted_trips,
                expected_arrival_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,
                R,Vehicular_Skim_Dict,share_ride_factor,output_flag,run_mode,reward_mode,num_cav,cav_use_mode,time_window_flag,single_model_runtime)
    route_info=extract_route_from_model_solution(x,T,sorted_trips,visit_candidate_zone,hh_num_trips,expected_arrival_time,expected_leave_time,superzone_map,num_cav,num_time_interval,run_mode)
    if not route_info.empty:
        route_info=break_route_to_seg(route_info,superzone_map)
    return route_info

def get_route_info_allhh(traveler_trips,output_flag,min_length,max_length,single_model_runtime,drivingcost_per_mile,
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
    for target_hh_id in traveler_trips.hh_id.unique():
        # row_number=row_number+len(traveler_trips[traveler_trips.hh_id==household_id])
        if counter%100==0: 
            print('Estimate Route for the ',counter,'th household ',datetime.datetime.now())
        counter=counter+1
        target_hh=traveler_trips[(traveler_trips['hh_id']==target_hh_id)]\
        .drop_duplicates(subset=['orig_maz','dest_maz','orig_purpose','dest_purpose',
                                 'starttime','joint_trip_flag'])
        #Sort all trips based on start time. This step could reduce the solving time and make it easier to track
        sorted_trips=target_hh.sort_values("starttime")
        hh_num_trips=sorted_trips.shape[0]
        sorted_trips["hh_index"]=(range(hh_num_trips))
        
        # sorted_trips=sorted_trips.loc[sorted_trips.tripmode<=6]
        darp_solutions[target_hh_id]=solve_with_schedule_partition(sorted_trips,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,three_link_walk_dict,
                                    superzone_map,min_length,max_length,reward_mode,drivingcost_per_mile,share_ride_factor,output_flag,run_mode,num_cav,
                                    cav_use_mode,time_window_flag,single_model_runtime,num_time_interval,TL,TU,transit_zone_dict,transit_zone_candidates,
                                    TransitMazTazFlag,TransitSkimTimeIntervalLength)
        print(counter,hh_num_trips,datetime.datetime.now())
        route_info=darp_solutions[target_hh_id]['route_info']
        if not route_info.empty:
            route_infos=route_infos.append(route_info)
       
    return  route_infos,darp_solutions
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
        if run_mode<2:
            route_seq_dict={i:j for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) if x[i,j,ve].x > 0.5}
        else:
            route_seq_dict={i:j for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) for ti in num_time_interval if x[i,j,ve,ti].x > 0.5}
        route_node=[0]
        upstream_node=0
        while upstream_node in route_seq_dict:
            route_node.extend([route_seq_dict[upstream_node]])
            upstream_node=route_node[-1]
        route_node=route_node[:-1]
        travelers_set=set()
        travelers=[0]
        for node in route_node[:-1]:
            if node<hh_num_trips+1 and node>0:
                travelers_set.add(sorted_trips.iloc[node-1]['person_id'])
                travelers.extend([sorted_trips.iloc[node-1]['person_id']])
            elif node<2*hh_num_trips+1 and node>hh_num_trips:
                travelers_set.remove(sorted_trips.iloc[node-1-hh_num_trips]['person_id'])
                if travelers_set==set():
                    travelers.extend([0])
                else:
                    travelers.extend([list(travelers_set)[0]])
        vot=[0.01 if traveler==0 
             else sorted_trips[sorted_trips.person_id==traveler]['value_of_time'].iloc[0]
            for traveler in travelers]
        
        activity_time=[0 if travelers[i]==0 
             else max(0,expected_leave_time[route_node[i+1]]-expected_arrival_time[route_node[i+1]])
            for i in range(len(travelers))]
        start_time=[expected_leave_time[route_node[i-1]] for i in range(1,len(route_node))]
        origin_arrival_time=[T[route_node[i-1]].x for i in range(1,len(route_node))] 
        dest_arrival_time= [T[node].x for node in route_node[1:]]
        dest_expected_arrival_time=[expected_arrival_time[node] for node in route_node[1:]]
        route=[visit_candidate_zone[x] for x in route_node]
        
        route_info_temp=pd.DataFrame({'orig_zone':route[1:-1],'dest_zone':route[2:],'person_id':travelers[1:],
                                 'orig_node_index':route_node[1:-1],'dest_node_index':route_node[2:],
                                 'origin_arrival_time':origin_arrival_time[1:],'dest_arrival_time':dest_arrival_time[1:],
                                 'dest_expected_arrival_time':dest_expected_arrival_time[1:],'value_of_time':vot[1:],
                                 'start_time':start_time[1:],'Activity_Time':activity_time[1:],
                                 'hh_id':np.ones(len(route[1:-1]))*hh_id,'hh_vehicle_id':np.ones(len(route[1:-1]))*ve},
                                columns=['orig_zone','dest_zone','orig_node_index','dest_node_index',
                                         'person_id','origin_arrival_time','dest_arrival_time','dest_expected_arrival_time','value_of_time'
                                         ,'start_time','Activity_Time','hh_id','hh_vehicle_id'])
       
        # Drop the trip between a person's current destination and next trips's origin, as those are the same node
        route_info_temp=route_info_temp.loc[((route_info_temp.orig_node_index-route_info_temp.dest_node_index!=hh_num_trips-1)
                                   | (route_info_temp.orig_zone!=route_info_temp.dest_zone)
                                  |(route_info_temp.start_time!=route_info_temp.dest_expected_arrival_time)) ] 
    #     sorted_trips.iloc[max(i for i in [route_info_temp.orig_node_index,route_info_temp.orig_node_index-hh_num_trips] if i >0)].person_id !=
    # sorted_trips.iloc[max(i for i in [route_info_temp.dest_node_index,route_info_temp.dest_node_index-hh_num_trips] if i >0)].person_id
        
        route_info=route_info.append(route_info_temp)
        
    return route_info


###############################
#This section contains functions associated with the schedule partition heuristic
def find_break_point(sorted_trips,min_length,max_length,Vehicular_Skim_Dict,superzone_map):
    #Let i and i+1 be two consective trips in the sorted_trip list. 
    # gap_ratio=((arrival time at origin of i+1)-(arrival time at destination of i))/(travel time from destination of i to origin of i+1)

    gap_ratio=(sorted_trips.iloc[1:].starttime.values-sorted_trips.iloc[0:-1].starttime.values-\
        sorted_trips.iloc[0:-1].travel_time.values)/[ Vehicular_Skim_Dict[i][superzone_map[j]][1][1]['Time'] \
        for (i,j) in zip(sorted_trips.dest_taz.values[0:-1], sorted_trips.orig_taz.values[1:])]

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

def schedule_partition(sorted_trips,Vehicular_Skim_Dict,min_length,max_length,superzone_map):
    if len(sorted_trips)>min_length:
        break_point=find_break_point(sorted_trips,min_length,max_length,Vehicular_Skim_Dict,superzone_map)
        left_sub_trips=sorted_trips[0:break_point+1]
        right_sub_trips=sorted_trips[break_point+1:]
    #     print(break_point,len(left_sub_trips),len(right_sub_trips))
        #Split the trips to left_sub_trips and right_subtrips
        if max(len(left_sub_trips),len(right_sub_trips))>max_length:
            if len(left_sub_trips)>len(right_sub_trips): #Furthur split the longer subtrips
                return [schedule_partition(left_sub_trips,Vehicular_Skim_Dict,min_length,max_length,superzone_map),[right_sub_trips]]
            else: 
                return [[left_sub_trips],schedule_partition(right_sub_trips,Vehicular_Skim_Dict,min_length,max_length,superzone_map)]
        else:
            return [left_sub_trips,right_sub_trips]
    else:
        return [sorted_trips]
    
def flatten(sub_sorted_trips):
    if isinstance(sub_sorted_trips, pd.DataFrame):
        return [sub_sorted_trips]
    else:
        return [a for i in sub_sorted_trips for a in flatten(i)]
    
def solve_with_schedule_partition(sorted_trips,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,three_link_walk_dict,
    superzone_map,min_length,max_length,reward_mode,drivingcost_per_mile,share_ride_factor,output_flag,run_mode,num_cav,cav_use_mode,
    time_window_flag,single_model_runtime,num_time_interval,TL,TU,transit_zone_dict,transit_zone_candidates,TransitMazTazFlag,TransitSkimTimeIntervalLength):
#     sub_sorted_trips=[item for sublist in schedule_partition(sorted_trips,Vehicular_Skim,min_length,max_length) for item in sublist]
    
    route_info=pd.DataFrame()

    sub_sorted_trips=flatten(schedule_partition(sorted_trips,Vehicular_Skim_Dict,min_length,max_length,superzone_map))
    total_previous_sub_trips_length=int(0)
    total_tailing_sub_trips_length=int(0)
    schedule_deviation=[]
    total_reward=0
    total_schedule_penalty=0 #The total penalty for early/late arrival
    total_travel_cost=0

    for sub_sorted_trip in sub_sorted_trips:
        
        num_hh_member,hh_num_trips,C,TT,expected_arrival_time,expected_leave_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,visit_candidate_zone\
        =prd.extract_hh_information(sub_sorted_trip,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,superzone_map,drivingcost_per_mile,num_time_interval)
        # R=estimate_transit_cost(sorted_trips,TransitMazTazFlag,TransitSkimTimeIntervalLength,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,transit_zone_candidates,three_link_walk_dict)
        
        R=prd.estimate_trip_reward(hh_num_trips,sub_sorted_trip,Vehicular_Skim_Dict,Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,three_link_walk_dict,
            reward_mode,superzone_map,drivingcost_per_mile,transit_zone_dict,transit_zone_candidates,TransitMazTazFlag,TransitSkimTimeIntervalLength)
        
        # print('start sovling problem at ',datetime.datetime.now())
        if run_mode<2:
            m1,x,T,obj1_value,obj2_value,obj3_value=dial_n_ride_model(num_hh_member,hh_num_trips,C,TT,sub_sorted_trip,
                expected_arrival_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,
                R,Vehicular_Skim_Dict,share_ride_factor,output_flag,run_mode,reward_mode,num_cav,cav_use_mode,time_window_flag,single_model_runtime)
        else:
            m1,x,T,obj1_value,obj2_value,obj3_value=dial_n_ride_model_schedule_adjustment(num_hh_member,hh_num_trips,C,TT,TL,TU,sub_sorted_trip,
                expected_arrival_time,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim_Dict,
                share_ride_factor,output_flag,run_mode,reward_mode,num_cav,num_time_interval,cav_use_mode,time_window_flag,single_model_runtime)
               
        # print('finish solving problem at ',datetime.datetime.now())
        sub_route_info=extract_route_from_model_solution(x,T,sub_sorted_trip,visit_candidate_zone,hh_num_trips,expected_arrival_time,
            expected_leave_time,superzone_map,num_cav,num_time_interval,run_mode)
        total_tailing_sub_trips_length=int(len(sorted_trips)-total_previous_sub_trips_length-len(sub_sorted_trip))
        sub_route_info['orig_node_index']=sub_route_info.orig_node_index.apply(
            lambda x: x+total_previous_sub_trips_length if x<=hh_num_trips else x+2*total_previous_sub_trips_length+total_tailing_sub_trips_length) 
        sub_route_info['dest_node_index']=sub_route_info.dest_node_index.apply(
            lambda x: x+total_previous_sub_trips_length if x<=hh_num_trips else x+2*total_previous_sub_trips_length+total_tailing_sub_trips_length) 
        total_previous_sub_trips_length=int(total_previous_sub_trips_length+len(sub_sorted_trip))
        route_info=route_info.append(sub_route_info)
        total_reward+=obj1_value
        total_schedule_penalty+=obj2_value
        total_travel_cost+=obj3_value
        T_sol=np.ones(2*hh_num_trips+2)
        for i in range(2*hh_num_trips+2):
        #     print(int(T[i].x),'\t',expected_arrival_time[i],'\t',T[i].x-expected_arrival_time[i])
            T_sol[i]=T[i].x
        schedule_deviation.extend(T_sol-expected_arrival_time)

        # print(sub_route_info[['start_time','origin_arrival_time','orig_node_index','dest_node_index']])
        # print(sub_sorted_trip.starttime)
        
#         for index, row in route_info.iterrows():
#             print(route_info.dest_expected_arrival_time,'\t',row.dest_arrival_time,'\t',row.start_time,'\t',T[row.dest_node_index].x)
#         #Estimate the delay and early arrival
    if not route_info.empty:
        route_info=break_route_to_seg(route_info,superzone_map)
    route_info.orig_zone=route_info.orig_zone.apply(lambda x: int(x))
    route_info.dest_zone=route_info.dest_zone.apply(lambda x: int(x))
    darp_solution={}
    darp_solution['route_info']=route_info
    darp_solution['schedule_deviation']=sum(list(map(abs,schedule_deviation)))
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
    darp_solution['objective_value']=total_reward+ total_schedule_penalty+total_travel_cost
    darp_solution['hh_id']=sorted_trips.hh_id.iloc[0]
    return darp_solution

def solve_with_VNS(initial_route_info,num_hh_member,hh_num_trips,C,TT,sorted_trips,
                    expected_arrival_time,early_penalty,late_penalty,
                    early_penalty_threshold,late_penalty_threshold,R,Vehicular_Skim_Dict,
                    share_ride_factor,output_flag,run_mode,reward_mode,
                    num_cav,cav_use_mode,time_window_flag,single_model_runtime,max_iter ):
    '''
        After getting an initial solution, this function update the solution with a variable neighborhood heuristic
        Shacking method:
        M: Move neighborhood
        S: Swap neighborhood
        C: Chain neighborhood
        Z: Zero  split neighborhood
    '''
    #Initial heuristic
    iter_num=0
    x_sol,T_sol=extract_solution_from_route_info(initial_route_info,hh_num_trips,num_cav)
    #while stopping critiera is not met
    while iter_num<max_iter:
        #shaking
        for i in range(5):
            print(i)
            #move neighborhood
            #swap neighborhood
            #chain neighborhood
            #zero split neighborhood
        #local search
        #update neighborhood 
    return 
def extract_solution_from_route_info(initial_route_info,hh_num_trips,num_cav):
    x_sol=np.zeros((2*hh_num_trips+2,2*hh_num_trips+2,num_cav))
    T_sol=np.zeros((2*hh_num_trips+2))
    for i in range(num_cav): #Set the link to/from depot
        x_sol[0,initial_route_info.loc[initial_route_info.hh_vehicle_id==i].orig_node_index.iloc[0],i]=1
        x_sol[initial_route_info.loc[initial_route_info.hh_vehicle_id==i].dest_node_index.iloc[-1],2*hh_num_trips+1,i]=1
        print(initial_route_info.loc[initial_route_info.hh_vehicle_id==i].dest_node_index.iloc[-1],2*hh_num_trips+1,i)

    x_sol[[initial_route_info.orig_node_index,
           initial_route_info.dest_node_index,
           initial_route_info.hh_vehicle_id]]=1 #
    for i in range(num_cav):
        temp=initial_route_info.loc[initial_route_info.hh_vehicle_id==i]         
        x_sol[temp[:-1].dest_node_index,
              temp[1:].orig_node_index,
              temp[1:].hh_vehicle_id]=1
    x_sol[[i for i in range(2*hh_num_trips+2)],
    [i for i in range(2*hh_num_trips+2)],
    :]=0
    T_sol[initial_route_info.orig_node_index]=initial_route_info.origin_arrival_time
    T_sol[initial_route_info.dest_node_index]=initial_route_info.dest_arrival_time
    return x_sol,T_sol

def evaluate_solution(x_sol,T_sol,R,TT,C):

    return
def evaluate_penalty(T_sol,expected_arrival_time):

    return 
def move_neighborhood(x_sol,T_sol,initial_route_info,hh_num_trips,num_cav,shake_size,Vehicular_Skim_Dict,visit_candidate_zone,share_ride_factor):
    #From each route 
    
    np.random.seed(seed=1)
    for i in range(num_cav):
        # Randomly select n=shake_size requests
        target_request=np.random.choice(initial_route_info.loc[initial_route_info.hh_vehicle_id==i].orig_node_index,size=2)
        target_nodes=[target_request[j]-hh_num_trips if target_request[j]>hh_num_trips
                      else target_request[j]+hh_num_trips for j in range(shake_size)]
        
        target_nodes.extend([target_request[i] for i in range(shake_size)])
        target_nodes.sort()
        
        print('target nodes are',target_nodes)
        # Remove the request from the route
        x_sol=sod.random_remove_request(x_sol,target_nodes,i,hh_num_trips)
        #Randomly select another route 
        vehicle_list=list(range(num_cav))
        vehicle_list.remove(i)
        target_route=np.random.choice(vehicle_list)
        # Insert the node one by one
        for node in target_nodes[0:int(len(target_nodes)/2)]:
            #First inseart before the first trip
            upstream_node=0
            downstream_node=initial_route_info.loc[initial_route_info.hh_vehicle_id==target_route].orig_node_index.iloc[0]
            upstreamtimeinterval=1
            downstreamtimeinterval=math.ceil(T_sol[downstream_node]/20.0)
            best_tt=1440
            for ti in range(upstreamtimeinterval,downstreamtimeinterval):                
                tt=Vehicular_Skim_Dict[visit_candidate_zone[upstream_node]][visit_candidate_zone[node]][ti][1]
                if tt<best_tt:
                    best_tt=tt
                    best_ti=ti
            driving_time_limit=share_ride_factor*Vehicular_Skim_Dict[visit_candidate_zone[node]][visit_candidate_zone[node+hh_num_trips]][ti][1]

            for (upstream_node,downstream_node) in zip(initial_route_info.loc[initial_route_info.hh_vehicle_id==target_route].orig_node_index,initial_route_info.loc[initial_route_info.hh_vehicle_id==target_route].dest_node_index):
                x_sol_temp=x_sol
                T_sol_temp=T_sol
    #             x_sol_temp[upstream_node,,target_route]=
    #             x_sol_temp
    #             upstream_node=

    #Inseart the request to the route

    return

def random_remove_request(x_sol,target_nodes,vehicle_id,hh_num_trips):
    for node in target_nodes:
        upstream_node=np.where(x_sol[:,node,vehicle_id]==1)
        downstream_node=np.where(x_sol[node,:,vehicle_id]==1)
        if node==1:
            upstream_node=0
        elif node==2*hh_num_trips:
            downstream_node=2*hh_num_trips+1
        x_sol[upstream_node,downstream_node,vehicle_id]=1
        x_sol[upstream_node,node,vehicle_id]=0
        x_sol[node,downstream_node,vehicle_id]=0
    return x_sol



def optimal_start_time(sorted_trips,x_sol,T_sol,hh_num_trips,expected_arrival_time,Vehicular_Skim_Dict,
    visit_candidate_zone,early_penalty,late_penalty,early_penalty_threshold,late_penalty_threshold,superzone_map):
    m3=Model("Optimal_Start_Time")
    T=m3.addVars(2*hh_num_trips+2,name="T",vtype=GRB.CONTINUOUS) #T represent the expected arrivial time at a node
    S=m3.addVars(2*hh_num_trips+2,name="S") 
    x=m3.addVars(2*hh_num_trips+2,2*hh_num_trips+2,72,name='x',vtype=GRB.BINARY)
    m3.setObjective(S.sum(),GRB.MINIMIZE)
    m3.addConstrs((S[i]>=early_penalty[i]*(expected_arrival_time[i]-T[i]) for i in range(2*hh_num_trips+2)),'earlyarrivalpenalty')
    m3.addConstrs((S[i]>=late_penalty[i]*(T[i]-expected_arrival_time[i]) for i in range(2*hh_num_trips+2)),'latearrivalpenalty')
    m3.addConstrs((S[i]>=2*early_penalty[i]*(expected_arrival_time[i]-T[i])-early_penalty[i]*early_penalty_threshold[i] 
        for i in range(2*hh_num_trips+2)),'earlyarrivaloverthrespenalty')
    m3.addConstrs((S[i]>=2*late_penalty[i]*(T[i]-expected_arrival_time[i])-late_penalty[i]*late_penalty_threshold[i] 
        for i in range(2*hh_num_trips+2)),'latearrivaloverthrespenalty')
    m3.addConstrs((x.sum(i,j,'*')==x_sol[i,j] for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2)),'sameasx_sol')
    ti=list(range(1,73)) #number of skim time interval
    route_seq_dict={i:j for i in range(2*hh_num_trips+2) for j in range(2*hh_num_trips+2) if x_sol[i,j] > 0.5}
    upstream_node_index=0
    current_time=1
    T[0].start=current_time
    while upstream_node_index in route_seq_dict:
        downstream_node_index=route_seq_dict[upstream_node_index]
        if downstream_node_index==upstream_node_index-hh_num_trips+1 and upstream_node_index <2*hh_num_trips and\
         sorted_trips.iloc[upstream_node_index-hh_num_trips-1].person_id == sorted_trips.iloc[downstream_node_index-1].person_id:
                C=0
                TT=0
        else:
            TT=[Vehicular_Skim_Dict[visit_candidate_zone[upstream_node_index]][superzone_map[visit_candidate_zone[downstream_node_index]]][i][1]['Time'] for i in ti ]
            ti_temp=math.ceil(current_time/20)
            latest_ti=math.ceil(T_sol[down])
            if (early_penalty[downstream_node_index]==0 and late_penalty[downstream_node_index]==0):
                TT_temp=[Vehicular_Skim_Dict[visit_candidate_zone[downstream_node_index]]\
                [superzone_map[visit_candidate_zone[route_seq_dict[downstream_node_index]]]][i][1]['Time'] for i in ti ]
                
            else: #set the arrival time at the downstream node to the T_sol or the earliest possible arrival time (whichever is latter)
                
                current_time=max(current_time+TT[ti_temp-1],T_sol[downstream_node_index])
                T[downstream_node_index].start=current_time
                x[upstream_node_index,downstream_node_index,ti_temp-1].start=1   

            m3.addConstrs((T[upstream_node_index]+TT[k]*x[upstream_node_index,downstream_node_index,k]<=T[downstream_node_index] 
                for k in range(72)),'arrival_time'+str(upstream_node_index))
            m3.setPWLObj(T[upstream_node_index],ti,TT)
        upstream_node_index=downstream_node_index
    m3.update()
    m3.optimize()
    return m3,T,S         
