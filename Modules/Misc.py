import numpy as np
def identify_indiv_trip_before_jointtrip(traveler_trips):
    before_joint_trip=pd.DataFrame()
    for traveler in traveler_trips.person_id.unique():
#         print(traveler,len(traveler_trips[traveler_trips.person_id==traveler])-1)
        for index in range(len(traveler_trips[traveler_trips.person_id==traveler])-1):
#             if(traveler==5982639):
#                 print(traveler,index,(traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index]['joint_trip_flag']==0),(traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index+1]['joint_trip_flag']==1)) 
            if (traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index]['joint_trip_flag']==0) \
            & (traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index+1]['joint_trip_flag']==1):  
#                 print(traveler,index)
                before_joint_trip=before_joint_trip.append(traveler_trips[traveler_trips.person_id==traveler].iloc[index])
    check_joint_trip=traveler_trips.loc[(traveler_trips['joint_trip_flag']==1) & (traveler_trips.trip_counter>2)]
    
#     if (len(check_joint_trip)/2 != len(before_joint_trip)):
#         print(traveler_trips.hh_id.unique(),len(check_joint_trip)/2, len(before_joint_trip))
    return before_joint_trip

#Derive the travel time matrix from google api
#origin_node=[]
#destination_node=[]
#travel_time=[]
#k=0
#for i in nodexy:
#    print(k)
#    k=k+1
#    for j in nodexy:
#        origin_node.extend([i])
#        destination_node.extend([j])
#        travel_time.extend([travel_time_between_nodes(int(i),int(j),nodexy)])
 
def estimate_num_vehicle_household(sorted_trips,method_flag):
    '''
    This function estimate the minimum number of AVs needed for a household
    method_flag=0: use av for all trips
    method_flag=1: use av only for trips previously conducted by car
    method_flag=2: use conventional vehicles for all trips
    method_flag=3: use conventional vehicles only for car trips 
    '''
    min_vector=np.zeros(1440)
    if method_flag==0: #Use AV for all trips
        for i, row in sorted_trips.iterrows():
            min_vector[int(row.starttime-1):int(row.starttime-1+row.travel_time)]+=1
    elif method_flag==1: #Use AV for car trips only
        for i, row in sorted_trips.loc[sorted_trips.tripmode<=6].iterrows():
            min_vector[int(row.starttime-1):int(row.starttime-1+row.travel_time)]+=1
    elif method_flag==2: #Conventional car for all trips
        for person in sorted_trips.person_id.unique():
            for index, row in sorted_trips.loc[sorted_trips.person_id==person].iterrows():
                if row.orig_purpose=='Home':
                    start_temp=row.starttime-1
                elif row.dest_purpose=='Home':
                    end_temp=row.starttime+row.travel_time-1
                    min_vector[int(start_temp):int(end_temp)]+=1
    elif method_flag==3: #Conventional car for car trips
        for person in sorted_trips.person_id.unique(): #For each person in the household
                start_temp=0
                end_temp=0
                #Go through all the car trips of the person
                for index, row in sorted_trips.loc[(sorted_trips.person_id==person) & (sorted_trips.tripmode<=6)].iterrows():
                    #Find the time that the traveler is away from home with a car
                    if row.orig_purpose=='Home':
                        start_temp=row.starttime-1
                    elif row.dest_purpose=='Home':
                        if start_temp==0:
                            start_temp=row.starttime
                        end_temp=row.starttime+row.travel_time-1
                        min_vector[int(start_temp):int(end_temp)]+=1
        if max(min_vector)==0: #Some trips do not have return trip to the home
            min_vector[0]=1
    return max(min_vector)

# def math_orig_dest_nodes(): 
#     current_traveler=0
#     orig_nodes=[]
#     dest_nodes=[]
#     home_node=0
#     for index,row in target_trips[target_trips.hh_id==target_hh_id].iterrows():
#         if row.orig_purpose=='Home' and home_node=0:
#             home_node=home_node
        
#         if row.person_id != current_traveler:
#             orig_nodes.extend([row.origin_node])
#             dest_nodes.extend([row.destination_node])
#             last_dest_node=row.destination_node
#             current_traveler=row.person_id
#         else:
#             orig_nodes.extend([last_dest_node])
#             dest_nodes.extend([row.destination_node])
#             last_dest_node=row.destination_node    
#   return


# def read_file_and convert_to_vehicle.dat():
# route_info_mv=pd.read_csv('Output/AllHH/MV/route_info.csv')
# darp_solutions_mv=pod.load_obj('darp_solutions','Output/AllHH/MV/' )
# external_factor='50'
# average_value_of_time=round(car_trips.value_of_time.mean(),4)
# dtd.write_darp_solution_to_file('MV/','Output/AllHH/',route_info_mv,darp_solutions_mv,origin_links,
#         superzone_map,intrasuperzone_path_dic,average_value_of_time,external_factor)   
#   return

# def run one test scenario for all household():
# output_flag=0
# # target_hh_id=2744264
# min_length=15
# max_length=20
# single_model_runtime=60*3
# drivingcost_per_mile=0.5
# reward_mode=1
# run_mode=0
# darp_solutions=[]
# cav_use_mode=0
# num_time_interval=1
# TL=[i*1440/num_time_interval for i in range(num_time_interval)]
# TU=[(i+1)*1440/num_time_interval for i in range(num_time_interval)]
# num_cav=1
# share_ride_factor=1.5
# time_window_flag=0

# print('********************************')
# print(share_ride_factor)
# # target_trips=car_trips_sample[car_trips_sample.hh_id==1486727]

# route_info_modechoice,darp_solutions_modechoice=\
# bsod.get_route_info_allhh(car_transit_trips,
#                          output_flag,min_length,max_length,single_model_runtime,drivingcost_per_mile,
#                          reward_mode,run_mode,cav_use_mode,num_time_interval,num_cav,share_ride_factor
#                          ,time_window_flag,Vehicular_Skim_Dict,
#                          Transit_AB_Cost_Skim_Dict,Transit_AB_Time_Skim_Dict,three_link_walk_dict,
#                          superzone_map,TL,TU,
#                          transit_zone_dict,transit_zone_candidates,TransitMazTazFlag,
#                          TransitSkimTimeIntervalLength )
# output_filepath='Output/AllHH/'
# dtd.save_run_result('MO/',route_info_modechoice,darp_solutions_modechoice,output_filepath)
# # pod.analysis_network_level_results(route_info,darp_solutions,car_trips_sample,
# #                                    Vehicular_Skim_Dict,superzone_map,drivingcost_per_mile)