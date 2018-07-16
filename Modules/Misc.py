def identify_indiv_trip_before_jointtrip(traveler_trips):
    before_joint_trip=pd.DataFrame()
    for traveler in traveler_trips.person_id.unique():
#         print(traveler,len(traveler_trips[traveler_trips.person_id==traveler])-1)
        for index in range(len(traveler_trips[traveler_trips.person_id==traveler])-1):
#             if(traveler==5982639):
#                 print(traveler,index,(traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index]['joint_trip_flag']==0),(traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index+1]['joint_trip_flag']==1)) 
            if (traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index]['joint_trip_flag']==0) & (traveler_trips.loc[traveler_trips.person_id==traveler].iloc[index+1]['joint_trip_flag']==1):  
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
    if method_flag==0: 
        for i, row in sorted_trips.iterrows():
            min_vector[int(row.starttime-1):int(row.starttime-1+row.travel_time)]+=1
    elif method_flag==1:
        for i, row in sorted_trips.loc[sorted_trips.tripmode<=6].iterrows():
            min_vector[int(row.starttime-1):int(row.starttime-1+row.travel_time)]+=1
    elif method_flag==2:
        for person in sorted_trips.person_id.unique():
            for index, row in sorted_trips.loc[sorted_trips.person_id==person].iterrows():
                if row.orig_purpose=='Home':
                    start_temp=row.starttime-1
                elif row.dest_purpose=='Home':
                    end_temp=row.starttime+row.travel_time-1
                    min_vector[int(start_temp):int(end_temp)]+=1
    elif method_flag==3:
        for person in sorted_trips.person_id.unique():
                for index, row in sorted_trips.loc[(sorted_trips.person_id==person) & (sorted_trips.tripmode<=6)].iterrows():
                    if row.orig_purpose=='Home':
                        start_temp=row.starttime-1
                    elif row.dest_purpose=='Home':
                        end_temp=row.starttime+row.travel_time-1
                        min_vector[int(start_temp):int(end_temp)]+=1
    
    return max(min_vector)

def math_orig_dest_nodes: 
    current_traveler=0
    orig_nodes=[]
    dest_nodes=[]
    home_node=0
    for index,row in target_trips[target_trips.hh_id==target_hh_id].iterrows():
        if row.orig_purpose=='Home' and home_node=0:
            home_node=home_node
        
        if row.person_id != current_traveler:
            orig_nodes.extend([row.origin_node])
            dest_nodes.extend([row.destination_node])
            last_dest_node=row.destination_node
            current_traveler=row.person_id
        else:
            orig_nodes.extend([last_dest_node])
            dest_nodes.extend([row.destination_node])
            last_dest_node=row.destination_node       