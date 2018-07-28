import numpy as np
import pandas as pd
import math
import os
import datetime
from Modules import Postprocess_DARP as pod
def read_vehicledat(vehicledat_filepath):
    '''
    This function read all sort of vehicledat file. It returns a dataframe vehicle_info that 
    contains all the information. Each row is corresponding to one vehicle in the vehicle.dat. 
    
    '''
    feature_list=['counter','iutmp','idtmp','StartTime','ivcltmp','ivcl2tmp','ihovtmp','veh_pathnodenum','NonrepetitiveCarNumTrip','infotmp','ribftmp','comptmp','TAZMap','value_of_time','value']
    i=0 #line number of vehicle.dat
    j=0 #line number of vehicle_info
    dest_chain=[]
    activity_chain=[]
    with open(vehicledat_filepath) as f: #Open the file
        for line in f:
            if i ==0: #Read the num_vehicle and max number of unit from the first line
                num_vehicle=int(line.split()[0])
                max_num_trips=int(line.split()[1])
                vehicle_info = pd.DataFrame(0,index=np.arange(num_vehicle), columns=feature_list)
            elif i >1:
                if (len(line)>30): #If the line is the first line of a vehicle, read the general information about vehicle
                    vehicle_info.iloc[j]=[float(k) for k in line.split()]
                    dest_chain.append([])
                    activity_chain.append([])
                    j=j+1
                else:
                    taz_temp,activity_time=line.split()
                    dest_chain[j-1].extend([taz_temp])
                    activity_chain[j-1].extend([activity_time])
                    
            i=i+1
        vehicle_info['dest_chain']=dest_chain
        vehicle_info['activity_time']=activity_chain
    return vehicle_info
def read_pathdat(pathdat_filepath):
    path_bank=[]
    with open(pathdat_filepath) as f:
        for line in f:
            path_bank.append([int(i) for i in line.split()])
    return path_bank
def read_intrasuperzone_files(intrasuperzone_vehicle_filepath,intrasuperzone_path_filepath):
    feature_list=['counter','iutmp','idtmp','StartTime','ivcltmp','ivcl2tmp','ihovtmp','veh_pathnodenum',
                  'NonrepetitiveCarNumTrip','infotmp','ribftmp','comptmp','TAZMap','value_of_time','value']
    i=0 #line number of vehicle.dat
    j=0 #line number of vehicle_info
    dest_chain=[]
    activity_chain=[]
    key=[]
    with open(intrasuperzone_vehicle_filepath) as f: #Open the file
        for line in f:
            if i ==0: #Read the num_vehicle and max number of unit from the first line
                num_vehicle=int(line.split()[0])
                max_num_trips=int(line.split()[1])
                vehicle_info = pd.DataFrame(0,index=np.arange(num_vehicle), columns=feature_list)
            elif i >1:
                if (len(line)>30): #If the line is the first line of a vehicle, read the general information about vehicle
                    vehicle_info.iloc[j]=[float(k) for k in line.split()]
                    
                    j=j+1
                else:
                    taz_temp,activity_time=line.split()
                    dest_chain.extend([taz_temp])
                    activity_chain.extend([activity_time])
                    key.append(tuple([int(vehicle_info.iloc[j-1]['TAZMap']),int(taz_temp)]))
            i=i+1
      
        vehicle_info['dest_chain']=dest_chain
        vehicle_info['activity_time']=activity_chain
        
    intrasuperzone_path=read_pathdat(intrasuperzone_path_filepath)
    intrasuperzone_info=vehicle_info
    intrasuperzone_info['path']=intrasuperzone_path
    intrasuperzone_path_dic=dict(zip(tuple(key),intrasuperzone_path))
    return intrasuperzone_info,intrasuperzone_path_dic
def route_to_vehiclepathdat(route_infos,origin_links,folder_filepath,vehicle_filepath,path_filepath,superzone_map,
                            intrasuperzone_path_dic,external_vehicle_filepath,average_value_of_time):
    '''
    This function write all the route information in the format of vehicle.dat
    '''

    route_infos=preprocess_routeinfo_for_vehicledat(route_infos)
    if not os.path.exists(folder_filepath):
        os.makedirs(folder_filepath)
    internal_mapdat=open(folder_filepath+'internal_map.dat','w')
    external_mapdat=open(folder_filepath+'external_map.dat','w')
    vehicledat=open(vehicle_filepath,'w')
    pathdat=open(path_filepath,'w')
    if os.path.isfile(external_vehicle_filepath):
        external_vehicle=open(external_vehicle_filepath,'r')
        num_external_vehicle=int(next(external_vehicle).split()[0])
        next(external_vehicle)
        excounter,exusec,exdsec,exstime,exusrcls,exvehtype,exioc,exonode,exintde,exinfo,exribf,excomp,exoz,temp,temp=next(external_vehicle).split()
        exTAZMap,exactivitytime=next(external_vehicle).split()
        exstime=float(exstime)+180 #External vehicle start from 0:00am but the abm file start from 3 am
    else:
        exstime=1555
    
    num_veh_seg=len(route_infos.veh_seg_index.unique())
    max_num_trip=route_infos.groupby(['veh_seg_index'])['start_time'].count().max()
    total_veh_seg=num_veh_seg+num_external_vehicle
    vehicledat.write(str(total_veh_seg)+'\t'+str(max_num_trip)+'\t'+'# of vehicles in the file, Max # of stops\n')
    vehicledat.write('counter  iutmp  idtmp  StartTime  ivcltmp  ivcl2tmp  ihovtmp  veh_pathnodenum  NonrepetitiveCarNumTrip  infotmp  ribftmp  comptmp  TAZMap  value_of_time  1\n')
    counter=1
    multiindex_route_infos=route_infos.set_index(['veh_seg_index'])
    route_infos.sort_values(by=['origin_arrival_time'],inplace=True)
    # for veh_seg in route_infos.sort_values('origin_arrival_time')['veh_seg_index'].unique():
    for (first_trip_starttime,veh_seg),target_seg in route_infos.groupby(['first_trip_start_time','veh_seg_index']):
        # print(1,datetime.datetime.now())
        # # target_seg=multiindex_route_infos.loc[[veh_seg]]
        # print(2,datetime.datetime.now())
        # print(first_trip_starttime,target_seg.origin_arrival_time.min())
        while target_seg.origin_arrival_time.min()>exstime:
            
            vehicledat.write(str(counter)+'\t'+exusec+'\t'+exdsec+'\t'+str(exstime)+'\t'+
                             exusrcls+'\t'+exvehtype+'\t'+exioc+'\t'+
                             '1'+'\t'+exintde+'\t'+exinfo+'\t'+exribf+'\t'+excomp+'\t'+
                             exoz+'\t'+str(average_value_of_time)+'\t'+'1'+'\n')
            vehicledat.write(exTAZMap+'\t'+exactivitytime+'\n')
            internal_mapdat.write(str(counter)+'\t'+'0'+'\n')
            internal_mapdat.write('0')
            external_mapdat.write(str(counter)+'\t'+excounter+'\n')

            pathdat.write('\n')
            counter +=1
            line_listtemp=next(external_vehicle).split()
            
            if len(line_listtemp)>3:
                excounter,exusec,exdsec,exstime,exusrcls,exvehtype,exioc,exonode,exintde,exinfo,exribf,excomp,exoz,temp,temp=line_listtemp
                exTAZMap,exactivitytime=next(external_vehicle).split()
                exstime=float(exstime)+180 #External vehicle start from 0:00am but the abm file start from 3 am
            else: 
                exstime=1555
        
        write_one_veh_seq(target_seg,vehicledat,pathdat,
            internal_mapdat,external_mapdat,counter,origin_links,superzone_map,intrasuperzone_path_dic)
        # print(4,datetime.datetime.now())
        counter +=1
        if counter%10000==0: 
            print(counter,datetime.datetime.now())
    vehicledat.close()
    pathdat.close()
    return
def write_one_veh_seq(route_info,file_obj,path_file_obj,internal_mapdat,
    external_mapdat,counter,origin_links,superzone_map,intrasuperzone_path_dic):
    '''
    This function convert the route information of one segment into vehicle.dat format. The function is called by route_to_vehiclepathdat
    '''
    ivcl2tmp=1        #Standard Input for Vehicle Specifications 
    ihovtmp=1         #Standard Input for Vehicle Specifications  
    veh_pathnodenum=1 #Standard Input for Vehicle Specifications
    ndestmp=1         #Standard Input for Vehicle Specifications
    infotmp=0         #Standard Input for Vehicle Specifications 
    ribftmp=0.0       #Standard Input for Vehicle Specifications  
    comptmp=0.0       #Standard Input for Vehicle Specifications  
   
    if (route_info.iloc[0].intrasuperzone_flag==1):
        path_temp=intrasuperzone_path_dic[(route_info.iloc[0]['orig_zone'],route_info.iloc[0]['dest_zone'])]
        orig_u_node,orig_d_node=path_temp[0:2]
        ivcltmp=1
        veh_pathnodenum=len(path_temp)
        path_file_obj.write(''.join( str(j).rjust(7) for j in intrasuperzone_path_dic[(route_info.iloc[0]['orig_zone'],route_info.iloc[0]['dest_zone'])])+'\n')
    else:
        target_zone=origin_links.loc[origin_links.zone_id==route_info.iloc[0]['orig_zone']]
        # print(list(range(len(target_zone))),list(target_zone.length/target_zone.length.sum()))
        generateion_link_index=np.random.choice(list(range(len(target_zone))),p=list(target_zone.length/target_zone.length.sum()))
        orig_u_node= target_zone.iloc[generateion_link_index]['u_node'] #Upstream node of the generation link 
        orig_d_node= target_zone.iloc[generateion_link_index]['d_node'] #Downstream node of the generation link
        ivcltmp=3
        veh_pathnodenum=1
        path_file_obj.write('\n')
        # if (route_info.iloc[0].person_id==0) and (len(route_info)==1):
        #     ivcltmp=2

    internal_mapdat.write(str(counter)+'\t'+route_info.iloc[0].veh_seg_index+'\n')
    
    external_mapdat.write(str(counter)+'\t'+'0'+'\n')

    file_obj.write(str(counter)+'\t'+str(int(orig_u_node))+'\t'
                   +str(int(orig_d_node))+'\t'+str(round(route_info.iloc[0]['origin_arrival_time'],1))+'\t'
                   +str(ivcltmp)+'\t'+str(ivcl2tmp)+'\t'+str(ihovtmp)+'\t'+str(veh_pathnodenum)+'\t'
                   +str(int(len(route_info)))+'\t'+str(infotmp)+'\t'+str(ribftmp)+'\t'
                   +str(comptmp)+'\t'+ str(route_info.iloc[0]['orig_zone'])+'\t'
                   +str(route_info.iloc[0]['value_of_time'])+'\t'+'1'+'\n')
    for (index,row) in route_info.iloc[:-1].iterrows():
        file_obj.write(str(row['dest_zone'])+'\t'+str(row['Activity_Time'])+'\n')
        internal_mapdat.write(str(row.origin_arrival_time)+'\n')
    file_obj.write(str(route_info['dest_zone'].iloc[-1])+'\t'+'0.0\n')
    internal_mapdat.write(str(route_info['origin_arrival_time'].iloc[-1])+'\n')
    if (route_info.dest_zone.iloc[-1]<1):
        print('error')
    return

def preprocess_routeinfo_for_vehicledat(route_info):
    route_info.sort_values(by=['origin_arrival_time'],inplace=True)
    print(datetime.datetime.now())
    earliest_time={}
    earliest_time_list=[]
    for index,row in route_info.iterrows():
        if row.veh_seg_index not in earliest_time:
            earliest_time[row.veh_seg_index]=row.origin_arrival_time
        earliest_time_list.extend([earliest_time[row.veh_seg_index]])
    route_info['first_trip_start_time']=earliest_time_list
    route_info.dest_zone=route_info.dest_zone.apply(lambda x: int(x))
    route_info.Activity_Time=route_info.Activity_Time.apply(lambda x: round(x,1))
    print(datetime.datetime.now())
    return route_info

def save_run_result(run_name,route_info,darp_solutions,output_filepath):
    output_filepath=output_filepath+run_name
    if not os.path.exists(output_filepath):
        os.makedirs(output_filepath)
    route_info.to_csv(output_filepath+'route_info.csv')
    pod.save_obj(darp_solutions,'darp_solutions',output_filepath)

    return

def write_darp_solution_to_file(run_name,output_filepath,route_info,darp_solutions,origin_links,
    superzone_map,intrasuperzone_path_dic,average_value_of_time,external_factor):
    # average_value_of_time=0.16
    # save_run_result(run_name,route_info,darp_solutions,output_filepath)
    output_filepath=output_filepath+run_name+str(external_factor)+'/'
    vehicle_filepath=output_filepath+'vehicle.dat'
    path_filepath=output_filepath+'path.dat'
    external_vehicle_filepath='Input/external_vehicle'+str(external_factor)+'.dat'
    # average_value_of_time=round(traveler_trips.value_of_time.mean(),4)
    
    
    route_to_vehiclepathdat(route_info,origin_links,output_filepath,vehicle_filepath,path_filepath,superzone_map,
                                intrasuperzone_path_dic,external_vehicle_filepath,average_value_of_time)
    return



