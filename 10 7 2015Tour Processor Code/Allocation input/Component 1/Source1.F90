program Console3
!Variables and Parameters Clarifications
    integer::hh_id, person_id, person_num, tour_id, stop_id, inbound, orig_maz, orig_walk_segment, dest_maz, dest_walk_segment, parking_taz, stop_period, trip_mode, tour_mode, board_tap, alight_tap, orig_taz, dest_taz
    integer::SpaceFinder, error, index,MaxPersonID,WorkCounter, EndofLoop,MaxNumTravellerInEachTimeInterval,MaxNumTimeInterval,MaxTAZ,NumTAZ,NumTraveller,MaxNumTrips,RealMaxNumTrips,counter
    integer::noofnodes_old,noofarcs_old,nzones_old,zone_old,IDGen,iu_new,id_new,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,ndestmp,infotmp,ribftmp,comptmp,temp2,iutmp,idtmp,driver,jjj,exTAZMap
    integer::EndLoop,EndLoop1,EndLoop2,trip_driver_passenger,stimedifference,exusec,exdsec,exusrcls,exvehtype,exioc,exonode,exintde,exinfo,exoz,ExNumCar,m,MaxNumInternalTAZZone
    integer:: jj1,jj2,excounter,VehicleLastTrip,abm_trip_index,AlexCheck,ParkRideFoundFlag,j,k,MaxHouseHoldID,MaxHHSize,person_id_of_last_traveler,maxnumtripstemp
    character(300)::Line   
    character(30):: tour_category
    character(15):: tour_purpose, orig_purpose, dest_purpose,Home, work1,work2,OriginPurposeTemp,DestPurposeTemp
    character(15),allocatable::TravellerTripOriginPurpose(:,:),TravellerTripDestinationPurpose(:,:)
           
    integer,allocatable::Traveller_index(:),Traveller_ID(:),TravellerNumTrips(:),TravellerTripMode(:,:),TravellerTripOriginMAZ(:,:),TravellerTripOriginTAZ(:,:),TravellerTripDestinationMAZ(:,:),TravellerTripDestinationTAZ(:,:),TravellerTripStartTimeInterval(:,:),TripDriverPassengerFlag(:,:),FirstTransitPassengerTripStartTimeInterval(:)
    integer,allocatable::TAZMap(:),ValidTraveller(:),NumTravellerInTimeInterval(:),TravellerInEachTimeInterval(:,:),TravellerNumFirstTimeIntervalTrip(:),TransitPassengerNumFirstTimeIntervalTrip(:)
    integer,allocatable::nodenum(:),izone(:),idnum(:),iu(:),id(:),MTbay(:),MTbayR(:),i3(:),nlanes(:),FlowModelNum(:),Vfadjust(:),SpeedLimit(:),mfrtp(:),sattp(:),link_iden(:), LGrade(:),node_to_link(:,:),LinkWeight(:),idnod(:),iunod(:),NoofGenLinksPerZone_old(:),ZoneTotalOriginLaneLength(:),ZoneGenerationLink(:,:)
    integer,allocatable::jjtemp(:),jjtemp1(:),ftjjtemp(:),NumTransitPassengerInTimeInterval(:),FirstTransitTrip(:),ABMTripIndex(:,:),MAZ(:),TAZ(:)
    real,allocatable:: TravellerStartTime(:),TripStartTime(:,:),VectorLengthofTimeInterval(:),VectorStartTimeofTimeInterval(:),TransitPassengerStartTime(:)
    real:: StartTimeofTimeInterval, LengthofTimeInterval,StartTime,exribf,excomp,exstime,exactivitytime
    real:: LWTmp,ActivityTime,ActivityTimeTemp,temp1,temp3,trip_travel_time
!Car (Changes for considering vehicles in addition to the travellers)
    integer,allocatable:: ValidCar(:),FirstCarTrip(:,:),CarNumFirstTimeIntervalTrip(:),NumCarInTimeInterval(:),CarInEachTimeInterval(:,:),CarIndexInEachTimeInterval(:,:),TransitPassengerInEachTimeInterval(:,:)
    real,allocatable:: CarStartTime(:),ValueOfTime(:),TravellerTripTravelTime(:,:)
    real::CumulativeActivityTime
!Joint (Changes for considering Joint in addition to the vehicles and travellers)
    character(500)::JointTourLine 
    character(1),allocatable::JointTourCharacter(:)
    integer::JointTourMaxNumCharacter,SpaceCount,i,tour_composition,depart_period,arrive_period,num_ob_stops,num_ib_stops,num_participants,MaxJointCarTrip
    integer,allocatable::tour_participants(:),PersonIDOfHH_PN(:,:),NumTourParticipant(:,:),PersonNumOfHH_TourID(:,:,:),TravellerTripDriver(:,:),CarNumTrip(:),Traveller_HH_ID(:),TravellerTripTourID(:,:),TravellerTripJointFlag(:,:)
    real,allocatable::util(:),prob(:)
    character(10)::gender,type1,activity_pattern
    integer::NumHouseHold,hh_idtemp
write(*,*) "Initial Input Variables for TourProcessor"
open(file='indivTripData_1.csv',unit=1) 
open(file='jointTripData_1.csv',unit=2)
open(file='AllocationInput.dat',unit=3)
open(file='network.dat',unit=4)
open(file='system.dat',unit=5)
open(file='scenario.dat',unit=6)
open(file='error.dat',unit=7)
!For checking purpose
!open(file='taz.csv',unit=4)
!open(file='maz.csv',unit=5)
!open(file='parktaz.csv',unit=6)
!allocate(MAZ(16820))
!    MAZ(:)=0
!allocate(TAZ(1950))
!    TAZ(:)=0
read(1,*)       
 MaxTAZ=0
NumMAZ=0
MaxPersonID=0
MaxNumTraveller=0
MaxNumTrips=1
MaxNumTimeInterval=0
MaxNumInternalTAZZone=0
MaxHouseHoldID=0
MaxHHSize=0
person_id_of_last_traveler=0
error=1
maxnumtripstemp=1
!For check
check1=1
check2=1
tazlength=1
mazlength=1
maxtripmode=0
NumIndivTrips=0
NumJointTrips=0 
    do while (error.ne.-1)   
        read(1,'(a)',iostat=error) Line
        if (error.ne.-1) then
             read(Line,*,iostat=error) hh_id, person_id, person_num, tour_id, stop_id, inbound, tour_purpose, orig_purpose, dest_purpose, orig_maz, orig_walk_segment,orig_park_maz,dest_maz, dest_walk_segment, parking_taz, stop_period, trip_mode, tour_mode, tour_category, board_tap, alight_tap, orig_taz, dest_taz,orig_park_taz,dest_park_taz,trip_driver_passenge
             NumIndivTrips=NumIndivTrips+1
             if(maxtripmode.lt.trip_mode) maxtripmode=trip_mode
             !if(orig_taz.gt.MaxTAZ.or.dest_taz.gt.MaxTAZ) MaxTAZ=max(orig_taz,dest_taz)
             if(orig_maz.gt.NumMAZ.or.dest_maz.gt.NumMAZ) NumMAZ=max(orig_maz,dest_maz)
             if(person_id.gt.MaxPersonID) MaxPersonID=person_id
             if(stop_period.gt.MaxNumTimeInterval) MaxNumTimeInterval=stop_period
             !if(stop_period.gt.41) write(*,*) hh_id, person_id, person_num
             if(person_id.ne.person_id_of_last_traveler)then
                 person_id_of_last_traveler=person_id
                 if(maxnumtripstemp.gt.MaxNumTrips) MaxNumTrips=maxnumtripstemp
                 maxnumtripstemp=1
            else
                maxnumtripstemp=maxnumtripstemp+1
            endif
            if(hh_id.gt.MaxHouseHoldID) MaxHouseHoldID=hh_id
            if (person_num.gt.MaxHHSize) MaxHHSize=person_num
        endif
    enddo
    
    !-----------------------------------------
    error=1
    read(2,*)
    do while (error.ne.-1)   
        read(2,'(a)',iostat=error) Line
        if (error.ne.-1) then
             read(Line,*,iostat=error) hh_id, tour_id, stop_id, inbound, tour_purpose, orig_purpose, dest_purpose, orig_maz, orig_walk_segment, orig_park_maz,dest_maz, dest_walk_segment,parking_taz, stop_period, trip_mode, num_participants, tour_mode, tour_category, board_tap, alight_tap, orig_taz, dest_taz,orig_park_taz,dest_park_taz, trip_driver_passenger
            NumJointTrips=NumJointTrips+1
             !if(orig_taz.gt.MaxTAZ.or.dest_taz.gt.MaxTAZ) MaxTAZ=max(orig_taz,dest_taz)
             if(orig_maz.gt.NumMAZ.or.dest_maz.gt.NumMAZ) NumMAZ=max(orig_maz,dest_maz)
             
             if(stop_period.gt.MaxNumTimeInterval) MaxNumTimeInterval=stop_period
             !if(stop_period.gt.41) write(*,*) hh_id, person_id, person_num
             
            if(hh_id.gt.MaxHouseHoldID) MaxHouseHoldID=hh_id
            
        endif
    enddo  
    
    error=1
    person_id_of_last_traveler=0
    open(file='personData_1.csv',unit=21)
    read(21,*,iostat=error)
    zz=0
    TotalValueoftime=0
    hh_idtemp=0
    NumHouseHold=0
    if (error.ne.-1) then      
        do while (error.ne.-1)   
            read(21,'(a)',iostat=error) Line
            if (error.ne.-1) then
                read(Line,*,iostat=error) hh_id,person_id,person_num,age,gender,type1,value_of_time,fp_choice,activity_pattern,imf_choice,inmf_choice,walk_time_weight,walk_speed,max_walk,user_class_work_walk,user_class_work_pnr,user_class_work_knr,user_class_non_work_walk,user_class_non_work_pnr,user_class_non_work_knr
                if(person_num.gt.MaxHHSize) MaxHHSize=person_num
                if(person_id.ne.person_id_of_last_traveler )then 
                    MaxNumTraveller=MaxNumTraveller+1
                    person_id_of_last_traveler=person_id
                    TotalValueoftime=TotalValueoftime+nint(value_of_time/60*10000.0)/10000.0
                    zz=zz+1
                endif
                if(hh_id.gt.MaxHouseHoldID) MaxHouseHoldID=hh_id
                if(person_num.gt.MaxHHSize) MaxHHSize=person_num
                if(person_id.gt.MaxPersonID) MaxPersonID=person_id
                if(hh_id.ne.hh_idtemp) then 
                    NumHouseHold=NumHouseHold+1
                    !write(*,*) NumHouseHold,hh_id,hh_idtemp,hh_id-hh_idtemp
                    hh_idtemp=hh_id
                    
                endif
            endif
        enddo
    endif
    close(21)   
    average_value_of_time=TotalValueoftime/zz
!For network.dat
read(4,*) MaxTaz,NumNode,NumLink,kay,SuperZoneSwitch

!System.dat
read(5,*) MaxTime
read(5,*) NumIteration,GMode
read(5,*) AggregationTime,AssignmentTime,value,value

read(6,*) 
read(6,*)
read(6,*)
read(6,*)CalculationTime,SimulationTime
if(NumIteration.eq.0) then
    LinkTimeInterval=CalculationTime*6/60.0
    SkimTimeInterval=LinkTimeInterval
else
    LinkTimeInterval=AggregationTime*6/60.0
    SkimTimeInterval=AssignmentTime*6/60.0
endif

write(3,*)MaxTAZ
write(3,*)NumMaz
write(3,*)MaxPersonID
write(3,*)MaxNumTraveller
write(3,*)MaxNumTrips+4
write(3,*)MaxNumTimeInterval
write(3,*)MaxHouseHoldID
write(3,*)MaxHHSize
write(3,*)average_value_of_time
write(3,*)NumNode
write(3,*)NumLink
write(3,*)LinkTimeInterval
write(3,*)SkimTimeInterval
write(3,*)NumIteration
write(3,*)NumHouseHold
write(3,*)NumIndivTrips
write(3,*)NumJointTrips
end program Console3
    