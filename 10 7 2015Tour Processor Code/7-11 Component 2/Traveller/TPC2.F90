Module TC1Output
use vector_mod

CONTAINS

SUBROUTINE WriteVeNTr !(PersonIDOfHH_PN,ValueOfTime,SuperZoneMap,MaztoTazMap,TaztoMazMap,NumTransitZoneCa,TransitZone
!use iso_c_binding
!implicit none
!-------------------------------------------------    
!Variables and Parameters Clarifications
    integer::hh_id, person_id, person_num, tour_id, stop_id, inbound, orig_maz, orig_walk_segment, dest_maz, dest_walk_segment, parking_taz, stop_period, trip_mode, tour_mode, board_tap, alight_tap, orig_taz, dest_taz
    integer::SpaceFinder, error, index,MaxPersonID,WorkCounter, EndofLoop,MaxNumTravellerInEachTimeInterval,MaxNumTimeInterval,MaxTAZ,NumTAZ,NumTraveller,MaxNumTrips,RealMaxNumTrips,counter
    integer::noofnodes_old,noofarcs_old,nzones_old,zone_old,IDGen,iu_new,id_new,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,ndestmp,infotmp,ribftmp,comptmp,temp2,iutmp,idtmp,driver,jjj,exTAZMap
    integer::EndLoop,EndLoop1,EndLoop2,trip_driver_passenger,stimedifference,exusec,exdsec,exusrcls,exvehtype,exioc,exonode,exintde,exinfo,exoz,ExNumCar,m
    integer:: jj1,jj2,excounter,VehicleLastTrip,abm_trip_index,AlexCheck,ParkRideFoundFlag,j,k,jjtemplength
    character(300)::Line,CheckLine
    character(30):: tour_category,type1
    character(15):: tour_purpose, orig_purpose, dest_purpose,Home, work1,work2,OriginPurposeTemp,DestPurposeTemp
    character(1)::gender,activity_pattern

    character(15),allocatable::TravellerTripOriginPurpose(:,:),TravellerTripDestinationPurpose(:,:)
           
    integer,allocatable::Traveller_index(:),Traveller_ID(:),TravellerNumTrips(:),TravellerTripMode(:,:),TravellerTripOriginMAZ(:,:),TravellerTripOriginTAZ(:,:),TravellerTripDestinationMAZ(:,:),TravellerTripDestinationTAZ(:,:),TravellerTripStartTimeInterval(:,:),TripDriverPassengerFlag(:,:),FirstTransitPassengerTripStartTimeInterval(:)
    integer,allocatable::TAZMap(:),ValidTraveller(:),NumTravellerInTimeInterval(:),TravellerInEachTimeInterval(:,:),TravellerNumFirstTimeIntervalTrip(:),TransitPassengerNumFirstTimeIntervalTrip(:)
    integer,allocatable::nodenum(:),izone(:),idnum(:),iu(:),id(:),MTbay(:),MTbayR(:),i3(:),nlanes(:),FlowModelNum(:),Vfadjust(:),SpeedLimit(:),mfrtp(:),sattp(:),link_iden(:), LGrade(:),node_to_link(:,:),LinkWeight(:),idnod(:),iunod(:),NoofGenLinksPerZone_old(:),ZoneTotalOriginLaneLength(:),ZoneGenerationLink(:,:)
    integer,allocatable::jjtemp(:),jjtemp1(:),ftjjtemp(:),NumTransitPassengerInTimeInterval(:),FirstTransitTrip(:),ABMTripIndex(:,:),SuperZoneMap(:,:),tempvalue(:)
    real,allocatable:: TravellerStartTime(:),TripStartTime(:,:),VectorLengthofTimeInterval(:),VectorStartTimeofTimeInterval(:),TransitPassengerStartTime(:)
    real:: StartTimeofTimeInterval, LengthofTimeInterval,StartTime,exribf,excomp,exstime,exactivitytime
    real:: LWTmp,ActivityTime,ActivityTimeTemp,temp1,temp3,trip_travel_time
    real,allocatable::TravellerActivityTime(:,:)
     integer,allocatable::countertoi(:),Traveller_counter(:)
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
!ParkRide
integer::NumTransitOption,otranz,dtranz,otap,dtap,otaptranz
integer,allocatable::TazToTransitTaz(:),MazToTransitMaz(:),TravellerTripParkRideFlag(:,:),TravellerTripOriginParkingTaz(:,:),TravellerTripDestinationParkingTaz(:,:),TravellerTripOriginParkingMaz(:,:),TravellerTripDestinationParkingMaz(:,:)
integer,allocatable::TravellerParkRideNum(:),MaztoTazMap(:),TaztoMazMap(:),PNRZone(:,:),TransitZone(:,:),NumTransitZoneCa(:),TravellerTripOriginTransitMaz(:,:),TravellerTripDestinationTransitMaz(:,:)
integer,allocatable::TravellerTripOriginTransitTAZ(:,:),TravellerTripDestinationTransitTAZ(:,:)
real,allocatable::PNRaTime(:,:),PNReTime(:,:)
!integer,allocatable::TravellerTripOriginTransitMAZ(:,:),TravellerTripDestinationTransitMAZ(:,:)
!Path bank
integer::value,TurnLinkIndex,nodea,nodeb,nodec,currentDYNASMARTtimeInterval
real::linktt,turntt,CURRENTDYNASMARTTIME
integer,allocatable::IntraSuperzonePathIndex(:,:),PathBank(:,:),NumIntraSuperzoneZone(:),UpStreamLink(:),DownStreamLink(:)
integer,allocatable::NodeIndex(:),LinkNumber(:,:),MovementLink(:,:),NumMovementLink(:)
real,allocatable::LinkTravelTime(:,:),TurnPenaltyTime(:,:,:),IntraSuperZoneSkim(:,:) !zonetozonetraveltime(:,:,:)
!TDSP Skim
character(3):: SPSkimFirstEnter
INTEGER:: NumOriginZone, NumSuperZone, numskimintervals,SpSkimTime
INTEGER:: NumSkimOriginZone, NumSkimSuperZone
INTEGER:: Origin,Destination,TimeInt,NumOfVOTGs
INTEGER:: OIntrst,DIntrst,TIntrst,numODTInterested,VOTGIntrst
INTEGER:: O,D,T,VOTG,error1,nlast,nvt
REAL:: VOTIntrst
REAL,Allocatable:: TempVOT(:)
REAL,Allocatable:: TempCost(:)
REAL,Allocatable:: TempTime(:)
REAL,Allocatable:: TempDistance(:)

!Intrasuperzone car trip
integer,allocatable:: IntraSuperZoneFlag(:,:),IntraSuperZoneTAZ(:,:),NumIntraSuperZoneTAZ(:)
!TransitSkim
integer:: NumPairTransitSkim,PNRTransitTimeInterval
integer,allocatable::TransitPairIndex(:,:),TransitPNRZonePair(:,:)
real,allocatable:: TransitSkimABTime(:,:),TransitSkimABCost(:,:),TransitSkimBATime(:,:),TransitSkimBACost(:,:),TransitSkimWalkTime(:,:)
!Walk
real::walkdistance
integer,allocatable:: TransitWalkZone(:,:),NumTransitWalkZoneCa(:),PNRWalkZone(:,:)
real,allocatable:: zone_walk_time(:,:)
!Check
integer,allocatable:: NumTravelerMaxNumTrip(:)
!-------------------------------------------------  
write(*,*) "TourProcessor Component 1"  
!Openning Required Files For the First Time
    open(file='indivTripData_1.csv',unit=1)  !Input file (Output of ABM), which includes all the individuals trips (including non-family shared rides)
    !open(file='output_transit.dat',unit=2)          !Intermediate Output file
    open(file='Error.dat',unit=3)            !Intermediate Output File which includes all the error or problems which are observed
    !open(file='ZoneMap.dat',unit=4)          !Input file, just for the subarea network to map original zone numbers (TAZ) to sub-area zone (TAZ)
    open(file='SuperZone.dat',unit=4)        !Input file, include the mapping between TAZ and Superzone
    open(file='TimeInterval.dat',unit=5)     !Input file, includes the time inervals information (starting time (simulation minute) and length (minute) of interval) based on the ABM definitions
    open(file='network.dat',unit=6)          !Input file for this code and a standard input of Dynasmart used for genreation link selection
    open(file='origin.dat',unit=7)           !Input file for this code and a standard input of Dynasmart used for genreation link selection
    open(file='traveler.dat',unit=8)        !Output file which includes traveller's information, for each trveller in the first line, genral information and the subsequent lines one trip information per line    
    open(file='AllocationInput.dat',unit=15) !Manually input data
!Car
    !open(file='Error-Vehicle.dat',unit=9)    !Errors observed for vehicle file writing
    open(file='vehicle.dat',unit=10)         !Final Output of the code which is the standard input of Dynasmart
!Joint
    open(file='jointTourData_1.csv',unit=11) !Input file (Output of ABM), which includes all the joint tours information (Family members who are sharing rides), this file is used to identify joint trips participants 
    open(file='jointTripData_1.csv',unit=12) !Input file (Output of ABM), which includes all the joint trips information (Family members who are sharing rides)           
    open(file='transit.dat',unit=18)
!Pathbank
    open(file='intrasuperzone_path.dat',unit=13)
    open(file='intrasuperzone_vehicle.dat',unit=14)
    open(file='path.dat',unit=16)
!External Zone Traffic
    open(file='external_vehicle.dat',unit=22)
    open(file='internalmap.dat',unit=23)
    open(file='externalmap.dat',unit=24)
    !open(file='TravellerTripTravelTime.dat',unit=25)
    open(file='maz_to_taz.dat',unit=17)
!Manual input file
    !open(file='ManualInput.dat',unit=9)
!Skim
    
    
    !open(file='PNRskim_BA.dat',unit=27)
    open(file='output_td_linktraveltime.dat',unit=19)
    open(file='output_td_turnpenalty.dat',unit=20)
    
!Fake output
    open(file='output_vehicleTT.dat',unit=40)
!    open(file='output_transit.dat',unit=41)
!Manual input---------------------------------------------------------

    !MaxTAZ=416
    !NumMAZ=80    
    Home= 'Home           '
    work1='Work           '
    work2='work           '
    !MaxPersonID=10454133
    !MaxNumTraveller=100000 
    !MaxNumTrips=35
    MaxNumCarTrip=0 !initial value which will be updated during the code
    !MaxNumTimeInterval=40
    MaxNumTravellerInEachTimeInterval=0 !initial value which will be updated during the code
    MaxNumCarInEachTimeInterval=0 !initial value which will be updated during the code     
    ActivityTime=0.0 !initial value which will be updated during the code
    ivcltmp=3         !Standard Input for Vehicle Specifications
    ivcl2tmp=1        !Standard Input for Vehicle Specifications 
    ihovtmp=1         !Standard Input for Vehicle Specifications  
    veh_pathnodenum=1 !Standard Input for Vehicle Specifications
    ndestmp=1         !Standard Input for Vehicle Specifications
    infotmp=0         !Standard Input for Vehicle Specifications 
    ribftmp=0.0       !Standard Input for Vehicle Specifications  
    !MaxNumInternalTAZZone=20
!Joint
    JointTourMaxNumCharacter=500       
    !MaxHouseHoldID=3866234
    MaxNumJointTourID=2 !General Tour_ID are from 0 to 21 in ABM, but for the joint trips just 0 and 1 are used
    !MaxHHSize=15
    jjtemplength=10
!External Vehicle
stimedifference=180
MaxNumVehicle=20


    read(15,*) value
    MaxTAZ=value
    read(15,*) value
    NumMAZ=value
    read(15,*) value
    MaxPersonID=value
    read(15,*) value
    MaxNumTraveller=value 
    read(15,*) value
    MaxNumTrips=value
    read(15,*) value
    MaxNumTimeInterval=value
    read(15,*) value
    MaxHouseHoldID=value
    read(15,*) value
    MaxHHSize=value
    read(15,*)average_value_of_time
    read(15,*) NumNode
    read(15,*) NumLink
    read(15,*) LinkTimeInterval
    read(15,*) SpSkimTimeInterval
    read(15,*) NumIteration
    close(15)
!---------------------------------------------
!Read The Transit Setting
open(file='TransitSetting.dat',unit=30)
read(30,*)TransitMazTazFlag
read(30,*)drivingweight
read(30,*)walkingweight
read(30,*)TransitSkimTimeIntervalLength
read(30,*)MaxNumTransitSkimTimeInterval
read(30,*)WalkSpeed
if(TransitMazTazFlag.eq.0) then !This will be MAZ based
    NumTotalTransitSkimZone=NumMAZ
elseif(TransitMazTazFlag.eq.1) then !This is TAZ based
    NumTotalTransitSkimZone=MAxTAZ
endif

close(30)
!------------------------------
!For NumPairPNRZone
if(TransitMazTazFlag.eq.0) then
    open(file='Input_threelink_PNR.dat',unit=26)
else
    open(file='Input_threelink_PNR_TAZ.dat',unit=26)
endif

error=1
read(26,*)
NumPairPNRZone=0
PNRZonetemp=0
NumTransitOption=0
do while (error.ne.-1) 
    read(26,*,iostat=error) zone1,zone2
    if(error.ne.-1) then
        if(zone1.eq.PNRZonetemp) then 
            NumTransitOptionTemp=NumTransitOptionTemp+1
        else
            NumTransitOptionTemp=1
            PNRZonetemp=zone1
        endif
        if(NumTransitOption.lt.NumTransitOptionTemp) NumTransitOption=NumTransitOptionTemp
    endif
    NumPairPNRZone=NumPairPNRZone+1
enddo
NumPairPNRZone=NumPairPNRZone-1
close(26)
!------------------------------
!For NumPairPNRWalkZone
if(TransitMazTazFlag.eq.0) then    
    open(file='input_threelink_walk.dat',unit=36)
else
    open(file='input_threelink_walk_TAZ.dat',unit=36)
endif
error=1
read(36,*)
NumPairPNRWalkZone=0
PNRZoneWalktemp=0
NumTransitWalkOption=0
do while (error.ne.-1) 
    read(36,*,iostat=error) zone1,zone2
    if(error.ne.-1) then
        if(zone1.eq.PNRZoneWalktemp) then 
            NumTransitWalkOptionTemp=NumTransitWalkOptionTemp+1
        else
            NumTransitWalkOptionTemp=1
            PNRZoneWalktemp=zone1
        endif
        if(NumTransitWalkOption.lt.NumTransitWalkOptionTemp) NumTransitWalkOption=NumTransitWalkOptionTemp
    endif
    NumPairPNRWalkZone=NumPairPNRWalkZone+1
enddo
NumPairPNRWalkZone=NumPairPNRWalkZone-1
close(36)
 
!-------------------------------------
!Get the Number of Link
do j=1,5
    read(19,'(a)') Line
enddo
read(19,'(a)') CheckLine
read(19,'(a)') Line
MaxNodeID=0
do while (Line.ne.CheckLine)
    read(19,'(a)') Line
    if(Line.ne.CheckLine) then
        read(Line,*) value,nodea,nodeb,value,value
        if(max(nodea,nodeb).gt.MaxNodeID) MaxNodeID=max(nodea,nodeb)
        !NumLink=NumLink+1
    endif
enddo
close(19)
!----------------------------------
!Get the Number of Turn
open(file='output_td_turnpenalty.dat',unit=20)
NumTurn=0
do j=1,5
    read(20,'(a)') Line
enddo
read(20,'(a)') CheckLine
read(20,'(a)') Line
do while (Line.ne.CheckLine)
    read(20,'(a)') Line
    if(Line.ne.CheckLine) then
        NumTurn=NumTurn+1
    endif
enddo
read(20,*)
close(20)
!-----------------------------
!Get the MaxPathLength
error= 1
read(14,*)
read(14,*)
MaxPathLength=0
do while (error.ne.-1)
    read(14,'(a)',iostat=error) Line
    if(error.ne.-1) then 
        read(Line,*,iostat=error) i,iutmp,idtmp,value,value,value,ihovtmp,veh_pathnodenum,value,infotmp,ribftmp,comptmp,origtaz,value,value
        if(veh_pathnodenum.gt.MaxPathLength) MaxPathLength=veh_pathnodenum
        read(14,*) desttaz,value
    endif
enddo
NumPath=i
close(14)

!---------------------------
!Get the NumPairTransitSkim
if(TransitMazTazFlag.eq.0) then 
    open(file='TransitPNRSkim_AB_cost.dat',unit=33)
else
    open(file='TransitPNRSkim_AB_cost_TAZ.dat',unit=33)
endif
error=1
NumPairTransitSkim=0
Zonetemp=0
do while (error.ne.-1) 
    read(33,*,iostat=error) 
    NumPairTransitSkim=NumPairTransitSkim+1
enddo
NumPairTransitSkim=NumPairTransitSkim-1
close(33)


!-------------------------------------------------------------------
!Allocation
    write(*,*)'Allocating arrays...'  	     	       
    read(6,*) nzones_old,noofnodes_old,noofarcs_old,kay,SuperZoneSwitch
    
    allocate (Traveller_index(MaxPersonID))
        Traveller_index(:)=0
    allocate (Traveller_ID(MaxNumTraveller))
        Traveller_ID(:)=0
    allocate (TravellerNumTrips(MaxNumTraveller))
        TravellerNumTrips(:)=0
    allocate (TravellerNumFirstTimeIntervalTrip(MaxNumTraveller))
        TravellerNumFirstTimeIntervalTrip(:)=0
    
!    allocate (TransitPassengerNumFirstTimeIntervalTrip(MaxNumTraveller))
!        TransitPassengerNumFirstTimeIntervalTrip(:)=0
    allocate (TravellerTripMode(MaxNumTraveller,MaxNumTrips))
        TravellerTripMode(:,:)=0
   
    allocate (TravellerTripOriginMAZ(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginMAZ(:,:)=0
    allocate (TravellerTripOriginTAZ(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginTAZ(:,:)=0
    allocate (TravellerTripDestinationMAZ(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationMAZ(:,:)=0
    allocate (TravellerTripDestinationTAZ(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationTAZ(:,:)=0
    allocate (TravellerTripOriginPurpose(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginPurpose(:,:)=' '
    allocate (TravellerTripDestinationPurpose(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationPurpose(:,:)=' '
    allocate (TravellerTripStartTimeInterval(MaxNumTraveller,MaxNumTrips))
        TravellerTripStartTimeInterval(:,:)=0
    allocate(TravellerActivityTime(MaxNumTraveller,MaxNumTrips))
        TravellerActivityTime(:,:)=0
     !For subarea network only
!    allocate (TAZMap(MaxTAZ))
!        TAZMap(:)=0
    allocate (ValidTraveller(MaxNumTraveller))
     ValidTraveller(:)=1
    allocate (NumTravellerInTimeInterval(MaxNumTimeInterval))
        NumTravellerInTimeInterval(:)=0
    allocate (TravellerStartTime(MaxNumTraveller))
        TravellerStartTime(:)=0
    allocate (TripDriverPassengerFlag(MaxNumTraveller,MaxNumTrips))
        TripDriverPassengerFlag(:,:)=0
        
    allocate (FirstTransitPassengerTripStartTimeInterval(MaxNumTraveller))
        FirstTransitPassengerTripStartTimeInterval(:)=MaxNumTimeInterval+1
!Car
    allocate (ValidCar(MaxNumTraveller))
     ValidCar(:)=0
    allocate (FirstCarTrip(MaxNumTraveller,MaxNumVehicle))
        FirstCarTrip(:,:)=0
!    allocate (CarNumFirstTimeIntervalTrip(MaxNumTraveller))
!        CarNumFirstTimeIntervalTrip(:)=0
!    allocate (CarNumTrip(MaxNumTraveller))
!        CarNumTrip(:)=0
    allocate (NumCarInTimeInterval(MaxNumTimeInterval))
        NumCarInTimeInterval(:)=0
!    allocate (CarStartTime(MaxNumTraveller))
!        CarStartTime(:)=0   
    allocate (nodenum(noofnodes_old))
        nodenum(:)=0
    allocate (izone(noofnodes_old))
        izone(:)=0
    allocate (idnum(noofnodes_old*100))
        idnum(:)=0
    allocate (iu(noofarcs_old))
        iu(:)=0
    allocate (id(noofarcs_old))
        id(:)=0
    allocate (MTbay(noofarcs_old))
        MTbay(:)=0
    allocate (MTbayR(noofarcs_old))
        MTbayR(:)=0
    allocate (i3(noofarcs_old))
        i3(:)=0
    allocate (nlanes(noofarcs_old))
        nlanes(:)=0
    allocate (FlowModelNum(noofarcs_old))
        FlowModelNum(:)=0
    allocate (Vfadjust(noofarcs_old))
        Vfadjust(:)=0
    allocate (SpeedLimit(noofarcs_old))
        SpeedLimit(:)=0
    allocate (mfrtp(noofarcs_old))
        mfrtp(:)=0
    allocate (sattp(noofarcs_old))
        sattp(:)=0
    allocate (link_iden(noofarcs_old))
        link_iden(:)=0
    allocate (LGrade(noofarcs_old))
        LGrade(:)=0
	allocate(iunod(noofarcs_old))
        iunod(:)=0
    allocate(idnod(noofarcs_old))
        idnod(:)=0
    allocate (node_to_link(noofnodes_old,noofnodes_old))
        node_to_link(:,:)=0
    allocate (LinkWeight(noofarcs_old))
        LinkWeight(:)=0
    allocate (NoofGenLinksPerZone_old(nzones_old))
        NoofGenLinksPerZone_old(:)=0
    allocate (ZoneTotalOriginLaneLength(nzones_old))
        ZoneTotalOriginLaneLength(:)=0
    allocate (ZoneGenerationLink(nzones_old,noofnodes_old))
        ZoneGenerationLink(:,:)=0  

!Joint
    allocate (JointTourCharacter(JointTourMaxNumCharacter))
        JointTourCharacter(:)=' '
    allocate (util(26))
        util(:)=0
    allocate (prob(26))
        prob(:)=0
    allocate (PersonIDOfHH_PN(MaxHouseHoldID,MaxHHSize))
        PersonIDOfHH_PN(:,:)=0
    allocate (NumTourParticipant(MaxHouseHoldID,MaxNumJointTourID))
        NumTourParticipant(:,:)=0
    allocate (PersonNumOfHH_TourID(MaxHouseHoldID,MaxNumJointTourID,MaxHHSize))
        PersonNumOfHH_TourID(:,:,:)=0
    allocate (TravellerTripDriver(MaxNumTraveller,MaxNumTrips))
        TravellerTripDriver(:,:)=0
    allocate (Traveller_HH_ID(MaxNumTraveller))
        Traveller_HH_ID(:)=0
    allocate (TravellerTripTourID(MaxNumTraveller,MaxNumTrips))
        TravellerTripTourID(:,:)=0        
    allocate (TravellerTripJointFlag(MaxNumTraveller,MaxNumTrips))
        TravellerTripJointFlag(:,:)=0
        
!ParkRide
!    allocate (TazToTransitTaz(MaxTAZ))
!        TazToTransitTaz(:)=0                                
!    allocate (MazToTransitMaz(NumMAZ))
!        MazToTransitMaz(:)=0   
    allocate(TravellerParkRideNum(MaxNumTraveller))
        TravellerParkRideNum(:)=0   
    allocate(PNRZone(NumPairPNRZone,2))
        PNRZone(:,:)=0
    allocate(TransitZone(NumTotalTransitSkimZone,NumTransitOption))
        TransitZone(:,:)=0
    allocate(NumTransitZoneCa(NumTotalTransitSkimZone))
        NumTransitZoneCa(:)=0
        
    allocate(TravellerTripOriginTransitMAZ(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginTransitMAZ(:,:)=0
    allocate(TravellerTripDestinationTransitMAZ(MaxNumTraveller,MaxNumTrips))    
        TravellerTripDestinationTransitMAZ(:,:)=0
    allocate(TravellerTripOriginTransitTAZ(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginTransitTAZ(:,:)=0
    allocate(TravellerTripDestinationTransitTAZ(MaxNumTraveller,MaxNumTrips))    
        TravellerTripDestinationTransitTAZ(:,:)=0
    
    allocate(PNRaTime(MaxNumTraveller,MaxNumTrips))
        PNRaTime(:,:)=0
!    allocate(PNReTime(MaxNumTraveller,MaxNumTrips))
!        PNReTime(:,:)=0

!TransitSkim
    allocate(TransitPNRZonePair(NumPairTransitSkim,2))
        TransitPNRZonePair(:,:)=0
!    allocate(TransitSkimABTime(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
!        TransitSkimABTime(:,:)=0
    allocate(TransitSkimABCost(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
        TransitSkimABCost(:,:)=0
    allocate(TransitSkimBATime(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
        TransitSkimBATime(:,:)=0
    allocate(TransitSkimBACost(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
        TransitSkimBACost(:,:)=0
  
    !Walk
    allocate(PNRWalkZone(NumPairPNRWalkZone,2))
        PNRWalkZone(:,:)=0
    allocate(TransitSkimWalkTime(NumPairPNRWalkZone,NumTransitWalkOption))
        TransitSkimWalkTime(:,:)=0
    allocate(TransitWalkZone(NumTotalTransitSkimZone,NumTransitWalkOption))
        TransitWalkZone(:,:)=0
    allocate(NumTransitWalkZoneCa(NumTotalTransitSkimZone))
        NumTransitWalkZoneCa(:)=0
  
!Driver or passenger                           
    allocate (TravellerTripParkRideFlag(MaxNumTraveller,MaxNumTrips))
        TravellerTripParkRideFlag(:,:)=0     
!For two car in one interval
    allocate(jjtemp(jjtemplength))
        jjtemp(:)=0
    allocate(jjtemp1(MaxNumTrips))
        jjtemp1(:)=0
    allocate(ftjjtemp(jjtemplength))
        ftjjtemp(:)=0
        
   ! allocate(TransitPassengerStartTime(MaxNumTraveller))
     !   TransitPassengerStartTime(:)=0
!Time Interval        
    allocate(VectorStartTimeofTimeInterval(MaxNumTimeInterval))
        VectorStartTimeofTimeInterval(:)=0    
    allocate(VectorLengthofTimeInterval(MaxNumTimeInterval))
        VectorLengthofTimeInterval(:)=0
          
    allocate(TripStartTime(MaxNumTraveller,MaxNumTrips))
        TripStartTime(:,:)=0
    allocate(FirstTransitTrip(MaxNumTraveller))
        FirstTransitTrip(:)=0
    allocate(ValueOfTime(MaxPersonID))
        ValueOfTime(:)=0
    allocate(TravellerTripTravelTime(MaxNumTraveller,MaxNumTrips))
        TravellerTripTravelTime(:,:)=10000
        
! To sort the trip back to the origin abm file
    allocate(ABMTripIndex(MaxNumTraveller,MaxNumTrips))
        ABMTripIndex(:,:)=0
!Parking taz
    allocate(TravellerTripOriginParkingTaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginParkingTaz(:,:)=0
    allocate(TravellerTripDestinationParkingTaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationParkingTaz(:,:)=0
    allocate(TravellerTripOriginParkingMaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginParkingMaz(:,:)=0
    allocate(TravellerTripDestinationParkingMaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationParkingMaz(:,:)=0
    allocate(MaztoTazMap(NumMAZ))
        MaztoTazMap(:)=0
    allocate(TaztoMazMap(MaxTaz))
        TaztoMazMap(:)=0
!PathBank
    allocate(IntraSuperzonePathIndex(MaxTaz,MaxTaz))
        IntraSuperzonePathIndex(:,:)=0
    allocate(tempvalue(15))
        tempvalue(:)=0
    allocate(SuperZoneMap(MaxTAZ,2))
        SuperZoneMap(:,:)=0
    allocate(PathBank(NumPath,MaxPathLength))
        PathBank(:,:)=0 
    allocate(NumIntraSuperzoneZone(NumPath))
        NumIntraSuperzoneZone(:)=0
    allocate(UpStreamLink(NumPath))
        UpStreamLink(:)=0
    allocate( DownStreamLink(NumPath))
        DownStreamLink(:)=0
        
!    allocate(Zonetozonetraveltime(MaxTAZ,MaxTAZ,MaxNumTimeInterval))
!        Zonetozonetraveltime(:,:,:)=0
!Intrasuperzone
    allocate (IntraSuperZoneFlag(MaxNumTraveller,MaxNumVehicle))
        IntraSuperZoneFlag(:,:)=0
!Check
allocate(NumTravelerMaxNumTrip(20))
    NumTravelerMaxNumTrip(:)=0
    
!!TDSP Allocation
!Allocate(TempVOT(6))
!Allocate(TempCost(6))
!Allocate(TempTime(6))
!Allocate(TempDistance(6))
!!open(file='NetworkAtt.dat',unit=25,status='unknown')
!!read(25,*) NumOriginZone, NumSuperZone, NumIntervals
!if (.NOT. ALLOCATED(ODTATT_Array)) then
!Allocate(ODTATT_Array(NumOriginZone,NumSuperZone,NumIntervals),stat=error)
!endif
!open(file='vehicularPNRSkim.dat', unit=31)

   
!-------------------------------------------------------------------       	
!Just for the Sub-area network
!Creating a map for TAZ to project the Original TAZ to the TAZ in subarea network using the input file      
!error=1
!do while(error.ne.-1)   
!    read(4,*,iostat=error) OriginalTAZ, MappedTaz
!    TAZMap(OriginalTAZ)= MappedTaz
!enddo
!close(4)
!-------------------------------------------------------------------       	    
!Creating mapping for PersonIDOfHH_PN
!Read the value of time
    error=1
    open(file='personData_1.csv',unit=21)
    read(21,*,iostat=error)
    zz=0
    if (error.ne.-1) then      
        do while (error.ne.-1)   
            read(21,'(a)',iostat=error) Line
            if (error.ne.-1) then
                read(Line,*,iostat=error) hh_id,person_id,person_num,age,gender,type1,value_of_time,fp_choice,activity_pattern,imf_choice,inmf_choice,walk_time_weight,walk_speed,max_walk,user_class_work_walk,user_class_work_pnr,user_class_work_knr,user_class_non_work_walk,user_class_non_work_pnr,user_class_non_work_knr
                PersonIDOfHH_PN(hh_id,person_num)=person_id
                ValueOfTime(person_id)=nint(value_of_time/60*10000.0)/10000.0  !May need to fix in the future, the index
                zz=zz+1
            endif
        enddo
    endif
    close(21)
    !average_value_of_time=Sum(ValueOfTime)/zz
!---------------------------------------------------------------------
!Create a map between the TAZ number and super zone number
error=1
read(4,*) maxnumsuperzone
read(4,*) 
i=1
NumColumnsinSuperZoneFile=15
zz=MaxTAZ/NumColumnsinSuperZoneFile
NumRowsinSuperZoneFile=floor(zz)

do m=1,NumRowsinSuperZoneFile
    read(4,*) tempvalue(1:NumColumnsinSuperZoneFile)
    SuperZoneMap(i:i+NumColumnsinSuperZoneFile-1,1)=tempvalue(1:NumColumnsinSuperZoneFile)
    i=i+NumColumnsinSuperZoneFile
enddo
lengthoflastrow=MaxTaz-(m-1)*NumColumnsinSuperZoneFile
read(4,*) tempvalue(1:lengthoflastrow)
SuperZoneMap(i:i+lengthoflastrow-1,1)=tempvalue(1:lengthoflastrow)
read(4,*)
i=1
do m=1,NumRowsinSuperZoneFile
    read(4,*) tempvalue(1:NumColumnsinSuperZoneFile)
    SuperZoneMap(i:i+NumColumnsinSuperZoneFile-1,2)=tempvalue(1:NumColumnsinSuperZoneFile)
    i=i+15
enddo
read(4,*) tempvalue(1:lengthoflastrow)
SuperZoneMap(i:i+lengthoflastrow-1,2)=tempvalue(1:lengthoflastrow)
close(4)
!---------------------------------------------------------------------
!Create the mapping between the maz and taz

read(17,*)
 do i=1,NumMAZ
    read(17,*) maz,taz
    MaztoTazMap(maz)=taz
    TaztoMazMap(taz)=maz   
 enddo
!--------------------------------------------------------------------
!ReadPNRZone
!open(file='PNRskim_AB.dat',unit=26)
if(TransitMazTazFlag.eq.0) then
    open(file='Input_threelink_PNR.dat',unit=26)
else
    open(file='Input_threelink_PNR_TAZ.dat',unit=26)
endif

read(26,*)
PNRZonetemp=0
NumTransitOptionTemp=0
do i=1,NumPairPNRZone
    read(26,*,iostat=error)PNRZone(i,1),PNRZone(i,2)
    if(error.ne.-1) then
        if(PNRZone(i,1).eq.PNRZonetemp) then 
            NumTransitOptionTemp=NumTransitOptionTemp+1
            NumTransitZoneCa(PNRZone(i,1))=NumTransitOptionTemp
        else
            NumTransitOptionTemp=1
            PNRZonetemp=PNRZone(i,1)
            NumTransitZoneCa(PNRZone(i,1))=NumTransitOptionTemp
        endif
        TransitZone(PNRZone(i,1),NumTransitOptionTemp)=PNRZone(i,2)
    endif
    
enddo
close(26)
!open(file='PNRSkim_AB.dat',unit=26)



!----------------------------------------------------------
 !Read in the output_path.dat (can be moved to right before write(10,*))
open(file='intrasuperzone_vehicle.dat',unit=14)
error= 1
read(14,*)
read(14,*)
do while (error.ne.-1)
    read(14,'(a)',iostat=error) Line
    if(error.ne.-1) then 
        read(Line,*,iostat=error) i,iutmp,idtmp,value,value,value,ihovtmp,veh_pathnodenum,value,infotmp,ribftmp,comptmp,origtaz,value,value
        NumIntraSuperzoneZone(i)=veh_pathnodenum
        !write(10,11) counter,iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,NonrepetitiveCarNumTrip,infotmp,ribftmp,comptmp,TravellerTripOriginTAZ(i,FirstCarTrip(i,m)),ValueOfTime(Traveller_ID(i)),1
        UpStreamLink(i)=iutmp
        DownStreamLink(i)=idtmp
        read(14,*) desttaz,value
        IntraSuperzonePathIndex(origtaz,desttaz)=i
        read(13,*) PathBank(i,1:veh_pathnodenum)
    endif
enddo

!-------------------------------------------------------
 open(file='traveler.dat',unit=8)
read(8,*)NumTraveller, RealMaxNumTrips
index=NumTraveller
    do i=1,2
        read(8,*)
    enddo
    index=0
    error=1
    allocate(countertoi(NumTraveller))
        countertoi(:)=0
    do while (error.ne.-1)  !read until the last line
        read(8,'(a)',iostat=error) Line
        if (error.ne.-1) then
            !read the traveller's information and store them
            read(Line,*,iostat=error)counter,i,person_id,StartTime,numtrips,Taz,valid_car,first_car_trip,first_transit_trip,value_of_time
            index=index+1
           ! Traveller_counter(i)=counter
            countertoi(counter)=i
            Traveller_ID(i)=person_id
            TravellerNumTrips(i)=numtrips
            ValidCar(i)=valid_car
            FirstTransitTrip(i)=first_transit_trip
            !FirstCarTrip(i)=first_car_trip
            ValueOfTime(Traveller_ID(i))=value_of_time
            TravellerTripOriginTaz(i,1)=Taz
            do j=1,numtrips ! For each travellre, read
                !read the information about each trip and store them
                read(8,'(a)',iostat=error) Line
                read(Line,*,iostat=error) Taz,ActivityTime,trip_mode,orig_purpose,dest_purpose,orig_maz,orig_taz,dest_maz,dest_taz,trip_driver_passenger,trip_joint_flag, trip_park_ride_flag,start_time_interval,trip_start_time,abm_trip_index,traveller_trip_driver,traveller_trip_destination_parkingtaz
                !store the value
                TravellerTripDestinationTaz(i,j)=Taz
                !if(j.gt.1) TravellerTripOriginTaz(i,j)=TravellerTripDestinationTaz(i,j-1)
                if(j.gt.1) then
                    if(TravellerTripDestinationTaz(i,j-1).ne.orig_taz)then
                        write(*,*) i,Traveller_ID(i),TravellerTripDestinationTaz(i,j-1),orig_taz
                        pause
                    endif
                endif
                 
                TravellerTripMode(i,j)=trip_mode
                TravellerTripDestinationPurpose(i,j)=dest_purpose
                TravellerTripOriginPurpose(i,j)=orig_purpose
                TravellerTripOriginMAZ(i,j)=orig_maz
                TravellerTripOriginTAZ(i,j)=orig_taz
                TravellerTripDestinationMAZ(i,j)=dest_maz
                TravellerTripDestinationTAZ(i,j)=dest_taz
                TripDriverPassengerFlag(i,j)=trip_driver_passenger
                TravellerTripJointFlag(i,j)=trip_joint_flag
                TravellerTripParkRideFlag(i,j)=trip_park_ride_flag
                TravellerTripStartTimeInterval(i,j)=start_time_interval
                ABMTripIndex(i,j)=abm_trip_index
                TravellerTripDriver(i,j)=traveller_trip_driver
                TripStartTime(i,j)=trip_start_time
                TravellerActivityTime(i,j)=ActivityTime
            enddo
        endif
    enddo


!-------------------------------------------------------------------       	    
!After reading the individual trips and joint trips, they are all sorted by start time interval, now we need to resort the trips in the same time interval, based on the origin-destination, zone and purpose of trip consistency
!   EndofLoop=0
!   WorkCounter=0
!   do i=1,index ! search over all the identified travellers from individual trip list of ABM output
!        j=1
!        jj=j
!        EndofLoop=0
!        ! Find the first trip starting from Home at the first time interval and put as the first trip     
!        validfirsttrip=0    
!        do while(TravellerTripStartTimeInterval(i,j).eq.TravellerTripStartTimeInterval(i,jj)) !search over all trips at the first interval
!            if (TravellerTripOriginPurpose(i,jj).eq.Home)then !If a trip at the first interval is found wich is originated at Home                  
!                validfirsttrip=validfirsttrip+1
!                ftjjtemp(validfirsttrip)=jj
!            endif
!            TravellerNumFirstTimeIntervalTrip(i)=TravellerNumFirstTimeIntervalTrip(i)+1 !Save number of trips generated
!            jj=jj+1 
!        enddo
!        !jj=ftjjtemp(1)
!        jjj=jj-1
!        if (validfirsttrip.eq.0) then 
!            write(*,*) "No trip starts from home"
!        else if (validfirsttrip.eq.1) then !if only one trip origin from home then update the trip based on the trip
!            jj=ftjjtemp(1)
!        else if(validfirsttrip.gt.1) then !if there are 2 trips 
!            m=j
!            EndLoop=0
!            !Find the trip has a destination of home
!            do while(EndLoop.eq.0.and.m.le.jjj)
!                if (TravellerTripDestinationPurpose(i,m).eq.Home)then
!                        EndLoop=1
!                else
!                        m=m+1
!                endif
!            enddo
!            if (EndLoop.eq.1)  then
!                !Then trace back to the valid trip ftjjtemp
!                !consisttrip=0
!                jjtemp1(:)=0
!                Endloop1=0
!                OriginMAZTemp=TravellerTripOriginMAZ(i,m)
!                OriginPurposeTemp=TravellerTripOriginPurpose(i,m)                      
!                ll=j-1
!                do while(EndLoop1.eq.0.and.ll.lt.jjj)
!                    ll=ll+1
!                    EndLoop2=0
!                    l=j-1
!                    do while (EndLoop2.eq.0.and.l.lt.jjj) 
!                        l=l+1
!                        if (OriginMAZTemp.eq.TravellerTripDestinationMAZ(i,l))then
!                            if ((OriginPurposeTemp.eq.TravellerTripDestinationPurpose(i,l)).or.(OriginPurposeTemp.eq.work1.and.TravellerTripDestinationPurpose(i,l).eq.work2).or.(OriginPurposeTemp.eq.work2.and.TravellerTripDestinationPurpose(i,l).eq.work1)) then 
!                                EndLoop2=1
!                                do n=1,jjtemplength
!                                    if(ftjjtemp(n).ne.0) then
!                                        if (OriginMAZTemp.eq.TravellerTripDestinationMAZ(i,ftjjtemp(n)))then
!                                            if ((OriginPurposeTemp.eq.TravellerTripDestinationPurpose(i,ftjjtemp(n))).or.(OriginPurposeTemp.eq.work1.and.TravellerTripDestinationPurpose(i,ftjjtemp(n)).eq.work2).or.(OriginPurposeTemp.eq.work2.and.TravellerTripDestinationPurpose(i,ftjjtemp(n)).eq.work1)) then
!                                                EndLoop1=1
!                                                jj=ftjjtemp(n)
!                                            endif
!                                        endif
!                                    endif
!                                enddo
!                                OriginMAZTemp=TravellerTripOriginMAZ(i,l)                   
!                                OriginPurposeTemp=TravellerTripOriginPurpose(i,l)
!                                DestMAZTemp=TravellerTripDestinationMAZ(i,l)
!                                !DestPurposeTemp=TravellerTripDestinationinPurpose(i,l)
!                            endif
!                        endif
!                    enddo
!                enddo
!                if(EndLoop1.eq.0) write(3,*) Traveller_ID(i),'No valid trip'   
!            else
!                write(*,*) Traveller_ID(i),'there are two departure trip without any arrival trip back'
!            endif
!       endif                    
!        !Make a copy of that trip
!        trip_mode=TravellerTripMode(i,jj)
!        orig_maz=TravellerTripOriginMAZ(i,jj)
!        orig_taz=TravellerTripOriginTAZ(i,jj)
!        dest_maz=TravellerTripDestinationMAZ(i,jj)
!        dest_taz=TravellerTripDestinationTAZ(i,jj)
!        orig_purpose=TravellerTripOriginPurpose(i,jj)
!        dest_purpose=TravellerTripDestinationPurpose(i,jj)
!        stop_period=TravellerTripStartTimeInterval(i,jj)
!        trip_driver_passenger=TripDriverPassengerFlag(i,jj)
!        !Joint
!        driver_id=TravellerTripDriver(i,jj)
!        tour_id_temp=TravellerTripTourID(i,jj)
!        TravellerTripJointFlagTemp=TravellerTripJointFlag(i,jj)
!        abm_trip_index=ABMTripINdex(i,jj)
!        parking_taz=TravellerTripDestinationParkingTaz(i,jj)
!        !Move the first trip to the place of the found trip
!        TravellerTripMode(i,jj)=TravellerTripMode(i,j)
!        TravellerTripOriginMAZ(i,jj)=TravellerTripOriginMAZ(i,j)
!        TravellerTripOriginTAZ(i,jj)=TravellerTripOriginTAZ(i,j)
!        TravellerTripDestinationMAZ(i,jj)=TravellerTripDestinationMAZ(i,j)
!        TravellerTripDestinationTAZ(i,jj)=TravellerTripDestinationTAZ(i,j)
!        TravellerTripOriginPurpose(i,jj)=TravellerTripOriginPurpose(i,j)
!        TravellerTripDestinationPurpose(i,jj)= TravellerTripDestinationPurpose(i,j)
!        TravellerTripStartTimeInterval(i,jj)=TravellerTripStartTimeInterval(i,j)
!        TripDriverPassengerFlag(i,jj)=TripDriverPassengerFlag(i,j)
!        !Joint
!        TravellerTripDriver(i,jj)=TravellerTripDriver(i,j)
!        TravellerTripTourID(i,jj)=TravellerTripTourID(i,j)
!        TravellerTripJointFlag(i,jj)=TravellerTripJointFlag(i,j)
!        ABMTripIndex(i,jj)=ABMTripIndex(i,j)
!        TravellerTripDestinationParkingTaz(i,jj)=TravellerTripDestinationParkingTaz(i,j)
!        !Move the found trip to the first trip
!        TravellerTripMode(i,j)=trip_mode
!        TravellerTripOriginMAZ(i,j)=orig_maz
!        TravellerTripOriginTAZ(i,j)=orig_taz
!        TravellerTripDestinationMAZ(i,j)=dest_maz
!        TravellerTripDestinationTAZ(i,j)=dest_taz
!        TravellerTripOriginPurpose(i,j)=orig_purpose
!        TravellerTripDestinationPurpose(i,j)=dest_purpose
!        TravellerTripStartTimeInterval(i,j)=stop_period
!        TripDriverPassengerFlag(i,j)=trip_driver_passenger
!        !Joint
!        TravellerTripDriver(i,j)=driver_id
!        TravellerTripTourID(i,j)=tour_id_temp
!        TravellerTripJointFlag(i,j)=TravellerTripJointFlagTemp
!        
!        ABMTripIndex(i,j)=abm_trip_index
!        TravellerTripDestinationParkingTaz(i,j)=parking_taz 
!        !Finding the first trip (as a benchmark and start point), we will start to sort subsequent trips    
!        do j=2,TravellerNumTrips(i)-1
!            jj=j+1
!            jjtemp(:)=0
!            !If there are unsorted trip start within same time interval
!            if (TravellerTripStartTimeInterval(i,j).eq.TravellerTripStartTimeInterval(i,jj)) then                    
!                k=0 !Number of qualify trips in same interval
!                !Check if trip j is a qualified trip
!                if ((TravellerTripDestinationPurpose(i,j-1).eq.TravellerTripOriginPurpose(i,j)).or.(TravellerTripDestinationPurpose(i,j-1).eq.work1.and.TravellerTripOriginPurpose(i,j).eq.work2).or.(TravellerTripDestinationPurpose(i,j-1).eq.work2.and.TravellerTripOriginPurpose(i,j).eq.work1)) then
!                    if (TravellerTripDestinationMAZ(i,j-1).eq.TravellerTripOriginMAZ(i,j)) then
!                        k=k+1
!                        jjtemp(k)=j
!                    endif
!                endif
!                !Check if any remaining trip is qualified trip
!                do while(TravellerTripStartTimeInterval(i,j).eq.TravellerTripStartTimeInterval(i,jj))
!                    if (TravellerTripDestinationMAZ(i,j-1).eq.TravellerTripOriginMAZ(i,jj))then
!                        if ((TravellerTripDestinationPurpose(i,j-1).eq.TravellerTripOriginPurpose(i,jj)).or.(TravellerTripDestinationPurpose(i,j-1).eq.work1.and.TravellerTripOriginPurpose(i,jj).eq.work2).or.(TravellerTripDestinationPurpose(i,j-1).eq.work2.and.TravellerTripOriginPurpose(i,jj).eq.work1)) then ! find the consistent trip jj and swap order of trip jj and j
!                            !if find qualify trips update the jjtemp                            
!                            k=k+1
!                            jjtemp(k)=jj
!                        endif                           
!                    endif
!                    jj=jj+1
!                   
!                enddo 
!!                do z=1,TravellerNumTrips(i)
!!                     if(i.eq.434746) write(*,*) j,z,jj,TravellerTripOriginPurpose(i,z),TravellerTripDestinationPurpose(i,z)
!!                enddo
!                
!                jjj=jj-1    
!                if(k.eq.0) then 
!                    !write(20,*) Traveller_ID(i),'interval error'
!                elseif(k.eq.1) then 
!                    !if only one valid trip, update based on this trip
!                    jj=sum(jjtemp)
!                else !if more than one valid trip
!                    m=j
!                    EndLoop=0
!                    !Find the trip with same desination information as fixed trip j-1
!                    do while(EndLoop.eq.0.and.m.lt.jj)
!                        if (TravellerTripDestinationMAZ(i,m).eq.TravellerTripDestinationMAZ(i,j-1))then
!                            if ((TravellerTripDestinationPurpose(i,m).eq.TravellerTripDestinationPurpose(i,j-1)).or.(TravellerTripDestinationPurpose(i,m).eq.work1.and.TravellerTripDestinationPurpose(i,j-1).eq.work2).or.(TravellerTripDestinationPurpose(i,m).eq.work2.and.TravellerTripDestinationPurpose(i,j-1).eq.work1)) then 
!                                EndLoop=1
!                            else 
!                                m=m+1
!                            endif
!                        else
!                            m=m+1
!                        endif
!                    enddo
!                    if (EndLoop.eq.1)  then
!                        !Then trace back to the valid trip jjtemp
!                        !consisttrip=0
!                        jjtemp1(:)=0
!                        Endloop1=0
!                        OriginMAZTemp=TravellerTripOriginMAZ(i,m)
!                        OriginPurposeTemp=TravellerTripOriginPurpose(i,m)
!                        !DestMAZTemp=TravellerTripDestinationMAZ(i,l)
!                        !DestPurposeTemp=TravellerTripDestinationPurpose(i,m)                           
!                        ll=j-1
!                        do while(EndLoop1.eq.0.and.ll.lt.jjj)
!                            ll=ll+1
!                            EndLoop2=0
!                            l=j-1
!                            do while (EndLoop2.eq.0.and.l.lt.jjj) 
!                                l=l+1
!                                if (OriginMAZTemp.eq.TravellerTripDestinationMAZ(i,l).and.jjtemp1(l).eq.0)then
!                                    if ((OriginPurposeTemp.eq.TravellerTripDestinationPurpose(i,l)).or.(OriginPurposeTemp.eq.work1.and.TravellerTripDestinationPurpose(i,l).eq.work2).or.(OriginPurposeTemp.eq.work2.and.TravellerTripDestinationPurpose(i,l).eq.work1)) then 
!                                        EndLoop2=1
!                                        jjtemp1(l)=1
!                                        do n=1,jjtemplength
!                                            if(jjtemp(n).ne.0) then
!                                                if (OriginMAZTemp.eq.TravellerTripDestinationMAZ(i,jjtemp(n)))then
!                                                    if ((OriginPurposeTemp.eq.TravellerTripDestinationPurpose(i,jjtemp(n))).or.(OriginPurposeTemp.eq.work1.and.TravellerTripDestinationPurpose(i,jjtemp(n)).eq.work2).or.(OriginPurposeTemp.eq.work2.and.TravellerTripDestinationPurpose(i,jjtemp(n)).eq.work1)) then
!                                                        EndLoop1=1
!                                                        jj=jjtemp(n)
!                                                    endif
!                                                endif
!                                            endif
!                                        enddo
!                                        OriginMAZTemp=TravellerTripOriginMAZ(i,l)                   
!                                        OriginPurposeTemp=TravellerTripOriginPurpose(i,l)
!                                        DestMAZTemp=TravellerTripDestinationMAZ(i,l)
!                                        DestPurposeTemp=TravellerTripDestinationPurpose(i,l)
!                                    endif
!                                endif
!                            enddo
!                        enddo
!                        !if(EndLoop1.eq.0) write(20,*) Traveller_ID(i),'No valid trip'  
!                        if(EndLoop1.eq.0) write(3,*) Traveller_ID(i),'No valid trip'  
!                    else
!                        !write(*,*) Traveller_ID(i),'there are two departure trip without any arrival trip back'
!                    endif
!                endif
!                if(k.gt.0) then
!                    trip_mode=TravellerTripMode(i,jj)
!                    orig_maz=TravellerTripOriginMAZ(i,jj)
!                    orig_taz=TravellerTripOriginTAZ(i,jj)
!                    dest_maz=TravellerTripDestinationMAZ(i,jj)
!                    dest_taz=TravellerTripDestinationTAZ(i,jj)
!                    orig_purpose=TravellerTripOriginPurpose(i,jj)
!                    dest_purpose=TravellerTripDestinationPurpose(i,jj)
!                    stop_period=TravellerTripStartTimeInterval(i,jj)
!                    trip_driver_passenger=TripDriverPassengerFlag(i,jj)
!                    abm_trip_index=ABMTripINdex(i,jj)
!                    parking_taz=TravellerTripDestinationParkingTaz(i,jj)
!                    !Joint
!                    driver_id=TravellerTripDriver(i,jj)
!                    tour_id_temp=TravellerTripTourID(i,jj)
!                    TravellerTripJointFlagTemp=TravellerTripJointFlag(i,jj)
!                    
!                    TravellerTripMode(i,jj)=TravellerTripMode(i,j)
!                    TravellerTripOriginMAZ(i,jj)=TravellerTripOriginMAZ(i,j)
!                    TravellerTripOriginTAZ(i,jj)=TravellerTripOriginTAZ(i,j)
!                    TravellerTripDestinationMAZ(i,jj)=TravellerTripDestinationMAZ(i,j)
!                    TravellerTripDestinationTAZ(i,jj)=TravellerTripDestinationTAZ(i,j)
!                    TravellerTripOriginPurpose(i,jj)=TravellerTripOriginPurpose(i,j)
!                    TravellerTripDestinationPurpose(i,jj)= TravellerTripDestinationPurpose(i,j)
!                    TravellerTripStartTimeInterval(i,jj)=TravellerTripStartTimeInterval(i,j)
!                    TripDriverPassengerFlag(i,jj)=TripDriverPassengerFlag(i,j)
!                    ABMTripIndex(i,jj)=ABMTripIndex(i,j)
!                    
!                    !Joint
!                    TravellerTripDriver(i,jj)=TravellerTripDriver(i,j)
!                    TravellerTripTourID(i,jj)=TravellerTripTourID(i,j)
!                    TravellerTripJointFlag(i,jj)=TravellerTripJointFlag(i,j)
!                    
!                    TravellerTripDestinationParkingTaz(i,jj)=TravellerTripDestinationParkingTaz(i,j)
!                    
!                    
!                    TravellerTripMode(i,j)=trip_mode
!                    TravellerTripOriginMAZ(i,j)=orig_maz
!                    TravellerTripOriginTAZ(i,j)=orig_taz
!                    TravellerTripDestinationMAZ(i,j)=dest_maz
!                    TravellerTripDestinationTAZ(i,j)=dest_taz
!                    TravellerTripOriginPurpose(i,j)=orig_purpose
!                    TravellerTripDestinationPurpose(i,j)=dest_purpose
!                    TravellerTripStartTimeInterval(i,j)=stop_period
!                    TripDriverPassengerFlag(i,j)=trip_driver_passenger
!                    ABMTripIndex(i,j)=abm_trip_index
!                    !Joint
!                    TravellerTripDriver(i,j)=driver_id
!                    TravellerTripTourID(i,j)=tour_id_temp
!                    TravellerTripJointFlag(i,j)=TravellerTripJointFlagTemp
!                    TravellerTripDestinationParkingTaz(i,j)=parking_taz
!                endif
!            endif            
!        enddo            
!   enddo
!---------------------------------------------------
!Read the starttime interval
do i=1,MaxNumTimeInterval
    read(5,*)  StartTimeofTimeInterval,LengthofTimeInterval
    VectorStartTimeofTimeInterval(i)=StartTimeofTimeInterval
    VectorLengthofTimeInterval(i)=LengthofTimeInterval
enddo

close(5)
!-------------------------------------------------------------------
!!Create the start time for each trip
!do i=1,index
!    jj1=1
!    jj2=1
!    do while (jj2.le.TravellerNumTrips(i))
!        TNInInterval=0
!        ST=TravellerTripStartTimeInterval(i,jj1)
!        do while (TravellerTripStartTimeInterval(i,jj2).eq.ST) 
!            TNInInterval=TNInInterval+1
!            !if(TravellerTripParkRideFlag(i,jj2).gt.0) TNInInterval=TNInInterval+1
!            jj2=jj2+1
!        enddo
!        !generate the start time
!        do jjj=jj1,jj2-1           
!            TripStartTime(i,jjj)=(VectorStartTimeofTimeInterval(ST)+VectorLengthofTimeInterval(ST)/TNInInterval*(jjj-jj1))+VectorLengthofTimeInterval(ST)/TNInInterval*rand(0)
!            TripStartTime(i,jjj)=nint(TripStartTime(i,jjj)*10.0)/10.0
!            if(TripStartTime(i,jjj).lt.0.1) TripStartTime(i,jjj)=0.1
!        enddo
!        
!!        if(TripStartTime(i,jjj).eq.0) then 
!!            write(*,*) i,j,m,n,otap,dtap,SpSkimTime,TripStartTime(i,j),SpSkimTimeInterval
!!        endif
!        jj1=jj2
!        !if(jj2.eq.0) write(*,*) Traveller_ID(i),TravellerTripStartTimeInterval(,jj2+1)
!    enddo  
!enddo


!Prepare for transitzone selection for PNR riders
write(*,*) "Park and ride path assignment"	    
!------------------------------------------------------------------   
!TDSP Allocation
Allocate(TempVOT(6))
Allocate(TempCost(6))
Allocate(TempTime(6))
Allocate(TempDistance(6))
!open(file='NetworkAtt.dat',unit=25,status='unknown')
!read(25,*) NumOriginZone, NumSuperZone, NumIntervals

open(file='vehicularPNRSkim.dat', unit=31)
! Read TDSP
read(31,*) value1,SPSkimFirstEnter
if(SPSkimFirstEnter.eq."NTI") then 
    NumIteration=0
    NumSkimIntervals=value1
    read(31,*) NumSkimSuperZone
    read(31,*) NumSkimOriginZone
else
    NumIteration=1
    NumSkimSuperZone=value1
    read(31,*) NumSkimOriginZone
    read(31,*) NumSkimIntervals
endif
if (.NOT. ALLOCATED(ODTATT_Array)) then
Allocate(ODTATT_Array(NumSkimOriginZone,NumSkimSuperZone ,NumSkimIntervals),stat=error)
endif

if (NumIteration.gt.0) then
    do D=1,NumSkimSuperZone 
      do O=1,NumSkimOriginZone
        do T=1,NumSkimIntervals
        read(31,400) NumOfVOTGs,(TempVOT(nvt),TempCost(nvt),TempTime(nvt),TempDistance(nvt), nvt=1,NumOfVOTGs)
        ODTATT_Array(O,D,T)%Psize=NumOfVOTGs
        call ODTATTAllocate(O,D,T,NumOfVOTGs)
         do i=1,NumOfVOTGs
           call ODTATTInsert(O,D,T,i,TempVOT(i),TempCost(i),TempTime(i),TempDistance(i))
         enddo     
        enddo
      enddo
    enddo
else 
    do T=1,NumSkimIntervals
        do D=1,NumSkimSuperZone 
            do O=1,NumSkimOriginZone
            read(31,400) NumOfVOTGs,(TempVOT(nvt),TempCost(nvt),TempTime(nvt),TempDistance(nvt), nvt=1,NumOfVOTGs)
            ODTATT_Array(O,D,T)%Psize=NumOfVOTGs
            call ODTATTAllocate(O,D,T,NumOfVOTGs)
            do i=1,NumOfVOTGs
              call ODTATTInsert(O,D,T,i,TempVOT(i),TempCost(i),TempTime(i),TempDistance(i))
            enddo     
            enddo
        enddo
    enddo    
endif
!--------------------------------------------------------------------------------

!Read the link travel time
open(file='output_td_linktraveltime.dat',unit=19)
read(19,*)value
read(19,*)NumLinkInterval
read(19,*)SkimHorizon
read(19,*)value
read(19,*) value
LinkTimeInterval=SkimHorizon/(NumLinkInterval-1)
SpSkimTimeInterval=SkimHorizon/(NumSkimIntervals-1)
allocate(LinkTravelTime(NumLink,NumLinkInterval))
    LinkTravelTime(:,:)=0
allocate(NodeIndex(MaxNodeID))
    NodeIndex(:)=0
read(19,*) tinterval
do j=1,NumLink
    read(19,'(a)',iostat=error) Line
    read(Line,*,iostat=error) value,nodea,nodeb,value,linktt
    !write(*,*) value,nodea,nodeb,value,linktt
    NodeIndex(nodea)=1
    NodeIndex(nodeb)=1
enddo
close(19)
j=0
do i=1,MaxNodeID
    if(NodeIndex(i).eq.1) then 
        j=j+1
        NodeIndex(i)=j
    endif
enddo
open(file='output_td_linktraveltime.dat',unit=19)
allocate(LinkNumber(NumNode,NumNode))
    LinkNumber(:,:)=0
do i=1,NumLinkInterval
    do j=1,5
        read(19,*)value
    enddo
    read(19,*) tinterval
    do j=1,NumLink
        !read(19,'(a)',iostat=error) Line
        read(19,*,iostat=error) value,nodea,nodeb,value,linktt
        LinkNumber(NodeIndex(nodea),NodeIndex(nodeb))=j
        LinkTravelTime(j,tinterval)=linktt
    enddo
    read(19,*)
enddo

!--------------------------------------------------
!Read Turn Penalty Time 
open(file='output_td_turnpenalty.dat',unit=20)
MaxNumMovement=11
allocate(TurnPenaltyTime(NumLink,MaxNumMovement,NumLinkInterval))
    TurnPenaltyTime(:,:,:)=0
allocate(MovementLink(NumLink,MaxNumMovement))
    MovementLink(:,:)=0
allocate(NumMovementLink(NumLink))
    NumMovementLink(:)=0 
do i=1,NumLinkInterval
    do j=1,5
        read(20,*)value
    enddo
    read(20,*) tinterval
    
    do j=1,NumTurn
        !read(20,'(a)',iostat=error) Line
        read(20,*,iostat=error) value,nodea,nodeb,nodec,value,turntt
        TurnLinkIndex=LinkNumber(NodeIndex(nodea),NodeIndex(nodeb))
        if(i.eq.1) then
            NumMovementLink(TurnLinkIndex)=NumMovementLink(TurnLinkIndex)+1
            MovementLink(TurnLinkIndex,NumMovementLink(TurnLinkIndex))=nodec 
        endif
        k=1
        do while(nodec.ne.MovementLink(TurnLinkIndex,k))
            k=k+1
        enddo
        TurnPenaltyTime(TurnLinkIndex,k,tinterval)=turntt
    enddo
    read(20,*)
enddo
!write(*,*) "alex"

!---------------------------------------------------------------------
!Read the transi skim files
!open(file='TransitPNRSkim_AB_time.dat',unit=32)
if(TransitMazTazFlag.eq.0) then
    open(file='TransitPNRSkim_AB_cost.dat',unit=33)
    open(file='TransitPNRSkim_BA_time.dat',unit=34)
    open(file='TransitPNRSkim_BA_cost.dat',unit=35)
else
    open(file='TransitPNRSkim_AB_cost_TAZ.dat',unit=33)
    open(file='TransitPNRSkim_BA_time_TAZ.dat',unit=34)
    open(file='TransitPNRSkim_BA_cost_TAZ.dat',unit=35)
endif

allocate(TransitPairIndex(NumTotalTransitSkimZone,NumTotalTransitSkimZone))
    TransitPairIndex(:,:)=0
!error=1
!i=1

do i=1,NumPairTransitSkim
    !read(32,*) value,value,TransitSkimABTime(i,1:MaxNumTransitSkimTimeInterval)
    read(33,*) TransitPNRZonePair(i,1),TransitPNRZonePair(i,2),TransitSkimABCost(i,1:MaxNumTransitSkimTimeInterval)
    read(34,*) value,value,TransitSkimBATime(i,1:MaxNumTransitSkimTimeInterval)
    read(35,*) value,value,TransitSkimBACost(i,1:MaxNumTransitSkimTimeInterval)
    TransitPairIndex(TransitPNRZonePair(i,1),TransitPNRZonePair(i,2))=i
enddo
close(32)
close(33)
close(34)
close(35)
!--------------------------------------------------------------------
!Read Walk Skim
if(TransitMazTazFlag.eq.0) then    
    open(file='input_threelink_walk.dat',unit=36)
else
    open(file='input_threelink_walk_TAZ.dat',unit=36)
endif
read(36,*)
PNRWalkZonetemp=0
NumTransitWalkOptionTemp=0
do i=1,NumPairPNRWalkZone
    read(36,*,iostat=error)PNRWalkZone(i,1),PNRWalkZone(i,2),walkdistance
    if(error.ne.-1) then
        if(PNRWalkZone(i,1).eq.PNRWalkZonetemp) then 
            NumTransitWalkOptionTemp=NumTransitWalkOptionTemp+1
            NumTransitWalkZoneCa(PNRZone(i,1))=NumTransitWalkOptionTemp
        else
            NumTransitWalkOptionTemp=1
            PNRWalkZonetemp=PNRWalkZone(i,1)
            NumTransitWalkZoneCa(PNRWalkZone(i,1))=NumTransitWalkOptionTemp
        endif
        TransitWalkZone(PNRWalkZone(i,1),NumTransitWalkOptionTemp)=PNRWalkZone(i,2)
        TransitSkimWalkTime(PNRWalkZone(i,1),NumTransitWalkOptionTemp)=(walkdistance/WalkSpeed)/60
        !maz_zone_walk_time(zone1,j) = float(int(maz_zone_walk_time(zone1,j)))
        !if (maz_zone_walk_time(zone1,j).gt.3600) maz_zone_walk_time(zone1,j) = inf   
        !TransitWalkPairIndex(PNRWalkZone(i,1),PNRWalkZone(i,2))=i
    endif 
enddo
close(36)


!-------------------------------------------------------------------    
!Mark the park and ride flag   
write(*,*) "Mark PNR Flag"
SameOriginDestinationParkRide=0
numofparkandride=0
!allocate(PNROutBound
do i=1,index
    j=1
    ParkRideFoundFlag=0
    TransitTemp=0
    TransitTemp1=0
    walktimetemp=0
    do while (j.le.TravellerNumTrips(i))
        if ((TravellerTripMode(i,j).eq.11).or.(TravellerTripMode(i,j).eq.12)) then
            ParkRideFoundFlag=ParkRideFoundFlag+1
            TravellerTripParkRideFlag(i,j)=ParkRideFoundFlag
            if(TravellerTripParkRideFlag(i,j).gt.0) then
                if (mod(TravellerTripParkRideFlag(i,j),2).eq.1) then
                     TravellerTripParkRideFlag(i,j)=1
                elseif (mod(TravellerTripParkRideFlag(i,j),2).eq.0) then
                    TravellerTripParkRideFlag(i,j)=2
                endif
            endif
            if (TravellerTripParkRideFlag(i,j).eq.1) then
                PNRTravelTime=10000000
                if(TransitMazTazFlag.eq.0) then 
                    otranz=TravellerTripOriginMAZ(i,j)
                    dtranz=TravellerTripDestinationMAZ(i,j)
                elseif(TransitMazTazFlag.eq.1) then
                    otranz=TravellerTripOriginTAZ(i,j)
                    dtranz=TravellerTripDestinationTAZ(i,j)
                endif
                
                do m=1,NumTransitZoneCa(otranz)
                    do n=1,NumTransitWalkZoneCa(dtranz)
                        !Calculate the time of driving part
                       
                        otap=TransitZone(otranz,m)
                        dtap=TransitWalkZone(dtranz,n)
                        if(otap.eq.0) then 
                            write(*,*) "maztotazmap error",i,j,m,n,otap,dtap,otranz,dtranz
                        endif
                        if(TransitMazTazFlag.eq.0) then 
                            otaptranz=MaztoTazMap(otap)
                        elseif(TransitMazTazFlag.eq.1) then
                            otaptranz=otap
                        endif
                        !Calculate the time for driving
                        pathindex=IntraSuperzonePathIndex(TravellerTripOriginTAZ(i,j),otaptranz)
                        if(pathindex.eq.0) then !Read from the vehicularPNRSkim.dat
                            !Determine the SP skim timeinterval
                            SpSkimTime=ceiling(TripStartTime(i,j)/SpSkimTimeInterval)
                            if(SpSkimTime.gt.numskimintervals) SpSkimTime=numskimintervals
                            if(SpSkimTime.eq.0) then 
                                write(*,*) i,j,m,n,otap,dtap,SpSkimTime,TripStartTime(i,j),SpSkimTimeInterval
                            endif
                            !Determine VOT group number
                            vv=1
                            !do while(average_value_of_time.gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                            do while(ValueofTime(Traveller_ID(i)).gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                                vv=vv+1
                            enddo
                            drivelegtime=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%Time
                            drivelegcost=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%Cost
                        else  !Read from the link travel time and turn penalty
                            currentDYNASMARTtime=TripStartTime(i,j)
                            do k=1,NumIntraSuperzoneZone(pathindex)-2
                                !Add all link travel time together
                                nodea=PathBank(pathindex,k)
                                nodeb=PathBank(pathindex,k+1)
                                nodec=PathBank(pathindex,k+2)
                                upstreamlinkNum=LinkNumber(NodeIndex(nodea),NodeIndex(nodeb))
                                currentDYNASMARTtimeinterval=ceiling(currentDYNASMARTtime/LinkTimeInterval)  !change the 10 to a variable
                                if(currentDYNASMARTtimeinterval.gt.NumLinkInterval) currentDYNASMARTtimeinterval=NumLinkInterval
                                !currentDYNASMARTtime=currentDYNASMARTtime+LinkTravelTime(upstreamlinkNum,currentDYNASMARTtimeinterval)
                                currentDYNASMARTtime=currentDYNASMARTtime+LinkTravelTime(upstreamlinkNum,currentDYNASMARTtimeinterval)
                                !if(currentDYNASMARTtime.lt.VectorStartTimeofTimeInterval(j)) write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,1
                                z=1
                                do while(nodec.ne.MovementLink(upstreamlinkNum,z))
                                    z=z+1
                                enddo
                                !currentDYNASMARTtimeinterval=ceiling(currentDYNASMARTtime/10)
                                currentDYNASMARTtimeinterval=ceiling(currentDYNASMARTtime/LinkTimeInterval)
                                if(currentDYNASMARTtimeinterval.gt.NumLinkInterval) currentDYNASMARTtimeinterval=NumLinkInterval
                                !write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,1
                                currentDYNASMARTtime=currentDYNASMARTtime+TurnPenaltyTime(upstreamlinkNum,z,currentDYNASMARTtimeinterval)
                                !if(currentDYNASMARTtime.lt.VectorStartTimeofTimeInterval(j)) write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,2
                            enddo
                            nodea=NodeIndex(PathBank(pathindex,NumIntraSuperzoneZone(pathindex)-1))
                            nodeb=NodeIndex(PathBank(pathindex,NumIntraSuperzoneZone(pathindex)))
                            !currentDYNASMARTtimeinterval=ceiling(currentDYNASMARTtime/10.0)
                            currentDYNASMARTtimeinterval=ceiling(currentDYNASMARTtime/LinkTimeInterval)
                            if(currentDYNASMARTtimeinterval.gt.NumLinkInterval) currentDYNASMARTtimeinterval=NumLinkInterval
                            if(currentDYNASMARTtimeinterval.eq.0) write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,1
                            currentDYNASMARTtime=currentDYNASMARTtime+LinkTravelTime(LinkNumber(nodea,nodeb),currentDYNASMARTtimeinterval)
                            drivelegtime=currentDYNASMARTtime-TripStartTime(i,j)
                            drivelegcost=0
                        endif
                        !Travel time of transit part
                        PRNTransitStartTime=drivelegtime+TripStartTime(i,j)
                        if(otap.eq.0) then
                            write(*,*) "otap=0",i,j,Traveller_ID(i),otranz,dtranz,otap,dtap
                        elseif(dtap.eq.0) then 
                            write(*,*) "dtap=0",i,j,Traveller_ID(i),otranz,dtranz,otap,dtap
                        endif
                        if(TransitPairIndex(otap,dtap).gt.0) then
                            PNRTransitTimeInterval=ceiling(PRNTransitStartTime/TransitSkimTimeIntervalLength)
                            if(PNRTransitTimeInterval.gt.MaxNumTransitSkimTimeInterval)PNRTransitTimeInterval=MaxNumTransitSkimTimeInterval
                            !transitlegtime=TransitSkimABTime(TransitPairIndex(otap,dtap),PNRTransitTimeInterval)
                            transitlegcost=TransitSkimABCost(TransitPairIndex(otap,dtap),PNRTransitTimeInterval)
                        else 
                            transitlegtime=10000
                            transitlegcost=10000
                        endif
                        !Calculate the time of walking park
                        walklegtime=TransitSkimWalkTime(dtranz,n)
                        PNRTravelTimeTemp=drivingweight*(drivelegtime+drivelegcost/ValueofTime(Traveller_ID(i)))+walkingweight*walklegtime+transitlegcost
                        !update the best route
                        if(PNRTravelTimeTemp.lt.PNRTravelTime) then
                            PNRTravelTime=PNRTravelTimeTemp
                            if(TransitMazTazFlag.eq.0) then 
                                TravellerTripOriginTransitMAZ(i,j)=otap
                                TravellerTripDestinationTransitMAZ(i,j)=dtap
                            else
                                TravellerTripOriginTransitTAZ(i,j)=otap
                                TravellerTripDestinationTransitTAZ(i,j)=dtap
                            endif 
                            TransitTemp=otap 
                            TransitTemp1=dtap
                            PNRaTime(i,j)=drivelegtime
                            walktimetemp=walklegtime

                        endif
                    enddo
                enddo    
                if(TransitTemp.eq.0) write(*,*) "TransitTempError",i,j,otranz,dtranz,otap,dtap,NumTransitZoneCa(otranz),NumTransitWalkZoneCa(dtranz)
            elseif(TravellerTripParkRideFlag(i,j).eq.2) then
                if(TransitMazTazFlag.eq.0) then 
                    TravellerTripDestinationTransitMAZ(i,j)=TransitTemp
                else
                    TravellerTripDestinationTransitTAZ(i,j)=TransitTemp
                endif
                dtap=TransitTemp
                PNRTravelTime=10000000
                if(TransitMazTazFlag.eq.0) then 
                    otranz=TravellerTripOriginMAZ(i,j)
                    dtranz=TravellerTripDestinationMAZ(i,j)
                elseif(TransitMazTazFlag.eq.1) then
                    otranz=TravellerTripOriginTAZ(i,j)
                    dtranz=TravellerTripDestinationTAZ(i,j)
                endif
                if(dtap.eq.0) then 
                    write(*,*) "maztotazmap error12",i,j,m,n,otap,dtap,otranz,dtranz
                endif
                do m=1,NumTransitWalkZoneCa(otranz)
                    otap=TransitWalkZone(otranz,m)
                    if(TransitMazTazFlag.eq.0) then 
                        dtaptranz=MaztoTazMap(dtap)
                    elseif(TransitMazTazFlag.eq.1) then
                        dtaptranz=dtap
                    endif
                    !Calculate the time of walking part
                    walklegtime=TransitSkimWalkTime(otranz,m)
                    
                    !Travel time of transit part
                    
                     if(otap.eq.0) then
                        write(*,*) "otap=0",i,j,Traveller_ID(i),otranz,dtranz,otap,dtap
                    elseif(dtap.eq.0) then 
                        write(*,*) "dtap=0",i,j,Traveller_ID(i),otranz,dtranz,otap,dtap
                    endif
                    
                    if(dtaptranz.eq.0) then 
                        write(*,*) i,j,Traveller_ID(i),otranz,dtranz,otap,dtap,maztotazmap(dtap)
                        pause
                    endif
                    PRNTransitStartTime=walklegtime+TripStartTime(i,j)
                    if(TransitPairIndex(dtap,otap).gt.0) then
                        PNRTransitTimeInterval=ceiling(PRNTransitStartTime/TransitSkimTimeIntervalLength)
                         if(PNRTransitTimeInterval.gt.MaxNumTransitSkimTimeInterval)PNRTransitTimeInterval=MaxNumTransitSkimTimeInterval
                        transitlegtime=TransitSkimBATime(TransitPairIndex(dtap,otap),PNRTransitTimeInterval)
                        transitlegcost=TransitSkimBACost(TransitPairIndex(dtap,otap),PNRTransitTimeInterval)
                    else 
                        transitlegtime=10000
                        transitlegcost=10000
                    endif
                    
               
                    
                    !Calculate the time for driving
!                    pathindex=IntraSuperzonePathIndex(dtaptranz,TravellerTripDestinationTAZ(i,j))
!                    if(pathindex.eq.0) then !Read from the vehicularPNRSkim.dat
!                        !Determine the SP skim timeinterval
!                        SpSkimTime=ceiling((TripStartTime(i,j)+transitlegtime+walklegtime)/SpSkimTimeInterval)
!                        if(SpSkimTime.gt.NumIntervals) SpSkimTime=NumIntervals
!                        !Determine VOT group number
!                        vv=1
!                        do while(average_value_of_time.gt.ODTATT_Array(dtaptranz,SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%VOT)
!                            vv=vv+1
!                        enddo
!                        drivelegtime=ODTATT_Array(dtaptranz,SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                    else  !Read from the link travel time and turn penalty
!                        currentDYNASMARTtime=TripStartTime(i,j)+transitlegtime+walklegtime
!                        do k=1,NumIntraSuperzoneZone(pathindex)-2
!                            !Add all link travel time together
!                            nodea=PathBank(pathindex,k)
!                            nodeb=PathBank(pathindex,k+1)
!                            nodec=PathBank(pathindex,k+2)
!                            upstreamlinkNum=LinkNumber(NodeIndex(nodea),NodeIndex(nodeb))
!                            currentDYNASMARTtimeinterval=int(ceiling(currentDYNASMARTtime/10) ) !change the 10 to a variable
!                            if(currentDYNASMARTtimeinterval.gt.NumLinkInterval) currentDYNASMARTtimeinterval=NumLinkInterval
!                            !currentDYNASMARTtime=currentDYNASMARTtime+LinkTravelTime(upstreamlinkNum,currentDYNASMARTtimeinterval)
!                            currentDYNASMARTtime=currentDYNASMARTtime+LinkTravelTime(upstreamlinkNum,currentDYNASMARTtimeinterval)
!                            !if(currentDYNASMARTtime.lt.VectorStartTimeofTimeInterval(j)) write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,1
!                            z=1
!                            do while(nodec.ne.MovementLink(upstreamlinkNum,z))
!                                z=z+1
!                            enddo
!                            currentDYNASMARTtimeinterval=ceiling(currentDYNASMARTtime/10)
!                            if(currentDYNASMARTtimeinterval.gt.NumLinkInterval) currentDYNASMARTtimeinterval=NumLinkInterval
!                            !write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,1
!                            currentDYNASMARTtime=currentDYNASMARTtime+TurnPenaltyTime(upstreamlinkNum,z,currentDYNASMARTtimeinterval)
!                            !if(currentDYNASMARTtime.lt.VectorStartTimeofTimeInterval(j)) write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,2
!                        enddo
!                        nodea=NodeIndex(PathBank(pathindex,NumIntraSuperzoneZone(pathindex)-1))
!                        nodeb=NodeIndex(PathBank(pathindex,NumIntraSuperzoneZone(pathindex)))
!                        currentDYNASMARTtimeinterval=ceiling(currentDYNASMARTtime/10.0)
!                        if(currentDYNASMARTtimeinterval.gt.NumLinkInterval) currentDYNASMARTtimeinterval=NumLinkInterval
!                        if(currentDYNASMARTtimeinterval.eq.0) write(*,*) i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,1
!                        currentDYNASMARTtime=currentDYNASMARTtime+LinkTravelTime(LinkNumber(nodea,nodeb),currentDYNASMARTtimeinterval)
!                        drivelegtime=currentDYNASMARTtime-(TripStartTime(i,j)+transitlegtime+walklegtime)
!                    endif

                    PNRTravelTimeTemp=walkingweight*walklegtime+transitlegcost
                    !update the best route
                    if(PNRTravelTimeTemp.lt.PNRTravelTime) then
                        PNRTravelTime=PNRTravelTimeTemp
                        if(TransitMazTazFlag.eq.0) then 
                            TravellerTripOriginTransitMAZ(i,j)=otap
                            TravellerTripDestinationTransitMAZ(i,j)=dtap
                        else
                            TravellerTripOriginTransitTAZ(i,j)=otap
                            TravellerTripDestinationTransitTAZ(i,j)=dtap
                        endif 
                    endif
                    
                enddo 
                PNRaTime(i,j)=walklegtime+transitlegtime

!                if(TransitTemp.eq.0) then
!                    write(3,*) "error123",i,j,Traveller_ID(i),TransitTemp,PNRTravelTimeTemp,PNRTravelTime
!                    pause
!                endif
!                if(TransitTemp1.eq.0) then 
!                    write(*,*) "error123",i,j,Traveller_ID(i),NumTransitZoneCa(otranz),NumTransitWalkZoneCa(dtranz)
!                    pause
!                endif   
                TransitTemp=0
                TransitTemp1=0
            endif     
            j=j+1        
            numofparkandride=numofparkandride+1                
        else
            j=j+1
        endif
    enddo 
    if (ParkRideFoundFlag.eq.1) then
        write(3,*) 'Number of trips with park and ride mode is odd for person_id and person_index ', Traveller_ID(i), i
        pause        
    endif
enddo

deallocate(LinkTravelTime)
deallocate(TurnPenaltyTime)
deallocate(ODTATT_Array)
deallocate(TransitZone)
deallocate(TransitWalkZone)
deallocate(NumTransitZoneCa)
deallocate(NumTransitWalkZoneCa)
!-------------------------------------------------------------------       	    
!Update the validcar
do i=1,index
    do j=1,MaxNumTrips
        if(TripDriverPassengerFlag(i,j).gt.0) ValidCar(i)=1
    enddo
enddo
!-------------------------------------------------------------------
!After sorting all the trips for each traveller based on the start time, origin-destination zone and purpose, the daily chain of trips consistency will be checked
!   WorkCounter=0
!   NumTraveller=0
!   NumCar=0
   do i=1,index
        !Check the first trip purpose (assuming everyone will start their daily chain of trips from Home)
        if (TravellerTripOriginPurpose(i,1).ne.Home) then
            write(3,*) 'The first trip is not started from home for Person_ID ', Traveller_ID(i),i
            ValidTraveller(i)=0  
            if(TravellerTripDestinationParkingTaz(i,1).gt.0) then
                if(TravellerTripDestinationParkingTaz(i,1).ne.TravellerTripDestinationMAZ(i,1)) write(3,*)Traveller_ID(i),'inconsistent type 4' 
            endif       
        endif
        LatestCarTrip=0
        ! In this loop two objectives will be acheived, the consistency of the trips will be check and for the car trips number of first time interval trips will be calculated to be used at thevehicle generation time (simulation minute), also car trip consistency will be checked
        do j=1,TravellerNumTrips(i)-1          
            !Trip consistency check for all the trips (including the last trip)
            if (((TravellerTripDestinationPurpose(i,j).ne.TravellerTripOriginPurpose(i,j+1))).or.(TravellerTripDestinationMAZ(i,j).ne.TravellerTripOriginMAZ(i,j+1))) then
                if (((TravellerTripDestinationPurpose(i,j).eq.work1.and.TravellerTripOriginPurpose(i,j+1).eq.work2).or.(TravellerTripDestinationPurpose(i,j).eq.work2.and.TravellerTripOriginPurpose(i,j+1).eq.work1)).and.(TravellerTripDestinationMAZ(i,j).eq.TravellerTripOriginMAZ(i,j+1))) then
                    WorkCounter=WorkCounter+1
                else
                    !write(3,*) 'The trips order is not valid for person_id ', Traveller_ID(i),' and trip ',j,TravellerTripOriginPurpose(i,j),TravellerTripDestinationMAZ(i,j)
                    ValidTraveller(i)=0
                    if(TravellerTripDestinationParkingTaz(i,j).gt.0) then
                        if(TravellerTripDestinationParkingTaz(i,j).ne.TravellerTripDestinationMAZ(i,j)) write(3,*)Traveller_ID(i),'inconsistent type 4'   
                    endif
                endif 
            endif            
        enddo
        !Check the last trip purpose which requires the daily chain of trips to be ended at Home
        if (TravellerTripDestinationPurpose(i,j).ne.Home) then
            write(3,*) 'The last trip is not ended at home for person_id ', Traveller_ID(i)
            ValidTraveller(i)=0
            if(TravellerTripDestinationParkingTaz(i,j).gt.0) then
                if(TravellerTripDestinationParkingTaz(i,j).ne.TravellerTripDestinationMAZ(i,j)) write(3,*)Traveller_ID(i),'inconsistent type 4'  
!                
            endif 
        endif    
   enddo  
   
 

!___________________________________________________________________
!Generate the new vehicle error
WorkCounter=0
!NumTraveller=0
NumCar=0           
do i=1,NumTraveller
    if (ValidTraveller(i).eq.1) then
        !NumTraveller=NumTraveller+1
        NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))+1
        if (MaxNumTravellerInEachTimeInterval.lt.NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))) MaxNumTravellerInEachTimeInterval=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))
    endif
enddo
!NumCar=NumCar+MaxNumTimeInterval*MaxNumInternalTAZZone




!-------------------------------------------------------------------       	    
!Assign each traveller (car) to a time interval to start its trip
!Also sort all the assigned travellers to each time interval based on their number of trips in that time interval (TravellerNumFirstTimeIntervalTrip and CarNumFirstTimeIntervalTrip)
!Travellers (cars) with higher number of trips in each time interval (their first time interval) should be generated earlier 
!TravellerInEachTimeInterval (CarInEachTimeInterval) will include the index of travellers (Cars) at each time interval sorted based on the number of trips at that interval. This can be allocated now as MaxNumTravellerInEachTimeInterval is updated in the previous section 
   !6-17 transitpassenger 
   
!Sorting Travelers
write(*,*) "Sorting Travelers"
   allocate (TravellerInEachTimeInterval(MaxNumTimeInterval,MaxNumTravellerInEachTimeInterval))
   TravellerInEachTimeInterval(:,:)=0
   NumTravellerInTimeInterval(:)=0
   
   !Go over all the travellers and assign each of them to one time interval 
   do i=1,index
        if (ValidTraveller(i).eq.1) then
            NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))+1            
            if (NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1)).eq.1) then !The first traveller for that time interval and no sorting
                TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1)))=i
            endif     
                  
!            j=1
!            EndofLoop=0
!            do while(j.lt.NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1)).and.EndofLoop.eq.0) !search for the proper place if it is not the first traveller for that time interval
!                if (TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j),1).gt.TripStartTime(i,1)) then !proper place
!                    do jj=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))-1,j,-1 !shift all the remaining
!                        TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jj+1)=TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jj)
!                    enddo
!                    TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j)=i !put the traveller at the proper place
!                    EndofLoop=1
!                elseif (j.eq.NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))-1) then !The proper place is the last place
!                    TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j+1)=i
!                endif
!                j=j+1
!            enddo  
            
            jleft=1
            jright=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))-1
            j=ceiling(jright*1.0/2.0)
            EndofLoop=0
            
            do while(EndofLoop.eq.0.and.NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1)).gt.1)
                if(abs(jleft-jright).gt.1) then
                    if(TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j),1).gt.TripStartTime(i,1)) then
                        jright=j
                        j=ceiling((jleft+jright)/2.0)
                    elseif(TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j),1).lt.TripStartTime(i,1))then
                        jleft=j
                        j=ceiling((jleft+jright)/2.0)
                    else
                        if(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j).gt.i) then 
                            jright=j
                            j=ceiling((jleft+jright)/2.0)
                        else
                            jleft=j
                            j=ceiling((jleft+jright)/2.0)
                        endif
                    endif
                    
                else    !Found the proper place
!                    if(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jright).eq.24192) then 
!                        write(*,*) i,jright
!                    endif
                    if(TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jleft),1).gt.TripStartTime(i,1)) then
                        j=jleft

                    elseif(TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jright),1).lt.TripStartTime(i,1))then
                        j=jright+1
                    
                    elseif(TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jleft),1).lt.TripStartTime(i,1).and.TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jright),1).gt.TripStartTime(i,1)) then
                        j=jleft+1
                      
                    else
                        if(TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jleft),1).eq.TripStartTime(i,1)) then 
                             if(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jleft).gt.i) then 
                                j=jleft
                               
                             else
                                j=jleft+1
                                
                             endif
                        elseif(TripStartTime(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jright),1).eq.TripStartTime(i,1)) then
                            if(TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jright).gt.i) then 
                                j=jright
                               
                             else
                                j=jright+1
                               
                             endif
                        endif
                    endif
                    
                    EndofLoop=1
                    do jj=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))-1,j,-1 !shift all the remaining
                        TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jj+1)=TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jj)
                    enddo
                    TravellerInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j)=i !put the traveller at the proper place
                    EndofLoop=1
                endif
            enddo
        endif
   enddo


!-------------------------------------------------------------------       	      
!Outputing traveller.dat in the same format of vehicle.dat (Dynasmart input)
!Note that selecting a generation link for a traveller might be meaningless
!Each valid traveller is assigned to one time interval (the time interval of the first trip for that traveller)
!In this approach the travellers will be reported based on an increasing order of start time
!Currently activity durations are generated randomly, while they should be as the input from ABM, which we need to change in future
!   counter=0 
!   NumCarTrip=0
!   write(*,*)'Writing traveler.dat ...'
!   write(8,5) NumTraveller, RealMaxNumTrips
!   write(8,16) 
!
!   write(8,15) 
!   open(file='TimeInterval.dat',unit=5)
!   do t=1,MaxNumTimeInterval
!       !read(5,*)  StartTimeofTimeInterval,LengthofTimeInterval
!       do k=1,NumTravellerInTimeInterval(t)
!            i=TravellerInEachTimeInterval(t,k) !getting the traveller index
!            counter=counter+1
!!            TravellerStartTime(i)=StartTimeofTimeInterval+(LengthofTimeInterval/NumTravellerInTimeInterval(t))*(k-1) !unifrom distribution of travellers over the time interval
!!            temp1=1.0 !The precision of start time (minute), where 1, makes xx.00, 10, xx.x0, and 100, xx.xx (the latest is not acceptable in Dynasmart)
!!            StartTime=(nint(TravellerStartTime(i)*temp1))/temp1
!            temp1=10.0
!            StartTime=(nint(TripStartTime(i,1)*temp1))/temp1
!    
!	        !writing the genral information of the traveller
!	        !write(8,6) counter,i,Traveller_ID(i),iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,TravellerNumTrips(i),infotmp,ribftmp,comptmp,TAZMap(TravellerTripOriginTAZ(i,1)) 
!	        write(8,12) counter,i,Traveller_ID(i),StartTime,TravellerNumTrips(i),TravellerTripOriginTAZ(i,1),ValidCar(i),FirstCarTrip(i,1),FirstTransitTrip(i),ValueOfTime(Traveller_ID(i))
!            
!            !writing the trips information
!            temp3=30.0 !constant used for the activity time generation
!            do j=1,TravellerNumTrips(i)-1  !go over all the trips except the last trip
!                temp1=rand(0)                
!                if (TravellerTripStartTimeInterval(i,j+1).eq.TravellerTripStartTimeInterval(i,j)) then !Zero activity time between two consequent trips with the same start time interval
!                    ActivityTime=0.0
!                    TravellerActivityTime(i,j)=0.0
!                else !Random number generation based on the differences of start time interval between two consequent trips and an additional random number
!                    !ActivityTimeTemp=(TravellerTripStartTimeInterval(i,j+1)-TravellerTripStartTimeInterval(i,j))-temp3*temp1
!                    ActivityTimeTemp=(TripStartTime(i,j+1)-TripStartTime(i,j))-temp3*temp1
!                    temp1=1.0
!                    ActivityTime=(nint(ActivityTimeTemp*temp1))/temp1 !The precision of Activity duration (minute), where 1, makes xx.00, 10, xx.x0, and 100, xx.xx (the latest is not acceptable in Dynasmart)
!                    if(ActivityTime.lt.0) ActivityTime=0.0
!                    TravellerActivityTime(i,j)=ActivityTime
!                endif
!                !write the trip information
!                !write(8,7) TAZMap(TravellerTripDestinationTAZ(i,j)),ActivityTime,TravellerTripMode(i,j) 
!                write(8,13) TravellerTripDestinationTAZ(i,j),TravellerActivityTime(i,j),TravellerTripMode(i,j),TravellerTripOriginPurpose(i,j),TravellerTripDestinationPurpose(i,j),TravellerTripOriginMAZ(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripDestinationTAZ(i,j),TripDriverPassengerFlag(i,j),TravellerTripJointFlag(i,j),TravellerTripParkRideFlag(i,j),TravellerTripStartTimeInterval(i,j),TripStartTime(i,j),ABMTripIndex(i,j),TravellerTripDriver(i,j),TravellerTripDestinationParkingTaz(i,j)
!                if(TravellerTripMode(i,j).gt.0) NumCarTrip=NumCarTrip+1
!            enddo
!            ActivityTime=0.0 !for the last trip it should be zero 
!            write(8,13) TravellerTripDestinationTAZ(i,TravellerNumTrips(i)),ActivityTime,TravellerTripMode(i,TravellerNumTrips(i)),TravellerTripOriginPurpose(i,TravellerNumTrips(i)),TravellerTripDestinationPurpose(i,TravellerNumTrips(i)),TravellerTripOriginMAZ(i,TravellerNumTrips(i)),TravellerTripOriginTAZ(i,TravellerNumTrips(i)),TravellerTripDestinationMAZ(i,TravellerNumTrips(i)),TravellerTripDestinationTAZ(i,TravellerNumTrips(i)),TripDriverPassengerFlag(i,TravellerNumTrips(i)),TravellerTripJointFlag(i,TravellerNumTrips(i)),TravellerTripParkRideFlag(i,TravellerNumTrips(i)),TravellerTripStartTimeInterval(i,TravellerNumTrips(i)),TripStartTime(i,TravellerNumTrips(i)),ABMTripIndex(i,TravellerNumTrips(i)),TravellerTripDriver(i,TravellerNumTrips(i)),TravellerTripDestinationParkingTaz(i,TravellerNumTrips(i))
!            if(TravellerTripMode(i,j).gt.0) NumCarTrip=NumCarTrip+1
!       enddo            
!   enddo
!   if (counter.ne.NumTraveller) then
!       write(3,*) 'Maximum Number of Valid Traveller is wrong, it is ', NumTraveller, 'while we have at least ', counter
!       pause
!   endif
!   close(5)
!   close(8)    
!-------------------------------------------------------------------
!After all trips are sorted, now we need to create the transit.dat which only record traveler with transit trip mode
allocate(TransitPassengerInEachTimeInterval(MaxNumTimeInterval,MaxNumTravellerInEachTimeInterval))
   TransitPassengerInEachTimeInterval(:,:)=0
allocate (NumTransitPassengerInTimeInterval(MaxNumTravellerInEachTimeInterval))
   NumTransitPassengerInTimeInterval(:)=0
do i=1,index 
    if(ValidTraveller(i).eq.1) then !go over all the valid traveller and store their time
        EndLoop=0
        j=1
        !Go over all the trips and find the start time of first transit trip
        do while(j.le.TravellerNumTrips(i))
            if (TravellerTripMode(i,j).ge.9.and.TravellerTripMode(i,j).le.12) then 
                if(EndLoop.eq.0) FirstTransitTrip(i)=j
                !FirstTransitPassengerTripStartTimeInterval(i)=TravellerTripStartTimeInterval(i,j)
                !TransitPassengerNumFirstTimeIntervalTrip(i)=TransitPassengerNumFirstTimeIntervalTrip(i)+1 !Calculating the number of transit trip in first time interval
                EndLoop=1
            endif
            j=j+1
        enddo
        if(FirstTransitTrip(i).ne.0) then
                NumTransitPassengerInTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)))=NumTransitPassengerInTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)))+1
                if (NumTransitPassengerInTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i))).eq.1) then !The first Transit Passsenger for that time interval and no sorting
                    TransitPassengerInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)),1)=i
                endif            
                j=1
                EndofLoop=0
                do while(j.lt.NumTransitPassengerInTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i))).and.EndofLoop.eq.0) !search for the proper place if it is not the first traveller for that time interval
                     TravellerIDforj=TransitPassengerInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)),j)
                     if(TripStartTime(TravellerIDforj,FirstTransitTrip(TravellerIDforj)).ge.TripStartTime(i,FirstTransitTrip(i))) then
                        do jj=NumTransitPassengerInTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)))-1,j,-1 !shift all the remaining
                            TransitPassengerInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)),jj+1)=TransitPassengerInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)),jj)
                        enddo
                        TransitPassengerInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)),j)=i !put the transit passenger at the proper place
                        EndofLoop=1
                    elseif (j.eq.NumTransitPassengerInTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)))-1) then
                        TransitPassengerInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstTransitTrip(i)),j+1)=i !The proper place is the last place
                    endif
                    j=j+1
                enddo
        endif
    endif
enddo

!---------------------------------------------------------------------------------
!Now write the transit.dat
counter=0 
   write(*,*)'Writing transit.dat ...'
   open(file='TimeInterval.dat',unit=5)
   if(TransitMazTazFlag.eq.0) then 
        write(18,18)
        !write(41,18)
   else
        write(18,21)
        !write(41,21)
   endif
   !write(2,18) 
   do tt=1,MaxNumTimeInterval
       read(5,*)  StartTimeofTimeInterval,LengthofTimeInterval
       !write(*,*) StartTimeofTimeInterval,LengthofTimeInterval
       !write(*,*) NumTransitPassengerInTimeInterval(1)
      
       !write(*,*) NumTransitPassengerInTimeInterval(tt)
       do k=1,NumTransitPassengerInTimeInterval(tt)
            i=TransitPassengerInEachTimeInterval(tt,k) !getting the traveller index
            counter=counter+1
            !TransitPassengerStartTime(i)=StartTimeofTimeInterval+(LengthofTimeInterval/NumTransitPassengerInTimeInterval(t))*(k-1) !unifrom distribution of travellers over the time interval
            temp1=1.0 !The precision of start time (minute), where 1, makes xx.00, 10, xx.x0, and 100, xx.xx (the latest is not acceptable in Dynasmart)
            !StartTime=(nint(TransitPassengerStartTime(i)*temp1))/temp1
            do j=1,TravellerNumTrips(i)   !go over all the trips   
                if (TravellerTripMode(i,j).ge.9.and.TravellerTripMode(i,j).le.12) then
                    TripStartTime(i,j)=nint(TripStartTime(i,j)*10.0)/10.0
                    !trip_travel_time=rand(0)*100
                    if(TransitMazTazFlag.eq.0) then
                        otranz=TravellerTripOriginMAZ(i,j)
                        dtranz=TravellerTripDestinationMAZ(i,j)
                        otap=TravellerTripOriginTransitMAZ(i,j)
                        dtap=TravellerTripDestinationTransitMAZ(i,j)
                    else
                        otranz=TravellerTripOriginTAZ(i,j)
                        dtranz=TravellerTripDestinationTAZ(i,j)
                        otap=TravellerTripOriginTransitTAZ(i,j)
                        dtap=TravellerTripDestinationTransitTAZ(i,j)
                    endif
                    
                    
                    if(TravellerTripParkRideFlag(i,j).eq.1)then
!                        write(18,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginTransitMAZ(i,j),ZoneToZoneTravelTime(alex1,alex2,alex3),0.0,0.0
                        write(18,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),otap,0,PNRaTime(i,j),0.0,0.0
                        !write(41,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),otap,0,PNRaTime(i,j),0.0,0.0

                    elseif(TravellerTripParkRideFlag(i,j).eq.2) then
                        !write(18,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginTransitMAZ(i,j),TravellerTripDestinationTransitMAZ(i,j),ZoneToZoneTravelTime(alex1,alex2,alex3),0.0
                        write(18,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,dtap,0.0,0.0,0.0
                        !write(41,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,dtap,0.0,0.0,0.0

                    else
                        !write(18,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginTransitMAZ(i,j),TravellerTripDestinationTransitMAZ(i,j),0.0,0.0,0.0
                         write(18,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,0,0.0,0.0,0.0
                         !write(41,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,0,0.0,0.0,0.0
                    endif
                    traveltimetemp=10.0
                    !write(2,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),traveltimetemp,traveltimetemp,traveltimetemp
                    TravellerTripTravelTime(i,j)=trip_travel_time
                endif
            enddo
       enddo            
   enddo 
close(5)

!----------------------------------------
!Dissaggregate the park and rid trip
write(*,*) "Dissaggregate the PNR trips"
SameOriginDestinationParkRide=0
!Disaggregate Park and Ride trip to a car trip and a transit trip
!ParkRide
error=1
do i=1,index
    otran_maz=0
    j=1
    do while(j.le.TravellerNumTrips(i))
        if(TravellerTripParkRideFlag(i,j).gt.0) then 
            !read(Line,*,iostat=error) person_id,person_index,trip_index,tripstarttim,origmaz,destmaz,trip_park_ride_flag,otran_maz,dtran_maz,a_time,e_time,tr_time 
            TravellerParkRideNum(i)=TravellerParkRideNum(i)+1
            !j=j+TravellerParkRideNum(i)-1  !Need to reexamine
            TravellerNumTrips(i)=TravellerNumTrips(i)+1
            do jj=TravellerNumTrips(i),j+1,-1      !Move trip jj to jj+1          
                TravellerTripMode(i,jj)=TravellerTripMode(i,jj-1)
                TravellerTripOriginMAZ(i,jj)=TravellerTripOriginMAZ(i,jj-1)
                TravellerTripOriginTAZ(i,jj)=TravellerTripOriginTAZ(i,jj-1)
                !TravellerTripOriginMappedTaz(i,jj)=TAZMap(TravellerTripOriginTAZ(i,jj))
                TravellerTripDestinationMAZ(i,jj)=TravellerTripDestinationMAZ(i,jj-1)
                TravellerTripDestinationTAZ(i,jj)=TravellerTripDestinationTAZ(i,jj-1)
                !TravellerTripDestinationMappedTaz(i,jj)=TAZMap(TravellerTripDestinationTAZ(i,jj))
                TravellerTripOriginPurpose(i,jj)=TravellerTripOriginPurpose(i,jj-1)
                TravellerTripDestinationPurpose(i,jj)= TravellerTripDestinationPurpose(i,jj-1)
                TravellerTripStartTimeInterval(i,jj)=TravellerTripStartTimeInterval(i,jj-1)
                TripDriverPassengerFlag(i,jj)=TripDriverPassengerFlag(i,jj-1)
                TravellerTripParkRideFlag(i,jj)=TravellerTripParkRideFlag(i,jj-1)
                TripStartTime(i,jj)=TripStartTime(i,jj-1)
                
                !TransitMaz
                TravellerTripOriginTransitMaz(i,jj)=TravellerTripOriginTransitMaz(i,jj-1)
                TravellerTripDestinationTransitMaz(i,jj)=TravellerTripDestinationTransitMaz(i,jj-1)
                !TransitTaz
                TravellerTripOriginTransitTaz(i,jj)=TravellerTripOriginTransitTaz(i,jj-1)
                TravellerTripDestinationTransitTaz(i,jj)=TravellerTripDestinationTransitTaz(i,jj-1)
                !Joint
                TravellerTripDriver(i,jj)=TravellerTripDriver(i,jj-1)
                TravellerTripTourID(i,jj)=TravellerTripTourID(i,jj-1)
                TravellerTripJointFlag(i,jj)=TravellerTripJointFlag(i,jj-1)                
                ABMTripIndex(i,jj)=ABMTripIndex(i,jj-1)
                TravellerTripOriginParkingTaz(i,jj)=TravellerTripOriginParkingTaz(i,jj-1)
                !PathAssignment
                PNRaTime(i,jj)=PNRaTime(i,jj-1)
            enddo
            if(TravellerTripParkRideFlag(i,j).gt.2) then 
                write(3,*) "ParkRideFlag" ,i,j
                pause 
            endif
            if (TravellerTripParkRideFlag(i,j).eq.1) then
                !Change information of the first leg (driving) 
                TravellerTripMode(i,j)=1
                !TravellerTripDestinationMAZ(i,j)=orig_maz
                if (TransitMazTazFlag.eq.0) then !MAZ based                    
                    TravellerTripDestinationMAZ(i,j)=TravellerTripOriginTransitMaz(i,j)
                    TravellerTripDestinationTAZ(i,j)=MaztoTazMap(TravellerTripDestinationMAZ(i,j))
                else !Taz based                    
                    TravellerTripDestinationTAZ(i,j)=TravellerTripOriginTransitTaz(i,j)
                    TravellerTripDestinationMAZ(i,j)=TaztoMazMap(TravellerTripDestinationTAZ(i,j))
                endif
                !TravellerTripDestinationTAZ(i,j)=TazToTransitTaz(TravellerTripOriginTAZ(i,j))
                
               ! TravellerTripDestinationMappedTaz(i,j)=TAZMap(TravellerTripDestinationTAZ(i,j))
                !TravellerTripDestinationTAZ(i,j)=transfer_taz
                TravellerTripDestinationPurpose(i,j)=TravellerTripOriginPurpose(i,j)
                if (TravellerTripJointFlag(i,j).eq.0) TravellerTripDriver(i,j)=Traveller_ID(i)
                !TravellerTripParkRideFlag(i,j)=trip_park_ride_flag 
                ABMTripIndex(i,j)=ABMTripIndex(i,j)
                TravellerTripOriginParkingTaz(i,j)=TravellerTripOriginParkingTaz(i,j)
                if (TravellerTripDestinationTAZ(i,j).eq.TravellerTripOriginTAZ(i,j)) SameOriginDestinationParkRide=SameOriginDestinationParkRide+1
                if(TravellerTripJointFlag(i,j).ne.1) then 
                    TripDriverPassengerFlag(i,j)=1000
                else
                    if (Traveller_ID(i).eq.TravellerTripDriver(i,j)) then 
                        TripDriverPassengerFlag(i,j)=1000
                    else
                        TripDriverPassengerFlag(i,j)=-1    
                    endif
                endif   
                !Second Part of park and ride                 
                TravellerTripMode(i,j+1)=TravellerTripMode(i,j+1)-2
               TravellerTripOriginMAZ(i,j+1)=TravellerTripDestinationMAZ(i,j)         
               TravellerTripOriginTAZ(i,j+1)=TravellerTripDestinationTAZ(i,j)  
!                TravellerTripOriginMAZ(i,j+1)=otran_maz
!                TravellerTripOriginTAZ(i,j+1)=maztotazmap(otran_maz)
                !TravellerTripOriginMappedTaz(i,j+1)=TAZMap(TravellerTripOriginTAZ(i,j+1))
                TripStartTime(i,j+1)=TripStartTime(i,j)+PNRaTime(i,j)
                
                if(j+2.le.TravellerNumTrips(i)) then
                    if(TripStartTime(i,j+1).gt.TripStartTime(i,j+2)) then
                        TripStartTime(i,j+1)=TripStartTime(i,j)+0.5*(TripStartTime(i,j+2)-TripStartTime(i,j))
                    endif
                elseif(TripStartTime(i,j+1).gt.1440) then
                    TripStartTime(i,j+1)=1440
                endif
                
                TripStartTime(i,j+1)=nint(TripStartTime(i,j+1)*10.0)/10.0
                if (TripStartTime(i,j+1).lt.VectorStartTimeofTimeInterval(2)) then 
                    TravellerTripStartTimeInterval(i,j+1)=1
                else if (TripStartTime(i,j+1).gt.VectorStartTimeofTimeInterval(MaxNumTimeInterval)) then 
                    TravellerTripStartTimeInterval(i,j+1)=MaxNumTimeInterval
                else 
                    TravellerTripStartTimeInterval(i,j+1)=ceiling((TripStartTime(i,j+1)-120)/30.0)+1
                endif
                
                TravellerTripParkRideFlag(i,j+1)=TravellerTripParkRideFlag(i,j)
                TripDriverPassengerFlag(i,j+1)=-1
                
                ABMTripIndex(i,j+1)=ABMTripIndex(i,j)
                TravellerTripOriginParkingTaz(i,j+1)=TravellerTripOriginParkingTaz(i,j)
               ! if(person_id.eq.701915) write(*,*) TravellerTripMode(i,:)
               !trip_travel_time=e_time+tr_time
               !TripTravelTime(i,j+1)=trip_travel_time
            endif
            if (TravellerTripParkRideFlag(i,j).eq.2) then
                 !trip_travel_time=a_time+tr_time
                TravellerTripMode(i,j)=TravellerTripMode(i,j)-2
                !TravellerTripDestinationMAZ(i,j)=dtran_maz
                if (TransitMazTazFlag.eq.0) then !MAZ based
                    TravellerTripDestinationMAZ(i,j)=TravellerTripDestinationTransitMAZ(i,j)
                    TravellerTripDestinationTAZ(i,j)=MaztoTazMap(TravellerTripDestinationTransitMAZ(i,j)) 
                   
                else !Taz based
                    TravellerTripDestinationTAZ(i,j)=TravellerTripDestinationTransitTAZ(i,j)
                    TravellerTripDestinationMAZ(i,j)=TaztoMazMap(TravellerTripDestinationTransitTAZ(i,j))
                endif
                
                !TravellerTripDestinationMAZ(i,j)=dest_maz
                !TravellerTripDestinationTAZ(i,j)=TazToTransitTaz(TravellerTripDestinationTAZ(i,j))    
                  
                !TravellerTripDestinationMappedTaz(i,j)=TAZMap(TravellerTripDestinationTAZ(i,j))
                !TripTravelTime(i,j)=trip_travel_time          
                TripDriverPassengerFlag(i,j)=-1
                !TravellerTripParkRideFlag(i,j)=
                !ABMTripIndex(i,j)=ABMTripIndex(i,j)
                TravellerTripOriginParkingTaz(i,j)=TravellerTripOriginParkingTaz(i,j)
                !Second part of park and ride
                TravellerTripMode(i,j+1)=1
                TravellerTripOriginMAZ(i,j+1)=TravellerTripDestinationMAZ(i,j)
                TravellerTripOriginTAZ(i,j+1)=TravellerTripDestinationTAZ(i,j)
                !TravellerTripOriginMappedTaz(i,j+1)=TAZMap(TravellerTripOriginTAZ(i,j+1))
                
                TravellerTripOriginPurpose(i,j+1)=TravellerTripDestinationPurpose(i,j+1)
                ABMTripIndex(i,j+1)=ABMTripIndex(i,j)
                TravellerTripOriginParkingTaz(i,j+1)=TravellerTripOriginParkingTaz(i,j)
                !trip_travel_time=a_time+tr_time
!                if(j+2.le.TravellerNumTrips(i)) then
!                    if(TripStartTime(i,j)+trip_travel_time.lt.TripStartTime(i,j+2)) then
!                        TripStartTime(i,j+1)=TripStartTime(i,j)+trip_travel_time
!                    else
!                        TripStartTime(i,j+1)=TripStartTime(i,j)
!                    endif
!                else
!                    TripStartTime(i,j+1)=TripStartTime(i,j)+trip_travel_time
!                endif
                TripStartTime(i,j+1)=TripStartTime(i,j)+PNRaTime(i,j)
                if(j+2.le.TravellerNumTrips(i)) then
                    if(TripStartTime(i,j+1).gt.TripStartTime(i,j+2)) then
                        TripStartTime(i,j+1)=TripStartTime(i,j)+0.5*(TripStartTime(i,j+2)-TripStartTime(i,j))
                    endif
                elseif(TripStartTime(i,j+1).gt.1440) then
                    TripStartTime(i,j+1)=1440
                endif
                TripStartTime(i,j+1)=nint(TripStartTime(i,j+1)*10.0)/10.0
                
                if (TripStartTime(i,j+1).lt.VectorStartTimeofTimeInterval(2)) then 
                    TravellerTripStartTimeInterval(i,j+1)=1
                else if (TripStartTime(i,j+1).gt.VectorStartTimeofTimeInterval(MaxNumTimeInterval)) then 
                    TravellerTripStartTimeInterval(i,j+1)=MaxNumTimeInterval
                else 
                    TravellerTripStartTimeInterval(i,j+1)=ceiling((TripStartTime(i,j+1)-120)/30.0)+1
                endif
                
                if (TravellerTripDestinationTAZ(i,j+1).eq.TravellerTripOriginTAZ(i,j+1)) SameOriginDestinationParkRide=SameOriginDestinationParkRide+1
                if(TravellerTripJointFlag(i,j+1).ne.1) then 
                    TripDriverPassengerFlag(i,j+1)=1000
                else
                    if (Traveller_ID(i).eq.TravellerTripDriver(i,j+1)) then 
                        TripDriverPassengerFlag(i,j+1)=1000
                    else
                        TripDriverPassengerFlag(i,j+1)=-1    
                    endif
                endif
                if (TravellerTripJointFlag(i,j+1).eq.0) TravellerTripDriver(i,j+1)=Traveller_ID(i)
                
                TravellerTripParkRideFlag(i,j+1)=TravellerTripParkRideFlag(i,j)
            endif
            j=j+2                   
        else
            j=j+1
            !TripTravelTime(i,j)=a_time+e_time+tr_time
        endif 
        
    enddo
enddo
close (18)
!-------------------------------------------------------------------       	    
!Update the validcar
do i=1,index
    do j=1,MaxNumTrips
        if(TripDriverPassengerFlag(i,j).gt.0) ValidCar(i)=1
    enddo
enddo


!Generate Extra car for inconsistent car trips
write(*,*) "Generate Extra Cars..."
   do i=1,index
        !Initial the parking counter to check the trips with different parking taz and destination taz
        parkingcounter=0
        !Check the first trip purpose (assuming everyone will start their daily chain of trips from Home)
        if (TravellerTripOriginPurpose(i,1).ne.Home) then
            write(3,*) 'The first trip is not started from home for Person_ID ', Traveller_ID(i),i
            ValidTraveller(i)=0             
        endif
        LatestCarTrip=0
        ! In this loop two objectives will be acheived, the consistency of the trips will be check and for the car trips number of first time interval trips will be calculated to be used at thevehicle generation time (simulation minute), also car trip consistency will be checked
        do j=1,TravellerNumTrips(i)-1
            !Car
            !if (TravellerTripMode(i,j).gt.0.and.TravellerTripMode(i,j).lt.7) then car trip
            if(TripDriverPassengerFlag(i,j).gt.0) then 
                if (FirstCarTrip(i,1).eq.0) FirstCarTrip(i,1)=j
                !if (TravellerTripStartTimeInterval(i,j).eq.TravellerTripStartTimeInterval(i,FirstCarTrip(i))) CarNumFirstTimeIntervalTrip(i)=CarNumFirstTimeIntervalTrip(i)+1 !calculate number of trips at the first time interval and also saving the first time interval of car trip
                if (LatestCarTrip.gt.0) then !no check for the first car trip
                    !Checking MAZ and for car trip, note that here we assume this as a requirment, which we need to change in future
                    if(TravellerTripDestinationMAZ(i,LatestCarTrip).ne.TravellerTripOriginMAZ(i,j)) then !if the origin of current trip and latest car trip do not match
                        ValidCar(i)=ValidCar(i)+1
                        FirstCarTrip(i,ValidCar(i))=j
                        !Checking purpose
!                        if(SuperZoneMap(TravellerTripOriginTAZ(i,j),2).eq.SuperZoneMap(TravellerTripDestinationTAZ(i,j),2).and.TravellerTripOriginTAZ(i,j).ne.TravellerTripDestinationTAZ(i,j)) then 
!                            alex6=alex6+1
!                            !write(26,*)i,Traveller_ID(i),j
!                        endif
                    elseif(SuperZoneMap(TravellerTripOriginTAZ(i,j),2).eq.SuperZoneMap(TravellerTripDestinationTAZ(i,j),2).and.TravellerTripOriginTAZ(i,j).ne.TravellerTripDestinationTAZ(i,j)) then 
                        ValidCar(i)=ValidCar(i)+1
                        FirstCarTrip(i,ValidCar(i))=j
                        IntraSuperZoneFlag(i,ValidCar(i))=1
                        !write(26,*) i,Traveller_ID(i),j
                    endif                                        
                endif
                LatestCarTrip=j       
            endif
            !Trip consistency check for all the trips (including the last trip)
            if (((TravellerTripDestinationPurpose(i,j).ne.TravellerTripOriginPurpose(i,j+1))).or.(TravellerTripDestinationMAZ(i,j).ne.TravellerTripOriginMAZ(i,j+1))) then
                if (((TravellerTripDestinationPurpose(i,j).eq.work1.and.TravellerTripOriginPurpose(i,j+1).eq.work2).or.(TravellerTripDestinationPurpose(i,j).eq.work2.and.TravellerTripOriginPurpose(i,j+1).eq.work1)).and.(TravellerTripDestinationMAZ(i,j).eq.TravellerTripOriginMAZ(i,j+1))) then
                    WorkCounter=WorkCounter+1
                else
                    write(3,*) 'The trips order is not valid for person_id ', Traveller_ID(i),' and trip ',j
                    ValidTraveller(i)=0
                endif 
            endif      
        enddo
        !This part is to check the number of different parking taz and destination taz inconsistent
!        if(TripDriverPassengerFlag(i,j).lt.1.and.TravellerTripDestinationParkingTaz(i,j).gt.0) then
!            if(TravellerTripDestinationMAZ(i,j).ne.TravellerTripDestinationParkingTaz(i,j)) write(3,*) Traveller_ID(i),'trip',j,'inconsistent type 3' 
!        endif
        !Check the last trip purpose which requires the daily chain of trips to be ended at Home
        if (TravellerTripDestinationPurpose(i,j).ne.Home) then
            write(3,*) 'The last trip is not ended at home for person_id ', Traveller_ID(i),i
            ValidTraveller(i)=0
        endif
        !Car
        !check the last trip for the car trip consistency        
        !if (TravellerTripMode(i,j).gt.0.and.TravellerTripMode(i,j).lt.7) then
        if(TripDriverPassengerFlag(i,j).gt.0) then
            if (FirstCarTrip(i,1).eq.0) FirstCarTrip(i,1)=j
           ! if (TravellerTripStartTimeInterval(i,j).eq.TravellerTripStartTimeInterval(i,FirstCarTrip(i))) CarNumFirstTimeIntervalTrip(i)=CarNumFirstTimeIntervalTrip(i)+1
            if (LatestCarTrip.gt.0) then                                        
                 !Checking MAZ and Purpose Consistency
                if (TravellerTripDestinationMAZ(i,LatestCarTrip).ne.TravellerTripOriginMAZ(i,j)). then
                    !write(9,*) 'The car trips order is not valid for car',Traveller_ID(i),'and trip',j
                    ValidCar(i)=ValidCar(i)+1
                    FirstCarTrip(i,ValidCar(i))=j
                    if(SuperZoneMap(TravellerTripOriginTAZ(i,j),2).eq.SuperZoneMap(TravellerTripDestinationTAZ(i,j),2).and.TravellerTripOriginTAZ(i,j).ne.TravellerTripDestinationTAZ(i,j)) then
                        alex6=alex6+1
                        !write(26,*) i, Traveller_ID(i),j
                    endif
                !Checking if the trip is intrasuperzonal trip
                elseif(SuperZoneMap(TravellerTripOriginTAZ(i,j),2).eq.SuperZoneMap(TravellerTripDestinationTAZ(i,j),2).and.TravellerTripOriginTAZ(i,j).ne.TravellerTripDestinationTAZ(i,j)) then 
                        ValidCar(i)=ValidCar(i)+1
                        FirstCarTrip(i,ValidCar(i))=j
                        IntraSuperZoneFlag(i,ValidCar(i))=1
                        !write(26,*) i,Traveller_ID(i),j
                endif                                        
            endif
            LatestCarTrip=j
        endif  
!        if(ValidCar(i).eq.18) then
!            do z=1,18
!                write(*,*) i,Traveller_ID(i),SuperZoneMap(TravellerTripDestinationTAZ(i,z),2),SuperZoneMap(TravellerTripOriginTAZ(i,z),2)
!            enddo
!        endif
   enddo 
   close(2) 
   close(9)



!___________________________________________________________________
!Generate the new vehicle error
WorkCounter=0
NumTraveller=0
NumCar=0           
do i=1,index
    !Counting the number of valid traveler and number of car
    if (ValidTraveller(i).eq.1) then
            !Car
           !Now counting the number of valid car
            do k=1,ValidCar(i)
                if(validcar(i).lt.0) write(*,*) validcar(i),i
                if(FirstCarTrip(i,k).gt.0) then
                    NumCar=NumCar+1
                    NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))=NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))+1
                    if (MaxNumCarInEachTimeInterval.lt.NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))) MaxNumCarInEachTimeInterval=NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))
                else
                    write(*,*) "numofcar"
                endif
           enddo
    endif
enddo
NumCar=NumCar

!-------------------------------------------------------------------       	    
!Assign each traveller (car) to a time interval to start its trip
!Also sort all the assigned travellers to each time interval based on their number of trips in that time interval (TravellerNumFirstTimeIntervalTrip and CarNumFirstTimeIntervalTrip)
!Travellers (cars) with higher number of trips in each time interval (their first time interval) should be generated earlier 
!TravellerInEachTimeInterval (CarInEachTimeInterval) will include the index of travellers (Cars) at each time interval sorted based on the number of trips at that interval. This can be allocated now as MaxNumTravellerInEachTimeInterval is updated in the previous section 
   !6-17 transitpassenger 
!   allocate (TravellerInEachTimeInterval(MaxNumTimeInterval,MaxNumTravellerInEachTimeInterval))
!   TravellerInEachTimeInterval(:,:)=0
!   NumTravellerInTimeInterval(:)=0
   !Car
   write(*,*)"Sort Cars..."
   allocate (CarInEachTimeInterval(MaxNumTimeInterval,MaxNumCarInEachTimeInterval))
   CarInEachTimeInterval(:,:)=0
   NumCarInTimeInterval(:)=0
   
   allocate(CarIndexInEachTimeInterval(MaxNumTimeInterval,MaxNumCarInEachTimeInterval))
   CarIndexInEachTimeInterval(:,:)=0
   
   !Go over all the validcars and assign each of them to one time interval 
   do i=1,index
        if (ValidTraveller(i).eq.1) then
            do k=1,ValidCar(i)
                 if(validcar(i).lt.0) write(*,*) validcar(i),i !can be deleted
                NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))=NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))+1
                if (NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k))).eq.1) then !The first Car for that time interval and no sorting
                    CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),1)=i
                    CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),1)=k
                endif  
                
!                j=1
!                EndofLoop=0
!                do while(j.lt.NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k))).and.EndofLoop.eq.0) !search for the proper place if it is not the first traveller for that time interval
!                    TravellerIDforj=CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)
!                    VehicleIndexforj=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)
!                    if (TripStartTime(TravellerIDforj,FirstCarTrip(TravellerIDforj,VehicleIndexforj)).gt.TripStartTime(i,FirstCarTrip(i,k))) then !proper place
!                        do jj=NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))-1,j,-1 !shift all the remaining
!                            CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj+1)=CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj)
!                            CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj+1)=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj)
!                        
!                        enddo
!                        CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)=i !put the car at the proper place
!                        CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)=k  !Write which car it is
!                        EndofLoop=1
!                    elseif (j.eq.NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))-1) then
!                        CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j+1)=i !The proper place is the last place
!                        CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j+1)=k
!                        
!                    endif
!                    
!                    j=j+1
!                enddo   
                jleft=1
                jright=NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))-1
                j=ceiling(jright*1.0/2.0)
                EndofLoop=0
                
                do while(EndofLoop.eq.0.and.NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k))).gt.1)
                    TravellerIDforj=CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)
                    VehicleIndexforj=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)
                    if(abs(jleft-jright).gt.1) then
                        if(TripStartTime(TravellerIDforj,FirstCarTrip(TravellerIDforj,VehicleIndexforj)).gt.TripStartTime(i,FirstCarTrip(i,k))) then
                            jright=j
                            j=ceiling((jleft+jright)/2.0)
                        elseif(TripStartTime(TravellerIDforj,FirstCarTrip(TravellerIDforj,VehicleIndexforj)).lt.TripStartTime(i,FirstCarTrip(i,k)))then
                            jleft=j
                            j=ceiling((jleft+jright)/2.0)
                        else
                            if(CarInEachTimeInterval(TravellerTripStartTimeInterval(i,1),j).gt.i) then 
                                jright=j
                                j=ceiling((jleft+jright)/2.0)
                            else
                                jleft=j
                                j=ceiling((jleft+jright)/2.0)
                            endif
                        endif
                    else    !Found the proper place
                        TravellerIDforjleft=CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jleft)
                        VehicleIndexforjleft=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jleft)
                        TravellerIDforjright=CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jright)
                        VehicleIndexforjright=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jright)
                        if(VehicleIndexforjleft.eq.0) then 
                            write(*,*) 'left',jleft,i,k 
                        elseif(VehicleIndexforjright.eq.0) then
                            write(*,*) 'right',jright,i,k
                        endif
                        if(TripStartTime(TravellerIDforjleft,FirstCarTrip(TravellerIDforjleft,VehicleIndexforjleft)).gt.TripStartTime(i,FirstCarTrip(i,k))) then
                            j=jleft
                        elseif(TripStartTime(TravellerIDforjright,FirstCarTrip(TravellerIDforjright,VehicleIndexforjright)).lt.TripStartTime(i,FirstCarTrip(i,k)))then
                            j=jright+1
                        elseif(TripStartTime(TravellerIDforjleft,FirstCarTrip(TravellerIDforjleft,VehicleIndexforjleft)).lt.TripStartTime(i,FirstCarTrip(i,k)).and.TripStartTime(TravellerIDforjright,FirstCarTrip(TravellerIDforjright,VehicleIndexforjright)).gt.TripStartTime(i,FirstCarTrip(i,k))) then
                            j=jleft+1
                        else
                            if(TripStartTime(TravellerIDforjleft,FirstCarTrip(TravellerIDforjleft,VehicleIndexforjleft)).eq.TripStartTime(i,FirstCarTrip(i,k))) then 
                                 if(CarInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jleft).gt.i) then 
                                    j=jleft
                                 else
                                    j=jleft+1
                                 endif
                            elseif(TripStartTime(TravellerIDforjright,FirstCarTrip(TravellerIDforjright,VehicleIndexforjright)).eq.TripStartTime(i,FirstCarTrip(i,k))) then
                                if(CarInEachTimeInterval(TravellerTripStartTimeInterval(i,1),jright).gt.i) then 
                                    j=jright
                                 else
                                    j=jright+1
                                 endif
                            endif
                        endif
                        
                        
                        do jj=NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)))-1,j,-1 !shift all the remaining
                            CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj+1)=CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj)
                            CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj+1)=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj)
                        enddo
                        CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)=i !The proper place is the last place
                        CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)=k
                        EndofLoop=1
                    endif
                enddo
                           
            enddo      
        endif
   enddo

!-------------------------------------------------------------------       	      
!READ network.dat for selecting generation links
    write(*,*)'Reading node info...'
    
	do i=1,noofnodes_old
        read(6,*) nodenum(i), izone(i)
	    idnum(nodenum(i)) = i
    enddo
    
    write(*,*)'Reading link info...'
    do i=1, noofarcs_old
        read(6,4)iu(i),id(i),MTbay(i),MTbayR(i),i3(i),nlanes(i),FlowModelNum(i),Vfadjust(i),SpeedLimit(i),mfrtp(i),sattp(i),link_iden(i), LGrade(i)
        iunod(i)=idnum(iu(i))
        idnod(i)=idnum(id(i))
        node_to_link(iunod(i),idnod(i))=i
        LinkWeight(i)=nlanes(i)*i3(i)         
	enddo
	close(6)
!-------------------------------------------------------------------       	      
!READ origin.dat for selecting generation links
     write(*,*)'Reading origin.dat...'
     do i=1,nzones_old
        read(7,*) zone_old, NoofGenLinksPerZone_old(i), IDGen              
        do j=1, NoofGenLinksPerZone_old(i)
            read(7,*) iu_new, id_new, LWTmp
            ZoneTotalOriginLaneLength(i)=ZoneTotalOriginLaneLength(i)+LinkWeight(node_to_link(idnum(iu_new),idnum(id_new)))
            ZoneGenerationLink(i,2*j-1)=iu_new
            ZoneGenerationLink(i,2*j)=id_new
        enddo
        read(7,*)            
    enddo
    close(7)      	      

!-------------------------------------------------------------------       	      
!Before writing the vehicle.dat we need to Calculate the maximum number of trips for vehicle.dat (input of Dynasmart)
!Note that here we are ignoring the interzonal trips and we are not including them, which we need to change in future
!Car  
   NonrepetitiveMaxNumCarTrips=0 !Initialization of variable which will be known after the following loops
   open(file='TimeInterval.dat',unit=5)
   do t=1,MaxNumTimeInterval
       read(5,*)  StartTimeofTimeInterval,LengthofTimeInterval
       do k=1,NumCarInTimeInterval(t)
            i=CarInEachTimeInterval(t,k) !getting the traveller index	
         !Calculating Number of trips (without interzonal and consistent zone and purpose)
            NonrepetitiveCarNumTrip=0 !initialization of number of trips for this car
            m=CarIndexInEachTimeInterval(t,k)
            j=FirstCarTrip(i,m)
            !Determine the last car trip of each car
            VehicleLastTrip=0
            if(IntraSuperZoneFlag(i,m).eq.1) then !If the car is an intrasuperzonal car trip
                VehicleLastTrip=FirstCarTrip(i,m)
            elseif(m.eq.ValidCar(i)) then !If the car is the last car
                VehicleLastTrip=TravellerNumTrips(i)
!            elseif (FirstCarTrip(i,m+1).eq.0) then ! if the car is the last car
!                VehicleLastTrip=TravellerNumTrips(i)
            else   !Else find the next non-intrasuperzonal car
                n=m+1
                do while(VehicleLastTrip.eq.0)
                    if(FirstCarTrip(i,n).gt.0.and.IntraSuperZoneFlag(i,n).eq.0) then
                        VehicleLastTrip=FirstCarTrip(i,n)-1
                    elseif (n.eq.ValidCar(i)) then
                        VehicleLastTrip=TravellerNumTrips(i)
                    endif
                    n=n+1
                enddo
            endif
            
            !do while (j.lt.TravellerNumTrips(i)) !go over all the trips except the last trip
            do while (j.lt.VehicleLastTrip) !Go over from all the trip between the kth cartrip and k+1th cartrip-1
                jj=0
                !if (TravellerTripMode(i,j).gt.0.and.TravellerTripMode(i,j).lt.7) then  !if the current trip (j) is a car trip, find the next car trip (jj)
                !if ((TripDriverPassengerFlag(i,j).gt.0.and.SuperZoneMap(TravellerTripOriginTAZ(i,j),2).ne.SuperZoneMap(TravellerTripDestinationTAZ(i,j),2)).or.j.eq.FirstCarTrip(i,1)) then !if the current trip (j) is a car trip, find the next car trip (jj)
                if (TripDriverPassengerFlag(i,j).gt.0) then !if the current trip (j) is a car trip, find the next car trip (jj)
                    jj=j+1
                    !do while ((TravellerTripMode(i,jj).gt.6).and.(jj.lt.TravellerNumTrips(i)))                   
                    !do while ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.lt.TravellerNumTrips(i)))  
                    !Find a trip which either is the last trip or a non-intrazonal and non-intrasuperzonal car trip.
                    do while ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.lt.VehicleLastTrip))
                        jj=jj+1
                    enddo

                    !if ((TravellerTripMode(i,jj).gt.6).and.(jj.eq.TravellerNumTrips(i))) then !if there is no other following car trip, just add one trip to NonrepetitiveCarNumTrip
                    !if ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.eq.TravellerNumTrips(i))) then !if there is no other following car trip, just add one trip to NonrepetitiveCarNumTrip                   
                    if ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.eq.VehicleLastTrip)) then !if there is no other following car trip, just add one trip to NonrepetitiveCarNumTrip                   
                         NonrepetitiveCarNumTrip=NonrepetitiveCarNumTrip+1
                         
                    elseif((jj.le.VehicleLastTrip).and.(TravellerTripDestinationTAZ(i,j).ne.TravellerTripDestinationTAZ(i,jj))) then !if there is another car trip in the following trips, check for the zone numbers and if they are not the same add one to NonrepetitiveCarNumTrip else do nothing, which we need to change in future (to add inter zonal trips)
                         if(SuperZoneMap(TravellerTripDestinationTAZ(i,j),2).ne.SuperZoneMap(TravellerTripDestinationTAZ(i,jj),2)) then !if the trip starts and ends within same superzone, skip the trip
                            NonrepetitiveCarNumTrip=NonrepetitiveCarNumTrip+1 
                         endif                    
                    endif                                                                    
                endif
                if (jj.lt.j+1) then !if the current trip (j) was not car trip go to the next trip (do nothing) till find a car trip
                    j=j+1
                else
                    j=jj !if the current trip (j) was a car trip skip the following trips to the next car trip (jj)
                endif
            enddo                     
            !if (TravellerTripMode(i,TravellerNumTrips(i)).gt.0.and.TravellerTripMode(i,TravellerNumTrips(i)).lt.7) then !check the last trip
            if (TripDriverPassengerFlag(i,VehicleLastTrip).gt.0) then !check the last trip
                 NonrepetitiveCarNumTrip=NonrepetitiveCarNumTrip+1  !update the NonrepetitiveMaxNumCarTrips                                              
            endif    
            if (NonrepetitiveMaxNumCarTrips.lt.NonrepetitiveCarNumTrip) NonrepetitiveMaxNumCarTrips=NonrepetitiveCarNumTrip !update the NonrepetitiveMaxNumCarTrips 
    enddo
  enddo
  close(5)  
               
!--------------------------------------------------------------------      
!Outputing vehicle.dat as the Dynasmart input just for the car trips
!Note that selecting a generation link for a vehicle is necessary
!Each valid Car is assigned to one time interval (the time interval of the first car trip for that traveller)
!In this approach the vehicles will be reported based on an increasing order of start time
!Currently activity durations are generated randomly, while they should be as the input from ABM, which we need to change in future
!Currently a valid car traveller is a person who have consistency of zone and purpose trips in their car trip lists, also we are skipping the interzonal trips, which we need to change in future

!Car
   error=1
   read(22,*) ExNumCar,ex1
   
   
   write(*,*)'Writing vehicle.dat ...'
   open(file='TimeInterval.dat',unit=5)
   counter=0    
   NumCar=NumCar+ExNumCar
   write(10,5) NumCar, NonrepetitiveMaxNumCarTrips
   !Fake Output_vehicleTT.dat
   !write(40,5) NumCar, NonrepetitiveMaxNumCarTrips
   write(10,19)    
   !write(40,19)
   
    if(ExNumCar.gt.0) then 
        read(22,*)
        read(22,*) excounter,exusec,exdsec,exstime,exusrcls,exvehtype,exioc,exonode,exintde,exinfo,exribf,excomp,exoz 
        read(22,*) exTAZMap,exactivitytime
        exstime=exstime+stimedifference 
    else 
        exstime=1550
    endif
    
    write(23,*) "vehicleindex, i, person_id"
    write(24,*) "vehicleindex, externalindex, dummy"
   NonrepetitiveMaxNumCarTrips=0
   do t=1,MaxNumTimeInterval
       read(5,*)  StartTimeofTimeInterval,LengthofTimeInterval
       do k=1,NumCarInTimeInterval(t)
!            if(counter.ge.142890) then 
!                write(3,*) "Calculate i"
!            endif            
            
            i=CarInEachTimeInterval(t,k) !getting the traveller index  
            
            counter=counter+1
!            if(counter.ge.142890) then 
!                write(3,*) counter,t,k,i,CarIndexInEachTimeInterval(t,k),"check1"
!            endif
            m=CarIndexInEachTimeInterval(t,k)
            !CarStartTime(i)=StartTimeofTimeInterval+(LengthofTimeInterval/NumCarInTimeInterval(t))*(k-1) !unifrom distribution of vehicles over the time interval
            !temp1=1.0 !The precision of start time (minute), where 1, makes xx.00, 10, xx.x0, and 100, xx.xx (the latest is not acceptable in Dynasmart) 
            !StartTime=(nint(CarStartTime(i)*temp1))/temp1
            StartTime=TripStartTime(i,FirstCarTrip(i,m))
	        !Calculating Number of trips (without interzonal and consistent zone and purpose)
            NonrepetitiveCarNumTrip=0 !initialization of number of trips for this car
            
!            if(counter.ge.142890) then 
!                write(3,*) "Get the first trip"
!            endif            
            j=FirstCarTrip(i,m)
            !Determine the last car trip of each car
            VehicleLastTrip=0
!            if(counter.ge.142890) then 
!                write(3,*) "Find the last trip of veh"
!            endif             
            if(IntraSuperZoneFlag(i,m).eq.1) then !If the car is an intrasuperzonal car trip
                VehicleLastTrip=FirstCarTrip(i,m)
            elseif(m.eq.ValidCar(i)) then !If the car is the last car
                VehicleLastTrip=TravellerNumTrips(i)
!            elseif (FirstCarTrip(i,m+1).eq.0) then ! if the car is the last car
!                VehicleLastTrip=TravellerNumTrips(i)
            else   !Else find the next non-intrasuperzonal car
                n=m+1
                do while(VehicleLastTrip.eq.0)
                    if(FirstCarTrip(i,n).gt.0.and.IntraSuperZoneFlag(i,n).eq.0) then
                        VehicleLastTrip=FirstCarTrip(i,n)-1
                    elseif (n.eq.ValidCar(i)) then
                        VehicleLastTrip=TravellerNumTrips(i)
                    endif
                    n=n+1
                enddo
            endif
            
            !if(i.eq.410043) write(*,*) i,VehicleLastTrip,FirstCarTrip(i,m)
                !Determine the vehicle typ
!            if(counter.ge.142890) then 
!                write(3,*) "Get the path index"
!            endif             
            pathindex=IntraSuperZonePathIndex(TravellerTripOriginTAZ(i,VehicleLastTrip),TravellerTripDestinationTAZ(i,VehicleLastTrip))
!            if(counter.ge.142890) then 
!                write(3,*) i,m,FirstCarTrip(i,m),pathindex,IntraSuperZoneFlag(i,m),VehicleLastTrip,"check2"
!                write(3,*) "General Statistic of vehicle"
!            endif
            
            if(IntraSuperZoneFlag(i,m).eq.1) then
                ivcltmp=1
                veh_pathnodenum=NumIntraSuperzoneZone(pathindex)
                iutmp=UpStreamLink(pathindex)
                idtmp=DownStreamLink(pathindex)
            else 
                ivcltmp=3
                veh_pathnodenum=1
                !Generation link selection
                temp1=rand(0)	    
	            temp2=int(ZoneTotalOriginLaneLength(TravellerTripOriginTAZ(i,FirstCarTrip(i,m)))*temp1)
	            TempZoneTotalOriginLaneLength=0
	            j=1
	            do while ((TempZoneTotalOriginLaneLength.le.temp2).and.(j.le.NoofGenLinksPerZone_old(TravellerTripOriginTAZ(i,FirstCarTrip(i,m)))))
                    iu_new=ZoneGenerationLink(TravellerTripOriginTAZ(i,FirstCarTrip(i,m)),2*j-1)
                    id_new=ZoneGenerationLink(TravellerTripOriginTAZ(i,FirstCarTrip(i,m)),2*j)
                    TempZoneTotalOriginLaneLength=TempZoneTotalOriginLaneLength+LinkWeight(node_to_link(idnum(iu_new),idnum(id_new)))
                    j=j+1
!                    if(counter.ge.142890) then 
!                        write(3,*) Temp1,Temp2,TempZoneTotalOriginLaneLength,j,"LinkGeneration"
!                    endif
                enddo
                iutmp=iu_new
	            idtmp=id_new
            endif
            !Write the information about external zone vehile 
!            if(counter.ge.142890) then                 
!                write(3,*) "Check External Vehicles"
!            endif	        	        
	        do while(exstime.lt.StartTime.and.error.ne.-1.and.ExNumCar.ne.0) ! If the starttime of the external vehile is smaller than current internal vehicle 
	            !Then write external vehicle infromation first
	            write(10,11) counter,exusec,exdsec,exstime,exusrcls,exvehtype,exioc,1,exintde,exinfo,exribf,excomp,exoz,average_value_of_time,1
                !write(40,11) counter,exusec,exdsec,exstime,exusrcls,exvehtype,exioc,1,exintde,exinfo,exribf,excomp,exoz,average_value_of_time,1
                
                trip_travel_time=rand(0)*100
                write(10,7) exTAZMap,exactivitytime !,trip_travel_time
                !write(40,7) exTAZMap,exactivitytime,trip_travel_time

	            !write the mapping
	            write(23,*) counter,0,0
	            write(23,*) 0
	            write(24,*) counter,excounter
	            !Empty path for External
	            write(16,*) 
	            counter=counter+1 !update the counter
	            !read the next vehicle
	            read(22,*,iostat=error) excounter,exusec,exdsec,exstime,exusrcls,exvehtype,exioc,exonode,exintde,exinfo,exribf,excomp,exoz 
                read(22,*,iostat=error) exTAZMap,exactivitytime
                
                exstime=exstime+stimedifference
                !if(exstime.gt.1440.and.ExNumCar.ne.0) pause 
!	            if(counter.ge.142890) then 
!                    write(3,*) counter,excounter,exstime,"external"
!                endif
	        enddo
!            
!            if(counter.ge.142890) then                 
!                write(3,*) "Write path.dat"
!            endif            
            
            !write the path.dat
            if(IntraSuperZoneFlag(i,m).gt.0) then 
                write(16,20) PathBank(pathindex,1:NumIntraSuperzoneZone(pathindex)) 
                veh_pathnodenum=NumIntraSuperzoneZone(pathindex)
            else
                write(16,*)
            endif
            j=FirstCarTrip(i,m)

!            if(counter.ge.142890) then                 
!                write(3,*) "Calculate the number of car trips"
!            endif            
            !do while (j.lt.TravellerNumTrips(i)) !go over all the trips except the last trip            
            do while (j.lt.VehicleLastTrip) !Go over from all the trip between the kth cartrip and k+1th cartrip-1
                jj=0
                !if (TravellerTripMode(i,j).gt.0.and.TravellerTripMode(i,j).lt.7) then  !if the current trip (j) is a car trip, find the next car trip (jj)
                !if ((TripDriverPassengerFlag(i,j).gt.0.and.SuperZoneMap(TravellerTripOriginTAZ(i,j),2).ne.SuperZoneMap(TravellerTripDestinationTAZ(i,j),2)).or.j.eq.FirstCarTrip(i,1)) then !if the current trip (j) is a car trip, find the next car trip (jj)
                if (TripDriverPassengerFlag(i,j).gt.0) then !if the current trip (j) is a car trip, find the next car trip (jj)
                    jj=j+1
                    !do while ((TravellerTripMode(i,jj).gt.6).and.(jj.lt.TravellerNumTrips(i)))                   
                    !do while ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.lt.TravellerNumTrips(i)))  
                    !Find a trip which either is the last trip or a non-intrazonal and non-intrasuperzonal car trip.
                    do while ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.lt.VehicleLastTrip))
                        jj=jj+1
                    enddo
                    !if ((TravellerTripMode(i,jj).gt.6).and.(jj.eq.TravellerNumTrips(i))) then !if there is no other following car trip, just add one trip to NonrepetitiveCarNumTrip
                    !if ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.eq.TravellerNumTrips(i))) then !if there is no other following car trip, just add one trip to NonrepetitiveCarNumTrip                   
                    if ((TripDriverPassengerFlag(i,jj).lt.1).and.(jj.eq.VehicleLastTrip)) then !if there is no other following car trip, just add one trip to NonrepetitiveCarNumTrip                   
                         NonrepetitiveCarNumTrip=NonrepetitiveCarNumTrip+1     
                    elseif((jj.le.VehicleLastTrip).and.(TravellerTripDestinationTAZ(i,j).ne.TravellerTripDestinationTAZ(i,jj))) then !if there is another car trip in the following trips, check for the zone numbers and if they are not the same add one to NonrepetitiveCarNumTrip else do nothing, which we need to change in future (to add inter zonal trips)
                         if(SuperZoneMap(TravellerTripDestinationTAZ(i,j),2).ne.SuperZoneMap(TravellerTripDestinationTAZ(i,jj),2)) then !if the trip starts and ends within same superzone, skip the trip
                            NonrepetitiveCarNumTrip=NonrepetitiveCarNumTrip+1    
                         endif                    
                    endif                                                                    
                endif
                if (jj.lt.j+1) then !if the current trip (j) was not car trip go to the next trip (do nothing) till find a car trip
                    j=j+1
                else
                    j=jj !if the current trip (j) was a car trip skip the following trips to the next car trip (jj)
                endif
            enddo      
            
            !if (TravellerTripMode(i,TravellerNumTrips(i)).gt.0.and.TravellerTripMode(i,TravellerNumTrips(i)).lt.7) then !check the last trip
            if (TripDriverPassengerFlag(i,VehicleLastTrip).gt.0) then !check the last trip
                 NonrepetitiveCarNumTrip=NonrepetitiveCarNumTrip+1  !update the NonrepetitiveMaxNumCarTrips                                              
            endif    
!            if(counter.ge.142890) then                 
!                write(3,*) "Write the vehicle first line"
!            endif             
!            if(NonrepetitiveMaxNumCarTrips.lt.NonrepetitiveCarNumTrip) NonrepetitiveMaxNumCarTrips=NonrepetitiveCarNumTrip
	        !writing the genral information of the vehicle
	        !write(10,6) counter,i,Traveller_ID(i),iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,NonrepetitiveCarNumTrip,infotmp,ribftmp,comptmp,TAZMap(TravellerTripOriginTAZ(i,FirstCarTrip(i)))
            write(10,11) counter,iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,NonrepetitiveCarNumTrip,infotmp,ribftmp,comptmp,TravellerTripOriginTAZ(i,FirstCarTrip(i,m)),ValueOfTime(Traveller_ID(i)),1
           ! write(40,11) counter,iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,NonrepetitiveCarNumTrip,infotmp,ribftmp,comptmp,TravellerTripOriginTAZ(i,FirstCarTrip(i,m)),ValueOfTime(Traveller_ID(i)),1
!            if(counter.ge.142890) then 
!                write(3,*) counter,NonrepetitiveCarNumTrip,veh_pathnodenum,"check3"
!            endif
            !write(18,*)Traveller_ID(i)
            !write the mapping
            write(23,*) counter,i,Traveller_ID(i)
            write(24,*) counter,0
            !writing the trips information   
            temp3=30.0 !constant used for the activity time generation
            j=FirstCarTrip(i,CarIndexInEachTimeInterval(t,k))
            CumulativeActivityTime=0.0
            foundcartrip=1
            !New Method for write the trip information revised in 2014-11-12
            realj=j
            temp1=rand(0)                    
            CumulativeActivityTime=TravellerActivityTime(i,j)
            if (CumulativeActivityTime.lt.0) CumulativeActivityTime=0
            NumCarTrips=0
            if(IntraSuperZoneFlag(i,CarIndexInEachTimeInterval(t,k)).gt.0) then 
                write(10,7) TravellerTripDestinationTAZ(i,j),0.0 !,trip_travel_time !,TripStartTime(i,j) !,TravellerTripMode(i,j)   
                !write(40,7) TravellerTripDestinationTAZ(i,j),0.0,trip_travel_time          
                !write(26,*)i,Traveller_ID(i),j        
                !Added for new method of reading output vehicle.dat 10-31-2014
                write(23,*) j  
                NumCarTrips=NumCarTrips+1
            else  !if the car is not an intrasuperzonal car
                do jj=FirstCarTrip(i,CarIndexInEachTimeInterval(t,k))+1,VehicleLastTrip
                    trip_travel_time=rand(0)*100
                    if(TripDriverPassengerFlag(i,jj).gt.0.and.(TravellerTripOriginTAZ(i,jj).ne.TravellerTripDestinationTAZ(i,jj)).and.(SuperZoneMap(TravellerTripOriginTAZ(i,jj),2).ne.SuperZoneMap(TravellerTripDestinationTAZ(i,jj),2))) then 
                        temp1=10.0
                        CumulativeActivityTime=(nint(CumulativeActivityTime*temp1))/temp1   
                        write(10,7) TravellerTripDestinationTAZ(i,realj),CumulativeActivityTime !,trip_travel_time !,TripStartTime(i,j) !,TravellerTripMode(i,j)   
                        !write(40,7) TravellerTripDestinationTAZ(i,realj),CumulativeActivityTime,trip_travel_time          
                        !write(26,*)i,Traveller_ID(i),realj        
                        !Added for new method of reading output vehicle.dat 10-31-2014
                        write(23,*) realj 
                        NumCarTrips=NumCarTrips+1
                        realj=jj
                        temp1=rand(0)
                        !CumulativeActivityTime=TripStartTime(i,jj+1)-TripStartTime(i,jj)-temp3*temp1
                        CumulativeActivityTime=TravellerActivityTime(i,jj)
                        if(CumulativeActivityTime.lt.0) CumulativeActivityTime=0.0
                    else
                        temp1=rand(0)                    
!                        ActivityTimeTemp=TripStartTime(i,jj+1)-TripStartTime(i,jj)-temp3*temp1
!                        if (ActivityTimeTemp.lt.0) ActivityTimeTemp=0
                        CumulativeActivityTime=TravellerActivityTime(i,jj)+CumulativeActivityTime
                    endif
                     
                enddo
                write(10,7) TravellerTripDestinationTAZ(i,realj),0.0 !,trip_travel_time !,TripStartTime(i,j) !,TravellerTripMode(i,j)   
                !write(40,7) TravellerTripDestinationTAZ(i,realj),0,trip_travel_time          
                !write(26,*)i,Traveller_ID(i),realj        
                !Added for new method of reading output vehicle.dat 10-31-2014
                write(23,*) realj 
                NumCarTrips=NumCarTrips+1
            endif
            if(NumCarTrips.eq.NonrepetitiveCarNumTrip) then 
                alexcheck=0
            else 
                alexcheck=1
            endif
       enddo 
       !-------------------------------------------------may be used in the future
       !Generate intrazonal vehicle. This will be used for generating PNRSkim.dat 

!       do m=1,MaxTAZ
!           counter=counter+1
!           !generate the link selection for the zone to zone vehicle
!           temp1=rand(0)	    
!	        temp2=int(ZoneTotalOriginLaneLength(m)*temp1)
!	        TempZoneTotalOriginLaneLength=0
!	        j=1
!	        do while (TempZoneTotalOriginLaneLength.le.temp2)
!                iu_new=ZoneGenerationLink(m,2*j-1)
!                id_new=ZoneGenerationLink(m,2*j)
!                TempZoneTotalOriginLaneLength=TempZoneTotalOriginLaneLength+LinkWeight(node_to_link(idnum(iu_new),idnum(id_new)))
!                j=j+1
!            enddo
!            iutmp=iu_new
!	        idtmp=id_new
!           write(10,11) counter,iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,1,infotmp,ribftmp,comptmp,m,average_value_of_time,1
!           write(12,11) counter,iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,1,infotmp,ribftmp,comptmp,m,average_value_of_time,1
!           write(10,7) m,ActivityTime !, trip_travel_time
!           write(12,7) m,ActivityTime,trip_travel_time
!           write(23,*) counter,0,0
!	       write(24,*) counter,0 
!	       write(16,*)
!	       write(23,*) 0
!       enddo           
   enddo
   
   if (counter.ne.NumCar) then
       write(3,*) 'Maximum Number of Valid Car Traveller is wrong, it is ', NumCar, ' while we have at least ', counter
       pause
   endif


 close(10)
1  format (3i10)
2  format (5i8)
3  format('      #   usec   dsec   stime usrcls vehtype ioc #ONode #IntDe info ribf    comp    OZ  valid_car  first_car_trip   first_transit_trip value_of_time')
4  format(2i7,2i5,i7,i3,i7,'  +',i1,i4,2i6,i3,'  +',i1)
5  format(2i12,'    # of vehicles in the file, Max # of stops')
6  format(5i8,f8.2,6i6,2f8.4,i5)
7  format(i12,f8.2,f7.1) 
8  format(3i7,f8.2,6i6,2f8.4,i5)
9  format(i12,f8.2)
10 format(i12,i8,i3,f8.2,2i6,i4) 
11  format(3i7,f12.2,6i6,2f8.4,i5,f8.4,i5)
12 format(3i10,f8.2,5i6,f8.4)
13 format(i12,f8.2,i6,2A16,2i5,2i10,4i5,f13.4,i10,i18,i10)
14 format('    person_id   person_index  trip_index  tripstarttim origmaz destmaz   trip_park_ride_flag')
15 format('  Trip Head: TAZMap_of_tripij  ActivityTime   tripmode   orig_purpose   dest_purpose  orig_maz  orig_taz   dest_maz  dest_taz  tripdriverpassengerflag    tripjointflag  tripparkrideflag   starttimeinterval   starttime  abm_trip_index   trip_driver_id   parking_taz')
16 format('  counter,i,Traveller_ID(i),StartTime,numtrips,MappedTAZ,valid_car,first_car_trip,first_transit_trip,value_of_time')
17 format(i12,i8,i3,f8.2,2i6,i4,2i10, 3f10.5) 
18 format('person_id	person_index	trip_index  	tripstarttime 	origmaz    	destmaz	trip_park_ride_flag	otran_maz	dtran_maz	a_time	e_time	tr_time')
19 format('counter  iutmp  idtmp  StartTime  ivcltmp  ivcl2tmp  ihovtmp  veh_pathnodenum  NonrepetitiveCarNumTrip  infotmp  ribftmp  comptmp  TAZMap  value_of_time  1')
20 format(100i7)
21 format('person_id	person_index	trip_index  	tripstarttime 	origtaz    	desttaz	trip_park_ride_flag	otran_taz	dtran_taz	a_time	e_time	tr_time')
400 format(i3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3)



END SUBROUTINE

END MODULE