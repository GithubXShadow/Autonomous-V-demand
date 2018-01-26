program Console3
!Component 3
use vector_mod
use iso_c_binding
use TC1Output
use skimtimeestimation
implicit none
!-------------------------------------------------   
! define the interface for the DLL methods called by this fortran program
interface
   subroutine hello_world () bind (C, name = "helloWorld")
     use iso_c_binding
     implicit none
   end subroutine hello_world
end interface
  
interface
   subroutine show_message (message) bind (C, name = "showMessage")
     use iso_c_binding
     implicit none
     character :: message
   end subroutine show_message
end interface
  
interface
   subroutine connect_java (javaClasspath, javaLibrarypath, parameterFileName) bind (C, name = "connectJavaScheduleAdjust")
     use iso_c_binding
     implicit none
     character(kind=c_char), intent(in) :: javaClasspath(*)
     character(kind=c_char), intent(in) :: javaLibrarypath(*)
     character(kind=c_char), intent(in) :: parameterFileName(*)
   end subroutine connect_java
end interface
  
interface
   subroutine disconnect_java () bind (C, name = "disconnectJavaScheduleAdjust")
     use iso_c_binding
     implicit none
   end subroutine disconnect_java
end interface
  
interface
   subroutine report_values (numHHMembers, persTypes, numTrips, orgActTypes, returnedValues) bind (C, name = "intializeJvm")
     use iso_c_binding
     implicit none
     integer, intent(in):: numHHMembers
     integer, intent(in), dimension(*):: persTypes
     integer, intent(in), dimension(*):: numTrips
     integer, intent(in), dimension(*):: orgActTypes
     integer, intent(out), dimension(*):: returnedValues
   end subroutine report_values
end interface

interface
   subroutine getAdjustedDeparts (numHHMembers, persTypes, numTrips, orgActTypes, dstActTypes, numHHJointTrips, &
   			numParticipants, participants, numJointTrips, jointTrips, numCompletedTrips, tripPlannedDeparture, &
   			tripPlannedTime, tripSimulatedTime, tripAdjustedDeparture) bind (C, name = "getAdjustedDeparts")
     use iso_c_binding
     implicit none
     integer, intent(in):: numHHMembers
     integer, intent(in), dimension(*):: persTypes
     integer, intent(in), dimension(*):: numTrips
     integer, intent(in), dimension(*):: orgActTypes
     integer, intent(in), dimension(*):: dstActTypes
     integer, intent(in):: numHHJointTrips
     integer, intent(in), dimension(*):: numParticipants
     integer, intent(in), dimension(*):: participants
     integer, intent(in), dimension(*):: numJointTrips
     integer, intent(in), dimension(*):: jointTrips
     integer, intent(in), dimension(*):: numCompletedTrips
     integer, intent(in), dimension(*):: tripPlannedDeparture
     integer, intent(in), dimension(*):: tripPlannedTime
     integer, intent(in), dimension(*):: tripSimulatedTime
     integer, intent(out), dimension(*):: tripAdjustedDeparture
   end subroutine getAdjustedDeparts
end interface

interface
   subroutine logSchedAdjustReport (numHHMembers, persTypes, numTrips, orgActTypes, dstActTypes, numHHJointTrips, &
   			numParticipants, participants, numJointTrips, jointTrips, numCompletedTrips, tripPlannedDeparture, &
   			tripPlannedTime, tripSimulatedTime, tripAdjustedDeparture) bind (C, name = "logSchedAdjustReport")
     use iso_c_binding
     implicit none
     integer, intent(in):: numHHMembers
     integer, intent(in), dimension(*):: persTypes
     integer, intent(in), dimension(*):: numTrips
     integer, intent(in), dimension(*):: orgActTypes
     integer, intent(in), dimension(*):: dstActTypes
     integer, intent(in):: numHHJointTrips
     integer, intent(in), dimension(*):: numParticipants
     integer, intent(in), dimension(*):: participants
     integer, intent(in), dimension(*):: numJointTrips
     integer, intent(in), dimension(*):: jointTrips
     integer, intent(in), dimension(*):: numCompletedTrips
     integer, intent(in), dimension(*):: tripPlannedDeparture
     integer, intent(in), dimension(*):: tripPlannedTime
     integer, intent(in), dimension(*):: tripSimulatedTime
     integer, intent(in), dimension(*):: tripAdjustedDeparture
   end subroutine logSchedAdjustReport
end interface

!-------------------------------------------------    
!Variables and Parameters Clarifications
    integer::hh_id, person_id, person_num, tour_id, stop_id, inbound, orig_maz, orig_walk_segment, dest_maz, dest_walk_segment, parking_taz, stop_period, trip_mode, tour_mode, board_tap, alight_tap, orig_taz, dest_taz
    integer::SpaceFinder, error, index,MaxPersonID,WorkCounter, EndofLoop,MaxNumTravellerInEachTimeInterval,MaxNumTimeInterval,MaxTAZ,NumTAZ,NumTraveller,NumCar,MaxNumTrips,RealMaxNumTrips,counter
    integer::noofnodes_old,noofarcs_old,nzones_old,zone_old,IDGen,iu_new,id_new,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,ndestmp,infotmp,ribftmp,comptmp,temp2,iutmp,idtmp,driver,jjj,exTAZMap
    integer::EndLoop,EndLoop1,EndLoop2,trip_driver_passenger,stimedifference,exusec,exdsec,exusrcls,exvehtype,exioc,exonode,exintde,exinfo,exoz,ExNumCar
    integer:: jj1,jj2,valid_car,start_time_interval,trip_joint_flag,vehicleindex,OriginalTAZ,orig_taz_temp,dest_taz_temp,abm_trip_index
    integer:: TransitTAZ,person_index,trip_index
    character(500)::Line,CheckLine
    character(500)::Text
    character(200)::TextFinal
    character(500)::TextFinal2
    character(30):: tour_category
    character(15):: tour_purpose, orig_purpose, dest_purpose,Home, work1,work2,OriginPurposeTemp,DestPurposeTemp
    character(15),allocatable::TravellerTripOriginPurpose(:,:),TravellerTripDestinationPurpose(:,:)
           
    integer,allocatable::Traveller_index(:),Traveller_ID(:),TravellerNumTrips(:),TravellerTripMode(:,:),TravellerTripOriginMAZ(:,:),TravellerTripOriginTAZ(:,:),TravellerTripDestinationMAZ(:,:),TravellerTripDestinationTAZ(:,:),TravellerTripStartTimeInterval(:,:),TripDriverPassengerFlag(:,:),FirstTransitPassengerTripStartTimeInterval(:)
    integer,allocatable::TAZMap(:),ValidTraveller(:),NumTravellerInTimeInterval(:),TravellerInEachTimeInterval(:,:),TravellerNumFirstTimeIntervalTrip(:),TransitPassengerNumFirstTimeIntervalTrip(:)
    integer,allocatable::nodenum(:),izone(:),idnum(:),iu(:),id(:),MTbay(:),MTbayR(:),i3(:),nlanes(:),FlowModelNum(:),Vfadjust(:),SpeedLimit(:),mfrtp(:),sattp(:),link_iden(:), LGrade(:),node_to_link(:,:),LinkWeight(:),idnod(:),iunod(:),NoofGenLinksPerZone_old(:),ZoneTotalOriginLaneLength(:),ZoneGenerationLink(:,:)
    integer,allocatable::NumTransitPassengerInTimeInterval(:),FirstTransitTrip(:),TravellerTripOriginMappedTaz(:,:),TravellerTripDestinationMappedTaz(:,:)
    integer,allocatable::ABMTripINdex(:,:),LatestCheckedTrip(:),TravellerParkRideNum(:),NonRepTazToTransitTaz(:),MaztoTazMap(:),TazToMazMap(:),NonRepMazToTransitMAZ(:)
    real,allocatable:: TravellerStartTime(:),TripStartTime(:,:),VectorLengthofTimeInterval(:),VectorStartTimeofTimeInterval(:),TransitPassengerStartTime(:)
    real:: StartTimeofTimeInterval, LengthofTimeInterval,StartTime,exribf,excomp,exstime,exactivitytime
    real:: LWTmp,ActivityTime,ActivityTimeTemp,temp1,temp3
    real,allocatable:: TripTravelTime(:,:),ZoneToZoneTravelTime(:,:,:),ABMTravelTime(:),park_ride_taz_travel_time(:),TravellerActivityTime(:,:),ABMTripStartTime(:),ABMTripActivityTime(:)
    integer::MaxNumJointTourID,Maz,Taz,TransitMazTazFlag,MaxNumTransitSkimTimeInterval,traveler_type
    integer::J,MaxNodeID,NodeA,NodeB,nodec,NumTurn,MaxPathLength,OrigTaz,DestTaz,NumPath,NumMaz,MaxNumTraveller,MaxHouseHoldID,MaxHHSize,NumNode,NumLink
    integer::MaxNumSuperZone,NumIteration,NumPairPNRMaz,NumRowsInSuperZoneFile,m,LengthofLastRow,NumTrips,first_car_trip,first_transit_trip
    integer::Traveller_Trip_Driver,SameOriginDestinationParkRide,jj,NonrepetitiveCarNumTrip,Dummy,jindex,Value1,NumLinkInterval,k,z,currentDYNASMARTtimeinterval
    integer::vv,tinterval,MaxNumMovement,PathIndex,UpstreamLinkNum,IterationID,TargetIterationNumber,traveller_trip_destination_parkingtaz
    integer,allocatable::countertoi(:),Traveller_counter(:),TravellerTripDestinationParkingTaz(:,:)
    real::Average_value_of_time,DrivingWeight,WalkingWeight,TrasitSkimTimeIntervalLength,WalkSpeed,LinkTimeInterval,SpskimTimeInterval
    real::TransitSkimTimeIntervalLength,NumColumnsInSuperZOneFile,zz,value_of_time,trip_travel_time,LatestStartTimeIinterval,SkimHorizon
    real::SpSkimTime,CurrentDynasmartTime
    integer,allocatable::TravellerType(:)
!personData_1.cvs
    integer::age,fp_choice,imf_choice,inmf_choice,user_class_work_walk,user_class_work_pnr,user_class_work_knr,user_class_non_work_walk,user_class_non_work_pnr,user_class_non_work_knr
    character(10)::gender,type1,activity_pattern
    real::walk_time_weight,walk_speed,max_walk
!Car (Changes for considering vehicles in addition to the travellers)
    integer,allocatable:: ValidCar(:),FirstCarTrip(:),CarNumFirstTimeIntervalTrip(:),NumCarInTimeInterval(:),CarInEachTimeInterval(:,:),CarIndexInEachTimeInterval(:,:),TransitPassengerInEachTimeInterval(:,:)
    real,allocatable:: CarStartTime(:),ValueOfTime(:),park_ride_maz_travel_time(:)
    real::CumulativeActivityTime
    integer::MaxNumCarTrip,MaxNumCarinEachTimeInterval,MaxNumVehicle,kay,superzoneswitch,TotalNumVehicle,MaxNumVehicleTrip,TotalNumVehicleandBus

!Joint (Changes for considering Joint in addition to the vehicles and travellers)
    character(500)::JointTourLine 
    character(1),allocatable::JointTourCharacter(:)
    integer::JointTourMaxNumCharacter,SpaceCount,i,tour_composition,depart_period,arrive_period,num_ob_stops,num_ib_stops,num_participants,MaxJointCarTrip
    integer,allocatable::tour_participants(:),PersonIDOfHH_PN(:,:),NumTourParticipant(:,:),PersonNumOfHH_TourID(:,:,:),TravellerTripDriver(:,:),CarNumTrip(:),Traveller_HH_ID(:),TravellerTripTourID(:,:),TravellerTripJointFlag(:,:)
    real,allocatable::util(:),prob(:)
!ParkRide
real:: trip_start_time,a_time,e_time,tr_time,ali 
integer::otranz,dtranz,trip_park_ride_flag,otap,dtap,NumPairPNRZone,PNRZoneTemp,Zone1,Zone2
integer:: NumTransitOptionTemp,NumPairPNRWalkZone,PNRZoneWalkTemp,NumTransitWalkOption,NumTransitWalkOptionTemp,ZoneTemp
integer::numofparkandride,otran_maz
integer,allocatable::TazToTransitTaz(:),MazToTransitMaz(:),TravellerTripParkRideFlag(:,:)    
real,allocatable::EsPNRTravelTime(:,:)
integer::ParkRideFoundFlag,TransitTemp,TransitTemp1,TransitTemp2,n,DtapTranz
real::WalkTimeTemp,drivelegtime,drivelegcost,transitlegtime,PNRTransitStartTime,PNRTravelTimeTemp
real::PNRTravelTime,TransitLegCost,WalkLegTime,PRNTransitStartTime
!IntraSuperzon
integer,allocatable:: LatestCheckedSuperZoneTrip(:),tempvalue(:),SuperZoneMap(:,:)
!PNRSkim and PathBank
integer::value,TurnLinkIndex
real::linktt,turntt
integer,allocatable:: PNRMAZ(:,:) 
integer,allocatable::IntraSuperzonePathIndex(:,:),PathBank(:,:),NumIntraSuperzoneZone(:),NodeIndex(:),LinkNumber(:,:),MovementLink(:,:),NumMovementLink(:)
real,allocatable::LinkTravelTime(:,:),TurnPenaltyTime(:,:,:)
!integer,allocatable::PathTemp(:)
!TDSP
character(3):: SPSkimFirstEnter
INTEGER:: NumSkimOriginZone, NumSkimSuperZone, NumSkimIntervals
INTEGER:: Origin,Destination,TimeInt,NumOfVOTGs
INTEGER:: OIntrst,DIntrst,TIntrst,numODTInterested,VOTGIntrst
INTEGER:: O,D,T,VOTG,error1,nlast,nvt
REAL:: VOTIntrst
REAL,Allocatable:: TempVOT(:)
REAL,Allocatable:: TempCost(:)
REAL,Allocatable:: TempTime(:)
REAL,Allocatable:: TempDistance(:)

!Input for schedule adjustment 
    integer:: checkcounter,checkcounter1,checkcounter2,checkcounter3,checkcounter4,todiff
    integer:: HouseHoldCounter,numHouseHold,TravellerIndex,numHHJointTrips,zzz,hh_idtemp,NumIndivTrips,TTNumJointTrips,kk,linecounter,lsup,bubble
    integer::tripcounter,participantcounter,numHHMembers,TotalHHTripNum,numTotalParticipants,mm
    integer,allocatable::HHID(:),PersonTypes(:),NumJointTrips(:),personTypeNumTrips(:),orgActTypes(:),dstActTypes(:),HouseHold_ID(:),Participants(:), HHIDofPerson(:),HouseHoldIDtoCounter(:)
    integer,allocatable::JointTrips(:),NumCompletedTrips(:),NumParticipants(:),HHnumJointTour(:),HHnumJointTrips(:),NumTripParticipant(:,:),PersonNumOfHH_TripID(:,:,:)
    real,allocatable:: HHJointTripStarTimeList(:,:)
    integer,allocatable::TripPlannedDeparture(:),TripPlannedTime(:),TripSimulatedTime(:),TripSimulateddeparture(:),TripExpectedTime(:),TripAdjustedDeparture(:)
    !input for read joint trip
    integer::orig_park_maz,orig_park_taz,dest_park_taz
    real::temp
    real,allocatable::tempvector(:,:)
!input for calculation after Schedule adjustment
!ParkRide
integer::NumTransitOption,otaptranz
integer,allocatable::TravellerTripOriginParkingTaz(:,:),TravellerTripOriginParkingMaz(:,:),TravellerTripDestinationParkingMaz(:,:)
integer,allocatable::PNRZone(:,:),TransitZone(:,:),NumTransitZoneCa(:),TravellerTripOriginTransitMaz(:,:),TravellerTripDestinationTransitMaz(:,:)
integer,allocatable::TravellerTripOriginTransitTAZ(:,:),TravellerTripDestinationTransitTAZ(:,:)
real,allocatable::PNRaTime(:,:),PNReTime(:,:)

!Intrasuperzone car trip
integer,allocatable:: IntraSuperZoneFlag(:,:),IntraSuperZoneTAZ(:,:),NumIntraSuperZoneTAZ(:)
!TransitSkim
integer:: NumPairTransitSkim,PNRTransitTimeInterval,NumTotalTransitSkimZone
integer,allocatable::TransitPairIndex(:,:),TransitPNRZonePair(:,:)
real,allocatable:: TransitSkimABTime(:,:),TransitSkimABCost(:,:),TransitSkimBATime(:,:),TransitSkimBACost(:,:),TransitSkimWalkTime(:,:)
!Walk
real::walkdistance,PNRWalkZonetemp
integer,allocatable:: TransitWalkZone(:,:),NumTransitWalkZoneCa(:),PNRWalkZone(:,:)
real,allocatable:: zone_walk_time(:,:)
!Check
integer,allocatable:: NumTravelerMaxNumTrip(:)
!Path bank

integer,allocatable::UpStreamLink(:),DownStreamLink(:)
!Read vehicle.dat
integer::trip_intermediate_num,upn,dpn
integer,allocatable::VehicleTIN(:,:),Vehicleupn(:,:),Vehicledpn(:,:),ijtovehicleindex(:,:),ijtovehicletripindex(:,:)
integer,allocatable::VehicleToTravellertripIndex(:,:),Vehicleiutmp(:),Vehicleidtmp(:),Vehicleivcltmp(:),Vehicleivcl2tmp(:)
integer,allocatable::vehicleihovtmp(:),vehicleveh_pathnodenum(:),VehicleNonrepetitiveCarNumTrip(:),Vehicleinfotmp(:),VehicleTaz(:,:)
integer,allocatable::VehicleStartTimeInterval(:)
real,allocatable::Vehicleribftmp(:),Vehiclecomptmp(:),VehicleValueofTime(:),VehicleStartTime(:),VehicleActivityTime(:,:)
real,allocatable:: VehicleTravelTime(:,:)       
!
integer::Jleft,Jright
!sorting transit
integer::TravellerIDForJ,tt
real::traveltimetemp
!sorting vehicles
integer::VehicleIndexForj,TripIndexForj,TravellerIDForJleft,VehicleIndexForJleft,TravellerIDForJright,VehicleIndexForJRight
!Dll
integer::negativeactivityperrsoncounter,afnegativeactivityperrsoncounter,afstressPcounter,afstressHHcounter,naPflag,StressdHHflag,naPcounter,naHHcounter,StressPcounter,StressHHcounter
integer::NumPersonType,NumPurposeType,stressPflag,stressHHflag,naHHflag,hhgap,checktemp,dstActTypesTemp
integer,allocatable::adjustFlag(:),DllHHflag(:),HHNumCarTrips(:),HHNumTransitTrips(:),HHNumPNRTrips(:),HHNumNeATrips(:),HHNumNhNeATrips(:),TravellerIncompleteFlag(:),HHIncompleteFlag(:),ABMIncompleteFlag(:)
real::MTT,TTO,MTA,META,TotalMeta,beta1,beta2,beta3,beta4,beta5,dllprobability,deltaU1temp,deltaU2temp,deltaU3temp,deltaU4temp,maxdeltaU4,totalnactivitytemp,totalactivitytemp
real,allocatable::StressMTT(:),StressTTO(:),StressMTA(:),deltaU1(:),deltaU2(:),deltaU3(:),deltaU4(:),totalnactivity(:),totalactivity(:)
integer:: dllflagtemp,dllmethodflag
character(3)::SkimText
character(50)::SkimTextFinal
real,allocatable::TripExpDeparture(:,:),TripExpArrival(:,:),TripExpActivityDuration(:,:)
real::planninghorizon
!Random Number
integer,allocatable::seed(:)
real::x,x1
integer::randomseed,seedsize

!Calculate penalty (obj function) 
!real,allocatable::penaltyEDep(:,:),penaltyEDepThre(:,:),EDepThre(:,:),penaltyLDep(:,:), penaltyLDepThre(:,:),LDepThre(:,:),penaltyEArr(:,:),penaltyEArrThre(:,:),EArrThre(:,:),penaltyLArr(:,:), penaltyLArrThre(:,:),LArrThre(:,:)
real,allocatable::DurShort1(:,:),DurShort2(:,:),DurLong1(:,:),DurLong2(:,:),DepEarly1(:,:),DepEarly2(:,:),DepEarly(:,:)
real,allocatable::DepLate1(:,:),DepLate2(:,:),DepLate(:,:),ArrEarly1(:,:),ArrEarly2(:,:),ArrEarly(:,:),ArrLate1(:,:),ArrLate2(:,:),ArrLate(:,:)
real,allocatable::DurShort2Thre(:,:),DurShort3Thre(:,:),DurShort4Thre(:,:),DurLong2Thre(:,:),DurLong3Thre(:,:)
real,allocatable::DepEarly2Thre(:,:),DepLate2Thre(:,:),ArrEarly2Thre(:,:),ArrLate2Thre(:,:)

!Map trajectory and abm index
integer,allocatable::VehIndextoID(:,:)
!Ali
integer,allocatable::stressHHflagVector(:)
real::MTT_Trn,MTT_PNR,MTT_Car
!Ali end
!-------------------------------------------------   
write(*,*) "Tour Processor Component 2" 
!Openning Required Files For the First Time
    open(file='indivTripData_1.csv',unit=1)  !Input file (Output of ABM), which includes all the individuals trips (including non-family shared rides)
    !open(file='Output.dat',unit=2)          !Intermediate Output file
    open(file='Error.dat',unit=3)            !Intermediate Output File which includes all the error or problems which are observed
    !open(file='ZoneMap.dat',unit=4)          !Input file, just for the subarea network to map original zone numbers (TAZ) to sub-area zone (TAZ)
    open(file='SuperZone.dat',unit=5)
    open(file='TimeInterval.dat',unit=9)     !Input file, includes the time inervals information (starting time (simulation minute) and length (minute) of interval) based on the ABM definitions
    !open(file='network.dat',unit=6)          !Input file for this code and a standard input of Dynasmart used for genreation link selection
    !open(file='origin.dat',unit=7)           !Input file for this code and a standard input of Dynasmart used for genreation link selection
    open(file='traveler.dat',unit=8)        !Output file which includes traveller's information, for each trveller in the first line, genral information and the subsequent lines one trip information per line    
   ! open(file='travelerTT.dat',unit=81)
!Car
    !open(file='Error-Vehicle.dat',unit=9)    !Errors observed for vehicle file writing
    open(file='output_vehicleTT.dat',unit=10)         !Final Output of the code which is the standard input of Dynasmart
    open(file='vehicleTT.dat',unit=101)
!Joint
    !open(file='jointTourData_1.csv',unit=11) !Input file (Output of ABM), which includes all the joint tours information (Family members who are sharing rides), this file is used to identify joint trips participants 
    open(file='jointTripData_1.csv',unit=12) !Input file (Output of ABM), which includes all the joint trips information (Family members who are sharing rides)           
!ParkRide
!    open(file='maz_tran_maz.dat',unit=13)
!    open(file='taz_tran_taz.dat',unit=14)   
!Driver or passenger 
    open(file='AllocationInput.dat',unit=15)
    !open(file='NewTrip.dat',unit=16)
    !open(file='Newerror-vehicle.dat',unit=17)
    open(file='output_transit.dat',unit=18)
    open(file='transit.dat',unit=181)
    !open(file='ChangeofError-Vehicle.dat',unit=19)
    !open(file='intervalvalidtriperro.dat',unit=20)
!External Zone Traffic
    !open(file='external_vehicle.dat',unit=22)
    open(file='internalmap.dat',unit=23)
    !open(file='internalmapTT.dat',unit=231)
    open(file='externalmap.dat',unit=24)
    !open(file='testtraveltime.dat',unit=26)
   
!PathBank& PNRSkim
    !open(file='PNRskim_AB.dat',unit=26)
    !open(file='PNRskim_BA.dat',unit=27)
    open(file='output_indivTripData_1.csv',unit=28)
    open(file='output_jointTripData_1.csv',unit=29)
    open(file='maz_to_taz.dat',unit=11)
    open(file='intrasuperzone_path.dat',unit=13)
    open(file='intrasuperzone_vehicle.dat',unit=14)
    open(file='output_td_linktraveltime.dat',unit=16)
    open(file='output_td_turnpenalty.dat',unit=20)
!new writeout
    open(file='newtraveller.dat',unit=21)
!check  
    open(file='dlltest.dat',unit=22)
    open(file='output_path.dat',unit=37)
    open(file='path.dat',unit=371)
!dll result
    open(file='stressthreshold.dat',unit=38)
!ABM Map
    open(file='DYNASMARTtoABMMap.dat',unit=43)
    open(file='NU-TRANStoABMMap.dat',unit=44)
    open(file='VehIndextoID.dat',unit=45)
    
    NumPersonType=8
    NumPurposeType=10   
    
    allocate(StressMTT(NumPersonType))
        StressMTT(:)=0
    allocate(StressTTO(NumPersonType))
        StressTTO(:)=0
    allocate(StressMTA(NumPersonType))
        StressMTA(:)=0
    error=1
    counter=0
     if (error.ne.-1) then      
        do while (error.ne.-1)   
            read(38,'(a)',iostat=error) Line
            if (error.ne.-1) then
                counter=counter+1                
                read(Line,*,iostat=error) StressMTT(counter),StressTTO(counter),StressMTA(counter)
            endif
        enddo
     endif
   
!call my_message()
!write(*,*) "check"
!!call my_values()
!!Check
    !open(file='alexcheck.dat',unit=30)
!Manual input---------------------------------------------------------
!Sub-area Network Specific  	

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
    !jjtemplength=5
!External Vehicle
stimedifference=180
MaxNumVehicle=18
!MaxNumZone=57

!PNR Skim & Path Bank
error=1
!For NumPairPNRMAZ
!read(26,*)
!NumPairPNRMAZ=0
!do while (error.ne.-1) 
!    read(26,*,iostat=error) 
!    NumPairPNRMAZ=NumPairPNRMAZ+1
!enddo
!NumPairPNRMAZ=NumPairPNRMAZ-1
!close(26)
!------------------------
!Get the Number of Link
do j=1,5
    read(16,'(a)') Line
enddo
read(16,'(a)') CheckLine
read(16,'(a)') Line
!NumLink=0
MaxNodeID=0
do while (Line.ne.CheckLine)
    read(16,'(a)') Line
    if(Line.ne.CheckLine) then
        read(Line,*) value,nodea,nodeb,value,value
        if(max(nodea,nodeb).gt.MaxNodeID) MaxNodeID=max(nodea,nodeb)
    endif
enddo
close(16)
!----------------------------------
!Get the Number of Turn
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
MaxPathLength=163
close (14)

planninghorizon=1440.0
read(15,*) MaxTAZ
read(15,*) NumMAZ
read(15,*) MaxPersonID
read(15,*) MaxNumTraveller
read(15,*) MaxNumTrips
read(15,*) MaxNumTimeInterval
read(15,*) MaxHouseHoldID
read(15,*) MaxHHSize
read(15,*) average_value_of_time
read(15,*) NumNode
read(15,*) NumLink
read(15,*) LinkTimeInterval
read(15,*) SpSkimTimeInterval
read(15,*) NumIteration   
read(15,*) NumHouseHold
read(15,*) NumIndivTrips
read(15,*) TTNumJointTrips

close(15)

open(file='IterationSetting.dat',unit=151) 
read(151,*) IterationID
read(151,*) TargetIterationNumber
close (151)

 
!-------------------------------------------------------------------
!Allocation
    write(*,*)'Allocating arrays...'  	     	       
   ! read(6,*) nzones_old,noofnodes_old,noofarcs_old,kay,SuperZoneSwitch
   ! NumTAZ=57
    allocate (Traveller_index(MaxPersonID))
        Traveller_index(:)=0
    allocate (Traveller_ID(MaxNumTraveller))
        Traveller_ID(:)=0
    allocate (TravellerNumTrips(MaxNumTraveller))
        TravellerNumTrips(:)=0
!    allocate (TravellerNumFirstTimeIntervalTrip(MaxNumTraveller))
!        TravellerNumFirstTimeIntervalTrip(:)=0
    
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
!    allocate (TAZMap(MaxTAZ))
!        TAZMap(:)=0
    allocate (ValidTraveller(MaxNumTraveller))
     ValidTraveller(:)=1
    allocate (NumTravellerInTimeInterval(MaxNumTimeInterval))
        NumTravellerInTimeInterval(:)=0
!    allocate (TravellerStartTime(MaxNumTraveller))
!        TravellerStartTime(:)=0
    allocate (TripDriverPassengerFlag(MaxNumTraveller,MaxNumTrips))
        TripDriverPassengerFlag(:,:)=0
        
!    allocate (FirstTransitPassengerTripStartTimeInterval(MaxNumTraveller))
!        FirstTransitPassengerTripStartTimeInterval(:)=MaxNumTimeInterval+1
    allocate(TravellerType(MaxPersonID))
        TravellerType(:)=0
!Car
    allocate (ValidCar(MaxNumTraveller))
     ValidCar(:)=1
    allocate (FirstCarTrip(MaxNumTraveller))
        FirstCarTrip(:)=0
    allocate (CarNumFirstTimeIntervalTrip(MaxNumTraveller))
        CarNumFirstTimeIntervalTrip(:)=0
    allocate (CarNumTrip(MaxNumTraveller))
        CarNumTrip(:)=0
    allocate (NumCarInTimeInterval(MaxNumTimeInterval))
        NumCarInTimeInterval(:)=0
    allocate (CarStartTime(MaxNumTraveller))
        CarStartTime(:)=0     
!
!   
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
    allocate(NumTripParticipant(MaxHouseHoldID,MaxNumTrips))
        NumTripParticipant(:,:)=0
    allocate (PersonNumOfHH_TripID(MaxHouseHoldID,MaxNumTrips,MaxHHSize))
        PersonNumOfHH_TripID(:,:,:)=0
!ParkRide
!    allocate (TazToTransitTaz(MaxTAZ))
!        TazToTransitTaz(:)=0                                
!    allocate (MazToTransitMaz(NumMAZ))
!        MazToTransitMaz(:)=0     
    allocate(TravellerParkRideNum(MaxNumTraveller))
        TravellerParkRideNum(:)=0
!Driver or passenger                           
    allocate (TravellerTripParkRideFlag(MaxNumTraveller,MaxNumTrips))
        TravellerTripParkRideFlag(:,:)=0  
    allocate(TravellerTripDestinationParkingTaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationParkingTaz(:,:)=0   
!For two car in one interval
!    allocate(jjtemp(jjtemplength))
!        jjtemp(:)=0
!    allocate(jjtemp1(jjtemplength))
!        jjtemp1(:)=0
!    allocate(ftjjtemp(jjtemplength))
!        ftjjtemp(:)=0
        
    allocate(TransitPassengerStartTime(MaxNumTraveller))
        TransitPassengerStartTime(:)=0
        
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
        
    allocate(TripTravelTime(MaxNumTraveller,MaxNumTrips))
        TripTravelTime(:,:)=0
        
    allocate(TravellerTripOriginMappedTaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginMappedTaz(:,:)=0
    allocate(TravellerTripDestinationMappedTaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationMappedTaz(:,:)=0
!        
!    allocate(ZoneToZoneTravelTime(MaxTAZ,MaxTAZ,MaxNumTimeInterval))
!        ZoneToZoneTravelTime(:,:,:)=0
    allocate(ABMTripIndex(MaxNumTraveller,MaxNumTrips))
        ABMTripIndex(:,:)=0
    allocate(LatestCheckedTrip(MaxNumTraveller))
        LatestCheckedTrip(:)=0
    allocate(park_ride_taz_travel_time(MaxNumTimeInterval))
        park_ride_taz_travel_time(:)=0
        
!      allocate(NonRepTazToTransitTaz(NumTAZ))
!        NonRepTazToTransitTaz(:)=0
    allocate(MaztoTazMap(NumMAZ))
        MaztoTazMap(:)=0
    allocate(TaztoMazMap(MaxTaz))
        TaztoMazMap(:)=0
    allocate(NonRepMazToTransitMaz(NumMAZ))
        NonRepMAZToTransitMAZ(:)=0
!      allocate(park_ride_maz_travel_time(48))
!        park_ride_maz_travel_time(:)=0
    !SuperZone
    allocate(LatestCheckedSuperZoneTrip(MaxNumTraveller))
        LatestCheckedSuperZoneTrip(:)=0
     allocate(tempvalue(15))
        tempvalue(:)=0
    allocate(SuperZoneMap(MaxTaz,2))
        SuperZoneMap(:,:)=0
!PNRSkim
    allocate(PNRMAZ(NumPairPNRMAZ,2))
        PNRMAZ(:,:)=0
!PathBank
    allocate(IntraSuperzonePathIndex(MaxTaz,MaxTaz))
        IntraSuperzonePathIndex(:,:)=0
    allocate(PathBank(NumPath,MaxPathLength))
        PathBank(:,:)=0 
    
    allocate(NumIntraSuperzoneZone(NumPath))
        NumIntraSuperzoneZone(:)=0

    allocate(TravellerIncompleteFlag(MaxNumTraveller))
        TravellerIncompleteFlag(:)=0
    
!Read the starttime interval
do i=1,MaxNumTimeInterval
    read(9,*)  StartTimeofTimeInterval,LengthofTimeInterval
    VectorStartTimeofTimeInterval(i)=StartTimeofTimeInterval
    VectorLengthofTimeInterval(i)=LengthofTimeInterval
enddo
close(9)

!-------------------------------------------------------------------       	
!Just for the Sub-area network
!Creating a map for TAZ to project the Original TAZ to the TAZ in subarea network using the input file         
!    do i=1,NumTAZ
!        read(4,*) OriginalTAZ, MappedTaz
!        TAZMap(OriginalTAZ)= MappedTaz
!        
!    enddo
!    close(4)
!error=1
!do while(error.ne.-1)   
!    read(4,*,iostat=error) OriginalTAZ, MappedTaz
!    TAZMap(OriginalTAZ)= MappedTaz
!enddo
!close(4)
    
 
!-------------------------------------------------------------------       	    

    !Creating a map for maz to taz
 read(11,*)
 do i=1,NumMAZ
    read(11,*) maz,taz
    MaztoTazMap(maz)=taz
    TaztoMazMap(taz)=maz
 enddo
 close(11)
!ReadPNRMAZ

!open(file='PNRskim_AB.dat',unit=26)
!read(26,*)
!do i=1,NumPairPNRMAZ
!    read(26,*)PNRMAZ(i,1),PNRMAZ(i,2)
!enddo
!close(26)
!open(file='PNRSkim_AB.dat',unit=26)

 !Read in the output_path.dat
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
        read(14,*) desttaz,value
        IntraSuperzonePathIndex(origtaz,desttaz)=i
        read(13,*) PathBank(i,1:veh_pathnodenum)
    endif
enddo
close(14)
close(13)
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
!-------------------------------------------------------------------
!!Create a map between the TAZ number and super zone number
error=1
read(5,*) maxnumsuperzone
read(5,*) 
i=1
NumColumnsinSuperZoneFile=15
zz=MaxTAZ/NumColumnsinSuperZoneFile
NumRowsinSuperZoneFile=floor(zz)

do m=1,NumRowsinSuperZoneFile
    read(5,*) tempvalue(1:NumColumnsinSuperZoneFile)
    SuperZoneMap(i:i+NumColumnsinSuperZoneFile-1,1)=tempvalue(1:NumColumnsinSuperZoneFile)
    i=i+NumColumnsinSuperZoneFile
enddo
lengthoflastrow=MaxTaz-(m-1)*NumColumnsinSuperZoneFile
read(5,*) tempvalue(1:lengthoflastrow)
SuperZoneMap(i:i+lengthoflastrow-1,1)=tempvalue(1:lengthoflastrow)
read(5,*)
i=1
do m=1,NumRowsinSuperZoneFile
    read(5,*) tempvalue(1:NumColumnsinSuperZoneFile)
    SuperZoneMap(i:i+NumColumnsinSuperZoneFile-1,2)=tempvalue(1:NumColumnsinSuperZoneFile)
    i=i+15
enddo
read(5,*) tempvalue(1:lengthoflastrow)
SuperZoneMap(i:i+lengthoflastrow-1,2)=tempvalue(1:lengthoflastrow)
close(5)


!-------------------------------------------------------------------       	
!Read the traveller.dat
write(*,*) "Read traveler.dat"
    error=1
    !read the heading
    read(8,*)NumTraveller, RealMaxNumTrips
    do i=1,2
        read(8,*)
    enddo
    index=0
    allocate(countertoi(NumTraveller))
        countertoi(:)=0
    do while (error.ne.-1)  !read until the last line
        read(8,'(a)',iostat=error) Line
        if (error.ne.-1) then
            !read the traveller's information and store them
            read(Line,*,iostat=error)counter,i,person_id,StartTime,numtrips,Taz,valid_car,first_car_trip,first_transit_trip,value_of_time,traveler_type
           
            index=max(i,index) !index+1 change 8-26-2015 by alex
           ! Traveller_counter(i)=counter
            countertoi(counter)=i
            Traveller_Index(person_id)=i
            Traveller_ID(i)=person_id
            TravellerNumTrips(i)=numtrips
            ValidCar(i)=valid_car
            FirstTransitTrip(i)=first_transit_trip
            FirstCarTrip(i)=first_car_trip
            ValueOfTime(Traveller_ID(i))=value_of_time
            TravellerType(Traveller_ID(i))=traveler_type
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
                if(j.gt.1) then 
                    if(TripStartTime(i,j).le.TripStartTime(i,j-1)) then 
                        !write(111,*) "Trip Start Time of",Traveller_ID(i),"is not assigned properly"
                        Traveller_Index(person_id)=0
                        
                    endif
                endif
                TravellerActivityTime(i,j)=ActivityTime
            enddo
        endif
    enddo
    close(8)
    SameOriginDestinationParkRide=0

!Disaggregate Park and Ride trip to a car trip and a transit trip
!ParkRide
write(*,*) "Read output_transit.dat"
error=1
!read the heading
checkcounter=0
read(18,*)
do while (error.ne.-1)  !read until the last line
    read(18,'(a)',iostat=error) Line
    if (error.ne.-1) then
        !read the traveller's information and store them
        read(Line,*,iostat=error) person_id,person_index,trip_index,trip_start_time,otranz,dtranz,trip_park_ride_flag,otap,dtap,a_time,e_time,tr_time 
        checkcounter=checkcounter+1
        write(44,*) person_id,trip_index,ABMTripINdex(person_index,trip_index)
        i=person_index
        j=trip_index
        !if (otap.gt.0) then 
           ! if(trip_park_ride_flag.gt.0.and.otran_maz.ne.0) TravellerParkRideNum(i)=TravellerParkRideNum(i)+1
            if(trip_park_ride_flag.gt.0) then 
                TravellerParkRideNum(i)=TravellerParkRideNum(i)+1
                j=j+TravellerParkRideNum(i)-1
                TravellerNumTrips(i)=TravellerNumTrips(i)+1
                do jj=TravellerNumTrips(i),j+1,-1                
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
                    TravellerTripParkRideFlag(i,jj)=TravellerTripParkRideFlag(i,jj-1)
                    TravellerActivityTime(i,jj)=TravellerActivityTime(i,jj-1)
                    !Joint
                    TravellerTripDriver(i,jj)=TravellerTripDriver(i,jj-1)
                    TravellerTripTourID(i,jj)=TravellerTripTourID(i,jj-1)
                    TravellerTripJointFlag(i,jj)=TravellerTripJointFlag(i,jj-1)                
                    ABMTripIndex(i,jj)=ABMTripIndex(i,jj-1)
                enddo
!                if(trip_park_ride_flag.gt.2) then 
!                    if (mod(trip_park_ride_flag,2).eq.0)then
!                        trip_park_ride_flag=2
!                    else
!                        trip_park_ride_flag=1
!                    endif
!                endif
                 
                    if (trip_park_ride_flag.eq.1) then
                        
                        TravellerTripMode(i,j)=1
                        !TravellerTripDestinationMAZ(i,j)=orig_maz
    !                    TravellerTripDestinationMAZ(i,j)=otap
    !                    !TravellerTripDestinationTAZ(i,j)=TazToTransitTaz(TravellerTripOriginTAZ(i,j))
    !                    TravellerTripDestinationTAZ(i,j)=MaztoTazMap(otran_maz)
    !                    !TravellerTripDestinationMappedTaz(i,j)=TAZMap(TravellerTripDestinationTAZ(i,j))
                        if (otap.gt.0) then
                            if (TransitMazTazFlag.eq.0) then !MAZ based                    
                                TravellerTripDestinationMAZ(i,j)=otap
                                TravellerTripDestinationTAZ(i,j)=MaztoTazMap(otap)
                            else !Taz based                    
                                TravellerTripDestinationTAZ(i,j)=otap
                                TravellerTripDestinationMAZ(i,j)=TaztoMazMap(otap)
                            endif
                        endif
                        !TravellerTripDestinationTAZ(i,j)=transfer_taz
                        TravellerTripDestinationPurpose(i,j)=TravellerTripOriginPurpose(i,j)
                        if (TravellerTripJointFlag(i,j).eq.0) TravellerTripDriver(i,j)=Traveller_ID(i)
                        TravellerTripParkRideFlag(i,j)=trip_park_ride_flag 
                        !ABMTripIndex(i,j)=ABMTripIndex(i,j)
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
                        TravellerActivityTime(i,j+1)=TravellerActivityTime(i,j)-1
                        TravellerActivityTime(i,j)=1                                   
                        TravellerTripMode(i,j+1)=TravellerTripMode(i,j+1)-2
                        TravellerTripOriginMAZ(i,j+1)=TravellerTripDestinationMAZ(i,j)         
                        TravellerTripOriginTAZ(i,j+1)=TravellerTripDestinationTAZ(i,j)   
                        !TravellerTripOriginMappedTaz(i,j+1)=TAZMap(TravellerTripOriginTAZ(i,j+1))
                                         
                        TravellerTripParkRideFlag(i,j+1)=trip_park_ride_flag
                        TripDriverPassengerFlag(i,j+1)=-1
                        TripStartTime(i,j+1)=TripStartTime(i,j)+a_time 
                        if(j+2.le.TravellerNumTrips(i)) then
                            if(TripStartTime(i,j+1).gt.TripStartTime(i,j+2)) then
                                TripStartTime(i,j+1)=TripStartTime(i,j)+0.5*(TripStartTime(i,j+2)-TripStartTime(i,j))
                            endif
                        elseif(TripStartTime(i,j+1).gt.1440) then
                            TripStartTime(i,j+1)=1440
                        endif
                        if (TripStartTime(i,j+1).lt.VectorStartTimeofTimeInterval(2)) then 
                            TravellerTripStartTimeInterval(i,j+1)=1
                        else if (TripStartTime(i,j+1).gt.VectorStartTimeofTimeInterval(MaxNumTimeInterval)) then 
                            TravellerTripStartTimeInterval(i,j+1)=MaxNumTimeInterval
                        else 
                            TravellerTripStartTimeInterval(i,j+1)=ceiling((TripStartTime(i,j+1)-120)/30.0)+1
                        endif
                        
                        TravellerTripStartTimeInterval(i,j+1)=TravellerTripStartTimeInterval(i,j)
                        ABMTripIndex(i,j+1)=ABMTripIndex(i,j)
                       ! if(person_id.eq.701915) write(*,*) TravellerTripMode(i,:)
                       trip_travel_time=e_time+tr_time
                       
                       TripTravelTime(i,j+1)=trip_travel_time
                       !TravellerActivityTime(i,j+1)=0
                       !if(TripTravelTime(i,j+1).eq.0) TripTravelTime(i,j+1)=1440
                    endif
                    
                    if (trip_park_ride_flag.eq.2) then
                         TripTravelTime(i,j)=a_time+tr_time
                         !if(TripTravelTime(i,j).eq.0) TripTravelTime(i,j)=1440
                        TravellerTripMode(i,j)=TravellerTripMode(i,j)-2
    !                    TravellerTripDestinationMAZ(i,j)=dtran_maz
    !                    !TravellerTripDestinationMAZ(i,j)=dest_maz
    !                    !TravellerTripDestinationTAZ(i,j)=TazToTransitTaz(TravellerTripDestinationTAZ(i,j))    
    !                    TravellerTripDestinationTAZ(i,j)=MaztoTazMap(dtran_maz)   
                       ! TravellerTripDestinationMappedTaz(i,j)=TAZMap(TravellerTripDestinationTAZ(i,j))
                       if (otap.gt.0) then
                            if (TransitMazTazFlag.eq.0) then !MAZ based
                                TravellerTripDestinationMAZ(i,j)=dtap
                                TravellerTripDestinationTAZ(i,j)=MaztoTazMap(dtap) 
                               
                            else !Taz based
                                TravellerTripDestinationTAZ(i,j)=dtap
                                TravellerTripDestinationMAZ(i,j)=TaztoMazMap(dtap)
                            endif
                       endif
                       
                        
                               
                        TripDriverPassengerFlag(i,j)=-1
                        TravellerTripParkRideFlag(i,j)=trip_park_ride_flag
                        !ABMTripIndex(i,j)=ABMTripIndex(i,j)
                        
                        TravellerActivityTime(i,j+1)=TravellerActivityTime(i,j)-1
                        TravellerActivityTime(i,j)=1
                        TravellerTripMode(i,j+1)=1
                        TravellerTripOriginMAZ(i,j+1)=TravellerTripDestinationMAZ(i,j)
                        TravellerTripOriginTAZ(i,j+1)=TravellerTripDestinationTAZ(i,j)
                        !TravellerTripOriginMappedTaz(i,j+1)=TAZMap(TravellerTripOriginTAZ(i,j+1))
                        
                        TravellerTripOriginPurpose(i,j+1)=TravellerTripDestinationPurpose(i,j+1)
                        ABMTripIndex(i,j+1)=ABMTripIndex(i,j)
                        trip_travel_time=a_time+tr_time
                        
                        TripStartTime(i,j+1)=TripStartTime(i,j)+TripTravelTime(i,j)
                        
                        if(j+2.le.TravellerNumTrips(i)) then
                            if(TripStartTime(i,j+1).gt.TripStartTime(i,j+2)) then
                                TripStartTime(i,j+1)=TripStartTime(i,j)+0.5*(TripStartTime(i,j+2)-TripStartTime(i,j))
                            endif
                        elseif(TripStartTime(i,j+1).gt.1440) then
                            TripStartTime(i,j+1)=1440
                        endif
                        
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

                        
                        TravellerTripParkRideFlag(i,j+1)=trip_park_ride_flag
                        !if(person_id.eq.701915) write(*,*) TravellerTripMode(i,:),j
                        
                    endif   
                               
            else
                j=j+TravellerParkRideNum(i)
                trip_travel_time=a_time+e_time+tr_time
                TripTravelTime(i,j)=trip_travel_time
                !if(TripTravelTime(i,j).eq.0) TripTravelTime(i,j)=1440
            endif 
    endif
enddo
close (18)
 !TripTravelTime=nint(TripTravelTime)*10.0/10.0
!-------------------------------------------
!Read in the travel time for the vehicle and store it
    !now read the vehicle.dat'
write(*,*) "Read output_vehicleTT.dat"
    error=1
    !read the heading
    
    read(10,*)TotalNumVehicle,MaxNumVehicleTrip
    read(10,*)
    read(23,*)
    TotalNumVehicleandBus=TotalNumVehicle*1.4
    allocate(ijtovehicleindex(MaxNumTraveller,MaxNumTrips))
        ijtovehicleindex(:,:)=0
    allocate(ijtovehicletripindex(MaxNumTraveller,MaxNumTrips))
        ijtovehicletripindex(:,:)=0
    allocate(VehicleToTravellertripIndex(TotalNumVehicle,MaxNumVehicleTrip+1))
        VehicleToTravellertripIndex(:,:)=0
    allocate(Vehicleiutmp(TotalNumVehicle))
        Vehicleiutmp(:)=0
    allocate(Vehicleidtmp(TotalNumVehicle))
        Vehicleidtmp(:)=0
    allocate(Vehicleivcltmp(TotalNumVehicle))
        Vehicleivcltmp(:)=0
    allocate(Vehicleivcl2tmp(TotalNumVehicle))
        Vehicleivcl2tmp(:)=0
    allocate(vehicleihovtmp(TotalNumVehicle))
        vehicleihovtmp(:)=0
    allocate(vehicleveh_pathnodenum(TotalNumVehicle))
        vehicleveh_pathnodenum(:)=0
    allocate(VehicleNonrepetitiveCarNumTrip(TotalNumVehicle))
        VehicleNonrepetitiveCarNumTrip(:)=0
    allocate(Vehicleinfotmp(TotalNumVehicle))
        Vehicleinfotmp(:)=0
    allocate(Vehicleribftmp(TotalNumVehicle))
        Vehicleribftmp(:)=0
    allocate(Vehiclecomptmp(TotalNumVehicle))
        Vehiclecomptmp(:)=0
    allocate(VehicleTaz(TotalNumVehicle,MaxNumVehicleTrip+1))
        VehicleTaz(:,:)=0
    allocate(VehicleStartTime(TotalNumVehicle))
        VehicleStartTime(:)=0
    allocate(VehicleStartTimeInterval(TotalNumVehicle))
        VehicleStartTimeInterval(:)=0
    allocate(VehicleValueofTime(TotalNumVehicle))
        VehicleValueofTime(:)=0
    allocate(VehicleActivityTime(TotalNumVehicle,MaxNumVehicleTrip))
        VehicleActivityTime(:,:)=0
!    allocate(PathTemp(MaxPathLength))
!        pathtemp(:)=0
    allocate(VehicleTIN(TotalNumVehicle,MaxNumVehicleTrip))
        VehicleTIN(:,:)=0
    allocate(Vehicleupn(TotalNumVehicle,MaxNumVehicleTrip))
        Vehicleupn(:,:)=0
    allocate(Vehicledpn(TotalNumVehicle,MaxNumVehicleTrip))
        Vehicledpn(:,:)=0
    allocate(VehicleTravelTime(TotalNumVehicle,MaxNumVehicleTrip))
        VehicleTravelTime(:,:)=0
    LatestStartTimeIinterval=0
!    allocate(VehiclePathBank(TotalNumVehicle,MaxPathLength))
!        VehiclePathBank(:,:)=0
    if (.NOT. ALLOCATED(vehiclePath_Array)) then
        Allocate(vehiclePath_Array(TotalNumVehicle),stat=error)
    endif    
    checkcounter1=0
    do while (error.ne.-1)  !read until the last line
        read(10,'(a)',iostat=error) Line
        if (error.ne.-1) then
            !read the vehicle's information and store them
            
            read(Line,*,iostat=error) counter,iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,NonrepetitiveCarNumTrip,infotmp,ribftmp,comptmp,Taz,value_of_time,dummy ,dllflagtemp
           
           ! write(*,*) Line
            orig_taz_temp=Taz
            read(23,*) vehicleindex,i,person_id 
            !write(*,*)vehicleindex,i,person_id
            !read(10,*)
            VehicleToTravellertripIndex(vehicleindex,1)=i
            Vehicleiutmp(vehicleindex)=iutmp
            Vehicleidtmp(vehicleindex)=idtmp
            Vehicleivcltmp(vehicleindex)=ivcltmp
            Vehicleivcl2tmp(vehicleindex)=ivcl2tmp
            vehicleihovtmp(vehicleindex)=ihovtmp
            vehicleveh_pathnodenum(vehicleindex)=veh_pathnodenum
            VehicleNonrepetitiveCarNumTrip(vehicleindex)=NonrepetitiveCarNumTrip
            Vehicleinfotmp(vehicleindex)=infotmp
            Vehicleribftmp(vehicleindex)=ribftmp
            Vehiclecomptmp(vehicleindex)=comptmp
            VehicleTaz(vehicleindex,1)=Taz
            VehicleStartTime(vehicleindex)=StartTime
            VehicleValueofTime(vehicleindex)=value_of_time
            if (StartTime.lt.VectorStartTimeofTimeInterval(2)) then 
                VehicleStartTimeInterval(vehicleindex)=1
            else if (StartTime.gt.VectorStartTimeofTimeInterval(MaxNumTimeInterval)) then 
                VehicleStartTimeInterval(vehicleindex)=MaxNumTimeInterval
            else 
                VehicleStartTimeInterval(vehicleindex)=ceiling((StartTime-120)/30.0)+1
            endif
            vehiclePath_Array(vehicleindex)%Psize=veh_pathnodenum
            call VehiclePathAllocate(vehicleindex,veh_pathnodenum)
            
            read(37,*)(VehiclePath_Array(vehicleindex)%p(kk)%pathnode,kk=1,veh_pathnodenum) !PathTemp(1:veh_pathnodenum)!VehiclePathBank(VehicleIndex,1:veh_pathnodenum)
            
            
            !call vehiclepathInsert(vehicleindex,veh_pathnodenum,pathtemp)
                    
            
            if (i.ne.0) then 
                !Changed for the new internalmap 10-31-2014
                
                do jj=1,NonrepetitiveCarNumTrip
                   
                    read(10,*) Taz,ActivityTime,trip_intermediate_num,upn,dpn,trip_travel_time
                    read(23,*) jindex
                    if(trip_travel_time.le.1.and.trip_travel_time.gt.0) trip_travel_time=1
                    trip_travel_time=nint(trip_travel_time)*10.0/10.0
                    
                    !if(jj.eq.1) then 
                        
                        ijtovehicleindex(i,jindex)=vehicleindex
                    !endif
                    VehicleToTravellertripIndex(vehicleindex,jj+1)=jindex
                    VehicleTaz(vehicleindex,jj+1)=Taz
                    VehicleActivityTime(vehicleindex,jj)=ActivityTime
                    VehicleTIN(vehicleindex,jj)=trip_intermediate_num
                    Vehicleupn(vehicleindex,jj)=upn
                    Vehicledpn(vehicleindex,jj)=dpn
                    !if(i.gt.0) then 
                    VehicleTravelTime(vehicleindex,jj)=trip_travel_time
                    TripTravelTime(i,jindex)=trip_travel_time
                    ijtovehicletripindex(i,jindex)=jj
                    !endif
!                    if(ZoneToZoneTravelTime(TravellerTripOriginTAZ(i,jindex),TravellerTripDestinationTAZ(i,jindex),TravellerTripStartTimeInterval(i,jindex)).eq.0) then 
!                        ZoneToZoneTravelTime(TravellerTripOriginTAZ(i,jindex),TravellerTripDestinationTAZ(i,jindex),TravellerTripStartTimeInterval(i,jindex))=TripTravelTime(i,jindex)
!                    endif
                    !if(jj.eq.1) LatestStartTimeIinterval=TravellerTripStartTimeInterval(i,jindex)
                    
                enddo   
            else
                jj=1
                read(10,*) Taz,ActivityTime,trip_intermediate_num,upn,dpn,trip_travel_time
                read(23,*)
                VehicleToTravellertripIndex(vehicleindex,2)=0
                VehicleTaz(vehicleindex,2)=Taz
                VehicleActivityTime(vehicleindex,1)=ActivityTime
                VehicleTIN(vehicleindex,jj)=trip_intermediate_num
                    Vehicleupn(vehicleindex,jj)=upn
                    Vehicledpn(vehicleindex,jj)=dpn
                     VehicleTravelTime(vehicleindex,jj)=trip_travel_time
!                if(Taz.eq.orig_taz_temp.and.ZoneToZoneTravelTime(orig_taz_temp,orig_taz_temp,LatestStartTimeIinterval).eq.0) then
!                    ZoneToZoneTravelTime(orig_taz_temp,orig_taz_temp,LatestStartTimeIinterval)=trip_travel_time
!                endif   
            endif
        endif
   enddo
   close(23)
   !Map the VehicleID and ABM index 
   write(*,*) "Map the VehicleID and ABM index"
   allocate(VehIndextoID(totalnumvehicleandbus,2))
        VehIndextoID(:,:)=0
   error=1
    do while (error.ne.-1)   
        read(45,*,iostat=error) checkcounter,checkcounter1
        VehIndextoID(checkcounter1,1)=checkcounter1
        VehIndextoID(checkcounter1,2)=checkcounter
    enddo
   ! write(*,*) maxval(VehIndextoID(:,1))
    do i=1,maxval(VehIndextoID(:,1))!go over all the vehicles in the vehicletrajectory
        !find the vehicle index=VehIndextoID(i,2)
        !Travellerindex=VehicleToTravellertripIndex(VehIndextoID(i,2),1)
        !tripindex=
        if(VehIndextoID(i,2).gt.0) then
            if(VehicleToTravellertripIndex(VehIndextoID(i,2),1).gt.0) then
                do j=1,VehicleNonrepetitiveCarNumTrip(VehIndextoID(i,2))
                    write(43,*) i,j,ABMTripIndex(VehicleToTravellertripIndex(VehIndextoID(i,2),1),VehicleToTravellertripIndex(VehIndextoID(i,2),j+1))
                enddo
                
            else
                write(43,*) i,1,0
            endif
        else
            write(43,*) i,0,0
        endif
    enddo
   deallocate(VehIndextoID)
!if(IterationID.eq.TargetIterationNumber) then !If it is the last iteration, then report the result back to ABM
    !------------------------------------------------------------------   
    !TDSP Allocation
    write(*,*)"Read TDSP Skim"
    Allocate(TempVOT(6))
    Allocate(TempCost(6))
    Allocate(TempTime(6))
    Allocate(TempDistance(6))
    !open(file='NetworkAtt.dat',unit=25,status='unknown')
    !read(25,*) NumOriginZone, NumSuperZone, NumIntervals

      NumSkimSuperZone=maxnumsuperzone    
    open(file='vehicularPNRSkim//1.dat', unit=31)
    ! Read TDSP
    read(31,*) value1,SPSkimFirstEnter
    if(SPSkimFirstEnter.eq."NTI") then 
        NumIteration=0
        NumSkimIntervals=value1
        read(31,*) NumSkimOriginZone
    else
        NumIteration=1
        NumSkimOriginZone=value1 
        read(31,*) NumSkimIntervals
    endif
    close(31)    

    if (.NOT. ALLOCATED(ODTATT_Array)) then
    Allocate(ODTATT_Array(NumSkimOriginZone,NumSkimSuperZone ,NumSkimIntervals),stat=error)
    endif
    do D=1,NumSkimSuperZone
        write(SkimText,'(i3)') D
        SkimTextFinal='vehicularPNRSkim//'// trim(adjustl(SkimText)) // '.dat'
        open(file=trim(SkimTextFinal),unit=31)    
        read(31,*) 
        read(31,*)     
        if (NumIteration.gt.0) then
          do O=1,NumSkimOriginZone
            do T=1,NumSkimIntervals
                read(31,400) NumOfVOTGs,(TempVOT(nvt),TempCost(nvt),TempTime(nvt),TempDistance(nvt), nvt=1,NumOfVOTGs)
                ODTATT_Array(O,D,T)%Psize=NumOfVOTGs
                call ODTATTAllocate(O,D,T,NumOfVOTGs)
                
                 do i=1,NumOfVOTGs
                       call ODTATTInsert(O,D,T,i,TempVOT(i),TempCost(i),TempTime(i),TempDistance(i))
                 enddo   
!                if(T.eq.1) then 
!                    ODTATT_Array(O,D,T)%Psize=NumOfVOTGs
!                    call ODTATTAllocate(O,D,T,NumOfVOTGs)
!                    !Changed for testing purpose only
!                     do i=1,NumOfVOTGs
!                           call ODTATTInsert(O,D,T,i,TempVOT(i),TempCost(i),TempTime(i),TempDistance(i))
!                     enddo    
!                else
!                    ODTATT_Array(O,D,T)%Psize=ODTATT_Array(O,D,1)%Psize
!                    call ODTATTAllocate(O,D,T,ODTATT_Array(O,D,1)%Psize)
!                    do i=1,ODTATT_Array(O,D,T)%Psize
!                           call ODTATTInsert(O,D,T,i,ODTATT_Array(O,D,1)%P(i)%VOT,ODTATT_Array(O,D,1)%P(i)%Cost,ODTATT_Array(O,D,1)%P(i)%Time,ODTATT_Array(O,D,1)%P(i)%Distance)
!                    enddo  
!                endif 
  
            enddo
          enddo    
        else 
            do T=1,NumSkimIntervals
                do O=1,NumSkimOriginZone
                read(31,400) NumOfVOTGs,(TempVOT(nvt),TempCost(nvt),TempTime(nvt),TempDistance(nvt), nvt=1,NumOfVOTGs)
                ODTATT_Array(O,D,T)%Psize=NumOfVOTGs
                call ODTATTAllocate(O,D,T,NumOfVOTGs)
                do i=1,NumOfVOTGs
                  call ODTATTInsert(O,D,T,i,TempVOT(i),TempCost(i),TempTime(i),TempDistance(i))
                enddo     
                enddo
            enddo
        endif
        close(31)    
    enddo  
    !--------------------------------------------------------------------------------

    !Read the link travel time
    open(file='output_td_linktraveltime.dat',unit=19)
    read(19,*)value
    read(19,*)NumLinkInterval
    read(19,*)SkimHorizon
    read(19,*)value
    read(19,*) value
   ! call timeestimate(ODTATT_Array,LinkTimeInterval,SpSkimTimeInterval)
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
            !Add only for testing purpose
            !LinkTravelTime(j,tinterval)=LinkTravelTime(j,1)
        enddo
        read(19,*)
    enddo
    close(19)
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
            !For testing purpose only
            !TurnPenaltyTime(TurnLinkIndex,k,tinterval)=TurnPenaltyTime(TurnLinkIndex,k,1)
        enddo
        read(20,*)
    enddo
    close(20)
    
    
    !Write the output in the abm format
    write(*,*) "Calculate ABM Travel Time"
     allocate(ABMTravelTime(sum(TravellerNumTrips(:))))
            ABMTravelTime(:)=0
     allocate(ABMTripStartTime(sum(TravellerNumTrips(:))))
            ABMTripStartTime(:)=0
     allocate(ABMTripActivityTime(sum(TravellerNumTrips(:))))
        ABMTripActivityTime(:)=0
     allocate(ABMIncompleteFlag(sum(TravellerNumTrips(:))))
        ABMIncompleteFlag(:)=0
    !indextemp=10000
     
    do i=1,MaxNumTraveller
        do j=1,TravellerNumTrips(i)
          
            !if the trip is intra-superzonal driver trip, read the travel time from the skim
            if (TravellerTripOriginTAZ(i,j).eq.TravellerTripDestinationTAZ(i,j).and.TripDriverPassengerFlag(i,j).gt.0.and.TripTravelTime(i,j).eq.0) then
                !TripTravelTime(i,j)=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),TravellerTripStartTimeInterval(i,j))
                !Determine the SP skim timeinterval
                
                SpSkimTime=ceiling(TripStartTime(i,j)/SpSkimTimeInterval)
                if(SpSkimTime.eq.0) SpSkimTime=1
                if(SpSkimTime.gt.NumSkimIntervals) SpSkimTime=NumSkimIntervals
                !Determine VOT group number
                vv=1
                !do while(average_value_of_time.gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                do while(ValueofTime(Traveller_ID(i)).gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%VOT)
                    vv=vv+1
                enddo
                TripTravelTime(i,j)=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
            endif
            if(TravellerTripJointFlag(i,j).eq.0) then !if this trip is a individual trip
                !if the trip is a non driver car trip
                if((TravellerTripMode(i,j).gt.2.and.TravellerTripMode(i,j).lt.7).and.TripDriverPassengerFlag(i,j).lt.1) then 
                    pathindex=IntraSuperzonePathIndex(TravellerTripOriginTAZ(i,j),TravellerTripDestinationTAZ(i,j))
                    !if no path has been pre stored for the trip
                    if(pathindex.eq.0) then !Read from the vehicularPNRSkim.dat
                        !Determine the SP skim timeinterval
                        SpSkimTime=ceiling(TripStartTime(i,j)/SpSkimTimeInterval)
                        if(SpSkimTime.eq.0) SpSkimTime=1
                        if(SpSkimTime.gt.NumSkimIntervals) SpSkimTime=NumSkimIntervals
                        !Determine VOT group number
                        vv=1
                        !do while(average_value_of_time.gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                        do while(ValueofTime(Traveller_ID(i)).gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%VOT)
                            vv=vv+1
                        enddo
                        ABMTravelTime(ABMTripIndex(i,j))=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                        !Add 5-10 to get the esimated travel time for schedule adjustment
!                        if(TripTravelTime(i,j).eq.0) then 
!                            TripTravelTime(i,j)=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                        endif
                    !else can read from the current     
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
                        
                        ABMTravelTime(ABMTripIndex(i,j))=currentDYNASMARTtime-TripStartTime(i,j)
!                        !Add 5-10 to get the esimated travel time for schedule adjustment
!                        if(TripTravelTime(i,j).eq.0) then 
!                            TripTravelTime(i,j)=currentDYNASMARTtime-TripStartTime(i,j)
!                        endif
                    endif
                else !
                    ABMTravelTime(ABMTripIndex(i,j))=ABMTravelTime(ABMTripIndex(i,j))+TripTravelTime(i,j)
                endif

            else  !If the trip is a joint trip
                if(TravellerTripParkRideFlag(i,j).eq.0) then 
                    if(Traveller_ID(i).eq.TravellerTripDriver(i,j)) then 
                        ABMTravelTime(ABMTripIndex(i,j))=ABMTravelTime(ABMTripIndex(i,j))+TripTravelTime(i,j)
                    endif
                    if(ABMTravelTime(ABMTripIndex(i,j)).eq.0) then
                        if(TravellerTripDriver(i,j).eq.0.and.TravellerTripMode(i,j).lt.7) then 
                            pathindex=IntraSuperzonePathIndex(TravellerTripOriginTAZ(i,j),TravellerTripDestinationTAZ(i,j))
                            if(pathindex.eq.0) then !Read from the vehicularPNRSkim.dat
                                !Determine the SP skim timeinterval
                                SpSkimTime=ceiling(TripStartTime(i,j)/SpSkimTimeInterval)
                                if(SpSkimTime.eq.0) SpSkimTime=1
                                if(SpSkimTime.gt.NumSkimIntervals) SpSkimTime=NumSkimIntervals
                                !Determine VOT group number
                                vv=1
                                !do while(average_value_of_time.gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                                do while(ValueofTime(Traveller_ID(i)).gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%VOT)
                                    vv=vv+1
                                enddo
                                ABMTravelTime(ABMTripIndex(i,j))=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                                !Add 5-10 to get the esimated travel time for schedule adjustment
!                                if(TripTravelTime(i,j).eq.0) then 
!                                    TripTravelTime(i,j)=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                                endif
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
                                
                                ABMTravelTime(ABMTripIndex(i,j))=currentDYNASMARTtime-TripStartTime(i,j)
!                                !Add 5-10 to get the esimated travel time for schedule adjustment
!                                if(TripTravelTime(i,j).eq.0) then 
!                                    TripTravelTime(i,j)=currentDYNASMARTtime-TripStartTime(i,j)
!                                endif
                            endif
                        else
                            ABMTravelTime(ABMTripIndex(i,j))=ABMTravelTime(ABMTripIndex(i,j))+TripTravelTime(i,j)
                        endif
!                        if(ABMTravelTime(ABMTripIndex(i,j)).eq.1440.and.TravellerTripMode(i,j).eq.10) then 
!                            write(1000,*) i,j,Traveller_ID(i)
!                        endif
                    else
!                        !Add 5-10 to get the esimated travel time for schedule adjustment
!                        if(TripTravelTime(i,j).eq.0) then 
!                            TripTravelTime(i,j)=ABMTravelTime(ABMTripIndex(i,j))
!                        endif
                    endif
                elseif(TravellerTripParkRideFlag(i,j).eq.1)  then  !Joint Park Ride 1
                    if(TravellerTripMode(i,j).lt.7) then 
                        if(TravellerTripDriver(i,j).eq.0) then
                            if(ABMTravelTime(ABMTripIndex(i,j)).eq.0) then !If it's 0, read the skim
                                pathindex=IntraSuperzonePathIndex(TravellerTripOriginTAZ(i,j),TravellerTripDestinationTAZ(i,j))
                                if(pathindex.eq.0) then !Read from the vehicularPNRSkim.dat
                                    !Determine the SP skim timeinterval
                                    SpSkimTime=ceiling(TripStartTime(i,j)/SpSkimTimeInterval)
                                    if(SpSkimTime.eq.0) SpSkimTime=1
                                    if(SpSkimTime.gt.NumSkimIntervals) SpSkimTime=NumSkimIntervals
                                    !Determine VOT group number
                                    vv=1
                                    !do while(average_value_of_time.gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                                    do while(ValueofTime(Traveller_ID(i)).gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%VOT)
                                        vv=vv+1
                                    enddo
                                    ABMTravelTime(ABMTripIndex(i,j))=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                                    !Add 5-10 to get the esimated travel time for schedule adjustment
!                                    if(TripTravelTime(i,j).eq.0) then 
!                                        TripTravelTime(i,j)=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                                    endif
                                    
                                    ABMTravelTime(ABMTripIndex(i,j))=ABMTravelTime(ABMTripIndex(i,j))+TripTravelTime(i,j+1)
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
                                    
                                    ABMTravelTime(ABMTripIndex(i,j))=currentDYNASMARTtime-TripStartTime(i,j)
!                                    !Add 5-10 to get the esimated travel time for schedule adjustment
!                                    if(TripTravelTime(i,j).eq.0) then 
!                                        TripTravelTime(i,j)=currentDYNASMARTtime-TripStartTime(i,j)
!                                    endif
                                    
                                    ABMTravelTime(ABMTripIndex(i,j))=ABMTravelTime(ABMTripIndex(i,j))+TripTravelTime(i,j+1)
                                endif
                            else
!                                !Add 5-10 to get the esimated travel time for schedule adjustment
!                                if(TripTravelTime(i,j).eq.0) then 
!                                    TripTravelTime(i,j)=ABMTravelTime(ABMTripIndex(i,j))
!                                endif
                            endif
                        elseif(Traveller_ID(i).eq.TravellerTripDriver(i,j))then
                            ABMTravelTime(ABMTripIndex(i,j))=TripTravelTime(i,j)+TripTravelTime(i,j+1)
                        endif
                    endif
                elseif(TravellerTripParkRideFlag(i,j).eq.2) then   !Joint Park Ride 2
                    if(TravellerTripMode(i,j).lt.7) then 
                        if(TravellerTripDriver(i,j).eq.0) then
                            if(ABMTravelTime(ABMTripIndex(i,j)).eq.0) then !If it's 0, read the skim
                                pathindex=IntraSuperzonePathIndex(TravellerTripOriginTAZ(i,j),TravellerTripDestinationTAZ(i,j))
                                if(pathindex.eq.0) then !Read from the vehicularPNRSkim.dat
                                    !Determine the SP skim timeinterval
                                    SpSkimTime=ceiling(TripStartTime(i,j)/SpSkimTimeInterval)
                                    if(SpSkimTime.eq.0) SpSkimTime=1
                                    if(SpSkimTime.gt.NumSkimIntervals) SpSkimTime=NumSkimIntervals
                                    !Determine VOT group number
                                    vv=1
                                    !do while(average_value_of_time.gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                                    do while(ValueofTime(Traveller_ID(i)).gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%VOT)
                                        vv=vv+1
                                    enddo
                                    ABMTravelTime(ABMTripIndex(i,j))=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                                    !Add 5-10 to get the esimated travel time for schedule adjustment
!                                    if(TripTravelTime(i,j).eq.0) then 
!                                        TripTravelTime(i,j)=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
!                                    endif
                                    ABMTravelTime(ABMTripIndex(i,j))=ABMTravelTime(ABMTripIndex(i,j))+TripTravelTime(i,j-1)
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
                                    
                                    ABMTravelTime(ABMTripIndex(i,j))=currentDYNASMARTtime-TripStartTime(i,j)
!                                    !Add 5-10 to get the esimated travel time for schedule adjustment
!                                    if(TripTravelTime(i,j).eq.0) then 
!                                        TripTravelTime(i,j)=currentDYNASMARTtime-TripStartTime(i,j)
!                                    endif
                                    ABMTravelTime(ABMTripIndex(i,j))=ABMTravelTime(ABMTripIndex(i,j))+TripTravelTime(i,j-1)
                                endif
                            else
!                                !Add 5-10 to get the esimated travel time for schedule adjustment
!                                if(TripTravelTime(i,j).eq.0) then 
!                                    TripTravelTime(i,j)=ABMTravelTime(ABMTripIndex(i,j))
!                                endif
                            endif
                        elseif(Traveller_ID(i).eq.TravellerTripDriver(i,j))then
                            ABMTravelTime(ABMTripIndex(i,j))=TripTravelTime(i,j)+TripTravelTime(i,j-1)
                        endif
                    endif
                endif          
            endif
        enddo
    enddo

    do i=1,MaxNumTraveller
        do j=1,TravellerNumTrips(i)
            if(TravellerTripMode(i,j).eq.7.or.TravellerTripMode(i,j).eq.8.or.TravellerTripMode(i,j).ge.13) then 
                if(j.ne.TravellerNumTrips(i)) then 
                    TripTravelTime(i,j)=TripStartTime(i,j+1)-TripStartTime(i,j)-TravellerActivityTime(i,j)
                else
                    TripTravelTime(i,j)=1440.0-TripStartTime(i,j)-TravellerActivityTime(i,j)
                endif
                if(TripTravelTime(i,j).eq.0) then 
                    TripTravelTime(i,j)=1
                endif
                ABMTravelTime(ABMTripIndex(i,j))=TripTravelTime(i,j)
            endif
!            if(ABMTravelTime(ABMTripIndex(i,j)).eq.0) then 
!                write(99,30) i,j,Traveller_ID(i),ABMTripIndex(i,j),TravellerTripMode(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationTAZ(i,j),SuperZoneMap(TravellerTripOriginTAZ(i,j),2),SuperZoneMap(TravellerTripDestinationTAZ(i,j),2)
!            endif
            !if(TripTravelTime(i,j).eq.0.and.TravellerTripJointFlag(i,j).ne.0) then 
            ABMTravelTime(ABMTripIndex(i,j))=NINT(ABMTravelTime(ABMTripIndex(i,j)))*10.0/10.0
            if(ABMTravelTime(ABMTripIndex(i,j)).eq.0) then 
                ABMTravelTime(ABMTripIndex(i,j))=1
            endif
            
            !if(TravellerTripJointFlag(i,j).ne.0.or.) then 
             TripTravelTime(i,j)=ABMTravelTime(ABMTripIndex(i,j))
             if(TripTravelTime(i,j).ge.1440) then 
                ABMIncompleteFlag(ABMTripIndex(i,j))=1
             endif
             
             if(TripTravelTime(i,j).ge.1440) then
                TravellerIncompleteFlag(i)=1
            endif
            if(TripTravelTime(i,j).eq.0.or.TripTravelTime(i,j).ge.1440) then 
                if(j.ne.TravellerNumTrips(i)) then 
                    TripTravelTime(i,j)=TripStartTime(i,j+1)-TripStartTime(i,j)-TravellerActivityTime(i,j)
                else
                    TripTravelTime(i,j)=1440.0-TripStartTime(i,j)-TravellerActivityTime(i,j)
                endif
                if(TripTravelTime(i,j).eq.0) then 
                    TripTravelTime(i,j)=1
                endif
            endif
            
        enddo
    enddo
!------------------------------------------------------------------   

!if(IterationID.gt.TargetIterationNumber) then !If it is the last iteration, then report the result back to ABM
!
!    read(1,'(a300)') Line
!    TextFinal2=trim(adjustl(Line))//","//'SimulatedTravelTime'
!    write(28,'(a350)')TextFinal2
!    index=0
!    error=1
!    do while (error.ne.-1)   
!        read(1,'(a)',iostat=error) Line
!        if (error.ne.-1) then
!            !read(Line,*,iostat=error) hh_id, person_id, person_num, tour_id, stop_id, inbound, tour_purpose, orig_purpose, dest_purpose, orig_maz, orig_walk_segment, dest_maz, dest_walk_segment, parking_taz, stop_period, trip_mode, tour_mode, tour_category, board_tap, alight_tap, orig_taz, dest_taz, trip_driver_passenger
!            read(Line,*,iostat=error) hh_id
!            if ((deltaU3(HouseHoldCounter).lt.0).or.(stressHHflagVector(HouseHoldCounter).eq.1).or.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then 
!                x=0
!            endif
!            index=index+1
!            write(Text,'(f8.2)') ABMTravelTime(index)
!            TextFinal=trim((trim(adjustl(Line))//","//trim(adjustl(Text))))
!            write(28,'(a150)')TextFinal
!        endif
!    enddo
!
!    read(12,'(a300)') Line       
!    error=1
!    TextFinal2=trim(adjustl(Line))//","//'SimulatedTravelTime'
!    write(29,'(a350)')TextFinal2
!    do while (error.ne.-1)   
!        read(12,'(a)',iostat=error) Line
!        if (error.ne.-1) then
!            !index=index+1
!            index=index+1
!            write(Text,'(f8.2)') ABMTravelTime(index)
!            TextFinal=trim((trim(adjustl(Line))//","//trim(adjustl(Text))))
!            write(29,'(a150)')TextFinal
!        endif
!    enddo
!    
!else !Otherwise aggregate the to write the new traveler.dat 
!    write(21,*) NumTraveller, RealMaxNumTrips
!    write(21,16) 
!    write(21,15) 
    do counter=1,NumTraveller
        i=countertoi(counter)
        TravellerNumTrips(i)=TravellerNumTrips(i)-TravellerParkRideNum(i) !Since each pnr is disaggregated to two trips
        write(21,15) counter,i,Traveller_ID(i),TripStartTime(i,1),TravellerNumTrips(i),TravellerTripOriginTAZ(i,1),ValidCar(i),FirstCarTrip(i),FirstTransitTrip(i),ValueOfTime(Traveller_ID(i))
        
        !pnrcounter=0
        do j=1,TravellerNumTrips(i) !+TravellerParkRideNum(i)
            
            !newj=j-pnrcounter
            if(TravellerTripParkRideFlag(i,j).eq.1.or.TravellerTripParkRideFlag(i,j).eq.2) then 
                if(TravellerTripParkRideFlag(i,j).eq.1) then 
                    TravellerTripMode(i,j)=TravellerTripMode(i,j+1)+2
                else
                    TravellerTripMode(i,j)=TravellerTripMode(i,j)+2
                endif
                
                TravellerTripDestinationTAZ(i,j)=TravellerTripDestinationTAZ(i,j+1)
                TravellerTripDestinationMAZ(i,j)=TravellerTripDestinationMAZ(i,j+1)
                TravellerActivityTime(i,j)=TravellerActivityTime(i,j)+TravellerActivityTime(i,j+1)
                TravellerTripDestinationPurpose(i,j)=TravellerTripDestinationPurpose(i,j+1)
                TripDriverPassengerFlag(i,j)=max(TripDriverPassengerFlag(i,j),TripDriverPassengerFlag(i,j+1))
                TripTravelTime(i,j)=TripTravelTime(i,j)+TripTravelTime(i,j+1)
                !TravellerTripJointFlag
                write(21,16) TravellerTripDestinationTAZ(i,j),TravellerActivityTime(i,j),TravellerTripMode(i,j),TravellerTripOriginPurpose(i,j),TravellerTripDestinationPurpose(i,j),TravellerTripOriginMAZ(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripDestinationTAZ(i,j),TripDriverPassengerFlag(i,j),TravellerTripJointFlag(i,j),TravellerTripParkRideFlag(i,j),TravellerTripStartTimeInterval(i,j),TripStartTime(i,j),ABMTripIndex(i,j),TravellerTripDriver(i,j),TravellerTripDestinationParkingTaz(i,j)
                !write(21,160) TravellerTripDestinationTAZ(i,j),TravellerActivityTime(i,j),TravellerTripOriginPurpose(i,j),TravellerTripDestinationPurpose(i,j),TravellerTripOriginMAZ(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripDestinationTAZ(i,j),TripDriverPassengerFlag(i,j),TravellerTripJointFlag(i,j),TravellerTripParkRideFlag(i,j),TravellerTripStartTimeInterval(i,j),TripStartTime(i,j),ABMTripIndex(i,j),TravellerTripDriver(i,j),TravellerTripDestinationParkingTaz(i,j)

                do jj=j+1,TravellerNumTrips(i)+TravellerParkRideNum(i)               
                    TravellerTripMode(i,jj)=TravellerTripMode(i,jj+1)
                    TravellerTripOriginMAZ(i,jj)=TravellerTripOriginMAZ(i,jj+1)
                    TravellerTripOriginTAZ(i,jj)=TravellerTripOriginTAZ(i,jj+1)
                    !TravellerTripOriginMappedTaz(i,jj)=TAZMap(TravellerTripOriginTAZ(i,jj))
                    TravellerTripDestinationMAZ(i,jj)=TravellerTripDestinationMAZ(i,jj+1)
                    TravellerTripDestinationTAZ(i,jj)=TravellerTripDestinationTAZ(i,jj+1)
                    !TravellerTripDestinationMappedTaz(i,jj)=TAZMap(TravellerTripDestinationTAZ(i,jj))
                    TravellerTripOriginPurpose(i,jj)=TravellerTripOriginPurpose(i,jj+1)
                    TravellerTripDestinationPurpose(i,jj)= TravellerTripDestinationPurpose(i,jj+1)
                    TravellerTripStartTimeInterval(i,jj)=TravellerTripStartTimeInterval(i,jj+1)
                    TripDriverPassengerFlag(i,jj)=TripDriverPassengerFlag(i,jj+1)
                    TravellerTripParkRideFlag(i,jj)=TravellerTripParkRideFlag(i,jj+1)
                    TripStartTime(i,jj)=TripStartTime(i,jj+1)
                    TravellerTripParkRideFlag(i,jj)=TravellerTripParkRideFlag(i,jj+1)
                    TravellerActivityTime(i,jj)=TravellerActivityTime(i,jj+1)
                    TripTravelTime(i,jj)=TripTravelTime(i,jj+1)
                    !Joint
                    TravellerTripDriver(i,jj)=TravellerTripDriver(i,jj+1)
                    TravellerTripTourID(i,jj)=TravellerTripTourID(i,jj+1)
                    TravellerTripJointFlag(i,jj)=TravellerTripJointFlag(i,jj+1)                
                    ABMTripIndex(i,jj)=ABMTripIndex(i,jj+1)
                enddo
               
            elseif (TripDriverPassengerFlag(i,j).eq.1.and.TravellerTripMode(i,j).gt.6) then 
                write(*,*) 
            else
                !just write the information without change it
                write(21,16) TravellerTripDestinationTAZ(i,j),TravellerActivityTime(i,j),TravellerTripMode(i,j),TravellerTripOriginPurpose(i,j),TravellerTripDestinationPurpose(i,j),TravellerTripOriginMAZ(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripDestinationTAZ(i,j),TripDriverPassengerFlag(i,j),TravellerTripJointFlag(i,j),TravellerTripParkRideFlag(i,j),TravellerTripStartTimeInterval(i,j),TripStartTime(i,j),ABMTripIndex(i,j),TravellerTripDriver(i,j),TravellerTripDestinationParkingTaz(i,j)
                !write(21,160) TravellerTripDestinationTAZ(i,j),TravellerActivityTime(i,j),TravellerTripOriginPurpose(i,j),TravellerTripDestinationPurpose(i,j),TravellerTripOriginMAZ(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripDestinationTAZ(i,j),TripDriverPassengerFlag(i,j),TravellerTripJointFlag(i,j),TravellerTripParkRideFlag(i,j),TravellerTripStartTimeInterval(i,j),TripStartTime(i,j),ABMTripIndex(i,j),TravellerTripDriver(i,j),TravellerTripDestinationParkingTaz(i,j)
            endif
        enddo
    enddo
    !Get household information
    write(*,*) "Read Household information"
    !-------------------------------------------------------------------       	    
!Creating mapping for PersonIDOfHH_PN
!Read the value of time
    
!    1	Full-time worker
!    2	Part-time worker
!    3	Non-worker
!    4	Retired
!    5	University Student
!    6	Student of driving age
!    7	Student of non-driving age
!    8	Child too young for school

    error=1
    open(file='personData_1.csv',unit=21)
    read(21,*,iostat=error)
    zzz=0
    hh_idtemp=0
    Allocate(HouseHold_ID(NumHouseHold ))
        HouseHold_ID(:)=0
    Allocate(HHIDofPerson(MaxPersonID)) 
        HHIDofPerson(:)=0
    Allocate(HouseHoldIDtoCounter(MaxHouseHoldID))
        HouseHoldIDtoCounter(:)=0
       
    if (error.ne.-1) then      
        do while (error.ne.-1)   
            read(21,'(a)',iostat=error) Line
            if (error.ne.-1) then
                read(Line,*,iostat=error) hh_id,person_id,person_num,age,gender,type1,value_of_time,fp_choice,activity_pattern,imf_choice,inmf_choice,walk_time_weight,walk_speed,max_walk,user_class_work_walk,user_class_work_pnr,user_class_work_knr,user_class_non_work_walk,user_class_non_work_pnr,user_class_non_work_knr
                PersonIDOfHH_PN(hh_id,person_num)=person_id
                HHIDofPerson(person_id)=hh_id
                ValueOfTime(person_id)=nint(value_of_time/60*10000.0)/10000.0  !May need to fix in the future, the index
               
                if(hh_id.ne.hh_idtemp) then 
                    zzz=zzz+1
                    HouseHold_ID(zzz)=hh_id
                    HouseHoldIDtoCounter(hh_id)=zzz
                    hh_idtemp=hh_id
                endif
            endif
        enddo
    endif
    close(21)
!-------------------------------------------------- 
    open(file='jointTourData_1.csv',unit=34) 
    read(34,*)
    error=1
    allocate(HHnumJointTrips(MaxHouseHoldID))
        HHnumJointTrips(:)=0
    allocate(HHnumJointTour(MaxHouseHoldID))
        HHnumJointTour(:)=0
        
    do while (error.ne.-1)
        read(34,'(a)',iostat=error) JointTourLine
        if (error.ne.-1) then
            read(JointTourLine,'(500a1)',iostat=error) JointTourCharacter(:) ! reading the whole line character by character
            
            if (error.eq.-1) then
                write(3,*) 'Error in reading Joint Tour file charcter by character at ... ', JointTourLine
                pause
            endif      
            i=1
            SpaceCount=0
            do j=1,6 !finding the sixth input for each tour (tour participant block)
                do while(JointTourCharacter(i).ne.',')
                    if (JointTourCharacter(i).eq.' ') then
                         if (j.ne.6) then
                            write(3,*) 'There is an extra space in the joint tour file at ... ', JointTourLine
                            pause
                        endif
                        SpaceCount=SpaceCount+1 !counting number of spaces at the sixth input of the whole line which are separators of the tour participants, to calculate number of participants
                    endif
                    i=i+1
                enddo
                i=i+1          
            enddo
            allocate (tour_participants(SpaceCount+1))
            !After knowing the number of tour participants we can  read the tour information, as following:
            read (JointTourLine,*,iostat=error) hh_id,tour_id,tour_category,tour_purpose,tour_composition,tour_participants(:),orig_maz,orig_walk_segment,dest_maz,dest_walk_segment,depart_period,arrive_period,tour_mode,num_ob_stops,num_ib_stops,util(:),prob(:)
            HHnumJointTour(hh_id)=HHnumJointTour(hh_id)+1
            if (error.eq.-1) then
                write(3,*) 'Error in reading Joint Tour Line because of the wrong calculated format at ... ', JointTourLine
                pause
            endif      
            NumTourParticipant(hh_id,tour_id+1)=SpaceCount+1
            if (NumTourParticipant(hh_id,tour_id+1).gt.MaxHHSize) then
                write(3,*) 'Number of maximum household members is not large enough, MaxHHSize is ', MaxHHSize,' and there is a household with ', NumTourParticipant(hh_id,tour_id+1)
                pause
            endif
            do k=1,NumTourParticipant(hh_id,tour_id+1)
                PersonNumOfHH_TourID(hh_id,tour_id+1,k)=tour_participants(k) 
            enddo              
            deallocate (tour_participants)
        endif
    enddo
    close(34)
    !-------------------------------
    !read the joint trip information
    
     read(12,*)       
    error=1
    !NonExistingTraveller=0 !number of travellers who do not have any individual trip 
    !NoNTravelerDriver=0
    j=1
    hh_idtemp=0
    allocate(HHJointTripStarTimeList(MaxHouseHoldID,MaxNumTrips))
        HHJointTripStarTimeList(:,:)=0
    Linecounter=0
    do while (error.ne.-1)
        read(12,'(a)',iostat=error) Line
        if (error.ne.-1) then
            !driver_id=0
            read(Line,*,iostat=error) hh_id, tour_id, stop_id, inbound, tour_purpose, orig_purpose, dest_purpose, orig_maz, orig_walk_segment, orig_park_maz,dest_maz, dest_walk_segment,parking_taz, stop_period, trip_mode, num_participants, tour_mode, tour_category, board_tap, alight_tap, orig_taz, dest_taz,orig_park_taz,dest_park_taz, trip_driver_passenger
            Linecounter=Linecounter+1
            HHnumJointTrips(hh_id)=HHnumJointTrips(hh_id)+1
            if(hh_id.eq.hh_idtemp) then 
                j=j+1
            else
                !if enter a new household, sort the trips of last household
                j=1
                hh_idtemp=hh_id
            endif
            
            NumTripParticipant(hh_id,j)=NumTourParticipant(hh_id,tour_id+1)
            do k=1,NumTourParticipant(hh_id,tour_id+1)
                PersonNumOfHH_TripID(hh_id,j,k)=PersonNumOfHH_TourID(hh_id,tour_id+1,k)
            enddo  
            !Get the trip start time
            TravellerIndex=Traveller_Index(PersonIDOfHH_PN(hh_id,PersonNumOfHH_TourID(hh_id,tour_id+1,1)))
           
            if(TravellerIndex.ne.0) then 
                do i=1,TravellerNumTrips(TravellerIndex)
                    if(ABMTripIndex(TravellerIndex,i).eq.Linecounter+NumIndivTrips)then 
                        HHJointTripStarTimeList(hh_id,j)=TripStartTime(TravellerIndex,i)
                    endif
                enddo
!                lsup=j
!                do while(lsup.gt.1) 
!                    bubble=0
!                    do jj=1,(lsup-1)
!                        if(HHJointTripStarTimeList(hh_id,jj).gt.HHJointTripStarTimeList(hh_id,jj+1))then 
!                            temp=HHJointTripStarTimeList(hh_id,jj)
!                            HHJointTripStarTimeList(hh_id,jj)=HHJointTripStarTimeList(hh_id,jj+1)
!                            HHJointTripStarTimeList(hh_id,jj+1)=temp
!                            bubble=jj
!                        endif
!                    enddo
!                    lsup=bubble
!                enddo
           endif
        endif
    enddo  
    close(12)
    
    !--------------------
    
    !Call the dll
    write(*,*) "Schedule Adjustment"
    call connect_java( &
		C_CHAR_"./software;&
			./software/timeAdjust.jar;&
			./software/log4j-1.2.9.jar;&
			./software/jxl.jar;&
			./software/com.google.ortools.linearsolver.jar"//C_NULL_CHAR, &
		C_CHAR_"./software"//C_NULL_CHAR, &
		C_CHAR_"./dllSource/Schedule_Adjustment_LP_Parameters.xls"//C_NULL_CHAR )
    !--------------------------------------------
    !calculate the stressed household beta
    naPcounter=0
    naHHcounter=0
    stressPcounter=0
    stressHHcounter=0
    if(IterationID.eq.1) then 
        open(file='DLLResultReport.dat',unit=40)
        write(40,*) 'HouseHold Information Summary'
        !open(file='deltaU.dat',unit=41)
        !write(41,*) IterationID,'Before DLL'
        open(file='dllparameterresult.dat',unit=100)
       ! write(100,*) IterationID,"TripInformation Before Dll"
       ! write(40,*) IterationID
    else  
        open(unit=40,file='DLLResultReport.dat',Access = 'append',Status='old')
        !open(unit=41,file='deltaU.dat',Access = 'append',Status='old')
        !open(unit=100,file='dllparameterresult.dat',access='append',status='old')
        !write(41,*) IterationID,'Before DLL'
       ! write(100,*) IterationID,"TripInformation Before Dll"
        !write(40,*) IterationID
    endif
    allocate(adjustFlag(NumHouseHold))
        adjustFlag(:)=0
    allocate(DllHHflag(NumHouseHold))
        DllHHflag(:)=0
    allocate(deltaU1(NumHouseHold))
        deltaU1(:)=0
    allocate(deltaU2(NumHouseHold))
        deltaU2(:)=0
    allocate(deltaU3(NumHouseHold))
        deltaU3(:)=0
    allocate(deltaU4(NumHouseHold))
        deltaU4(:)=0
    allocate(totalnactivity(NumHouseHold))
        totalnactivity(:)=0
    allocate(totalactivity(NumHouseHOld))
        totalactivity(:)=0
    allocate(HHIncompleteFlag(NumHouseHold))
        HHIncompleteFlag(:)=0
    allocate(HHNumCarTrips(NumHouseHold))
        HHNumCarTrips(:)=0
    allocate(HHNumTransitTrips(NumHouseHold))
        HHNumTransitTrips(:)=0
    allocate(HHNumPNRTrips(NumHouseHold))
        HHNumPNRTrips(:)=0   
    allocate(HHNumNeATrips(NumHouseHold))
        HHNumNeATrips(:)=0    
    allocate(HHNumNhNeATrips(NumHouseHold))
        HHNumNhNeATrips(:)=0    
    allocate(stressHHflagVector(NumHouseHold))
        stressHHflagVector(:)=0     
    allocate(TripExpDeparture(MaxNumTraveller,MaxNumTrips))
        TripExpDeparture(:,:)=0
    allocate(TripExpArrival(MaxNumTraveller,MaxNumTrips))
        TripExpArrival(:,:)=0
    allocate(TripExpActivityDuration(MaxNumTraveller,MaxNumTrips))
        TripExpActivityDuration(:,:)=0
    allocate(DurShort1(NumPurposeType,NumPersonType))
        DurShort1(:,:)=0
    allocate(DurShort2(NumPurposeType,NumPersonType))
        DurShort2(:,:)=0
    allocate(DurLong1(NumPurposeType,NumPersonType))
        DurLong1(:,:)=0
    allocate(DurLong2(NumPurposeType,NumPersonType))
        DurLong2(:,:)=0
    allocate(DepEarly1(NumPurposeType,NumPersonType))
        DepEarly1(:,:)=0
    allocate(DepEarly2(NumPurposeType,NumPersonType))
        DepEarly2(:,:)=0
    allocate(DepEarly(NumPurposeType,NumPersonType))
        DepEarly(:,:)=0
    allocate(DepLate1(NumPurposeType,NumPersonType))
        DepLate1(:,:)=0
    allocate(DepLate2(NumPurposeType,NumPersonType))
        DepLate2(:,:)=0
    allocate(DepLate(NumPurposeType,NumPersonType))
        DepLate(:,:)=0
    allocate(ArrEarly1(NumPurposeType,NumPersonType))
        ArrEarly1(:,:)=0
    allocate(ArrEarly2(NumPurposeType,NumPersonType))
        ArrEarly2(:,:)=0
    allocate(ArrEarly(NumPurposeType,NumPersonType))
        ArrEarly(:,:)=0
    allocate(ArrLate1(NumPurposeType,NumPersonType))
        ArrLate1(:,:)=0
    allocate(ArrLate2(NumPurposeType,NumPersonType))
        ArrLate2(:,:)=0
    allocate(ArrLate(NumPurposeType,NumPersonType))
        ArrLate(:,:)=0
    !allocate threshold 
    allocate(DurShort2Thre(NumPurposeType,NumPersonType))
       DurShort2Thre(:,:)=0
    allocate(DurShort3Thre(NumPurposeType,NumPersonType))
       DurShort3Thre(:,:)=0
    allocate(DurShort4Thre(NumPurposeType,NumPersonType))
        DurShort4Thre(:,:)=0
    allocate(DurLong2Thre(NumPurposeType,NumPersonType))
        DurLong2Thre(:,:)=0
    allocate(DurLong3Thre(NumPurposeType,NumPersonType))
        DurLong3Thre(:,:)=0
    allocate(DepEarly2Thre(NumPurposeType,NumPersonType))
        DepEarly2Thre(:,:)=0
    allocate(DepLate2Thre(NumPurposeType,NumPersonType))
        DepLate2Thre(:,:)=0
    allocate(ArrEarly2Thre(NumPurposeType,NumPersonType))
        ArrEarly2Thre(:,:)=0
    allocate(ArrLate2Thre(NumPurposeType,NumPersonType))
        ArrLate2Thre(:,:)=0
        
    open(file='objfunction.dat',unit=39) 
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DurShort1(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DurShort2(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DurLong1(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DurLong2(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DepEarly1(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DepEarly2(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DepEarly(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DepLate1(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DepLate2(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) DepLate(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) ArrEarly1(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) ArrEarly2(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) ArrEarly(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) ArrLate1(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) ArrLate2(i,1:NumPersonType)
    enddo
    read(39,*)
    do i=1,NumPurposeType
        read(39,*) ArrLate(i,1:NumPersonType)
    enddo       
     !Read the threshold
    open(file='objfunctionthre.dat',unit=42)  
    
    
    read(42,*)
    do i=1,NumPurposeType
        read(42,*) DurShort2Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*) 
    do i=1,NumPurposeType
        read(42,*) DurShort3Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*) 
    do i=1,NumPurposeType
        read(42,*) DurShort4Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*) 
    do i=1,NumPurposeType
        read(42,*) DurLong2Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*) 
    do i=1,NumPurposeType
        read(42,*) DurLong3Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*)
    do i=1,NumPurposeType
        read(42,*) DepEarly2Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*)
    do i=1,NumPurposeType
        read(42,*) DepLate2Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*)
    do i=1,NumPurposeType
        read(42,*) ArrEarly2Thre(i,1:NumPersonType)
    enddo 
    
    read(42,*)
    do i=1,NumPurposeType
        read(42,*) ArrLate2Thre(i,1:NumPersonType)
    enddo    
        
  !Stress check and N Activity Time check
  
  TotalMeta=0
  !open(file='dlltravellertripresult.dat',unit=1000) 
  do   HouseHoldCounter=1,NumHouseHold !Go through all the household
  !do householdcounter=599022,599022
        StressHHFlag=0
        naHHFlag=0
        do m=1,MaxHHSize !for each household go through all the travelers
            MTT=0
            !Ali
            MTT_Trn=0
            MTT_PNR=0
            MTT_Car=0
            !Ali end
            MTA=0
            META=0
            naPflag=0
            StressPFlag=0           
            deltaU1temp=0
            deltaU2temp=0
            deltaU3temp=0
            deltaU4temp=0
            if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).gt.0) then
                i=Traveller_Index(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m))
                
                if(i.ne.0) then 
                    if(TravellerIncompleteFlag(i).eq.1) HHIncompleteFlag(HouseHoldCounter)=1
                    do tripcounter=1,TravellerNumTrips(i)
                        !Calculate the trip experienced arrival/departure time
                        if(tripcounter.eq.1) then 
                            TripExpDeparture(i,1)=TripStartTime(i,tripcounter)
                            TripExpArrival(i,1)=min(TripExpDeparture(i,1)+TripTravelTime(i,1),1440.0)
                          
                        else
                            TripExpDeparture(i,tripcounter)=min(TripExpDeparture(i,tripcounter-1)+TripTravelTime(i,tripcounter-1)+TravellerActivityTime(i,tripcounter-1),1440.0)
                            TripExpArrival(i,tripcounter)=min(TripExpDeparture(i,tripcounter)+TripTravelTime(i,tripcounter),1440.0) 
                        endif
                        if(TripExpArrival(i,tripcounter).eq.1440.and.TripExpDeparture(i,tripcounter).ne.1440) then 
                            TripTravelTime(i,tripcounter)=1440-TripExpDeparture(i,tripcounter)
                        endif
                    enddo
                    do tripcounter=1,TravellerNumTrips(i)
                        !Calculate the arrival/departure penalty objective function
                        if(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'Home') then 
                            dstActTypesTemp=0
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'work'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'Work') then 
                            dstActTypesTemp=1
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'Shop'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'shop') then
                            dstActTypesTemp=5
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'Escort'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'escort') then
                            dstActTypesTemp=4
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'work-based') then
                            dstActTypesTemp=1
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'Discretionary'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'discretionary') then 
                            dstActTypesTemp=9
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'EatingOut'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'eatingout') then
                            dstActTypesTemp=7
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'work-related') then
                            dstActTypesTemp=1
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'maintenance'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'Maintenance') then
                            dstActTypesTemp=6
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'school'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'School') then
                            dstActTypesTemp=3
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'Visiting'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'visiting') then
                            dstActTypesTemp=8
                        elseif(trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'university'.or.trim(TravellerTripDestinationPurpose(i,tripcounter)).eq.'University') then
                            dstActTypesTemp=2
                        endif
                        dstActTypesTemp=dstActTypesTemp+1
                        
                        if(tripcounter.ne.TravellerNumTrips(i)) then 
                             TripExpActivityDuration(i,tripcounter)=max(TripExpDeparture(i,tripcounter+1)-TripExpArrival(i,tripcounter),0.0)
                        else
                             TripExpActivityDuration(i,tripcounter)=max(1440-TripExpArrival(i,tripcounter),0.0)
                        endif
                       
                        !calculate the penalty
                         if(tripcounter.ne.TravellerNumTrips(i)) then
                            !Penalty of arrival
                            if(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter)).eq.0) then 
                                deltaU4temp=deltaU4temp
                            elseif(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter)).lt.0) then !early arrival
                                !fix penalty
                                deltaU4temp=deltaU4temp+ArrEarly(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                !variable penalty
                                deltaU4temp=deltaU4temp+ArrEarly1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(-(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter))))
                                !extra penalty for above the threshold
                                if(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter)).lt.(-1*ArrEarly2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))) then
                                    deltaU4temp=deltaU4temp+ArrEarly2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(abs(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter)))-ArrEarly2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            else
                                !late arrival
                                deltaU4temp=deltaU4temp+ArrLate(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                deltaU4temp=deltaU4temp+ArrLate1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter)))
                                !extra penalty for above the threshold
                                if(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter)).gt.ArrLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))) then
                                    deltaU4temp=deltaU4temp+ArrLate2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(abs(TripExpArrival(i,tripcounter)-(TripStartTime(i,tripcounter+1)-TravellerActivityTime(i,tripcounter)))-ArrLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            endif
                            !penalty of departure
                            if(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter).eq.0) then
                                deltaU4temp=deltaU4temp
                            elseif(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter).lt.0) then !early departure
                                !fix penalty
                                deltaU4temp=deltaU4temp+DepEarly(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                !early departure
                                deltaU4temp=deltaU4temp+DepEarly1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripStartTime(i,tripcounter)-TripExpDeparture(i,tripcounter))
                                !extra penalty for above the threshold
                                if((TripStartTime(i,tripcounter)-TripExpDeparture(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))).gt.0) then
                                    deltaU4temp=deltaU4temp+DepEarly2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripStartTime(i,tripcounter)-TripExpDeparture(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            else !late departure
                                !Fix late departure penalty
                                deltaU4temp=deltaU4temp+DepLate(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                !variable
                                deltaU4temp=deltaU4temp+DepLate1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter))
                                !extra penalty for above the threshold
                                if((TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))).gt.0) then
                                    deltaU4temp=deltaU4temp+DepLate2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            endif
                        else !When it is the last trip
                            !Penalty of arrival
                            if(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter)).eq.0) then 
                                deltaU4temp=deltaU4temp
                            elseif(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter)).lt.0) then !early arrival
                                !fix penalty
                                deltaU4temp=deltaU4temp+ArrEarly(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                !variable penalty
                                deltaU4temp=deltaU4temp+ArrEarly1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(-(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter))))
                                !extra penalty for above the threshold
                                if(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter)).lt.(-1*ArrEarly2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))) then
                                    deltaU4temp=deltaU4temp+ArrEarly2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(abs(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter)))-ArrEarly2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            else
                                !late arrival
                                deltaU4temp=deltaU4temp+ArrLate(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                deltaU4temp=deltaU4temp+ArrLate1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter)))
                                !extra penalty for above the threshold
                                if(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter)).gt.ArrLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))) then
                                    deltaU4temp=deltaU4temp+ArrLate2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(abs(TripExpArrival(i,tripcounter)-(planninghorizon-TravellerActivityTime(i,tripcounter)))-ArrLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            endif
                            !penalty of departure
                            if(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter).eq.0) then
                                deltaU4temp=deltaU4temp
                            elseif(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter).lt.0) then !early departure
                                !fix penalty
                                deltaU4temp=deltaU4temp+DepEarly(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                !early departure
                                deltaU4temp=deltaU4temp+DepEarly1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripStartTime(i,tripcounter)-TripExpDeparture(i,tripcounter))
                                !extra penalty for above the threshold
                                if((TripStartTime(i,tripcounter)-TripExpDeparture(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))).gt.0) then
                                    deltaU4temp=deltaU4temp+DepEarly2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripStartTime(i,tripcounter)-TripExpDeparture(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            else !late departure
                                !Fix late departure penalty
                                deltaU4temp=deltaU4temp+DepLate(dstActTypesTemp,TravellerType(Traveller_ID(i)))
                                !variable
                                deltaU4temp=deltaU4temp+DepLate1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter))
                                !extra penalty for above the threshold
                                if((TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))).gt.0) then
                                    deltaU4temp=deltaU4temp+DepLate2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(TripExpDeparture(i,tripcounter)-TripStartTime(i,tripcounter)-DepLate2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i))))
                                endif
                            endif
                            
                        endif
                        !penalty of duration 
                        if(TripExpActivityDuration(i,tripcounter)-TravellerActivityTime(i,tripcounter).lt.0) then !Shorten duration
                            deltaU4temp=deltaU4temp+DurShort1(dstActTypesTemp,TravellerType(Traveller_ID(i)))*abs(TripExpActivityDuration(i,tripcounter)-TravellerActivityTime(i,tripcounter)) !penalty for 1 min of shortening activity duration
                            if(TripExpActivityDuration(i,tripcounter)*DurShort2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))-TravellerActivityTime(i,tripcounter).gt.0) then 
                                deltaU4temp=deltaU4temp+DurShort2(dstActTypesTemp,TravellerType(Traveller_ID(i)))*(abs(TripExpActivityDuration(i,tripcounter)*DurShort2Thre(dstActTypesTemp,TravellerType(Traveller_ID(i)))-TravellerActivityTime(i,tripcounter)))
                            endif
                        elseif(TripExpActivityDuration(i,tripcounter)-TravellerActivityTime(i,tripcounter).gt.0) then !longer duration
                        endif
                        
                        
                        !Stress calculation and the negative activity time
                        if(tripcounter.ne.TravellerNumTrips(i)) then 
                            if(TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                naPflag=1
                                naHHFlag=1
                                !write(40,*) HouseHoldCounter,i,tripcounter," N Atime before"
                            endif
                            if(TripTravelTime(i,tripcounter).gt.0.and.TripTravelTime(i,tripcounter).lt.1440) then 
                                MTT=MTT+TripTravelTime(i,tripcounter)
                                !Ali                                    
                                if(TravellerTripMode(i,tripcounter).le.6)then 
                                    MTT_Car=MTT_Car+TripTravelTime(i,tripcounter)
                                elseif(TravellerTripMode(i,tripcounter).eq.9.or.TravellerTripMode(i,tripcounter).eq.10) then 
                                    MTT_Trn=MTT_Trn+TripTravelTime(i,tripcounter)
                                elseif(TravellerTripMode(i,tripcounter).eq.11.or.TravellerTripMode(i,tripcounter).eq.12) then 
                                    MTT_PNR=MTT_PNR+TripTravelTime(i,tripcounter)
                                endif
                                !Ali end                              
                            else
                                if(TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter).gt.0) then !The planned time has to be larger than zero, otherwise in the nextiteration, the non-zero expected time will cause inconsistency 
                                    MTT=MTT+TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    !Ali                                    
                                    if(TravellerTripMode(i,tripcounter).le.6)then 
                                        MTT_Car=MTT_Car+TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    elseif(TravellerTripMode(i,tripcounter).eq.9.or.TravellerTripMode(i,tripcounter).eq.10) then 
                                        MTT_Trn=MTT_Trn+TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    elseif(TravellerTripMode(i,tripcounter).eq.11.or.TravellerTripMode(i,tripcounter).eq.12) then 
                                        MTT_PNR=MTT_PNR+TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    endif
                                    !Ali end            
                                else
                                    MTT=MTT+1
                                    !Ali                                    
                                    if(TravellerTripMode(i,tripcounter).le.6)then 
                                        MTT_Car=MTT_Car+1
                                    elseif(TravellerTripMode(i,tripcounter).eq.9.or.TravellerTripMode(i,tripcounter).eq.10) then 
                                        MTT_Trn=MTT_Trn+1
                                    elseif(TravellerTripMode(i,tripcounter).eq.11.or.TravellerTripMode(i,tripcounter).eq.12) then 
                                        MTT_PNR=MTT_PNR+1
                                    endif
                                    !Ali end                                     
                                endif
                            endif
                            if(TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                HHNumNeATrips(HouseHoldCounter)=HHNumNeATrips(HouseHoldCounter)+1
                            endif
                        else
                            if(1440.0-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                naPflag=1
                                naHHFlag=1
                            endif
                            if(TripTravelTime(i,tripcounter).gt.0.and.TripTravelTime(i,tripcounter).lt.1440) then 
                                MTT=MTT+TripTravelTime(i,tripcounter)
                                !Ali                                    
                                if(TravellerTripMode(i,tripcounter).le.6)then 
                                    MTT_Car=MTT_Car+TripTravelTime(i,tripcounter)
                                elseif(TravellerTripMode(i,tripcounter).eq.9.or.TravellerTripMode(i,tripcounter).eq.10) then 
                                    MTT_Trn=MTT_Trn+TripTravelTime(i,tripcounter)
                                elseif(TravellerTripMode(i,tripcounter).eq.11.or.TravellerTripMode(i,tripcounter).eq.12) then 
                                    MTT_PNR=MTT_PNR+TripTravelTime(i,tripcounter)
                                endif
                                !Ali end                             
                            else
                                if(1440.0-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter).gt.0) then 
                                    MTT=MTT+1440.0-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    !Ali                                    
                                    if(TravellerTripMode(i,tripcounter).le.6)then 
                                        MTT_Car=MTT_Car+1440.0-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    elseif(TravellerTripMode(i,tripcounter).eq.9.or.TravellerTripMode(i,tripcounter).eq.10) then 
                                        MTT_Trn=MTT_Trn+1440.0-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    elseif(TravellerTripMode(i,tripcounter).eq.11.or.TravellerTripMode(i,tripcounter).eq.12) then 
                                        MTT_PNR=MTT_PNR+1440.0-TripStartTime(i,tripcounter)-TravellerActivityTime(i,tripcounter)
                                    endif
                                    !Ali end                                     
                                else
                                    MTT=MTT+1
                                    !Ali                                    
                                    if(TravellerTripMode(i,tripcounter).le.6)then 
                                        MTT_Car=MTT_Car+1
                                    elseif(TravellerTripMode(i,tripcounter).eq.9.or.TravellerTripMode(i,tripcounter).eq.10) then 
                                        MTT_Trn=MTT_Trn+1
                                    elseif(TravellerTripMode(i,tripcounter).eq.11.or.TravellerTripMode(i,tripcounter).eq.12) then 
                                        MTT_PNR=MTT_PNR+1
                                    endif
                                    !Ali end                                     
                                endif
                            endif
                            if(1440.0-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                HHNumNeATrips(HouseHoldCounter)=HHNumNeATrips(HouseHoldCounter)+1
                            endif
                         
                        endif
                        if(trim(TravellerTripDestinationPurpose(i,tripcounter)).ne.'Home'.and.trim(TravellerTripDestinationPurpose(i,tripcounter)).ne.'home') then 
                            MTA=MTA+TravellerActivityTime(i,tripcounter)
                            if(tripcounter.ne.TravellerNumTrips(i)) then
                                if(TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                    !meta=meta+TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter)
                                    HHNumNhNeATrips(HouseHoldCounter)=HHNumNhNeATrips(HouseHoldCounter)+1
                                endif
                            else
                                if(1440-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                    !meta=meta+1440-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter)
                                    HHNumNhNeATrips(HouseHoldCounter)=HHNumNhNeATrips(HouseHoldCounter)+1
                                endif
                            endif
                        endif
                        if(tripcounter.ne.TravellerNumTrips(i)) then
                            if(TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                meta=meta+TripStartTime(i,tripcounter+1)-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter)
                                !HHNumNhNeATrips(HouseHoldCounter)=HHNumNhNeATrips(HouseHoldCounter)+1
                            endif
                        else
                            if(1440-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter).lt.0) then 
                                meta=meta+1440-TripStartTime(i,tripcounter)-TripTravelTime(i,tripcounter)
                                !HHNumNhNeATrips(HouseHoldCounter)=HHNumNhNeATrips(HouseHoldCounter)+1
                            endif
                        endif
                        if(meta.gt.0) then 
                            write(*,*)
                        endif
                        
                        !write(1000,*)Traveller_id(i),tripcounter, MTT
                        if(TravellerTripMode(i,tripcounter).le.6)then 
                            HHNumCarTrips(HouseHoldCounter)=HHNumCarTrips(HouseHoldCounter)+1
                        elseif(TravellerTripMode(i,tripcounter).eq.9.or.TravellerTripMode(i,tripcounter).eq.10) then 
                            HHNumTransitTrips(HouseHoldCounter)=HHNumTransitTrips(HouseHoldCounter)+1
                        elseif(TravellerTripMode(i,tripcounter).eq.11.or.TravellerTripMode(i,tripcounter).eq.12) then 
                            HHNumPNRTrips(HouseHoldCounter)=HHNumPNRTrips(HouseHoldCounter)+1
                        endif
!                        if(tripcounter.ne.TravellerNumTrips(i)) then
!                            write(100,29)HouseHoldCounter,i,tripcounter,MTT,MTA,META,TripStartTime(i,tripcounter),TripStartTime(i,tripcounter+1),TripTravelTime(i,tripcounter),TravellerActivityTime(i,tripcounter)
!                        else
!                            write(100,29)HouseHoldCounter,i,tripcounter,MTT,MTA,META,TripStartTime(i,tripcounter),1440.0,TripTravelTime(i,tripcounter),TravellerActivityTime(i,tripcounter)
!                        endif
                    enddo

                    if(MTA.ne.0) then 
                        TTO=MTT/MTA
                        if(MTT.gt.StressMTT(TravellerType(Traveller_ID(i))).or.(TTO.gt.StressTTO(TravellerType(Traveller_ID(i))).and.MTA.gt.StressMTA(TravellerType(Traveller_ID(i))))) then 
                            !Ali
!                            if(MTT.gt.StressMTT(TravellerType(Traveller_ID(i)))) then 
!                                write(1364,*) 'MTT',HouseHoldCounter,i,tripcounter-1,MTT,MTA,MTT_Trn,MTT_PNR,MTT_Car
!                            endif
!                            if(TTO.gt.StressTTO(TravellerType(Traveller_ID(i))).and.MTA.gt.StressMTA(TravellerType(Traveller_ID(i)))) then 
!                                write(1364,*) 'TTO',HouseHoldCounter,i,tripcounter-1,MTT,MTA,MTT_Trn,MTT_PNR,MTT_Car
!                            endif                                 
                            !Ali end
                            StressPflag=1
                            StressHHFlag=1
                            !DllHHflag(HouseHoldCounter)=1
                        !Ali
                        else                    
                            !write(1364,*) 'NTS',HouseHoldCounter,i,tripcounter-1,MTT,MTA,MTT_Trn,MTT_PNR,MTT_Car
                        !Ali end
                        endif
                        deltaU1temp=(MTT-StressMTT(TravellerType(Traveller_ID(i))))/StressMTT(TravellerType(Traveller_ID(i)))
                        
                        if(TTO.gt.StressTTO(TravellerType(Traveller_ID(i)))) then 
                            if(MTA.gt.StressMTA(TravellerType(Traveller_ID(i)))) then 
                                deltaU2temp=(TTO-StressTTO(TravellerType(Traveller_ID(i))))/StressTTO(TravellerType(Traveller_ID(i)))
                            endif
                        else
                            deltaU2temp=(TTO-StressTTO(TravellerType(Traveller_ID(i))))/StressTTO(TravellerType(Traveller_ID(i)))
                        endif
                        deltaU3temp=META/MTA
                    else 
                        if(MTT.gt.StressMTT(TravellerType(Traveller_ID(i)))) then 
                            StressPflag=1
                            StressHHFlag=1
                            !DllHHflag(HouseHoldCounter)=1
                        endif
                        deltaU1temp=(MTT-StressMTT(TravellerType(Traveller_ID(i))))/StressMTT(TravellerType(Traveller_ID(i)))
                        !write(3,*) 'Traveller', i,'has zero activity time'
                        deltau2temp=0
                        deltaU3temp=0
                    endif
                    
                    if(naPflag.eq.1) then 
                        naPcounter=naPcounter+1
                    endif
                    if(StressPFlag.eq.1) then 
                        StressPcounter=StressPcounter+1
                    endif
                endif
            endif
            deltaU1(HouseHoldCounter)=deltaU1(HouseHoldCounter)+deltaU1temp
            deltaU2(HouseHoldCounter)=deltaU2(HouseHoldCounter)+deltaU2temp
            deltaU3(HouseHoldCounter)=deltaU3(HouseHoldCounter)+deltaU3temp
            deltaU4(HouseHoldCounter)=deltaU4(HouseHoldCounter)+deltaU4temp
            maxdeltaU4=max(maxdeltaU4,deltaU4(HouseHoldCounter))
            totalnactivity(HouseHOldCounter)=totalnactivity(HouseHoldCounter)+Meta
            totalactivity(HouseHoldCounter)=totalactivity(HouseHoldCounter)+MTA
            TotalMeta=TotalMeta+Meta    
        enddo
        if(StressHHFlag.eq.1) then 
            StressHHCounter=StressHHCounter+1
            stressHHflagVector(HouseHoldCounter)=1
        endif
        if(naHHFlag.eq.1) then 
            naHHCounter=naHHCounter+1
        endif
  enddo
    write(40,*)'....................................'
    write(40,*) IterationID
    write(40,*) "NO. NA Traveller, NO. NA HH, Stress Traveller, Stress HH"
    write(40,*) naPcounter,naHHcounter,stressPcounter,stressHHcounter,sum(deltaU4)/size(deltaU4)
    
    write(40,*) sum(DeltaU1)/(NumHouseHold*10.0/10.0),sum(DeltaU2)/(NumHouseHold*10.0/10.0),sum(DeltaU3)/(NumHouseHold*10.0/10.0)
    write(40,*) sqrt(sum((DeltaU1(1:NumHouseHold)-sum(DeltaU1)/(NumHouseHold*10.0/10.0))**2)/(NumHouseHold*1.0)),sqrt(sum((DeltaU2(1:NumHouseHold)-sum(DeltaU2)/(NumHouseHold*10.0/10.0))**2)/(NumHouseHold*1.0)),sqrt(sum((DeltaU3(1:NumHouseHold)-sum(DeltaU3)/(NumHouseHold*10.0/10.0))**2)/(NumHouseHold*1.0))
    !write(41,*)"------------------------------------"
        
!    hhgap=50          
!   do m=1,IterationID      
!        hhgap=hhgap*m
!   enddo     
   !hhgap=IterationID+1
   checktemp=0
   !-------------------------------------------------------
   !Start the Dll
   !randomseed=IterationID*100
    !test random number
     allocate(seed(1))
     seed(1)=IterationID
     !seedsize=1
     !call random_seed(size = seedsize)
    call random_seed(put=seed)
    call random_number(x)
    open(file='dllinputparameter.dat',unit=46)
    read(46,*) beta1
    read(46,*) beta2
    read(46,*) beta3
    read(46,*) beta4
    read(46,*) beta5
    read(46,*)dllmethodflag !0 is the utility, 1 is the objective function

   maxdeltau4=maxdeltau4/beta5
  !write(3,*) "beta",beta1,beta2,beta3,beta4
 do HouseHoldCounter=1,NumHouseHold !Go through all the household
   !do HouseHoldCounter=1,1
!        HouseHoldCounter=1
!        do while (HouseHold_ID(HouseHoldCounter).ne.1309496) 
!        !Get the household ID then loop over all the travelers in the household
!            HouseHoldCounter=HouseHoldCounter+1
!        enddo
        !do HouseHoldCounter=2043,2043
        !-----------
        !all stress household
        !if(DllHHflag(HouseHoldCounter).gt.0) then !Stess method
        !-----------
        !-----------
        !random method
!        call random_number(x)
!        if(x.lt.1.0/(((IterationID+1)*10.0)/10.0)) then 
        !---------- 
        !----------
        !Gap Method
        call random_number(x)
        !if(deltaU3(HouseHoldCounter).eq.0) deltaU3(HouseHoldCounter)=0.00277258872/10.0
        !if(IterationID.ne.1) then 
           if ((deltaU3(HouseHoldCounter).lt.0).or.(stressHHflagVector(HouseHoldCounter).eq.1).or.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then 
            !if ((deltaU3(HouseHoldCounter).lt.0).or.(stressHHflagVector(HouseHoldCounter).eq.1)) then 
           ! if (deltaU3(HouseHoldCounter).lt.0) then
                x=0
!            else
!                x=10
            endif
            !if(HouseHoldCounter.eq.3962) x=10
          if(householdcounter.eq.599022) then 
            x=10000
          endif
          do m=1,MaxHHSize !for each household go through all the travelers
                if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).gt.0) then
                    TravellerIndex=Traveller_Index(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m))
                    if(TravellerIndex.eq.0) then 
                       x=10000
                    endif
                endif
               ! if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).eq.0) x=10000
            enddo
          
        !endif
        !x=0.0
!  
        !1/(1+exp(beta3*(-beta1*deltaU1(i)-beta2*deltaU2(i))))*
        !1.0/(((IterationID+1)*10.0)/10.0)
        if(dllmethodflag.eq.0) then 
            dllprobability=1/(1+exp(beta4*(-beta1*deltaU1(HouseHoldCounter)-beta2*deltaU2(HouseHoldCounter)+beta3*deltaU3(HouseHoldCounter))))*1.0/(((IterationID+1)*10.0)/10.0)
        else
            dllprobability=deltaU4(HouseHoldCounter)/maxdeltaU4/(IterationID+1)
        endif
        
        
        
        if(x.lt.dllprobability) then 
         !if(x.lt.) then 
            dllHHflag(HouseHoldCounter)=1
            !-----------------------------------------   
            !if(dllHHflag(HouseHoldCounter).gt.0) then  
            checktemp=checktemp+1
            write(*,*) HouseHold_ID(HouseHoldCounter),HouseHoldCounter
            numHHMembers=0
            TotalHHTripNum=0
            numHHJointTrips=HHnumJointTrips(HouseHold_ID(HouseHoldCounter))
            numTotalParticipants=0
            do m=1,MaxHHSize !for each household go through all the travelers
                if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).gt.0) then
                    TravellerIndex=Traveller_Index(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m))
                    if(TravellerIndex.ne.0) then 
                        numHHMembers=numHHMembers+1  
                        do tripcounter=1,TravellerNumTrips(TravellerIndex)
                            TotalHHTripNum=TotalHHTripNum+1
                        enddo
                    endif
                endif
            enddo
            
            do j=1,numHHJointTrips
                numTotalParticipants=numTotalParticipants+NumTripParticipant(HouseHold_ID(HouseHoldCounter),j)
            enddo
            
            allocate(PersonTypes(0:numHHMembers))
                PersonTypes(:)=0 
            allocate(personTypeNumTrips(0:numHHMembers))
                personTypeNumTrips(:)=0  
            allocate(orgActTypes(0:TotalHHTripNum))
                orgActTypes(:)=0
            allocate(dstActTypes(0:TotalHHTripNum))
                dstActTypes(:)=0       
            allocate(Participants(0:numTotalParticipants))               
                Participants(:)=0
            allocate(NumParticipants(0:numHHJointTrips))
                NumParticipants(:)=0 
            allocate(numJointTrips(0:numHHMembers)) 
                numJointTrips(:)=0
            allocate(jointTrips(0:TotalHHTripNum))
                jointTrips(:)=0    
            allocate(TripPlannedDeparture(0:TotalHHTripNum))
                TripPlannedDeparture(:)=0
            allocate(TripPlannedTime(0:TotalHHTripNum))
                TripPlannedTime(:)=0
            allocate(NumCompletedTrips(0:numHHMembers))
                NumCompletedTrips(:)=0
            allocate(TripSimulateddeparture(0:TotalHHTripNum))
                TripSimulateddeparture(:)=0
            allocate(TripSimulatedTime(0:TotalHHTripNum))
                TripSimulatedTime(:)=0
            allocate(TripExpectedTime(0:TotalHHTripNum))
                TripExpectedTime(:)=0
            allocate(tripAdjustedDeparture(0:TotalHHTripNum)) 
                tripAdjustedDeparture(:)=0
            
            
            i=0  
            zzz=0  
            do j=1,numHHJointTrips
                NumParticipants(j)=NumTripParticipant(HouseHold_ID(HouseHoldCounter),j)
                do participantcounter=1,NumParticipants(j)
                    zzz=zzz+1
                    Participants(zzz)=PersonNumOfHH_TripID(HouseHold_ID(HouseHoldCounter),j,participantcounter)
                enddo
            enddo
             mm=0
            
            do m=1,MaxHHSize !for each household go through all the travelers
                
                if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).gt.0) then 
                    TravellerIndex=Traveller_Index(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m))
                    if(TravellerIndex.ne.0) then
                       
                        mm=mm+1
                        PersonTypes(mm)=TravellerType(Traveller_ID(TravellerIndex))
                        personTypeNumTrips(mm)=TravellerNumTrips(TravellerIndex)
                        NumCompletedTrips(mm)=TravellerNumTrips(TravellerIndex)
                        do tripcounter=1,personTypeNumTrips(mm) 
                            if(TravellerTripJointFlag(TravellerIndex,tripcounter).eq.1) then 
                                numJointTrips(mm)=numJointTrips(mm)+1
                            endif
                            i=i+1
                            if(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'Home') then 
                                orgActTypes(i)=0
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'work'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'Work') then 
                                orgActTypes(i)=1
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'Shop'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'shop') then
                                orgActTypes(i)=5
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'Escort'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'escort') then
                                orgActTypes(i)=4
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'work-based') then
                                orgActTypes(i)=1
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'Discretionary'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'discretionary') then 
                                orgActTypes(i)=9
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'EatingOut'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'eatingout') then
                                orgActTypes(i)=7
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'work-related') then
                                orgActTypes(i)=1
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'maintenance'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'Maintenance') then
                                orgActTypes(i)=6
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'school'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'School') then
                                orgActTypes(i)=3
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'Visiting'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'visiting') then
                                orgActTypes(i)=8
                            elseif(trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'university'.or.trim(TravellerTripOriginPurpose(TravellerIndex,tripcounter)).eq.'University') then
                                orgActTypes(i)=2
                            endif
                            
                            if(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'Home') then 
                                dstActTypes(i)=0
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'work'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'Work') then 
                                dstActTypes(i)=1
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'Shop'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'shop') then
                                dstActTypes(i)=5
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'Escort'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'escort') then
                                dstActTypes(i)=4
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'work-based') then
                                dstActTypes(i)=1
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'Discretionary'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'discretionary') then 
                                dstActTypes(i)=9
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'EatingOut'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'eatingout') then
                                dstActTypes(i)=7
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'work-related') then
                                dstActTypes(i)=1
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'maintenance'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'Maintenance') then
                                dstActTypes(i)=6
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'school'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'School') then
                                dstActTypes(i)=3
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'Visiting'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'visiting') then
                                dstActTypes(i)=8
                            elseif(trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'university'.or.trim(TravellerTripDestinationPurpose(TravellerIndex,tripcounter)).eq.'University') then
                                dstActTypes(i)=2
                            endif
                            if (TravellerTripJointFlag(TravellerIndex,tripcounter).eq.0) then !if this trip is an individual tripThis trip is the JointTrips(m,i)th joint trip of the traveller 
                                JointTrips(i)=0
                            else
                                do jj1=1,numHHJointTrips
                                    if(TripStartTime(TravellerIndex,tripcounter).eq.HHJointTripStarTimeList(HouseHold_ID(HouseHoldCounter),jj1)) then 
                                        do participantcounter=1,NumParticipants(jj1)
                                            if(PersonNumOfHH_TripID(HouseHold_ID(HouseHoldCounter),jj1,participantcounter).eq.m) then
                                                JointTrips(i)=jj1
                                            endif
                                        enddo
                                    endif
                                enddo
                            endif   
                            TripPlannedDeparture(i)=TripStartTime(TravellerIndex,tripcounter)+180
                            if(TripPlannedDeparture(i).eq.1620) then 
                                TripPlannedDeparture(i)=1619
                            endif
                            if(tripcounter.lt.TravellerNumTrips(TravellerIndex)) then 
                                temp=(TripStartTime(TravellerIndex,tripcounter+1))-(TripStartTime(TravellerIndex,tripcounter))-(TravellerActivityTime(TravellerIndex,tripcounter))
                                TripPlannedTime(i)=NINT(NINT(temp*10.0)/10.0)
                            else
                                temp=1440.0-(TripStartTime(TravellerIndex,tripcounter))-(TravellerActivityTime(TravellerIndex,tripcounter))
                                TripPlannedTime(i)=NINT(NINT(temp*10.0)/10.0)
                            endif
                            if(TripPlannedTime(i).eq.0) then 
                                TripPlannedTime(i)=1
                            endif
                            
                            if(TripTravelTime(TravellerIndex,tripcounter).lt.1440.and.TripTravelTime(TravellerIndex,tripcounter).gt.0) then 
                                TripSimulatedTime(i)=NINT(NINT(TripTravelTime(TravellerIndex,tripcounter)*10.0)/10.0)
                            else
                                if(TripTravelTime(TravellerIndex,tripcounter).lt.1440) then
                                    if(TravellerTripMode(TravellerIndex,tripcounter).ne.7.and.TravellerTripMode(TravellerIndex,tripcounter).ne.8.and.TravellerTripMode(TravellerIndex,tripcounter).le.12) then 
                                        write(*,*)
                                    endif
                                endif
                                TripSimulatedTime(i)=NINT(NINT(TripPlannedTime(i)*10.0)/10.0)
                            endif
                            if(TripSimulatedTime(i).eq.0) then 
                                TripSimulatedTime(i)=1
                            endif
                            TripExpectedTime(i)=TripSimulatedTime(i)
                            TripSimulatedTime(i)=TripPlannedTime(i)
                            TripSimulateddeparture(i)=TripPlannedDeparture(i)
                        enddo
                    endif
                endif
            enddo
            allocate(tempvector(8,0:maxval(jointTrips)))
                tempvector(:,:)=0
    !        !TripExpectedTime(13)=28
    	    write(22,*) HouseHoldCounter,TravellerIndex !,Traveller_Id(TravellerIndex)
            write(22,*) numHHMembers,numHHJointTrips
            write(22,21) 'personTypes',personTypes
            write(22,21) 'personTypeNumTrips',personTypeNumTrips
    	    write(22,21) 'numParticipants',numParticipants
    	    write(22,21) 'participants',participants
	        write(22,21) 'numJointTrips',numJointTrips
	        write(22,21) 'numCompletedTrips',numCompletedTrips
	        write(22,21) 'orgActTypes',orgActTypes
	        write(22,21) 'dstActTypes',dstActTypes
	        write(22,21) 'jointTrips',jointTrips
	        write(22,21) 'tripPlannedDeparture',tripPlannedDeparture
	        write(22,21) 'tripPlannedTime',tripPlannedTime
	        write(22,21) 'tripSimulatedDeparture',tripSimulatedDeparture
	        write(22,21) 'tripSimulatedTime',tripSimulatedTime
	        write(22,21) 'tripExpectedTime',tripExpectedTime
	        write(22,21) 'tripAdjustedDeparture',tripAdjustedDeparture 
            write(22,21) 'diffPlan,Sim',    tripSimulatedTime-tripPlannedTime
!            tripPlannedDeparture(8)=1619
!            tripSimulatedDeparture(8)=1619
            !if(Traveller_ID(i).eq.533378) then 
            !call my_values()
            !if(TravellerIndex
                
!            
            call getAdjustedDeparts (numHHMembers, personTypes, personTypeNumTrips, orgActTypes, &
			    dstActTypes, numHHJointTrips, numParticipants, participants, numJointTrips, jointTrips, numCompletedTrips, &
			    tripSimulatedDeparture, tripSimulatedTime, tripExpectedTime, tripAdjustedDeparture )
            !tripAdjustedDeparture=tripPlannedDeparture
   		    !Change the vehicle activity time
   		    if(sum(personTypeNumTrips).gt.0) then 
   		        if(tripAdjustedDeparture(1).lt.0) then 
   		            tripAdjustedDeparture(1)=-tripAdjustedDeparture(1)
   		            adjustFlag(HouseHoldCounter)=0
   		            write(3,*) HouseHoldCounter,tripAdjustedDeparture(1)
   		        else
   		            adjustFlag(HouseHoldCounter)=1
   		        endif
   		    endif
   		    write(22,*) dllHHflag(HouseHoldCounter)
   		    write(22,*) 1/(1+exp(beta3*(-beta1*deltaU1(HouseHoldCounter)-beta2*deltaU2(HouseHoldCounter))))*1.0/(((IterationID)*10.0)/10.0)
            write(22,*) deltaU1(HouseHoldCounter),deltaU2(HouseHOldCounter)
    		write(22,*) HouseHoldCounter,TravellerIndex 
            write(22,*) numHHMembers,numHHJointTrips
            write(22,21) 'personTypes',personTypes
            write(22,21) 'personTypeNumTrips',personTypeNumTrips
    	    write(22,21) 'numParticipants',numParticipants
    	    write(22,21) 'participants',participants
	        write(22,21) 'numJointTrips',numJointTrips
	        write(22,21) 'numCompletedTrips',numCompletedTrips
	        write(22,21) 'orgActTypes',orgActTypes
	        write(22,21) 'dstActTypes',dstActTypes
	        write(22,21) 'jointTrips',jointTrips
	        write(22,21) 'tripPlannedDeparture',tripPlannedDeparture
	        write(22,21) 'tripPlannedTime',tripPlannedTime
	        write(22,21) 'tripSimulatedDeparture',tripSimulatedDeparture
	        write(22,21) 'tripSimulatedTime',tripSimulatedTime
	        write(22,21) 'tripExpectedTime',tripExpectedTime
	        write(22,21) 'tripAdjustedDeparture',tripAdjustedDeparture 
            write(22,21) 'diffPlan,Sim',    tripSimulatedTime-tripPlannedTime	
   		    if(minval(tripAdjustedDeparture(1:TotalHHTripNum)).lt.180.or.maxval(tripAdjustedDeparture(1:TotalHHTripNum)).ge.(1440+180)) then
   		        dllHHflag(HouseHoldCounter)=0
   		    endif 
   		     write(22,*) dllHHflag(HouseHoldCounter)
   		     write(22,*) "-----------------------------------------------"
   		     
   		     
   		    mm=0
   		    do m=1,TotalHHTripNum
   		        if(tripAdjustedDeparture(m).ne.TripPlannedDeparture(m))then 
   		            mm=1
   		        endif
   		    enddo
            !write(41,28) HouseHold_ID(HouseHoldCounter),deltaU1(HouseHoldCounter),deltaU2(HouseHoldCounter),deltaU3(HouseHoldCounter),deltau4(HouseHoldCounter),x,1/(1+exp(beta4*(-beta1*deltaU1(HouseHoldCounter)-beta2*deltaU2(HouseHoldCounter)+beta3*deltaU3(HouseHoldCounter))))*1.0/(((IterationID+1)*10.0)/10.0),dllHHflag(HouseHoldCounter),mm,HHNumCarTrips(HouseHoldCounter),HHNumTransitTrips(HouseHoldCounter),HHNumPNRTrips(HouseHoldCounter),HHNumNeATrips(HouseHoldCounter),HHNumNhNeATrips(HouseHoldCounter),totalnactivity(HouseHOldCounter),totalactivity(HouseHOldCounter)
   		
   		
   		    if(dllHHflag(HouseHoldCounter).gt.0) then
   		        checkcounter1=0
   		        mm=0
   		        do m=1,MaxHHsize
   		            if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).gt.0) then 
                        i=Traveller_Index(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m))
                        !i=TravellerIndex
                        if(i.ne.0) then
                            mm=mm+1
                            do j=1,personTypeNumTrips(mm)
                                checkcounter1=checkcounter1+1
                                TripStartTime(i,j)=tripAdjustedDeparture(checkcounter1)-180.0
                                if(TripStartTime(i,j).le.0)then 
                                     TripStartTime(i,j)=1
                                    write(3,*) i,j,tripAdjustedDeparture(checkcounter1),'0 start time'
                                endif
                                
                                if(ijtovehicletripindex(i,j).eq.1) then 
                                    VehicleStartTime(ijtovehicleindex(i,j))=TripStartTime(i,j)
                                    if (TripStartTime(i,j).lt.VectorStartTimeofTimeInterval(2)) then 
                                        VehicleStartTimeInterval(ijtovehicleindex(i,j))=1
                                    else if (TripStartTime(i,j).gt.VectorStartTimeofTimeInterval(MaxNumTimeInterval)) then 
                                        VehicleStartTimeInterval(ijtovehicleindex(i,j))=MaxNumTimeInterval
                                    else 
                                        VehicleStartTimeInterval(ijtovehicleindex(i,j))=ceiling((TripStartTime(i,j)-120)/30.0)+1
                                    endif
                                endif
                                
                                if(j.ne.personTypeNumTrips(mm)) then 
                                    TravellerActivityTime(i,j)=tripAdjustedDeparture(checkcounter1+1)-tripAdjustedDeparture(checkcounter1)-tripExpectedTime(checkcounter1)
                                else
                                    TravellerActivityTime(i,j)=1440.0+180.0-tripAdjustedDeparture(checkcounter1)-tripExpectedTime(checkcounter1)
                                endif
                                if(TravellerActivityTime(i,j).lt.0) TravellerActivityTime(i,j)=0

                            enddo
                            if(sum(TravellerActivityTime(i,:)).ge.1440)then 
                                write(3,*) "person", i, "has error in activity time"
                            endif
                        endif
                        
                    endif
                enddo
        !        checkcounter1=0
        !   		mm=0
        !   		do m=1,MaxHHsize
        !   		    if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).gt.0) then 
        !                i=Traveller_Index(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m))
        !                !i=TravellerIndexll get 
        
        !                if(i.ne.0) then
        !                    mm=mm+1
        !                    do j=1,personTypeNumTrips(mm)
        !                        checkcounter1=checkcounter1+1
        !                        if(ijtovehicleindex(i,j).gt.0) then 
        !                            if((ijtovehicletripindex(i,j)).ne.VehicleNonrepetitiveCarNumTrip(ijtovehicleindex(i,j))) then 
        !                                if(VehicleToTravellertripIndex(ijtovehicleindex(i,j),ijtovehicletripindex(i,j)+1+1).eq.0) then 
        !                                    write(*,*)
        !                                endif
        !                                VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j))=TripStartTime(i,VehicleToTravellertripIndex(ijtovehicleindex(i,j),ijtovehicletripindex(i,j)+1+1))-TripStartTime(i,j)-tripexpectedTime(checkcounter1)
        !                            else
        !                                VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j))=0
        !                            endif
        !                            if(VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j)).lt.0) then 
        !                                VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j))=0
        !                            endif
        !                        endif
        !                    enddo
        !                endif
        !            endif
        !        enddo
               
        !        checkcounter1=0
        !        todiff=0
        !        checkcounter4=0
        !        do checkcounter2=1,numHHMembers
        !            do checkcounter3=1,personTypeNumTrips(checkcounter2)
        !                checkcounter1=checkcounter1+1
        !                if(checkcounter3.ne.personTypeNumTrips(checkcounter2)) then 
        !                    if(tripAdjustedDeparture(checkcounter1+1)-tripAdjustedDeparture(checkcounter1)-tripExpectedTime(checkcounter1).lt.0) then 
        !                        write(3,*) HouseHoldCounter,PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),checkcounter2),"Trip expected time is larger than the gap"
        !                        write(40,*)HouseHoldCounter,PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),checkcounter2),"N Atime after"
        !                    endif
        !                else
        !                    if(1440.0-tripAdjustedDeparture(checkcounter1)-tripExpectedTime(checkcounter1).lt.0) then 
        !                        write(3,*) HouseHoldCounter,"Trip expected time is larger than the gap"
        !                        write(40,*)HouseHoldCounter,PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),checkcounter2),"N Atime after"
        !                    endif
        !                endif
        !                if(tripAdjustedDeparture(checkcounter1).ne.tripplanneddeparture(checkcounter1) )then 
        !                    checkcounter4=checkcounter4+1
        !                endif
        !                todiff=todiff+abs(tripexpectedtime(checkcounter1)-tripplannedtime(checkcounter1))
        !                if(JointTrips(checkcounter1).ne.0) then 
        !                    if(tempvector(1,JointTrips(checkcounter1)).eq.0) then 
        !                        tempvector(1,JointTrips(checkcounter1))=orgActTypes(checkcounter1)
        !                        tempvector(2,JointTrips(checkcounter1))=dstActTypes(checkcounter1)
        !                        tempvector(3,JointTrips(checkcounter1))=jointTrips(checkcounter1)
        !                        tempvector(4,JointTrips(checkcounter1))=tripPlannedDeparture(checkcounter1)
        !                        tempvector(5,JointTrips(checkcounter1))=tripPlannedTime(checkcounter1)
        !                        tempvector(6,JointTrips(checkcounter1))=tripSimulatedDeparture(checkcounter1)
        !                        tempvector(7,JointTrips(checkcounter1))=tripSimulatedTime(checkcounter1)
        !                        tempvector(8,JointTrips(checkcounter1))=tripExpectedTime(checkcounter1)
        !                    else 
        !                    
        !                        if(tempvector(1,JointTrips(checkcounter1)).ne.orgActTypes(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent1',tempvector(1,JointTrips(checkcounter1))-orgActTypes(checkcounter1)
        !                        elseif(tempvector(2,JointTrips(checkcounter1)).ne.dstActTypes(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent2',tempvector(2,JointTrips(checkcounter1))-dstActTypes(checkcounter1)
        !                        elseif(tempvector(3,JointTrips(checkcounter1)).ne.jointTrips(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent3',tempvector(3,JointTrips(checkcounter1))-jointTrips(checkcounter1)
        !                        elseif(tempvector(4,JointTrips(checkcounter1)).ne.tripPlannedDeparture(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent4',tempvector(4,JointTrips(checkcounter1))-jointTrips(checkcounter1)
        !                        elseif(tempvector(5,JointTrips(checkcounter1)).ne.tripPlannedTime(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent5',tempvector(5,JointTrips(checkcounter1))-tripPlannedTime(checkcounter1)
        !                        elseif(tempvector(6,JointTrips(checkcounter1)).ne.tripSimulatedDeparture(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent6',tempvector(6,JointTrips(checkcounter1))-tripSimulatedDeparture(checkcounter1)
        !                        elseif(tempvector(7,JointTrips(checkcounter1)).ne.tripSimulatedTime(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent7',tempvector(7,JointTrips(checkcounter1))-tripSimulatedTime(checkcounter1)
        !                        elseif(tempvector(8,JointTrips(checkcounter1)).ne.tripExpectedTime(checkcounter1))then
        !                            write(3,*) HouseHoldCounter,'joint trip inconsistent8',tempvector(8,JointTrips(checkcounter1))-tripExpectedTime(checkcounter1)
        !                        endif
        !                    endif
        !                endif
        !            enddo
        !        enddo
        !        
        !        if(checkcounter4.eq.0) then 
        !            if(todiff.gt.0) then 
        !                write(3,*) HouseHoldCounter,"DPT not changed", todiff
        !            endif
        !        endif
                write(*,*) "end one household"
                write(*,*)"-----------------------------------------------------------------------------------"
                write(*,*)
                !endif
                 
!             dllHHflag(HouseHoldCounter)=1
!            else
                
            endif
            deallocate(PersonTypes)
            deallocate(personTypeNumTrips)
            deallocate(orgActTypes)
            deallocate(dstActTypes)
            deallocate(Participants)               
            deallocate(NumParticipants)
            deallocate(numJointTrips) 
            deallocate(jointTrips)
            deallocate(TripPlannedDeparture)
            deallocate(TripPlannedTime)
            deallocate(NumCompletedTrips)
            deallocate(TripSimulatedDeparture)
            deallocate(TripSimulatedTime)
            deallocate(TripExpectedTime)
            deallocate(tripAdjustedDeparture) 
            deallocate(tempvector)
        else
            dllHHflag(HouseHoldCounter)=0
            
           
           ! write(41,28) HouseHold_ID(HouseHoldCounter),deltaU1(HouseHoldCounter),deltaU2(HouseHoldCounter),deltaU3(HouseHoldCounter),deltau4(HouseHoldCounter),x,1/(1+exp(beta4*(-beta1*deltaU1(HouseHoldCounter)-beta2*deltaU2(HouseHoldCounter)+beta3*deltaU3(HouseHoldCounter))))*1.0/(((IterationID+1)*10.0)/10.0),dllHHflag(HouseHoldCounter),mm,HHNumCarTrips(HouseHoldCounter),HHNumTransitTrips(HouseHoldCounter),HHNumPNRTrips(HouseHoldCounter),HHNumNeATrips(HouseHoldCounter),HHNumNhNeATrips(HouseHoldCounter),totalnactivity(HouseHOldCounter),totalactivity(HouseHOldCounter)
        endif
        
    enddo
    
    
    !-----------------------------------------------------------
    write(40,*) 'In this iteration',checktemp,'households are adjusted'
    !Write new vehicle.dat and transit.dat
    !call WriteVeNTr
    !update the abmtrip start time
    if(IterationID.eq.1) then 
       ! open(file='abmtraveltime.dat',unit=50) 
        do i=1,NumTraveller
            do j=1,TravellerNumTrips(i) 
                
                ABMTripStartTime(Abmtripindex(i,j))=TripStartTime(i,j)
                ABMTripActivityTime(Abmtripindex(i,j))=TravellerActivityTime(i,j)
            enddo
            
        enddo
!        do i=1,sum(TravellerNumTrips(:))
!            write(50,*) ABMTripStartTime(i),ABMTripActivityTime(i),ABMTravelTime(i)
!        enddo
    endif
    
    !----------------------------------------------------------
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
!    read(6,*) nzones_old,noofnodes_old,noofarcs_old,kay,SuperZoneSwitch
!    
!  !-----------------------------------------
!!Car
!    allocate (nodenum(noofnodes_old))
!        nodenum(:)=0
!    allocate (izone(noofnodes_old))
!        izone(:)=0
!    allocate (idnum(noofnodes_old*100))
!        idnum(:)=0
!    allocate (iu(noofarcs_old))
!        iu(:)=0
!    allocate (id(noofarcs_old))
!        id(:)=0
!    allocate (MTbay(noofarcs_old))
!        MTbay(:)=0
!    allocate (MTbayR(noofarcs_old))
!        MTbayR(:)=0
!    allocate (i3(noofarcs_old))
!        i3(:)=0
!    allocate (nlanes(noofarcs_old))
!        nlanes(:)=0
!    allocate (FlowModelNum(noofarcs_old))
!        FlowModelNum(:)=0
!    allocate (Vfadjust(noofarcs_old))
!        Vfadjust(:)=0
!    allocate (SpeedLimit(noofarcs_old))
!        SpeedLimit(:)=0
!    allocate (mfrtp(noofarcs_old))
!        mfrtp(:)=0
!    allocate (sattp(noofarcs_old))
!        sattp(:)=0
!    allocate (link_iden(noofarcs_old))
!        link_iden(:)=0
!    allocate (LGrade(noofarcs_old))
!        LGrade(:)=0
!	allocate(iunod(noofarcs_old))
!        iunod(:)=0
!    allocate(idnod(noofarcs_old))
!        idnod(:)=0
!    allocate (node_to_link(noofnodes_old,noofnodes_old))
!        node_to_link(:,:)=0
!    allocate (LinkWeight(noofarcs_old))
!        LinkWeight(:)=0
!    allocate (NoofGenLinksPerZone_old(nzones_old))
!        NoofGenLinksPerZone_old(:)=0
!    allocate (ZoneTotalOriginLaneLength(nzones_old))
!        ZoneTotalOriginLaneLength(:)=0
!    allocate (ZoneGenerationLink(nzones_old,noofnodes_old))
!        ZoneGenerationLink(:,:)=0  

    !ParkRide
!    allocate (TazToTransitTaz(MaxTAZ))
!        TazToTransitTaz(:)=0                                
!    allocate (MazToTransitMaz(NumMAZ))
!        MazToTransitMaz(:)=0   
!    allocate(TravellerParkRideNum(MaxNumTraveller))
!        TravellerParkRideNum(:)=0   
    allocate(PNRZone(NumPairPNRZone,2))
        PNRZone(:,:)=0
    allocate(TransitZone(NumTotalTransitSkimZone,NumTransitOption))
        TransitZone(:,:)=0
    allocate(NumTransitZoneCa(NumTotalTransitSkimZone))
        NumTransitZoneCa(:)=0
!        
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
    allocate(PNReTime(MaxNumTraveller,MaxNumTrips))
        PNReTime(:,:)=0
    
    !!TransitSkim
        allocate(TransitPNRZonePair(NumPairTransitSkim,2))
            TransitPNRZonePair(:,:)=0
        allocate(TransitSkimABTime(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
            TransitSkimABTime(:,:)=0
        allocate(TransitSkimABCost(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
            TransitSkimABCost(:,:)=0
        allocate(TransitSkimBATime(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
            TransitSkimBATime(:,:)=0
        allocate(TransitSkimBACost(NumPairTransitSkim,MaxNumTransitSkimTimeInterval))
            TransitSkimBACost(:,:)=0
    !  
    !    !Walk
        allocate(PNRWalkZone(NumPairPNRWalkZone,2))
            PNRWalkZone(:,:)=0
        allocate(TransitSkimWalkTime(NumPairPNRWalkZone,NumTransitWalkOption))
            TransitSkimWalkTime(:,:)=0
        allocate(TransitWalkZone(NumTotalTransitSkimZone,NumTransitWalkOption))
            TransitWalkZone(:,:)=0
        allocate(NumTransitWalkZoneCa(NumTotalTransitSkimZone))
            NumTransitWalkZoneCa(:)=0
    !  
    !Driver or passenger                           
!    allocate (TravellerTripParkRideFlag(MaxNumTraveller,MaxNumTrips))
!        TravellerTripParkRideFlag(:,:)=0     

        
   ! allocate(TransitPassengerStartTime(MaxNumTraveller))
     !   TransitPassengerStartTime(:)=0
    !Time Interval        
!    allocate(VectorStartTimeofTimeInterval(MaxNumTimeInterval))
!        VectorStartTimeofTimeInterval(:)=0    
!    allocate(VectorLengthofTimeInterval(MaxNumTimeInterval))
!        VectorLengthofTimeInterval(:)=0
!          
!    allocate(TripStartTime(MaxNumTraveller,MaxNumTrips))
!        TripStartTime(:,:)=0
!    allocate(FirstTransitTrip(MaxNumTraveller))
!        FirstTransitTrip(:)=0
!    allocate(ValueOfTime(MaxPersonID))
!        ValueOfTime(:)=0
    !    allocate(TravellerTripTravelTime(MaxNumTraveller,MaxNumTrips))
    !        TravellerTripTravelTime(:,:)=10000
        
    ! To sort the trip back to the origin abm file
!    allocate(ABMTripIndex(MaxNumTraveller,MaxNumTrips))
!        ABMTripIndex(:,:)=0
    !Parking taz
    allocate(TravellerTripOriginParkingTaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginParkingTaz(:,:)=0
!    allocate(TravellerTripDestinationParkingTaz(MaxNumTraveller,MaxNumTrips))
!        TravellerTripDestinationParkingTaz(:,:)=0
    allocate(TravellerTripOriginParkingMaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripOriginParkingMaz(:,:)=0
    allocate(TravellerTripDestinationParkingMaz(MaxNumTraveller,MaxNumTrips))
        TravellerTripDestinationParkingMaz(:,:)=0
!    allocate(MaztoTazMap(NumMAZ))
!        MaztoTazMap(:)=0
!    allocate(TaztoMazMap(MaxTaz))
!        TaztoMazMap(:)=0
    !PathBank
!    allocate(IntraSuperzonePathIndex(MaxTaz,MaxTaz))
!        IntraSuperzonePathIndex(:,:)=0
!    allocate(tempvalue(15))
!        tempvalue(:)=0
!    allocate(SuperZoneMap(MaxTAZ,2))
!        SuperZoneMap(:,:)=0
!    allocate(PathBank(NumPath,MaxPathLength))
!        PathBank(:,:)=0 
!    allocate(NumIntraSuperzoneZone(NumPath))
!        NumIntraSuperzoneZone(:)=0
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
    
     !---------------------------------------------------------------------
    !Read the transit skim files
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

    allocate(EsPNRTravelTime(MaxNumTraveller,MaxNumTrips))
            EsPNRTravelTime(:,:)=0
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
                                write(3,*) "maztotazmap error",i,j,m,n,otap,dtap,otranz,dtranz
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
                                if(SpSkimTime.eq.0) SpSkimTime=1
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
                                if(currentDYNASMARTtimeinterval.eq.0) write(*,*) "currentDynasmarttimeinterval equals 0", i,j,k,currentDYNASMARTtime,currentDYNASMARTtime/10,ceiling(currentDYNASMARTtime/10),currentDYNASMARTtimeinterval,1
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
                                transitlegtime=TransitSkimABTime(TransitPairIndex(otap,dtap),PNRTransitTimeInterval)
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
                                EsPNRTravelTime(i,j)=transitlegtime+drivelegtime+walklegtime
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
                    if(TransitMazTazFlag.eq.0) then 
                        dtaptranz=MaztoTazMap(dtap)
                    elseif(TransitMazTazFlag.eq.1) then
                        dtaptranz=dtap
                    endif
                    !-----------------------------
                    !The drive part
                    pathindex=IntraSuperzonePathIndex(dtaptranz,TravellerTripDestinationTAZ(i,j))
                    if(pathindex.eq.0) then !Read from the vehicularPNRSkim.dat
                        !Determine the SP skim timeinterval
                        SpSkimTime=ceiling(TripStartTime(i,j)/SpSkimTimeInterval)
                        if(SpSkimTime.eq.0) SpSkimTime=1
                        if(SpSkimTime.gt.numskimintervals) SpSkimTime=numskimintervals
                        if(SpSkimTime.eq.0) then 
                            write(*,*) i,j,m,n,otap,dtap,SpSkimTime,TripStartTime(i,j),SpSkimTimeInterval
                        endif
                        !Determine VOT group number
                        vv=1
                        !do while(average_value_of_time.gt.ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%VOT)
                        do while(ValueofTime(Traveller_ID(i)).gt.ODTATT_Array(dtaptranz,SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%VOT)
                            vv=vv+1
                        enddo
                        drivelegtime=ODTATT_Array(dtaptranz,SuperZoneMap(TravellerTripDestinationTAZ(i,j),2),SpSkimTime)%P(vv)%Time
                        !drivelegcost=ODTATT_Array(TravellerTripOriginTAZ(i,j),SuperZoneMap(otaptranz,2),SpSkimTime)%P(vv)%Cost
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
    !                    if(TransitMazTazFlag.eq.0) then 
    !                        dtaptranz=MaztoTazMap(dtap)
    !                    elseif(TransitMazTazFlag.eq.1) then
    !                        dtaptranz=dtap
    !                    endif
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
                        PNRTravelTimeTemp=walkingweight*walklegtime+transitlegcost
                        !update the best route
                        if(PNRTravelTimeTemp.lt.PNRTravelTime) then
                            PNRTravelTime=PNRTravelTimeTemp
                             EsPNRTravelTime(i,j)=transitlegtime+drivelegtime+walklegtime
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
                    EsPNRTravelTime(i,j)=transitlegtime+drivelegtime+walklegtime
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
    !-------------------------------------------------------------------       	    
    !Update the validtraveller and valid car
    do i=1,index
        do j=1,MaxNumTrips
            !if(TripDriverPassengerFlag(i,j).gt.0) ValidCar(i)=1
            if(Traveller_ID(i).eq.0) ValidTraveller(i)=0
        enddo
    enddo
    !___________________________________________________________________
    !Calculate Num of Traveller
    WorkCounter=0
    NumTraveller=0
    do i=1,index
        if (ValidTraveller(i).eq.1) then
                NumTraveller=NumTraveller+1
                NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))+1
                if (MaxNumTravellerInEachTimeInterval.lt.NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))) MaxNumTravellerInEachTimeInterval=NumTravellerInTimeInterval(TravellerTripStartTimeInterval(i,1))
        endif
    enddo
    
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
       counter=0 
       open(file='traveler.dat',unit=8)
       write(*,*)'Writing traveler.dat ...'
!       write(81,5) NumTraveller, RealMaxNumTrips
!       write(81,23) 
!       write(81,22) 
       write(8,5) NumTraveller, RealMaxNumTrips
       write(8,23) 
       write(8,22) 
       open(file='TimeInterval.dat',unit=5)
       do t=1,MaxNumTimeInterval
           !read(5,*)  StartTimeofTimeInterval,LengthofTimeInterval
           do k=1,NumTravellerInTimeInterval(t)
                i=TravellerInEachTimeInterval(t,k) !getting the traveller index
                
                counter=counter+1
    !            TravellerStartTime(i)=StartTimeofTimeInterval+(LengthofTimeInterval/NumTravellerInTimeInterval(t))*(k-1) !unifrom distribution of travellers over the time interval
    !            temp1=1.0 !The precision of start time (minute), where 1, makes xx.00, 10, xx.x0, and 100, xx.xx (the latest is not acceptable in Dynasmart)
    !            StartTime=(nint(TravellerStartTime(i)*temp1))/temp1
                temp1=10.0
                StartTime=(nint(TripStartTime(i,1)*temp1))/temp1
        
	            !writing the genral information of the traveller
	            !write(81,6) counter,i,Traveller_ID(i),iutmp,idtmp,StartTime,ivcltmp,ivcl2tmp,ihovtmp,veh_pathnodenum,TravellerNumTrips(i),infotmp,ribftmp,comptmp,TAZMap(TravellerTripOriginTAZ(i,1)) 
	            !write(81,24) counter,i,Traveller_ID(i),StartTime,TravellerNumTrips(i),TravellerTripOriginTAZ(i,1),ValidCar(i),FirstCarTrip(i),FirstTransitTrip(i),ValueOfTime(Traveller_ID(i)),TravellerType(Traveller_ID(i))
                write(8,24) counter,i,Traveller_ID(i),StartTime,TravellerNumTrips(i),TravellerTripOriginTAZ(i,1),ValidCar(i),FirstCarTrip(i),FirstTransitTrip(i),ValueOfTime(Traveller_ID(i)),TravellerType(Traveller_ID(i))

                !writing the trips information
                temp3=30.0 !constant used for the activity time generation
                do j=1,TravellerNumTrips(i)  !go over all the trips except the last trip
!                    if(j.lt.TravellerNumTrips(i)) then
!                        ActivityTimeTemp=(TripStartTime(i,j+1)-TripStartTime(i,j))-ABMTempTime(AbmTripIndex(i,j))
!                        ActivityTime=(nint(ActivityTimeTemp*temp1))/temp1 !The precision of Activity duration (minute), where 1, makes xx.00, 10, xx.x0, and 100, xx.xx (the latest is not acceptable in Dynasmart)
!                        if(ActivityTime.lt.0) ActivityTime=0.0
!                        TravellerActivityTime(i,j)=ActivityTime
!                    else
!                        ActivityTime=1440.0-TripStartTime(i,j)-ABMTempTime(AbmTripIndex(i,j))
!                        TravellerActivityTime(i,j)=(nint(ActivityTime*temp1))/temp1
!                    endif
!                    if(TravellerActivityTime(i,j).lt.0) then 
!                        write(3,*)
!                    endif
                    !write the trip information
                    !write(81,25) TravellerTripDestinationTAZ(i,j),TravellerActivityTime(i,j),TravellerTripMode(i,j),TravellerTripOriginPurpose(i,j),TravellerTripDestinationPurpose(i,j),TravellerTripOriginMAZ(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripDestinationTAZ(i,j),TripDriverPassengerFlag(i,j),TravellerTripJointFlag(i,j),TravellerTripParkRideFlag(i,j),TravellerTripStartTimeInterval(i,j),TripStartTime(i,j),ABMTripIndex(i,j),TravellerTripDriver(i,j),TravellerTripDestinationParkingTaz(i,j)
                    write(8,25) TravellerTripDestinationTAZ(i,j),TravellerActivityTime(i,j),TravellerTripMode(i,j),TravellerTripOriginPurpose(i,j),TravellerTripDestinationPurpose(i,j),TravellerTripOriginMAZ(i,j),TravellerTripOriginTAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripDestinationTAZ(i,j),TripDriverPassengerFlag(i,j),TravellerTripJointFlag(i,j),TravellerTripParkRideFlag(i,j),TravellerTripStartTimeInterval(i,j),TripStartTime(i,j),ABMTripIndex(i,j),TravellerTripDriver(i,j),TravellerTripDestinationParkingTaz(i,j)

                enddo
           enddo            
       enddo
       if (counter.ne.NumTraveller) then
           write(3,*) 'Maximum Number of Valid Traveller is wrong, it is ', NumTraveller, 'while we have at least ', counter
           pause
       endif
       close(5)
       close(8)   
       
    deallocate(LinkTravelTime)
    deallocate(TurnPenaltyTime)
    deallocate(ODTATT_Array)
    deallocate(TransitZone)
    deallocate(TransitWalkZone)
    deallocate(NumTransitZoneCa)
    deallocate(NumTransitWalkZoneCa) 
    !-----------------------------------------------------------------------------------------
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
   open(file='TimeInterval.dat',unit=9)
   if(TransitMazTazFlag.eq.0) then 
        write(181,18)
   else
        write(181,21)
   endif
   !write(2,18) 
   do tt=1,MaxNumTimeInterval
       read(9,*)  StartTimeofTimeInterval,LengthofTimeInterval
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
!                        write(181,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginTransitMAZ(i,j),ZoneToZoneTravelTime(alex1,alex2,alex3),0.0,0.0
!                        write(181,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),otap,0,PNRaTime(i,j),0.0,0.0
                        write(181,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),otap,0,PNRaTime(i,j),0.0,0.0
   
                        !write(41,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),otap,0,PNRaTime(i,j),0.0,0.0

                    elseif(TravellerTripParkRideFlag(i,j).eq.2) then
                        !write(181,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginTransitMAZ(i,j),TravellerTripDestinationTransitMAZ(i,j),ZoneToZoneTravelTime(alex1,alex2,alex3),0.0
                        write(181,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,dtap,0.0,0.0,0.0
                        !write(41,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,dtap,0.0,0.0,0.0

                    else
                        !write(181,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginTransitMAZ(i,j),TravellerTripDestinationTransitMAZ(i,j),0.0,0.0,0.0
                         write(181,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,0,0.0,0.0,0.0
                         !write(41,17) Traveller_ID(i),i,j,TripStartTime(i,j),otranz,dtranz,TravellerTripParkRideFlag(i,j),0,0,0.0,0.0,0.0
                    endif
                    traveltimetemp=10.0
                    !write(2,17) Traveller_ID(i),i,j,TripStartTime(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),TravellerTripParkRideFlag(i,j),TravellerTripOriginMAZ(i,j),TravellerTripDestinationMAZ(i,j),traveltimetemp,traveltimetemp,traveltimetemp
                endif
            enddo
       enddo            
   enddo 
    close(9) 

    !----------------------------------------
    !Dissaggregate the park and ride trip
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
                    TravellerActivityTime(i,jj)=TravellerActivityTime(i,jj-1)
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
!                if(TravellerTripParkRideFlag(i,j).gt.2) then 
!                    write(3,*) "ParkRideFlag" ,i,j
!                    pause 
!                endif
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
                    TravellerActivityTime(i,j+1)=TravellerActivityTime(i,j)-1
                    TravellerActivityTime(i,j)=1
                    !Second Part of park and ride                 
                    TravellerTripMode(i,j+1)=TravellerTripMode(i,j+1)-2
                   TravellerTripOriginMAZ(i,j+1)=TravellerTripDestinationMAZ(i,j)         
                   TravellerTripOriginTAZ(i,j+1)=TravellerTripDestinationTAZ(i,j)  
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
                    TripDriverPassengerFlag(i,j)=-1
                    !TravellerTripParkRideFlag(i,j)=
                    !ABMTripIndex(i,j)=ABMTripIndex(i,j)
                    TravellerTripOriginParkingTaz(i,j)=TravellerTripOriginParkingTaz(i,j)
                    TravellerActivityTime(i,j+1)=TravellerActivityTime(i,j)-1
                    TravellerActivityTime(i,j)=1
                    
                    
                    !Second part of park and ride
                    TravellerTripMode(i,j+1)=1
                    TravellerTripOriginMAZ(i,j+1)=TravellerTripDestinationMAZ(i,j)
                    TravellerTripOriginTAZ(i,j+1)=TravellerTripDestinationTAZ(i,j)
                    !TravellerTripOriginMappedTaz(i,j+1)=TAZMap(TravellerTripOriginTAZ(i,j+1))
                    
                    TravellerTripOriginPurpose(i,j+1)=TravellerTripDestinationPurpose(i,j+1)
                    ABMTripIndex(i,j+1)=ABMTripIndex(i,j)
                    TravellerTripOriginParkingTaz(i,j+1)=TravellerTripOriginParkingTaz(i,j)
                    !trip_travel_time=a_time+tr_time
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
    close (181)  
    
do   HouseHoldCounter=1,NumHouseHold    
    if(dllHHflag(HouseHoldCounter).gt.0) then 
        !!if(DllHHflag(HouseHoldCounter).gt.0) then !Stress Method
        checkcounter1=0
   		!mm=0
   		do m=1,MaxHHsize
   		    if(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m).gt.0) then 
                i=Traveller_Index(PersonIDOfHH_PN(HouseHold_ID(HouseHoldCounter),m))
                !i=TravellerIndex
                if(i.ne.0) then
                    !mm=mm+1
                    do j=1,TravellerNumTrips(i)
                        checkcounter1=checkcounter1+1
                        if(ijtovehicleindex(i,j).gt.0) then 
                            if((ijtovehicletripindex(i,j)).ne.VehicleNonrepetitiveCarNumTrip(ijtovehicleindex(i,j))) then 
                                if(VehicleToTravellertripIndex(ijtovehicleindex(i,j),ijtovehicletripindex(i,j)+1+1).eq.0) then 
                                    write(*,*)
                                endif
                                if(TripTravelTime(i,j).lt.1440.0) then 
                                    VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j))=TripStartTime(i,VehicleToTravellertripIndex(ijtovehicleindex(i,j),ijtovehicletripindex(i,j)+1+1))-TripStartTime(i,j)-TripTravelTime(i,j)
                                else
                                    temp=TripStartTime(i,j+1)-TripStartTime(i,j)-TravellerActivityTime(i,j)
                                    VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j))=TripStartTime(i,VehicleToTravellertripIndex(ijtovehicleindex(i,j),ijtovehicletripindex(i,j)+1+1))-TripStartTime(i,j)-temp
                                endif
                            else
                                VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j))=0
                            endif
                            
                            if(VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j)).lt.0) then 
                                VehicleActivityTime(ijtovehicleindex(i,j),ijtovehicletripindex(i,j))=0
                            endif
                        endif
                    enddo
                endif
            endif
        enddo
    endif
enddo        
    !___________________________________________________________________
    !Generate the new vehicle error
    write(*,*) "Get max num vehicles in each timeinterval"
    WorkCounter=0
    NumTraveller=0
    NumCar=0           
    do i=1,vehicleindex
        NumCarInTimeInterval(VehicleStartTimeInterval(i))=NumCarInTimeInterval(VehicleStartTimeInterval(i))+1
        if (MaxNumCarInEachTimeInterval.lt.NumCarInTimeInterval(VehicleStartTimeInterval(i))) then 
            MaxNumCarInEachTimeInterval=NumCarInTimeInterval(VehicleStartTimeInterval(i))
        endif
        NumCar=NumCar+1
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
   do i=1,vehicleindex
        !if(validcar(i).lt.0) write(*,*) validcar(i),i !can be deleted
        NumCarInTimeInterval(VehicleStartTimeInterval(i))=NumCarInTimeInterval(VehicleStartTimeInterval(i))+1
        !if (NumCarInTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k))).eq.1) then !The first Car for that time interval and no sorting
        if(NumCarInTimeInterval(VehicleStartTimeInterval(i)).eq.1) then 
            CarInEachTimeInterval(VehicleStartTimeInterval(i),1)=i
           ! CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),1)=k
        endif  
        jleft=1
        jright=NumCarInTimeInterval(VehicleStartTimeInterval(i))-1
        j=ceiling(jright*1.0/2.0)
        EndofLoop=0
        
        do while(EndofLoop.eq.0.and.NumCarInTimeInterval(VehicleStartTimeInterval(i)).gt.1)
!            TravellerIDforj=CarInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)
!            VehicleIndexforj=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)
            VehicleIndexforj=CarInEachTimeInterval(VehicleStartTimeInterval(i),j)
            TravellerIDforj=VehicleToTravellertripIndex(VehicleIndexforj,1)
            tripindexforj=VehicleToTravellertripIndex(VehicleIndexforj,2)
            if(abs(jleft-jright).gt.1) then
                if(VehicleStartTime(VehicleIndexforj).gt.VehicleStartTime(i)) then
                    jright=j
                    j=ceiling((jleft+jright)/2.0)
                elseif(VehicleStartTime(VehicleIndexforj).lt.VehicleStartTime(i))then
                    jleft=j
                    j=ceiling((jleft+jright)/2.0)
                else
                    !if(CarInEachTimeInterval(TravellerTripStartTimeInterval(VehicleToTravellertripIndex(i,1),1),j).gt.i) then 
                    if( VehicleIndexforj.gt.i)then
                        jright=j
                        j=ceiling((jleft+jright)/2.0)
                    else
                        jleft=j
                        j=ceiling((jleft+jright)/2.0)
                    endif
                endif
            else    !Found the proper place
                TravellerIDforjleft=VehicleToTravellertripIndex(CarInEachTimeInterval(VehicleStartTimeInterval(i),jleft),1)
                VehicleIndexforjleft=CarInEachTimeInterval(VehicleStartTimeInterval(i),jleft)
                TravellerIDforjright=VehicleToTravellertripIndex(CarInEachTimeInterval(VehicleStartTimeInterval(i),jright),1)
                VehicleIndexforjright=CarInEachTimeInterval(VehicleStartTimeInterval(i),jright)
                if(VehicleIndexforjleft.eq.0) then 
                    write(*,*) 'left',jleft,i,k 
                elseif(VehicleIndexforjright.eq.0) then
                    write(*,*) 'right',jright,i,k
                endif
                if(VehicleStartTime(VehicleIndexforjleft).gt.VehicleStartTime(i)) then
                    j=jleft
                elseif(VehicleStartTime(VehicleIndexforjright).lt.VehicleStartTime(i))then
                    j=jright+1
                elseif(VehicleStartTime(VehicleIndexforjleft).lt.VehicleStartTime(i).and.VehicleStartTime(VehicleIndexforjright).gt.VehicleStartTime(i)) then
                    j=jleft+1
                else
                    if(VehicleStartTime(VehicleIndexforjleft).eq.VehicleStartTime(i).and.VehicleStartTime(VehicleIndexforjright).eq.VehicleStartTime(i)) then 
                        if(VehicleIndexforjleft.gt.i) then
                            j=jleft
                        elseif(VehicleIndexforjleft.lt.i.and.VehicleIndexforjright.gt.i) then 
                            j=jleft+1
                        else
                            j=jright+1
                        endif
                    elseif(VehicleStartTime(VehicleIndexforjleft).eq.VehicleStartTime(i)) then 
                         if(VehicleIndexforjleft.gt.i) then 
                            j=jleft
                         else
                            j=jleft+1
                         endif
                    elseif(VehicleStartTime(VehicleIndexforjright).eq.VehicleStartTime(i)) then
                        if(VehicleIndexforjright.gt.i) then 
                            j=jright
                         else
                            j=jright+1
                         endif
                    endif
                endif
                do jj=NumCarInTimeInterval(VehicleStartTimeInterval(i))-1,j,-1 !shift all the remaining
                    CarInEachTimeInterval(VehicleStartTimeInterval(i),jj+1)=CarInEachTimeInterval(VehicleStartTimeInterval(i),jj)
                    !CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj+1)=CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),jj)
                enddo
                CarInEachTimeInterval(VehicleStartTimeInterval(i),j)=i !The proper place is the last place
                !CarIndexInEachTimeInterval(TravellerTripStartTimeInterval(i,FirstCarTrip(i,k)),j)=k
                EndofLoop=1
            endif
        enddo
                           
!            enddo      
!        endif
   enddo
    !--------------------------------------------------------------------------------
    !write vehicleTT.dat
    open(file='internalmap.dat',unit=23)
    write(*,*) 'write vehicleTT.dat'
    write(101,5)TotalNumVehicle,MaxNumVehicleTrip
    write(101,19)
    write(23,*)'vehicleindex, i, person_id'
    
    counter=0
    do t=1,MaxNumTimeInterval
       do k=1,NumCarInTimeInterval(t)
            
          
            i=CarInEachTimeInterval(t,k)
            TravellerIDforj=VehicleToTravellertripIndex(i,1) !travellerindex
            tripindexforj=VehicleToTravellertripIndex(i,2)   !tripindx
            dllflagtemp=0
            
            !Ali
            if(TravellerIDforj.ne.0) then 
                if(Traveller_ID(TravellerIDforj).gt.0) then 
                    if(DllHHFLAG(HouseHoldIDtoCounter(HHIDofPerson(Traveller_ID(TravellerIDforj)))).ne.0) then 
                        dllflagtemp=1
                        
                    endif
                endif
            endif 
            !Ali end
!            if(TravellerIDforj.ne.0) then 
!                if(DllHHFLAG(HouseHoldIDtoCounter(HHIDofPerson(Traveller_ID(TravellerIDforj)))).eq.0) then 
!                    Vehicleivcltmp(i)=1
!                else
!                    dllflagtemp=1
!                    if(SuperZoneMap(TravellerTripOriginTAZ(TravellerIDforj,tripindexforj),2).ne.SuperZoneMap(TravellerTripDestinationTAZ(TravellerIDforj,tripindexforj),2))then
!                        Vehicleivcltmp(i)=3
!                    endif
!                endif
!            else    
!               Vehicleivcltmp(i)=3
!            endif   
          counter=counter+1
            write(101,11) counter,Vehicleiutmp(i),Vehicleidtmp(i),VehicleStartTime(i),Vehicleivcltmp(i),Vehicleivcl2tmp(i),Vehicleihovtmp(i),Vehicleveh_pathnodenum(i),VehicleNonrepetitiveCarNumTrip(i),Vehicleinfotmp(i),Vehicleribftmp(i),Vehiclecomptmp(i),VehicleTaz(i,1),VehicleValueofTime(i),dummy,dllflagtemp
            if(VehicleToTravellertripIndex(i,1).gt.0) then  
                write(23,*) counter,VehicleToTravellertripIndex(i,1),Traveller_ID(VehicleToTravellertripIndex(i,1))
            else 
                write(23,*)counter,0,0
            endif
            
            do j=1, VehicleNonrepetitiveCarNumTrip(i)
!                if(VehicleToTravellerTripIndex(i,j).gt.0) then 
!                    activitytime=TravellerActivitytime(VehicletoTravellerTripIndex(i,1),VehicletoTravellerTripIndex(i,j+1))
!                    if(TravellerActivitytime(VehicletoTravellerTripIndex(i,1),VehicletoTravellerTripIndex(i,j+1)).ne.VehicleActivityTime(i,j)) then 
!                        write(*,*)i,j,VehicletoTravellerTripIndex(i,1),VehicletoTravellerTripIndex(i,j+1)
!                    endif
!                else
                
                    activitytime=VehicleActivityTime(i,j)
!                endif
                write(101,7)VehicleTaz(i,j+1),activitytime,VehicleTIN(i,j),Vehicleupn(i,j),Vehicledpn(i,j),VehicleTravelTime(i,j)
                write(23,*)VehicleToTravellertripIndex(i,j+1)
            enddo
            
            write(371,27) (VehiclePath_Array(i)%p(kk)%pathnode,kk=1,Vehicleveh_pathnodenum(i))
       enddo
    enddo
!endif
   ! if(IterationID.eq.TargetIterationNumber) then !If it is the last iteration, then report the result back to ABM
        read(1,'(a400)') Line
        TextFinal2=trim(adjustl(Line))//","//'AdjustedTripStartTime'//","//'SimulatedTravelTime'//","//'StressFlag'
        

        write(28,'(a400)')TextFinal2
        index=0
        error=1
        do while (error.ne.-1)   
            read(1,'(a)',iostat=error) Line
            if (error.ne.-1) then
                !read(Line,*,iostat=error) hh_id, person_id, person_num, tour_id, stop_id, inbound, tour_purpose, orig_purpose, dest_purpose, orig_maz, orig_walk_segment, dest_maz, dest_walk_segment, parking_taz, stop_period, trip_mode, tour_mode, tour_category, board_tap, alight_tap, orig_taz, dest_taz, trip_driver_passenger
                read(Line,*,iostat=error) hh_id
                HouseHoldCounter=HouseHoldIDtoCounter(hh_id)
                if ((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then 
                    x=1
                elseif ((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then
                    x=2
                elseif((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=3
                elseif((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=4
                elseif((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then 
                    x=5
                elseif ((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then
                    x=6
                elseif((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=7
                elseif((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=8
                endif
                index=index+1
                
                write(Text,'(f8.2)') ABMTripStartTime(index)
                TextFinal=trim((trim(adjustl(Line))//","//trim(adjustl(Text))))
                if(ABMIncompleteFlag(index).eq.1)  ABMTravelTime(index)=1440
                
                write(Text,'(f8.2)') ABMTravelTime(index)
                TextFinal=trim((trim(adjustl(TextFinal))//","//trim(adjustl(Text))))
                
                write(Text,'(i3)') int(x)
                TextFinal=trim((trim(adjustl(TextFinal))//","//trim(adjustl(Text))))
                
                write(28,'(a200)')TextFinal
            endif
        enddo
        open(file='jointTripData_1.csv',unit=12)
        read(12,'(a300)') Line       
        error=1
        TextFinal2=trim(adjustl(Line))//","//'AdjustedTripStartTime'//","//'SimulatedTravelTime'//","//'StressFlag'
        write(29,'(a400)')TextFinal2
        do while (error.ne.-1)   
            read(12,'(a300)',iostat=error) Line
            if (error.ne.-1) then
                index=index+1
                
                read(Line,*,iostat=error) hh_id
                HouseHoldCounter=HouseHoldIDtoCounter(hh_id)
                 if ((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then 
                    x=1
                elseif ((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then
                    x=2
                elseif((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=3
                elseif((deltaU3(HouseHoldCounter).lt.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=4
                elseif((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then 
                    x=5
                elseif ((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.0)) then
                    x=6
                elseif((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.1).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=7
                elseif((deltaU3(HouseHoldCounter).ge.0).and.(stressHHflagVector(HouseHoldCounter).eq.0).and.(HHIncompleteFlag(HouseHoldCounter).eq.1)) then
                    x=8
                endif
            
                !index=index+1
                if(ABMIncompleteFlag(index).eq.1)  ABMTravelTime(index)=1440
                write(Text,'(f8.2)') ABMTripStartTime(index)
                TextFinal=trim((trim(adjustl(Line))//","//trim(adjustl(Text))))
                
                 write(Text,'(f8.2)') ABMTravelTime(index)
                TextFinal=trim((trim(adjustl(TextFinal))//","//trim(adjustl(Text))))
                
                write(Text,'(i3)') int(x)
                TextFinal=trim((trim(adjustl(TextFinal))//","//trim(adjustl(Text))))
                write(29,'(a200)')TextFinal
            endif
        enddo
    ! endif       

     write(*,*) "Finishing Component 2" 

!    open(file='IterationSetting.dat',unit=151) 
!    write(151,*) IterationID+1
!    write(151,*) TargetIterationNumber
!    close (151)
     close(5)
   close(10)
1  format (3i10)
2  format (5i8)
3  format('      #   usec   dsec   stime usrcls vehtype ioc #ONode #IntDe info ribf    comp    OZ')
4  format(2i7,2i5,i7,i3,i7,'  +',i1,i4,2i6,i3,'  +',i1)
5  format(2i12,'    # of vehicles in the file, Max # of stops')
6  format(5i8,f8.2,6i6,2f8.4,i5)
7  format(i12,f8.2,i8,2i12,f11.2) 
8  format(3i7,f8.2,6i6,2f8.4,i5)
9  format(i12,f8.2)
10 format(i12,i8,i3,f8.2,2i6) 
11  format(3i7,f12.2,6i6,2f8.4,i5,f8.4,i5,i2)
12 format(5i8,f8.2,6i6,2f8.4,4i5,f8.4)
13 format(i12,f8.2,i6,2A16,8i5,f13.4)
14 format(2i16,48 f15.3)
15 format(3i10,f8.2,5i6,f8.4)
16 format(i12,f8.2,i6,2A16,2i5,2i10,4i5,f13.4,i10,i18,i10)
17 format(i12,i8,i3,f8.2,2i6,i4,2i10, 3f10.5) 
18 format('person_id	person_index	trip_index  	tripstarttime 	origmaz    	destmaz	trip_park_ride_flag	otran_maz	dtran_maz	a_time	e_time	tr_time')
19 format('counter  iutmp  idtmp  StartTime  ivcltmp  ivcl2tmp  ihovtmp  veh_pathnodenum  NonrepetitiveCarNumTrip  infotmp  ribftmp  comptmp  TAZMap  value_of_time  1')
20 format(100i7)

21 format(a20,200 i10)
22 format('  Trip Head: TAZMap_of_tripij  ActivityTime   tripmode   orig_purpose   dest_purpose  orig_maz  orig_taz   dest_maz  dest_taz  tripdriverpassengerflag    tripjointflag  tripparkrideflag   starttimeinterval   starttime  abm_trip_index   trip_driver_id   parking_taz')
23 format('  counter,i,Traveller_ID(i),StartTime,numtrips,MappedTAZ,valid_car,first_car_trip,first_transit_trip,value_of_time')
24 format(3i10,f8.2,5i6,f8.4,i8)

25 format(i12,f8.2,i6,2A16,2i5,2i10,4i5,f13.4,i10,i18,i10)
26 format(f11.6) 

27 format(200i8)
28 format(i12,6f17.7,2i4,5i8,2f17.7)
29 format( 2i13,i5,7f12.3)
30 format(4i10,5i8)
160 format(i12,f8.2,2A16,2i5,2i10,4i5,f13.4,i10,i18,i10)

400 format(i3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3,f5.2,f8.2,f8.1,f8.3)

end program Console3
    