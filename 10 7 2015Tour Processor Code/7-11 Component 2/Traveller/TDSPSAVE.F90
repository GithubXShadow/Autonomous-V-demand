Module vector_mod

TYPE ODTATT
  REAL VOT
  REAL COST
  REAL TIME
  REAL DISTANCE
ENDTYPE ODTATT

TYPE ODTMEMBER
  TYPE(ODTATT), POINTER:: P(:)
  INTEGER Psize
ENDTYPE ODTMEMBER

TYPE(ODTMEMBER),ALLOCATABLE:: ODTATT_Array(:,:,:) 
!INTEGER NumOriginZone, NumSuperZone, NumIntervals

TYPE VehiclePath
  integer pathnode
ENDTYPE VehiclePath

TYPE pathmember
  TYPE(VehiclePath), POINTER:: P(:)
  INTEGER Psize
ENDTYPE pathmember

TYPE(pathmember),ALLOCATABLE:: VehiclePath_Array(:) 
CONTAINS



SUBROUTINE ODTATTAllocate(O,D,T,NumVOTGs)
 INTEGER::O,D,T,NumVOTGs
! Allocate(ODTATT_Array(NumOriginZone,NumSuperZone,NumIntervals), stat=error)
 Allocate(ODTATT_Array(O,D,T)%P(NumVOTGs))
 
END SUBROUTINE

SUBROUTINE ODTATTInsert(O,D,T,it,VALUE1,VALUE2,VALUE3,VALUE4)

INTEGER O,D,T,it
REAL VALUE1,VALUE2,VALUE3,VALUE4

ODTATT_Array(O,D,T)%P(it)%VOT=VALUE1
ODTATT_Array(O,D,T)%P(it)%COST=VALUE2
ODTATT_Array(O,D,T)%P(it)%TIME=VALUE3
ODTATT_Array(O,D,T)%P(it)%DISTANCE=VALUE4

END SUBROUTINE

SUBROUTINE VehiclePathAllocate(vehicleindex,veh_pathnodenum)
 INTEGER::vehicleindex,veh_pathnodenum
 Allocate(VehiclePath_Array(vehicleindex)%P(veh_pathnodenum))
 
END SUBROUTINE

SUBROUTINE vehiclepathInsert(vehicleindex,veh_pathnodenum,pathtemp)

INTEGER vehicleindex,veh_pathnodenum
integer,allocatable::pathtemp(:)

do i=1,veh_pathnodenum
    VehiclePath_Array(vehicleindex)%p(i)%pathnode=pathtemp(i)
enddo


END SUBROUTINE

END MODULE