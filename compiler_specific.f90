!------------------------------------------------------------------------------
 MODULE mod_CallSystem
!------------------------------------------------------------------------------
!
!++m* compiler_specific.f90/mod_CallSystem
!
! NAME 
!   MODULE mod_CallSystem
!
! PURPOSE
!   This module contains compiler specific subroutines.
!
! CONTAINS
!   o SUBROUTINE CallSystem
!   o SUBROUTINE SystemMemoryUsage
!
! FILENAME
!   common/compiler_specific.f90
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE CallSystem(SystemCommandC)
!------------------------------------------------------------------------------
!
!++s* mod_CallSystem/CallSystem
!
! NAME
!   SUBROUTINE CallSystem
!
! PURPOSE
!  
!
! USAGE
!   CALL CallSystem(SystemCommandC)
! 
! INPUT 
!   o SystemCommandC      command for command prompt
!
! OUTPUT
!   none
!
! NOTES
!   CHECK: CALL EXECUTE_COMMAND_LINE is new Fortran standard instead of CALLSystem.
!
!##
!
!------------------------------------------------------------------------------
!USE f90_unix_proc         ,ONLY:system  ! COMPILER DEPENDENT (NAG): only necessary for Fortran compiler NAG 5.2 on Linux but not for NAG 5.2 on Windows

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: SystemCommandC

 CALL system( TRIM(SystemCommandC) )

!------------------------------------------------------------------------------
 END SUBROUTINE CallSystem
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_CallSystem
!------------------------------------------------------------------------------
