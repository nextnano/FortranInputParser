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
!   o SUBROUTINE Execute_CommandLine
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
 SUBROUTINE Execute_CommandLine(SystemCommandC)
!------------------------------------------------------------------------------
!
!++s* mod_CallSystem/Execute_CommandLine
!
! NAME
!   SUBROUTINE Execute_CommandLine
!
! PURPOSE
!    Executes a command in the command line.
!
! USAGE
!   CALL Execute_CommandLine(SystemCommandC)
! 
! INPUT 
!   o SystemCommandC      command for command prompt
!
! OUTPUT
!   none
!
!##
!
!------------------------------------------------------------------------------
!USE f90_unix_proc         ,ONLY:system  ! COMPILER DEPENDENT (NAG): only necessary for Fortran compiler NAG 5.2 on Linux but not for NAG 5.2 on Windows
!USE IFPORT                              ! for Intel compiler

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: SystemCommandC

 CHARACTER(len=:),ALLOCATABLE :: CommandC

 CommandC = TRIM(SystemCommandC)

 !----------------------------------------------------------------------------------
 ! CALL EXECUTE_COMMAND_LINE is new Fortran standard instead of 'CALL system(...)'.
 !----------------------------------------------------------------------------------
 CALL execute_command_line( CommandC )  ! should be the method of choice for newer compilers
!CALL system(               CommandC )  ! work-around for older compilers

!------------------------------------------------------------------------------
 END SUBROUTINE Execute_CommandLine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_CallSystem
!------------------------------------------------------------------------------
