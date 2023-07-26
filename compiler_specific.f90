!------------------------------------------------------------------------------
 MODULE mod_CallSystem_Commands
!------------------------------------------------------------------------------
!
!++m* compiler_specific.f90/mod_CallSystem_Commands
!
! NAME 
!   MODULE mod_CallSystem_Commands
!
! PURPOSE
!   This module defines the variables needed for executing commands in directories.
!
! VARIABLES
!   o DirectoryPreC
!   o DirectoryPostC
!   o CommandLinePreC
!   o CommandLinePostC
!   o CommandLineBatchL
!
! FILENAME
!   common/compiler_specific.f90
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=:),ALLOCATABLE :: DirectoryPreC
 CHARACTER(len=:),ALLOCATABLE :: DirectoryPostC
 CHARACTER(len=:),ALLOCATABLE :: CommandLinePreC
 CHARACTER(len=:),ALLOCATABLE :: CommandLinePostC
 LOGICAL                      :: CommandLineBatchL

!------------------------------------------------------------------------------
 END MODULE mod_CallSystem_Commands
!------------------------------------------------------------------------------
!
!
!
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
!   o SUBROUTINE Execute_Command_in_Directory
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
!   o SystemCommandC:      command for command prompt
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
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Execute_Command_in_Directory(DirectoryC,CommandC,output_unit,OperatingSystemC)
!------------------------------------------------------------------------------
!
!++s* mod_CallSystem/Execute_Command_in_Directory
!
! NAME
!   SUBROUTINE Execute_Command_in_Directory
!
! PURPOSE
!    Executes a command in the command line from a specific directory.
!
! USAGE
!   CALL Execute_Command_in_Directory(DirectoryC,CommandC,output_unit,OperatingSystemC)
! 
! INPUT 
!   o DirectoryC:          directory where command should be executed; can be empty
!   o CommandC:            command for command prompt
!   o output_unit:         usually '6' which is default output unit
!   o OperatingSystemC:    'windows'; any other entry assumes Linux-like operating system
!
! OUTPUT
!   none
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: DirectoryC
 CHARACTER(len=*),INTENT(in)  :: CommandC
 INTEGER         ,INTENT(in)  :: output_unit
 CHARACTER(len=*),INTENT(in)  :: OperatingSystemC

 CHARACTER(len=:),ALLOCATABLE :: CommandLineC

 !-----------------------
 ! Execute command line.
 !-----------------------
 IF ( CommandC /= "" ) THEN
    IF ( TRIM(DirectoryC) /= '' ) THEN
       !--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       ! 'cd /d "C:\YourFolder" && myfile.bat'
       ! 'cd /d "C:\YourFolder"': Changes the directory to the specified folder (/d is used to change the drive as well, if the folder is on a different drive).
       ! '&&':                    This is a command separator in Windows. It allows you to execute the next command only if the previous one succeeds (returns with an exit code of 0).
       !--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
     SELECT CASE( TRIM(OperatingSystemC) )
      CASE('windows')
       CommandLineC = 'cd /d "'// TRIM(DirectoryC) // '"' // ' && ' // TRIM(CommandC) ! works on Windows only
      CASE DEFAULT
       CommandLineC = ''                                            // TRIM(CommandC) ! works on Linux and macOS only (CHECK: Add relevant command here suited for Linux/macOS.)
     END SELECT
    ELSE
       CommandLineC =                                                  TRIM(CommandC)
    END IF
    WRITE(output_unit,'(A)') "================================================================================"
    WRITE(output_unit,'(A)') " ==> Executing command line:"
    WRITE(output_unit,'(A)') " "//TRIM(CommandLineC)
    WRITE(output_unit,'(A)') "================================================================================"

    CALL Execute_CommandLine( TRIM(CommandLineC) )

    WRITE(output_unit,'(A)') "================================================================================"
    WRITE(output_unit,'(A)') "DONE. (Executing command line)"
    WRITE(output_unit,'(A)') "================================================================================"
 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE Execute_Command_in_Directory
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_CallSystem
!------------------------------------------------------------------------------
