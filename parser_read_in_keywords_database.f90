!------------------------------------------------------------------------------
 MODULE variables_database
!------------------------------------------------------------------------------
!
! Module to store values read in from inputfile.
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 !-------------------------------------------------
 ! These variables are read in from the inputfile.
 !-------------------------------------------------

 TYPE :: type_PhysicsConstants
  REAL(8)                    :: electron_charge
  REAL(8)                    :: Planck_constant
 END TYPE type_PhysicsConstants

 TYPE(type_PhysicsConstants) :: PhysicsConstants

!------------------------------------------------------------------------------
 END MODULE variables_database
!------------------------------------------------------------------------------
!
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_collect_database
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Collect_Database_Entries
!------------------------------------------------------------------------------
!
! To read entries from inputfile.
!
!------------------------------------------------------------------------------
 USE generic_database  ,ONLY:get_from_database
 USE mod_input_data    ,ONLY:type_data
 !-------------------------------------------------
 ! These variables are read in from the inputfile.
 !-------------------------------------------------
 USE variables_database,ONLY:PhysicsConstants
                       
 IMPLICIT NONE

 INTEGER,PARAMETER             :: String_Length = 300
 LOGICAL                       :: newL,continueL,presentL,lastL ! control variables
 CHARACTER(len=String_Length)  :: keywordC,specifierC           ! string variables for keyword and specifier (input of SUBROUTINE get_from_inputfile)
 INTEGER                       :: line
 INTEGER                       :: counter
 TYPE(type_data)               :: value

 !-----------------------------------------------------------------------------
  keywordC = '$physical-constants'
 !-----------------------------------------------------------------------------

   newL = .TRUE.  ; continueL = .FALSE. ; lastL = .FALSE.                     ! new search for keyword
  !----------------------------------------------------------------------------
   specifierC = 'electron-charge'
  !----------------------------------------------------------------------------

  counter = 0                                                                 ! set counter to zero
  DO
   IF (lastL) EXIT                                                            ! Exit if last record was read
    CALL get_from_database(keywordC,newL,specifierC,continueL,value%double,presentL,line,lastL) ! get data
    IF (presentL) THEN
     PhysicsConstants%electron_charge = value%double
     counter = counter + 1                                                     ! count entries to check for unique database
    ELSE
     PhysicsConstants%electron_charge = 0d0
    END IF
    newL = .FALSE. ; continueL = .TRUE.                                       ! stay at records for actual keyword, scan for next specifier entry
  END DO

  IF (counter /= 1) CALL ERROR(1)

  !----------------------------------------------------------------------------
   specifierC = 'Planck-constant'
  !----------------------------------------------------------------------------
   newL= .FALSE. ; continueL = .FALSE.                                        ! stay at records for actual keyword, scan for next specifier entry

    CALL get_from_database(keywordC,newL,specifierC,continueL,value%double,presentL,line,lastL)  ! get data
  ! IF (.NOT. presentL) CALL ERROR
    PhysicsConstants%Planck_constant = value%double

  !----------------------------------------------------------------------------


 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE ERROR(error_number)
!------------------------------------------------------------------------------
 USE Parser_Errors,ONLY:Print_Keyword_Specifier_Line

 IMPLICIT NONE

 INTEGER,INTENT(in)  :: error_number

   WRITE(*,*) "ERROR detected in SUBROUTINE Collect_Database_Entries."

 SELECT CASE(error_number)
  CASE(1)
   WRITE(*,*) "Specifier occurs more than 1 time."
   WRITE(*,*) " value%double = ",value%double
   CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)
  CASE DEFAULT
   WRITE(*,*) "Unknown error number = ",error_number
   STOP
  END SELECT
!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE Collect_Database_Entries
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Write_Database_Entries
!------------------------------------------------------------------------------
!
! Write out entries found in inputfile.
!
!------------------------------------------------------------------------------
 USE variables_database,ONLY:PhysicsConstants

 IMPLICIT NONE


 WRITE(*,'(A)') ""
 WRITE(*,'(A)') "==============================================================================="
 WRITE(*,'(A)') " Content of database:"
 WRITE(*,'(A)') "-------------------------------------------------------------------------------"
 WRITE(*,'(A)') ""

 WRITE(*,*)     " The electron charge is ",PhysicsConstants%electron_charge," [As]."
 WRITE(*,*)     " Planck's constant is   ",PhysicsConstants%Planck_constant," [Js]."

 WRITE(*,'(A)') ""

 WRITE(*,'(A)') "==============================================================================="
 WRITE(*,'(A)') ""

!------------------------------------------------------------------------------
 END SUBROUTINE Write_Database_Entries
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_collect_database
!------------------------------------------------------------------------------
