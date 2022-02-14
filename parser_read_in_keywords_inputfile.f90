!------------------------------------------------------------------------------
 MODULE variables_inputfile
!------------------------------------------------------------------------------
!
! Module to store values read in from inputfile.
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 !-------------------------------------------------
 ! These variables are read in from the inputfile.
 !-------------------------------------------------

 TYPE :: type_MagneticField
  LOGICAL                 :: onL        = .TRUE.    ! .TRUE./.FALSE.
  REAL(8)                 :: strength   = 0d0       ! Tesla
  INTEGER,DIMENSION(3)    :: directionV = [0,0,1]   ! e.g. 1 0 0, 0 1 0 or 
 END TYPE type_MagneticField

 TYPE(type_MagneticField) :: MagneticField

 TYPE :: type_material
  INTEGER                          :: number
  INTEGER,DIMENSION(:),ALLOCATABLE :: ClusterNumbersV
  CHARACTER(len=:)    ,ALLOCATABLE :: NameC
  CHARACTER(len=:)    ,ALLOCATABLE :: AlloyFunctionC
  REAL(8)                          :: AlloyConcentration
  REAL(8),DIMENSION(:),ALLOCATABLE :: BandGapsV
  CHARACTER(len=:)    ,ALLOCATABLE :: CrystalC
  LOGICAL                          :: from_DatabaseL
 END TYPE type_material

 TYPE(type_material),DIMENSION(:),ALLOCATABLE :: MaterialV

!------------------------------------------------------------------------------
 END MODULE variables_inputfile
!------------------------------------------------------------------------------
!
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_collect_inputfile
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Collect_Inputfile_Entries
!------------------------------------------------------------------------------
!
! To read entries from inputfile.
!
!------------------------------------------------------------------------------
 USE generic_inputfile   ,ONLY:get_from_inputfile
 USE Parser_Tools       ,ONLY:is_yes, &
                              is_no
 USE Parser_Errors      ,ONLY:STOP_UnexpectedNumbers
 USE mod_input_data     ,ONLY:type_data
 !-------------------------------------------------
 ! These variables are read in from the inputfile.
 !-------------------------------------------------
 USE variables_inputfile,ONLY:MagneticField, &
                              MaterialV
                       
 IMPLICIT NONE

 INTEGER,PARAMETER                :: String_Length = 300
 LOGICAL                          :: newL,continueL,presentL,lastL ! control variables
 CHARACTER(len=String_Length)     :: keywordC,specifierC           ! string variables for keyword and specifier (input of SUBROUTINE get_from_inputfile)
 INTEGER                          :: line
 INTEGER                          :: counter
 TYPE(type_data)                  :: value
 LOGICAL,DIMENSION(:),ALLOCATABLE :: logical_vector_maskLV
 INTEGER                          :: expected_size_of_integer_array
 INTEGER                          :: i

 !++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ! Material
 !++++++++++++++++++++++++++++++++++++++++++++++++++++++

 !-----------------------------------------------------------------------------
  keywordC = '$material'
 !-----------------------------------------------------------------------------

   newL = .TRUE.  ; continueL = .FALSE. ; lastL = .FALSE.                      ! new search for keyword
  !----------------------------------------------------------------------------
   specifierC = 'material-number'                                              ! separating specifier for material entries
  !----------------------------------------------------------------------------

  !--------------------------------------------------------------------------------------
  ! First, we count the number of materials, i.e. how often the first specifier appears.
  !--------------------------------------------------------------------------------------
  counter = 0                                                                 ! set counter to zero
    DO 
     IF (lastL) EXIT ! Exit if last record was read.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%int,presentL,line,lastL)
      counter = counter + 1
      newL = .FALSE.  ;  continueL = .TRUE.
    END DO

! WRITE(*,*) " I found ",counter," materials. (",TRIM(specifierC),")"

  !----------------------------------------
  ! Now we know the size and can allocate.
  !----------------------------------------
  ALLOCATE(MaterialV(counter))
  ALLOCATE(logical_vector_maskLV(counter))                                           ! allocate checking array

  newL = .TRUE.   ;  continueL = .FALSE. ; lastL = .FALSE. ; counter = 0             ! search for keyword again
    DO 
     IF (lastL) EXIT ! Exit if last record was read.
     !----------------------------------------------------------------------------
     specifierC = 'material-number'                                                  ! specifier for material-number, get number
     !----------------------------------------------------------------------------
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%int,presentL,line,lastL) ! get data
     IF ( value%int <= 0)               CALL ERROR(1)                                ! INPUT ERROR EXIT
     IF ( value%int > SIZE(MaterialV) ) CALL ERROR(2)                                ! INPUT ERROR EXIT
     counter = counter + 1
     MaterialV(value%int)%number = value%int
     logical_vector_maskLV(value%int) = .TRUE.
     newL = .FALSE.  ;  continueL = .TRUE.                                           ! stay at records for actual keyword, scan for next specifier entry
    END DO
    IF (.NOT. ALL(logical_vector_maskLV)) CALL ERROR(3)                              ! INPUT ERROR EXIT (material numbering must be unique)
    DEALLOCATE(logical_vector_maskLV)                                                ! free up memory used for checking array


    newL = .TRUE.   ;  continueL = .FALSE. ; lastL = .FALSE. ; counter = 0           ! new search for keyword
    DO 
     IF (lastL) EXIT ! Exit if last record was read.
    keywordC   = '$material'                                                         ! scan for entries of keyword $material
     !----------------------------------------------------------------------------
     specifierC = 'material-number'                                                  ! separating specifier for $material entries is material-number
     !----------------------------------------------------------------------------
     counter = counter + 1
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%int,presentL,line,lastL) ! get material numbers

     !----------------------------------------------------------------------------
     specifierC = 'cluster-numbers'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%int_arrayV,presentL,line,lastL)
     ALLOCATE(MaterialV(value%int)%ClusterNumbersV( SIZE(value%int_arrayV)) )
              MaterialV(value%int)%ClusterNumbersV  =    value%int_arrayV

     !----------------------------------------------------------------------------
     specifierC = 'alloy-function'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%stringC,presentL,line,lastL)
     IF (presentL) THEN
      MaterialV(value%int)%AlloyFunctionC = value%stringC
     ELSE
      MaterialV(value%int)%AlloyFunctionC = ''
     END IF

     !----------------------------------------------------------------------------
     specifierC = 'alloy-concentration'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%double,presentL,line,lastL)
     IF (presentL) THEN
      MaterialV(value%int)%AlloyConcentration = value%double
     ELSE
      MaterialV(value%int)%AlloyConcentration = 0d0
     END IF

     !----------------------------------------------------------------------------
     specifierC = 'material-name'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%stringC,presentL,line,lastL)
     MaterialV(value%int)%NameC = value%stringC

     !----------------------------------------------------------------------------
     specifierC = 'crystal-type'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%stringC,presentL,line,lastL)
     IF (presentL) THEN
      MaterialV(value%int)%CrystalC = value%stringC
     ELSE
      MaterialV(value%int)%CrystalC = 'not specified'
     END IF

     !----------------------------------------------------------------------------
     specifierC = 'band-gaps'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%double_arrayV,presentL,line,lastL)

     IF (presentL) THEN
      expected_size_of_integer_array = 3 ! 3 values are expected
      IF ( SIZE(value%double_arrayV) /= expected_size_of_integer_array ) THEN
        CALL STOP_UnexpectedNumbers(keywordC,specifierC,line,expected_size_of_integer_array,value%double_arrayV)
      END IF
      ALLOCATE(MaterialV(value%int)%BandGapsV( SIZE(value%double_arrayV)) )
               MaterialV(value%int)%BandGapsV  =    value%double_arrayV
     END IF

     !----------------------------------------------------------------------------
     specifierC = 'use-material-parameters-from-database'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE.
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%booleanL,presentL,line,lastL)
     IF (presentL) THEN
      MaterialV(value%int)%from_DatabaseL = value%booleanL
     ELSE
      MaterialV(value%int)%from_DatabaseL = .TRUE. ! Use default value.
     END IF

     newL = .FALSE.  ;  continueL = .TRUE.         ! stay at records for $material, scan for next material-number
    END DO


 !++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ! Magnetic field
 ! Direction will be specified like "1 0 0" or "0 1 0".
 !++++++++++++++++++++++++++++++++++++++++++++++++++++++

 newL = .TRUE.   ;  continueL = .FALSE. ; lastL = .FALSE.

 !-----------------------------------------------------------------------------
  keywordC = '$magnetic-field'
 !-----------------------------------------------------------------------------

  DO WHILE (.NOT. lastL)                                                   

     !----------------------------------------------------------------------------
     specifierC = 'magnetic-field-on'
     !----------------------------------------------------------------------------
     CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%stringC,presentL,line,lastL)    
     IF (presentL) THEN                                                           
        IF (      is_yes(value%stringC) ) THEN
           MagneticField%onL = .TRUE.
        ELSE IF ( is_no( value%stringC)  ) THEN
           MagneticField%onL = .FALSE.
        ELSE
           PRINT *,"Input for this flag should be either 'yes' or 'no'."
           PRINT *,"Line:",line
           PRINT *,"Keyword:",keywordC
           PRINT *,"Specifier:",specifierC
           STOP
        END IF

     !----------------------------------------------------------------------------
     specifierC = 'magnetic-field-strength'
     !----------------------------------------------------------------------------
     newL = .FALSE.  ;  continueL = .FALSE. ; specifierC = 'magnetic-field-strength'       
        CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%double,presentL,line,lastL)        
        
        IF (presentL) THEN
           MagneticField%strength = value%double
        ELSE
           PRINT *,"Input for this flag should be a double value."
           PRINT *,"Line:",line
           PRINT *,"Keyword:",keywordC
           PRINT *,"Specifier:",specifierC
           STOP
        END IF


      !----------------------------------------------------------------------------
      specifierC = 'magnetic-field-direction'
      !----------------------------------------------------------------------------
      newL = .FALSE.  ;  continueL = .FALSE. ; specifierC = 'magnetic-field-direction'       
        CALL get_from_inputfile(keywordC,newL,specifierC,continueL,value%int_arrayV,presentL,line,lastL)
        IF (presentL) THEN
           MagneticField%directionV = value%int_arrayV
        ELSE
           PRINT *,"Input for this flag should be an integer array."
           PRINT *,"Line:",line
           PRINT *,"Keyword:",keywordC
           PRINT *,"Specifier:",specifierC
           STOP
        END IF

      END IF ! If 'magnetic-field-on' was present.
      newL = .FALSE.  ;  continueL = .TRUE.                                          
  END DO                                                                 

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE ERROR(error_number)
!------------------------------------------------------------------------------
 USE Parser_Errors,ONLY:Print_Keyword_Specifier_Line

 IMPLICIT NONE

 INTEGER,INTENT(in)  :: error_number

     WRITE(*,*) "ERROR detected in SUBROUTINE Collect_Inputfile_Entries."

 SELECT CASE(error_number)
  CASE(1)
     WRITE(*,*) "You must specify a value > 0."
     WRITE(*,*) "Your wrong entry was: ",value%int
     CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)
  CASE(2)
     WRITE(*,*) "You cannot specify a value that is larger than the number of materials."
     WRITE(*,*) "Your wrong entry was: ",value%int
     CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)
  CASE(3)
     WRITE(*,*) "Your wrong entry was: ",value%int
     CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)
  CASE DEFAULT
   WRITE(*,*)  "Unknown error number = ",error_number
   STOP
  END SELECT
!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE Collect_Inputfile_Entries
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Write_Inputfile_Entries
!------------------------------------------------------------------------------
!
! Write out entries found in inputfile.
!
!------------------------------------------------------------------------------
 USE variables_inputfile,ONLY:MaterialV, &
                              MagneticField

 IMPLICIT NONE

 INTEGER                      :: i

   WRITE(*,'(A)') ""
   WRITE(*,'(A)') "==============================================================================="
   WRITE(*,'(A)') " Content of inputfile:"
   WRITE(*,'(A)') "-------------------------------------------------------------------------------"
   WRITE(*,'(A)') ""

 !----------------
 ! material
 !----------------

    WRITE(*,'(A)') " ------------------------------------------------------------------------------"
    WRITE(*,'(A)') " Material info."
  DO i=1,SIZE(MaterialV)
    WRITE(*,'(A)') " ------------------------------------------------------------------------------"
    WRITE(*,*)     "   material-number                       = ",     MaterialV(i)%number
    WRITE(*,*)     "   cluster-numbers                       = ",     MaterialV(i)%ClusterNumbersV
    WRITE(*,*)     "   material-name                         = ",TRIM(MaterialV(i)%NameC)
   IF ( TRIM(MaterialV(i)%AlloyFunctionC) /= '' ) THEN
    WRITE(*,*)     "   alloy-function                        = ",TRIM(MaterialV(i)%AlloyFunctionC)
    WRITE(*,*)     "   alloy-concentration                   = ",     MaterialV(i)%AlloyConcentration
   END IF
    WRITE(*,*)     "   crystal-type                          = ",TRIM(MaterialV(i)%CrystalC)
    WRITE(*,*)     "   use-material-parameters-from-database = ",     MaterialV(i)%from_DatabaseL
   IF ( ALLOCATED(MaterialV(i)%BandGapsV) ) THEN
    WRITE(*,*)     "   band-gaps                             = ",     MaterialV(i)%BandGapsV
   END IF
  END DO
    WRITE(*,'(A)') " ------------------------------------------------------------------------------"

 !----------------
 ! magnetic field
 !----------------
   WRITE(*,'(A)') ""
   WRITE(*,'(A)') ""

   WRITE(*,'(A)') " ------------------------------------------------------------------------------"
 IF ( MagneticField%onL ) THEN
  WRITE(*,'(A)') " Magnetic field is switched on."
  WRITE(*,*)     "   Its strength is "          ,MagneticField%strength  ," [T]."
  WRITE(*,*)     "   It is oriented along the [",MagneticField%directionV,"] direction."
 ELSE
  WRITE(*,'(A)') " Magnetic field is switched off."
 END IF
   WRITE(*,'(A)') " ------------------------------------------------------------------------------"
   WRITE(*,'(A)') ""

   WRITE(*,'(A)') "==============================================================================="
   WRITE(*,'(A)') ""

!------------------------------------------------------------------------------
 END SUBROUTINE Write_Inputfile_Entries
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_collect_inputfile
!------------------------------------------------------------------------------
