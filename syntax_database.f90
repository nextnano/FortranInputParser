!------------------------------------------------------------------------------
 MODULE mod_keywords_validator_database
!------------------------------------------------------------------------------
!
!++m* syntax_database.f90/mod_keywords_validator_database
!
! NAME 
!   MODULE mod_keywords_validator_database
!
! CONTAINS
!   o SUBROUTINE InputSyntax_Database
!
! FILENAME
!   syntax_database.f90
!
! NOTES
!   This module is similar to MODULE mod_keywords_validator_inputfile.
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE InputSyntax_Database(DebugL,optionC, StringC, filenameC)
!------------------------------------------------------------------------------
!
!++s* mod_keywords_validator_database/InputSyntax_Database
!
! NAME
!   SUBROUTINE InputSyntax_Database
!
! PURPOSE
!   Defines input syntax of database file.
!
! USAGE
!   CALL InputSyntax_Database(DebugL,optionC, StringC)
! 
! INPUT
!   o DebugL:            If .TRUE., print debug information to screen.
!   o optionC:           'default-filename': Returns default filename of database
!                        'write-to-file':    Write syntax definition to file.
!   o filenameC:         (optional) write syntax definition to file
!
! OUTPUT
!   o StringC:           string containing syntax definition or default filename
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units      ,ONLY:my_output_unit

 IMPLICIT NONE

 LOGICAL                     ,INTENT(in)          :: DebugL
 CHARACTER(len=*)            ,INTENT(in)          :: optionC
 CHARACTER(len=*)            ,INTENT(in),OPTIONAL :: filenameC
 CHARACTER(len=:),ALLOCATABLE,INTENT(out)         :: StringC

 INTEGER                                          :: StringLength

 CHARACTER(len=:),ALLOCATABLE                     :: DefaultFilenameC
 LOGICAL                                          :: ReturnDefaultFilenameL
 LOGICAL                                          :: WriteToFileL

 CHARACTER(len=:),ALLOCATABLE                     :: nC ! new line character
 CHARACTER(len=:),ALLOCATABLE                     :: sC ! string

 !------------------------------------------------------------------------- 
 ! Store the 'new line' character which is '\n' in C programming language.
 !------------------------------------------------------------------------- 
 nC = NEW_LINE('n')

 WriteToFileL           = .FALSE.
 ReturnDefaultFilenameL = .FALSE.
     ! DefaultFilenameC = "../Syntax/database.in"
       DefaultFilenameC =     "input/database.in"
     ! DefaultFilenameC =           "database.in"

 SELECT CASE( TRIM(optionC) )
  CASE('default-filename')
   ReturnDefaultFilenameL = .TRUE.
  CASE('write-to-file')
   WriteToFileL           = .TRUE.
  CASE('syntax-definition')
   !----------------------------
   ! Returns syntax definition.
   !----------------------------
  CASE DEFAULT
   WRITE(my_output_unit,'(A)') " Error InputSyntax_Database: optionC ill-defined. optionC = "//TRIM(optionC)
   STOP
 END SELECT

IF ( ReturnDefaultFilenameL ) THEN

 StringC = DefaultFilenameC

ELSE
 sC = ''

 sC=sC// "!------------------------------------------------------------------------------!"//nC
 sC=sC// "$input_filename                                                    optional    !"//nC
!sC=sC// " ../Syntax/database.in                            character        optional    !"//nC
 sC=sC// " " // DefaultFilenameC // &
                           "                                character        optional    !"//nC
 sC=sC// "$end_input_filename                                                optional    !"//nC
 sC=sC// "!------------------------------------------------------------------------------!"//nC
 sC=sC//                                                                                 ""//nC
 sC=sC// "!------------------------------------------------------------------------------!"//nC
 sC=sC// "$physical-constants                                                required    !"//nC
 sC=sC// " electron-charge                                  double           required    !"//nC
 sC=sC// " Planck-constant                                  double           required    !"//nC
 sC=sC// "$end_physical-constants                                            required    !"//nC
 sC=sC// "!------------------------------------------------------------------------------!"//nC

  stringC = sC

  StringLength = LEN_TRIM( StringC )

  IF (DebugL) THEN
   WRITE(my_output_unit,*) " length of string = ",StringLength
  END IF

  IF ( StringC(StringLength:StringLength) /= nC ) THEN
    WRITE(my_output_unit,'(A)') " Error InputSyntax_Database: Line must end with a new line symbol."
    STOP
  END IF

  !----------------------------------
  ! Write syntax definition to file.
  !----------------------------------
  IF (WriteToFileL) THEN
   OPEN(10,file = filenameC)
    WRITE(10,'(A)') StringC
   CLOSE(10)
  END IF

END IF ! If DefaultFilenameL.

!------------------------------------------------------------------------------
 END SUBROUTINE InputSyntax_Database
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_keywords_validator_database
!------------------------------------------------------------------------------
