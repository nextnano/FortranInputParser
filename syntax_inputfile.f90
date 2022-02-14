!------------------------------------------------------------------------------
 MODULE mod_keywords_validator_inputfile
!------------------------------------------------------------------------------
!
!++m* syntax_inputfile.f90/mod_keywords_validator_inputfile
!
! NAME 
!   MODULE mod_keywords_validator_inputfile
!
! CONTAINS
!   o SUBROUTINE InputSyntax_InputFile
!
! FILENAME
!   syntax_input_inputfile.f90
!
! NOTES
!   This module is similar to MODULE mod_keywords_validator_database.
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE InputSyntax_InputFile(DebugL,optionC, stringC, filenameC)
!------------------------------------------------------------------------------
!
!++s* mod_keywords_validator_inputfile/InputSyntax_InputFile
!
! NAME
!   SUBROUTINE InputSyntax_InputFile
!
! PURPOSE
!   Defines input syntax of input file.
!
! USAGE
!   CALL InputSyntax_InputFile(DebugL,optionC, stringC, filenameC)
! 
! INPUT
!   o DebugL:            If .TRUE., print debug information to screen.
!   o optionC:           'default-filename': Returns default filename of input file
!                        'write-to-file':    Write syntax definition to file.
!   o filenameC:         (optional) write syntax definition to file
!
! OUTPUT
!   o stringC:           string containing syntax definition or default filename
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units      ,ONLY:my_output_unit

 IMPLICIT NONE

 LOGICAL                     ,INTENT(in)          :: DebugL
 CHARACTER(len=*)            ,INTENT(in)          :: optionC
 CHARACTER(len=*)            ,INTENT(in),OPTIONAL :: filenameC
 CHARACTER(len=:),ALLOCATABLE,INTENT(out)         :: stringC

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
       DefaultFilenameC = "inputfile.in"

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
   WRITE(my_output_unit,'(A)') " Error InputSyntax_InputFile: optionC ill-defined. optionC = "//TRIM(optionC)
   STOP
 END SELECT

IF ( ReturnDefaultFilenameL ) THEN

 stringC = DefaultFilenameC

ELSE
 sC=''

 sC=sC// "!-----------------------------------------------------------------------------!"//nC
 sC=sC// "! This must be the first keyword. Do not change the order."//nC
 sC=sC// "!-----------------------------------------------------------------------------!"//nC
 sC=sC// "$input_filename                                                    optional   !"// &
              " Don't change this. This must be the first keyword in this file. Do not change the order!"//nC
!sC=sC// " inputfile.in.in                                  character        optional   !"// &
!             " Do not change this. This must be the first specifier in case no input file is specified."//nC
 sC=sC// " "//DefaultFilenameC//    "                       character        optional   !"// &
              " Do not change this. This must be the first specifier in case no input file is specified."//nC
 sC=sC// "$end_input_filename                                                optional   !"//nC
 sC=sC// "!-----------------------------------------------------------------------------!"//nC
 sC=sC// "! End of first keyword. Now the order does not matter."//nC
 sC=sC// "!-----------------------------------------------------------------------------!"//nC
 sC=sC//                                                                                ""//nC
 sC=sC// "!-----------------------------------------------------------------------------!"//nC
 sC=sC// "$magnetic-field                                  optional                     !"//nC
 sC=sC// " magnetic-field-on                               character        required      CHOICE[yes,no]"//nC
 sC=sC// " magnetic-field-strength                         double           required    !"//nC
 sC=sC// " magnetic-field-direction                        integer_array    required      CHOICE[1 0 0,0 1 0,0 0 1]"//nC
 sC=sC// "$end_magnetic-field                              optional                     !"//nC
 sC=sC// "!-----------------------------------------------------------------------------!"//nC
 sC=sC//                                                                                ""//nC
 sC=sC// "!-----------------------------------------------------------------------------!"//nC
 sC=sC// "$material                                                         required    !"//nC
 sC=sC// " material-number                                 integer          required    !"// &
              " first entry is separator for new input sequence"//nC
 sC=sC// " cluster-numbers                                 integer_array    required    !"//nC
 sC=sC// " material-name                                   character        required    !"//nC
 sC=sC// " alloy-function                                  character        optional      CHOICE[constant,linear]"//nC
 sC=sC// " alloy-concentration                             double           optional    !"//nC
 sC=sC// " band-gaps                                       double_array     optional    !"//nC
 sC=sC// " crystal-type                                    character        optional      CHOICE[zincblende,wurtzite]"//nC
 sC=sC// " use-material-parameters-from-database           logical          optional      CHOICE[.TRUE.,.FALSE.]"//nC
 sC=sC// "$end_material                                                     required    !"//nC
 sC=sC// "!-----------------------------------------------------------------------------!"//nC

  stringC = sC

  StringLength = LEN_TRIM( stringC )

  IF (DebugL) THEN
   WRITE(my_output_unit,*) " length of string = ",StringLength
  END IF

  IF ( stringC(StringLength:StringLength) /= nC ) THEN
    WRITE(my_output_unit,'(A)') " Error InputSyntax_InputFile: Line must end with a new line symbol."
    STOP
  END IF

  !----------------------------------
  ! Write syntax definition to file.
  !----------------------------------
  IF (WriteToFileL) THEN
   OPEN(10,file = filenameC)
    WRITE(10,'(A)') stringC
   CLOSE(10)
  END IF

 END IF ! If DefaultFilenameL.

!------------------------------------------------------------------------------
 END SUBROUTINE InputSyntax_InputFile
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_keywords_validator_inputfile
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_syntax_validator
!------------------------------------------------------------------------------
!
!++m* syntax_input_inputfile.f90/mod_syntax_validator
!
! NAME 
!   MODULE mod_syntax_validator
!
! CONTAINS
!   o SUBROUTINE InputSyntax
!
! FILENAME
!   input_parser/syntax_input_inputfile.f90
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE InputSyntax(kind_of_fileC,DebugL,optionC, stringC, filenameC)
!------------------------------------------------------------------------------
!
!++s* mod_keywords_validator_inputfile/InputSyntax
!
! NAME
!   SUBROUTINE InputSyntax
!
! PURPOSE
!   Defines input syntax of input file.
!
! USAGE
!   CALL InputSyntax(kind_of_fileC,DebugL,optionC, stringC, filenameC)
! 
! INPUT
!   o kind_of_fileC:     'inputfile', 'database'
!   o DebugL:            If .TRUE., print debug information to screen.
!   o optionC:           'default-filename': Returns default filename of input file
!                        'write-to-file':    Write syntax definition to file.
!   o filenameC:         (optional) write syntax definition to file
!
! OUTPUT
!   o stringC:           string containing syntax definition or default filename
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units       ,ONLY:my_output_unit
 USE mod_keywords_validator_inputfile,ONLY:InputSyntax_InputFile
 USE mod_keywords_validator_database ,ONLY:InputSyntax_Database

 IMPLICIT NONE

 CHARACTER(len=*)            ,INTENT(in)          :: kind_of_fileC
 LOGICAL                     ,INTENT(in)          :: DebugL
 CHARACTER(len=*)            ,INTENT(in)          :: optionC
 CHARACTER(len=*)            ,INTENT(in),OPTIONAL :: filenameC
 CHARACTER(len=:),ALLOCATABLE,INTENT(out)         :: stringC

 SELECT CASE( TRIM(kind_of_fileC) )
  CASE('inputfile')
   IF ( PRESENT(filenameC) ) THEN
    CALL InputSyntax_InputFile(DebugL,optionC, stringC, filenameC)
   ELSE
    CALL InputSyntax_InputFile(DebugL,optionC, stringC)
   END IF
  CASE('database')
   IF ( PRESENT(filenameC) ) THEN
    CALL InputSyntax_Database( DebugL,optionC, stringC, filenameC)
   ELSE
    CALL InputSyntax_Database( DebugL,optionC, stringC)
   END IF
  CASE DEFAULT
   WRITE(my_output_unit,'(A)') " Error InputSyntax: kind_of_fileC ill-defined. kind_of_fileC = ",TRIM(kind_of_fileC)
   STOP
 END SELECT

!------------------------------------------------------------------------------
 END SUBROUTINE InputSyntax
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_syntax_validator
!------------------------------------------------------------------------------
