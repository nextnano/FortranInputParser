!------------------------------------------------------------------------------
 MODULE mod_FolderNames
!------------------------------------------------------------------------------
! The meaning of 'RelativeFolderPathC' is the location of the 'Syntax/' folder
! relative to the executable.
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*),PARAMETER :: FolderNameForDefinitionFilesC    = 'Syntax'  ! to be consistent to nextnano++: folder name for keyword definition files, database, ...
 CHARACTER(len=*),PARAMETER :: RelativeFolderPathC              = '..'      ! ..\Syntax or ../Syntax:
!CHARACTER(len=*),PARAMETER :: RelativeFolderPathC              = '.'       !  .\Syntax or  ./Syntax
 CHARACTER(len=*),PARAMETER :: RelativeFolderPathSameDirectoryC = '.'       !  .\Syntax or  ./Syntax:

!------------------------------------------------------------------------------
 END MODULE mod_FolderNames
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE system_specific_parser
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=30)               :: OperatingSystemC      ! 'windows' (default), 'mac', 'linux', 'unix'

 TYPE :: SystemSpecific
   CHARACTER(len=1)              :: DirectorySeparatorCharC
   CHARACTER(len=10)             :: Directory_ls_C
   CHARACTER(len=10)             :: MakeDirC
   CHARACTER(len=10)             :: PrintWorkingDirC
   CHARACTER(len=80)             :: HomeDirC
 END TYPE SystemSpecific

 TYPE(SystemSpecific)            :: Windows
 TYPE(SystemSpecific)            :: Linux
 TYPE(SystemSpecific)            :: CurrentSystem

 LOGICAL                         :: ParseKeywordsInputFileL = .FALSE.
 LOGICAL                         :: ParseKeywordsDatabaseL  = .FALSE.
 LOGICAL                         :: ParseInputFileOnlyL     = .FALSE.

 LOGICAL                         ::   SetInputFileViaCommandLineL
 LOGICAL                         ::    SetDatabaseViaCommandLineL
 LOGICAL                         :: SetLicenseFileViaCommandLineL
 LOGICAL                         ::   SetOutputDirViaCommandLineL
 LOGICAL                         ::     SetThreadsViaCommandLineL
 LOGICAL                         ::  SetDebugLevelViaCommandLineL

!CHARACTER(len=987)              ::      OutputDir_defaultC           = 'output'                ! ==> Replace 'output' with 'InputFileName_NoDirectoryNoExtensionC'.
 CHARACTER(len=50)               ::      OutputDir_default_IndicatorC = '<name_of_input_file>'  ! ==> Replace 'output' with 'InputFileName_NoDirectoryNoExtensionC'.
 CHARACTER(len=987)              :: InputFileName_DirectoryExtension_C    = ''                  ! name of input file wit     directory and with    file extension. Example: "input/QuantumDot.in" ==> "input/QuantumDot.in"
 CHARACTER(len=987)              :: InputFileName_NoDirectoryNoExtensionC = ''                  ! name of input file without directory and without file extension. Example: "input/QuantumDot.in" ==> "QuantumDot"
 CHARACTER(len=987)              :: InputFileName_NoDirectoryC            = ''                  ! name of input file without directory.                            Example: "input/QuantumDot.in" ==> "QuantumDot.in"

 CHARACTER(len=987)              ::      OutputDirC             = ''             ! name of global output directory, e.g. 'H:\nextnano3\output\test'

 INTEGER                         ::     DebugLevel              = 0              ! '0' = no debug information

 LOGICAL                         :: GenerateLogFileL = .FALSE.     ! .TRUE. if .log file should be written, i.e. if screen output should be written to file.
 CHARACTER(len=987)              :: LogFilenameC                   ! name of .log file

                                  ! NEXTNANO_PathC                = 'C:\Program Files (x86)\nextnano\nextnano3'
 CHARACTER(len=987)              :: NEXTNANO_PathC                = ''        ! can be changed by environment variable. The location where the nextnano software is installed.

 LOGICAL                         :: ExecuteHTCondorL     = .FALSE.
 LOGICAL                         :: ExecutionOnHTCondorL = .FALSE.  ! True, if executable is actually run on HTCondor.

!------------------------------------------------------------------------------
 END MODULE system_specific_parser
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE My_Input_and_Output_Units
!------------------------------------------------------------------------------
!
!++m* parser_definitions.f90/My_Input_and_Output_Units
!
! NAME 
!   MODULE My_Input_and_Output_Units
!
! PURPOSE
!   Subroutines to define standard input and output units.
!
! CONTAINS
!   o SUBROUTINE Define_Input_and_Output_Units
!
! FILENAME
!   FortranInputParser/parser_definitions.f90
!
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 !----------------------------------------------------------------------
 ! Initialize with default values but the values are overwritten later.
 !----------------------------------------------------------------------

 INTEGER           :: my_error_unit        = 0  ! The standard error reporting unit number.
 INTEGER           :: my_input_unit        = 5  ! The standard  input unit number. This is the one used by               READ  with an asterisk ('*') unit.
 INTEGER           :: my_output_unit       = 6  ! The standard output unit number. This is the one used by PRINT, and by WRITE with an asterisk ('*') unit.
 INTEGER,PARAMETER :: output_unit_log_file = 81 ! Note: This number must be UNIQUE. It must not be used elsewhere in the code!!!

!------------------------------------------------------------------------------
 END MODULE My_Input_and_Output_Units
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE Input_and_Output_Units
!------------------------------------------------------------------------------
!
!++m* parser_definitions.f90/Input_and_Output_Units
!
! NAME 
!   MODULE Input_and_Output_Units
!
! PURPOSE
!   Subroutines to define standard input and output units.
!
! CONTAINS
!   o SUBROUTINE   Define_Input_and_Output_Units
!   o SUBROUTINE Redefine_Input_and_Output_Units
!
! FILENAME
!   FortranInputParser/parser_definitions.f90
!
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Redefine_Input_and_Output_Units(new_error_unit,new_input_unit,new_output_unit)
!------------------------------------------------------------------------------
!
!++s* Input_and_Output_Units/Redefine_Input_and_Output_Units
!
! NAME
!   FUNCTION Redefine_Input_and_Output_Units
!
! PURPOSE
!   Redefines standard input and output units.
!
! USAGE
!   CALL Redefine_Input_and_Output_Units(new_error_unit,new_input_unit,new_output_unit)
! 
! INPUT
!   o new_error_unit  (optional)
!   o new_input_unit  (optional)
!   o new_output_unit (optional)
!
! OUTPUT
!   none
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY: my_error_unit , &
                                     my_input_unit , &
                                     my_output_unit

 IMPLICIT NONE

 INTEGER,INTENT(in),OPTIONAL :: new_error_unit
 INTEGER,INTENT(in),OPTIONAL :: new_input_unit
 INTEGER,INTENT(in),OPTIONAL :: new_output_unit

 !----------------------
 ! Take default values.
 !----------------------
 IF ( PRESENT(new_error_unit ) ) my_error_unit  = new_error_unit
 IF ( PRESENT(new_input_unit ) ) my_input_unit  = new_input_unit
 IF ( PRESENT(new_output_unit) ) my_output_unit = new_output_unit

!------------------------------------------------------------------------------
 END SUBROUTINE Redefine_Input_and_Output_Units
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Define_Input_and_Output_Units
!------------------------------------------------------------------------------
!
!
!++s* Input_and_Output_Units/Define_Input_and_Output_Units
!
! NAME
!   FUNCTION Define_Input_and_Output_Units
!
! PURPOSE
!   Defines (or redefines) standard input and output units.
!
! USAGE
!   CALL Define_Input_and_Output_Units
! 
! INPUT
!   none
!
! OUTPUT
!   none
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE,INTRINSIC :: ISO_FORTRAN_ENV,ONLY:error_unit,input_unit,output_unit

 IMPLICIT NONE

!INTEGER,PARAMETER ::  error_unit = 0 ! The standard error reporting unit number.
!INTEGER,PARAMETER ::  input_unit = 5 ! The standard  input unit number. This is the one used by               READ  with an asterisk ('*') unit.
!INTEGER,PARAMETER :: output_unit = 6 ! The standard output unit number. This is the one used by PRINT, and by WRITE with an asterisk ('*') unit.

 !----------------------
 ! Take default values.
 !----------------------
 CALL Redefine_Input_and_Output_Units(error_unit,input_unit,output_unit)

!------------------------------------------------------------------------------
 END SUBROUTINE Define_Input_and_Output_Units
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE Input_and_Output_Units
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_Array_of_Strings
!------------------------------------------------------------------------------

 IMPLICIT NONE

 TYPE  :: String_in_Line
   CHARACTER(len=:),ALLOCATABLE :: StringC
 END TYPE String_in_Line

!------------------------------------------------------------------------------
 END MODULE mod_Array_of_Strings 
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE String_Utility 
!------------------------------------------------------------------------------
!
!++m* general_module_definitions.f90/String_Utility
!
! NAME 
!   MODULE String_Utility
!
! CONTAINS
!   o FUNCTION StringLowerCase
!   o FUNCTION StringUpperCase
!
! FILENAME
!   common/general_module_definitions.f90
!
!##
!   CHECK: It would be useful to have a subroutine that compares strings and that is not case sensitive.
!
!------------------------------------------------------------------------------

 IMPLICIT NONE 

 CONTAINS 

!------------------------------------------------------------------------------
 FUNCTION StringLowerCase( Input_StringC ) RESULT( Output_StringC )
!------------------------------------------------------------------------------
!
!++f* String_Utility/StringLowerCase
!
! NAME
!   FUNCTION StringLowerCase
!
! PURPOSE
!   Converts a string into lower case.
!
! USAGE
!   StringLowerCase(Input_StringC)
! 
! INPUT 
!   o Input_StringC
!
! OUTPUT
!   o Output_StringC
!
! NOTES
!   Compare with SUBROUTINE LowCase (str1, str2) of MODULE fparser.
!   
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)  :: Input_StringC
 CHARACTER(LEN(Input_StringC))             :: Output_StringC  ! RESULT

 INTEGER                                   :: i, n 

 ! List of characters for case conversion 
 CHARACTER(len=*),PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
 CHARACTER(len=*),PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

 !---------------------
 ! Copy input string .
 !---------------------
 Output_StringC = Input_StringC

 !--------------------------------------
 ! Convert case character by character.
 !--------------------------------------
 DO i = 1, LEN(Output_StringC)
    n = INDEX(UPPER_CASE, Output_StringC(i:i))
    IF ( n > 0 ) Output_StringC(i:i) = LOWER_CASE(n:n)
 END DO

!------------------------------------------------------------------------------
 END FUNCTION StringLowerCase 
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION StringUpperCase( Input_StringC ) RESULT( Output_StringC )
!------------------------------------------------------------------------------
!
!++f* String_Utility/StringUpperCase
!
! NAME
!   FUNCTION StringUpperCase
!
! PURPOSE
!   Converts a string into upper case.
!
! USAGE
!   StringUpperCase(Input_StringC)
! 
! INPUT 
!   o Input_StringC
!
! OUTPUT
!   o Output_StringC
!
! NOTES
!   Compare with SUBROUTINE LowCase (str1, str2) of MODULE fparser.
!   
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)  :: Input_StringC
 CHARACTER(LEN(Input_StringC))             :: Output_StringC  ! RESULT

 INTEGER                                   :: i, n 

 ! List of characters for case conversion 
 CHARACTER(len=*),PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
 CHARACTER(len=*),PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

 !---------------------
 ! Copy input string .
 !---------------------
 Output_StringC = Input_StringC

 !--------------------------------------
 ! Convert case character by character.
 !--------------------------------------
 DO i = 1, LEN(Output_StringC)
    n = INDEX(LOWER_CASE, Output_StringC(i:i))
    IF ( n > 0 ) Output_StringC(i:i) = UPPER_CASE(n:n)
 END DO

!------------------------------------------------------------------------------
 END FUNCTION StringUpperCase 
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE String_Utility 
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE SpecialCharacters
!------------------------------------------------------------------------------
! Some compilers treat the backslash character '\' differently.
!   o Intel Fortran compiler:
!     "Treat Backslash as Normal Character in Strings:
!      Treats backslash (\) as a normal graphic character, not an escape character."
!   o PGI Fortran compiler:
!     "Treat Backslash as Character": Yes
!-------------------------------------------------------------------------------

 IMPLICIT NONE
 
!CHARACTER(len=*),PARAMETER ::       BackSlashC = "\"
 CHARACTER(len=*),PARAMETER ::       BackSlashC = '\'

!CHARACTER(len=*),PARAMETER :: DoubleBackSlashC = "\\"
 CHARACTER(len=*),PARAMETER :: DoubleBackSlashC = '\\'

!------------------------------------------------------------------------------
 END MODULE SpecialCharacters
!------------------------------------------------------------------------------
