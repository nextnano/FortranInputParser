!------------------------------------------------------------------------------
 MODULE input_driver_module
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE input_driver(InputFilenameC,DatabaseFilenameC)
!------------------------------------------------------------------------------
 USE mod_input_driver     ,ONLY:InputDriver
 USE mod_collect_database ,ONLY:Collect_Database_Entries
 USE mod_collect_inputfile,ONLY:Collect_Inputfile_Entries
                       
 IMPLICIT NONE

 CHARACTER(len=*)  ,INTENT(in) ::    InputFilenameC
 CHARACTER(len=*)  ,INTENT(in) :: DatabaseFilenameC

 LOGICAL                       :: SetInputFileViaCommandLineL
 LOGICAL                       :: SetDatabaseViaCommandLineL

 SetInputFileViaCommandLineL = .TRUE.
 SetDatabaseViaCommandLineL  = .FALSE.

 CALL InputDriver(    InputFilenameC , SetInputFileViaCommandLineL, &
                   DatabaseFilenameC ,  SetDatabaseViaCommandLineL    )

 CALL Collect_Database_Entries

 CALL Collect_Inputfile_Entries

!------------------------------------------------------------------------------
 END SUBROUTINE input_driver
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE input_driver_module
!------------------------------------------------------------------------------
