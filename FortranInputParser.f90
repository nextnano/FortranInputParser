!------------------------------------------------------------------------------
 PROGRAM FortranInputParser
!------------------------------------------------------------------------------
!
!++m* FortranInputParser.f90/FortranInputParser
!
! NAME 
!   PROGRAM FortranInputParser
!
! PURPOSE
!   Program to test the Fortran Input Parser.
!
! FILENAME
!   FortranInputParser.f90
!
!##
!
!-----------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE EnvironmentAndSystem     ,ONLY:AssignDefaultOperatingSystem
 USE input_driver_module      ,ONLY:input_driver
 USE mod_collect_database     ,ONLY:Write_Database_Entries
 USE mod_collect_inputfile    ,ONLY:Write_Inputfile_Entries
 USE system_specific_parser   ,ONLY:OperatingSystemC   , &
                                    ParseInputFileOnlyL, &
                                    WriteCompactFileL  , &
                                    Write_INI_FileL    , &
                                    OutputDirC
 USE parser_parameters        ,ONLY:DATA_FileC
 USE mod_CallSystem           ,ONLY:Execute_Command_in_Directory
 USE variables_inputfile      ,ONLY:CommandLineBatchL
                              
 IMPLICIT NONE

 CHARACTER(len=:),ALLOCATABLE  :: InputFilenameC
 CHARACTER(len=:),ALLOCATABLE  :: DatabaseFilenameC

 CHARACTER(len=:),ALLOCATABLE  :: commandC
 CHARACTER(len=:),ALLOCATABLE  :: Directory_for_execute_commandC

 WRITE(*,'(A)') "============================================================"
 WRITE(*,'(A)') " Fortran Input Parser"
 WRITE(*,'(A)') "============================================================"
 WRITE(*,'(A)') ""
 WRITE(*,'(A)') "------------------------------------------------------------"
 WRITE(*,'(A)') " (c) nextnano GmbH (BSD license)"
 WRITE(*,'(A)') "------------------------------------------------------------"
 WRITE(*,'(A)') ""
 WRITE(*,'(A)') " This example programs reads in the following two files:"
 WRITE(*,'(A)') "   - database.in"
 WRITE(*,'(A)') "   - inputfile.in"
 WRITE(*,'(A)') " It parses the data and stores it in variables."
 WRITE(*,'(A)') " It demonstrates the features of this Fortran Input Parser."
 WRITE(*,'(A)') ""
 WRITE(*,'(A)') "------------------------------------------------------------"
 WRITE(*,'(A)') ""

!OperatingSystemC = 'windows' ! defines backslash or slash ==> backslash "/" for directories
 OperatingSystemC = 'linux'   ! defines backslash or slash ==>     slash "\" for directories

 OutputDirC = 'output/'      ! Define name of output directory

 InputFilenameC              = 'input/inputfile.in'
 DatabaseFilenameC           = 'input/database.in'
 
 ParseInputFileOnlyL = .TRUE. ! If .TRUE., an .xml     file is written out.
 WriteCompactFileL   = .TRUE. ! If .TRUE., a  .compact file is written out.
 Write_INI_FileL     = .TRUE. ! If .TRUE., a  .ini     file is written out.

 !--------------------------
 ! Update operating system.
 !--------------------------
 CALL AssignDefaultOperatingSystem(OperatingSystemC)
 
 !--------------------------------
 ! Call input driver and read in:
 !  - database
 !  - inputfile
 !--------------------------------
 CALL input_driver(InputFilenameC,DatabaseFilenameC)

 !--------------------------------
 ! Print variables of database.
 !--------------------------------
 CALL Write_Database_Entries

 !--------------------------------
 ! Print variables of inputfile.
 !--------------------------------
 CALL Write_Inputfile_Entries

  WRITE(*,'(A)') ""
  WRITE(*,'(A)') "------------------------------------------------------------"
  WRITE(*,'(A)') " Input files have been parsed successfully."
  WRITE(*,'(A)') "------------------------------------------------------------"
  WRITE(*,'(A)') ""

 !------------------------------------
 ! Execute command line (==> '!DATA')
 !------------------------------------
 IF ( CommandLineBatchL ) THEN
    !------------------------------------------------
    ! Always execute command in local output folder.
    !------------------------------------------------
  IF ( DATA_FILEC /= '') THEN ! If file is not present, it has not been written. Then there is no need for post-processing.
    Directory_for_execute_commandC = ''

    IF ( TRIM(OperatingSystemC) /= 'windows' ) THEN

     !-----------------------------------------------
     ! Make sure that script has execute permission.
     !-----------------------------------------------
     commandC = 'chmod a+x '//'"'//TRIM(DATA_FileC)//'"'

     CALL Execute_Command_in_Directory(Directory_for_execute_commandC,commandC, &
                                       my_output_unit,OperatingSystemC)
    END IF

     commandC =               '"'//TRIM(DATA_FileC)//'"'
     CALL Execute_Command_in_Directory(Directory_for_execute_commandC,commandC, &
                                       my_output_unit,OperatingSystemC)
  END IF
 ELSE
  WRITE(*,'(A)') ""
  WRITE(*,'(A)') "------------------------------------------------------------"
  WRITE(*,'(A)') " Batch file has not been executed."
  WRITE(*,'(A)') "------------------------------------------------------------"
  WRITE(*,'(A)') ""
 END IF
 
!------------------------------------------------------------------------------
 END PROGRAM FortranInputParser
!------------------------------------------------------------------------------
