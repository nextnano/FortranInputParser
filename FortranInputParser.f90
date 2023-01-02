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
 USE input_driver_module   ,ONLY:input_driver
 USE mod_collect_database  ,ONLY:Write_Database_Entries
 USE mod_collect_inputfile ,ONLY:Write_Inputfile_Entries
 USE system_specific_parser,ONLY:ParseInputFileOnlyL, &
                                 WriteCompactFileL
                              
 IMPLICIT NONE

 CHARACTER(len=:),ALLOCATABLE  :: InputFilenameC
 CHARACTER(len=:),ALLOCATABLE  :: DatabaseFilenameC

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
 
 InputFilenameC              = 'inputfile.in'
 DatabaseFilenameC           = 'database.in'
 
 ParseInputFileOnlyL = .TRUE. ! If .TRUE., an .xml     file is written out.
 WriteCompactFileL   = .TRUE. ! If .TRUE., a  .compact file is written out.

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
 
!------------------------------------------------------------------------------
 END PROGRAM FortranInputParser
!------------------------------------------------------------------------------
