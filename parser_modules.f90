!------------------------------------------------------------------------------
!  o MODULE DirectoryFileExist
!  o MODULE mod_SyntaxFolder
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE DirectoryFileExist
!------------------------------------------------------------------------------
!
!++m* general_modules.f90/DirectoryFileExist
!
! NAME 
!   MODULE DirectoryFileExist
!
! PURPOSE
!   Subroutines to check if file or directory exist.
!
! CONTAINS
!   o SUBROUTINE DeleteFile
!   o FUNCTION file_existsL
!   o SUBROUTINE          FileExistREAD (           filenameC)
!   o SUBROUTINE DirectoryFileExistREAD (directoryC,filenameC)
!   o FUNCTION directory_existsL
!   o SUBROUTINE DirectoryExist         (directoryC         )
!   o SUBROUTINE CreateDirectory
!   o FUNCTION Position_of_last_slash
!   o FUNCTION FileName_Without_Directory
!   o FUNCTION FileName_Without_FileExtension
!   o SUBROUTINE Add_Slash_To_DirectoryName
!   o SUBROUTINE ReplaceSlashes
!   o SUBROUTINE          FileExistWRITE(           filenameC)
!   o SUBROUTINE ErrorMessage
!   o FUNCTION SetGlobalDirectoryName
!   o FUNCTION GetGlobalDirectoryName
!   o FUNCTION GetTotalDirectoryName_C
!   o FUNCTION GetDirectoryName_int_to_char000
!   o SUBROUTINE CountLinesInFile
!   o SUBROUTINE ReadFileAndStoreLines
!   o SUBROUTINE CopyFile
!
! FILENAME
!   common/general_modules.f90
!
! NOTES
!   o Status="OLD"        It is an error if the file does not exist before the OPEN is executed.
!   o Status="NEW"        It is an error if the file exists already before the OPEN is executed.
!   o Status="REPLACE"    - If file does not exist, it is created.
!                         - If file exists, it is deleted and created anew.
!
!##
!
!-----------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS
 
!------------------------------------------------------------------------------
 SUBROUTINE DeleteFile(filenameC)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/DeleteFile
!
! NAME
!   SUBROUTINE DeleteFile
!
! PURPOSE
!   Deletes a file.
!
! USAGE
!   CALL DeleteFile(filenameC)
! 
! INPUT
!   o filenameC
!
! OUTPUT
!   none
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:DebugLevel

 IMPLICIT NONE
 
 CHARACTER(len=*),INTENT(in)          :: filenameC

 INTEGER                              :: status

 !--------------
 ! Delete file.
 !--------------
  OPEN (UNIT=10,FILE=filenameC,STATUS='OLD',IOSTAT=status)
 IF (status == 0) THEN
  IF (DebugLevel > 5) THEN
   WRITE(my_output_unit,'(A,A)') " Delete File: ",TRIM(filenameC)
  END IF
  CLOSE(UNIT=10,               STATUS='DELETE')
 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE DeleteFile
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION file_existsL(filenameC) RESULT(FileExistsL)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/file_existsL
!
! NAME
!   FUNCTION file_existsL
!
! PURPOSE
!   Check if file exists.
!   If yes, flag 'FileExistsL' is .TRUE.
!   If no , flag 'FileExistsL' is .FALSE.
!
! USAGE
!   file_existsL(filenameC)
! 
! INPUT
!   o filenameC
!
! OUTPUT
!   o FileExistsL
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 
 CHARACTER(len=*),INTENT(in)          :: filenameC
 LOGICAL                              :: FileExistsL   ! RESULT

 LOGICAL                              :: it_existsL

 !-----------------------------------------------------------
 ! The INQUIRE statement determines whether the file exists.
 !-----------------------------------------------------------
 INQUIRE (FILE = filenameC, EXIST = it_existsL)
 
 IF (it_existsL) THEN
     FileExistsL = .TRUE.
 ELSE
     FileExistsL = .FALSE.
   ! WRITE(my_output_unit,'(2A/)') " >> Cannot find file = ", TRIM(filenameC)
   ! IF (PRESENT(error)) THEN
   !    CALL ErrorMessage(error)
   ! END IF
 END IF

!------------------------------------------------------------------------------
 END FUNCTION file_existsL
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE FileExistREAD(filenameC,error,ErrorMessageC)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/FileExistREAD
!
! NAME
!   SUBROUTINE FileExistREAD
!
! PURPOSE
!   Checks if file exists.
!   If it does not exist, an error is printed.
!
! USAGE
!   CALL FileExistREAD(filenameC,error,ErrorMessageC)
! 
! INPUT
!   o filenameC
!   o error          (optional)
!   o ErrorMessageC  (optional)
!
! OUTPUT
!   none
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE
 
 CHARACTER(len=*),INTENT(in)          :: filenameC
 INTEGER         ,INTENT(in),OPTIONAL :: error
 CHARACTER(len=*),INTENT(in),OPTIONAL :: ErrorMessageC

 IF ( .NOT. file_existsL(filenameC) ) THEN
   WRITE(my_output_unit,'(A,A,A)') " Error FileExistREAD: File ",TRIM(filenameC)," does not exist."
   WRITE(my_output_unit,'(A)')     " Please check if either directory or filename is wrong."
   WRITE(my_output_unit,'(A)')     ' Note: Blanks are allowed in filename if the filename is between quotation marks'// &
                                   ' "<filename>".'
   WRITE(my_output_unit,'(A)')     "       Blanks are allowed in filename if the filename is between apostrophes    "// &
                                   " '<filename>'."

  IF ( PRESENT(ErrorMessageC) ) THEN
   WRITE(my_output_unit,'(1x,A)') TRIM(ErrorMessageC)
  END IF

  IF ( PRESENT(error)         ) THEN
     CALL ErrorMessage(error)
  END IF

  STOP
 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE FileExistREAD
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE DirectoryFileExistREAD(directoryC,filenameC)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/DirectoryFileExistREAD
!
! NAME
!   SUBROUTINE DirectoryFileExistREAD
!
! PURPOSE
!   Checks if file 'filenameC' exists in directory 'directoryC'.
!   If it does not exist, an error is printed.
!
! USAGE
!   CALL DirectoryFileExistREAD(directoryC,filenameC)
! 
! INPUT
!   o directoryC
!   o filenameC
!
! OUTPUT
!   none
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE
 
 CHARACTER(len=*), INTENT(in) :: directoryC
 CHARACTER(len=*), INTENT(in) :: filenameC

 CHARACTER(len=:),ALLOCATABLE :: filename2C

 filename2C = TRIM(directoryC)//TRIM(filenameC)

 IF ( .NOT. file_existsL(filename2C) ) THEN
  WRITE(my_output_unit,'(A,A,A)') " Error DirectoryFileExistREAD: File ",TRIM(filenameC)," does"
  WRITE(my_output_unit,'(A,A)')   " not exist in directory: ",TRIM(directoryC)
  WRITE(my_output_unit,'(A)')     " Please check if either directory or filename is wrong."
  WRITE(my_output_unit,'(A)')     ' Note: Blanks are allowed in filename if the filename is between quotation marks'// &
                                  ' "<filename>".'
  WRITE(my_output_unit,'(A)')     "       Blanks are allowed in filename if the filename is between apostrophes    "// &
                                  " '<filename>'."
  STOP
 END IF

!-----------------------------------------------------------------
 END SUBROUTINE DirectoryFileExistREAD
!-----------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION directory_existsL(directoryC) RESULT(DirectoryExistsL)
!------------------------------------------------------------------------------
!
!++f* FUNCTION/directory_existsL
!
! NAME
!   SUBROUTINE directory_existsL
!
! PURPOSE
!   Checks if directory exists.
!   If yes, flag 'DirectoryExistsL' is .TRUE.
!   If no , flag 'DirectoryExistsL' is .FALSE.
!
! USAGE
!   directory_existsL(directoryC, DirectoryExistsL)
! 
! INPUT
!   o directoryC
!
! OUTPUT
!   o DirectoryExistsL
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:DebugLevel

 IMPLICIT NONE
 
 CHARACTER(len=*),INTENT(in)  :: directoryC
 LOGICAL                      :: DirectoryExistsL  ! RESULT

 INTEGER                      :: ios
 CHARACTER(len=:),ALLOCATABLE :: filenameC
 LOGICAL                      :: NewRoutineL
 INTEGER,PARAMETER            :: DebugNumber = 123
 

 IF ( DebugLevel == DebugNumber) THEN ! For testing
      NewRoutineL = .TRUE.    ! It seems that this is still not working with Intel Fortran compiler on Windows. (2020-01-20)
 ELSE
      NewRoutineL = .FALSE.
 END IF

 IF (NewRoutineL) THEN 
   !------------------------------------------------------------------------------
   ! On Windows and Linux/UNIX/OSX a directory contains a special file named ".".
   !------------------------------------------------------------------------------
 ! INQUIRE( FILE = 'mydirectory/.'      , EXIST = DirectoryExistsL )
   INQUIRE( FILE = TRIM(directoryC)//'.', EXIST = DirectoryExistsL ) ! CurrentSystem%DirectorySeparatorCharC is already contained in 'directoryC'.

  IF ( DebugLevel == DebugNumber ) THEN
   WRITE(my_output_unit,*) "Check existence of directoryC = ",TRIM(directoryC)//'.'
   WRITE(my_output_unit,*) "DirectoryExistsL = ", DirectoryExistsL
  END IF
 ELSE

  !----------------------------------------------------------------
  ! Note: This file will be deleted at the end of this subroutine.
  ! CHECK: What happens if this directory is write protected?
  !----------------------------------------------------------------
  filenameC = TRIM(directoryC)//'temp_file'
  OPEN(UNIT=8,FILE=filenameC,IOSTAT=ios,STATUS="REPLACE",ACTION="WRITE")

   IF (ios /= 0) THEN
      DirectoryExistsL = .FALSE.
   ELSE
      DirectoryExistsL = .TRUE.
   END IF

  !-----------------------------------------------------------
  ! Delete file 'test.dat' because it is not needed any more.
  !-----------------------------------------------------------
  CLOSE(UNIT=8,STATUS='DELETE')

 END IF

 IF (.NOT. DirectoryExistsL) THEN
  WRITE(my_output_unit,'(A,A,A)') " The directory '",TRIM(directoryC),"' does not exist yet."
 END IF

!------------------------------------------------------------------------------
 END FUNCTION directory_existsL
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE DirectoryExist(directoryC)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/DirectoryExist
!
! NAME
!   SUBROUTINE DirectoryExist
!
! PURPOSE
!   Checks if directory exists.
!   If not, directory is created, and it is checked if directory
!   had been created successfully.
!
! USAGE
!   CALL DirectoryExist(directoryC)
! 
! INPUT
!   o directoryC
!
! OUTPUT
!   none
! 
! NOTES
!   CHECK: IF a slash or backslash is not present, how does this affect this subroutine?
!          Shall the slash or backslash be added automatically in this case?
!   
!##
!
!-----------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE
 
 CHARACTER(len=*),INTENT(in)          :: directoryC

    !------------------------------------
    ! Check if directory exists already.
    !------------------------------------
    IF ( directory_existsL(directoryC) ) THEN
     !-------------------------------------------------------------------------
     ! If .TRUE., then directory exists. Everything is fine. So let's return.
     !-------------------------------------------------------------------------
     RETURN
    ELSE
     !-------------------------------------------------------------------------
     ! If .FALSE., then create directory.
     !-------------------------------------------------------------------------
     CALL CreateDirectory(directoryC)

     !-------------------------------------------------------------------------
     ! Here, I guess the program should wait for some time until the creation
     ! of the directory has finished.
     !-------------------------------------------------------------------------

     !---------------------------------------------------
     ! Check if directory has been created successfully.
     !---------------------------------------------------
     IF ( directory_existsL(directoryC) ) THEN
      !------------------------------------------------------------------------
      ! If .TRUE., then directory exists. Everything is fine. So let's return.
      !------------------------------------------------------------------------
      RETURN
     ELSE
       WRITE(my_output_unit,'(A)')    " Error DirectoryExist: I was not successful in creating this directory: "
       WRITE(my_output_unit,'(1x,A)')   TRIM(directoryC)
       WRITE(my_output_unit,'(A)')    " Please create it manually and start the executable again."
       STOP
     END IF
    END IF

!------------------------------------------------------------------------------
 END SUBROUTINE DirectoryExist
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE CreateDirectory(directory_inC)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/CreateDirectory
!
! NAME
!   SUBROUTINE CreateDirectory
!
! PURPOSE
!   Creates directory.
!
! USAGE
!   CALL CreateDirectory(directory_inC)
! 
! INPUT
!   o directory_inC
!
! OUTPUT
!   none
! 
! NOTES
!   Program can now create a directory like this:
!   'simulations/1D/test/strain/' (i.e. including subdirectories)
!
!   The slashes are modified if necessary, e.g.
!   'current/'
!   'current\'
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_CallSystem           ,ONLY:CallSystem
 USE system_specific_parser   ,ONLY:Windows,Linux,CurrentSystem
 USE SpecialCharacters        ,ONLY:BackSlashC
 USE CharacterManipulation    ,ONLY:CharacterReplace

 IMPLICIT NONE
 
 CHARACTER(len=*),INTENT(in)  :: directory_inC  ! Input

 INTEGER                      :: last
 CHARACTER(len=987)           :: SystemCommandC
 CHARACTER(len=987)           :: directoryC
 CHARACTER(len=987)           :: directory_for_mkdirC
!CHARACTER(len=:),ALLOCATABLE :: SystemCommandC
!CHARACTER(len=:),ALLOCATABLE :: directoryC
!CHARACTER(len=:),ALLOCATABLE :: directory_for_mkdirC
 CHARACTER(len=1)             :: char
 INTEGER                      :: directoryC_StringLength

 directoryC = TRIM(directory_inC)

 WRITE(my_output_unit,'(A,A,A)') " Trying to create:   '",TRIM(directoryC),"'"

 !---------------------------------------------------------------
 ! Get string length (i.e. number of characters of 'directoryC'.
 !---------------------------------------------------------------
 directoryC_StringLength = LEN_TRIM(directoryC)
       
 !-------------------------------------------------------
 ! Find position of last character '/' or '\' in string.
 !-------------------------------------------------------
 last = Position_of_last_slash(directoryC)

 IF (last == 0) THEN
    WRITE(my_output_unit,*) "Warning CreateDirectory:"
    WRITE(my_output_unit,*) "It seems that no slash '/' or backslash '",BackSlashC,"' was present in the directory name."
    WRITE(my_output_unit,*) "directory_inC = ",TRIM(directory_inC)
 END IF

 !------------------------------------------------------------------------------------------------
 ! Replace in string 'stringC' the character 'CharacterBeforeC' with character 'CharacterAfterC'.
 ! CALL CharacterReplace (stringC, CharacterBeforeC, CharacterAfterC )
 ! CurrentSystem%DirectorySeparatorCharC = '/' or '\'
 !------------------------------------------------------------------------------------------------
 IF      (CurrentSystem%DirectorySeparatorCharC == Windows%DirectorySeparatorCharC) THEN
    !-------------------------------------------
    ! Replace in 'directoryC' all '/' with '\'.
    !-------------------------------------------
    CALL CharacterReplace (directoryC,               Linux%DirectorySeparatorCharC , CurrentSystem%DirectorySeparatorCharC )
 ELSE IF (CurrentSystem%DirectorySeparatorCharC ==   Linux%DirectorySeparatorCharC) THEN
    !-------------------------------------------
    ! Replace in 'directoryC' all '\' with '/'.
    !-------------------------------------------
    CALL CharacterReplace (directoryC,             Windows%DirectorySeparatorCharC , CurrentSystem%DirectorySeparatorCharC )
 ELSE
   WRITE(my_output_unit,*) "Error CreateDirectory: This should not happen."
   STOP
 END IF

 !------------------------------------------------------
 ! Take string 'directoryC' without the last slash '/'.
 ! This is achieved by using 'last-1'.
 ! Create directory, e.g. 'mkdir directoryC'.
 !------------------------------------------------------
 IF (last /= 0) THEN
  char = directoryC(last:last) ! 'last' must be larger than zero.

  IF (char /= CurrentSystem%DirectorySeparatorCharC) THEN
   WRITE(my_output_unit,'(A,A,A,A)')  " Warning CreateDirectory:", char, "/=" ,CurrentSystem%DirectorySeparatorCharC
   WRITE(my_output_unit,'(A,A,A)')    " Last character of directory name should be equal to slash '/' or backslash '", &
                                        BackSlashC,"'."
   WRITE(my_output_unit,'(A,A)')      "  directory name = ",TRIM(directoryC)
   WRITE(my_output_unit,'(A,A,A,I5)') "  last character = ", char," at position ",last
  END IF

  IF (last /= directoryC_StringLength) THEN
   WRITE(my_output_unit,'(A)')      " Warning CreateDirectory:"
   WRITE(my_output_unit,'(A,A,A)')  " Last character of directory name should be equal to slash '/' or backslash '", &
                                      BackSlashC,"'."
   WRITE(my_output_unit,'(A,A,A)')  "  directory name = ",TRIM(directoryC)       ,"    (directory that should be created)"
   WRITE(my_output_unit,'(A,A,A)')  "  directory name = ",     directoryC(1:last),"    (directory that will be created)"
   WRITE(my_output_unit,'(A,I5)')   "  string length of directory name = ",directoryC_StringLength
   WRITE(my_output_unit,'(A,I5,A)') "  But I only use the first ",last," characters."
  END IF

       END IF

       directory_for_mkdirC = TRIM(directoryC(1:last-1))
       CALL Add_Slash_To_DirectoryName(directory_for_mkdirC) ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]
     ! SystemCommandC       = TRIM(CurrentSystem%MakeDirC)//' '     //TRIM(directory_for_mkdirC)
       SystemCommandC       = TRIM(CurrentSystem%MakeDirC)//' '//'"'//TRIM(directory_for_mkdirC)//'"' ! "my folder" is possible now, i.e. including blanks.

       !---------------------------------------------------------------------------
       ! If the correct slash is contained in 'directoryC', then this should work. BUT IT DOES NOT WORK!
       !---------------------------------------------------------------------------
     ! SystemCommandC       = TRIM(CurrentSystem%MakeDirC)//' '     //TRIM(directoryC(1:last))


       !---------------------------------------------------------------------
       ! Now call the system (e.g. cmd.exe on Windows) and create directory.
       !---------------------------------------------------------------------
       WRITE(my_output_unit,'(A,A,A)') " CALL system: '",TRIM(SystemCommandC),"'"
 ! CHECK: CALL EXECUTE_COMMAND_LINE is new Fortran standard instead of CALLSystem.
                             CALL CallSystem     (SystemCommandC)
       WRITE(my_output_unit,'(A,A,A)') " CALL system: '",TRIM(SystemCommandC),"' finished."

!------------------------------------------------------------------------------
 END SUBROUTINE CreateDirectory
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION Position_of_last_slash(stringC) RESULT (index_of_last)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/Position_of_last_slash
!
! NAME
!   FUNCTION Position_of_last_slash
!
! PURPOSE
!   Finds the position of the last slash or backslash.
!
! USAGE
!   Position_of_last_slash(stringC)
! 
! INPUT
!   o stringC:                string
!
! OUTPUT
!   o index_of_last           position of last slash '/' or '\'
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE SpecialCharacters     ,ONLY:BackSlashC
 USE system_specific_parser,ONLY:Windows,Linux

 IMPLICIT NONE
 
 CHARACTER(len=*)     ,INTENT(in)  :: stringC
 INTEGER                           :: index_of_last       ! RESULT

 INTEGER                           :: index_of_slash
 INTEGER                           :: index_of_backslash
 LOGICAL                           :: backL

 index_of_slash     = 0
 index_of_backslash = 0

 !-----------------------------------------------------------
 ! If backL=.TRUE., INDEX returns the maximum value of I
 ! such that string(I : I + LEN (substring) - 1) = substring
 ! (or zero if there is no such value).
 !-----------------------------------------------------------
 backL = .TRUE.

 !--------------------------------------------------------------------------------------
 ! Here, we check both slashes because the user could have specified either '/' or '\'.
 !--------------------------------------------------------------------------------------
 index_of_slash     = INDEX(TRIM(stringC),  Linux%DirectorySeparatorCharC,backL) ! Get position of '/'.
 index_of_backslash = INDEX(TRIM(stringC),Windows%DirectorySeparatorCharC,backL) ! Get position of '\'.

 !------------------------------------------------------------------------
 ! We assume that the slash that is present has the higher integer value.
 ! The other value should be zero.
 !------------------------------------------------------------------------
 index_of_last = MAX(index_of_slash,index_of_backslash)

 ! IF (index_of_last == 0) THEN
 !  WRITE(my_output_unit,*) "Warning Position_of_last_slash:"
 !  WRITE(my_output_unit,*) "It seems that no slash '/' or backslash '",BackSlashC,"' was present in the string."
 !  WRITE(my_output_unit,*) "stringC = ",TRIM(stringC)
 ! END IF

!------------------------------------------------------------------------------
 END FUNCTION Position_of_last_slash
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION FileName_Without_Directory(FilenameC) RESULT (Filename_without_DirectoryC)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/FileName_Without_Directory
!
! NAME
!   FUNCTION FileName_Without_Directory
!
! PURPOSE
!   Returns the actual filename without the leading directory path.
!
! USAGE
!   FileName_Without_Directory(FilenameC)
! 
! INPUT
!   o FilenameC:                    string
!
! OUTPUT
!   o Filename_without_DirectoryC:  string without 'directory/'
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 
 CHARACTER(len=*)            ,INTENT(in)  :: FilenameC
 CHARACTER(len=:),ALLOCATABLE             :: Filename_without_DirectoryC ! RESULT

 INTEGER                                  :: last

 !---------------------------------------------------------------
 ! 'input_filenameC' is a string that could include directories.
 !---------------------------------------------------------------

 !-------------------------------------------------------
 ! Find position of last character '/' or '\' in string.
 !-------------------------------------------------------
 last = Position_of_last_slash(FilenameC)

 !-------------------------------------------------------------
 ! Get actual filename, i.e. filename without any directories.
 !-------------------------------------------------------------
 Filename_without_DirectoryC = FilenameC( last+1 : LEN_TRIM(TRIM(FilenameC)) )

!------------------------------------------------------------------------------
 END FUNCTION FileName_Without_Directory
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION FileName_Without_FileExtension(FilenameC) RESULT (Filename_without_FileExtensionC)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/FileName_Without_FileExtension
!
! NAME
!   FUNCTION FileName_Without_FileExtension
!
! PURPOSE
!   Returns the filename without file extension such as '.dat' or '.in'.
!
! USAGE
!   FileName_Without_FileExtension(FilenameC)
! 
! INPUT
!   o FilenameC:                        string
!
! OUTPUT
!   o filename_without_FileExtensionC:  string without '.*'
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 
 CHARACTER(len=*)            ,INTENT(in)  :: FilenameC
 CHARACTER(len=:),ALLOCATABLE             :: Filename_without_FileExtensionC ! RESULT

 INTEGER                                  :: index_of_dot
 LOGICAL                                  :: backL

 index_of_dot     = 0

 !-----------------------------------------------------------
 ! If backL=.TRUE., INDEX returns the maximum value of I
 ! such that string(I : I + LEN (substring) - 1) = substring
 ! (or zero if there is no such value).
 !-----------------------------------------------------------
 backL = .TRUE.

 !----------------------------------
 ! Get index of last '.' in string.
 !----------------------------------
 index_of_dot = INDEX(TRIM(FilenameC),'.',backL) ! Get position of '.'.

 Filename_without_FileExtensionC = FilenameC(1:index_of_dot-1)

 ! IF (index_of_dot == 0) THEN
 !  WRITE(my_output_unit,*) "Warning FileName_Without_FileExtension:"
 !  WRITE(my_output_unit,*) "It seems that no '.' was present in the string."
 !  WRITE(my_output_unit,*) "FilenameC = ",TRIM(FilenameC)
 ! END IF

!------------------------------------------------------------------------------
 END FUNCTION FileName_Without_FileExtension
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Add_Slash_To_DirectoryName(DirectoryNameC)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/Add_Slash_To_DirectoryName
!
! NAME
!   SUBROUTINE Add_Slash_To_DirectoryName
!
! PURPOSE
!   Checks if the last letter of this directory name is a slash or backslash.
!   If this is not the case, a slash or backslash is added.
!
! USAGE
!   CALL Add_Slash_To_DirectoryName(DirectoryNameC)
! 
! INPUT
!   o DirectoryNameC
!
! OUTPUT
!   none
! 
! NOTES
!   Example:  Input:  output/test
!             Output: output/test/
!   NOTE: The variable DirectoryNameC which is input to this routine must not be an allocatable string [ CHARACTER(len=:),ALLOCATABLE ]!!!
!   
!##
!
!-----------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:CurrentSystem
 USE SpecialCharacters        ,ONLY:BackSlashC

 IMPLICIT NONE
 
 CHARACTER(len=*),INTENT(inout)  :: DirectoryNameC

 INTEGER                         :: directoryC_StringLength
 INTEGER                         :: directoryC_StringVariableLength
 INTEGER                         :: last


 !-----------------------------------------------------------------------------------------
 ! Get string length (i.e. number of characters of 'directoryC' including trailing blanks.
 !-----------------------------------------------------------------------------------------
 directoryC_StringVariableLength =      LEN(DirectoryNameC)
 
 !-------------------------------------------------------------------------------------------------
 ! Get string length (i.e. number of characters of 'directoryC' without including trailing blanks.
 !-------------------------------------------------------------------------------------------------
 directoryC_StringLength         = LEN_TRIM(DirectoryNameC)

 IF (directoryC_StringLength > 0) THEN
  !-----------------------------------------------------------------
  ! Here we check if at least one character is contained in string.
  !-----------------------------------------------------------------

  !---------------------------------
  ! Define index of last character.
  !---------------------------------
  last = directoryC_StringLength

  !-----------------------------------------------------------------------------
  ! If the last character is not a slash or backslash, we add it to the string.
  !-----------------------------------------------------------------------------
  IF ( ( DirectoryNameC(last:last) /= BackSlashC )  .AND. &  ! Check if last letter = '\'
       ( DirectoryNameC(last:last) /= '/'        ) ) THEN    ! Check if last letter = '/'

    IF ( directoryC_StringVariableLength == &
         directoryC_StringLength ) THEN
     WRITE(my_output_unit,'(A,A)') " Warning Add_Slash_To_DirectoryName: Cannot add slash to directory = ", &
                                     TRIM(DirectoryNameC)
    END IF

         DirectoryNameC = TRIM(DirectoryNameC)//CurrentSystem%DirectorySeparatorCharC ! This variable is not initialized yet.

  END IF
 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE Add_Slash_To_DirectoryName
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ReplaceSlashes( DirectoryFilenameC )
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/ReplaceSlashes
!
! NAME
!   SUBROUTINE ReplaceSlashes
!
! PURPOSE
!   Replace in string all backslashes and slashes with the specific one of the operating system.
!
! USAGE
!   CALL ReplaceSlashes(DirectoryFilenameC)
! 
! INPUT
!   o DirectoryFilenameC (also output)
!
! OUTPUT
!   o DirectoryFilenameC (also input)
! 
! NOTES
!   
!##
!
!-----------------------------------------------------------------
 USE system_specific_parser,ONLY:CurrentSystem
 USE SpecialCharacters     ,ONLY:BackSlashC
 USE CharacterManipulation ,ONLY:CharacterReplace

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(inout) :: DirectoryFilenameC

 CALL CharacterReplace( DirectoryFilenameC , '/'       , CurrentSystem%DirectorySeparatorCharC )
 CALL CharacterReplace( DirectoryFilenameC , BackSlashC, CurrentSystem%DirectorySeparatorCharC )

!------------------------------------------------------------------------------
 END SUBROUTINE ReplaceSlashes
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE FileExistWRITE(filenameC)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

    IMPLICIT NONE

    CHARACTER(len=*),INTENT(in) :: filenameC          ! Input

    INTEGER                     :: ios                ! Helper

    OPEN(UNIT=8,FILE=filenameC,IOSTAT=ios,STATUS="REPLACE",ACTION="WRITE")

    IF (ios /= 0) THEN
        WRITE(my_output_unit,'(A)')     " Error FileExistWRITE: Unable to open the file:"
        WRITE(my_output_unit,'(A,A,A)') " '",TRIM(filenameC),"'"
        WRITE(my_output_unit,'(A)')     " Please check if directory exists."
        STOP
    END IF

    !------------------------------------------------------------
    ! Delete file 'filenameC' because it is not needed any more.
    !------------------------------------------------------------
    CLOSE(UNIT=8,STATUS='DELETE')

 !-----------------------------------------------------------------
  END SUBROUTINE FileExistWRITE
 !-----------------------------------------------------------------
 !
 !
 !
 !-----------------------------------------------------------------
  SUBROUTINE ErrorMessage(error)
 !-----------------------------------------------------------------
  USE My_Input_and_Output_Units,ONLY:my_output_unit

  IMPLICIT NONE

  INTEGER,INTENT(in) :: error

  SELECT CASE(error)

  CASE(1)
   WRITE(my_output_unit,'(A)') " Error in keyword '$simulation-flow-control':"
   WRITE(my_output_unit,'(A)') " You chose 'strain-calculation = raw-strain-in',"
   WRITE(my_output_unit,'(A)') " therefore a file containing the raw data must exist."
   WRITE(my_output_unit,'(A)') " If the file exists, maybe you forgot to specify the"
   WRITE(my_output_unit,'(A)') " directory to read in from: 'raw-directory-in = ...'"

  CASE(2)
   WRITE(my_output_unit,'(A)') " Error in keyword '$import-data-on-material-grid' or '$doping-function', "
   WRITE(my_output_unit,'(A)') " or any other keyword that reads in an ASCII file."
   WRITE(my_output_unit,'(A)') " A file containing data to be read in must exist."

  CASE(4)
   WRITE(my_output_unit,'(A)') " Error macro: A macro file must exist if you want to read in a macro from a file."

  CASE DEFAULT
   WRITE(my_output_unit,'(A)') "[No specific error message defined.]"
  END SELECT

!------------------------------------------------------------------------------
  END SUBROUTINE ErrorMessage
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION SetGlobalDirectoryName(directoryC,Create_inL) RESULT (GlobalDirectoryC)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/SetGlobalDirectoryName
!
! NAME
!   FUNCTION SetGlobalDirectoryName
!
! PURPOSE
!   Gets and if not existing yet, sets global output directory name and creates it if not present.
!
! USAGE
!   SetGlobalDirectoryName(directoryC,Create_inL)
! 
! INPUT
!   o directoryC:                input can be without 'TRIM(directoryC)'
!   o Create_inL: (optional)     If .FALSE. this function does not check if the directory is present and does not create it.
!
! OUTPUT
!   o GlobalDirectoryC:          directory name
! 
! NOTES
!   CHECK: It might be necessary here to check the length of the strings
!          to make sure that no characters are lost. (This probably applies to the PGI compiler only.)
!
!   The input string 'directoryC' can be without 'TRIM()' which is recommended.
!   Example: 
!    MaterialDIRECTORY_OUT_C = SetGlobalDirectoryName(      DirectoryNameC )   ! <== without TRIM()
!    MaterialDIRECTORY_OUT_C = SetGlobalDirectoryName( TRIM(DirectoryNameC) )  !
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:OutputDirC, &
                                    DebugLevel
 
 IMPLICIT NONE
 
 CHARACTER(len=*)        ,INTENT(in)          :: directoryC       ! Input
 LOGICAL                 ,INTENT(in),OPTIONAL :: Create_inL
!CHARACTER(len=LEN( TRIM(OutputDirC) ) + &                                    ! supported by Intel, NAG, gfortran, g95 compilers, not supported by PGI
!              LEN( TRIM(directoryC) ))       :: GlobalDirectoryC ! Output    ! supported by Intel, NAG, gfortran, g95 compilers, not supported by PGI
 CHARACTER(len=:),ALLOCATABLE                 :: GlobalDirectoryC ! Output

 LOGICAL                                      :: CreateL

!PRINT *," directoryC       = ",TRIM(directoryC)

 GlobalDirectoryC = TRIM(OutputDirC)//TRIM(directoryC)//' ' ! Here we add an empty character to make sure that the string is large enough that a slash can be added if necessary.

!PRINT *," GlobalDirectoryC = ",TRIM(GlobalDirectoryC)

 !-------------------------------------------------------------------------
 ! Add slash to directory name in case no slash or backslash was provided.
 !-------------------------------------------------------------------------
 CALL Add_Slash_To_DirectoryName(GlobalDirectoryC) ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]

!PRINT *," GlobalDirectoryC = ",TRIM(GlobalDirectoryC)

 IF ( PRESENT( Create_inL ) ) THEN
     CreateL = Create_inL
   ! PRINT *," Create_inL = ",Create_inL
 ELSE
     CreateL = .TRUE.
   ! PRINT *," CreateL    = ",CreateL
 END IF

   ! PRINT *," CreateL    = ",CreateL

 IF ( CreateL ) THEN
   CALL DirectoryExist(GlobalDirectoryC)

   IF (DebugLevel > 99) THEN
    WRITE(my_output_unit,'(A,A)')  "                OutputDirC   = ",         OutputDirC
    WRITE(my_output_unit,'(A,I5)') " LEN(TRIM(      OutputDirC)) = ",LEN(TRIM(OutputDirC))
    WRITE(my_output_unit,'(A,A)')  "                directoryC   = ",         directoryC
    WRITE(my_output_unit,'(A,I5)') " LEN(TRIM(      directoryC)) = ",LEN(TRIM(directoryC))
    WRITE(my_output_unit,'(A,A)')  "          GlobalDirectoryC   = ",         GlobalDirectoryC
    WRITE(my_output_unit,'(A,I5)') " LEN(TRIM(GlobalDirectoryC)) = ",LEN(TRIM(GlobalDirectoryC))
   END IF
 END IF

!------------------------------------------------------------------------------
 END FUNCTION SetGlobalDirectoryName
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION GetGlobalDirectoryName(directoryC) RESULT (GlobalDirectoryC)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/GetGlobalDirectoryName
!
! NAME
!   FUNCTION GetGlobalDirectoryName
!
! PURPOSE
!   Get global output directory name.
!
! USAGE
!   GetGlobalDirectoryName(directoryC)
! 
! INPUT
!   o directoryC:        input can be without 'TRIM(directoryC)'
!
! OUTPUT
!   o GlobalDirectoryC:  
! 
!##
!
!------------------------------------------------------------------------------
 
 IMPLICIT NONE
 
 CHARACTER(len=*)                ,INTENT(in)  :: directoryC       ! Input
!CHARACTER(len=LEN( TRIM(OutputDirC) ) + &                                    ! supported by Intel, NAG, gfortran, g95 compilers, not supported by PGI
!              LEN( TRIM(directoryC) ))       :: GlobalDirectoryC ! Output    ! supported by Intel, NAG, gfortran, g95 compilers, not supported by PGI
 CHARACTER(len=:),ALLOCATABLE                 :: GlobalDirectoryC ! Output

 
 GlobalDirectoryC = SetGlobalDirectoryName(directoryC,Create_inL=.FALSE.)

!------------------------------------------------------------------------------
 END FUNCTION GetGlobalDirectoryName
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION GetTotalDirectoryName_C(directoryC,directory2C) RESULT (TotalDirectoryNameC)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/GetTotalDirectoryName_C
!
! NAME
!   FUNCTION GetTotalDirectoryName_C
!
! PURPOSE
!   Generates directory name 'directoryC/directory2C/'
!   taking into account the correct slash (or backslash).
!
! USAGE
!   GetTotalDirectoryName_C(directoryC,directory2C)
! 
! INPUT
!   o directoryC
!   o directory2C
!
! OUTPUT
!   o TotalDirectoryNameC     e.g. 'directoryC/ind001/'
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 
 CHARACTER(len=*)                ,INTENT(in)  :: directoryC
 CHARACTER(len=*)                ,INTENT(in)  :: directory2C
 CHARACTER(len=:),ALLOCATABLE                 :: TotalDirectoryNameC ! RESULT
 
 TotalDirectoryNameC = TRIM(directoryC)//TRIM(directory2C)//' ' ! Here we add an empty character to make sure that the string is large enough that a slash can be added if necessary.
 CALL Add_Slash_To_DirectoryName(TotalDirectoryNameC)           ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]
 CALL DirectoryExist(TotalDirectoryNameC)

!------------------------------------------------------------------------------
 END FUNCTION GetTotalDirectoryName_C
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION GetDirectoryName_int_to_char000(directoryC,labelC,index) RESULT (indexDirC)
!------------------------------------------------------------------------------
!
!++f* DirectoryFileExist/GetDirectoryName_int_to_char000
!
! NAME
!   FUNCTION GetDirectoryName_int_to_char000
!
! PURPOSE
!   Generates directory name 'directoryC/ind001/'.
!
! USAGE
!   GetDirectoryName_int_to_char000(directoryC,labelC,index)
! 
! INPUT
!   o directoryC
!   o labelC        e.g. 'ind'
!   o index         e.g. '0', '1', '2', ...
!
! OUTPUT
!   o indexDirC     e.g. 'directoryC/ind001/'
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_int_to_char999       ,ONLY:int_to_char000

 IMPLICIT NONE
 
 CHARACTER(len=*)                ,INTENT(in)  :: directoryC
 CHARACTER(len=*)                ,INTENT(in)  :: labelC
 INTEGER                         ,INTENT(in)  :: index
 CHARACTER(len=:),ALLOCATABLE                 :: indexDirC ! Output
!CHARACTER(LEN(TRIM(directoryC)) + &
!          LEN(TRIM(labelC)) + 3 + 1)         :: indexDirC ! Output      '+3' because of '00i'
                                                           !             '+1' because of '\' or '/'
 
 IF (index < 0) THEN
  WRITE(my_output_unit,*) "Error GetDirectoryName_int_to_char000: index < 0"
  WRITE(my_output_unit,*) "directoryC = ",directoryC
  WRITE(my_output_unit,*) "labelC     = ",labelC
  WRITE(my_output_unit,*) "index      = ",index
  STOP
 END IF
 
 indexDirC = TRIM(directoryC)//TRIM(labelC)//int_to_char000(index)//' ' ! Here we add an empty character to make sure that the string is large enough that a slash can be added if necessary.
 CALL Add_Slash_To_DirectoryName(indexDirC)                             ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]
 CALL DirectoryExist(indexDirC)

!------------------------------------------------------------------------------
 END FUNCTION GetDirectoryName_int_to_char000
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE CountLinesInFile(filenameC, NumberOfLines)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/CountLinesInFile
!
! NAME
!   SUBROUTINE CountLinesInFile
!
! PURPOSE
!   Returns the number of lines of file 'filenameC'.
!
! USAGE
!   CALL CountLinesInFile(filenameC, NumberOfLines)
! 
! INPUT
!   o filenameC
!
! OUTPUT
!   o NumberOfLines
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: filenameC
 INTEGER         ,INTENT(out) :: NumberOfLines  ! RESULT

 CHARACTER(len=1)             :: stringC        ! String length is kept short (len=1) to save time because it is actually not of interest here.
 INTEGER                      :: ios
 INTEGER                      :: line_number

 CALL FileExistREAD(filenameC)

 OPEN(10,STATUS='OLD', IOSTAT=ios, file=filenameC)

 ! IF (ios /= 0) THEN
 !  WRITE(my_output_unit,'(A,A)') " Error CountLinesInFile: Cannot OPEN file = ",TRIM(filenameC)
 !  WRITE(my_output_unit,'(A)')   " File doesn't exist."
 !  STOP
 ! END IF

   line_number = 0

   DO

     READ (10,'(A)',IOSTAT = ios) stringC

     IF (ios < 0) EXIT
     line_number = line_number + 1

   END DO

 CLOSE(10)

 NumberOfLines = line_number

!------------------------------------------------------------------------------
 END SUBROUTINE CountLinesInFile
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ReadFileAndStoreLines(filenameC, StringsV,max_string_length)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/ReadFileAndStoreLines
!
! NAME
!   SUBROUTINE ReadFileAndStoreLines
!
! PURPOSE
!   Read file and store the lines.
!
! USAGE
!   CALL ReadFileAndStoreLines(filenameC, StringsV,max_string_length)
! 
! INPUT
!   o filenameC
!
! OUTPUT
!   o StringsV:                        A string for each line.
!   o max_string_length:  (optional)   maximum length of characters contained in all lines, i.e. string length of longest line
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_Array_of_Strings     ,ONLY:String_in_Line

 IMPLICIT NONE

 CHARACTER(len=*)                 ,INTENT(in)           :: filenameC
 TYPE(String_in_Line),DIMENSION(:),INTENT(out)          :: StringsV
 INTEGER                          ,INTENT(out),OPTIONAL :: max_string_length

 INTEGER                                                :: ios
 INTEGER                                                :: line_number
 INTEGER,PARAMETER                                      :: Linelength_very_long = 98765   ! Data_len_very_long
 CHARACTER(len=Linelength_very_long)                    :: lineC       ! We use a very large number in order to allow for extremely long lines.

 IF ( PRESENT(max_string_length) ) THEN
    max_string_length = 0
 END IF

 CALL FileExistREAD(filenameC)

 OPEN(10,STATUS='OLD', IOSTAT=ios, file=filenameC)

   line_number = 1

   DO

     IF ( line_number > SIZE(StringsV) ) EXIT

   ! READ (10,'(A)',IOSTAT = ios) StringsV(line_number)%StringC  ! This does not work. It is not a valid Fortran statement. READ does not support allocatable character length.
     READ (10,'(A)',IOSTAT = ios) lineC
     IF (ios < 0) EXIT

     StringsV(line_number)%StringC = TRIM(lineC)

     IF ( PRESENT(max_string_length) ) THEN
        max_string_length = MAX( LEN_TRIM( StringsV(line_number)%StringC) , max_string_length )
     END IF

     line_number = line_number + 1

   END DO

 CLOSE(10)

!------------------------------------------------------------------------------
 END SUBROUTINE ReadFileAndStoreLines
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE CopyFile(filenameC_in,directory_copyC_in,filename_copyC_in,ExactCopyL_in,PrintInfoL_in)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/CopyFile
!
! NAME
!   SUBROUTINE CopyFile
!
! PURPOSE
!   Copies a file from one location to another location.
!
! USAGE
!   CALL CopyFile(filenameC_in,directory_copyC_in,filename_copyC_in,ExactCopyL_in,PrintInfoL_in)
! 
! INPUT
!   o filenameC_in:       original file
!   o directory_copyC_in: directory where the new file should be written to
!   o filename_copyC_in:  new filename without directory (copy of original file)
!   o ExactCopyL_in:      If .TRUE., an exact copy is made. If .FALSE., trailing blanks are deleted.
!   o PrintInfoL_in:      If .TRUE., a message is printed to the screen.
!
! OUTPUT
!   none
! 
! NOTES
!   All trailing blanks are deleted.
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_Array_of_Strings     ,ONLY:String_in_Line

 IMPLICIT NONE

 CHARACTER(len=*)         ,INTENT(in)          :: filenameC_in
 CHARACTER(len=*)         ,INTENT(in)          :: directory_copyC_in
 CHARACTER(len=*)         ,INTENT(in)          :: filename_copyC_in
 LOGICAL                  ,INTENT(in),OPTIONAL :: ExactCopyL_in
 LOGICAL                  ,INTENT(in),OPTIONAL :: PrintInfoL_in

 CHARACTER(len=:),ALLOCATABLE                  :: filenameC
 CHARACTER(len=:),ALLOCATABLE                  :: directory_copyC
 CHARACTER(len=:),ALLOCATABLE                  :: filename_copyC
 LOGICAL                                       :: ExactCopyL
 LOGICAL                                       :: PrintInfoL
 INTEGER                                       :: slash
 INTEGER                                       :: line
 INTEGER                                       :: NumberOfLines
 TYPE(String_in_Line),DIMENSION(:),ALLOCATABLE :: StringsV

 IF ( PRESENT(PrintInfoL_in) ) THEN
    PrintInfoL = PrintInfoL_in
 ELSE
    PrintInfoL = .FALSE.
 END IF

 IF ( PRESENT(ExactCopyL_in) ) THEN
    ExactCopyL = ExactCopyL_in
 ELSE
    ExactCopyL = .FALSE.
 END IF

 filenameC       = filenameC_in
 directory_copyC = directory_copyC_in
 filename_copyC  = filename_copyC_in

 !----------------------------------------------------------------------------------------------
 ! Replace in string all backslashes and slashes with the specific one of the operating system.
 ! We want to compare filenames irrespective of mixed use of backslash/slash.
 !----------------------------------------------------------------------------------------------
 CALL ReplaceSlashes( filenameC )
 CALL ReplaceSlashes( directory_copyC )
 CALL ReplaceSlashes( filename_copyC )

 IF ( TRIM(filenameC) == TRIM(directory_copyC)//TRIM(filename_copyC) ) THEN
  !-----------------------------------------------------------------------------------------------------
  ! If the strings coincide, nothing has to be done because the file is already there. So let's return.
  !-----------------------------------------------------------------------------------------------------
  RETURN
 ELSE
  IF (PrintInfoL) THEN
   WRITE(my_output_unit,'(A,A,A,A,A)') " Copying file '",TRIM(filenameC_in)     ,"' to '", &
                   TRIM(directory_copyC_in)//TRIM(filename_copyC_in),"'"
  END IF
 
  !-----------------------------------------------
  ! Check if directory exists. If not, create it.
  !-----------------------------------------------
  CALL DirectoryExist(directory_copyC)

  !-------------------------------------------------
  ! Check if filename does not contain a directory.
  !-------------------------------------------------
  slash = Position_of_last_slash(filename_copyC)
  IF (slash /= 0) THEN
   WRITE(my_output_unit,'(A)')   " Error CopyFile: filename_copyC contains a slash ('/') or backslash ('\'). This is unexpected."
   WRITE(my_output_unit,'(A,A)') " filenameC       = ",TRIM(filenameC)
   WRITE(my_output_unit,'(A,A)') " directory_copyC = ",TRIM(directory_copyC)
   WRITE(my_output_unit,'(A,A)') " filename_copyC  = ",TRIM(filename_copyC)
   STOP
  END IF

  !----------------------------------------------
  ! Get number of lines that this file contains.
  !----------------------------------------------
  CALL CountLinesInFile(filenameC, NumberOfLines)

  !---------------------------------------------------------------------------------
  ! Allocate storage so that each line of the input file can be stored as a string.
  !---------------------------------------------------------------------------------
  ALLOCATE(StringsV( NumberOfLines ))

  !--------------------------------
  ! Read file and store the lines.
  !--------------------------------
  CALL ReadFileAndStoreLines(filenameC, StringsV)

  OPEN (UNIT=10,FILE = TRIM(directory_copyC)//filename_copyC)

   DO line=1,NumberOfLines

    IF (ExactCopyL) THEN
      WRITE(UNIT=10,FMT='(A)')      StringsV(line)%StringC
    ELSE
      WRITE(UNIT=10,FMT='(A)') TRIM(StringsV(line)%StringC) ! All trailing blanks are deleted. So it is not an exact copy.
    END IF
   END DO

  CLOSE(UNIT=10)

  DEALLOCATE(StringsV)

 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE CopyFile
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE DirectoryFileExist
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_SyntaxFolder
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 FUNCTION GetFilenameIncludingSyntaxFolder(directoryC,filenameC) RESULT(FolderFilenameC)
!------------------------------------------------------------------------------
!
!++f* mod_SyntaxFolder/GetFilenameIncludingSyntaxFolder
!
! NAME
!   FUNCTION GetFilenameIncludingSyntaxFolder
!
! PURPOSE
!   This function returns (default) (==> Here, we assume that the Syntax folder is one level above the nextnano3.exe executable.)
!     o '..\Syntax\database_nn3_keywords.val'    for 'database_nn3_keywords'.   [directoryC must be = '', i.e. empty.]
!     o '..\Syntax\keywords.val'                 for 'keywords'.                [directoryC must be = '', i.e. empty.]
!
!   or if ExecuteHTCondorL=.TRUE.   (==> Here, we assume that the Syntax folder is in the same directory as the nextnano3.exe executable.)
!     o  '.\Syntax\database_nn3_keywords.val'    for 'database_nn3_keywords'.   [directoryC must be = '', i.e. empty.]
!     o  '.\Syntax\keywords.val'                 for 'keywords'.                [directoryC must be = '', i.e. empty.]
!
!   Additionally, the following is possible:
!     o '..\Syntax\directoryC\filenameC'         for 'directoryC,filenameC'.    [directoryC must be /= '', i.e. not empty, e.g. "NEGF files".]
!        e.g. '..\Syntax\NEGF files\BesselI.dat' for 'BesselI.dat'
!
!   Finally, also an environment variable can be specified.
!   Instead of '..\' an environment variable NEXTNANO_PATHC could be used.
!
! USAGE
!   GetFilenameIncludingSyntaxFolder(directoryC,filenameC)
!
! INPUT
!   o directoryC
!   o filenameC
!
! OUTPUT
!   o FolderFilenameC
!
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE system_specific_parser,ONLY:NEXTNANO_PathC, &
                                 ExecuteHTCondorL
 USE mod_FolderNames       ,ONLY:RelativeFolderPathC, &
                                 RelativeFolderPathSameDirectoryC, &
                                 FolderNameForDefinitionFilesC                                 
 USE DirectoryFileExist    ,ONLY:Add_Slash_To_DirectoryName

 IMPLICIT NONE

 CHARACTER(len=*)                                  ,INTENT(in)  :: directoryC
 CHARACTER(len=*)                                  ,INTENT(in)  :: filenameC
 CHARACTER(len=:),ALLOCATABLE                                   :: FolderFilenameC ! RESULT
!CHARACTER(LEN(TRIM(RelativeFolderPathC))                   + &
!          LEN(TRIM(FolderNameForDefinitionFilesC))         + &
!        2*LEN(TRIM(CurrentSystem%DirectorySeparatorCharC)) + &
!!       3*LEN(TRIM(CurrentSystem%DirectorySeparatorCharC)) + &
!          LEN(TRIM(filenameC))                             + &
!          LEN(TRIM(ValidatorC)))                               :: FolderFilenameC ! RESULT

!CHARACTER(len=:),ALLOCATABLE                                   :: RelativeFolderPathC_temp           ! cannot be CHARACTER(len=:),ALLOCATABLE
!CHARACTER(len=:),ALLOCATABLE                                   :: FolderNameForDefinitionFilesC_temp ! cannot be CHARACTER(len=:),ALLOCATABLE
!CHARACTER(len=:),ALLOCATABLE                                   :: directoryC_temp                    ! cannot be CHARACTER(len=:),ALLOCATABLE
 CHARACTER(len=987)                                             :: RelativeFolderPathC_temp           ! cannot be CHARACTER(len=:),ALLOCATABLE
 CHARACTER(len=987)                                             :: FolderNameForDefinitionFilesC_temp ! cannot be CHARACTER(len=:),ALLOCATABLE
 CHARACTER(len=987)                                             :: directoryC_temp                    ! cannot be CHARACTER(len=:),ALLOCATABLE

 !-----------------------------------------------------
 ! Use local variables because slashes might be added.
 !-----------------------------------------------------
 IF ( ExecuteHTCondorL ) THEN
  !-------------------------------------------------------------------------
  ! Assume Syntax/ folder is on the same level as nextnano3.exe executable.
  !-------------------------------------------------------------------------
  RelativeFolderPathC_temp           = RelativeFolderPathSameDirectoryC ! '.' ==> './'
 ELSE IF ( TRIM(NEXTNANO_PathC) /= '' ) THEN
  RelativeFolderPathC_temp = TRIM(NEXTNANO_PathC)
  CALL Add_Slash_To_DirectoryName(RelativeFolderPathC_temp) ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]
  RelativeFolderPathC_temp = TRIM(RelativeFolderPathC_temp)//'nextnano3' ! Assume installation folder like "C:\Program Files\nextnano\nextnano3".
  ! CHECK: A date could be included such as "C:\Program Files\nextnano\2015-08-05\nextnano\nextnano3".
 ELSE
  !----------------------------------------------
  ! This should be the most realistic situation.
  !----------------------------------------------
  RelativeFolderPathC_temp           = RelativeFolderPathC               ! '..' ==> '../'
 END IF
  FolderNameForDefinitionFilesC_temp = FolderNameForDefinitionFilesC     ! 'Syntax'
  directoryC_temp                    = directoryC

 CALL Add_Slash_To_DirectoryName(RelativeFolderPathC_temp)           ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]
 CALL Add_Slash_To_DirectoryName(FolderNameForDefinitionFilesC_temp) ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]
 CALL Add_Slash_To_DirectoryName(directoryC_temp)                    ! String must not be an allocatable string! [ CHARACTER(len=:),ALLOCATABLE ]

 FolderFilenameC = TRIM(RelativeFolderPathC_temp)           // &
                   TRIM(FolderNameForDefinitionFilesC_temp) // &
                   TRIM(directoryC_temp)                    // &
                   TRIM(filenameC)

!------------------------------------------------------------------------------
 END FUNCTION GetFilenameIncludingSyntaxFolder
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_SyntaxFolder
!------------------------------------------------------------------------------
