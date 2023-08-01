!------------------------------------------------------------------------------
! o MODULE parser_parameters
! o MODULE mod_queue
! o MODULE input_data_types
! o MODULE input_spec_node_def
! o MODULE input_specifier_queue_def
! o MODULE input_key_node_def
! o MODULE input_key_queue_def
! o MODULE mod_init_input_key_queue
! o MODULE mod_add_input_key
! o MODULE position_at_key_interface
! o MODULE add_inp_spec_interface
! o MODULE scan_inp_key_interface
! o MODULE input_built_up_interface
! o MODULE mod_value_to_queue
! o MODULE common_queues
! o MODULE common_nodes
! o MODULE mod_check_presence
! o MODULE mod_read_and_analyze_input
! o MODULE generic_inputfile
!
! Modules not present in FortranInputParser/builder_database.f90:
! o MODULE mod_input_driver
! o MODULE mod_TEST_FortranInputParser
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 MODULE parser_parameters
!------------------------------------------------------------------------------
 USE mod_FileExtensions_parser,ONLY:ValidatorC

 IMPLICIT NONE

!------------------------------------------------------------------------------
! Note: For the specification of some large 1D devices it might be an advantage
!       to have lines containing more than 267 characters.
!       (One then does not have to specify a line break.)
!       It seems that having 'Data_len = 987' is not time critical.
!       This is in contrast to the material database file where 'Data_len = 987'
!       is indeed time critical (order of seconds).
!------------------------------------------------------------------------------
! INTEGER                      ,PARAMETER :: Data_len               =  987                    ! Maximum line length in input file (267 - SUN, arbitrary - DEC)
  INTEGER                      ,PARAMETER :: Data_len               =  267                    ! Maximum line length in input file (267 - SUN, arbitrary - DEC)
  INTEGER                      ,PARAMETER :: Data_len_long          =  987                    ! Maximum line length in input file
  INTEGER                      ,PARAMETER :: Data_len_very_long     = 98765                   ! Maximum line length in input file (When using variables in the input file, very long lines can occur for z-grid-lines for instance.
  INTEGER                      ,PARAMETER :: char_length_specifier_content = 267              ! A specifier content can contain up to 267 characters.
  CHARACTER(len=*)             ,PARAMETER :: SpecialMacroCharacterC = '%'                     ! special character used in macro definition
  CHARACTER(len=*)             ,PARAMETER :: key_char               = '$'                     ! Character which specifies beginning of keyword (can be chosen)
  CHARACTER(len=*)             ,PARAMETER :: end_key_char           = ' '                     ! Character which specifies end of keyword (must be a blank)
  CHARACTER(len=*)             ,PARAMETER :: spec_char              = ' '                     ! Character which separates a specifier (must be a blank)
  CHARACTER(len=*)             ,PARAMETER :: end_spec_char          = '='                     ! Character which specifies end of specifier (can be chosen)
  CHARACTER(len=*),DIMENSION(4),PARAMETER :: IF_STATEMENT_CV        = [ '!IF  ', '#IF  ', &
                                                                      ! '!if  ', '#if  '      ! <== also works as IF is not case-sensitive.
                                                                        '!WHEN', '#WHEN' ]
                                                                      ! '!when', '#when'      ! <== also works as IF is not case-sensitive.
  CHARACTER(len=*),DIMENSION(4),PARAMETER :: comment_signsCV        = [ '! '   , '# '  , &
                                                                        '//'   , '/*' ]       ! Comment signs. Comment sign and text towards right is ignored.
! CHARACTER(len=*),DIMENSION(4),PARAMETER :: XML_tagCV              = [ '< '   , '</'  , &
!                                                                       '> '   , '/>' ]       ! XML tags
  LOGICAL                      ,PARAMETER :: DATA_Filename_FixedL   = .TRUE.                  ! use fixed filename for batch file rather than filename which is <inputfilename>.bat
  CHARACTER(len=:),ALLOCATABLE            :: DATA_FileC
  CHARACTER(len=*)             ,PARAMETER :: DATA_String_BeginC     = '!DATA'                 ! special string                       (case sensitive)
  CHARACTER(len=*)             ,PARAMETER :: DATA_String_EndC       = '!ENDDATA'              ! special string                       (case sensitive)
  CHARACTER(len=*)             ,PARAMETER :: TEXT_String_BeginC     = '!TEXT'                 ! special string for multiline comment (case sensitive)
  CHARACTER(len=*)             ,PARAMETER :: TEXT_String_EndC       = '!ENDTEXT'              ! special string for multiline comment (case sensitive)

  CHARACTER(len=*)             ,PARAMETER :: keyword_filetypeC      = 'input file'            ! This is how we call these type of files that we want to parse.
  CHARACTER(len=*)             ,PARAMETER :: keyword_filenameC      = 'keywords'//ValidatorC  ! name of file, containing definitions of keywords and specifiers without file extension

  !-------------------------------------------------------------------------------------
  ! These flags are needed for the input parser if input file is read in several times.
  !-------------------------------------------------------------------------------------
  LOGICAL :: SecondEntry_InputBuiltUpL = .FALSE.

!------------------------------------------------------------------------------
 END MODULE parser_parameters
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_queue
!------------------------------------------------------------------------------
!
!++m* builder_inputfile.f90/mod_queue
!
! NAME 
!   MODULE mod_queue
!
! PURPOSE
!   MODULE to build up the queue of the input file.
!
! CONTAINS
!   o SUBROUTINE queue_built_up
!
! FILENAME
!   FortranInputParser/builder_inputfile.f90
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 PRIVATE

 PUBLIC queue_built_up

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE queue_built_up(input_filename_C, queue_1)
!------------------------------------------------------------------------------
!
!++s* mod_queue/queue_built_up
!
! NAME
!   SUBROUTINE queue_built_up
!
! PURPOSE
!   Builds up the queue of the input file.
!
! USAGE
!   CALL queue_built_up(input_filename_C, queue_1)
!
! INPUT
!   o input_filename_C:
!
! OUTPUT
!   o queue_1:
! 
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:DebugLevel, &
                                    InputFileName_NoDirectoryNoExtensionC, &
                                    InputFileName_NoDirectoryC, &
                                    WriteCompactFileL, &
                                    OperatingSystemC
 USE MacroForInputFile        ,ONLY:ApplyMacro
 USE parser_parameters        ,ONLY:comment_signsCV        , &
                                    Data_len               , &
                                    Data_len_long          , &
                                    SpecialMacroCharacterC , &
                                    IF_STATEMENT_CV        , &
                                    DATA_String_BeginC     , &
                                    DATA_String_EndC       , &
                                    DATA_Filename_FixedL   , &
                                    DATA_FileC             , &
                                    TEXT_String_BeginC     , &
                                    TEXT_String_EndC
 USE mod_FileExtensions_parser,ONLY:Batch_WindowsC, &
                                    Batch_LinuxC
 USE mod_SpecialStrings       ,ONLY:Multiline_Comment, &
                                    Extract_DATA
 USE mod_chrpak               ,ONLY:StringReplace
 USE queue_type_def           ,ONLY:queue_type
 USE mod_init_queue           ,ONLY:init_queue
 USE mod_push                 ,ONLY:push
 USE CharacterManipulation    ,ONLY:ReplaceTAB              , &
                                    Replace_NonBreakingSpace, &
                                    ReplaceXMLTag
 USE mod_Array_of_Strings     ,ONLY:String_in_Line
 USE DirectoryFileExist       ,ONLY:CountLinesInFile          , &
                                    FileExistREAD             , &
                                    ReplaceSlashes            , &
                                    GetGlobalDirectoryName    , &
                                    FileName_Without_Directory, &
                                    FileName_Without_FileExtension, &
                                    ReadFileAndStoreLines

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)                            :: input_filename_C
 TYPE(queue_type),INTENT(out)                           :: queue_1

 INTEGER,PARAMETER                                      :: Linelength_very_long = 98765   ! Data_len_very_long
 CHARACTER(Linelength_very_long)                        :: bufferC_read
 CHARACTER(Data_len_long)                               :: bufferC_long
 CHARACTER(len=:),ALLOCATABLE                           :: bufferC
 CHARACTER(len=:),ALLOCATABLE                           :: buffer_before_ReplacementC
 CHARACTER(len=:),ALLOCATABLE                           :: Folder_InputFilename_outC
 TYPE(String_in_Line)         ,DIMENSION(:),ALLOCATABLE :: StringsV
 TYPE(String_in_Line)         ,DIMENSION(:),ALLOCATABLE :: Strings_for_DATA_V
 INTEGER                                                :: Line_of_DATA           ! line where special string '!DATA' was found
 INTEGER                                                :: Line_of_DATA_count     ! line where special string '!DATA' was found
 LOGICAL                                                :: Write_DATA_FileL       ! .TRUE.  if special string '!DATA' was found
 CHARACTER(len=:),ALLOCATABLE                           :: Batch_FileExtensionC
 LOGICAL                                                :: DATA_Comment_OnL       ! .TRUE.  if special string '!DATA' was found
 LOGICAL                                                :: Multiline_Comment_OnL  ! .TRUE.  if special string '!TEXT' was found
 INTEGER                                                :: max_string_length
 INTEGER                                                :: ios
 INTEGER                                                :: NumberOfLines
 INTEGER                                                :: line_number
 LOGICAL                                                :: AnyMacroL

 !********************************************************************
 ! For 'Find & Replace String' macro. (obsolete feature)
 ! The 'Find & Replace String' macro is deprecated.
 ! Instead, use 'ApplyMacro'.
 !********************************************************************
 CHARACTER(Data_len),DIMENSION(:),ALLOCATABLE :: StringToFindCV
 CHARACTER(Data_len),DIMENSION(:),ALLOCATABLE :: StringToReplaceCV
 LOGICAL            ,DIMENSION(:),ALLOCATABLE :: StringFindAndReplaceLV

 LOGICAL                         :: UseMacroFindAndReplaceL
 INTEGER                         :: NumberOfReplacementsMade
 INTEGER                         :: i

 CHARACTER(Data_len_long)        :: filename  ! Use 'filename' and not 'filenameC' because of namelist syntax.
 INTEGER                         :: NumberOfFindReplace_max
 CHARACTER(Data_len)             :: UnrealisticStringC

 TYPE ::  Find_and_Replace_type
  CHARACTER(Data_len) :: StringToFindC
  CHARACTER(Data_len) :: StringToReplaceC
 END TYPE Find_and_Replace_type

 TYPE(Find_and_Replace_type)  :: Find_and_Replace   , &
                                 Find_and_Replace_1 , &
                                 Find_and_Replace_2 , &
                                 Find_and_Replace_3 , &
                                 Find_and_Replace_4 , &
                                 Find_and_Replace_5 , &
                                 Find_and_Replace_6 , &
                                 Find_and_Replace_7 , &
                                 Find_and_Replace_8 , &
                                 Find_and_Replace_9
  TYPE(Find_and_Replace_type) :: Find_and_Replace_10, &
                                 Find_and_Replace_11, &
                                 Find_and_Replace_12, &
                                 Find_and_Replace_13, &
                                 Find_and_Replace_14, &
                                 Find_and_Replace_15, &
                                 Find_and_Replace_16, &
                                 Find_and_Replace_17, &
                                 Find_and_Replace_18, &
                                 Find_and_Replace_19
  TYPE(Find_and_Replace_type) :: Find_and_Replace_20, &
                                 Find_and_Replace_21, &
                                 Find_and_Replace_22, &
                                 Find_and_Replace_23, &
                                 Find_and_Replace_24, &
                                 Find_and_Replace_25, &
                                 Find_and_Replace_26, &
                                 Find_and_Replace_27, &
                                 Find_and_Replace_28, &
                                 Find_and_Replace_29
  TYPE(Find_and_Replace_type) :: Find_and_Replace_30, &
                                 Find_and_Replace_31, &
                                 Find_and_Replace_32, &
                                 Find_and_Replace_33, &
                                 Find_and_Replace_34, &
                                 Find_and_Replace_35, &
                                 Find_and_Replace_36, &
                                 Find_and_Replace_37, &
                                 Find_and_Replace_38, &
                                 Find_and_Replace_39, &
                                 Find_and_Replace_40

   NAMELIST /macro/       filename, &
                          Find_and_Replace_1 , Find_and_Replace_2 , Find_and_Replace_3 , Find_and_Replace_4 , &
                          Find_and_Replace_5 , &
                          Find_and_Replace_6 , Find_and_Replace_7 , Find_and_Replace_8 , Find_and_Replace_9 , &
                          Find_and_Replace_10, &
                          Find_and_Replace_11, Find_and_Replace_12, Find_and_Replace_13, Find_and_Replace_14, &
                          Find_and_Replace_15, &
                          Find_and_Replace_16, Find_and_Replace_17, Find_and_Replace_18, Find_and_Replace_19, &
                          Find_and_Replace_20, &
                          Find_and_Replace_21, Find_and_Replace_22, Find_and_Replace_23, Find_and_Replace_24, &
                          Find_and_Replace_25, &
                          Find_and_Replace_26, Find_and_Replace_27, Find_and_Replace_28, Find_and_Replace_29, &
                          Find_and_Replace_30, &
                          Find_and_Replace_31, Find_and_Replace_32, Find_and_Replace_33, Find_and_Replace_34, &
                          Find_and_Replace_35, &
                          Find_and_Replace_36, Find_and_Replace_37, Find_and_Replace_38, Find_and_Replace_39, &
                          Find_and_Replace_40

 Multiline_Comment_OnL   = .FALSE.
 DATA_Comment_OnL        = .FALSE.
 DATA_FileC              = ''
 Write_DATA_FileL        = .FALSE.
 Line_of_DATA_count      = 0

 UseMacroFindAndReplaceL = .FALSE.
 NumberOfFindReplace_max = 40       ! Find_and_Replace_1,Find_and_Replace_2,...,Find_and_Replace_40
 filename                = ''

 !--------------------------------------
 ! Default initialization is necessary.
 !--------------------------------------
   UnrealisticStringC = 'unrealistic_stringC'
   Find_and_Replace%StringToFindC    = UnrealisticStringC
   Find_and_Replace%StringToReplaceC = UnrealisticStringC
   Find_and_Replace_1  = Find_and_Replace
   Find_and_Replace_2  = Find_and_Replace
   Find_and_Replace_3  = Find_and_Replace
   Find_and_Replace_4  = Find_and_Replace
   Find_and_Replace_5  = Find_and_Replace
   Find_and_Replace_6  = Find_and_Replace
   Find_and_Replace_7  = Find_and_Replace
   Find_and_Replace_8  = Find_and_Replace
   Find_and_Replace_9  = Find_and_Replace
   Find_and_Replace_10 = Find_and_Replace
   Find_and_Replace_11 = Find_and_Replace
   Find_and_Replace_12 = Find_and_Replace
   Find_and_Replace_13 = Find_and_Replace
   Find_and_Replace_14 = Find_and_Replace
   Find_and_Replace_15 = Find_and_Replace
   Find_and_Replace_16 = Find_and_Replace
   Find_and_Replace_17 = Find_and_Replace
   Find_and_Replace_18 = Find_and_Replace
   Find_and_Replace_19 = Find_and_Replace
   Find_and_Replace_20 = Find_and_Replace
   Find_and_Replace_21 = Find_and_Replace
   Find_and_Replace_22 = Find_and_Replace
   Find_and_Replace_23 = Find_and_Replace
   Find_and_Replace_24 = Find_and_Replace
   Find_and_Replace_25 = Find_and_Replace
   Find_and_Replace_26 = Find_and_Replace
   Find_and_Replace_27 = Find_and_Replace
   Find_and_Replace_28 = Find_and_Replace
   Find_and_Replace_29 = Find_and_Replace
   Find_and_Replace_30 = Find_and_Replace
   Find_and_Replace_31 = Find_and_Replace
   Find_and_Replace_32 = Find_and_Replace
   Find_and_Replace_33 = Find_and_Replace
   Find_and_Replace_34 = Find_and_Replace
   Find_and_Replace_35 = Find_and_Replace
   Find_and_Replace_36 = Find_and_Replace
   Find_and_Replace_37 = Find_and_Replace
   Find_and_Replace_38 = Find_and_Replace
   Find_and_Replace_39 = Find_and_Replace
   Find_and_Replace_40 = Find_and_Replace
   !********************************************************************

 !----------------------------------------------------------------------------
 ! By default, no macro is used, thus this logical variable is set to .FALSE.
 !----------------------------------------------------------------------------
 AnyMacroL               = .FALSE.

   !----------------------------------------------------------
   ! Disassociate pointer 'queue_1' to top and rear of queue.
   !----------------------------------------------------------
   CALL init_queue (queue_1)                                           ! create a new queue
   !--------------------------------------------------------------------
   ! 'queue_1%top' and 'queue_1%rear' are now nullified.
   !--------------------------------------------------------------------
   
   !----------------------------------------------
   ! Get number of lines that this file contains.
   !----------------------------------------------
   CALL CountLinesInFile(input_filename_C, NumberOfLines)
   IF (DebugLeveL > 10) WRITE(my_output_unit,*) "NumberOfLines = ",NumberOfLines

   !---------------------------------------------------------------------------------
   ! Allocate storage so that each line of the input file can be stored as a string.
   !---------------------------------------------------------------------------------
   ALLOCATE(StringsV( NumberOfLines ))

   !--------------------------------
   ! Read file and store the lines.
   !--------------------------------
   CALL ReadFileAndStoreLines(input_filename_C, StringsV,max_string_length)

   !---------------
   ! Replace tabs.
   !---------------
   DO line_number=1,NumberOfLines
    !--------------------------------------------------------------------
    ! Check if line contains a TAB.
    ! If yes, replace it with one blank.
    ! There are two reasons for this. If tabs are not replaced,
    ! (1) the ApplyMacro feature is not working properly,
    !     e.g. if a variable name is followed by a tab or
    !     if a tab is in front of a variable definition.
    !     In this case a strange error is produced.
    ! (2) the nextnano3 input parser produces
    !     a strange error and stops.
    !--------------------------------------------------------------------
    CALL ReplaceTAB(               StringsV(line_number)%StringC )
    CALL Replace_NonBreakingSpace( StringsV(line_number)%StringC )

    !-----------------------------------------------------------------------------------------
    ! XML tags <...> can be present. They are ignored and replaced with blanks.
    ! Note: As they are replaced with blanks, they are not visible in the file *.in.no_macro.
    !-----------------------------------------------------------------------------------------
    CALL ReplaceXMLTag(            StringsV(line_number)%StringC )

   END DO 

   !---------------------------------------------------------------------
   ! Apply macro
   !
   !     %width = 10d0 ! quantum well width
   !
   ! to input file, i.e. replace all occurences of '%width' with '10.0'.
   !---------------------------------------------------------------------
   CALL ApplyMacro(SpecialMacroCharacterC,comment_signsCV,IF_STATEMENT_CV,DebugLevel,NumberOfLines,max_string_length, &
                   StringsV,AnyMacroL)

   !********************************************************************
   !-------------------------------------------------------
   ! Necessary for previous 'Find & Replace String' macro.
   !-------------------------------------------------------

   CALL FileExistREAD(input_filename_C)

   !-----------------------------------------------------------------
   ! In principle, the input file does not have to be read in again.
   ! All relevant information is stored in 'StringsV'.
   !-----------------------------------------------------------------
   OPEN(10,STATUS='OLD', IOSTAT=ios, file=input_filename_C)
  ! IF (ios /= 0) THEN
  !  WRITE(my_output_unit,*)"ERROR in OPEN file = ",TRIM(input_filename_C)
  !  WRITE(my_output_unit,*)"File doesn't exist or unit 10 is already connected!"
  !  STOP
  ! END IF

    !********************************************************************
    ! For 'Find & Replace String' macro.
    ! Try to find namelist 'macro' in input file.
    !********************************************************************
    READ(10,macro, IOSTAT=ios) ! macro = namelist

!********************************************************************
 IF (ios /= 0) THEN
!********************************************************************
     UseMacroFindAndReplaceL = .FALSE.
    IF (DebugLeveL > 10) THEN
     WRITE(my_output_unit,'(A)') " Macro status: No 'Find & Replace' macro definition (obsolete feature) found in input file."
    END IF
             !------------------------------------
   CLOSE(10) ! Input file has to be closed again.
             !------------------------------------
   !********************************************************************

   !----------------------------------------------------------------------------------
   ! Input file has to be opened again. (necessary for 'Find & Replace String' macro)
   !----------------------------------------------------------------------------------
   OPEN(10,STATUS='OLD', IOSTAT=ios, file=input_filename_C)
    IF (ios /= 0) THEN
     WRITE(my_output_unit,*)"ERROR in OPEN file = ",TRIM(input_filename_C)
     WRITE(my_output_unit,*)"File doesn't exist or unit 10 is already connected!"
     STOP
    END IF
!********************************************************************
 ELSE
!********************************************************************
     UseMacroFindAndReplaceL = .TRUE.
     AnyMacroL               = .TRUE.
     WRITE(my_output_unit,'(A)') " Macro status: Macro definition found in input file."

     IF (DebugLeveL > 4) WRITE(my_output_unit,macro) ! macro was found, so let's write it.

     IF (TRIM(filename) /= '') THEN
      WRITE(my_output_unit,'(A,A)') " Reading in macro from file: ",TRIM(filename)
      CALL FileExistREAD(filename,4) ! '4' = error message for macro file
      OPEN (3,status='unknown',file=filename)
       READ(3,macro)
      CLOSE(3)
      IF (DebugLeveL > 4) WRITE(my_output_unit,macro)
     END IF
     WRITE(my_output_unit,'(A)') " "

     ALLOCATE(StringToFindCV        (NumberOfFindReplace_max))
     ALLOCATE(StringToReplaceCV     (NumberOfFindReplace_max))
     ALLOCATE(StringFindAndReplaceLV(NumberOfFindReplace_max))

     StringFindAndReplaceLV = .FALSE.

     DO i=1,NumberOfFindReplace_max
      SELECT CASE(i)
       CASE(1)
        Find_and_Replace = Find_and_Replace_1
       CASE(2)
        Find_and_Replace = Find_and_Replace_2
       CASE(3)
        Find_and_Replace = Find_and_Replace_3
       CASE(4)
        Find_and_Replace = Find_and_Replace_4
       CASE(5)
        Find_and_Replace = Find_and_Replace_5
       CASE(6)
        Find_and_Replace = Find_and_Replace_6
       CASE(7)
        Find_and_Replace = Find_and_Replace_7
       CASE(8)
        Find_and_Replace = Find_and_Replace_8
       CASE(9)
        Find_and_Replace = Find_and_Replace_9
       CASE(10)
        Find_and_Replace = Find_and_Replace_10
       CASE(11)
        Find_and_Replace = Find_and_Replace_11
       CASE(12)
        Find_and_Replace = Find_and_Replace_12
       CASE(13)
        Find_and_Replace = Find_and_Replace_13
       CASE(14)
        Find_and_Replace = Find_and_Replace_14
       CASE(15)
        Find_and_Replace = Find_and_Replace_15
       CASE(16)
        Find_and_Replace = Find_and_Replace_16
       CASE(17)
        Find_and_Replace = Find_and_Replace_17
       CASE(18)
        Find_and_Replace = Find_and_Replace_18
       CASE(19)
        Find_and_Replace = Find_and_Replace_19
       CASE(20)
        Find_and_Replace = Find_and_Replace_20
       CASE(21)
        Find_and_Replace = Find_and_Replace_21
       CASE(22)
        Find_and_Replace = Find_and_Replace_22
       CASE(23)
        Find_and_Replace = Find_and_Replace_23
       CASE(24)
        Find_and_Replace = Find_and_Replace_24
       CASE(25)
        Find_and_Replace = Find_and_Replace_25
       CASE(26)
        Find_and_Replace = Find_and_Replace_26
       CASE(27)
        Find_and_Replace = Find_and_Replace_27
       CASE(28)
        Find_and_Replace = Find_and_Replace_28
       CASE(29)
        Find_and_Replace = Find_and_Replace_29
       CASE(30)
        Find_and_Replace = Find_and_Replace_30
       CASE(31)
        Find_and_Replace = Find_and_Replace_31
       CASE(32)
        Find_and_Replace = Find_and_Replace_32
       CASE(33)
        Find_and_Replace = Find_and_Replace_33
       CASE(34)
        Find_and_Replace = Find_and_Replace_34
       CASE(35)
        Find_and_Replace = Find_and_Replace_35
       CASE(36)
        Find_and_Replace = Find_and_Replace_36
       CASE(37)
        Find_and_Replace = Find_and_Replace_37
       CASE(38)
        Find_and_Replace = Find_and_Replace_38
       CASE(39)
        Find_and_Replace = Find_and_Replace_39
       CASE(40)
        Find_and_Replace = Find_and_Replace_40
       CASE DEFAULT
        WRITE(my_output_unit,*) "NumberOfFindReplace_max ill-defined. NumberOfFindReplace_max = ",NumberOfFindReplace_max
        STOP
      END SELECT
      StringToFindCV   (i) = TRIM(Find_and_Replace%StringToFindC)    ! Note: String to find is case sensitive.
      StringToReplaceCV(i) = TRIM(Find_and_Replace%StringToReplaceC)

      IF (TRIM(Find_and_Replace%StringToFindC) /= TRIM(UnrealisticStringC)) THEN
                                  StringFindAndReplaceLV(i) = .TRUE.
       WRITE(my_output_unit,'(1x,A,A,A)') TRIM(StringToFindCV   (i))," ==> ", &
                                          TRIM(StringToReplaceCV(i))
      END IF

     END DO  
     WRITE(my_output_unit,'(A)') " "

!********************************************************************
 END IF
!********************************************************************

 !----------------------------------------------------------------
 ! 'input_filename_C' is a string that could include directories.
 !----------------------------------------------------------------

 !-------------------------------------------------------------
 ! Get actual filename, i.e. filename without any directories.
 !-------------------------------------------------------------
 InputFileName_NoDirectoryC = FileName_Without_Directory(input_filename_C)

 !------------------------------------------------------------------------------------------------------------------------
 ! Get actual filename, i.e. filename without any directories and without file extension and store it in global variable.
 !------------------------------------------------------------------------------------------------------------------------
 InputFileName_NoDirectoryNoExtensionC = FileName_Without_FileExtension(InputFileName_NoDirectoryC)
!WRITE(my_output_unit,'(A,A,A)')     " Input filename without file extension is '",TRIM(InputFileName_NoDirectoryNoExtensionC),"'."

 Folder_InputFilename_outC = TRIM(GetGlobalDirectoryName(''))//TRIM(InputFileName_NoDirectoryC)
 !----------------------------------------------------------------------------------------------
 ! Replace in string all backslashes and slashes with the specific one of the operating system.
 !----------------------------------------------------------------------------------------------
 CALL ReplaceSlashes( Folder_InputFilename_outC )

   IF (AnyMacroL) THEN
     !---------------------------------------------------------------------------
     ! Here we open a new file where we write out the input file information.
     ! Here, the macro is "executed", i.e. the appropriate strings are replaced.
     ! Then we have a backup of the input file without macro variables.
     ! Example: 'my_input_file.in' ==> 'my_input_file.in.no_macro'
     !                                 'my_input_file.in.compact'
     !---------------------------------------------------------------------------
     OPEN (33,file = TRIM(Folder_InputFilename_outC)//'.no_macro')  ! This works if --outputdirectory is specified via the command line but if it is specified via input file, it is not taken into account.
   END IF
   IF (WriteCompactFileL) THEN
     OPEN (34,file = TRIM(Folder_InputFilename_outC)//'.compact')   ! This works if --outputdirectory is specified via the command line but if it is specified via input file, it is not taken into account.
   END IF

   line_number = 0

   DO

     line_number = line_number + 1

     !---------------------------------------------------------------
     ! CHECK: This assignment is not necessary any more because the
     ! information is contained in 'StringsV'.
     ! However, we keep it because of 'Find & Replace String' macro
     ! as we (probably) do not know the line number in this case.
     !---------------------------------------------------------------
     READ (10,'(A)',IOSTAT = ios) bufferC_read
     IF (ios < 0) EXIT
     bufferC = TRIM(bufferC_read)

     IF (.NOT. UseMacroFindAndReplaceL) THEN
        bufferC = StringsV(line_number)%StringC
     END IF

    !********************************************************************
    IF (UseMacroFindAndReplaceL) THEN
    !********************************************************************
    ! For 'Find & Replace String' macro.
    !********************************************************************

     !------------------------
     ! We ignore empty lines.
     !------------------------
     IF ( TRIM(bufferC) /= '' ) THEN
      IF (DebugLeveL > 100) WRITE(my_output_unit,"(A,A)") "bufferC = ",TRIM(bufferC)
      DO i=1,SIZE(StringToFindCV)
     
       IF (StringFindAndReplaceLV(i)) THEN
        buffer_before_ReplacementC = TRIM(bufferC)
        bufferC_long               = buffer_before_ReplacementC
        CALL StringReplace ( bufferC_long, TRIM(StringToFindCV   (i)), & ! The buffer string must be long enough to cover the case where the replacing string is longer than the original string to be replaced.
                                           TRIM(StringToReplaceCV(i)), NumberOfReplacementsMade )
        IF ( ABS(NumberOfReplacementsMade) > 0 ) THEN
          bufferC = TRIM(bufferC_long)
          WRITE(my_output_unit,"(A,A)")                " Find:    ",TRIM(buffer_before_ReplacementC)
          WRITE(my_output_unit,"(A,A,A,A,A,A,A,I8,A)") " Replace: ",TRIM(bufferC)," ! '", &
                                                       TRIM(StringToFindCV   (i)),"' ==> '", &
                                                       TRIM(StringToReplaceCV(i)),"' (line number ",line_number,")"
         IF ( NumberOfReplacementsMade < 0 ) THEN
          WRITE(my_output_unit,"(A,I5)") "number of string replacements made = ",NumberOfReplacementsMade
          WRITE(my_output_unit,"(A)")    "A negative number indicates a problem. "// &
                                         "At least one substring could not be replaced because there was no more space."
         END IF
          WRITE(my_output_unit,"(A)") " "
        END IF
       END IF

      END DO
     END IF

    END IF
    !********************************************************************

     !---------------------------------------------
     ! Write replaced string to '*.no_macro' file.
     !---------------------------------------------
     IF (AnyMacroL) THEN
         WRITE(33,'(A)') TRIM(bufferC)
     END IF

     !----------------------------------------------------
     ! Allow for multiline comments.
     !
     !   !TEXT
     !       This is a multi-
     !       line comment.
     !   !ENDTEXT
     !
     ! If multiline comment, then bufferC will be erased.
     !----------------------------------------------------
     CALL Multiline_Comment( TEXT_String_BeginC, &
                             TEXT_String_EndC  , &
                             bufferC , Multiline_Comment_OnL )

     !---------------------------------------------------------------------
     ! Allow for '!DATA'.
     !
     !   !DATA
     !       Some data.
     !   !ENDDATA                ('!ENDDATA' is optional)
     !
     ! Everything until '!ENDDATA' or end of file will be treated as DATA.
     !---------------------------------------------------------------------
     CALL Extract_DATA( DATA_String_BeginC, &
                        DATA_String_EndC  , &
                        bufferC , DATA_Comment_OnL ) ! bufferC is input and output
     IF (DATA_Comment_OnL) THEN
        IF ( .NOT. ALLOCATED(Strings_for_DATA_V) ) THEN
           Line_of_DATA = line_number
           Line_of_DATA_count = 0
           ALLOCATE( Strings_for_DATA_V(NumberOfLines - Line_of_DATA) )
         ! bufferC = ''                             ! erase bufferC (Not needed here as bufferC was already erased in SUBROUTINE Extract_DATA.)
         Write_DATA_FileL = .TRUE.
         SELECT CASE( TRIM(OperatingSystemC) )
          CASE('windows')
           Batch_FileExtensionC = Batch_WindowsC    ! '.bat' on Windows
          CASE DEFAULT
           Batch_FileExtensionC = Batch_LinuxC      ! '.sh' on Linux
         END SELECT
         IF ( DATA_Filename_FixedL ) THEN
              DATA_FileC = TRIM(GetGlobalDirectoryName(''))//'batch'//Batch_FileExtensionC
         ELSE
              DATA_FileC = TRIM(Folder_InputFilename_outC)          //Batch_FileExtensionC
         END IF
        ELSE
           Line_of_DATA_count = Line_of_DATA_count + 1
           Strings_for_DATA_V(Line_of_DATA_count)%StringC = bufferC
           bufferC = ''                             ! erase bufferC
        END IF
     END IF

     !---------------------------------------------------
     ! 'bufferC' is input and output to this subroutine.
     !---------------------------------------------------
     CALL push(queue_1,bufferC,line_number,comment_signsCV)              ! push node onto top of queue_1

     IF (WriteCompactFileL) THEN
      !-------------------------------------------------------------------------------------
      ! Compact file does not include content that was intended for special string '!DATA'.
      !-------------------------------------------------------------------------------------
      IF (bufferC /= '') THEN
          WRITE(34,'(A)') TRIM(bufferC)
      END IF
     END IF

   END DO
   
   !-----------------------------------------------------------
   ! Close the input file where the macro has been "executed".
   !-----------------------------------------------------------
   IF (AnyMacroL) THEN
      CLOSE(33)
   ELSE
    IF (DebugLevel > 1) THEN
      WRITE(my_output_unit,'(A)') " Macro status: No macro definition found in input file. Macro feature is not used."
    END IF
   END IF

   IF (WriteCompactFileL) THEN
      CLOSE(34)
   END IF

   IF (Write_DATA_FileL) THEN
      OPEN (35,file = DATA_FileC)   ! This works if --outputdirectory is specified via the command line but if it is specified via input file, it is not taken into account.
       DO i=1,Line_of_DATA_count
        WRITE(35,'(A)') TRIM( Strings_for_DATA_V(i)%StringC )
       END DO
      CLOSE(35)
   END IF
   
   !-----------------------------------------------------------
   ! Close the real input file.
   !-----------------------------------------------------------
   CLOSE(10)

   DEALLOCATE(StringsV)

   IF ( ALLOCATED( Strings_for_DATA_V ) ) THEN
        DEALLOCATE(Strings_for_DATA_V)
   END IF

   !********************************************************************
   IF (UseMacroFindAndReplaceL) THEN
   !********************************************************************
   ! For 'Find & Replace String' macro.
   !********************************************************************
    DEALLOCATE(StringToFindCV)
    DEALLOCATE(StringToReplaceCV)
    DEALLOCATE(StringFindAndReplaceLV)
   END IF
   !********************************************************************

!------------------------------------------------------------------------------
 END SUBROUTINE queue_built_up
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE input_data_types
!------------------------------------------------------------------------------
 USE parser_parameters,ONLY:char_length_specifier_content

 IMPLICIT NONE

 REAL(4)                                  :: xs_t !
 REAL(8)                                  :: xd_t !
 INTEGER                                  :: in_t !
 LOGICAL                                  :: lo_t !
!CHARACTER(Data_len/3)                    :: ca_t !
!CHARACTER(Data_len)                      :: ca_t ! to allow for long strings, e.g. long directory names
 CHARACTER(char_length_specifier_content) :: ca_t ! to allow for long strings, e.g. long directory names
!CHARACTER(len=:),ALLOCATABLE             :: ca_t ! to allow for long strings, e.g. long directory names  <= does not work

!------------------------------------------------------------------------------
 END MODULE input_data_types
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE input_spec_node_def                                             ! define derived data type for one node in queue of specifiers
!------------------------------------------------------------------------------
 USE input_type_names       ,ONLY:char_length_type_name, &
                                  char_length_specifier_name
 USE parser_parameters      ,ONLY:char_length_specifier_content
 USE array_type_specifiers  ,ONLY:in_array_node, &
                                  xs_array_node, &
                                  xd_array_node, &
                                  in_array_queue, &
                                  xs_array_queue, &
                                  xd_array_queue

 IMPLICIT NONE

   TYPE :: input_specifier                                             !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spxs       ! specifier names for real numbers
     CHARACTER(char_length_type_name)     ,DIMENSION(:),POINTER :: tyxs       ! type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opxs                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prxs                ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: lixs                ! value in line number of input file
     REAL(4)     ,DIMENSION(:),POINTER :: xs                  ! array for real numbers (values for specifiers)
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spxd                ! specifier names for real double precision numbers
     CHARACTER(char_length_type_name)     ,DIMENSION(:),POINTER :: tyxd                ! data type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opxd                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prxd                ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: lixd                ! value in line number of input file
     REAL(8)     ,DIMENSION(:),POINTER :: xd                  ! array for double precision real numbers
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spin                ! specifier names for integer numbers
     CHARACTER(char_length_type_name)     ,DIMENSION(:),POINTER :: tyin                ! data type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opin                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prin                ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: liin                ! value in line number of input file
     INTEGER              ,DIMENSION(:),POINTER :: in                  ! array for integer numbers
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spca                ! specifier names for character string valued input
     CHARACTER(char_length_type_name)     ,DIMENSION(:),POINTER :: tyca                ! data type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opca                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prcaL               ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: lica                ! value in line number of input file
     CHARACTER(char_length_specifier_content)  ,DIMENSION(:),POINTER :: ca                  ! array for character string valued input ! to allow for long strings, e.g. long directory names
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: splo                ! specifier names for logical valued input
     CHARACTER(char_length_type_name)     ,DIMENSION(:),POINTER :: tylo                ! data type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: oplo                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prlo                ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: lilo                ! value in line number of input file
     LOGICAL              ,DIMENSION(:),POINTER :: lo                  ! array for logical valued input
                                                                       !
     TYPE(in_array_queue),POINTER :: inar_queue                        ! queue for specifiers with array valued integer input
                                                                       !
     TYPE(xs_array_queue),POINTER :: xsar_queue                        ! queue for specifiers with array valued real input
                                                                       !
     TYPE(xd_array_queue),POINTER :: xdar_queue                        ! queue for specifiers with array valued double precision input
                                                                       !
   END TYPE input_specifier                                            !
!----------------------------------------------------------------------!
                                                                       !
   TYPE :: inp_spec_node                                               !
     TYPE(input_specifier),POINTER :: input_spec                       !
     TYPE(inp_spec_node) ,POINTER :: next_specifier                    ! pointer to successor node in queue
   END TYPE inp_spec_node                                              !
!------------------------------------------------------------------------------
END MODULE input_spec_node_def                                         !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE input_specifier_queue_def                                       ! queue type definition for specifier queue, associated to a certain keyword node
!------------------------------------------------------------------------------
   USE input_spec_node_def ! ,ONLY:inp_spec_node

 IMPLICIT NONE

   TYPE :: input_spec_queue                                            !
     TYPE(inp_spec_node), POINTER :: top                               ! pointer to top  node of specifier queue
     TYPE(inp_spec_node), POINTER :: rear                              ! pointer to rear node of specifier queue
   END TYPE input_spec_queue                                           !
!------------------------------------------------------------------------------
END MODULE input_specifier_queue_def                                   !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE input_key_node_def                                              ! define derived data type for one node in queue of keywords
!------------------------------------------------------------------------------
   USE input_specifier_queue_def ! ,ONLY:input_spec_queue

 IMPLICIT NONE

   TYPE :: input_key_node                                              !
     CHARACTER(1),DIMENSION(:),POINTER :: keyword                      ! pointer to character array, containing a keyword
     INTEGER                           :: length                       ! number of characters in keyword name
     TYPE(input_spec_queue),   POINTER :: entries                      ! pointer to specifier queue, associated with a keyword node
     TYPE(input_key_node)  ,   POINTER :: next_key                     ! pointer to successor node in keyword queue
   END TYPE input_key_node                                             !
!------------------------------------------------------------------------------
END MODULE input_key_node_def                                          !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE input_key_queue_def                                             ! queue type definition for keyword queue
!------------------------------------------------------------------------------
   USE input_key_node_def ! ,ONLY:input_key_node

 IMPLICIT NONE

   TYPE :: input_key_queue                                             !
     TYPE(input_key_node), POINTER :: top                              ! pointer to top  node of keyword queue
     TYPE(input_key_node), POINTER :: rear                             ! pointer to rear node of keyword queue
   END TYPE input_key_queue                                            !
!------------------------------------------------------------------------------
END MODULE input_key_queue_def                                         !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_init_input_key_queue
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE init_input_key_queue(s)                                    ! Initialize an empty keyword_queue 
!------------------------------------------------------------------------------
   USE input_key_queue_def,ONLY:input_key_queue

   IMPLICIT NONE

   TYPE(input_key_queue), INTENT (OUT) :: s                            !
   NULLIFY (s%top,s%rear)                                              ! Disassociate pointer to top and rear of keyword queue

!------------------------------------------------------------------------------
END SUBROUTINE init_input_key_queue
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_init_input_key_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_add_input_key
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE add_input_key (s, bufferC)
!------------------------------------------------------------------------------
 USE parser_parameters  ,ONLY: key_char
 USE input_key_queue_def,ONLY:input_key_queue

 IMPLICIT NONE

 TYPE(input_key_queue), INTENT (IN OUT) :: s                         !
 CHARACTER(*),     INTENT (IN OUT) :: bufferC                        !
 CHARACTER(1),DIMENSION(:),POINTER :: local_line                     !

 INTEGER :: line_length,ichar

   bufferC = TRIM(ADJUSTL(bufferC))                                    ! remove leading and trailing blanks from line
   line_length = LEN_TRIM(ADJUSTL(bufferC))                            ! get length of input line
                                                                       !
   IF (line_length == 0) CALL ERROR(1)                                 ! if line is empty, do not generate queue entry ==>>>STOP and call for ERROR
   IF (bufferC(1:1)/=key_char) CALL ERROR(2)                            !
                                                                       !
   ALLOCATE(local_line(line_length))                                   !
   DO ichar=1,line_length                                              !
   local_line(ichar) = bufferC(ichar:ichar)                             ! store data in local_line; new node keyword will poit to this line
   END DO                                                              !
                                                                       ! 
     IF (.NOT. ASSOCIATED (s%top)) THEN                                !
       ALLOCATE(s%top)                                                 !
       ALLOCATE(s%top%entries)                                         !
       NULLIFY (s%top%entries%top)                                     !
       NULLIFY (s%top%entries%rear)                                    !
       s%top%keyword => local_line                                     !
       s%top%length  =  line_length                                    !
       NULLIFY(s%top%next_key)                                         !
       s%rear => s%top                                                 !
     ELSE                                                              !
       ALLOCATE(s%rear%next_key)                                       !
       ALLOCATE(s%rear%next_key%entries)                               !
       NULLIFY (s%rear%next_key%entries%top)                           !
       NULLIFY (s%rear%next_key%entries%rear)                          !
       s%rear%next_key%keyword => local_line                           !
       s%rear%next_key%length  =  line_length                          !
       NULLIFY(s%rear%next_key%next_key)                               !
       s%rear => s%rear%next_key                                       !
     END IF                                                            !
       NULLIFY(s%rear%next_key)                                        !          vmtl ueberfluessig
                                                                       !
!----------------------------------------------------------------------!
CONTAINS                                                               !
!------------------------------------------------------------------------------
  SUBROUTINE ERROR(error_number)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Empty keyword string not allowed in SUB. add_input_key.'!
    WRITE(my_output_unit,*)'Illegal attempt to add keyword input node'              !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE add_input_key                            '!
    WRITE(my_output_unit,*)'keyword string must start with ',key_char,' sign       '!
    WRITE(my_output_unit,*)'Illegal attempt to add keyword input node              '!
    STOP                                                               !
   ELSE IF(error_number>=3 .OR. error_number<=0)THEN                   !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE add_input_key         '!
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE add_input_key
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE mod_add_input_key
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE position_at_key_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE position_at_key(s,bufferC,node,key_positioned)
!------------------------------------------------------------------------------
 USE input_key_queue_def,ONLY:input_key_queue
 USE input_key_node_def ,ONLY:input_key_node

 IMPLICIT NONE

 TYPE(input_key_queue)           :: s                                  !
 CHARACTER(len=*)    ,INTENT(in) :: bufferC                             !
 TYPE(input_key_node),POINTER    :: node                               !
 LOGICAL                         :: key_positioned                     !

 INTEGER                         :: i
                                                                       !
   key_positioned = .FALSE.                                            !
   IF (.NOT.ASSOCIATED(s%top)) CALL ERROR(1)                           !
    node => s%top                                                      !
    DO 
     IF (.NOT. ASSOCIATED(node) .OR. key_positioned) EXIT
     IF (node%length < 1) CALL ERROR(1)                                !
     IF (LEN_TRIM(bufferC) < 1) CALL ERROR(2)                           !
     IF (LEN_TRIM(bufferC)==node%length) THEN                           !
     key_positioned = .TRUE.                                           !
     DO i=1,node%length                                                !
      IF (bufferC(i:i)/=node%keyword(i)) key_positioned = .FALSE.       !
     ENDDO                                                             !
     END IF                                                            !
    IF (.NOT.key_positioned) node => node%next_key                     !
    ENDDO                                                              !
!----------------------------------------------------------------------!
CONTAINS                                                               !
!------------------------------------------------------------------------------
  SUBROUTINE ERROR(error_number)                                       !
!------------------------------------------------------------------------------

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   INTEGER error_number                                                !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE position_at_key.    '!
    WRITE(my_output_unit,*)'Got empty input_key_queue with empty entry'             !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE position_at_key.                         '!
    WRITE(my_output_unit,*)'Empty keyword string not allowed in this context.      '!
    WRITE(my_output_unit,*)'Illegal attempt to position at keyword input node      '!
    STOP                                                               !
   ELSE IF(error_number>2 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE position_at_key.      '!
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF

!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE position_at_key
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE position_at_key_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE scan_inp_key_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE scan_inp_keys(s,bufferC,found_inp_key)
!------------------------------------------------------------------------------
 USE input_key_queue_def,ONLY:input_key_queue
 USE input_key_node_def ,ONLY:input_key_node

 IMPLICIT NONE

   TYPE(input_key_queue)             :: s                              !
   CHARACTER(*), INTENT (IN)         :: bufferC                         !
   TYPE(input_key_node), POINTER     :: a1                             !
   LOGICAL                           :: found_inp_key                  !
   integer i
                                                                       !
   found_inp_key = .FALSE.                                             !
                                                                       !
   IF (ASSOCIATED(s%top)) THEN                                         !
    a1 => s%top                                                        !
    DO
     IF (.NOT. ASSOCIATED(a1) .OR. found_inp_key) EXIT
     IF (a1%length < 1) CALL ERROR(1)                                  !
     IF (LEN_TRIM(bufferC) < 1) CALL ERROR(2)                           !
     IF (LEN_TRIM(bufferC)==a1%length) THEN                             !
     found_inp_key = .TRUE.                                            !
     DO i=1,a1%length                                                  !
      IF (bufferC(i:i)/=a1%keyword(i)) found_inp_key = .FALSE.          !
     ENDDO                                                             !
     END IF                                                            !
    a1 => a1%next_key                                                  !
    ENDDO                                                              !
   END IF                                                              !
   NULLIFY(a1)                                                         !
!----------------------------------------------------------------------!
CONTAINS                                                               !
!------------------------------------------------------------------------------
  SUBROUTINE ERROR(error_number)                                       !
!------------------------------------------------------------------------------

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   INTEGER error_number                                                !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Empty keyword string not allowed in SUB. scan_inp_keys.'!
    WRITE(my_output_unit,*)'Got input_key_queue with empty entry'                   !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE scan_inp_keys.                           '!
    WRITE(my_output_unit,*)'Keyword string must be not empty.                      '!
    WRITE(my_output_unit,*)'Illegal attempt to scan for keyword input node         '!
    STOP                                                               !
   ELSE IF(error_number>2 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE scan_inp_keys.        '!
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !

!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE scan_inp_keys
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE scan_inp_key_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE add_inp_spec_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE add_inp_spec(s,keywords,keyC,spec)
!------------------------------------------------------------------------------
 USE input_type_names,ONLY:name_xs,name_xd,name_in,name_ca,name_lo, &
                           name_inar,name_xsar,name_xdar, &
                           char_length_type_name, &
                           char_length_choice_name
 USE keyword_queue_def,ONLY:keyword_queue
 USE input_key_queue_def ! ,ONLY:input_key_queue
 USE mod_get_key_info,ONLY:get_key_info
 USE position_at_key_interface,ONLY:position_at_key

 IMPLICIT NONE

   TYPE(input_key_queue)             :: s                              !
   TYPE(in_array_node),POINTER       :: local_inar                     !
   TYPE(xs_array_node),POINTER       :: local_xsar                     !
   TYPE(xd_array_node),POINTER       :: local_xdar                     !
   TYPE(input_key_node),POINTER,SAVE :: node                           !
   TYPE(inp_spec_node),POINTER       :: spec_node                      !
   TYPE(input_specifier),POINTER     :: spec_structure                 !
   TYPE(keyword_queue),POINTER       :: keywords                       !
   LOGICAL                           :: key_positioned                 !
   LOGICAL                           :: found_no_specifierL,optional_specifierL    !
                                                                       !
 CHARACTER(len=char_length_type_name)   :: data_type
 LOGICAL                                :: ChoiceL
 CHARACTER(len=char_length_choice_name) :: ChoiceC

   CHARACTER(*), INTENT (IN OUT) :: keyC
   CHARACTER(*), INTENT (IN OUT) :: spec

   INTEGER :: num_xs,num_xd,num_in,num_ca,num_lo                       !
   integer num_inar,num_xsar,num_xdar,i
                                                                       !
   CALL position_at_key(s,keyC,node,key_positioned)
   IF (.NOT. key_positioned) CALL ERROR(2)                             !
                                                                       !
   ALLOCATE(spec_node)                                                 !
   ALLOCATE(spec_structure)                                            !
   ALLOCATE(spec_structure%inar_queue)                                 !
   ALLOCATE(spec_structure%xsar_queue)                                 !
   ALLOCATE(spec_structure%xdar_queue)                                 !
   NULLIFY(spec_structure%inar_queue%top)                              !
   NULLIFY(spec_structure%inar_queue%rear)                             !
   NULLIFY(spec_structure%xsar_queue%top)                              !
   NULLIFY(spec_structure%xsar_queue%rear)                             !
   NULLIFY(spec_structure%xdar_queue%top)                              !
   NULLIFY(spec_structure%xdar_queue%rear)                             !
                                                                       !
    num_xs   = 0                                                       !
    num_xd   = 0                                                       !
    num_in   = 0                                                       !
    num_ca   = 0                                                       !
    num_lo   = 0                                                       !
    num_inar = 0                                                       !
    num_xsar = 0                                                       !
    num_xdar = 0                                                       !
   DO                                                                  !
    CALL get_key_info(keywords,keyC, &
                      spec,data_type,optional_specifierL,ChoiceL,ChoiceC,found_no_specifierL)
    IF (found_no_specifierL) EXIT
    IF      (TRIM(data_type)==TRIM(name_xs)) THEN                      !
     num_xs = num_xs + 1                                               !
    ELSE IF (TRIM(data_type)==TRIM(name_xd)) THEN                      !
     num_xd = num_xd + 1                                               !
    ELSE IF (TRIM(data_type)==TRIM(name_in)) THEN                      !
     num_in = num_in + 1                                               !
    ELSE IF (TRIM(data_type)==TRIM(name_ca)) THEN                      !
     num_ca = num_ca + 1                                               !
    ELSE IF (TRIM(data_type)==TRIM(name_lo)) THEN                      !
     num_lo = num_lo + 1                                               !
    ELSE IF (TRIM(data_type)==TRIM(name_inar)) THEN                    !
     num_inar = num_inar + 1                                           !
    ELSE IF (TRIM(data_type)==TRIM(name_xsar)) THEN                    !
     num_xsar = num_xsar + 1                                           !
    ELSE IF (TRIM(data_type)==TRIM(name_xdar)) THEN                    !
     num_xdar = num_xdar + 1                                           !
    ELSE                                                               !
     CALL ERROR(1)                                                     !
    END IF                                                             !
   END DO                                                              !
                                                                       !
                                                                       !
   ALLOCATE(spec_structure%spxs(num_xs))                               !
   ALLOCATE(spec_structure%tyxs(num_xs))                               !
   ALLOCATE(spec_structure%opxs(num_xs))                               !
   ALLOCATE(spec_structure%prxs(num_xs))                               !
   ALLOCATE(spec_structure%lixs(num_xs))                               !
   ALLOCATE(spec_structure%xs  (num_xs))                               !
                                                                       !
   ALLOCATE(spec_structure%spxd(num_xd))                               !
   ALLOCATE(spec_structure%tyxd(num_xd))                               !
   ALLOCATE(spec_structure%opxd(num_xd))                               !
   ALLOCATE(spec_structure%prxd(num_xd))                               !
   ALLOCATE(spec_structure%lixd(num_xd))                               !
   ALLOCATE(spec_structure%xd  (num_xd))                               !
                                                                       !
   ALLOCATE(spec_structure%spin(num_in))                               !
   ALLOCATE(spec_structure%tyin(num_in))                               !
   ALLOCATE(spec_structure%opin(num_in))                               !
   ALLOCATE(spec_structure%prin(num_in))                               !
   ALLOCATE(spec_structure%liin(num_in))                               !
   ALLOCATE(spec_structure%in  (num_in))                               !
                                                                       !
   ALLOCATE(spec_structure%spca(num_ca))                               !
   ALLOCATE(spec_structure%tyca(num_ca))                               !
   ALLOCATE(spec_structure%opca(num_ca))                               !
   ALLOCATE(spec_structure%prcaL(num_ca))                              !
   ALLOCATE(spec_structure%lica(num_ca))                               !
   ALLOCATE(spec_structure%ca  (num_ca))                               !
                                                                       !
   ALLOCATE(spec_structure%splo(num_lo))                               !
   ALLOCATE(spec_structure%tylo(num_lo))                               !
   ALLOCATE(spec_structure%oplo(num_lo))                               !
   ALLOCATE(spec_structure%prlo(num_lo))                               !
   ALLOCATE(spec_structure%lilo(num_lo))                               !
   ALLOCATE(spec_structure%lo  (num_lo))                               !

   DO i=1,num_inar                                                     !
    IF (i==1) THEN                                                     !  IF (.NOT. ASSOCIATED (s%top)) THEN
       ALLOCATE(spec_structure%inar_queue%top)                         !
       ALLOCATE(spec_structure%inar_queue%rear)                        !
       NULLIFY (spec_structure%inar_queue%top%inar)                    !
       NULLIFY (spec_structure%inar_queue%top%next_inar)               !
       spec_structure%inar_queue%rear => spec_structure%inar_queue%top !
     ELSE                                                              !
       ALLOCATE(spec_structure%inar_queue%rear%next_inar)              !
       NULLIFY (spec_structure%inar_queue%rear%next_inar%inar)         !
       NULLIFY (spec_structure%inar_queue%rear%next_inar%next_inar)    !
       spec_structure%inar_queue%rear =>                              &
                              spec_structure%inar_queue%rear%next_inar !
     END IF                                                            !
   END DO                                                              !
                                                                       !
   DO i=1,num_xsar                                                     !
    IF (i==1) THEN                                                     !  IF (.NOT. ASSOCIATED (s%top)) THEN
       ALLOCATE(spec_structure%xsar_queue%top)                         !
       ALLOCATE(spec_structure%xsar_queue%rear)                        !
       NULLIFY (spec_structure%xsar_queue%top%xsar)                    !
       NULLIFY (spec_structure%xsar_queue%top%next_xsar)               !
       spec_structure%xsar_queue%rear => spec_structure%xsar_queue%top !
     ELSE                                                              !
       ALLOCATE(spec_structure%xsar_queue%rear%next_xsar)              !
       NULLIFY (spec_structure%xsar_queue%rear%next_xsar%xsar)         !
       NULLIFY (spec_structure%xsar_queue%rear%next_xsar%next_xsar)    !
       spec_structure%xsar_queue%rear =>                              &
                              spec_structure%xsar_queue%rear%next_xsar !
     END IF                                                            !
   END DO                                                              !
                                                                       !
   DO i=1,num_xdar                                                     !
    IF (i==1) THEN                                                     !  IF (.NOT. ASSOCIATED (s%top)) THEN
       ALLOCATE(spec_structure%xdar_queue%top)                         !
       ALLOCATE(spec_structure%xdar_queue%rear)                        !
       NULLIFY (spec_structure%xdar_queue%top%xdar)                    !
       NULLIFY (spec_structure%xdar_queue%top%next_xdar)               !
       spec_structure%xdar_queue%rear => spec_structure%xdar_queue%top !
     ELSE                                                              !
       ALLOCATE(spec_structure%xdar_queue%rear%next_xdar)              !
       NULLIFY (spec_structure%xdar_queue%rear%next_xdar%xdar)         !
       NULLIFY (spec_structure%xdar_queue%rear%next_xdar%next_xdar)    !
       spec_structure%xdar_queue%rear =>                              &
                              spec_structure%xdar_queue%rear%next_xdar !
     END IF                                                            !
   END DO                                                              !
                                                                       !
                                                                       !
                                                                       !
   spec_node%input_spec => spec_structure                              !
   NULLIFY(spec_node%next_specifier)                                   !
                                                                       !
   IF (.NOT.ASSOCIATED(node%entries%top)) THEN                         !
     ALLOCATE(node%entries%top)                                        !
     node%entries%top    => spec_node                                  !
     node%entries%rear   => node%entries%top                           !
   ELSE                                                                !
     DO
      IF (.NOT. ASSOCIATED(node%entries%rear%next_specifier)) EXIT
      node%entries%rear => node%entries%rear%next_specifier            !
     END DO                                                            !
     ALLOCATE(node%entries%rear%next_specifier)                        !
     node%entries%rear%next_specifier   => spec_node                   !
     node%entries%rear => node%entries%rear%next_specifier             !
   END IF                                                              !
                                                                       !
                                                                       !
    num_xs = 0                                                         !
    num_xd = 0                                                         !
    num_in = 0                                                         !
    num_ca = 0                                                         !
    num_lo = 0                                                         !
    IF(ASSOCIATED(spec_structure%inar_queue%top)) THEN
     local_inar => spec_structure%inar_queue%top
    END IF
    IF(ASSOCIATED(spec_structure%xsar_queue%top)) THEN
     local_xsar => spec_structure%xsar_queue%top
    END IF
    IF(ASSOCIATED(spec_structure%xdar_queue%top)) THEN
     local_xdar => spec_structure%xdar_queue%top
    END IF
   DO                                                                  !
    CALL get_key_info(keywords,keyC, &
                      spec,data_type,optional_specifierL,ChoiceL,ChoiceC,found_no_specifierL)
    IF (found_no_specifierL) EXIT
    IF      (TRIM(data_type)==TRIM(name_xs)) THEN                      !
     num_xs = num_xs + 1                                               !
     spec_structure%spxs(num_xs) = TRIM(spec)                          !
     spec_structure%tyxs(num_xs) = TRIM(data_type)                     !
     spec_structure%opxs(num_xs) = optional_specifierL                 !
     spec_structure%lixs(num_xs) = 0                                   !
     spec_structure%prxs(num_xs) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_xd)) THEN                      !
     num_xd = num_xd + 1                                               !
     spec_structure%spxd(num_xd) = TRIM(spec)                          !
     spec_structure%tyxd(num_xd) = TRIM(data_type)                     !
     spec_structure%opxd(num_xd) = optional_specifierL                 !
     spec_structure%lixd(num_xd) = 0                                   !
     spec_structure%prxd(num_xd) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_in)) THEN                      !
     num_in = num_in + 1                                               !
     spec_structure%spin(num_in) = TRIM(spec)                          !
     spec_structure%tyin(num_in) = TRIM(data_type)                     !
     spec_structure%opin(num_in) = optional_specifierL                 !
     spec_structure%liin(num_in) = 0                                   !
     spec_structure%prin(num_in) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_ca)) THEN                      !
     num_ca = num_ca + 1                                               !
     spec_structure%spca(num_ca) = TRIM(spec)                          !
     spec_structure%tyca(num_ca) = TRIM(data_type)                     !
     spec_structure%opca(num_ca) = optional_specifierL                 !
     spec_structure%lica(num_ca) = 0                                   !
     spec_structure%prcaL(num_ca) = .FALSE.                            !
    ELSE IF (TRIM(data_type)==TRIM(name_lo)) THEN                      !
     num_lo = num_lo + 1                                               !
     spec_structure%splo(num_lo) = TRIM(spec)                          !
     spec_structure%tylo(num_lo) = TRIM(data_type)                     !
     spec_structure%oplo(num_lo) = optional_specifierL                 !
     spec_structure%lilo(num_lo) = 0                                   !
     spec_structure%prlo(num_lo) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_inar)) THEN                    !
     num_inar = num_inar + 1                                           !
     local_inar%spinar = TRIM(spec)                                    !
     local_inar%tyinar = TRIM(data_type)                               !
     local_inar%opinar = optional_specifierL                           !
     local_inar%liinar = 0                                             !
     local_inar%prinar = .FALSE.                                       !
!    ALLOCATE(local_inar%inar(0))                                      !
     local_inar => local_inar%next_inar                                !
    ELSE IF (TRIM(data_type)==TRIM(name_xsar)) THEN                    !
     num_xsar = num_xsar + 1                                           !
     local_xsar%spxsar = TRIM(spec)                                    !
     local_xsar%tyxsar = TRIM(data_type)                               !
     local_xsar%opxsar = optional_specifierL                           !
     local_xsar%lixsar = 0                                             !
     local_xsar%prxsar = .FALSE.                                       !
!    ALLOCATE(local_xsar%xsar(0))                                      !
     local_xsar => local_xsar%next_xsar                                !
    ELSE IF (TRIM(data_type)==TRIM(name_xdar)) THEN                    !
     num_xdar = num_xdar + 1                                           !
     local_xdar%spxdar = TRIM(spec)                                    !
     local_xdar%tyxdar = TRIM(data_type)                               !
     local_xdar%opxdar = optional_specifierL                           !
     local_xdar%lixdar = 0                                             !
     local_xdar%prxdar = .FALSE.                                       !
!    ALLOCATE(local_xdar%xdar(0))                                      !
     local_xdar => local_xdar%next_xdar                                !
    END IF                                                             !
   END DO                                                              !
!----------------------------------------------------------------------!
 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE ERROR(error_number)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 INTEGER,INTENT(in) :: error_number

    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    IF(error_number==1)THEN                                            !
    WRITE(my_output_unit,*)'In SUBROUTINE add_inp_key.                             '!
    WRITE(my_output_unit,*)'Got illegal data type = ',TRIM(data_type)
    WRITE(my_output_unit,*)'Check consistency of specifier data types in files     '!
    WRITE(my_output_unit,*)'keywords.f90 and input_type_names.f90                  '!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)'Occured in SUBROUTINE add_inp_spec.                    '!
    WRITE(my_output_unit,*)"Couldn't position at keyword = ",TRIM(keyC)              !
    WRITE(my_output_unit,*)'Stoped after call to Subroutine position_at_key.       '!
    STOP                                                               !
   ELSE IF(error_number>2 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE add_inp_spec.         '!
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !

!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE add_inp_spec
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE add_inp_spec_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE input_built_up_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE input_built_up(collected_input,bufferC)
!------------------------------------------------------------------------------
 USE parser_parameters        ,ONLY:SecondEntry_InputBuiltUpL
 USE input_key_queue_def      ,ONLY:input_key_queue,input_key_node
 USE scan_inp_key_interface   ,ONLY:scan_inp_keys
 USE position_at_key_interface,ONLY:position_at_key
 USE mod_add_input_key        ,ONLY:add_input_key
 USE mod_init_input_key_queue ,ONLY:init_input_key_queue

 IMPLICIT NONE

 TYPE(input_key_queue)              :: collected_input                !
 CHARACTER(*)        ,INTENT(inout) :: bufferC                         !
 TYPE(input_key_node),POINTER       :: node                           !
!LOGICAL                            :: first_entry = .TRUE.           ! first_entry acquires SAVE attribute automatically!!!
 LOGICAL,SAVE                       :: first_entry = .TRUE.           ! S. Birner: To make this statement more obvious, we explicitly state the SAVE attribute.
 LOGICAL                            :: found_inp_key                  !
 LOGICAL                            :: key_positioned                 !

   !------------------------------------------------------------------------------
   ! The flag SecondEntry_InputBuiltUpL is needed for the input parser
   ! if input file is read in several times.
   ! If SecondEntry_InputBuiltUpL = .TRUE. then we must set first_entry to .TRUE.
   !------------------------------------------------------------------------------
   IF (SecondEntry_InputBuiltUpL .AND. .NOT.first_entry) THEN
    first_entry               = .TRUE.
    SecondEntry_InputBuiltUpL = .FALSE.
   END IF

   IF (first_entry) THEN
    !--------------------------------------------------------------------------
    ! Disassociate pointer 'collected_input' to top and rear of keyword queue.
    !--------------------------------------------------------------------------
    CALL init_input_key_queue (collected_input)                        ! create a new queue to collect sorted input data
    !--------------------------------------------------------------------
    ! 'collected_input%top' and 'collected_input%rear' are now nullified.
    !--------------------------------------------------------------------
    first_entry = .FALSE.                                              !
   END IF                                                              !
                                                                       !
   CALL scan_inp_keys (collected_input,bufferC,found_inp_key)           !
   IF (.NOT.found_inp_key) THEN                                        !
    CALL add_input_key (collected_input,bufferC)                        ! push node for new keyword onto top of queue collected_input
   END IF                                                              !
   CALL position_at_key(collected_input,bufferC,node,key_positioned)    ! kann spaeter eliminiert werden
   IF(.NOT.key_positioned) CALL ERROR(1)                               ! kann spaeter eliminiert werden
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   INTEGER error_number                                                !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    IF(error_number==1)THEN                                            !
    WRITE(my_output_unit,*)'In SUBROUTINE input_built_up.                          '!
    WRITE(my_output_unit,*)'After call to SUBROUTINE add_input_key, a control call '!
    WRITE(my_output_unit,*)'to SUBROUTINE position_at_key was not able to find     '!
    WRITE(my_output_unit,*)'position of new keyword entry in queue collected_input '!
    STOP                                                               !
   ELSE IF(error_number>1 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE input_built_up.       '!
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 2.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!

!------------------------------------------------------------------------------
 END SUBROUTINE input_built_up
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE input_built_up_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_value_to_queue
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE value_to_queue(s,keywords,keywordC,specifierC,SpecifierValueC,line_number)
!------------------------------------------------------------------------------
!
!++s* mod_value_to_queue/value_to_queue
!
! NAME
!   SUBROUTINE value_to_queue
!
! PURPOSE
!   Add value of specifier 'SpecifierValueC' to queue.
!
! USAGE
!   CALL value_to_queue(s,keywords,keywordC,specifierC,SpecifierValueC,line_number)
! 
! INPUT
!   o s:
!   o keywords:
!   o keywordC:
!   o specifierC:
!   o SpecifierValueC:
!   o line_number
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
 USE mod_int_to_char999       ,ONLY:int_2_char ! converts integer to character
 USE String_Utility           ,ONLY:StringLowerCase, &
                                    Substring_is_contained_in_StringL
 USE Parser_Errors            ,ONLY:Print_Keyword_Specifier_Line
 USE mod_Print_Keywords_Queue ,ONLY:Replace_Choice_and_Delimiters
 USE mod_string_to_value      ,ONLY:convert_string_to_value
 USE input_type_names         ,ONLY:name_xs,name_xd,name_in,name_ca,       &
                                    name_lo,name_inar,name_xsar,name_xdar, &
                                    char_length_choice_name, &
                                    char_length_type_name
 USE input_data_types         ,ONLY:xs_t,xd_t,in_t,ca_t,lo_t
 USE keyword_queue_def,ONLY:keyword_queue
 USE input_key_queue_def
 USE position_at_key_interface,ONLY:position_at_key
 USE mod_string_in_list       ,ONLY:string_in_list
 USE generic_add                                                     !
 USE parser_parameters        ,ONLY:Data_len, &
                                    key_char, &
                                    keyword_filetypeC

 IMPLICIT NONE

 TYPE(input_key_queue)              :: s                              !
 TYPE(keyword_queue) ,POINTER       :: keywords                       !
 CHARACTER(len=*)    ,INTENT(in)    :: keywordC                       !
 CHARACTER(len=*)    ,INTENT(in)    :: specifierC                     !
 CHARACTER(len=*)    ,INTENT(in)    :: SpecifierValueC                !
 INTEGER             ,INTENT(in)    :: line_number                    !

 TYPE(input_key_node),POINTER,SAVE  :: node                           !
 TYPE(inp_spec_node) ,POINTER       :: spec_node                      !
 TYPE(in_array_node) ,POINTER       :: local_inar                     !
 TYPE(xs_array_node) ,POINTER       :: local_xsar                     !
 TYPE(xd_array_node) ,POINTER       :: local_xdar                     !
 LOGICAL                            :: key_positioned                 !
 LOGICAL                            :: found_keyL,found_specL         !
 LOGICAL                            :: added_value                    !

 CHARACTER(len=char_length_type_name)   :: data_typeC                 !
 LOGICAL                                :: choiceL
 CHARACTER(len=char_length_choice_name) :: choiceC
 CHARACTER(len=char_length_choice_name) :: choiceC_temp

 INTEGER                                :: num_xs,num_xd,num_in,num_ca,num_lo
 LOGICAL                                :: CheckStringL = .FALSE.
 LOGICAL                                :: String_is_contained_in_ChoiceL = .FALSE.
 CHARACTER(len=:),ALLOCATABLE           :: IntegerArrayStringC              
 INTEGER                                :: i

 IF ( DebugLevel > 100000 ) THEN
  WRITE(my_output_unit,'(A)') TRIM(keywordC)//": "//TRIM(specifierC)//" = "//TRIM(SpecifierValueC)
 END IF

 !------------------------------------------------------
 ! Check if specifier provided in input file is valid
 ! and get related information on data type and choice.
 !------------------------------------------------------
 CALL string_in_list(Data_len,key_char,keywords,keywordC,found_keyL,specC_in=specifierC, &
                     found_specL=found_specL, &
                     data_typeC_out=data_typeC, &
                     choiceL_out=choiceL,choiceC_out=choiceC)
   IF (.NOT. found_specL) CALL ERROR(1)                                !

   CALL position_at_key(s,keywordC,node,key_positioned)
   IF (.NOT. key_positioned) CALL ERROR(2)                             !

   IF (.NOT.ASSOCIATED(node%entries%rear)) CALL ERROR(3)               !

   spec_node => node%entries%rear                                      !
   DO 
     IF (.NOT. ASSOCIATED(spec_node%next_specifier)) EXIT
     spec_node => spec_node%next_specifier                             !
   END DO                                                              !

   IF (.NOT.ASSOCIATED(spec_node%input_spec)) CALL ERROR(4)            !


   !----------------------------------------------------------------
   ! Convert the string 'SpecifierValueC' of specifier 'specifierC'
   ! in line number 'line_number' to data type 'data_typeC'.
   !----------------------------------------------------------------
   CALL convert_string_to_value(keyword_filetypeC,data_typeC,SpecifierValueC,line_number,specifierC, &
                                xs_t,xd_t,in_t,ca_t,lo_t)

    added_value = .FALSE.                                              !
    DO                                                                 !
    IF (added_value) EXIT                                              !
    IF      (TRIM(data_typeC)==TRIM(name_xs)) THEN                      !
     DO num_xs=1,SIZE(spec_node%input_spec%spxs)                       !
     IF(TRIM(spec_node%input_spec%spxs(num_xs))==TRIM(specifierC))THEN       !
      IF(spec_node%input_spec%prxs(num_xs))THEN                        !
       CALL ERROR(5)                                                   !
      END IF                                                           !
      spec_node%input_spec%xs(num_xs)   = xs_t                         !
      spec_node%input_spec%prxs(num_xs) = .TRUE.                       !
      spec_node%input_spec%lixs(num_xs) = line_number                  !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     END DO                                                            !
    ELSE IF (TRIM(data_typeC)==TRIM(name_xd)) THEN                      !
     DO num_xd=1,SIZE(spec_node%input_spec%spxd)                       !
     IF(TRIM(spec_node%input_spec%spxd(num_xd))==TRIM(specifierC))THEN       !
      IF(spec_node%input_spec%prxd(num_xd))THEN                        !
       CALL ERROR(5)                                                   !
      END IF                                                           !
      spec_node%input_spec%xd(num_xd)   = xd_t                         !
      spec_node%input_spec%prxd(num_xd) = .TRUE.                       !
      spec_node%input_spec%lixd(num_xd) = line_number                  !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     END DO                                                            !
    ELSE IF (TRIM(data_typeC)==TRIM(name_in)) THEN                      !
     DO num_in=1,SIZE(spec_node%input_spec%spin)                       !
     IF(TRIM(spec_node%input_spec%spin(num_in))==TRIM(specifierC))THEN       !
      IF(spec_node%input_spec%prin(num_in))THEN                        !
       CALL ERROR(5)                                                   !
      END IF                                                           !
      spec_node%input_spec%in(num_in)   = in_t                         !
      spec_node%input_spec%prin(num_in) = .TRUE.                       !
      spec_node%input_spec%liin(num_in) = line_number                  !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     END DO                                                            !
    ELSE IF (TRIM(data_typeC)==TRIM(name_ca)) THEN                      !
     DO num_ca=1,SIZE(spec_node%input_spec%spca)                       !
     IF( TRIM(spec_node%input_spec%spca(num_ca)) == TRIM(specifierC) )THEN   !
      IF (spec_node%input_spec%prcaL(num_ca)) THEN                     !
       CALL ERROR(5)                                                   !
      END IF                                                           !
      spec_node%input_spec%ca(num_ca)    = TRIM(ca_t)                  !
      spec_node%input_spec%prcaL(num_ca) = .TRUE.                      !
      spec_node%input_spec%lica(num_ca)  = line_number                 !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     END DO                                                            !
    ELSE IF (TRIM(data_typeC)==TRIM(name_lo)) THEN                      !
     DO num_lo=1,SIZE(spec_node%input_spec%splo)                       !
     IF(TRIM(spec_node%input_spec%splo(num_lo))==TRIM(specifierC))THEN       !
      IF(spec_node%input_spec%prlo(num_lo))THEN                        !
       CALL ERROR(5)                                                   !
      END IF                                                           !
      spec_node%input_spec%lo(num_lo)   = lo_t                         !
      spec_node%input_spec%prlo(num_lo) = .TRUE.                       !
      spec_node%input_spec%lilo(num_lo) = line_number                  !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     END DO                                                            !
    ELSE IF (TRIM(data_typeC)==TRIM(name_inar)) THEN                   !
     local_inar => spec_node%input_spec%inar_queue%top                 !
     DO
     IF (.NOT. ASSOCIATED(local_inar)) EXIT
     IF(TRIM(local_inar%spinar)==TRIM(specifierC))THEN                 !
      local_inar%prinar = .TRUE.                                       ! (is present)
      local_inar%liinar = line_number                                  !
      CALL add_array_element(local_inar,in_t)                          !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     local_inar => local_inar%next_inar                                !
     ENDDO                                                             !
     IF (.NOT.ASSOCIATED(local_inar)) CALL ERROR(6)                    !
    ELSE IF (TRIM(data_typeC)==TRIM(name_xsar)) THEN                   !
     local_xsar => spec_node%input_spec%xsar_queue%top                 !
     DO
     IF (.NOT. ASSOCIATED(local_xsar)) EXIT
     IF(TRIM(local_xsar%spxsar)==TRIM(specifierC))THEN                       !
      local_xsar%prxsar = .TRUE.                                       !
      local_xsar%lixsar = line_number                                  !
      CALL add_array_element(local_xsar,xs_t)                             !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     local_xsar => local_xsar%next_xsar                                !
     END DO                                                            !
     IF (.NOT.ASSOCIATED(local_xsar)) CALL ERROR(6)                    !
    ELSE IF (TRIM(data_typeC)==TRIM(name_xdar)) THEN                    !
     local_xdar => spec_node%input_spec%xdar_queue%top                 !
     DO
     IF (.NOT. ASSOCIATED(local_xdar)) EXIT
     IF(TRIM(local_xdar%spxdar)==TRIM(specifierC))THEN                       !
      local_xdar%prxdar = .TRUE.                                       !
      local_xdar%lixdar = line_number                                  !
      CALL add_array_element(local_xdar,xd_t)                             !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     local_xdar => local_xdar%next_xdar                                !
     END DO                                                            !
     IF (.NOT.ASSOCIATED(local_xdar)) CALL ERROR(6)                    !
    ELSE                                                               !
     CALL ERROR(6)                                                     !
    END IF                                                             !
   END DO                                                              !

   IF (choiceL) THEN
      !-------------------------------
      ! Check if choices are defined.
      !-------------------------------
      !-----------------------------------------------------------------------------------------------
      ! So far, we verify only choices of type 'character', 'logical', 'integer' and 'integer_array'.
      !-----------------------------------------------------------------------------------------------
      choiceC_temp = TRIM(choiceC)

      !-----------------------------------------------------------------------------------------
      ! Eliminate the substrings 'CHOICE[' (begin of string) and ']' character (end of string).
      ! Example: "CHOICE[yes,no,yes[A/m^2],yes[A/cm^2]]"  (before)
      !           ==>   "yes,no,yes[A/m^2],yes[A/cm^2]"   (after)
      !-----------------------------------------------------------------------------------------
      CALL Replace_Choice_and_Delimiters(choiceC_temp)

      !-----------------------------------------------------------------------------
      ! ==> Now all allowed options are contained exactly between ChoiceSeparatorC
      !     (which could be a comma ',') on the left and right side of each option.
      !-----------------------------------------------------------------------------
    IF ( TRIM(data_typeC) == TRIM(name_ca) .OR. &  ! Check character.
         TRIM(data_typeC) == TRIM(name_lo)) THEN   ! Check logical (.TRUE./.FALSE.)
      CheckStringL = .TRUE. ! We check 'SpecifierValueC' in case it is a character or logical.

         !-----------------------------------------------------------------------------
         ! Check if substring 'SpecifierValueC' is contained in string 'choiceC_temp'.
         !-----------------------------------------------------------------------------
         !--------------------------------------------------------------------------------------------------------
         ! Here we allow the specifier value to be in lower case (mostly due to backwards compatibility),
         ! i.e. we compare the specifier value string to the list of (lower case) choices.
         ! e.g. '... = arpack' does not match 'CHOICE[ARPACK]'. Not we convert 'ARPACK' to 'arpack' (lower case).
         ! Now  '... = arpack' matches        'CHOICE[arpack]'.
         !--------------------------------------------------------------------------------------------------------
         IF ( Substring_is_contained_in_StringL( SpecifierValueC ,                  choiceC_temp ) .OR. &    ! Check exact case, e.g. 'ARPACK' is contained in 'CHOICE[ARPACK]'.
              Substring_is_contained_in_StringL( SpecifierValueC , StringLowerCase( choiceC_temp ) ) ) THEN  ! Check lower case, e.g. 'arpack' is contained in 'CHOICE[arpack]'.
          String_is_contained_in_ChoiceL = .TRUE.
         ELSE
          String_is_contained_in_ChoiceL = .FALSE.
         END IF


    ELSE IF ( TRIM(data_typeC) == TRIM(name_in) ) THEN ! Check integer.
      CheckStringL = .TRUE. ! We check 'SpecifierValueC' in case it is an integer.
         IntegerArrayStringC = int_2_char(in_t)
       ! WRITE(my_output_unit,*)  "IntegerArrayStringC = ",TRIM(IntegerArrayStringC)

         !---------------------------------------------------------------------------------
         ! Check if substring 'IntegerArrayStringC' is contained in string 'choiceC_temp'.
         !---------------------------------------------------------------------------------
         String_is_contained_in_ChoiceL = Substring_is_contained_in_StringL( IntegerArrayStringC , choiceC_temp )

    ELSE IF ( TRIM(data_typeC) == TRIM(name_inar) ) THEN ! Check integer array.
      CheckStringL = .TRUE. ! We check 'SpecifierValueC' in case it is an integer array.
       ! WRITE(my_output_unit,*)  TRIM(keywordC),": ",TRIM(SpecifierC)," = ",TRIM(SpecifierValueC),": ",in_t,"==> ",local_inar%inar
         IntegerArrayStringC = ''
         DO i = 1,SIZE(local_inar%inar)
            IntegerArrayStringC = TRIM(IntegerArrayStringC)//' '//int_2_char(local_inar%inar(i))
         END DO
         IntegerArrayStringC = TRIM( ADJUSTL(IntegerArrayStringC) )
       ! WRITE(my_output_unit,*)  "IntegerArrayStringC = ",TRIM(IntegerArrayStringC)

         !---------------------------------------------------------------------------------
         ! Check if substring 'IntegerArrayStringC' is contained in string 'choiceC_temp'.
         !---------------------------------------------------------------------------------
         String_is_contained_in_ChoiceL = Substring_is_contained_in_StringL( IntegerArrayStringC , choiceC_temp )

    END IF ! End: TRIM(data_typeC)

    IF (CheckStringL) THEN
      IF (.NOT. String_is_contained_in_ChoiceL) THEN
         WRITE(my_output_unit,'(A)')         " -----------------------------------------------------------------------------------"
         WRITE(my_output_unit,'(A)')         " Error input file: Your specified string does not match the list of allowed strings."
       IF ( TRIM(data_typeC) == TRIM(name_in)   .OR. &
            TRIM(data_typeC) == TRIM(name_inar) ) THEN
         WRITE(my_output_unit,'(A,A,A,A,A)') " You specified:  ==>   '",TRIM(specifierC)," = ",TRIM(IntegerArrayStringC),"'   <=="
       ELSE
         WRITE(my_output_unit,'(A,A,A,A,A)') " You specified:  ==>   '",TRIM(specifierC)," = ",TRIM(SpecifierValueC),"'   <=="
       END IF
         WRITE(my_output_unit,'(A)')         " List of allowed strings:"
         WRITE(my_output_unit,'(4x,A)')      TRIM(choiceC)
        IF (DebugLevel > 3) THEN
         WRITE(my_output_unit,'(1x,A)')      choiceC_temp
        END IF
         CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line_number,STOP_L=.TRUE.)
     END IF
    END IF   ! End: IF (CheckStringL)

   END IF    ! End: IF (choiceL)

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE ERROR(error_number)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE parser_parameters        ,ONLY:keyword_filetypeC, &
                                    keyword_filenameC

 IMPLICIT NONE

 INTEGER,INTENT(in) :: error_number

    IF(error_number==1)THEN                                            !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Got illegal specifier = ',TRIM(specifierC)                    !
    WRITE(my_output_unit,*)'line number           = ',line_number
    WRITE(my_output_unit,*)'Check consistency with keyword =',TRIM(keywordC)             !
    WRITE(my_output_unit,*)'For legal values see file ',TRIM(keyword_filenameC),'.'
    WRITE(my_output_unit,*)                                                         !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)"Couldn't position at keyword = ",TRIM(keywordC)              !
    WRITE(my_output_unit,*)'Stoped after call to Subroutine position_at_key.       '!
    WRITE(my_output_unit,*)                                                         !
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)"Couldn't find specifier queue associated with the      "!
    WRITE(my_output_unit,*)'keyword = ',TRIM(keywordC),' in queue of collected input'    !
    WRITE(my_output_unit,*)                                                         !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Pointer to specifier structure not associated          '!
    WRITE(my_output_unit,*)'actual keyword   = ',TRIM(keywordC)                          !
    WRITE(my_output_unit,*)'actual specifier = ',TRIM(specifierC)                         !
    WRITE(my_output_unit,*)'actual line number of ',TRIM(keyword_filetypeC),' = ',line_number
    WRITE(my_output_unit,*)                                                         !
    STOP                                                               !
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*)'>>>> WARNING <<<< >>>> WARNING <<<< >>>> WARNING <<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE value_to_queue. Multiple data for        '!
    WRITE(my_output_unit,*)'specifier = ',TRIM(specifierC),'. I take the last one.       '!
    WRITE(my_output_unit,*)'Specifier used is ',TRIM(specifierC),' = ',TRIM(SpecifierValueC)
    WRITE(my_output_unit,*)'Actual line number of ',TRIM(keyword_filetypeC),' = ',line_number
    WRITE(my_output_unit,*)'>>>> WARNING <<<< >>>> WARNING <<<< >>>> WARNING <<<<<<'!
    WRITE(my_output_unit,*)                                                         !
   ELSE IF(error_number==6)THEN                                        !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)"In SUBROUTINE value_to_queue. Couldn't find specifier  "!
    WRITE(my_output_unit,*)'in queue of collected input. This should not happen -   '!
    WRITE(my_output_unit,*)"I have no idea what's going on."
    WRITE(my_output_unit,*)'keyword = ',TRIM(keywordC)                                   !
    WRITE(my_output_unit,*)'specifier = ',TRIM(specifierC)                                !
    WRITE(my_output_unit,*)'Actual line number of ',TRIM(keyword_filetypeC),' = ',line_number
    WRITE(my_output_unit,*)                                                         !
    STOP
   ELSE IF(error_number>6 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 7.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    WRITE(my_output_unit,*)                                                         !
    STOP
   END IF

!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE value_to_queue
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_value_to_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE common_queues
!------------------------------------------------------------------------------
 USE keyword_queue_def   ,ONLY:keyword_queue
 USE input_key_queue_def!,ONLY:input_key_queue

 IMPLICIT NONE

 TYPE(input_key_queue)        :: collected_input
 TYPE(keyword_queue),POINTER  :: keywords

!------------------------------------------------------------------------------
 END MODULE common_queues
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE common_nodes
!------------------------------------------------------------------------------
 USE input_type_names,ONLY:char_length_keyword_name
 USE common_queues   ,ONLY:input_key_node, inp_spec_node              ! Module containing queues and type definitions of queues

 IMPLICIT NONE

 TYPE(input_key_node), POINTER, SAVE     :: a1                        ! local pointer to keyword node of collected input queue
 TYPE(inp_spec_node) , POINTER, SAVE     :: b1                        ! local pointer to specifier structure in queue associated to keyword
 LOGICAL                      , SAVE     :: first_entry = .TRUE.      ! check first entry to ensure pointer status
 CHARACTER(len=char_length_keyword_name) :: last_keyC = ''            !

!------------------------------------------------------------------------------
 END MODULE common_nodes
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_check_presence
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE CheckPresenceOfRequiredInputSpecifiers
!------------------------------------------------------------------------------
 USE input_type_names,ONLY:char_length_keyword_name, &
                           char_length_specifier_name
 USE common_queues ! ,ONLY:collected_input                           ! Module containing queues and type definitions of queues

 IMPLICIT NONE

 TYPE(input_key_node), POINTER  :: a1                                ! local pointer to keyword node of collected input queue
 TYPE(inp_spec_node) , POINTER  :: b1                                ! local pointer to specifier structure in queue associated to keyword
 TYPE(in_array_node) , POINTER  :: c1                                !
 TYPE(xs_array_node) , POINTER  :: c2                                !
 TYPE(xd_array_node) , POINTER  :: c3                                !
 CHARACTER(char_length_keyword_name)   :: keyword                    !
 CHARACTER(char_length_specifier_name) :: specifier                  !
 LOGICAL                        :: everything_fine                   !
 INTEGER                        :: i,j
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
                                                                       !
     everything_fine = .TRUE.                                          !
     a1 => collected_input%top                                         !

     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      b1 => a1%entries%top                                             !

!---------------------------------------------
! Print list of keywords found in input file.
!---------------------------------------------
!         keyword   = ''                                                !
!         DO j=1,a1%length                                              !
!          keyword(j:j) = a1%keyword(j)                                 !
!         END DO                                                        !
! PRINT *,TRIM(keyword)

      DO
       IF (.NOT. ASSOCIATED(b1)) EXIT
       DO i=1,SIZE(b1%input_spec%spin)                                 !
        IF( (.NOT. b1%input_spec%opin(i)) .AND. &
            (.NOT. b1%input_spec%prin(i))       ) THEN
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = b1%input_spec%spin(i)                            !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       END DO                                                          !
       DO i=1,SIZE(b1%input_spec%spxs)                                 !
        IF( (.NOT. b1%input_spec%opxs(i)) .AND. &
            (.NOT. b1%input_spec%prxs(i))       ) THEN
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = b1%input_spec%spxs(i)                            !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       END DO                                                          !
       DO i=1,SIZE(b1%input_spec%spxd)                                 !
        IF(( .NOT. b1%input_spec%opxd(i)) .AND. &
           ( .NOT. b1%input_spec%prxd(i))       ) THEN
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = b1%input_spec%spxd(i)                            !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       END DO                                                          !
       DO i=1,SIZE(b1%input_spec%spca)                                 !
        IF((.NOT.b1%input_spec%opca(i)) .AND.                         &!
           (.NOT.b1%input_spec%prcaL(i))       ) THEN                  !
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = b1%input_spec%spca(i)                            !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       END DO                                                          !
       DO i=1,SIZE(b1%input_spec%splo)                                 !
        IF((.NOT.b1%input_spec%oplo(i)) .AND.                         &!
           (.NOT.b1%input_spec%prlo(i))       ) THEN                   !
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = b1%input_spec%splo(i)                            !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       END DO                                                          !
       IF (ASSOCIATED(b1%input_spec%inar_queue%top)) THEN              !
       c1 => b1%input_spec%inar_queue%top                              !
       DO
        IF (.NOT. ASSOCIATED(c1)) EXIT
        IF((.NOT.c1%opinar) .AND. (.NOT.c1%prinar)) THEN               !
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = c1%spinar                                        !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       c1 => c1%next_inar                                              !
       END DO                                                          !
       END IF                                                          !
       IF (ASSOCIATED(b1%input_spec%xsar_queue%top)) THEN              !
       c2 => b1%input_spec%xsar_queue%top                              !
       DO
        IF (.NOT. ASSOCIATED(c2)) EXIT
        IF((.NOT.c2%opxsar) .AND. (.NOT.c2%prxsar)) THEN               !
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = c2%spxsar                                        !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       c2 => c2%next_xsar                                              !
       END DO                                                          !
       END IF                                                          !
       IF (ASSOCIATED(b1%input_spec%xdar_queue%top)) THEN              !
       c3 => b1%input_spec%xdar_queue%top                              !
       DO
        IF (.NOT. ASSOCIATED(c3)) EXIT
        IF((.NOT.c3%opxdar) .AND. (.NOT.c3%prxdar)) THEN               !
         ! Error !
         keyword   = ''                                                !
         specifier = ''                                                !
         DO j=1,a1%length                                              !
          keyword(j:j) = a1%keyword(j)                                 !
         END DO                                                        !
          specifier = c3%spxdar                                        !
          everything_fine = .FALSE.                                    !
         CALL ERROR                                                    !
        END IF                                                         !
       c3 => c3%next_xdar                                              !
       END DO                                                          !
       END IF                                                          !
      b1 => b1%next_specifier                                          !
      END DO                                                           !
     a1 => a1%next_key                                                 !
     END DO                                                            !
     IF(.NOT.everything_fine) STOP                                     !

 CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE ERROR
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)">>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<<"!
    WRITE(my_output_unit,*)"Couldn't find required input for:"                      !
    WRITE(my_output_unit,*)"Keyword   = " ,TRIM(keyword)                            !
    WRITE(my_output_unit,*)"Specifier =  ",TRIM(specifier)                          !
    WRITE(my_output_unit,*)"Input for this specifier was defined to be not optional."!
    WRITE(my_output_unit,*)">>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<<"!
!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  END SUBROUTINE CheckPresenceOfRequiredInputSpecifiers
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_check_presence
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_read_and_analyze_input
!------------------------------------------------------------------------------
!
!++m* builder_inputfile.f90/mod_read_and_analyze_input
!
! NAME 
!   MODULE mod_read_and_analyze_input
!
! CONTAINS
!   o SUBROUTINE read_and_analyze_input
!
! FILENAME
!   builder_inputfile.f90
!
!##
!
!------------------------------------------------------------------------------

IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE read_and_analyze_input(InputFilenameC_in,FileNamePresentL,SecondEntryL, InputFilename_usedC)
!------------------------------------------------------------------------------
!
!++s* mod_read_and_analyze_input/read_and_analyze_input
!
! NAME
!   SUBROUTINE read_and_analyze_input
!
! PURPOSE
!   Build up input queue.
!
! USAGE
!   CALL read_and_analyze_input(InputFilenameC_in,FileNamePresentL,SecondEntryL, InputFilename_usedC)
!
! INPUT
!   o InputFilenameC_in:      string could be undefined
!   o FileNamePresentL:       
!   o SecondEntryL:           The SecondEntry flag is needed for the input parser
!                             if the input file is read in several times.
!     SecondEntryL = .FALSE.: If the input file is read in for the first time.
!     SecondEntryL = .TRUE. : If the input file is read in later again, this flag must be set to .TRUE.
!
! OUTPUT
!   o InputFilename_usedC:    filename that was actually used
!
! CONTAINS
!   o SUBROUTINE Add_value_to_queue
!   o SUBROUTINE ERROR
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:DebugLevel, &
                                    ParseInputFileOnlyL, &
                                    ParseKeywordsInputFileL
 USE mod_SyntaxFolder         ,ONLY:GetFilenameIncludingSyntaxFolder
 USE mod_InputFileName        ,ONLY:GetInputFileNameFromValidatorFile
 USE parser_parameters,ONLY:key_char, &
                      end_key_char, &
                         spec_char, &
                     end_spec_char, &
                       comment_signsCV, &
                       keyword_filenameC           , &
                       keyword_filetypeC           , &
                       SecondEntry_InputBuiltUpL, &
                       Data_len_very_long, &
                       Data_len_long, &
                       Data_len
 USE input_type_names       ,ONLY:required_key  ,not_required_key, &
                                  required_input,not_required_input
 USE mod_keyword_queue_built_up,ONLY:keyword_queue_built_up
 USE keyword_node_def       ,ONLY:keyword_node
 USE queue_type_def         ,ONLY:queue_type
 USE node_type_def          ,ONLY:node_type
 USE mod_string_in_list     ,ONLY:string_in_list
 USE mod_queue              ,ONLY:queue_built_up
 USE mod_get_line           ,ONLY:get_next_line, &
                                  get_prev_line
 USE input_built_up_interface,ONLY:input_built_up
 USE mod_key_positions      ,ONLY:key_positions, &
                                  spec_positions
 USE add_inp_spec_interface,ONLY:add_inp_spec
 USE mod_Get_Separation_Specifier,ONLY:Get_Separation_Specifier
 USE common_queues     ,ONLY:keywords,collected_input,input_key_node                ! Module containing queues and type definitions of queues
 USE mod_get_keyword   ,ONLY:get_keyword
 USE mod_check_presence,ONLY:CheckPresenceOfRequiredInputSpecifiers
 USE mod_Print_Keywords_Queue,ONLY:Print_Keywords
 USE mod_syntax_validator    ,ONLY:InputSyntax

 IMPLICIT NONE

 CHARACTER(len=*)    ,INTENT(in) :: InputFilenameC_in
 LOGICAL             ,INTENT(in) :: FileNamePresentL
 LOGICAL             ,INTENT(in) :: SecondEntryL
 CHARACTER(len=*)   ,INTENT(out) :: InputFilename_usedC

 LOGICAL                         :: Print_Keywords_XML_File_DebugLevelL
 CHARACTER(len=Data_len_long)    :: input_filenameC                  !
 CHARACTER(len=:),ALLOCATABLE :: input_filename_allocatableC
 INTEGER,DIMENSION(:),POINTER    ::     key_pos                      !
 INTEGER,DIMENSION(:),POINTER    :: end_key_pos                      !
 INTEGER                         :: NumberOfKeywordsInLine

 INTEGER,DIMENSION(:),POINTER    ::     spec_pos                     !
 INTEGER,DIMENSION(:),POINTER    :: end_spec_pos                     !
 INTEGER                         :: NumOfSpecifiersInLine            !

 INTEGER                         :: start_position                   !
 INTEGER                         :: stop_position                    !

 LOGICAL                         :: found_keyL                       !
 LOGICAL                         :: found_specL                      !
 CHARACTER(Data_len)             :: keyC                             !
 CHARACTER(Data_len)             :: spec                             !
 CHARACTER(Data_len_very_long)   :: bufferC                          ! allow for long lines
 CHARACTER(Data_len_very_long)   :: buffer_aC                        ! allow for long lines
 CHARACTER(Data_len_very_long)   :: buffer_bC                        ! allow for long lines
 CHARACTER(len=:),ALLOCATABLE    :: tempC                            !

 TYPE(node_type) ,POINTER        :: act_node                         !
 TYPE(queue_type)                :: input_queue                      !
 INTEGER                         :: line_number                      !
 LOGICAL                         :: start_at_topL,found_endL         !
 LOGICAL                         :: found_enda,found_endb            !
 LOGICAL                         :: start_at_endL,found_topL         !
 LOGICAL                         :: start_reading
 LOGICAL                         :: read_restL                       !

 LOGICAL                         :: keys_are_ok                      !

 CHARACTER(Data_len)             :: first_spec                       !
 CHARACTER(Data_len)             :: last_spec                        !
 CHARACTER(Data_len)             :: sep_spec                         !
 LOGICAL                         :: got_first_spec                   !
 LOGICAL                         :: new_keyword                      !
 LOGICAL                         :: process_line                     !

 TYPE(keyword_node)  ,POINTER    :: a1                               !
 TYPE(input_key_node),POINTER    :: a2                               ! local pointer to keyword node of collected input queue
 INTEGER                         :: icount,i,j,ioff,k,ii,ii_stop,iii

 CHARACTER(len=*),PARAMETER      :: KeywordFileTypeC = 'inputfile'

   bufferC = '' ! has to be initialized because it is an allocatable object

   IF (SecondEntryL) THEN
       SecondEntry_InputBuiltUpL = .TRUE.
   ELSE
       SecondEntry_InputBuiltUpL = .FALSE.
   END IF
    
    !---------------------------------------
    ! Get name of input file to be read in.
    !---------------------------------------
    IF (FileNamePresentL) THEN
        !----------------------------------------
        ! This is typically the case if the flag
        !   '--inputfile <inputfilename>'
        ! is used.
        !----------------------------------------
        input_filenameC = TRIM(InputFilenameC_in)
    ELSE
      !-------------------------------------------------------------------------
      ! Get input file name from 'keyword_filenameC'.
      ! The name of the input file is specified in 'keywords.val'.
      !-------------------------------------------------------------------------
      IF (ParseKeywordsInputFileL) THEN ! <== Use keywords validator file, i.e. the file
                                        !     keywords.val
                                        !     is read in.
       !------------------------------------------
       ! Get input file name from validator file.
       !------------------------------------------
       WRITE(my_output_unit,'(A)') ""
       WRITE(my_output_unit,'(A)') " Reading in syntax validator file: "//TRIM(keyword_filenameC)
       CALL GetInputFileNameFromValidatorFile(GetFilenameIncludingSyntaxFolder('',keyword_filenameC), &
                                              comment_signsCV,key_char,spec_char, &
                                              input_filenameC)
       ! input_filenameC contains the filename (possibly including a relative or absolute path)
      ELSE
       !----------------------------------------------------------------------------
       ! Get input file name from source code,
       ! i.e. the content of keywords.val is contained in source code.
       !----------------------------------------------------------------------------
       input_filename_allocatableC = ''
       CALL InputSyntax(KeywordFileTypeC,.FALSE.,'default-filename', input_filename_allocatableC)
       input_filenameC = input_filename_allocatableC
      END IF
    END IF

    InputFilename_usedC = TRIM(input_filenameC)

    WRITE(my_output_unit,'(A)')         ""
    WRITE(my_output_unit,'(A)')         " ------------------------------------------------------------------------------"
    WRITE(my_output_unit,'(A,A,A,A,A)') " Reading in ",TRIM(keyword_filetypeC),": '", &
                                                       TRIM(InputFilename_usedC),"'"

    !-----------------------------
    ! ==> 1: queue for input file
    !-----------------------------
    CALL queue_built_up(InputFilename_usedC, input_queue) ! Essentially stores lines and associated information.

    !----------------------------------------------------------------------
    ! Initialize variable in order to avoid that it is used uninitialized.
    !----------------------------------------------------------------------
    last_spec = '(default initialization of this specifier with arbitrary value)'

    IF (SecondEntryL) THEN
     !------------------------------------------------------------------
     ! As the keywords file does not change, it is not necessary to set
     ! up the queue for the keywords file again if the input file is
     ! read in later again.
     !------------------------------------------------------------------
    ELSE
     !--------------------------------------------------------------
     ! If the input file is read in for the first time.
     ! we have to store a list of possible keywords and specifiers.
     !--------------------------------------------------------------
     ALLOCATE(keywords)
     !--------------------------------
     ! ==> 2: queue for keywords file
     !        ==> We use 'Data_len_long' because the line can be very long if lots of options for CHOICE are specified.
     !--------------------------------
     CALL keyword_queue_built_up(KeywordFileTypeC,keyword_filenameC,ParseKeywordsInputFileL, &
                                 Data_len_long, &
                                 key_char,comment_signsCV,required_key,not_required_key, &
                                 required_input,not_required_input, keywords)
   ! WRITE(my_output_unit,*) TRIM(InputFilename_usedC),": queue for keywords and specifiers file built up."

     IF (ParseInputFileOnlyL) THEN
         Print_Keywords_XML_File_DebugLevelL = .TRUE.
     ELSE
         Print_Keywords_XML_File_DebugLevelL = .FALSE.
     END IF
     IF (Print_Keywords_XML_File_DebugLevelL) THEN
      !-----------------------------------------
      ! Print queue of keywords and specifiers.
      !-----------------------------------------
      CALL Print_Keywords(KeywordFileTypeC,keyword_filenameC,keywords)
     END IF

    END IF

    start_at_topL = .TRUE.                                             !
    found_endL    = .FALSE.                                            !
    icount       = 0                                                   !
    keys_are_ok  = .TRUE.                                              !

    !---------------------------------------------------------------------------------------
    ! It has to be nullified because SUBROUTINE key_positions checks its associated status.
    !---------------------------------------------------------------------------------------
    NULLIFY(key_pos)
    NULLIFY(end_key_pos)

    DO                                                                 !
     CALL get_next_line (input_queue,bufferC,line_number,start_at_topL, &! Call queue for next line in input file
                         found_endL,act_node)                           !
     IF (found_endL) EXIT                                               ! quit after reading last line in input queue
     CALL key_positions(key_char,end_key_char,bufferC,key_pos,end_key_pos,NumberOfKeywordsInLine)  ! find keyword positions in input line. key_pos(i) gives the starting position
                                                                       ! which is determined by the separating character key_char.
     DO i=1,NumberOfKeywordsInLine                                     ! end_key_pos(i) is last character position of keyword
      icount        = icount + 1                                       !
      keyC = ""                                                         !
      DO j=key_pos(i),end_key_pos(i)                                   !
       keyC(j:j) = bufferC(j:j)                                          !
      END DO                                                           !
      IF (key_pos(i) > 1) THEN                                         !
       IF (bufferC(key_pos(i)-1:key_pos(i)-1) /= " ") CALL ERROR(1)     !
      END IF                                                           !
      !----------------------------------------------------------------------------
      ! Check if keyword provided in input file (or database input file) is valid.
      !----------------------------------------------------------------------------
      CALL string_in_list(Data_len,key_char,keywords,keyC,found_keyL)
      IF (.NOT. found_keyL)  CALL ERROR(1)                              !
      IF (.NOT. found_keyL)  keys_are_ok = found_keyL                   !
                                                                       !
      IF (mod(icount,2)==1) tempC = key_char//"end_"//keyC(2:LEN_TRIM(keyC))!
      IF (mod(icount,2)==0) THEN                                       !
       IF(TRIM(tempC) /= TRIM(keyC)) CALL ERROR(8)                     ! check matching of end keyword
      END IF                                                           !
                                                                       !
      IF(mod(icount,2)==1) CALL input_built_up(collected_input,keyC)    ! add new node to queue collected input if new keyword in input
     END DO                                                            !
    END DO                                                             !

    IF (icount == 0)       CALL ERROR(2)                               ! check: is at least one keyword in input file
    IF (.NOT. keys_are_ok) CALL ERROR(3)                               ! stop if invalid keywords are in input file
    icount = MOD(icount,2)                                             !
    IF (icount /= 0)       CALL ERROR(4)                               ! check that number of keywords in input file is even (keyC + end_keyC present)
                                                                       !
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
    start_at_topL = .TRUE.                                              !
    start_at_endL = .FALSE.                                             !
    found_topL    = .FALSE.                                             !
    found_endL    = .FALSE.                                             !
    found_enda   = .FALSE.                                             !
    found_endb   = .FALSE.                                             !
    icount       = 0                                                   !

  ! IF (DebugLevel > 3) &
  ! WRITE(my_output_unit,'(1x,A,A,A,A)') &
  !      TRIM(InputFilename_usedC),": Build up input queue for all ",TRIM(keyword_filetypeC)," entries."

    DO                                                                 !
     bufferC  = " "                                                     !
     buffer_aC = " "                                                     !
     buffer_bC = " "                                                     !
        CALL get_next_line (input_queue,bufferC ,line_number,start_at_topL, found_endL,act_node) ! Get input line.
     IF  (.NOT.found_enda) THEN                                        !
        CALL get_next_line (input_queue,buffer_aC,line_number,start_at_topL, found_enda,act_node) ! Get input line 'a'.
      IF (.NOT.found_endb) THEN                                        !
        CALL get_next_line (input_queue,buffer_bC,line_number,start_at_topL, found_endb,act_node) ! Get input line 'b'.
      END IF                                                           !
     END IF                                                            !
                                                                       !
     IF (.NOT.found_endb) THEN                                         !
        CALL get_prev_line (input_queue,buffer_aC,line_number,start_at_endL, found_topL,act_node)
     END IF                                                            !
     IF (.NOT.found_enda) THEN                                         !
        CALL get_prev_line (input_queue,bufferC ,line_number,start_at_endL,  found_topL,act_node)
     END IF                                                            !
     IF (found_endL) EXIT                                               ! quit after reading last line in input queue
                                                                       !
     CALL key_positions(key_char,end_key_char,bufferC,key_pos,end_key_pos,NumberOfKeywordsInLine)            ! determine position of keywords in actual line of input
                                                                       !
!----------------------------------------------------------------------!
     tempC = " "                                                       !
     DO i=0,NumberOfKeywordsInLine                                     ! starts at zero to handle lines without keywords in same loop
      start_reading = .FALSE.                                          !
      new_keyword   = .FALSE.                                          !
      IF (i+NumberOfKeywordsInLine == 0) THEN                          ! no keywords in input line
       start_reading = .TRUE.                                          !
       start_position= 1                                               !
      ELSE IF (i > 0) THEN                                             ! At least one keyword in input line.
       icount        = icount + 1                                      ! Global counting of keywords.
       IF (mod(icount,2) == 1) THEN                                    ! Is it a starting keyword ?
         keyC = ""                                                     !
         ioff= -key_pos(i)+1                                           !
         DO j=key_pos(i),end_key_pos(i)                                !
          keyC(j+ioff:j+ioff) = bufferC(j:j)                           !
         END DO                                                        !
         start_reading = .TRUE.                                        !
         start_position= end_key_pos(i)+1                              !
         new_keyword   = .TRUE.                                        !
       ELSE IF ((mod(icount,2)==0).AND.(i==1)) THEN                    ! special handling, input in front of first keyword which is an end_keyword 
         IF (key_pos(i) > 1) THEN                                      !
          start_reading  = .TRUE.                                      !
          start_position = 1                                           !
         END IF                                                        !
       ELSE IF (mod(icount,2)==0) THEN                                 ! it is an end_keyword
          start_reading  = .FALSE.                                     !
       END IF                                                          !
      END IF                                                           !
                                                                       !
      process_line = .FALSE.                                           ! to filter out loops with i=0 if keywords are present in input line
      IF ((i > 0) .OR. (NumberOfKeywordsInLine == 0)) process_line = .TRUE.           ! to filter out loops with i=0 if keywords are present in input line
      IF (process_line) THEN                                           ! to filter out loops with i=0 if keywords are present in input line
       IF ((start_reading).AND.(NumberOfKeywordsInLine > 0)) THEN                     ! fill up bufferC with blanks up to valid input after starting keyword
        DO k=1,start_position-1                                        !
         bufferC(k:k) = " "                                             !
        END DO                                                         !
       END IF                                                          !
      END IF                                                           !
                                                                       !
!----------------------------------------------------------------------!
        got_first_spec = .FALSE.                                       !

         !----------------------------------------------------------------------------------------
         ! It has to be nullified because SUBROUTINE spec_positions checks its associated status.
         !----------------------------------------------------------------------------------------
         NULLIFY(spec_pos)
         NULLIFY(end_spec_pos)

         !---------------------------------------------------
         ! Look for specifiers in 'bufferC' and 'buffer_aC'.
         !---------------------------------------------------
         CALL spec_positions(spec_char,end_spec_char,bufferC , &
                             buffer_aC,spec_pos,end_spec_pos,NumOfSpecifiersInLine)
      IF (new_keyword) THEN                                            !
         !---------------------------------------------------
         ! Look for specifiers in 'bufferC' and 'buffer_aC'.
         !---------------------------------------------------
         CALL spec_positions(spec_char,end_spec_char,bufferC , &
                             buffer_aC,spec_pos,end_spec_pos,NumOfSpecifiersInLine)
       IF (NumOfSpecifiersInLine == 0) THEN                            !
         !-----------------------------------------------------
         ! Look for specifiers in 'buffer_aC' and 'buffer_bC'.
         !-----------------------------------------------------
         CALL spec_positions(spec_char,end_spec_char,buffer_aC, &
                             buffer_bC,spec_pos,end_spec_pos,NumOfSpecifiersInLine)  ! beep next line
         IF (NumOfSpecifiersInLine == 0)    CALL ERROR(10)             !
         IF (spec_pos(1) /= 1) CALL ERROR(11)                          !
         first_spec = " "                                              !
         DO ii=spec_pos(1),end_spec_pos(1)                             !
         first_spec(ii:ii) = buffer_aC(ii:ii)                          !
         END DO                                                        !
         ii = end_spec_pos(1)                                          !
         IF (first_spec(ii:ii)==end_spec_char)  first_spec(ii:ii) =' ' !
         first_spec = TRIM(ADJUSTL(first_spec))                        !
         !-----------------------------------------------------
         ! Check if specifier provided in input file is valid.
         !-----------------------------------------------------
         CALL string_in_list(Data_len,key_char,keywords,keyC,found_keyL,first_spec, found_specL)
       IF(.NOT.found_keyL)  CALL ERROR(1)                               !
       IF(.NOT.found_keyL)  CALL ERROR(3)                               !
         IF (.NOT.found_specL) CALL ERROR(5)                            !
         got_first_spec = .TRUE.                                       !
       ELSE                                                            !
        first_spec = ' '                                               !
        DO ii=start_position,end_spec_pos(1)                           !
        first_spec(ii:ii) = bufferC(ii:ii)                              !
        END DO                                                         !
        ii = end_spec_pos(1)                                           !
        IF (first_spec(ii:ii)==end_spec_char)  first_spec(ii:ii) =' '  !
        first_spec = TRIM(ADJUSTL(first_spec))                         !
        !-----------------------------------------------------
        ! Check if specifier provided in input file is valid.
        !-----------------------------------------------------
        CALL string_in_list(Data_len,key_char,keywords,keyC,found_keyL,first_spec, found_specL)
       IF(.NOT.found_keyL)    CALL ERROR(1)                             !
       IF(.NOT.found_keyL)    CALL ERROR(3)                             !
       IF(.NOT.found_specL)   CALL ERROR(5)                             !
        got_first_spec = .TRUE.                                        !
                                                                       !
       END IF                                                          !
        IF(got_first_spec) THEN                                        !
         CALL Get_Separation_Specifier(keywords,keyC, sep_spec)
         IF (TRIM(sep_spec)/=TRIM(first_spec)) CALL ERROR(9)           !
        END IF                                                         !
                                                                       !
      END IF                                                           !
!----------------------------------------------------------------------!
                                                                       !
       IF (start_reading) THEN                                         !
        stop_position = SCAN(bufferC,key_char)-1                        !
        IF (stop_position <= 0) stop_position = LEN_TRIM(bufferC)       !
        read_restL = .FALSE.                                           !
        IF (NumOfSpecifiersInLine > 0) THEN                            !
         IF (spec_pos(1) > 0) THEN                                     !
          IF (spec_pos(1) > stop_position) THEN                        !
           read_restL = .TRUE.                                         !
          ELSE IF ((spec_pos(1) > 1).AND.(start_position==1)) THEN     !
           tempC = ""                                                  !
           tempC = bufferC(1:spec_pos(1)-1)                            !
           IF (LEN_TRIM(tempC) > 0) THEN                               !
            read_restL = .TRUE.                                        !
            stop_position = spec_pos(1)-2                              !
           END IF                                                      !
           tempC = ""                                                  !
          END IF                                                       !
         END IF                                                        !
        END IF                                                         !
        IF ((NumOfSpecifiersInLine == 0).OR. read_restL) THEN          !
           ii      = start_position                                    !
           ii_stop =  stop_position                                    !
           !oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
           DO                                                          !
            IF (ii > ii_stop) EXIT                                     !
            DO 
             IF (bufferC(ii:ii) /= spec_char) EXIT                     ! Skip blanks.
             ii = ii + 1                                               !
            END DO                                                     !

            !---------------------
            ! Add value to queue.
            !---------------------
            CALL Add_value_to_queue(bufferC, ii) ! 'ii' is input and output.
           
           END DO                                                      !
           !oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
        END IF                                                         !
                                                                       !
        iii = 0                                                        !
        stop_position = SCAN(bufferC,key_char)-1                        !
        IF (stop_position < 0) stop_position = LEN_TRIM(bufferC)        !
        DO 
         IF (NumOfSpecifiersInLine <= 0) EXIT
        iii = iii + 1                                                  !
        IF ((iii > NumOfSpecifiersInLine)) EXIT                                     !
         IF (end_spec_pos(iii) <= LEN_TRIM(bufferC)) THEN               !
           IF (spec_pos(iii) > 0) THEN                                 !
            spec = ' '                                                 !
           DO ii=spec_pos(iii),end_spec_pos(iii)                       !
            spec(ii:ii) = bufferC(ii:ii)                                !
           END DO                                                      !
            ii = end_spec_pos(iii)                                     !
           IF (spec(ii:ii)==end_spec_char)  spec(ii:ii) =' '           !
           spec = TRIM(ADJUSTL(spec))                                  !
           last_spec = spec                                            !
           !-----------------------------------------------------
           ! Check if specifier provided in input file is valid.
           !-----------------------------------------------------
           CALL string_in_list(Data_len,key_char,keywords,keyC,found_keyL,spec, found_specL)
       IF(.NOT.found_keyL)  CALL ERROR(1)                               !
       IF(.NOT.found_keyL)  CALL ERROR(3)                               !
       IF(.NOT.found_specL) CALL ERROR(5)                               !
           IF((TRIM(spec) == TRIM(first_spec)).AND.                   &
              (TRIM(spec) == TRIM(sep_spec  ))         ) THEN           !
           tempC = ""                                                   !
           tempC = spec                                                 !
           !----------------------------------------------------------------------------------------------------------
           ! Add input specifier node and transfer information of keywords.val to queue collected_input
           !----------------------------------------------------------------------------------------------------------
           CALL add_inp_spec (collected_input,keywords,keyC,spec)       !
           spec = tempC                                                 !
           tempC = ""                                                   !
           END IF                                                      !
           END IF                                                      !
          IF(end_spec_pos(iii)==LEN_TRIM(bufferC)) THEN                 !
           CONTINUE                                                    !
          ELSE                                                         !
           ii_stop = LEN_TRIM(bufferC)                                  !
           IF (iii+1 <= NumOfSpecifiersInLine) ii_stop = spec_pos(iii+1)-1          !
           ii_stop = MIN(ii_stop,stop_position)                        !
            ii = end_spec_pos(iii)+1                                   !
           !oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
           DO                                                          !
            IF (ii > ii_stop) EXIT                                     !
            DO
             IF (bufferC(ii:ii) /= spec_char) EXIT                     ! Skip blanks.
             ii = ii + 1                                               !
            END DO                                                     !
            IF (ii > ii_stop) EXIT                                     !

            !---------------------
            ! Add value to queue.
            !---------------------
            CALL Add_value_to_queue(bufferC, ii) ! 'ii' is input and output.

           END DO
           !oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
          END IF                                                       !
         END IF                                                        !
        END DO                                                         !
       END IF                                                          !
!----------------------------------------------------------------------!
                                                                       !
     END DO                                                            ! end loop over keywords
                                                                       !
    END DO                                                             ! end loop over input lines

  ! WRITE(my_output_unit,'(1x,A,A,A,A)') &
  !     TRIM(InputFilename_usedC),": Build up input queue for all ",TRIM(keyword_filetypeC)," entries. (finished)"

!----------------------------------------------------------------------!
  CALL CheckPresenceOfRequiredInputSpecifiers                          ! Check presence of required input specifiers.

  found_endL = .FALSE. ; start_at_topL = .TRUE. ; icount = 0             ! start to check presence of required input keywords
 DO 
  IF (found_endL) EXIT
  icount = icount + 1                                                  !
  CALL get_keyword(keywords,bufferC,start_at_topL,found_endL,a1)          ! a1 is a local pointer to node in keyword queue
   start_at_topL = .FALSE.                                              !
    IF ((a1%required).AND.(MOD(icount,2)==1)) THEN                     ! scan input for required keyword. Only for starting keywords
     a2 => collected_input%top                                         ! local pointer to queue with collected input
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a2)) EXIT
      IF(SIZE(a2%keyword)==SIZE(a1%keyword))THEN                       !
       found_keyL = .TRUE.                                              !
       DO i=1,SIZE(a2%keyword)                                         !
        IF(a2%keyword(i) /= a1%keyword(i)) found_keyL = .FALSE.         !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) EXIT                                              !
      a2 => a2%next_key                                                !
     END DO                                                            !
      IF (.NOT.found_keyL) CALL ERROR(12)                               !Call error routine if required keyword isn't found
    END IF                                                             !
  END DO                                                               !

  NULLIFY(a2)                                                          !

! WRITE(my_output_unit,*) TRIM(InputFilename_usedC),": read_and_analyze_input finished."

!**********************************************************************!
!----------------------------------------------------------------------!
CONTAINS                                                               !

!------------------------------------------------------------------------------
 SUBROUTINE Add_value_to_queue(bufferC, ii)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE CharacterManipulation    ,ONLY:CharacterReplace
 USE parser_parameters        ,ONLY:spec_char, &
                                    SpecialMacroCharacterC
 USE input_type_names         ,ONLY:QuotationMarkC, &
                                    ApostropheC
 USE mod_value_to_queue       ,ONLY:value_to_queue

 IMPLICIT NONE

 CHARACTER(len=*)                 ,INTENT(in)    :: bufferC 
 INTEGER                          ,INTENT(inout) :: ii
 
 CHARACTER(len=LEN_TRIM(bufferC))                :: SpecifierValueC
 CHARACTER(len=LEN_TRIM(SpecialMacroCharacterC)) :: SpecifierFirstCharacterC
 LOGICAL                                         ::  FirstQuotationMarkL
 LOGICAL                                         :: SecondQuotationMarkL
 LOGICAL                                         ::  FirstApostropheL
 LOGICAL                                         :: SecondApostropheL

 INTEGER                                         :: ii_beginning

 SpecifierValueC = " "

 !---------------------------------------------------------
 ! Initialize control flags for quotation mark/apostrophe.
 !---------------------------------------------------------
  FirstQuotationMarkL = .FALSE.
 SecondQuotationMarkL = .FALSE.
  FirstApostropheL    = .FALSE.
 SecondApostropheL    = .FALSE.

!----------------------------------------------------------------------------------------------------------
            ! 'bufferC' contains the whole line, e.g.:
            ! source-directory    =   "N:\users\nextnano\nextnano3\input files\tutorials\1D strained silicon\strain1\"
            ! Note: This line contains a blank in the folder name. This is not expected because a 'blank' is intended
            !       to separate specifier entries.
            ! Solution: We check if the first character of the specifier entry is a quotation mark '"'
            !           and look for the second quotation mark '"'.
            !----------------------------------------------------------------------------------------------------------
          ! WRITE(my_output_unit,*) "bufferC =",TRIM(bufferC)

            !---------------------------------------------------------------------------------------------------
            ! 'ii_beginning' contains the position of the first character of the specifier entry to be read in.
            !---------------------------------------------------------------------------------------------------
            ii_beginning = ii

            IF      ( bufferC(ii_beginning:ii_beginning) == QuotationMarkC ) THEN  ! QuotationMarkC = "
                 FirstQuotationMarkL = .TRUE.
            ELSE IF ( bufferC(ii_beginning:ii_beginning) == ApostropheC    ) THEN  ! ApostropheC    = '
                 FirstApostropheL    = .TRUE.
            END IF

            !++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            DO
             !------------------------------
             ! We increase ii in each loop.
             !------------------------------

             !---------------------------------------------------------
             ! If quotation marks are present, we do not exit yet.
             ! Instead we read until the second quotation mark occurs.
             ! Finally, we replace the quotation marks.
             !---------------------------------------------------------

             !-------------------------------------------------------------------------------
             ! We exit this do loop if we encounter either of the following (in this order):
             !   1) the end of the string
             !   2) second quotation mark  ==> "
             !   3) second apostrophe      ==> '
             !   4) the specifier 'blank'  ==> ' '
             !-------------------------------------------------------------------------------

              IF (         ii     >  LEN_TRIM(bufferC) ) THEN
               EXIT

              !------------------------------------------------------------------------------------
              ! Now we check if the first character of the specifier entry is a quotation mark '"'
              ! or apostrophe "'".
              ! We read until we have found the second quotation mark or apostrophe.
              !------------------------------------------------------------------------------------

              ELSE IF ( FirstQuotationMarkL .AND. ii > ii_beginning) THEN  ! QuotationMarkC = "
                !--------------------------------------------------------------------------------------------
                ! The first character of the specifier is a quotation mark.
                ! In this case we do not exit yet. We wait until we have obtained the second quotation mark.
                !--------------------------------------------------------------------------------------------
                IF     ( bufferC(ii:ii)                == QuotationMarkC ) THEN  ! QuotationMarkC = "
                 SecondQuotationMarkL = .TRUE.
                END IF

              ELSE IF ( FirstApostropheL    .AND. ii > ii_beginning ) THEN  ! ApostropheC = '
                !--------------------------------------------------------------------------------------------
                ! The first character of the specifier is a apostrophe.
                ! In this case we do not exit yet. We wait until we have obtained the second apostrophe.
                !--------------------------------------------------------------------------------------------
                IF    ( bufferC(ii:ii)                 == ApostropheC    ) THEN  ! ApostropheC = '
                 SecondApostropheL = .TRUE.
                END IF

              ELSE IF (  bufferC(ii:ii) == spec_char        ) THEN
                !-----------------------------------------------------------------------------------------------------
                ! This line checks if 'spec_char' = ' ' is present. Why? Because a blank separates specifier entries!
                ! So this is an important check.
                ! If .TRUE., then exit. But before we exit, we had to check if quotation marks are present.
                ! We did this already above.
                !-----------------------------------------------------------------------------------------------------

                !---------------------------------------------------------------------------------
                ! This should be the usual case, i.e. no quotation mark is present. We thus EXIT.
                !---------------------------------------------------------------------------------
                EXIT
             END IF
             SpecifierValueC(ii:ii) = bufferC(ii:ii)
           ! WRITE(my_output_unit,*) "ii, SpecifierValueC = ",ii,TRIM(SpecifierValueC)
             ii = ii + 1

             IF      (FirstQuotationMarkL .AND. SecondQuotationMarkL) THEN
               EXIT
             ELSE IF (FirstApostropheL    .AND. SecondApostropheL)    THEN
               EXIT
             END IF

            END DO
           ! WRITE(my_output_unit,*) "ii, SpecifierValueC = ",ii,TRIM(SpecifierValueC)," (final)"
            !++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            SpecifierFirstCharacterC = TRIM( ADJUSTL(SpecifierValueC) )
            IF ( SpecifierFirstCharacterC == SpecialMacroCharacterC ) THEN
             !------------------------------------------------------------------------------------------------------------------
             ! Check if first character in specifier string is a special macro character, e.g. '%'.
             !   Example:             %TEMPERATURE = 300.0  ! Initialize variable.
             !            temperate = %Temperature          ! %Temperature is not defined here as variables are case sensitive.
             !------------------------------------------------------------------------------------------------------------------
             WRITE(my_output_unit,'(A)') "================================================================="
             WRITE(my_output_unit,*)     "Error in input file in line number = ",line_number
             WRITE(my_output_unit,'(A)') "  " //TRIM(keyC)
             WRITE(my_output_unit,'(A)') "   "//TRIM(last_spec)//" = "   //TRIM( ADJUSTL(SpecifierValueC) )
             WRITE(my_output_unit,'(A)') "    The specifier starts with the special character used in the macro definition."
             WRITE(my_output_unit,'(A)') "    specifier               = "//TRIM( ADJUSTL(SpecifierValueC) )
             WRITE(my_output_unit,'(A)') "    special macro character = "//SpecialMacroCharacterC
             WRITE(my_output_unit,'(A)') "    It seems that this variable has not been initialized with a value."
             WRITE(my_output_unit,'(A)') "    Please note that variables are case sensitive."
             STOP
            END IF

            
            IF ( FirstQuotationMarkL .AND. .NOT. SecondQuotationMarkL ) THEN
             WRITE(my_output_unit,*) "Some inconsistency when using quotation marks occured in your input file."
             WRITE(my_output_unit,*) " FirstQuotationMarkL = ",FirstQuotationMarkL
             WRITE(my_output_unit,*) "SecondQuotationMarkL = ",SecondQuotationMarkL
             WRITE(my_output_unit,*) "last specifier       = ",TRIM(last_spec)
             WRITE(my_output_unit,*) "line content         = ",TRIM(bufferC)
             WRITE(my_output_unit,*) "line number          = ",line_number
            END IF
            IF ( FirstApostropheL    .AND. .NOT. SecondApostropheL    ) THEN
             WRITE(my_output_unit,*) "Some inconsistency when using apostrophes occured in your input file."
             WRITE(my_output_unit,*) " FirstApostropheL    = ",FirstApostropheL
             WRITE(my_output_unit,*) "SecondApostropheL    = ",SecondApostropheL
             WRITE(my_output_unit,*) "last specifier       = ",TRIM(last_spec)
             WRITE(my_output_unit,*) "line content         = ",TRIM(bufferC)
             WRITE(my_output_unit,*) "line number          = ",line_number
            END IF

            IF ( FirstQuotationMarkL .OR. FirstApostropheL ) THEN
!            IF (DebugLevel > 3) &
              WRITE(my_output_unit,'(A,A,A)') " Quotation marks: specifier entry = ",TRIM(ADJUSTL(SpecifierValueC))," (included)"
            END IF

            !----------------------------------------------------------------
            ! Now we have to get rid of '"' or "'" in case they are present.
            !----------------------------------------------------------------
            IF      ( FirstQuotationMarkL .AND. SecondQuotationMarkL ) THEN
             !-----------------------------------------------------
             ! Replace in string 'SpecifierValueC' all '"' with ' ' (blank).
             !-----------------------------------------------------
             ! CALL CharacterReplace (SpecifierValueC,      '"'       , ' ' )
               CALL CharacterReplace (SpecifierValueC, QuotationMarkC , ' ' )
            ELSE IF ( FirstApostropheL    .AND. SecondApostropheL    ) THEN
             !-----------------------------------------------------
             ! Replace in string 'SpecifierValueC' all '"' with ' ' (blank).
             !-----------------------------------------------------
             ! CALL CharacterReplace (SpecifierValueC,      "'"       , ' ' )
               CALL CharacterReplace (SpecifierValueC, ApostropheC    , ' ' )
            END IF

            !------------------------------------------
            ! Get rid of leading and trailing blanks.
            !------------------------------------------
            SpecifierValueC = TRIM(ADJUSTL(SpecifierValueC))

            IF ( FirstQuotationMarkL .OR. FirstApostropheL) THEN
!            IF (DebugLevel > 3) &
              WRITE(my_output_unit,'(A,A,A)') " Quotation marks: specifier entry = ",TRIM(SpecifierValueC)," (removed)"
            END IF

             CALL value_to_queue(collected_input,keywords,keyC,       &
                                 last_spec,SpecifierValueC,line_number)
           ! WRITE(my_output_unit,*) "keyC             = ",keyC
           ! WRITE(my_output_unit,*) "last_spec       = ",last_spec
           ! WRITE(my_output_unit,'(A,A)') " Quotation marks: specifier entry = ",TRIM(SpecifierValueC)
           ! WRITE(my_output_unit,*) "line_number     = ",line_number
           ! PAUSE

!------------------------------------------------------------------------------
 END SUBROUTINE Add_value_to_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ERROR(ierr)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE Parser_Errors            ,ONLY:Error_PrintStandardMessage
 USE parser_parameters        ,ONLY:keyword_filetypeC, &
                                    SpecialMacroCharacterC
 USE MacroForInputFile        ,ONLY:Check_forbidden_characters

 IMPLICIT NONE

 INTEGER      ,INTENT(in)  :: ierr

 CALL Error_PrintStandardMessage(keyword_filetypeC)

 SELECT CASE(ierr)
  CASE(1)
      WRITE(my_output_unit,*)'got invalid keyword >',TRIM(keyC), &                   !
                '< in line number = ',line_number                      !
  CASE(2)
      WRITE(my_output_unit,*)'found no keywords in ',TRIM(keyword_filetypeC),'.'
  CASE(3)
      WRITE(my_output_unit,*)'invalid keywords in ',TRIM(keyword_filetypeC),'.'
  CASE(4)
      WRITE(my_output_unit,*)'Odd number of keywords in ',TRIM(keyword_filetypeC),'.'
      WRITE(my_output_unit,'(A)') &
       '(Unix/Linux/MacOS: Maybe a carriage return (End of Line) in the last line of the file is missing.)'
  CASE(5)
      WRITE(my_output_unit,'(A,A,A,I15)') &
                              ' I found invalid specifier >>>',TRIM(spec),'<<<  in line number = ',line_number
      WRITE(my_output_unit,'(A)') " Allowed values are defined in the file: "//TRIM(keyword_filenameC)
      !------------------------------------------------------------------
      ! Check if first character is special sign for variable, i.e. '%'.
      !------------------------------------------------------------------
      IF ( spec(1:1) == SpecialMacroCharacterC ) THEN
         WRITE(my_output_unit,'(A)') " You tried to define the variable: "//TRIM(spec)
         CALL Check_forbidden_characters( TRIM(spec) )
      END IF
  CASE(6)
      WRITE(my_output_unit,*)'invalid specifier(s) in ',TRIM(keyword_filetypeC),'.'
  CASE(7)
      WRITE(my_output_unit,*)'expect an = after specifier >',TRIM(spec), &          !
                '<  in line number = ',line_number                     !
  CASE(8)
      WRITE(my_output_unit,*)'nonmatching end keyword  >',TRIM(keyC), &              !
                '<  in line number = ',line_number                     !
      WRITE(my_output_unit,*)'expected end keyword is >',TRIM(tempC),'<'            !
  CASE(9)
      WRITE(my_output_unit,'(A)')         ""
      WRITE(my_output_unit,'(A,A,A)')     " The first specifier after the keyword '",TRIM(keyC),"'"
      WRITE(my_output_unit,'(A,A,A)')     " must be the separating specifier '",TRIM(sep_spec),"'."
      WRITE(my_output_unit,'(A,I8)')      " However, the actual specifier in line number ",line_number
      WRITE(my_output_unit,'(A,A,A,A,A)') " of the ",TRIM(keyword_filetypeC)," was '",TRIM(first_spec),"'"// &
                                          ", i.e. you specified the following:"
      WRITE(my_output_unit,'(A)')         ""
      WRITE(my_output_unit,'(6x,A)')        TRIM(keyC)
      WRITE(my_output_unit,'(7x,A,A)')      TRIM(first_spec)," = ...              !  <==  wrong"
      WRITE(my_output_unit,'(7x,A)')        "..."
      WRITE(my_output_unit,'(6x,A)')        key_char//"end_"//keyC(2:LEN_TRIM(keyC))
      WRITE(my_output_unit,'(A)')         ""
      WRITE(my_output_unit,'(A)')         " Note that however the following syntax is required:"
      WRITE(my_output_unit,'(6x,A)')        TRIM(keyC)
      WRITE(my_output_unit,'(7x,A,A)')      TRIM(sep_spec)  ," = ...              !  <==  correct"
      WRITE(my_output_unit,'(7x,A)')        "..."
      WRITE(my_output_unit,'(6x,A)')        key_char//"end_"//keyC(2:LEN_TRIM(keyC))
  CASE(10)
      WRITE(my_output_unit,*)"Couldn't find specifier after keyword = ",TRIM(keyC)   !
      WRITE(my_output_unit,*)'Keyword was in line number = ',line_number,' of '     !
      WRITE(my_output_unit,*) TRIM(keyword_filetypeC),' Neither in line number = ',line_number
      WRITE(my_output_unit,*)'nor in line number = ',line_number+1,' a valid'       !
      WRITE(my_output_unit,*)'specifier appears at the expected position.'
  CASE(11)
      WRITE(my_output_unit,*)"Couldn't find specifier after keyword = ",TRIM(keyC)   !
      WRITE(my_output_unit,*)'keyword was in line number = ',line_number,' of '     !
      WRITE(my_output_unit,*) TRIM(keyword_filetypeC),'. Neither in line number = ',line_number
      WRITE(my_output_unit,*)'nor in line number = ',line_number+1,' a valid'       !
      WRITE(my_output_unit,*)'specifier appears at the expected position.'
  CASE(12)
      WRITE(my_output_unit,*)"Couldn't find keyword = ",TRIM(ADJUSTL(bufferC))       !
      WRITE(my_output_unit,*)'This keyword was defined to be required, but it'      !
      WRITE(my_output_unit,*)"does not show up in your ",TRIM(keyword_filetypeC)," = ",TRIM(InputFilename_usedC),"."
      WRITE(my_output_unit,*)                                                       !
  CASE DEFAULT
      WRITE(my_output_unit,*)"Error number not defined: ierr = ",ierr 
 END SELECT

 STOP

!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE read_and_analyze_input
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_read_and_analyze_input
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE generic_inputfile
!------------------------------------------------------------------------------
 USE mod_Error_control_variables,ONLY:Error_control_variables

 IMPLICIT NONE

 ! The INTERFACE 'CALL get_from_inputfile(...)' can be used for all different types of variables.
  INTERFACE  get_from_inputfile
     MODULE PROCEDURE  get_spec_data_in    ! integer
     MODULE PROCEDURE  get_spec_data_xs    ! real (single)
     MODULE PROCEDURE  get_spec_data_xd    ! double
     MODULE PROCEDURE  get_spec_data_ca    ! character
     MODULE PROCEDURE  get_spec_data_lo    ! logical
     MODULE PROCEDURE  get_spec_data_inar  ! integer array
     MODULE PROCEDURE  get_spec_data_xsar  ! real (single) array
     MODULE PROCEDURE  get_spec_data_xdar  ! double array
  END INTERFACE

 CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE get_spec_data_in(keyword,new,specifier,cont,in_t,        &
                              pres,line,last)                          
!------------------------------------------------------------------------------
 USE parser_parameters   ,ONLY:keyword_filetypeC
 USE common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes        ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

 LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
 CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
 LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
 LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry 
 INTEGER      :: in_t                                                ! dummy variable for expected integer data type: output
 INTEGER      :: line                                                ! dummy variable, containing line number in input file of actual data value: output
 LOGICAL      :: pres                                                ! dummy variable, signals presence of input data for specifier in actual structure
 INTEGER      :: i
!----------------------------------------------------------------------!
                                                                       !
    !-----------------------------------------------------------------
    ! The NULLIFY statement disassociates a pointer from its target.
    ! Note: The initial association status of a pointer is undefined.
    !       You can use NULLIFY to initialize an undefined pointer,
    !       giving it disassociated status.
    !       Then the pointer can be tested using the intrinsic
    !       function ASSOCIATED.
    !-----------------------------------------------------------------
    IF (first_entry) NULLIFY(a1)                                       !

    IF (new) THEN                                                      !
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF(LEN_TRIM(keyword)==a1%length)THEN                             !
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !(a1%keyword(i),i=1,a1%length)            !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !
      IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !
      IF (.NOT.found_keyL) CALL ERROR(1)                                !
    ELSE                                                               !
      IF (TRIM(last_keyC) /= TRIM(keyword)) CALL ERROR(4)               !
    END IF                                                             !
    IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
    IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     DO i=1,SIZE(b1%input_spec%spin)                                   !
      found_specL = .TRUE.                                              !
      IF(TRIM(b1%input_spec%spin(i))/=TRIM(specifier))found_specL=.FALSE.
      IF (found_specL) EXIT                                             !
     END DO                                                            !
     IF (found_specL) THEN                                              !
      i = MIN(i,SIZE(b1%input_spec%spin))                              !
      pres = b1%input_spec%prin(i)                                     !
      IF (b1%input_spec%prin(i)) THEN                                  !
      in_t = b1%input_spec%in(i)                                       !
      line = b1%input_spec%liin(i)                                     !
      END IF                                                           !
     ELSE                                                              !
      CALL ERROR(3)                                                    !
     END IF                                                            !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR in<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_in',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE get_spec_data_in                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE get_spec_data_xs(keyword,new,specifier,cont,xs_t,        &!
                              pres,line,last)                          !
!----------------------------------------------------------------------!
 USE parser_parameters   ,ONLY:keyword_filetypeC
 USE common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes        ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

   IMPLICIT NONE

   LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   REAL(4) :: xs_t                                           !
   INTEGER      :: line                                                ! dummy variable, containing line number in input file of actual data value: output
   LOGICAL      :: pres                                                ! dummy variable, signals presence of input data for specifier in actual structure
   integer i
!----------------------------------------------------------------------!
                                                                       !
    IF (first_entry) NULLIFY(a1)                                       !
    IF (new) THEN                                                      !
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO 
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF(LEN_TRIM(keyword)==a1%length)THEN                             !
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !
      IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !

      IF (.NOT.found_keyL) CALL ERROR(1)                                !(a1%keyword(i),i=1,a1%length)            !
    ELSE                                                               !
      IF (TRIM(last_keyC) /= TRIM(keyword)) CALL ERROR(4)               !
    END IF                                                             !
    IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
    IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     DO i=1,SIZE(b1%input_spec%spxs)                                   !
      found_specL = .TRUE.                                              !
      IF(TRIM(b1%input_spec%spxs(i))/=TRIM(specifier))found_specL=.FALSE.
      IF (found_specL) EXIT                                             !
     END DO                                                            !
     IF (found_specL) THEN                                              !
      i = MIN(i,SIZE(b1%input_spec%spxs))                              !
      pres = b1%input_spec%prxs(i)                                     !
      IF (b1%input_spec%prxs(i)) THEN                                  !
      xs_t = b1%input_spec%xs(i)                                       !
      line = b1%input_spec%lixs(i)                                     !
      END IF                                                           !
     ELSE                                                              !
      CALL ERROR(3)                                                    !
     END IF                                                            !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR xs<<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xs',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE get_spec_data_xs                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE get_spec_data_xd(keyword,new,specifier,cont,xd_t,        &!
                              pres,line,last)                          !
!----------------------------------------------------------------------!
 USE parser_parameters   ,ONLY:keyword_filetypeC
 USE common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes        ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

   IMPLICIT NONE

   LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   REAL(8) :: xd_t                                           !
   INTEGER      :: line                                                ! dummy variable, containing line number in input file of actual data value: output
   LOGICAL      :: pres                                                ! dummy variable, signals presence of input data for specifier in actual structure
   integer i
!----------------------------------------------------------------------!
                                                                       !
    IF (first_entry) NULLIFY(a1)                                       !
    IF (new) THEN                                                      !
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF(LEN_TRIM(keyword)==a1%length)THEN                             !
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !
      IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !

      IF (.NOT.found_keyL) CALL ERROR(1)                                !(a1%keyword(i),i=1,a1%length)            !
    ELSE                                                               !
      IF (TRIM(last_keyC) /= TRIM(keyword)) CALL ERROR(4)               !
    END IF                                                             !
    IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
    IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     DO i=1,SIZE(b1%input_spec%spxd)                                   !
      found_specL = .TRUE.                                              !
      IF(TRIM(b1%input_spec%spxd(i))/=TRIM(specifier))found_specL=.FALSE.
      IF (found_specL) EXIT                                             !
     END DO                                                            !
     IF (found_specL) THEN                                              !
      i = MIN(i,SIZE(b1%input_spec%spxd))                              !
      pres = b1%input_spec%prxd(i)                                     !
      IF (b1%input_spec%prxd(i)) THEN                                  !
      xd_t = b1%input_spec%xd(i)                                       !
      line = b1%input_spec%lixd(i)                                     !
      END IF                                                           !
     ELSE                                                              !
      CALL ERROR(3)                                                    !
     END IF                                                            !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE  

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR xd<<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xd',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE get_spec_data_xd                                      !
!**********************************************************************!

!**********************************************************************!
 SUBROUTINE get_spec_data_ca(keyword,new,specifier,cont,ca_t,        &!
                             pres,line,last)                          !
!----------------------------------------------------------------------!
!USE system_specific_parser,ONLY:DebugLevel
 USE parser_parameters     ,ONLY:keyword_filetypeC
 USE common_queues         ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes          ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: keyword                             ! keyword   string: Input
 LOGICAL         ,INTENT(in)  :: new                                 ! control parameter (new: new key/stay at key)
 CHARACTER(len=*),INTENT(in)  :: specifier                           ! specifier string: Input
 LOGICAL         ,INTENT(in)  :: cont                                ! control parameter (cont: next/actual spec structure)
 CHARACTER(len=*),INTENT(out) :: ca_t                                ! string valued data
 LOGICAL         ,INTENT(out) :: pres                                  ! dummy variable, signals presence of input data for specifier in actual structure
 INTEGER         ,INTENT(out) :: line                                  ! dummy variable, containing line number in input file of actual data value: output
 LOGICAL         ,INTENT(out) :: last                                  ! control parameter: .TRUE. at output if actual input structure is last queue entry

 INTEGER                      :: i
 LOGICAL                      :: found_keyL
 LOGICAL                      :: found_specL

 ca_t = ''

 IF (first_entry) NULLIFY(a1)

 IF (new) THEN
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF (LEN_TRIM(keyword)==a1%length) THEN
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !

     IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
     END IF                                                           !
     IF (.NOT.found_keyL) CALL ERROR(1)                                ! (a1%keyword(i),i=1,a1%length)
 ELSE
      IF ( TRIM(last_keyC) /= TRIM(keyword) ) CALL ERROR(4)
 END IF
 IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
 IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     DO i=1,SIZE(b1%input_spec%spca)                                   !
      found_specL = .TRUE.                                              !
      IF(TRIM(b1%input_spec%spca(i))/=TRIM(specifier))found_specL=.FALSE.
      IF (found_specL) EXIT                                             !
     END DO                                                            !
     IF (found_specL) THEN                                              !
      i = MIN(i,SIZE(b1%input_spec%spca))                              !
      pres = b1%input_spec%prcaL(i)                                    !
      IF (b1%input_spec%prcaL(i)) THEN                                 !
      ca_t = b1%input_spec%ca(i)                                       !
      line = b1%input_spec%lica(i)                                     !
      END IF                                                           !
     ELSE                                                              !
      CALL ERROR(3)                                                    !
     END IF                                                            !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE  

 INTEGER,INTENT(in)  :: error_number
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR ca<<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    WRITE(my_output_unit,*)" new  = ",new
    WRITE(my_output_unit,*)" cont = ",cont
    WRITE(my_output_unit,*)" last = ",last
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_ca',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE get_spec_data_ca                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE get_spec_data_lo(keyword,new,specifier,cont,lo_t,        &!
                              pres,line,last)                          !
!----------------------------------------------------------------------!
 USE parser_parameters   ,ONLY:keyword_filetypeC
 USE common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes        ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE   

 LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   LOGICAL      :: lo_t    ! INTENT(out)                               ! logical valued data
   INTEGER      :: line                                                ! dummy variable, containing line number in input file of actual data value: output
   LOGICAL      :: pres                                                ! dummy variable, signals presence of input data for specifier in actual structure
   integer i
!----------------------------------------------------------------------!
                                                                       !
    IF (first_entry) NULLIFY(a1)                                       !
    IF (new) THEN                                                      !
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF(LEN_TRIM(keyword)==a1%length)THEN                             !
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !
      IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !

      IF (.NOT.found_keyL) CALL ERROR(1)                                !(a1%keyword(i),i=1,a1%length)            !
    ELSE                                                               !
      IF (TRIM(last_keyC) /= TRIM(keyword)) CALL ERROR(4)               !
    END IF                                                             !
    IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
    IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     DO i=1,SIZE(b1%input_spec%splo)                                   !
      found_specL = .TRUE.                                              !
      IF(TRIM(b1%input_spec%splo(i))/=TRIM(specifier))found_specL=.FALSE.
      IF (found_specL) EXIT                                             !
     END DO                                                            !
     IF (found_specL) THEN                                              !
      i = MIN(i,SIZE(b1%input_spec%splo))                              !
      pres = b1%input_spec%prlo(i)                                     !
      IF (b1%input_spec%prlo(i)) THEN                                  !
      lo_t = b1%input_spec%lo(i)                                       !
      line = b1%input_spec%lilo(i)                                     !
      END IF                                                           !
     ELSE                                                              !
      CALL ERROR(3)                                                    !
     END IF                                                            !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE  

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_lo',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE get_spec_data_lo                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE get_spec_data_inar(keyword,new,specifier,cont,inar_t, &
                                pres,line,last)
!----------------------------------------------------------------------!
 USE parser_parameters   ,ONLY:keyword_filetypeC
 USE common_queues     ! ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes        ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

   LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   INTEGER,DIMENSION(:),POINTER :: inar_t                              !
   INTEGER      :: line                                                ! dummy variable, containing line number in input file of actual data value: output
   LOGICAL      :: pres                                                ! dummy variable, signals presence of input data for specifier in actual structure
   TYPE(in_array_node), POINTER :: c1                                  !
   integer i
!----------------------------------------------------------------------!
                                                                       !
    IF (first_entry) NULLIFY(a1)                                       !
    IF (new) THEN                                                      !
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF(LEN_TRIM(keyword)==a1%length)THEN                             !
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !
      IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !

      IF (.NOT.found_keyL) CALL ERROR(1)                                !(a1%keyword(i),i=1,a1%length)            !
    ELSE                                                               !
      IF (TRIM(last_keyC) /= TRIM(keyword)) CALL ERROR(4)               !
    END IF                                                             !
    IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
    IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     IF (ASSOCIATED(b1%input_spec%inar_queue%top)) THEN                !
      c1 => b1%input_spec%inar_queue%top                               !
     ELSE                                                              !
      CALL ERROR(5)                                                    !
     END IF                                                            !
     DO
      IF (.NOT. ASSOCIATED(c1)) EXIT
      IF (TRIM(c1%spinar)==TRIM(specifier)) THEN                       !
       found_specL = .TRUE.                                             !
       pres = c1%prinar                                                !
       IF (pres) THEN                                                  !
        inar_t => c1%inar                                              !
        line = c1%liinar                                               !
       ELSE                                                            !
        NULLIFY(inar_t)                                                !
        line = 0                                                       !
       END IF                                                          !
       EXIT                                                            !
      END IF                                                           !
      c1 => c1%next_inar                                               !
     END DO                                                            !
     IF (.NOT. found_specL) NULLIFY(inar_t)                             !
     IF (.NOT. found_specL) CALL ERROR(3)                               !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE  

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword      '!
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR inar< >>>>> ERROR 3<<<< >>> ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_inar',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)"Couldn't find queue for integer arrays, associated to  "!
    WRITE(my_output_unit,*)'keyword = ',TRIM(keyword),'. Most likely, you did not   '!
    WRITE(my_output_unit,*)'define an array valued specifier of integer type for   '!
    WRITE(my_output_unit,*)'this keyword. Check the keyword definition file for    '!
    WRITE(my_output_unit,*)'the specifiers which you have really defined.          '!
    WRITE(my_output_unit,*)'specifier = ',TRIM(specifier)
    WRITE(my_output_unit,*)'Control variables are set as follows:                  '!
    WRITE(my_output_unit,*)'new =',new,'      cont = ',cont                         !
    STOP                                                               !
   ELSE IF(error_number>5 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 6.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
                                                                       !
!----------------------------------------------------------------------!
  END SUBROUTINE get_spec_data_inar
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE get_spec_data_xsar(keyword,new,specifier,cont,xsar_t,    &!
                                pres,line,last)                        !
!----------------------------------------------------------------------!
 USE parser_parameters   ,ONLY:keyword_filetypeC
 USE common_queues     ! ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes        ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

   LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   REAL(4),DIMENSION(:),POINTER :: xsar_t                              !
   INTEGER      :: line                                                ! dummy variable, containing line number in input file of actual data value: output
   LOGICAL      :: pres                                                ! dummy variable, signals presence of input data for specifier in actual structure
   TYPE(xs_array_node), POINTER :: c1                                  !
   integer i
!----------------------------------------------------------------------!
                                                                       !
    IF (first_entry) NULLIFY(a1)                                       !
    IF (new) THEN                                                      !
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF(LEN_TRIM(keyword)==a1%length)THEN                             !
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !
      IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !

      IF (.NOT.found_keyL) CALL ERROR(1)                                !(a1%keyword(i),i=1,a1%length)            !
    ELSE                                                               !
      IF (TRIM(last_keyC) /= TRIM(keyword)) CALL ERROR(4)               !
    END IF                                                             !
    IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
    IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     IF (ASSOCIATED(b1%input_spec%xsar_queue%top)) THEN                !
      c1 => b1%input_spec%xsar_queue%top                               !
     ELSE                                                              !
      CALL ERROR(5)                                                    !
     END IF                                                            !
     DO
      IF (.NOT. ASSOCIATED(c1)) EXIT
      IF (TRIM(c1%spxsar)==TRIM(specifier)) THEN                       !
       found_specL = .TRUE.                                             !
       pres = c1%prxsar                                                !
       IF (pres) THEN                                                  !
        xsar_t => c1%xsar                                              !
        line = c1%lixsar                                               !
       ELSE                                                            !
        NULLIFY(xsar_t)                                                !
        line = 0                                                       !
       END IF                                                          !
       EXIT                                                            !
      END IF                                                           !
      c1 => c1%next_xsar                                               !
     END DO                                                            !
     IF (.NOT. found_specL) NULLIFY(xsar_t)                             !
     IF (.NOT. found_specL) CALL ERROR(3)                               !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !

 USE My_Input_and_Output_Units,ONLY:my_output_unit
  
 IMPLICIT NONE

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword      '!
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>> ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xsar',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE get_from_inputfile.                        '!
    WRITE(my_output_unit,*)"Couldn't find queue for real arrays, associated to     "!
    WRITE(my_output_unit,*)'keyword = ',TRIM(keyword),'. Most likely, you did not   '!
    WRITE(my_output_unit,*)'define an array valued specifier of single precision   '!
    WRITE(my_output_unit,*)'type for this keyword.                                 '!
    WRITE(my_output_unit,*)'Check the keyword definition file for the specifiers   '!
    WRITE(my_output_unit,*)'which you have really defined.                         '!
    WRITE(my_output_unit,*)'Control variables are set as follows:                  '!
    WRITE(my_output_unit,*)'Actual specifier = ',TRIM(specifier)                    !
    WRITE(my_output_unit,*)'new =',new,'      cont = ',cont                         !
    STOP                                                               !
   ELSE IF(error_number>5 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile              '!
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 6.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
                                                                       !
  END SUBROUTINE get_spec_data_xsar                                    !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE get_spec_data_xdar(keyword,new,specifier,cont,xdar_t,    &!
                                pres,line,last)                        !
!----------------------------------------------------------------------!
 USE parser_parameters   ,ONLY:keyword_filetypeC
 USE common_queues     ! ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE common_nodes        ,ONLY:a1,b1,first_entry,last_keyC           ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

   LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   REAL(8),DIMENSION(:),POINTER :: xdar_t                              !
   INTEGER      :: line                                                ! dummy variable, containing line number in input file of actual data value: output
   LOGICAL      :: pres                                                ! dummy variable, signals presence of input data for specifier in actual structure
   TYPE(xd_array_node), POINTER :: c1                                  !
   integer i
!----------------------------------------------------------------------!
                                                                       !
    IF (first_entry) NULLIFY(a1)                                       !
    IF (new) THEN                                                      !
     last_keyC = ''                                                     !
     a1 => collected_input%top                                         !
     first_entry = .FALSE.                                             !
     found_keyL = .FALSE.                                               !
     DO
      IF (.NOT. ASSOCIATED(a1)) EXIT
      IF(LEN_TRIM(keyword)==a1%length)THEN                             !
       found_keyL = .TRUE.                                              !
       DO i=1,a1%length                                                !
        IF(a1%keyword(i) /= keyword(i:i)) found_keyL = .FALSE.          !
       END DO                                                          !
      END IF                                                           !
      IF (found_keyL) last_keyC = keyword                                !
      IF (found_keyL) EXIT                                              !
      a1 => a1%next_key                                                !
     END DO                                                            !

      IF (.NOT.found_keyL) THEN                                         !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !
      IF (.NOT.found_keyL) CALL ERROR(1)                                !(a1%keyword(i),i=1,a1%length)            !
    ELSE                                                               !
      IF (TRIM(last_keyC) /= TRIM(keyword)) CALL ERROR(4)               !
    END IF                                                             !
    IF(.NOT.ASSOCIATED(a1)) CALL ERROR(2)                              !
                                                                       !
    IF ( new ) b1 => a1%entries%top                                    !
    IF (cont .AND. .NOT.new ) b1 => b1%next_specifier                  !
    last = .FALSE.                                                     !
    IF (.NOT.ASSOCIATED(b1%next_specifier)) last = .TRUE.              !
     found_specL = .FALSE.                                              !
    IF (ASSOCIATED(b1)) THEN                                           !
     IF (ASSOCIATED(b1%input_spec%xdar_queue%top)) THEN                !
      c1 => b1%input_spec%xdar_queue%top                               !
     ELSE                                                              !
      CALL ERROR(5)                                                    !
     END IF                                                            !
     DO
      IF (.NOT. ASSOCIATED(c1)) EXIT
      IF (TRIM(c1%spxdar)==TRIM(specifier)) THEN                       !
       found_specL = .TRUE.                                             !
       pres = c1%prxdar                                                !
       IF (pres) THEN                                                  !
        xdar_t => c1%xdar                                              !
        line = c1%lixdar                                               !
       ELSE                                                            !
        NULLIFY(xdar_t)                                                !
        line = 0                                                       !
       END IF                                                          !
       EXIT                                                            !
      END IF                                                           !
      c1 => c1%next_xdar                                               !
     END DO                                                            !
     IF (.NOT. found_specL) NULLIFY(xdar_t)                             !
     IF (.NOT. found_specL) CALL ERROR(3)                               !
    END IF                                                             !
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !

 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE  

   INTEGER error_number                                                !
                                                                       !
   IF(error_number==1)THEN                                             !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Detected in Subroutine get_from_inputfile - unknown keyword      '!
    WRITE(my_output_unit,*)'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*)'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*)'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*)'`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>> ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*)'In SUBROUTINE get_spec_data_xdar'!
    WRITE(my_output_unit,*)"Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*)'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*)'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*)'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*)'`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*)'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*)"specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xdar',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE get_from_inputfile.                        '!
    WRITE(my_output_unit,*)"Couldn't find queue for real arrays, associated to     "!
    WRITE(my_output_unit,*)'keyword = ',TRIM(keyword),'. Most likely, you did not   '!
    WRITE(my_output_unit,*)'define an array valued specifier of double precision   '!
    WRITE(my_output_unit,*)'type for this keyword.                                 '!
    WRITE(my_output_unit,*)'Check the keyword definition file for the specifiers   '!
    WRITE(my_output_unit,*)'which you have really defined.                         '!
    WRITE(my_output_unit,*)'Control variables are set as follows:                  '!
    WRITE(my_output_unit,*)'Actual specifier = ',TRIM(specifier)                    !
    WRITE(my_output_unit,*)'new =',new,'      cont = ',cont                         !
    STOP
   ELSE IF (error_number > 5 .OR. error_number < 1 )THEN
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'In ERROR(i) called by SUBROUTINE get_from_inputfile              '!
    WRITE(my_output_unit,*)'Dummy argument must be > 0 and < 6.                    '!
    WRITE(my_output_unit,*)'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*)'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP
   END IF

!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  END SUBROUTINE get_spec_data_xdar
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE generic_inputfile
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_input_driver
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE InputDriver(InputFilenameC,SetInputFileViaCommandLineL, &
                        DatabaseFilenameC,SetDatabaseViaCommandLineL, &
                        SecondEntryL_in)
!------------------------------------------------------------------------------
 USE mod_read_and_analyze_input     ,ONLY:read_and_analyze_input
 USE d_mod_read_and_analyze_input   ,ONLY:d_read_and_analyze_input
 USE system_specific_parser         ,ONLY:DatabaseFileName_DirectoryExtension_C, &
                                             InputFileName_DirectoryExtension_C

 IMPLICIT NONE

 CHARACTER(len=*)         ,INTENT(in)          ::    InputFilenameC
 LOGICAL                  ,INTENT(in)          :: SetInputFileViaCommandLineL
 CHARACTER(len=*)         ,INTENT(in),OPTIONAL :: DatabaseFilenameC
 LOGICAL                  ,INTENT(in),OPTIONAL :: SetDatabaseViaCommandLineL
 LOGICAL                  ,INTENT(in),OPTIONAL :: SecondEntryL_in

 LOGICAL                                       :: SecondEntryL ! control variable for reading in input file several times
 CHARACTER(len=987)                            :: InputFilename_usedC

 !----------------------------------------------------------------------------------------------------
 ! The SecondEntryL flag is needed for the input parser if the input file is read in several times.
 ! Note: If not PRESENT, SecondEntryL = .FALSE. because the input file is read in for the first time.
 !       If it is read in later again, this flag must be set to .TRUE.
 !----------------------------------------------------------------------------------------------------
 IF ( PRESENT(SecondEntryL_in) ) THEN
      SecondEntryL = SecondEntryL_in
 ELSE
      SecondEntryL = .FALSE.
 END IF

! WRITE(*,'(A)') ' Reading input file: '//TRIM(InputFilenameC)
  CALL read_and_analyze_input(InputFilenameC,SetInputFileViaCommandLineL,SecondEntryL, InputFilename_usedC) ! Build up input queue.
  InputFileName_DirectoryExtension_C    = TRIM(InputFilename_usedC)

 IF ( PRESENT(DatabaseFilenameC) ) THEN
! WRITE(*,'(A)') ' Reading database:   '//TRIM(DatabaseFilenameC)
  CALL d_read_and_analyze_input(DatabaseFilenameC,SetDatabaseViaCommandLineL,          InputFilename_usedC)  ! Build up database queue.
  DatabaseFileName_DirectoryExtension_C = TRIM(InputFilename_usedC)
 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE InputDriver
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_input_driver
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_TEST_FortranInputParser
!------------------------------------------------------------------------------
!
!++m* builder_inputfile.f90/mod_TEST_FortranInputParser
!
! NAME 
!   MODULE mod_TEST_FortranInputParser
!
! CONTAINS
!   o SUBROUTINE TEST_CheckIfPositionIsUnique
!   o SUBROUTINE TEST_ReplaceVariables
!   o SUBROUTINE TEST_FortranInputParser
!
! FILENAME
!   input_parser/parser.f90
!
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE TEST_CheckIfPositionIsUnique
!------------------------------------------------------------------------------
!
!++s* mod_TEST_FortranInputParser/TEST_CheckIfPositionIsUnique
!
! NAME
!   SUBROUTINE TEST_CheckIfPositionIsUnique
!
! PURPOSE
!   Test routine for SUBROUTINE CheckIfPositionIsUnique.
!
! USAGE
!   CALL TEST_CheckIfPositionIsUnique
! 
! INPUT
!   none
!
! OUTPUT
!   none
! 
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE MacroForInputFile        ,ONLY:CheckIfPositionIsUnique

 IMPLICIT NONE

 CHARACTER(len=:),ALLOCATABLE   :: variableC
 CHARACTER(len=:),ALLOCATABLE   :: variable_otherC
 CHARACTER(len=:),ALLOCATABLE   :: StringC
 CHARACTER(len=:),ALLOCATABLE   :: String_replacedC
 CHARACTER(len=:),ALLOCATABLE   :: VariableValueC

 INTEGER                        :: position_variable
 CHARACTER(len=40),DIMENSION(4) :: VariableNameCV
 LOGICAL                        :: position_uniqueL
 INTEGER                        :: number_of_tests
 INTEGER                        :: i

 VariableValueC    = '2.332423424523d0'
 variableC         = '%xmin'
 variable_otherC   = '%xmin10'
 VariableNameCV(1) = '%x'
 VariableNameCV(2) = variableC
 VariableNameCV(3) = variable_otherC
 VariableNameCV(4) = '%QuantumWellWidth'
 StringC           = ' %xmin10   =20.0* %xmin '//       '+ %xmin10 ! Variable definition  '
 String_replacedC  = ' %xmin10   =20.0* 2.332423424523d0 + %xmin10 ! Variable definition  '
 position_variable = 2

 number_of_tests = 2

 DO i=1,number_of_tests

  SELECT CASE(i)
   CASE(1)
       StringC        = ' %xmin10 =20.0* %xmin   '//       '+ %xmin10 ! Variable definition  '
   CASE(2)
       StringC        = ' %xmin   =20.0* %xmin10 '//       '+ %xmin10 ! Variable definition  '
  END SELECT

    CALL CheckIfPositionIsUnique(StringC,position_variable,variableC,VariableNameCV, &
                                 position_uniqueL)

  SELECT CASE(i)
   CASE(1)
    IF (position_uniqueL) THEN
       WRITE(my_output_unit,*) " TEST_CheckIfPositionIsUnique failed: "   ,i
       STOP
    ELSE
       WRITE(my_output_unit,*) " TEST_CheckIfPositionIsUnique - success: ",i
    END IF

   CASE(2)
    IF (.NOT. position_uniqueL) THEN
       WRITE(my_output_unit,*) " TEST_CheckIfPositionIsUnique failed: "   ,i
       STOP
    ELSE
       WRITE(my_output_unit,*) " TEST_CheckIfPositionIsUnique - success: ",i
    END IF

  END SELECT

 END DO

!------------------------------------------------------------------------------
 END SUBROUTINE TEST_CheckIfPositionIsUnique
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE TEST_ReplaceVariables
!------------------------------------------------------------------------------
!
!++s* mod_TEST_FortranInputParser/TEST_ReplaceVariables
!
! NAME
!   SUBROUTINE TEST_ReplaceVariables
!
! PURPOSE
!   Test routine for SUBROUTINE ReplaceVariables.
!
! USAGE
!   CALL TEST_ReplaceVariables
! 
! INPUT
!   none
!
! OUTPUT
!   none
! 
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE MacroForInputFile        ,ONLY:MacroC, &
                                    type_variable
 USE parser_parameters        ,ONLY:comment_signsCV
 USE mod_Array_of_Strings     ,ONLY:String_in_Line

 IMPLICIT NONE

 INTEGER,PARAMETER                             :: max_string_length = 1234
 INTEGER                                       :: NumberOfVariables
 INTEGER,PARAMETER                             :: NumberOfLines     = 20
 TYPE(String_in_Line),DIMENSION(NumberOfLines) :: Strings_inputV
 TYPE(String_in_Line),DIMENSION(NumberOfLines) :: StringsV
 TYPE(type_variable) ,DIMENSION(:),ALLOCATABLE :: VariableV

 INTEGER                                       :: i
 INTEGER                                       :: line
 CHARACTER(len=:),ALLOCATABLE                  :: TestStringC
 CHARACTER(len=:),ALLOCATABLE                  :: MacroStringC

 INTEGER                                       :: num_tests
 INTEGER                                       :: test

!MacroStringC = "!***macro*** "
 MacroStringC = TRIM(comment_signsCV(1)) // TRIM(MacroC) // ' '

 num_tests = 5

 DO test=1,num_tests
  WRITE(my_output_unit,'(A)')  "======================"
  WRITE(my_output_unit,*)      " Test #",test
  WRITE(my_output_unit,'(A)')  "======================"

  SELECT CASE(test)
   CASE(1)
    NumberOfVariables = 3
   CASE(2,3)
    NumberOfVariables = 2
   CASE(4)
    NumberOfVariables = 4
   CASE(5)
    NumberOfVariables = 2
  END SELECT

  ALLOCATE(VariableV(    NumberOfVariables))

  DO line=1,NumberOfLines
    Strings_inputV(line)%StringC = ''
  END DO

  SELECT CASE(test)
   CASE(1)

    Strings_inputV(  1 )%StringC = "! hello"
    Strings_inputV(  2 )%StringC = "%x = 5.0"
    Strings_inputV(  3 )%StringC = "%y=2.0"
    Strings_inputV(  4 )%StringC = ""
    Strings_inputV(  5 )%StringC = " x-coordinates = %x %x %x  ! "
    Strings_inputV(  6 )%StringC = "%xmin = -3"
    Strings_inputV(  7 )%StringC = " y-coordinates = %x %xmin %xmin %x"     ! %x occurs twice and at end of line
    Strings_inputV(  8 )%StringC = " z-coordinates = %xmin              ! "

    VariableV(1)%nameC           = "%x"
    VariableV(1)%value_initialC  = "5.0"
    VariableV(1)%defined_in_line = 2

    VariableV(2)%nameC           = "%y"
    VariableV(2)%value_initialC  = "2.0"
    VariableV(2)%defined_in_line = 3

    VariableV(3)%nameC           = "%xmin"
    VariableV(3)%value_initialC  = "-3"
    VariableV(3)%defined_in_line = 6

   CASE(2,3)

    Strings_inputV(  1 )%StringC = "%zmin = 0.0"
    Strings_inputV(  2 )%StringC = "%zmin_sim = 11.0"

    SELECT CASE(test)
     CASE(2)
    Strings_inputV(  3 )%StringC = "z-grid-lines = %zmin %zmin_sim" ! %substring %zmin is left of larger string %zmin_sim
     CASE(3)
    Strings_inputV(  3 )%StringC = "z-grid-lines = %zmin_sim %zmin" ! %substring %zmin is right of larger string %zmin_sim
    END SELECT

    VariableV(1)%nameC           = "%zmin"
    VariableV(1)%value_initialC  = "0.0"
    VariableV(1)%defined_in_line = 1

    VariableV(2)%nameC           = "%zmin_sim"
    VariableV(2)%value_initialC  = "11.0"
    VariableV(2)%defined_in_line = 2

   CASE(4)

    Strings_inputV(  1 )%StringC = "%zmin = 0.0"
    Strings_inputV(  2 )%StringC = "%zmax = 10.0"
    Strings_inputV(  3 )%StringC = "%zmin_sim = -1.0"
    Strings_inputV(  4 )%StringC = "%zmax_sim = 11.0"
    Strings_inputV(  5 )%StringC = "z-grid-lines = %zmin_sim %zmin %zmax %zmax_sim"

    VariableV(1)%nameC           = "%zmin"
    VariableV(1)%value_initialC  = "0.0"
    VariableV(1)%defined_in_line = 1

    VariableV(2)%nameC           = "%zmax"
    VariableV(2)%value_initialC  = "10.0"
    VariableV(2)%defined_in_line = 2

    VariableV(3)%nameC           = "%zmin_sim"
    VariableV(3)%value_initialC  = "-1.0"
    VariableV(3)%defined_in_line = 3

    VariableV(4)%nameC           = "%zmax_sim"
    VariableV(4)%value_initialC  = "11.0"
    VariableV(4)%defined_in_line = 4

   CASE(5)

   ! %Radius      = 2.0
   ! %RadiusShell = 2.0 * %Radius    ! ==> Substring %Radius is contained in string %RadiusShell and afterwards it is used to evaluate %RadiusShell.
    Strings_inputV(  1 )%StringC = "%Radius      = 3.0"
    Strings_inputV(  2 )%StringC = "%RadiusShell = 2.0 * %Radius"

    VariableV(1)%nameC           = "%Radius"
    VariableV(1)%value_initialC  = "3.0"
    VariableV(1)%defined_in_line = 1

    VariableV(2)%nameC           = "%RadiusShell"
    VariableV(2)%value_initialC  = "2.0 * %Radius"
    VariableV(2)%defined_in_line = 2

  END SELECT

  DO line=1,NumberOfLines
    StringsV(line)%StringC = Strings_inputV(line)%StringC
  END DO

  DO i=1,NumberOfVariables
    VariableV(i)%valueC    = VariableV(i)%value_initialC  
  END DO

  !-----------------------------------
  ! Count, get and replace variables.
  !-----------------------------------
  CALL Count_Get_Replace_Variables

  DO line=1,NumberOfLines

   SELECT CASE(test)
    CASE(1)

       SELECT CASE(line)
        CASE(2)
         TestStringC = MacroStringC // Strings_inputV(line)%StringC     //' '//"(5 replacements)" ! variable was replaced
        CASE(3)
         TestStringC = MacroStringC // Strings_inputV(line)%StringC     //' '//"(0 replacements)" ! variable was not replaced
        CASE(5)
         TestStringC =                 " x-coordinates = 5.0 5.0 5.0  ! " ! variable was replaced
        CASE(6)
         TestStringC = MacroStringC // Strings_inputV(line)%StringC     //' '//"(3 replacements)" ! variable was not replaced
        CASE(7)
         TestStringC =                 " y-coordinates = 5.0 -3 -3 5.0"
        CASE(8)
         TestStringC =                 " z-coordinates = -3              ! "
        CASE DEFAULT
         TestStringC =                 Strings_inputV(line)%StringC                            ! unchanged
       END SELECT

    CASE(2,3)
       SELECT CASE(line)
        CASE(1,2)
         TestStringC = MacroStringC // Strings_inputV(line)%StringC     //' '//"(1 replacement)" ! variable was replaced
        CASE(3)

    SELECT CASE(test)
     CASE(2)
         TestStringC = "z-grid-lines = 0.0 11.0"
     CASE(3)
         TestStringC = "z-grid-lines = 11.0 0.0"
    END SELECT

        CASE DEFAULT
         TestStringC =                 Strings_inputV(line)%StringC                            ! unchanged
       END SELECT

    CASE(4)
       SELECT CASE(line)
        CASE(1,2,3,4)
         TestStringC = MacroStringC // Strings_inputV(line)%StringC     //' '//"(1 replacement)" ! variable was replaced
        CASE(5)
         TestStringC = "z-grid-lines = -1.0 0.0 10.0 11.0"
        CASE DEFAULT
         TestStringC =                 Strings_inputV(line)%StringC                            ! unchanged
       END SELECT

    CASE(5)
       SELECT CASE(line)
        CASE(1)
         TestStringC = MacroStringC // Strings_inputV(line)%StringC     //' '//"(1 replacement)" ! variable was replaced
        CASE(2)
         TestStringC = MacroStringC // "%RadiusShell = 2.0 * 3.0"       //' '//"(0 replacements)" ! variable was not replaced
        CASE DEFAULT
         TestStringC =                 Strings_inputV(line)%StringC                            ! unchanged
       END SELECT
   END SELECT 

   IF ( TRIM(StringsV(line)%StringC) /= TestStringC ) THEN
        WRITE(my_output_unit,*) "Error TEST_ReplaceVariables failed: line = ",line
        WRITE(my_output_unit,'(A)')  "       " // TRIM(Strings_inputV(line)%StringC) // " (input)"
        WRITE(my_output_unit,'(A)')  "       " // TestStringC // " (expected string)"
        WRITE(my_output_unit,'(A)')  "       " // TRIM(StringsV(      line)%StringC)
        STOP
   ELSE
       IF ( TRIM(StringsV(line)%StringC) /= "" ) THEN ! omit empty lines
        WRITE(my_output_unit,'(A)') " [okay] " // TRIM(StringsV(line)%StringC)
       END IF
   END IF

  END DO ! Loop over lines.

  DEALLOCATE(VariableV)

 END DO ! End: Loop over test

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Count_Get_Replace_Variables
!------------------------------------------------------------------------------
 USE parser_parameters        ,ONLY:SpecialMacroCharacterC, &
                                    comment_signsCV, &
                                    IF_STATEMENT_CV
 USE MacroForInputFile        ,ONLY:type_variable, &
                                    CountGetVariables, &
                                    ReplaceVariables

 IMPLICIT NONE

 INTEGER                                       :: NumberOfVariables_count
 TYPE(type_variable) ,DIMENSION(:),ALLOCATABLE :: Variable_getV

 ALLOCATE(Variable_getV(NumberOfVariables))

  !-----------------------------------------------------
  ! 1) Count variables.
  !-----------------------------------------------------
  CALL CountGetVariables('count',NumberOfLines,SpecialMacroCharacterC,comment_signsCV,StringsV, &
                                 NumberOfVariables_count) ! Note: 'NumberOfVariables_count' is output.
  IF ( NumberOfVariables_count /= NumberOfVariables ) THEN
      WRITE(my_output_unit,*) "Error CountGetVariables failed: Unexpected number of variables."
      WRITE(my_output_unit,*) "NumberOfVariables_count = ",NumberOfVariables_count
      WRITE(my_output_unit,*) "NumberOfVariables       = ",NumberOfVariables      ," (expected)"
      STOP
  ELSE
      WRITE(my_output_unit,*) NumberOfVariables_count," variables found."
  END IF

  !-----------------------------------------------------
  ! 2) Get variables and its values.
  !    e.g. %variable = <value>
  !    VariableV(:)%nameC = VariableV(:)%value_initialC
  !-----------------------------------------------------
  CALL CountGetVariables('get'  ,NumberOfLines,SpecialMacroCharacterC,comment_signsCV,StringsV, &
                                 NumberOfVariables_count,Variable_getV) ! Note: 'NumberOfVariables' is input.
  WRITE(my_output_unit,'(A)')    ""
 
  !--------------------------------------------------
  ! Test if values of all variables have been found.
  !--------------------------------------------------
  DO i=1,NumberOfVariables
   IF ( VariableV(i)%nameC /= Variable_getV(i)%nameC ) THEN
      WRITE(my_output_unit,*) "variable name inconsistent."
      WRITE(my_output_unit,*) "VariableV(    i)%nameC = ",VariableV(    i)%nameC
      WRITE(my_output_unit,*) "Variable_getV(i)%nameC = ",Variable_getV(i)%nameC
   END IF
   IF ( VariableV(i)%valueC /= Variable_getV(i)%valueC ) THEN
      WRITE(my_output_unit,*) "variable name inconsistent."
      WRITE(my_output_unit,*) "VariableV(    i)%valueC = ",VariableV(    i)%valueC
      WRITE(my_output_unit,*) "Variable_getV(i)%valueC = ",Variable_getV(i)%valueC
   END IF
   IF ( VariableV(i)%defined_in_line /= Variable_getV(i)%defined_in_line ) THEN
      WRITE(my_output_unit,*) "variable name inconsistent."
      WRITE(my_output_unit,*) "VariableV(    i)%defined_in_line = ",VariableV(    i)%defined_in_line
      WRITE(my_output_unit,*) "Variable_getV(i)%defined_in_line = ",Variable_getV(i)%defined_in_line
   END IF
      WRITE(my_output_unit,*) "Variable ",i," has been found and its value is as expected."
  END DO

  DEALLOCATE(Variable_getV)

  !-----------------------------------------------------------------------------------
  ! 3) Replace variables either with its value, or with its evaluated function value.
  !-----------------------------------------------------------------------------------
  CALL ReplaceVariables(NumberOfVariables,max_string_length,VariableV,comment_signsCV, &
                        IF_STATEMENT_CV, &
                        StringsV)

!------------------------------------------------------------------------------
 END SUBROUTINE Count_Get_Replace_Variables
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE TEST_ReplaceVariables
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE TEST_ApplyMacro
!------------------------------------------------------------------------------
!
!++s* mod_TEST_FortranInputParser/TEST_ApplyMacro
!
! NAME
!   SUBROUTINE TEST_ApplyMacro
!
! PURPOSE
!   Test routine for SUBROUTINE ApplyMacro.
!
! USAGE
!   CALL TEST_ApplyMacro
! 
! INPUT
!   none
!
! OUTPUT
!   none
! 
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE MacroForInputFile        ,ONLY:MacroC, &
                                    ApplyMacro
 USE parser_parameters        ,ONLY:comment_signsCV, &
                                    IF_STATEMENT_CV
 USE mod_Array_of_Strings     ,ONLY:String_in_Line
 USE MacroForInputFile        ,ONLY:type_variable

 IMPLICIT NONE

 INTEGER,PARAMETER                             :: max_string_length = 1234
 INTEGER,PARAMETER                             :: NumberOfLines     = 60
 TYPE(String_in_Line),DIMENSION(NumberOfLines) :: Strings_inputV
 TYPE(String_in_Line),DIMENSION(NumberOfLines) :: StringsV

 INTEGER,PARAMETER                             :: DebugLevel = 1
 INTEGER                                       :: i
 INTEGER                                       :: line
 LOGICAL                                       :: MacroActiveL
 CHARACTER(len=*),PARAMETER                    :: SpecialMacroCharacterC = '%'
 CHARACTER(len=:),ALLOCATABLE                  :: TestStringC
 CHARACTER(len=:),ALLOCATABLE                  :: MacroStringC

!MacroStringC = "!***macro*** "
 MacroStringC = TRIM(comment_signsCV(1)) // TRIM(MacroC) // ' '

 DO line=1,NumberOfLines
    Strings_inputV(line)%StringC = ''
 END DO

    Strings_inputV(   1 )%StringC = "! hello"
    Strings_inputV(   2 )%StringC = "%x = 5.0"
    Strings_inputV(   3 )%StringC = "%y=2.0"
    Strings_inputV(   4 )%StringC = ""
    Strings_inputV(   5 )%StringC = " %USE_BAND_GAPS = use-band-gaps = yes         ! Set this line."
    Strings_inputV(   6 )%StringC = "%USE_BAND_GAPS"
    Strings_inputV(   7 )%StringC = ""
    Strings_inputV(   8 )%StringC = " %IncludeHoles     = .TRUE."
    Strings_inputV(   9 )%StringC = ""
    Strings_inputV(  10 )%StringC = "%FunctionParser = yes" ! From now on evaluate all functions.
    Strings_inputV(  11 )%StringC = ""
    Strings_inputV(  12 )%StringC = "%xmin = %x - 3.0"
    Strings_inputV(  13 )%StringC = "%ymin = %y + 13.0"
    Strings_inputV(  14 )%StringC = ""
    Strings_inputV(  15 )%StringC = "%z1 = -2.0"
    Strings_inputV(  16 )%StringC = "  %alpha = %z1 + 3.0"
    Strings_inputV(  17 )%StringC = " x-coordinates = %x  %xmin  ! "
    Strings_inputV(  18 )%StringC = " %IncludeElectrons = '.FALSE.'"
    Strings_inputV(  19 )%StringC = "!IF %IncludeHoles  valence-band-numbers = 1 2 3"
    Strings_inputV(  20 )%StringC = "!IF %IncludeElectrons  conductions-band-numbers = 1 2 3"
    Strings_inputV(  21 )%StringC = ""
    Strings_inputV(  22 )%StringC = "%a=2.5"
    Strings_inputV(  23 )%StringC = "%b=-3.5"
    Strings_inputV(  24 )%StringC = "%c=4"
    Strings_inputV(  25 )%StringC = "%d=%b+%c"
    Strings_inputV(  26 )%StringC = "%e= MIN(%a,%b,2.0,%c,%d)"
    Strings_inputV(  27 )%StringC = "%f =MAX(%a,%b,2.0,%c,%e)"
    Strings_inputV(  28 )%StringC = "%g=%e+%f"
    Strings_inputV(  29 )%StringC = " ==> Result: %g"
    Strings_inputV(  30 )%StringC = "%h=MIN(2)"
    Strings_inputV(  31 )%StringC = "%i=MAX(-3.5)"
    Strings_inputV(  32 )%StringC = "%j=INT(2.0)"
    Strings_inputV(  33 )%StringC = "%k=MIN(%a)"
    Strings_inputV(  34 )%StringC = "%result1=%h+%i+%j+%k"
    Strings_inputV(  35 )%StringC = "%l=min(2)"
    Strings_inputV(  36 )%StringC = "%m=Max(-3.5)"
    Strings_inputV(  37 )%StringC = "%n=Int(2.0)"
    Strings_inputV(  38 )%StringC = "%o=mIN(%a)"
    Strings_inputV(  39 )%StringC = "%result2=%l+%m+%n+%o"
    Strings_inputV(  40 )%StringC = " ==> Result 1: %result1"
    Strings_inputV(  41 )%StringC = " ==> Result 2: %result2"
    Strings_inputV(  42 )%StringC = "%p=INT( %a + %c )"
    Strings_inputV(  43 )%StringC = "%q=MAX( %a + %c )"
!#! Strings_inputV(  42 )%StringC = "%p=INT( MIN( %a , %c))"   ! not working yet
!#! Strings_inputV(  43 )%StringC = "%q=INT( MAX( %a , %c ) )" ! not working yet
    Strings_inputV(  44 )%StringC = " ==> p = %p; q = %q"
    Strings_inputV(  45 )%StringC = ""

 DO line=1,NumberOfLines
    StringsV(line)%StringC = Strings_inputV(line)%StringC
 END DO


  CALL ApplyMacro(SpecialMacroCharacterC,comment_signsCV,IF_STATEMENT_CV,DebugLevel,NumberOfLines,max_string_length, &
                  StringsV,MacroActiveL)

 DO line=1,NumberOfLines

     SELECT CASE(line)
      CASE(2)
       TestStringC = MacroStringC // Strings_inputV(line)%StringC//' '//"(2 replacements)"   ! variable was replaced
      CASE(3)
       TestStringC = MacroStringC // Strings_inputV(line)%StringC//' '//"(1 replacement)"    ! variable was replaced
      CASE(5)
       TestStringC = MacroStringC // TRIM(ADJUSTL(Strings_inputV(line)%StringC)) //' '//"(1 replacement)"  ! variable was replaced
      CASE(6)
       TestStringC =                 "use-band-gaps = yes"                                   !
      CASE(8)
       TestStringC = MacroStringC // TRIM(ADJUSTL(Strings_inputV(line)%StringC))//' '//"(1 replacement)"  ! variable was replaced
      CASE(10)
       TestStringC = MacroStringC // Strings_inputV(line)%StringC  //' '//"(0 replacements)"   ! variable was not replaced
      CASE(12)
       TestStringC = MacroStringC // "%xmin = 5.0 - 3.0"           //' '//"(1 replacement)"  ! variable was replaced
      CASE(13)
       TestStringC = MacroStringC // "%ymin = 2.0 + 13.0"          //' '//"(0 replacements)" !
      CASE(15)
       TestStringC = MacroStringC // "%z1 = -2.0"                  //' '//"(1 replacement)"  !
      CASE(16)
       TestStringC = MacroStringC // "%alpha = -2.00000000e0 + 3.0"//' '//"(0 replacements)" !
      CASE(17)
       TestStringC = " x-coordinates = 5.0  2.00000000e0  ! "
      CASE(18)
       TestStringC = MacroStringC // TRIM(ADJUSTL(Strings_inputV(line)%StringC)) //' '//"(0 replacements)"   ! variable was not replaced
      CASE(19)
                ! "!IF %IncludeHoles  valence-band-numbers = 1 2 3"
       TestStringC = "                valence-band-numbers = 1 2 3"
      CASE(22)
       TestStringC = MacroStringC // TRIM(ADJUSTL(Strings_inputV(line)%StringC)) //' '//"(6 replacements)"
      CASE(23)
       TestStringC = MacroStringC // TRIM(ADJUSTL(Strings_inputV(line)%StringC)) //' '//"(3 replacements)"
      CASE(24)
       TestStringC = MacroStringC // TRIM(ADJUSTL(Strings_inputV(line)%StringC)) //' '//"(5 replacements)"
      CASE(25)
       TestStringC = MacroStringC // "%d=-3.50000000e0+4.00000000e0"             //' '//"(1 replacement)"
      CASE(26)
       TestStringC = MacroStringC // "%e= MIN(2.50000000e0,-3.50000000e0,2.0,4.00000000e0,0.500000000e0)" &
                                                                                 //' '//"(2 replacements)" !
      CASE(27)
       TestStringC = MacroStringC // "%f =MAX(2.50000000e0,-3.50000000e0,2.0,4.00000000e0,-3.50000000e0)" &
                                                                                 //' '//"(1 replacement)" !
      CASE(28)
       TestStringC = MacroStringC // "%g=-3.50000000e0+4.00000000e0"//' '//"(1 replacement)"
      CASE(29)
       TestStringC = " ==> Result: 0.500000000e0"
      CASE(30,31,32,35,36,37)
       TestStringC = MacroStringC // Strings_inputV(line)%StringC  //' '//"(1 replacement)"
      CASE(33)
       TestStringC = MacroStringC // "%k=MIN(2.50000000e0)"//' '//"(1 replacement)"
      CASE(34)
       TestStringC = MacroStringC // "%result1=2.00000000e0+-3.50000000e0+2+2.50000000e0"//' '//"(1 replacement)"
      CASE(38)
       TestStringC = MacroStringC // "%o=mIN(2.50000000e0)"//' '//"(1 replacement)"
      CASE(39)
       TestStringC = MacroStringC // "%result2=2.00000000e0+-3.50000000e0+2+2.50000000e0"//' '//"(1 replacement)"
      CASE(40)
       TestStringC = " ==> Result 1: 3.00000000e0"
      CASE(41)
       TestStringC = " ==> Result 2: 3.00000000e0"
      CASE(42)
       TestStringC = MacroStringC // "%p=INT( 2.50000000e0 + 4.00000000e0 )"//' '//"(1 replacement)"
!#!    TestStringC = MacroStringC // "%p=INT( MIN( 2.50000000e0 , 4.00000000e0 ) )"//' '//"(1 replacement)"
      CASE(43)
       TestStringC = MacroStringC // "%q=MAX( 2.50000000e0 + 4.00000000e0 )"//' '//"(1 replacement)"
!#!    TestStringC = MacroStringC // "%q=INT( MAX( 2.50000000e0 , 4.00000000e0 ) )"//' '//"(1 replacement)"
      CASE(44)
     ! TestStringC = " ==> p = 3; q = 4"
       TestStringC = " ==> p = 7; q = 6.50000000e0"
      CASE DEFAULT
       TestStringC =                 Strings_inputV(line)%StringC                            ! unchanged
     END SELECT

     IF ( TRIM(StringsV(line)%StringC) /= TestStringC ) THEN
        WRITE(my_output_unit,*) "Error TEST_ApplyMacro failed: line = ",line
        WRITE(my_output_unit,'(A)')  "       " // TRIM(Strings_inputV(line)%StringC) // " (input)"
        WRITE(my_output_unit,'(A)')  "       " // TestStringC                        // " (expected string)"
        WRITE(my_output_unit,'(A)')  "       " // TRIM(StringsV(      line)%StringC) // " (result)"
        !STOP
     ELSE
        WRITE(my_output_unit,'(A)') " [okay] " // TRIM(StringsV(line)%StringC)
     END IF

 END DO

    WRITE(my_output_unit,'(A)')  ""
    WRITE(my_output_unit,'(A)')  " --------------"
    WRITE(my_output_unit,'(A)')  " Print strings:"
    WRITE(my_output_unit,'(A)')  " --------------"
    WRITE(my_output_unit,'(A)')  ""
    WRITE(my_output_unit,'(A)')  " -----"
    WRITE(my_output_unit,'(A)')  " INPUT"
    WRITE(my_output_unit,'(A)')  " -----"
 DO line=1,NumberOfLines
    WRITE(my_output_unit,'(A)') Strings_inputV(line)%StringC
 END DO
    WRITE(my_output_unit,'(A)')  ""
    WRITE(my_output_unit,'(A)')  " ------"
    WRITE(my_output_unit,'(A)')  " OUTPUT"
    WRITE(my_output_unit,'(A)')  " ------"
 DO line=1,NumberOfLines
    WRITE(my_output_unit,'(A)') StringsV(line)%StringC
 END DO
    WRITE(my_output_unit,'(A)')  ""
    WRITE(my_output_unit,'(A)')  " ---------------------"
    WRITE(my_output_unit,'(A)')  " Done: TEST_ApplyMacro"
    WRITE(my_output_unit,'(A)')  " ---------------------"

! STOP

!------------------------------------------------------------------------------
 END SUBROUTINE TEST_ApplyMacro
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE TEST_FortranInputParser
!------------------------------------------------------------------------------
!
!++s* mod_TEST_FortranInputParser/TEST_FortranInputParser
!
! NAME
!   SUBROUTINE TEST_FortranInputParser
!
! PURPOSE
!   Test routine for Fortran Input Parser.
!
! USAGE
!   CALL TEST_FortranInputParser
! 
! INPUT
!   none
!
! OUTPUT
!   none
! 
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE CharacterManipulation    ,ONLY:TEST_ReplaceXMLTag

 IMPLICIT NONE

     CALL TEST_ReplaceXMLTag
 WRITE(my_output_unit,*) " TEST_ReplaceXMLTag                - successful."

     CALL TEST_CheckIfPositionIsUnique
 WRITE(my_output_unit,*) " TEST_CheckIfPositionIsUnique      - successful."

     CALL TEST_ReplaceVariables
 WRITE(my_output_unit,*) " TEST_ReplaceVariables             - successful."

     CALL TEST_ApplyMacro
 WRITE(my_output_unit,*) " TEST_ApplyMacro                   - successful."

!------------------------------------------------------------------------------
 END SUBROUTINE TEST_FortranInputParser
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_TEST_FortranInputParser
!------------------------------------------------------------------------------
