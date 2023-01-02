! MODULE d_parser_parameters
! MODULE mod_d_queue
! MODULE d_input_data_types
! MODULE d_input_spec_node_def
! MODULE d_input_specifier_queue_def
! MODULE d_input_key_node_def
! MODULE d_input_key_queue_def
! MODULE mod_d_init_input_key_queue
! MODULE mod_d_add_input_key
! MODULE d_position_at_key_interface
! MODULE d_scan_inp_key_interface
! MODULE d_add_inp_spec_interface
! MODULE d_input_built_up_interface
! MODULE d_mod_value_to_queue
! MODULE d_common_queues
! MODULE d_common_nodes
! MODULE mod_d_check_presence
! MODULE mod_d_read_and_analyze_input
! MODULE generic_database

!------------------------------------------------------------------------------
 MODULE d_parser_parameters
!------------------------------------------------------------------------------
 USE mod_FileExtensions_parser,ONLY:ValidatorC, &
                                    InputC
 
 IMPLICIT NONE

!------------------------------------------------------------------------------
! Note: It seems that using 'Data_len = 987' for everything substantially
!       increases the CPU time necessary for reading in material database.
!       Thus we still use '267' for most character variables.
!------------------------------------------------------------------------------
! INTEGER,PARAMETER :: Data_len      = 987                                       ! NOT RECOMMENDED, significant increase in CPU time consumption for the large material database file
  INTEGER,PARAMETER :: Data_len      = 267                                       ! Maximum line length in input file (267 - SUN, arbitrary - DEC)
! INTEGER,PARAMETER :: Data_len      = 120  ! a little bit faster                ! Maximum line length in input file (267 - SUN, arbitrary - DEC)
  INTEGER,PARAMETER :: Data_len_long = 987                                       ! Maximum line length in input file
! INTEGER,PARAMETER :: Data_len_very_long = 2987                                 ! Maximum line length in input file
  INTEGER,PARAMETER :: char_length_specifier_content = 80                        ! A specifier content can contain up to 267 characters.
  CHARACTER(len=*),PARAMETER      :: SpecialMacroCharacterC = '%'                ! special character used in macro definition
  CHARACTER(len=*),PARAMETER      :: key_char         = '$'                      ! Character which specifies beginning of keyword (can be chosen)
  CHARACTER(len=*),PARAMETER      :: end_key_char     = ' '                      ! Character which specifies end of keyword (must be a blank)
  CHARACTER(len=*),PARAMETER      :: spec_char        = ' '                      ! Character which separates a specifier (must be a blank)
  CHARACTER(len=*),PARAMETER      :: end_spec_char    = '='                      ! Character which specifies end of specifier (can be chosen)
  CHARACTER(len=*),DIMENSION(4),PARAMETER :: comment_signsCV        = [ '! '   , '# '  , &
                                                                        '//'   , '/*' ]       ! Comment signs. Comment sign and text towards right is ignored.
! CHARACTER(len=*),DIMENSION(4),PARAMETER :: XML_tagCV              = [ '< '   , '</'  , &
!                                                                       '> '   , '/>' ]       ! XML tags

  ! Define 'database_nn3_keywords.val'
  ! Define 'database_nn3.in'
  CHARACTER(len=*),PARAMETER :: keyword_filetypeC         = 'database_nn3'                                     ! This is how we call these type of files that we want to parse.
  CHARACTER(len=*),PARAMETER :: keyword_filetype_nnpC     = 'database_nnp'                                     ! This is how we call these type of files that we want to parse.
  CHARACTER(len=*),PARAMETER :: keyword_filenameC         = TRIM(keyword_filetypeC)//'_keywords'//ValidatorC   ! name of file, containing definitions of keywords and specifiers without file extension
! CHARACTER(len=*),PARAMETER :: MaterialDatabaseFileNameC = TRIM(keyword_filetypeC)//InputC                    ! default name for material database: database_nn3.in

!------------------------------------------------------------------------------
 END MODULE d_parser_parameters
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_d_queue
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE d_queue_built_up(input_filename_C,queue_1)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:DebugLevel
 USE d_parser_parameters      ,ONLY:comment_signsCV
 USE queue_type_def           ,ONLY:queue_type
 USE mod_init_queue           ,ONLY:init_queue
 USE mod_push                 ,ONLY:push
 USE CharacterManipulation    ,ONLY:ReplaceTAB, &
                                    ReplaceXMLTag
 USE mod_Array_of_Strings     ,ONLY:String_in_Line
 USE DirectoryFileExist       ,ONLY:CountLinesInFile, &
                                    ReadFileAndStoreLines

 IMPLICIT NONE

 CHARACTER(len=*)                 ,INTENT(in)  :: input_filename_C
 TYPE(queue_type)                              :: queue_1

 CHARACTER(len=:),ALLOCATABLE                  :: bufferC
 TYPE(String_in_Line),DIMENSION(:),ALLOCATABLE :: StringsV
 INTEGER                                       :: NumberOfLines
 INTEGER                                       :: line_number

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
 CALL ReadFileAndStoreLines(input_filename_C, StringsV)

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
    !-------------------------------------------------------------------------------------
    ! CHECK:
    ! The routine ReplaceTAB takes a long time for a large material database file.
    ! Thus we set it into comments currently.
    ! Comment S. Birner: I am actually not sure if this routine takes a long time.
    !                    It seems that it is pretty fast.
    !                    So we better use this feature to make life easier for the users.
    !-------------------------------------------------------------------------------------
    CALL ReplaceTAB(    StringsV(line_number)%StringC )

    !---------------------------------------------------------------------------
    ! XML tags <...> can be present. They are ignored and replaced with blanks.
    !---------------------------------------------------------------------------
    CALL ReplaceXMLTag( StringsV(line_number)%StringC )

   END DO 

   DO line_number=1,NumberOfLines

     bufferC = StringsV(line_number)%StringC

     !---------------------------------------------------
     ! 'bufferC' is input and output to this subroutine.
     !---------------------------------------------------
     CALL push(queue_1,bufferC,line_number,comment_signsCV)               ! push node onto top of queue_1

   END DO

   DEALLOCATE(StringsV)

   !-------------------------------------------------------------------
   ! Reading in the database is usually very fast so far.
   ! However, everything that comes afterwards is more time-consuming.
   !-------------------------------------------------------------------
   IF (DebugLeveL > 4)  WRITE(my_output_unit,*) "Reading in database: done"

!------------------------------------------------------------------------------
 END SUBROUTINE d_queue_built_up
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_d_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE d_input_data_types
!------------------------------------------------------------------------------
  USE d_parser_parameters,ONLY:char_length_specifier_content

 IMPLICIT NONE

 REAL(4)                                  :: xs_t !
 REAL(8)                                  :: xd_t !
 INTEGER                                  :: in_t !
 LOGICAL                                  :: lo_t !
!CHARACTER(Data_len/3)                    :: ca_t !
!CHARACTER(Data_len)                      :: ca_t ! to allow for long strings, e.g. long directory names
                                                  ! Notes: For database: If character string is long, e.g. 267 characters, then the reading of the database takes longer.
 CHARACTER(char_length_specifier_content) :: ca_t ! to allow for long strings, e.g. long directory names
!CHARACTER(len=:),ALLOCATABLE             :: ca_t ! to allow for long strings, e.g. long directory names  <= does not work

!------------------------------------------------------------------------------
END MODULE d_input_data_types
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE d_input_spec_node_def                                             ! define derived data type for one node in queue of specifiers
!------------------------------------------------------------------------------
 USE input_type_names       ,ONLY:char_length_type_name, &
                                  char_length_specifier_name
   USE d_parser_parameters, ONLY: char_length_specifier_content
   USE array_type_specifiers  ,ONLY:in_array_node, &
                                  xs_array_node, &
                                  xd_array_node, &
                                  in_array_queue, &
                                  xs_array_queue, &
                                  xd_array_queue

 IMPLICIT NONE

   TYPE :: input_specifier                                             !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spxs                ! specifier names for real numbers
     CHARACTER(char_length_type_name),DIMENSION(:),POINTER :: tyxs                ! type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opxs                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prxs                ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: lixs                ! value in line number of input file
     REAL(4)     ,DIMENSION(:),POINTER :: xs                  ! array for real numbers (values for specifiers)
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spxd                ! specifier names for real double precision numbers
     CHARACTER(char_length_type_name),DIMENSION(:),POINTER :: tyxd                ! data type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opxd                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prxd                ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: lixd                ! value in line number of input file
     REAL(8)     ,DIMENSION(:),POINTER :: xd                  ! array for double precision real numbers
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spin                ! specifier names for integer numbers
     CHARACTER(char_length_type_name),DIMENSION(:),POINTER :: tyin                ! data type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opin                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prin                ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: liin                ! value in line number of input file
     INTEGER              ,DIMENSION(:),POINTER :: in                  ! array for integer numbers
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: spca                ! specifier names for character string valued input
     CHARACTER(char_length_type_name),DIMENSION(:),POINTER :: tyca                ! data type expected for specifiers
     LOGICAL              ,DIMENSION(:),POINTER :: opca                ! logical values for is_it_an_optional_input_specifier
     LOGICAL              ,DIMENSION(:),POINTER :: prcaL               ! logical values for is_input_specifier_present_in_current_entry
     INTEGER              ,DIMENSION(:),POINTER :: lica                ! value in line number of input file
     CHARACTER(char_length_specifier_content),DIMENSION(:),POINTER :: ca                  ! array for character string valued input ! to allow for long strings, e.g. long directory names
                                                                       !
     CHARACTER(char_length_specifier_name),DIMENSION(:),POINTER :: splo                ! specifier names for logical valued input
     CHARACTER(char_length_type_name),DIMENSION(:),POINTER :: tylo                ! data type expected for specifiers
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
END MODULE d_input_spec_node_def                                       !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE d_input_specifier_queue_def                                     ! queue type definition for specifier queue, associated to a certain keyword node
!------------------------------------------------------------------------------
   USE d_input_spec_node_def ! ,ONLY:inp_spec_node

 IMPLICIT NONE

   TYPE :: input_spec_queue                                            !
     TYPE(inp_spec_node), POINTER :: top                               ! pointer to top  node of specifier queue
     TYPE(inp_spec_node), POINTER :: rear                              ! pointer to rear node of specifier queue
   END TYPE input_spec_queue                                           !
!------------------------------------------------------------------------------
END MODULE d_input_specifier_queue_def                                 !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE d_input_key_node_def                                            ! define derived data type for one node in queue of keywords
!------------------------------------------------------------------------------
 USE d_input_specifier_queue_def ! ,ONLY:input_spec_queue

 IMPLICIT NONE

   TYPE :: input_key_node                                              !
     CHARACTER(1),DIMENSION(:),POINTER :: keyword                      ! pointer to character array, containing a keyword
     INTEGER                           :: length                       ! number of characters in keyword name
     TYPE(input_spec_queue),   POINTER :: entries                      ! pointer to specifier queue, associated with a keyword node
     TYPE(input_key_node)  ,   POINTER :: next_key                     ! pointer to successor node in keyword queue
   END TYPE input_key_node                                             !
!------------------------------------------------------------------------------
END MODULE d_input_key_node_def                                        !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE d_input_key_queue_def                                           ! queue type definition for keyword queue
!------------------------------------------------------------------------------
   USE d_input_key_node_def ! ,ONLY:input_key_node

 IMPLICIT NONE

   TYPE :: input_key_queue                                             !
     TYPE(input_key_node), POINTER :: top                              ! pointer to top  node of keyword queue
     TYPE(input_key_node), POINTER :: rear                             ! pointer to rear node of keyword queue
   END TYPE input_key_queue                                            !
!------------------------------------------------------------------------------
END MODULE d_input_key_queue_def                                       !
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_d_init_input_key_queue
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE d_init_input_key_queue (s)                                  ! Initialize an empty keyword_queue 
!------------------------------------------------------------------------------
   USE d_input_key_queue_def,ONLY:input_key_queue

   IMPLICIT NONE

   TYPE(input_key_queue), INTENT (OUT) :: s                            !
   NULLIFY (s%top,s%rear)                                              ! Disassociate pointer to top and rear of keyword queue

!------------------------------------------------------------------------------
 END SUBROUTINE d_init_input_key_queue
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_d_init_input_key_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE mod_d_add_input_key
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE d_add_input_key (s, bufferC)
!------------------------------------------------------------------------------
   USE d_parser_parameters, ONLY: key_char
   USE d_input_key_queue_def,ONLY:input_key_queue

 IMPLICIT NONE

   TYPE(input_key_queue), INTENT (IN OUT) :: s                         !
   CHARACTER(*),     INTENT (IN OUT) :: bufferC                         !
   CHARACTER(1),DIMENSION(:),POINTER :: local_line                     !

   integer ::  line_length,ichar

   bufferC = TRIM(ADJUSTL(bufferC))                                      ! remove leading and trailing blanks from line
   line_length = LEN_TRIM(ADJUSTL(bufferC))                             ! get length of input line
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
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Empty keyword string not allowed in SUB. add_input_key.'!
    WRITE(my_output_unit,*) 'Illegal attempt to add keyword input node'              !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE add_input_key                            '!
    WRITE(my_output_unit,*) 'keyword string must start with ',key_char,' sign       '!
    WRITE(my_output_unit,*) 'Illegal attempt to add keyword input node              '!
    STOP                                                               !
   ELSE IF(error_number>=3 .OR. error_number<=0)THEN                   !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE add_input_key         '!
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE d_add_input_key
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE mod_d_add_input_key
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE d_position_at_key_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE d_position_at_key(s,bufferC,node,key_positioned)
!------------------------------------------------------------------------------
   USE d_input_key_queue_def,ONLY:input_key_queue
   USE d_input_key_node_def ,ONLY:input_key_node

 IMPLICIT NONE

   TYPE(input_key_queue)         :: s                                  !
   CHARACTER(len=*),     INTENT (IN) :: bufferC                             !
   TYPE(input_key_node),POINTER  :: node                               !
   LOGICAL                       :: key_positioned                     !

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
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE position_at_key.    '!
    WRITE(my_output_unit,*) 'Got empty input_key_queue with empty entry'             !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE position_at_key.                         '!
    WRITE(my_output_unit,*) 'Empty keyword string not allowed in this context.      '!
    WRITE(my_output_unit,*) 'Illegal attempt to position at keyword input node      '!
    STOP                                                               !
   ELSE IF(error_number>2 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE position_at_key.      '!
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF

!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE d_position_at_key
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE d_position_at_key_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE d_scan_inp_key_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE d_scan_inp_keys (s,bufferC,found_inp_key)
!------------------------------------------------------------------------------
 USE d_input_key_queue_def,ONLY:input_key_queue
 USE d_input_key_node_def ,ONLY:input_key_node

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
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Empty keyword string not allowed in SUB. scan_inp_keys.'!
    WRITE(my_output_unit,*) 'Got input_key_queue with empty entry'                   !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE scan_inp_keys.                           '!
    WRITE(my_output_unit,*) 'Keyword string must be not empty.                      '!
    WRITE(my_output_unit,*) 'Illegal attempt to scan for keyword input node         '!
    STOP                                                               !
   ELSE IF(error_number>2 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE scan_inp_keys.        '!
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !

!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE d_scan_inp_keys
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE d_scan_inp_key_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE d_add_inp_spec_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE d_add_inp_spec(s,keywords,keyC,spec)
!------------------------------------------------------------------------------
   USE input_type_names,ONLY:name_xs,name_xd,name_in,name_ca,name_lo,&
                             name_inar,name_xsar,name_xdar, &
                             char_length_type_name, &
                             char_length_choice_name
   USE keyword_queue_def,ONLY:keyword_queue
   USE d_input_key_queue_def ! ,ONLY:input_key_queue
   USE mod_get_key_info,ONLY:get_key_info
   USE d_position_at_key_interface,ONLY:d_position_at_key

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
   CALL d_position_at_key(s,keyC,node,key_positioned)
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
    IF (TRIM(data_type)==TRIM(name_xs))      THEN                      !
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
    IF (TRIM(data_type)==TRIM(name_xs))      THEN                      !
     num_xs = num_xs + 1                                               !
     spec_structure%spxs(num_xs) = TRIM(spec)                          !
     spec_structure%tyxs(num_xs) = TRIM(data_type)                     !
     spec_structure%opxs(num_xs) = optional_specifierL                       !
     spec_structure%lixs(num_xs) = 0                                   !
     spec_structure%prxs(num_xs) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_xd)) THEN                      !
     num_xd = num_xd + 1                                               !
     spec_structure%spxd(num_xd) = TRIM(spec)                          !
     spec_structure%tyxd(num_xd) = TRIM(data_type)                     !
     spec_structure%opxd(num_xd) = optional_specifierL                       !
     spec_structure%lixd(num_xd) = 0                                   !
     spec_structure%prxd(num_xd) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_in)) THEN                      !
     num_in = num_in + 1                                               !
     spec_structure%spin(num_in) = TRIM(spec)                          !
     spec_structure%tyin(num_in) = TRIM(data_type)                     !
     spec_structure%opin(num_in) = optional_specifierL                       !
     spec_structure%liin(num_in) = 0                                   !
     spec_structure%prin(num_in) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_ca)) THEN                      !
     num_ca = num_ca + 1                                               !
     spec_structure%spca(num_ca) = TRIM(spec)                          !
     spec_structure%tyca(num_ca) = TRIM(data_type)                     !
     spec_structure%opca(num_ca) = optional_specifierL                       !
     spec_structure%lica(num_ca) = 0                                   !
     spec_structure%prcaL(num_ca) = .FALSE.                            !
    ELSE IF (TRIM(data_type)==TRIM(name_lo)) THEN                      !
     num_lo = num_lo + 1                                               !
     spec_structure%splo(num_lo) = TRIM(spec)                          !
     spec_structure%tylo(num_lo) = TRIM(data_type)                     !
     spec_structure%oplo(num_lo) = optional_specifierL                       !
     spec_structure%lilo(num_lo) = 0                                   !
     spec_structure%prlo(num_lo) = .FALSE.                             !
    ELSE IF (TRIM(data_type)==TRIM(name_inar)) THEN                    !
     num_inar = num_inar + 1                                           !
     local_inar%spinar = TRIM(spec)                                    !
     local_inar%tyinar = TRIM(data_type)                               !
     local_inar%opinar = optional_specifierL                                 !
     local_inar%liinar = 0                                             !
     local_inar%prinar = .FALSE.                                       !
!     ALLOCATE(local_inar%inar(0))                                      !
     local_inar => local_inar%next_inar                                !
    ELSE IF (TRIM(data_type)==TRIM(name_xsar)) THEN                    !
     num_xsar = num_xsar + 1                                           !
     local_xsar%spxsar = TRIM(spec)                                    !
     local_xsar%tyxsar = TRIM(data_type)                               !
     local_xsar%opxsar = optional_specifierL                                 !
     local_xsar%lixsar = 0                                             !
     local_xsar%prxsar = .FALSE.                                       !
!     ALLOCATE(local_xsar%xsar(0))                                      !
     local_xsar => local_xsar%next_xsar                                !
    ELSE IF (TRIM(data_type)==TRIM(name_xdar)) THEN                    !
     num_xdar = num_xdar + 1                                           !
     local_xdar%spxdar = TRIM(spec)                                    !
     local_xdar%tyxdar = TRIM(data_type)                               !
     local_xdar%opxdar = optional_specifierL                                 !
     local_xdar%lixdar = 0                                             !
     local_xdar%prxdar = .FALSE.                                       !
!     ALLOCATE(local_xdar%xdar(0))                                      !
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

 INTEGER,INTENT(in)  :: error_number

    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    IF(error_number==1)THEN                                            !
    WRITE(my_output_unit,*) 'In SUBROUTINE add_inp_key.                             '!
    WRITE(my_output_unit,*) 'Got illegal data type = ',TRIM(data_type)
    WRITE(my_output_unit,*) 'Check consistency of specifier data types in files     '!
    WRITE(my_output_unit,*) 'keywords.f90 and input_type_names.f90                  '!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*) 'Occured in SUBROUTINE add_inp_spec.                    '!
    WRITE(my_output_unit,*) "Couldn't position at keyword = ",TRIM(keyC)              !
    WRITE(my_output_unit,*) 'Stoped after call to Subroutine position_at_key.       '!
    STOP                                                               !
   ELSE IF(error_number>2 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE add_inp_spec.         '!
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 3.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !

!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE d_add_inp_spec
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE d_add_inp_spec_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE d_input_built_up_interface
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE d_input_built_up(collected_input,bufferC)
!------------------------------------------------------------------------------
   USE d_input_key_queue_def      ,ONLY:input_key_queue,input_key_node
   USE d_scan_inp_key_interface   ,ONLY:d_scan_inp_keys
   USE d_position_at_key_interface,ONLY:d_position_at_key
   USE mod_d_add_input_key        ,ONLY:d_add_input_key
   USE mod_d_init_input_key_queue ,ONLY:d_init_input_key_queue

 IMPLICIT NONE

   TYPE(input_key_queue)             :: collected_input                !
   CHARACTER(*),     INTENT (IN OUT) :: bufferC                         !
      TYPE(input_key_node),POINTER   :: node                           !
 ! LOGICAL                           :: first_entry = .TRUE.           ! first_entry acquires SAVE attribute automatically!!!
   LOGICAL,SAVE                      :: first_entry = .TRUE.           ! S. Birner: To make this statement more obvious, we explicitly state the SAVE attribute.
   LOGICAL                           :: found_inp_key                  !
   LOGICAL                           :: key_positioned                 !
   IF (first_entry) THEN
    !--------------------------------------------------------------------------
    ! Disassociate pointer 'collected_input' to top and rear of keyword queue.
    !--------------------------------------------------------------------------
    CALL d_init_input_key_queue (collected_input)                      ! create a new queue to collect sorted input data
    !--------------------------------------------------------------------
    ! 'collected_input%top' and 'collected_input%rear' are now nullified.
    !--------------------------------------------------------------------
    first_entry = .FALSE.                                              !
   END IF                                                              !
                                                                       !
   CALL d_scan_inp_keys (collected_input,bufferC,found_inp_key)           !
   IF (.NOT.found_inp_key) THEN                                        !
    CALL d_add_input_key (collected_input,bufferC)                        ! push node for new keyword onto top of queue collected_input
   END IF                                                              !
   CALL d_position_at_key(collected_input,bufferC,node,key_positioned)    ! kann spaeter eliminiert werden
   IF(.NOT.key_positioned) CALL ERROR(1)                               ! kann spaeter eliminiert werden
!----------------------------------------------------------------------!
CONTAINS                                                               !
  SUBROUTINE ERROR(error_number)                                       !
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   INTEGER error_number                                                !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    IF(error_number==1)THEN                                            !
    WRITE(my_output_unit,*) 'In SUBROUTINE input_built_up.                          '!
    WRITE(my_output_unit,*) 'After call to SUBROUTINE add_input_key, a control call '!
    WRITE(my_output_unit,*) 'to SUBROUTINE position_at_key was not able to find     '!
    WRITE(my_output_unit,*) 'position of new keyword entry in queue collected_input '!
    STOP                                                               !
   ELSE IF(error_number>1 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE input_built_up.       '!
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 2.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!

!------------------------------------------------------------------------------
 END SUBROUTINE d_input_built_up
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE d_input_built_up_interface
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE d_mod_value_to_queue
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
   SUBROUTINE d_value_to_queue (s,keywords,keywordC,specifierC,SpecifierValueC,line_number)
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
   USE mod_string_to_value      ,ONLY:convert_string_to_value
   USE input_type_names  ,ONLY:name_xs,name_xd,name_in,name_ca,       &
                               name_lo,name_inar,name_xsar,name_xdar, &
                               char_length_type_name
 ! USE   input_data_types, ONLY: xs_t,xd_t,in_t,ca_t,lo_t
   USE d_input_data_types, ONLY: xs_t,xd_t,in_t,ca_t,lo_t                ! If character string is long, e.g. 267 characters, then the reading of the database takes longer.
   USE keyword_queue_def,ONLY:keyword_queue
   USE d_input_key_queue_def
   USE d_position_at_key_interface,ONLY:d_position_at_key
   USE mod_string_in_list,ONLY:string_in_list
   USE generic_add                                                     !
   USE d_parser_parameters,ONLY:Data_len, &
                                key_char, &
                                keyword_filetypeC

   IMPLICIT NONE

   TYPE(input_key_queue)             :: s                              !
   TYPE(keyword_queue),POINTER       :: keywords                       !
   CHARACTER(len=*)   ,INTENT(in)    :: keywordC                            !
   CHARACTER(len=*)   ,INTENT(in)    :: specifierC                           !
   CHARACTER(len=*)   ,INTENT(in)    :: SpecifierValueC                !
   INTEGER            ,INTENT(in)    :: line_number                    !

   TYPE(input_key_node),POINTER,SAVE :: node                           !
   TYPE(inp_spec_node),POINTER       :: spec_node                      !
   TYPE(in_array_node),POINTER       :: local_inar                     !
   TYPE(xs_array_node),POINTER       :: local_xsar                     !
   TYPE(xd_array_node),POINTER       :: local_xdar                     !
   LOGICAL                           :: key_positioned                 !
   LOGICAL                           :: found_keyL,found_specL           !
   LOGICAL                           :: added_value                    !

   CHARACTER(len=char_length_type_name) :: data_typeC                   !

   INTEGER :: num_xs,num_xd,num_in,num_ca,num_lo

 !------------------------------------------------------
 ! Check if specifier provided in input file is valid
 ! and get related information on data type and choice.
 !------------------------------------------------------
   CALL string_in_list(Data_len,key_char,keywords,keywordC,found_keyL,specC_in=specifierC, &
                       found_specL=found_specL, &
                       data_typeC_out=data_typeC)
   IF (.NOT. found_specL) CALL ERROR(1)                                 !

   CALL d_position_at_key(s,keywordC,node,key_positioned)
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
 ! PRINT *,"SpecifierValueC = ",TRIM(SpecifierValueC)
   CALL convert_string_to_value(keyword_filetypeC,data_typeC,SpecifierValueC,line_number,specifierC, &
                                xs_t,xd_t,in_t,ca_t,lo_t)

    added_value = .FALSE.                                              !
    DO                                                                 !
    IF (added_value) EXIT                                              !
    IF (TRIM(data_typeC)==TRIM(name_xs))      THEN                      !
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
     ENDDO                                                             !
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
     ENDDO                                                             !
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
     ENDDO                                                             !
    ELSE IF (TRIM(data_typeC)==TRIM(name_ca)) THEN                      !
     DO num_ca=1,SIZE(spec_node%input_spec%spca)                       !
     IF(TRIM(spec_node%input_spec%spca(num_ca))==TRIM(specifierC))THEN       !
      IF(spec_node%input_spec%prcaL(num_ca))THEN                       !
       CALL ERROR(5)                                                   !
      END IF                                                           !
      spec_node%input_spec%ca(num_ca)   = TRIM(ca_t)                   !
      spec_node%input_spec%prcaL(num_ca) = .TRUE.                      !
      spec_node%input_spec%lica(num_ca) = line_number                  !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     ENDDO                                                             !
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
     ENDDO                                                             !
    ELSE IF (TRIM(data_typeC)==TRIM(name_inar)) THEN                    !
     local_inar => spec_node%input_spec%inar_queue%top                 !
     DO
     IF (.NOT. ASSOCIATED(local_inar)) EXIT
     IF(TRIM(local_inar%spinar)==TRIM(specifierC))THEN                       !
      local_inar%prinar = .TRUE.                                       ! (is present)
      local_inar%liinar = line_number                                  !
      CALL add_array_element(local_inar,in_t)                             !
      added_value = .TRUE.                                             !
     END IF                                                            !
     IF (added_value) EXIT                                             !
     local_inar => local_inar%next_inar                                !
     ENDDO                                                             !
     IF (.NOT.ASSOCIATED(local_inar)) CALL ERROR(6)                    !
    ELSE IF (TRIM(data_typeC)==TRIM(name_xsar)) THEN                    !
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
     ENDDO                                                             !
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
     ENDDO                                                             !
     IF (.NOT.ASSOCIATED(local_xdar)) CALL ERROR(6)                    !
    ELSE                                                               !
     CALL ERROR(6)                                                     !
    END IF                                                             !
   END DO                                                              !

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE ERROR(error_number)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE d_parser_parameters      ,ONLY:keyword_filetypeC, &
                                    keyword_filenameC

   IMPLICIT NONE

   INTEGER,INTENT(in) :: error_number

    IF(error_number==1)THEN                                            !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Got illegal specifier = ',TRIM(specifierC)                    !
    WRITE(my_output_unit,*) 'line number           = ',line_number
    WRITE(my_output_unit,*) 'Check consistency with keyword =',TRIM(keywordC)             !
    WRITE(my_output_unit,*) 'For legal values see file ',TRIM(keyword_filenameC),'.'
    WRITE(my_output_unit,*)                                                          !
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Occured in SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) "Couldn't position at keyword = ",TRIM(keywordC)              !
    WRITE(my_output_unit,*) 'Stoped after call to Subroutine position_at_key.       '!
    WRITE(my_output_unit,*)                                                          !
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Occured in SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) "Couldn't find specifier queue associated with the      "!
    WRITE(my_output_unit,*) 'keyword = ',TRIM(keywordC),' in queue of collected input'    !
    WRITE(my_output_unit,*)                                                          !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Occured in SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Pointer to specifier structure not associated          '!
    WRITE(my_output_unit,*) 'actual keyword   = ',TRIM(keywordC)                          !
    WRITE(my_output_unit,*) 'actual specifier = ',TRIM(specifierC)                         !
    WRITE(my_output_unit,*) 'actual line number of ',TRIM(keyword_filetypeC),' = ',line_number
    WRITE(my_output_unit,*)                                                          !
    STOP                                                               !
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*) '>>>> WARNING <<<< >>>> WARNING <<<< >>>> WARNING <<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE value_to_queue. Multiple data for        '!
    WRITE(my_output_unit,*) 'specifier = ',TRIM(specifierC),'. I take the last one.       '!
    WRITE(my_output_unit,*) 'specifier used is ',TRIM(specifierC),' = ',TRIM(SpecifierValueC)
    WRITE(my_output_unit,*) 'Actual line number of ',TRIM(keyword_filetypeC),' = ',line_number
    WRITE(my_output_unit,*) '>>>> WARNING <<<< >>>> WARNING <<<< >>>> WARNING <<<<<<'!
    WRITE(my_output_unit,*)                                                          !
   ELSE IF(error_number==6)THEN                                        !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) "In SUBROUTINE value_to_queue. Couldn't find specifier  "!
    WRITE(my_output_unit,*) 'in queue of collected input. This should not happen -   '!
    WRITE(my_output_unit,*) "I have no idea what's going on."
    WRITE(my_output_unit,*) 'keyword = ',TRIM(keywordC)                                   !
    WRITE(my_output_unit,*) 'specifier = ',TRIM(specifierC)                                !
    WRITE(my_output_unit,*) 'Actual line number of ',TRIM(keyword_filetypeC),' = ',line_number
    WRITE(my_output_unit,*)                                                          !
    STOP
   ELSE IF(error_number>6 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE value_to_queue: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 7.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    WRITE(my_output_unit,*)                                                          !
    STOP
   END IF

!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END SUBROUTINE d_value_to_queue
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE d_mod_value_to_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE d_common_queues
!------------------------------------------------------------------------------
  USE keyword_queue_def     ,ONLY:keyword_queue
  USE d_input_key_queue_def!,ONLY:input_key_queue

 IMPLICIT NONE

   TYPE(input_key_queue)        :: collected_input
   TYPE(keyword_queue),POINTER  :: keywords

!------------------------------------------------------------------------------
 END MODULE d_common_queues
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE d_common_nodes
!------------------------------------------------------------------------------
 USE input_type_names,ONLY:char_length_keyword_name
 USE d_common_queues, ONLY: input_key_node, inp_spec_node               ! Module containing queues and type definitions of queues

 IMPLICIT NONE

  TYPE(input_key_node), POINTER, SAVE   :: a1                          ! local pointer to keyword node of collected input queue
  TYPE(inp_spec_node) , POINTER, SAVE   :: b1                          ! local pointer to specifier structure in queue associated to keyword
  LOGICAL, SAVE :: first_entry = .TRUE.                                ! check first entry to ensure pointer status
 CHARACTER(len=char_length_keyword_name) :: last_keyC =''                                !

!------------------------------------------------------------------------------
END MODULE d_common_nodes
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_d_check_presence
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE d_CheckPresenceOfRequiredInputSpecifiers
!------------------------------------------------------------------------------
 USE input_type_names,ONLY:char_length_keyword_name, &
                           char_length_specifier_name
 USE d_common_queues ! ,ONLY:collected_input                           ! Module containing queues and type definitions of queues

   IMPLICIT NONE

   TYPE(input_key_node), POINTER  :: a1                                ! local pointer to keyword node of collected input queue
   TYPE(inp_spec_node) , POINTER  :: b1                                ! local pointer to specifier structure in queue associated to keyword
   TYPE(in_array_node), POINTER   :: c1                                !
   TYPE(xs_array_node), POINTER   :: c2                                !
   TYPE(xd_array_node), POINTER   :: c3                                !
   CHARACTER(char_length_keyword_name) :: keyword                                    !
   CHARACTER(char_length_specifier_name) :: specifier                                  !
   LOGICAL                        :: everything_fine                   !
   integer :: i,j
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
        IF((.NOT.b1%input_spec%opin(i)) .AND.                         &
           (.NOT.b1%input_spec%prin(i))       ) THEN                   
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
        IF((.NOT.b1%input_spec%opxs(i)) .AND.                         &
           (.NOT.b1%input_spec%prxs(i))       ) THEN                   
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
        IF((.NOT.b1%input_spec%opxd(i)) .AND.                         &
           (.NOT.b1%input_spec%prxd(i))       ) THEN                   
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

    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) ">>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<<"!
    WRITE(my_output_unit,*) "Couldn't find required input for:"                      !
    WRITE(my_output_unit,*) "Keyword   = " ,TRIM(keyword)                            !
    WRITE(my_output_unit,*) "Specifier =  ",TRIM(specifier)                          !
    WRITE(my_output_unit,*) "Input for this specifier was defined to be not optional."!
    WRITE(my_output_unit,*) ">>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<<"!
!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  END SUBROUTINE d_CheckPresenceOfRequiredInputSpecifiers
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_d_check_presence
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
MODULE mod_d_read_and_analyze_input
!------------------------------------------------------------------------------

IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE d_read_and_analyze_input(InputFilenameC_in,FileNamePresentL)
!------------------------------------------------------------------------------
!
! PURPOSE
!   Build up input queue.
!
! INPUT
!   o InputFilenameC_in:      string could be undefined
!
! OUTPUT
!   none
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE system_specific_parser   ,ONLY:DebugLevel, &
                                    ParseInputFileOnlyL, &
                                    ParseKeywordsDatabaseL
 USE DirectoryFileExist       ,ONLY:file_existsL
 USE mod_FileExtensions_parser,ONLY:InputC
 USE mod_SyntaxFolder         ,ONLY:GetFilenameIncludingSyntaxFolder
 USE mod_InputFileName        ,ONLY:GetInputFileNameFromValidatorFile
 USE d_parser_parameters,ONLY:key_char, &
                       end_key_char, &
                       spec_char, &
                       end_spec_char, &
                       comment_signsCV, &
                       keyword_filenameC           , &
                       keyword_filetypeC           , &
                       Data_len_long, &
                       Data_len
 USE input_type_names       ,ONLY:required_key  ,not_required_key, &
                                  required_input,not_required_input, &
                                  QuotationMarkC, &
                                  ApostropheC
 USE mod_keyword_queue_built_up,ONLY:keyword_queue_built_up
 USE keyword_node_def         ,ONLY:keyword_node
 USE queue_type_def         ,ONLY:queue_type
 USE node_type_def          ,ONLY:node_type
 USE mod_string_in_list,ONLY:string_in_list
 USE mod_d_queue
 USE mod_get_line               ,ONLY:get_next_line, &
                                      get_prev_line
 USE d_input_built_up_interface,ONLY:d_input_built_up
 USE mod_key_positions  ,ONLY:key_positions, &
                              spec_positions
 USE d_add_inp_spec_interface,ONLY:d_add_inp_spec
 USE mod_Get_Separation_Specifier,ONLY:Get_Separation_Specifier
 USE d_common_queues     ,ONLY:keywords,collected_input,input_key_node              ! Module containing queues and type definitions of queues
 USE mod_get_keyword     ,ONLY:get_keyword
 USE mod_d_check_presence,ONLY:d_CheckPresenceOfRequiredInputSpecifiers
 USE mod_Print_Keywords_Queue,ONLY:Print_Keywords
 USE mod_syntax_validator    ,ONLY:InputSyntax

 IMPLICIT NONE

 CHARACTER(len=*) ,INTENT(in) :: InputFilenameC_in
 LOGICAL          ,INTENT(in) :: FileNamePresentL

 LOGICAL                         :: Print_Keywords_XML_File_DebugLevelL
 CHARACTER(len=Data_len_long) :: input_filenameC                     !
 CHARACTER(len=:),ALLOCATABLE :: input_filename_allocatableC
 INTEGER,DIMENSION(:),POINTER :: key_pos                             !
 INTEGER,DIMENSION(:),POINTER :: end_key_pos                         !
 INTEGER                      :: NumberOfKeywordsInLine

 INTEGER,DIMENSION(:),POINTER :: spec_pos                            !
 INTEGER,DIMENSION(:),POINTER :: end_spec_pos                        !
 INTEGER                      :: NumOfSpecifiersInLine               !

 INTEGER                      :: start_position                      !
 INTEGER                      :: stop_position                       !

 LOGICAL                      :: found_keyL                          !
 LOGICAL                      :: found_specL                         !
 CHARACTER(Data_len)          :: keyC                                !
 CHARACTER(Data_len)          :: spec                                !
 CHARACTER(Data_len)          :: bufferC                             !
 CHARACTER(Data_len)          :: buffer_aC                           !
 CHARACTER(Data_len)          :: buffer_bC                           !
 CHARACTER(len=:),ALLOCATABLE    :: tempC                            !

 TYPE(node_type) ,POINTER     :: act_node                            !
 TYPE(queue_type)             :: input_queue                         !
 INTEGER                      :: line_number                         !
 LOGICAL                      :: start_at_topL,found_endL            !
 LOGICAL                      :: found_enda,found_endb               !
 LOGICAL                      :: start_at_endL,found_topL            !
 LOGICAL                      :: start_reading
 LOGICAL                      :: stop_reading                        !
 LOGICAL                      :: read_restL                          !

 LOGICAL                      :: keys_are_ok                         !
 LOGICAL                      :: spec_are_ok                         !

 CHARACTER(Data_len)          :: first_spec                          !
 CHARACTER(Data_len)          :: last_spec                           !
 CHARACTER(Data_len)          :: sep_spec                            !
 LOGICAL                      :: got_first_spec                      !
 LOGICAL                      :: spec_next_line                      !
 LOGICAL                      :: new_keyword                         !
 LOGICAL                      :: process_line                        !

 TYPE(keyword_node)  ,POINTER :: a1                                  !
 TYPE(input_key_node),POINTER :: a2                                  ! local pointer to keyword node of collected input queue
 INTEGER                      :: icount,i,j,ioff,k,ii,ii_stop,iii

 CHARACTER(len=*),PARAMETER   :: KeywordFileTypeC = 'database'

   bufferC = '' ! has to be initialized because it is an allocatable object

    !---------------------------------------
    ! Get name of input file to be read in.
    !---------------------------------------
    IF (FileNamePresentL) THEN
      !----------------------------------------
      ! This is typically the case if the flag
      !   '--database <databasefilename>'
      ! is used.
      !----------------------------------------
      input_filenameC = TRIM(InputFilenameC_in)
    ELSE
      !-------------------------------------------------------------------------
      ! Get input file name from 'keyword_filenameC'.
      ! The name of the input file is specified in 'database_nn3_keywords.val'.
      !-------------------------------------------------------------------------
      IF (ParseKeywordsDatabaseL) THEN ! <== Use keywords validator file, i.e. the file database_nn3_keywords.val is read in.
       !------------------------------------------
       ! Get input file name from validator file.
       !------------------------------------------
       WRITE(my_output_unit,'(A)') ""
       WRITE(my_output_unit,'(A)') " Reading in syntax validator file: "//TRIM(keyword_filenameC)
       CALL GetInputFileNameFromValidatorFile(GetFilenameIncludingSyntaxFolder('',keyword_filenameC), &
                                              comment_signsCV,key_char,spec_char, &
                                              input_filenameC)
       ! input_filenameC contains the filename (possibly including a relative or absolute path)
       ! Here, it is a good idea to check if this file exists.
       ! If not, we print a message to the screen.
       IF (.NOT. file_existsL(input_filenameC)) THEN
        WRITE(my_output_unit,'(1x,A,A,A,A)') TRIM(keyword_filetypeC)," file '",TRIM(input_filenameC),"' does not exist."
        input_filenameC = GetFilenameIncludingSyntaxFolder('',TRIM(keyword_filetypeC)//InputC)            ! 'Syntax/database_nn3.in'
        STOP
      ! WRITE(my_output_unit,'(A,A,A,A,A)') &
      !                   " Taking default ",TRIM(keyword_filetypeC)," file '",TRIM(input_filenameC),"'." ! This corresponds to reading in 'Syntax/database_nn3.in'.
       END IF

      ELSE
       !----------------------------------------------------------------------------
       ! Get input file name from source code,
       ! i.e. the content of database_nn3_keywords.val is contained in source code.
       !----------------------------------------------------------------------------
       input_filename_allocatableC = ''
       CALL InputSyntax(KeywordFileTypeC,.FALSE.,'default-filename', input_filename_allocatableC)
       input_filenameC = input_filename_allocatableC
      END IF
    END IF

    WRITE(my_output_unit,'(A)')         ""
    WRITE(my_output_unit,'(A)')         " ------------------------------------------------------------------------------"
    WRITE(my_output_unit,'(A,A,A,A,A)') " Reading in ",TRIM(keyword_filetypeC),":   '", &
                                                       TRIM(  input_filenameC),"'"

    !-----------------------------
    ! ==> 1: queue for input file
    !-----------------------------
    CALL d_queue_built_up(input_filenameC, input_queue) ! Essentially stores lines and associated information.

    !----------------------------------------------------------------------
    ! Initialize variable in order to avoid that it is used uninitialized.
    !----------------------------------------------------------------------
    last_spec = '(default initialization of this specifier with arbitrary value)'

    ALLOCATE(keywords)
     !--------------------------------
     ! ==> 2: queue for keywords file
     !--------------------------------
    CALL keyword_queue_built_up(KeywordFileTypeC,keyword_filenameC,ParseKeywordsDatabaseL, &
                                Data_len, &
                                key_char,comment_signsCV,required_key,not_required_key, &
                                required_input,not_required_input, keywords)
  ! WRITE(my_output_unit,*) TRIM(input_filenameC),": queue for keywords and specifiers file built up."

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

    start_at_topL = .TRUE.                                             !
    found_endL    = .FALSE.                                            !
    icount       = 0                                                   !
    keys_are_ok  = .TRUE.                                              !
    spec_are_ok  = .TRUE.                                              !

    !---------------------------------------------------------------------------------------
    ! It has to be nullified because SUBROUTINE key_positions checks its associated status.
    !---------------------------------------------------------------------------------------
    NULLIFY(key_pos)
    NULLIFY(end_key_pos)

    DO                                                                 !
     CALL get_next_line (input_queue,bufferC,line_number,start_at_topL, &! Call queue for next line in input file
                         found_endL,act_node)                           !
     IF (found_endL) EXIT                                               ! quit after reading last line in input queue
     CALL key_positions(key_char,end_key_char,bufferC,key_pos,end_key_pos,NumberOfKeywordsInLine)          ! find keyword positions in input line. key_pos(i) gives the starting position
                                                                       ! which is determined by the separating character key_char.
     DO i=1,NumberOfKeywordsInLine                                                    ! end_key_pos(i) is last character position of keyword
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
      IF (.NOT. found_keyL)  keys_are_ok = found_keyL                    !
                                                                       !
      IF (mod(icount,2)==1) tempC = key_char//"end_"//keyC(2:LEN_TRIM(keyC))!
      IF (mod(icount,2)==0) THEN                                       !
       IF(TRIM(tempC)/=TRIM(keyC)) CALL ERROR(8)                       ! check matching of end keyword
      END IF                                                           !
                                                                       !
      IF(mod(icount,2)==1) CALL d_input_built_up(collected_input,keyC)  ! add new node to queue collected input if new keyword in input
     END DO                                                            !
    END DO                                                             !

    IF (icount == 0)       CALL ERROR(2)                               ! check: is at least one keyword in input file
    IF (.NOT. keys_are_ok) CALL ERROR(3)                               ! stop if invalid keywords are in input file
    icount = MOD(icount,2)                                             !
    IF (icount /= 0)       CALL ERROR(4)                               ! check that number of keywords in input file is even (keyC + end_keyC present)
                                                                       !
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
    start_at_topL = .TRUE.                                             !
    start_at_endL = .FALSE.                                            !
    found_topL    = .FALSE.                                            !
    found_endL    = .FALSE.                                            !
    found_enda   = .FALSE.                                             !
    found_endb   = .FALSE.                                             !
    icount       = 0                                                   !

    IF (DebugLevel > 3) &
     WRITE(my_output_unit,'(1x,A,A,A,A)') &
          TRIM(input_filenameC),": Build up input queue for all ",TRIM(keyword_filetypeC)," entries."

    DO                                                                 !
     bufferC  = " "                                                     !
     buffer_aC = " "                                                     !
     buffer_bC = " "                                                     !
     CALL get_next_line (input_queue,bufferC,line_number,start_at_topL,found_endL,act_node) ! Get input line.
     IF(.NOT.found_enda) THEN                                          !
     CALL get_next_line (input_queue,buffer_aC,line_number,start_at_topL,found_enda,act_node) ! Get input line 'a'.
     IF(.NOT.found_endb) THEN                                          !
     CALL get_next_line (input_queue,buffer_bC,line_number,start_at_topL,found_endb,act_node) ! Get input line 'b'.
     END IF                                                            !
     END IF                                                            !
                                                                       !
     IF(.NOT.found_endb) THEN                                          !
     CALL get_prev_line (input_queue,buffer_aC,line_number,start_at_endL,found_topL,act_node)
     END IF                                                            !
     IF(.NOT.found_enda) THEN                                          !
     CALL get_prev_line (input_queue,bufferC,line_number,start_at_endL,found_topL,act_node)
     END IF                                                            !
     IF (found_endL) EXIT                                              ! quit after reading last line in input queue
                                                                       !
     CALL key_positions(key_char,end_key_char,bufferC,key_pos,end_key_pos,NumberOfKeywordsInLine)          ! determine position of keywords in actual line of input
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
        spec_next_line = .TRUE.                                        !

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
       IF(NumOfSpecifiersInLine == 0) THEN                                          !
         !-----------------------------------------------------
         ! Look for specifiers in 'buffer_aC' and 'buffer_bC'.
         !-----------------------------------------------------
         CALL spec_positions(spec_char,end_spec_char,buffer_aC, &
                 buffer_bC,spec_pos,end_spec_pos,NumOfSpecifiersInLine)  ! beep next line
         IF (NumOfSpecifiersInLine == 0) CALL ERROR(10)                             !
         IF (spec_pos(1) /= 1) CALL ERROR(11)                          !
         first_spec = " "                                              !
         DO ii=spec_pos(1),end_spec_pos(1)                             !
         first_spec(ii:ii) = buffer_aC(ii:ii)                            !
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
         spec_next_line = .TRUE.                                       !
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
       IF(.NOT.found_keyL)  CALL ERROR(1)                               !
       IF(.NOT.found_keyL)  CALL ERROR(3)                               !
        IF (.NOT.found_specL) CALL ERROR(5)                             !
        got_first_spec = .TRUE.                                        !
        spec_next_line = .FALSE.                                       !
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
        IF (start_position > LEN_TRIM(bufferC)) stop_reading = .TRUE.   !
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
           ii = start_position                                         !
           ii_stop = stop_position                                     !
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
              (TRIM(spec) == TRIM(sep_spec  ))         ) THEN          !
           tempC = ""                                                   !
           tempC = spec                                                 !
           !----------------------------------------------------------------------------------------------------------
           ! Add input specifier node and transfer information of database_nn3_keywords.val to queue collected_input.
           !----------------------------------------------------------------------------------------------------------
           CALL d_add_inp_spec (collected_input,keywords,keyC,spec)     !
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

  IF (DebugLevel > 0 ) THEN
    WRITE(my_output_unit,'(1x,A,A,A,A)') &
          TRIM(input_filenameC),": Build up input queue for all ",TRIM(keyword_filetypeC)," entries. (finished)"
  END IF

!----------------------------------------------------------------------!
  CALL d_CheckPresenceOfRequiredInputSpecifiers                        ! Check presence of required input specifiers.

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

! WRITE(my_output_unit,*) TRIM(input_filenameC),": d_read_and_analyze_input finished."

!**********************************************************************!
!----------------------------------------------------------------------!
CONTAINS                                                               !

!------------------------------------------------------------------------------
 SUBROUTINE Add_value_to_queue(bufferC, ii)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE CharacterManipulation    ,ONLY:CharacterReplace
 USE d_parser_parameters      ,ONLY:spec_char, &
                                    SpecialMacroCharacterC
 USE input_type_names         ,ONLY:QuotationMarkC, &
                                    ApostropheC
 USE d_mod_value_to_queue     ,ONLY:d_value_to_queue

 IMPLICIT NONE

 CHARACTER(len=*) ,INTENT(in)    :: bufferC 
 INTEGER          ,INTENT(inout) :: ii

 CHARACTER(len=LEN_TRIM(bufferC))                :: SpecifierValueC
 CHARACTER(len=LEN_TRIM(SpecialMacroCharacterC)) :: SpecifierFirstCharacterC
 LOGICAL                         ::  FirstQuotationMarkL
 LOGICAL                         :: SecondQuotationMarkL
 LOGICAL                         ::  FirstApostropheL
 LOGICAL                         :: SecondApostropheL

 INTEGER                         :: ii_beginning

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

             IF (        ii     >  LEN_TRIM(bufferC) ) THEN
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

             ELSE IF ( bufferC(ii:ii) == spec_char        ) THEN
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

            IF ( FirstQuotationMarkL .OR. FirstApostropheL) THEN
             IF (DebugLevel > 3) &
              WRITE(my_output_unit,'(A,A,A)') " Quotation marks: specifier entry = ",TRIM(ADJUSTL(SpecifierValueC))," (included)"
            END IF

            !----------------------------------------------------------------
            ! Now we have to get rid of '"' or "'" in case they are present.
            !----------------------------------------------------------------
            IF ( FirstQuotationMarkL .AND. SecondQuotationMarkL ) THEN
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

            IF ( FirstQuotationMarkL .OR. FirstApostropheL ) THEN
             IF (DebugLevel > 3) &
              WRITE(my_output_unit,'(A,A,A)') " Quotation marks: specifier entry = ",TRIM(SpecifierValueC)," (removed)"
            END IF

             CALL d_value_to_queue(collected_input,keywords,keyC,       &
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
 USE d_parser_parameters      ,ONLY:keyword_filetypeC

 IMPLICIT NONE

 INTEGER      ,INTENT(in)  :: ierr

 CALL Error_PrintStandardMessage(keyword_filetypeC)

 SELECT CASE(ierr)
  CASE(1)
      WRITE(my_output_unit,*) 'got invalid keyword >',TRIM(keyC), &                   !
                '< in line number = ',line_number                      !
  CASE(2)
      WRITE(my_output_unit,*) 'found no keywords in ',TRIM(keyword_filetypeC),'.'
  CASE(3)
      WRITE(my_output_unit,*) 'invalid keywords in ',TRIM(keyword_filetypeC),'.'
  CASE(4)
      WRITE(my_output_unit,*) 'odd number of keywords in ',TRIM(keyword_filetypeC),'.'
      WRITE(my_output_unit,'(A)') &
        ' (Unix/Linux/MacOS: Maybe a carriage return (End of Line) in the last line of the file is missing.)'
  CASE(5)
      WRITE(my_output_unit,'(A,A,A,I15)') &
                              ' I found invalid specifier >>>',TRIM(spec),'<<<  in line number = ',line_number
      WRITE(my_output_unit,'(A)') " Allowed values are defined in the file: "//TRIM(keyword_filenameC)
  CASE(6)
      WRITE(my_output_unit,*) 'invalid specifier(s) in ',TRIM(keyword_filetypeC),'.'
  CASE(7)
      WRITE(my_output_unit,*) 'expect an = after specifier >',TRIM(spec), &          !
                '<  in line number = ',line_number                     !
  CASE(8)
      WRITE(my_output_unit,*) 'nonmatching end keyword  >',TRIM(keyC), &              !
                '<  in line number = ',line_number                     !
      WRITE(my_output_unit,*) 'expected end keyword is >',TRIM(tempC),'<'            !
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
      WRITE(my_output_unit,*) "Couldn't find specifier after keyword = ",TRIM(keyC)   !
      WRITE(my_output_unit,*) 'Keyword was in line number = ',line_number,' of '     !
      WRITE(my_output_unit,*)  TRIM(keyword_filetypeC),' Neither in line number = ',line_number
      WRITE(my_output_unit,*) 'nor in line number = ',line_number+1,' a valid'       !
      WRITE(my_output_unit,*) 'specifier  appears at the expected position.'
  CASE(11)
      WRITE(my_output_unit,*) "Couldn't find specifier after keyword = ",TRIM(keyC)   !
      WRITE(my_output_unit,*) 'keyword was in line number = ',line_number,' of '     !
      WRITE(my_output_unit,*)  TRIM(keyword_filetypeC),'. Neither in line number = ',line_number
      WRITE(my_output_unit,*) 'nor in line number = ',line_number+1,' a valid'       !
      WRITE(my_output_unit,*) 'specifier  appears at the expected position.'
  CASE(12)
      WRITE(my_output_unit,*) "Couldn't find keyword = ",TRIM(ADJUSTL(bufferC))       !
      WRITE(my_output_unit,*) 'This keyword was defined to be required, but it'      !
      WRITE(my_output_unit,*) "does not show up in your ",TRIM(keyword_filetypeC)," = ",TRIM(input_filenameC),"."
      WRITE(my_output_unit,*)                                                        !
  CASE DEFAULT
      WRITE(my_output_unit,*)"Error number not defined: ierr = ",ierr 
 END SELECT

 STOP

!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE d_read_and_analyze_input
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_d_read_and_analyze_input
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE generic_database
!------------------------------------------------------------------------------
 USE mod_Error_control_variables,ONLY:Error_control_variables

 IMPLICIT NONE

 ! The INTERFACE 'CALL get_from_database(...)' can be used for all different types of variables.
  INTERFACE  get_from_database
     MODULE PROCEDURE  d_get_spec_datab_in    ! integer
     MODULE PROCEDURE  d_get_spec_datab_xs    ! real (single)
     MODULE PROCEDURE  d_get_spec_datab_xd    ! double
     MODULE PROCEDURE  d_get_spec_datab_ca    ! character
     MODULE PROCEDURE  d_get_spec_datab_lo    ! logical
     MODULE PROCEDURE  d_get_spec_datab_inar  ! integer array
     MODULE PROCEDURE  d_get_spec_datab_xsar  ! real (single) array
     MODULE PROCEDURE  d_get_spec_datab_xdar  ! double array
  END INTERFACE

 CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE d_get_spec_datab_in(keyword,new,specifier,cont,in_t,        &
                              pres,line,last)
!------------------------------------------------------------------------------
   USE d_parser_parameters   ,ONLY:keyword_filetypeC
   USE d_common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
   USE d_common_nodes                                                    ! Module to share actual pointer to nodes among subroutines for generic get_from_database

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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR in<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_in',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE d_get_spec_datab_in                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE d_get_spec_datab_xs(keyword,new,specifier,cont,xs_t,        &!
                              pres,line,last)                          !
!----------------------------------------------------------------------!
   USE d_parser_parameters   ,ONLY:keyword_filetypeC
   USE d_common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
   USE d_common_nodes                                                    ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR xs<<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xs',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE d_get_spec_datab_xs                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE d_get_spec_datab_xd(keyword,new,specifier,cont,xd_t,        &!
                              pres,line,last)                          !
!----------------------------------------------------------------------!
   USE d_parser_parameters   ,ONLY:keyword_filetypeC
   USE d_common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
   USE d_common_nodes                                                    ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR xd<<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xd',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE d_get_spec_datab_xd                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE d_get_spec_datab_ca(keyword,new,specifier,cont,ca_t,        &!
                              pres,line,last)                          !
!----------------------------------------------------------------------!
 USE d_parser_parameters   ,ONLY:keyword_filetypeC
 USE d_common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
 USE d_common_nodes                                                    ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: keyword                               ! keyword string : Input
 LOGICAL         ,INTENT(in)  :: new                                   ! control parameter (new: new key/stay at key)
 CHARACTER(len=*),INTENT(in)  :: specifier                             ! specifier string : Input
 LOGICAL         ,INTENT(in)  :: cont                                  ! control parameter (cont: next/actual spec structure)
 CHARACTER(len=*),INTENT(out) :: ca_t                                  ! string valued data
 LOGICAL         ,INTENT(out) :: pres                                  ! dummy variable, signals presence of input data for specifier in actual structure
 INTEGER         ,INTENT(out) :: line                                  ! dummy variable, containing line number in input file of actual data value: output
 LOGICAL         ,INTENT(out) :: last                                  ! control parameter: .TRUE. at output if actual input structure is last queue entry

 INTEGER                      :: i
 LOGICAL                      :: found_keyL
 LOGICAL                      :: found_specL

   ca_t = ''
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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR ca<<< >>>>> ERROR <<<< >>> ',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    WRITE(my_output_unit,*)" new  = ",new
    WRITE(my_output_unit,*)" cont = ",cont
    WRITE(my_output_unit,*)" last = ",last
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_ca',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE d_get_spec_datab_ca                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE d_get_spec_datab_lo(keyword,new,specifier,cont,lo_t, &    !
                                 pres,line,last)                       !
!----------------------------------------------------------------------!
   USE d_parser_parameters   ,ONLY:keyword_filetypeC
   USE d_common_queues       ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
   USE d_common_nodes                                                  ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

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
    IF (.NOT.found_keyL) THEN                                           !
       pres = .FALSE. ; line = -1 ; last = .TRUE. ; RETURN             !
      END IF                                                           !

      IF (.NOT.found_keyL) CALL ERROR(1)                                !(a1%keyword(i),i=1,a1%length) !
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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>',TRIM(keyword_filetypeC),' <<<' !
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_lo',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number>4 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 5.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
  END SUBROUTINE d_get_spec_datab_lo                                      !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE d_get_spec_datab_inar(keyword,new,specifier,cont,inar_t,    &!
                                pres,line,last)                        !
!----------------------------------------------------------------------!
   USE d_parser_parameters   ,ONLY:keyword_filetypeC
   USE d_common_queues    ! ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
   USE d_common_nodes                                                    ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword      '!
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR inar< >>>>> ERROR 3<<<< >>> ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_inar',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Occured in SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) "Couldn't find queue for integer arrays, associated to  "!
    WRITE(my_output_unit,*) 'keyword = ',TRIM(keyword),'. Most likely, you did not   '!
    WRITE(my_output_unit,*) 'define an array valued specifier of integer type for   '!
    WRITE(my_output_unit,*) 'this keyword. Check the keyword definition file for    '!
    WRITE(my_output_unit,*) 'the specifiers which you have really defined.          '!
    WRITE(my_output_unit,*) 'specifier = ',TRIM(specifier)
    WRITE(my_output_unit,*) 'Control variables are set as follows:                  '!
    WRITE(my_output_unit,*) 'new =',new,'      cont = ',cont                         !
    STOP                                                               !
   ELSE IF(error_number>5 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 6.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
                                                                       !
!----------------------------------------------------------------------!
  END SUBROUTINE d_get_spec_datab_inar                                    !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE d_get_spec_datab_xsar(keyword,new,specifier,cont,xsar_t,    &!
                                pres,line,last)                        !
!----------------------------------------------------------------------!
   USE d_parser_parameters   ,ONLY:keyword_filetypeC
   USE d_common_queues    ! ,ONLY:collected_input                       ! Module containing queues and type definitions of queues
   USE d_common_nodes                                                    ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

   LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   REAL(4),DIMENSION(:),POINTER :: xsar_t                    !
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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword      '!
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile: ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>> ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xsar',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Occured in SUBROUTINE get_from_inputfile.                        '!
    WRITE(my_output_unit,*) "Couldn't find queue for real arrays, associated to     "!
    WRITE(my_output_unit,*) 'keyword = ',TRIM(keyword),'. Most likely, you did not   '!
    WRITE(my_output_unit,*) 'define an array valued specifier of single precision   '!
    WRITE(my_output_unit,*) 'type for this keyword.                                 '!
    WRITE(my_output_unit,*) 'Check the keyword definition file for the specifiers   '!
    WRITE(my_output_unit,*) 'which you have really defined.                         '!
    WRITE(my_output_unit,*) 'Control variables are set as follows:                  '!
    WRITE(my_output_unit,*) 'Actual specifier = ',TRIM(specifier)                    !
    WRITE(my_output_unit,*) 'new =',new,'      cont = ',cont                         !
    STOP                                                               !
   ELSE IF(error_number>5 .OR. error_number<1)THEN                     !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile              '!
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 6.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP                                                               !
   END IF                                                              !
  END SUBROUTINE ERROR                                                 !
!----------------------------------------------------------------------!
                                                                       !
  END SUBROUTINE d_get_spec_datab_xsar                                 !
!**********************************************************************!

!**********************************************************************!
  SUBROUTINE d_get_spec_datab_xdar(keyword,new,specifier,cont,xdar_t,    &!
                                pres,line,last)                        !
!----------------------------------------------------------------------!
   USE d_parser_parameters   ,ONLY:keyword_filetypeC
   USE d_common_queues     ! ,ONLY:collected_input                     ! Module containing queues and type definitions of queues
   USE d_common_nodes                                                  ! Module to share actual pointer to nodes among subroutines for generic get_from_inputfile

 IMPLICIT NONE

   LOGICAL       :: found_keyL, found_specL                              ! local logicals
!----------------------------------------------------------------------!
   CHARACTER(*) :: keyword, specifier                                  ! keyword and specifier string : Input
   LOGICAL      :: new, cont                                           ! control parameters (new: new key/stay at key)(cont: next/actual spec structure)
   LOGICAL      :: last                                                ! control parameters: .TRUE. at output if actual input structure is last queue entry
   REAL(8),DIMENSION(:),POINTER :: xdar_t                    !
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
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Detected in Subroutine get_from_inputfile - unknown keyword      '!
    WRITE(my_output_unit,*) 'I do not know requested keyword = ',TRIM(keyword),' !   '!
    WRITE(my_output_unit,*) 'Are you sure that you have defined it?                 '!
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    STOP                                                               !
   ELSE IF(error_number==2)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In SUBROUTINE get_from_inputfile                                 '!
    WRITE(my_output_unit,*) 'At first entry in this Subroutine, the control variable'!
    WRITE(my_output_unit,*) '`new` must be set to .TRUE.                            '!
    STOP                                                               !
   ELSE IF(error_number==3)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>> ',TRIM(keyword_filetypeC)
    WRITE(my_output_unit,*) 'In SUBROUTINE get_spec_data_xdar'!
    WRITE(my_output_unit,*) "Couldn't find requested specifier = ",TRIM(specifier)   !
    WRITE(my_output_unit,*) 'Actual keyword = ',TRIM(keyword)                        !
    WRITE(my_output_unit,*) 'You asked for a specifier which is not defined or at   '!
    WRITE(my_output_unit,*) 'last entry in this subroutine, the control variable    '!
    WRITE(my_output_unit,*) '`last` was .TRUE. which signals the end of the queue   '!
    WRITE(my_output_unit,*) 'containing the specifier structures. Pointer to next   '!
    WRITE(my_output_unit,*) "specifier structures wouldn't be associated."           !
    STOP                                                               !
   ELSE IF(error_number==4)THEN                                        !
    CALL Error_control_variables('get_spec_data_xdar',keyword_filetypeC,keyword,specifier,line,last_keyC,new,cont)
   ELSE IF(error_number==5)THEN                                        !
    WRITE(my_output_unit,*)                                                          !
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'Occured in SUBROUTINE get_from_inputfile.                        '!
    WRITE(my_output_unit,*) "Couldn't find queue for real arrays, associated to     "!
    WRITE(my_output_unit,*) 'keyword = ',TRIM(keyword),'. Most likely, you did not   '!
    WRITE(my_output_unit,*) 'define an array valued specifier of double precision   '!
    WRITE(my_output_unit,*) 'type for this keyword.                                 '!
    WRITE(my_output_unit,*) 'Check the keyword definition file for the specifiers   '!
    WRITE(my_output_unit,*) 'which you have really defined.                         '!
    WRITE(my_output_unit,*) 'Control variables are set as follows:                  '!
    WRITE(my_output_unit,*) 'Actual specifier = ',TRIM(specifier)                    !
    WRITE(my_output_unit,*) 'new =',new,'      cont = ',cont                         !
    STOP
   ELSE IF (error_number>5 .OR. error_number<1) THEN
    WRITE(my_output_unit,*) '>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*) 'In ERROR(i) called by SUBROUTINE get_from_inputfile              '!
    WRITE(my_output_unit,*) 'Dummy argument must be > 0 and < 6.                    '!
    WRITE(my_output_unit,*) 'Actual dummy argument is i = ',error_number             !
    WRITE(my_output_unit,*) 'Illegal attempt to call SUBROUTINE ERROR               '!
    STOP
   END IF

!------------------------------------------------------------------------------
  END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  END SUBROUTINE d_get_spec_datab_xdar
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE generic_database
!------------------------------------------------------------------------------
