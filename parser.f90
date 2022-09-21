! MODULE mod_FileExtensions_parser
! MODULE Use_FunctionParser
! MODULE input_type_names
! MODULE mod_input_data
! MODULE input_line_definition
! MODULE node_type_def
! MODULE queue_type_def
! MODULE specifier_node_def
! MODULE specifier_queue_def
! MODULE keyword_node_def
! MODULE keyword_queue_def
! MODULE mod_init_queue
! MODULE mod_push
! MODULE mod_get_line
! MODULE mod_init_keyword_queue
! MODULE mod_get_keyword
! MODULE mod_add_keyword
! MODULE mod_get_specifier
! MODULE mod_check_keyword
! MODULE mod_check_specifier
! MODULE mod_get_key_info
! MODULE mod_Get_Separation_Specifier
! MODULE mod_Print_Keywords_Queue
! MODULE mod_keyword_queue_built_up
! MODULE Parser_Errors
! MODULE MacroForInputFile
! MODULE mod_queue
! MODULE mod_InputFileName
! MODULE mod_string_in_list
! MODULE array_type_specifiers
! MODULE generic_add
! MODULE mod_key_positions
! MODULE mod_string_to_value
! MODULE mod_log_file

!------------------------------------------------------------------------------
 MODULE mod_FileExtensions_parser
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*),PARAMETER :: ValidatorC    = '.val'      ! file extension validator files (syntax definition)
 CHARACTER(len=*),PARAMETER :: InputC        = '.in'       ! file extension input files
 CHARACTER(len=*),PARAMETER :: TextC         = '.txt'      ! file extension for text data which cannot be plotted (ASCII format)
 CHARACTER(len=*),PARAMETER :: XML_C         = '.xml'      ! file extension for XML files
 CHARACTER(len=*),PARAMETER :: LogC          = '.log'      ! file extension for log files

!------------------------------------------------------------------------------
 END MODULE mod_FileExtensions_parser
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 module Filenames_mod_parser
!------------------------------------------------------------------------------
 use mod_FileExtensions_parser, only:TextC

 implicit none

 character(len=*), parameter :: FILENAME_VARIABLES_C = 'variables_input'//TextC             ! index label like 'ind000', 'ind001', ...

!------------------------------------------------------------------------------
 end module Filenames_mod_parser
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_Brackets
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 FUNCTION is_even(i) RESULT (evenL)
!------------------------------------------------------------------------------
!
!++f* mod_Brackets/is_even
!
! NAME
!   FUNCTION is_even
!
! PURPOSE
!   Checks if an integer number 'i' is even, i.e. if it is 0,2,4,6,8,... (including negative numbers).
!
! USAGE
!   is_even(i)
!
! INPUT
!   o i:          integer number
!
! OUTPUT
!   o evenL:      .TRUE. if number is even, else .FALSE.
!
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER               ,INTENT(in) :: i
 LOGICAL                           :: evenL

 INTEGER                           :: rest

 rest = MOD(i, 2)

 IF (rest == 0) THEN
  evenL = .TRUE.
 ELSE
  evenL = .FALSE.
 END IF

!------------------------------------------------------------------------------
 END FUNCTION is_even
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION is_odd(i) RESULT (oddL)
!------------------------------------------------------------------------------
!
!++f* mod_Brackets/is_odd
!
! NAME
!   FUNCTION is_odd
!
! PURPOSE
!   Checks if an integer number 'i' is odd, i.e. if it is 1,3,5,6,7,... (including negative numbers).
!
! USAGE
!   is_odd(i)
!
! INPUT
!   o i:          integer number
!
! OUTPUT
!   o oddL:      .TRUE. if number is odd, else .FALSE.
!
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER               ,INTENT(in) :: i
 LOGICAL                           :: oddL

 LOGICAL                           :: evenL

 evenL = is_even(i)

 IF (evenL) THEN
  oddL = .FALSE.
 ELSE
  oddL = .TRUE.
 END IF

!------------------------------------------------------------------------------
 END FUNCTION is_odd
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION CompletelyEnclosed(StringC,BracketTypeC) RESULT (EnclosedL)
!------------------------------------------------------------------------------
!
!++f* mod_Brackets/CompletelyEnclosed
!
! NAME
!   FUNCTION CompletelyEnclosed
!
! PURPOSE
!   Counts how often a character or substring occurs in a string.
!
! USAGE
!   CompletelyEnclosed(StringC,BracketTypeC)
! 
! INPUT 
!   o StringC:       string to be examined
!   o BracketTypeC:  type of bracket
!
! OUTPUT
!   o EnclosedL:     .TRUE. if fully enclosed, else .FALSE.
!
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE CharacterManipulation    ,ONLY:CountCharacters

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: StringC
 CHARACTER(len=*),INTENT(in)  :: BracketTypeC
 LOGICAL                      :: EnclosedL

 INTEGER                                               :: Brackets_Open
 INTEGER                                               :: Brackets_Closed

 CHARACTER(len=1)                                      :: Character_OpenC
 CHARACTER(len=1)                                      :: Character_ClosedC
 
 SELECT CASE( TRIM(BracketTypeC) )
    CASE('(','()')
     Character_OpenC = '('   ;   Character_ClosedC = ')'
    CASE('[','[]')
     Character_OpenC = '['   ;   Character_ClosedC = ']'
    CASE('][')
     Character_OpenC = ']'   ;   Character_ClosedC = '['
    CASE('<','<>')
     Character_OpenC = '<'   ;   Character_ClosedC = '>'
    CASE('><')
     Character_OpenC = '>'   ;   Character_ClosedC = '<'
    CASE('{','{}')
     Character_OpenC = '{'   ;   Character_ClosedC = '}'
    CASE('"','""')
     Character_OpenC = '"'   ;   Character_ClosedC = '"'
    CASE("'","''")
     Character_OpenC = "'"   ;   Character_ClosedC = "'"
    CASE DEFAULT
     WRITE(my_output_unit,'(A)')   " Error CompletelyEnclosed: Bracket type not supported."
     WRITE(my_output_unit,'(A,A)') "                           Bracket type = ",TRIM(BracketTypeC)
     STOP
 END SELECT
 
 !--------------------------------------------------------------------------------------
 ! Count number of brackets '(') and ')' to determine if string is completely enclosed.
 ! Compare with FUNCTION CompletelyEnclosed.
 !--------------------------------------------------------------------------------------
 Brackets_Open   = CountCharacters(StringC,Character_OpenC)
 Brackets_Closed = CountCharacters(StringC,Character_ClosedC)
      
 IF ( is_odd( Brackets_Open + Brackets_Closed ) ) THEN
      EnclosedL = .FALSE.
 ELSE
      EnclosedL = .TRUE.
 END IF

!------------------------------------------------------------------------------
 END FUNCTION CompletelyEnclosed
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_Brackets
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE Use_FunctionParser
!------------------------------------------------------------------------------
!
!++m* parser.f90/Use_FunctionParser
!
! NAME 
!   MODULE Use_FunctionParser
!
! CONTAINS
!   o FUNCTION Evaluate_Function
!
! FILENAME
!   input_parser/parser.f90
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 FUNCTION Evaluate_Function(FunctionC,VariableNamesCV,VariableValuesV) RESULT(FunctionResult)
!------------------------------------------------------------------------------
!
!++f* Use_FunctionParser/Evaluate_Function
!
! NAME
!   FUNCTION Evaluate_Function
!
! PURPOSE
!   Returns the evaluated value of a function.
!
! USAGE
!   Evaluate_Function(FunctionC,VariableNamesCV, VariableValuesV)
! 
! INPUT 
!   o FunctionC:        function, e.g. 'x^2 + y^2 + z^2'
!   o VariableNamesCV:  name of variables, e.g. 'x','y','z'
!   o VariableValuesV:  values of variables x,y,z
!
! OUTPUT
!   o FunctionResult:   result of function
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE system_specific_parser   ,ONLY:DebugLevel
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE parameters               ,ONLY:rn
 USE fparser                  ,ONLY:initf, parsef, evalf, EvalErrType, EvalErrMsg

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)  :: FunctionC
 CHARACTER(len=*),DIMENSION(:),INTENT(in)  :: VariableNamesCV
 REAL(rn)        ,DIMENSION(:),INTENT(in)  :: VariableValuesV
 REAL(rn)                                  :: FunctionResult         ! RESULT

 INTEGER,PARAMETER                    :: NumberOfFunctions = 1
 INTEGER,PARAMETER                    :: FunctionNumber    = 1
 LOGICAL                              :: SuccessL

 FunctionResult = 0d0

 !---------------------------------------------------------------
 ! Initialize function parser for 'NumberOfFunctions' functions.
 !---------------------------------------------------------------
 CALL initf(NumberOfFunctions)

 !----------------------------------------
 ! Parse and bytecompile function string.
 !----------------------------------------
!WRITE(my_output_unit,('(A,A)')) " Parse function = ",TRIM(FunctionC)
 CALL parsef(FunctionNumber,FunctionC,VariableNamesCV)

      !-------------------------------------------------
      ! Interprete bytecode representation of function.
      !-------------------------------------------------
      FunctionResult = evalf(FunctionNumber,VariableValuesV)

      IF (EvalErrType > 0) THEN
         WRITE(my_output_unit,*) '*** Error Evaluate_Function: ',EvalErrMsg ()
         SuccessL = .FALSE.
      ELSE
       IF (DebugLevel > 100) THEN
      !  WRITE(my_output_unit,*) " variable names  = ",VariableNamesCV
         WRITE(my_output_unit,*) " variable values = ",VariableValuesV
         WRITE(my_output_unit,*) " function = ",TRIM(FunctionC)," = "
         WRITE(my_output_unit,*) "          = ",FunctionResult
       END IF
         SuccessL = .TRUE.
      END IF

 IF (.NOT. SuccessL) THEN
    WRITE(my_output_unit,('(A,A)')) " Error Evaluate_Function: function = ",TRIM(FunctionC)
    WRITE(my_output_unit,*)         " variable names  = ",VariableNamesCV
    WRITE(my_output_unit,*)         " variable values = ",VariableValuesV
    STOP
 END IF

!------------------------------------------------------------------------------
 END FUNCTION Evaluate_Function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE Use_FunctionParser
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE input_type_names
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER,PARAMETER                    :: char_length_keyword_name   = 100     ! A keyword   can contain up to 100 characters. This should be large enough.
 INTEGER,PARAMETER                    :: char_length_specifier_name = 100     ! A specifier can contain up to 100 characters. This should be large enough.
 INTEGER,PARAMETER                    :: char_length_type_name      = 30
 INTEGER,PARAMETER                    :: char_length_required_name  = 30
 INTEGER,PARAMETER                    :: char_length_choice_name    = 600

 CHARACTER(len=*),PARAMETER :: name_xs   = 'real'                          !
 CHARACTER(len=*),PARAMETER :: name_xd   = 'double'                        !
 CHARACTER(len=*),PARAMETER :: name_in   = 'integer'                       !
 CHARACTER(len=*),PARAMETER :: name_ca   = 'character'                     !
 CHARACTER(len=*),PARAMETER :: name_lo   = 'logical'                       !
 CHARACTER(len=*),PARAMETER :: name_inar = 'integer_array'                 !
 CHARACTER(len=*),PARAMETER :: name_xsar = 'real_array'                    !
 CHARACTER(len=*),PARAMETER :: name_xdar = 'double_array'                  !

 CHARACTER(len=*),PARAMETER :: String_RequiredC    = 'required'
 CHARACTER(len=*),PARAMETER :: String_NotRequiredC = 'optional'

 CHARACTER(len=*),PARAMETER :: required_key       = String_RequiredC          ! string, to check if keyword   is required in input file
 CHARACTER(len=*),PARAMETER :: not_required_key   = String_NotRequiredC       ! string, to check if keyword   is required in input file
 CHARACTER(len=*),PARAMETER :: required_input     = String_RequiredC          ! string, to check if specifier is required in input file
 CHARACTER(len=*),PARAMETER :: not_required_input = String_NotRequiredC       ! string, to check if specifier is required in input file

 !-------------------------
 ! Example: CHOICE[yes,no]
 !-------------------------
 CHARACTER(len=*),PARAMETER :: String_ChoiceC         = 'CHOICE'              ! string to indicate choices in input file
 CHARACTER(len=*),PARAMETER :: ChoiceSeparatorC       = ','                   ! several allowed options are separated by a comma, e.g. CHOICE[yes,no,maybe]
 CHARACTER(len=*),PARAMETER :: DelimiterChoice_leftC  = '['
 CHARACTER(len=*),PARAMETER :: DelimiterChoice_rightC = ']'

 CHARACTER(len=*),PARAMETER :: QuotationMarkC = '"'             !  "
 CHARACTER(len=*),PARAMETER :: ApostropheC    = "'"             !  '

!------------------------------------------------------------------------------
 END MODULE input_type_names
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_input_data
!------------------------------------------------------------------------------
! These are helper variables used to read in data from input file.
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER,PARAMETER                         :: char_length_specifier_content = 267  ! A specifier content can contain up to 267 characters.

 TYPE  :: type_data
  INTEGER                                  :: int
  INTEGER,DIMENSION(:),POINTER             :: int_arrayV
  REAL(4)                                  :: single
  REAL(4),DIMENSION(:),POINTER             :: single_arrayV
  REAL(8)                                  :: double
  REAL(8),DIMENSION(:),POINTER             :: double_arrayV
  LOGICAL                                  :: booleanL
  CHARACTER(char_length_specifier_content) :: stringC ! to allow for long strings, e.g. long directory names
! CHARACTER(len=:),ALLOCATABLE             :: stringC ! to allow for long strings, e.g. long directory names  <= does not work
 END TYPE type_data

!------------------------------------------------------------------------------
 END MODULE mod_input_data
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE input_line_definition
!------------------------------------------------------------------------------

 IMPLICIT NONE

   TYPE :: input_lines                                                   ! define derived type for one input line
     CHARACTER(len=1),DIMENSION(:),POINTER :: text                       ! text in input line (leading and trailing blanks removed)
     INTEGER                               :: length                     ! number of characters in input line
     INTEGER                               :: number                     ! line number
   END TYPE input_lines

!------------------------------------------------------------------------------
 END MODULE input_line_definition
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE node_type_def                                                   ! define derived data type for one node in stack of input lines
!------------------------------------------------------------------------------
 USE input_line_definition,ONLY:input_lines

 IMPLICIT NONE

   TYPE :: node_type
     TYPE(input_lines), POINTER :: input_line                          ! pointer to derived data type for one input line
     TYPE(node_type)  , POINTER :: successor                           ! pointer to successor node in queue
     TYPE(node_type)  , POINTER :: previous                            ! pointer to successor node in queue
   END TYPE node_type

!------------------------------------------------------------------------------
 END MODULE node_type_def
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE queue_type_def
!------------------------------------------------------------------------------
 USE node_type_def,ONLY:node_type

 IMPLICIT NONE

   TYPE :: queue_type
     TYPE(node_type), POINTER :: top                                   !
     TYPE(node_type), POINTER :: rear                                  !
   END TYPE queue_type

!------------------------------------------------------------------------------
 END MODULE queue_type_def
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE specifier_node_def
!------------------------------------------------------------------------------
! define derived data type for one node in queue of specifiers
!------------------------------------------------------------------------------

 IMPLICIT NONE

   TYPE :: specifier_node
     CHARACTER(len=1),DIMENSION(:),POINTER :: specifier                ! pointer character array, containing a specifier
     INTEGER                               :: length                   ! number of characters in specifier name
     CHARACTER(len=1),DIMENSION(:),POINTER :: data_type                ! data type expected for a specific specifier
     INTEGER                               :: length_type              ! number of characters in name for data_type
     LOGICAL                               :: optionalL                ! store, whether specifier is optional input or not
     LOGICAL                               :: ChoiceL                  ! store, whether specific choices for this specifier are defined
     CHARACTER(len=1),DIMENSION(:),POINTER :: ChoiceC                  ! possible choices
     INTEGER                               :: length_Choice            ! number of characters contained in choice
     TYPE(specifier_node)         ,POINTER :: next_specifier           ! pointer to successor node in queue
   END TYPE specifier_node

!------------------------------------------------------------------------------
 END MODULE specifier_node_def
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE specifier_queue_def                                             ! queue type definition for specifier queue, associated to a certain keyword node
!------------------------------------------------------------------------------
 USE specifier_node_def,ONLY:specifier_node

 IMPLICIT NONE

   TYPE :: specifier_queue                                             !
     TYPE(specifier_node), POINTER :: top                              ! pointer to top  node of specifier queue
     TYPE(specifier_node), POINTER :: rear                             ! pointer to rear node of specifier queue
   END TYPE specifier_queue

!------------------------------------------------------------------------------
 END MODULE specifier_queue_def
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE keyword_node_def                                                ! define derived data type for one node in queue of keywords
!------------------------------------------------------------------------------
 USE specifier_queue_def,ONLY:specifier_queue

 IMPLICIT NONE

   TYPE :: keyword_node
     CHARACTER(len=1),DIMENSION(:),POINTER :: keyword                      ! pointer to character array, containing a keyword
     INTEGER                               :: length                       ! number of characters in keyword name
     LOGICAL                               :: required                     ! number of characters in keyword name
     TYPE(specifier_queue)        ,POINTER :: specify                      ! pointer to specifier queue, associated with a keyword node
     TYPE(keyword_node)           ,POINTER :: next_keyword                 ! pointer to successor node in keyword queue
   END TYPE keyword_node

!------------------------------------------------------------------------------
 END MODULE keyword_node_def
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE keyword_queue_def                                               ! queue type definition for keyword queue
!------------------------------------------------------------------------------
 USE keyword_node_def,ONLY:keyword_node

 IMPLICIT NONE

   TYPE :: keyword_queue
     TYPE(keyword_node), POINTER :: top                                ! pointer to top  node of keyword queue
     TYPE(keyword_node), POINTER :: rear                               ! pointer to rear node of keyword queue
   END TYPE keyword_queue

!------------------------------------------------------------------------------
 END MODULE keyword_queue_def
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_init_queue
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE init_queue (s)
!------------------------------------------------------------------------------
 USE queue_type_def,ONLY:queue_type

 IMPLICIT NONE

 TYPE(queue_type), INTENT (OUT) :: s                                 !
 NULLIFY (s%top,s%rear)                                              ! Disassociate pointer to top and rear of queue

!------------------------------------------------------------------------------
 END SUBROUTINE init_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION is_empty (s) RESULT (empty)
!------------------------------------------------------------------------------
 USE queue_type_def,ONLY:queue_type

 IMPLICIT NONE

 TYPE(queue_type), INTENT(in) :: s                                   !
 LOGICAL :: empty                                                    ! result variable
                                                                     !
 empty = .NOT. ASSOCIATED (s%top)                                    ! return .TRUE. if s%top is disassociated

!------------------------------------------------------------------------------
 END FUNCTION is_empty
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_init_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_push
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE push(s, bufferC,line_number,comment_signs_CV)
!------------------------------------------------------------------------------
! PURPOSE
!   push node onto top of queue 's'
!------------------------------------------------------------------------------
 USE input_line_definition,ONLY:input_lines
 USE queue_type_def       ,ONLY:queue_type

 IMPLICIT NONE

 TYPE(queue_type)              ,INTENT(inout) :: s
 CHARACTER(len=*)              ,INTENT(inout) :: bufferC
 INTEGER                       ,INTENT(in)    :: line_number
 CHARACTER(len=*) ,DIMENSION(:),INTENT(in)    :: comment_signs_CV

 TYPE(input_lines),POINTER        :: local_line
 INTEGER                          :: line_length,ichar
 INTEGER                          :: ipos
 LOGICAL                          :: CommentSign_foundL 


 bufferC = TRIM(ADJUSTL(bufferC))                                      ! remove leading and trailing blanks from line

 !--------------------------------------------------------------------------
 ! Scan for position of comment signs. No comment sign results in ipos = 0.
 !--------------------------------------------------------------------------
 CALL FindCommentSigns(comment_signs_CV,bufferC, CommentSign_foundL,ipos)

 IF ( CommentSign_foundL ) THEN
    !-----------------------------------------------------------
    ! Comment sign was found, ipos is position of comment sign.
    !-----------------------------------------------------------
    CALL Replace_Comment_with_blanks(ipos,bufferC)
 END IF

   bufferC = TRIM(ADJUSTL(bufferC))                                    ! remove leading and trailing blanks from line
   line_length = LEN_TRIM(ADJUSTL(bufferC))                            ! get length of input line
                                                                       !
   if (line_length == 0)                                        RETURN ! if line is empty, do not generate queue entry =========>>> EXIT POINT
                                                                       !
   ALLOCATE(local_line)                                                !
   ALLOCATE(local_line%text(line_length))                              !
   DO ichar=1,line_length                                              !
   local_line%text(ichar) = bufferC(ichar:ichar)                       ! store data in new node
   END DO                                                              !
   local_line%length = line_length                                     ! store data in new node
   local_line%number = line_number                                     ! store data in new node
                                                                       !
                                                                       !
   IF (.NOT. ASSOCIATED (s%top)) THEN                                  !
     ALLOCATE(s%top)                                                   !
     s%top%input_line => local_line                                    !
     NULLIFY(s%top%previous) ! i.e. "no previous item" for "s%top" (linked list)
     s%rear => s%top         ! Make 'rear' the new 'top'.   !
   ELSE                                                                !
     ALLOCATE(s%rear%successor)                                        !
     s%rear%successor%input_line => local_line                         !
     s%rear%successor%previous => s%rear                               !
     s%rear => s%rear%successor                                        !
   END IF                                                              !
                                                                       !
   NULLIFY(s%rear%successor)                                           !
                                                                       !
!------------------------------------------------------------------------------
 END SUBROUTINE push
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE FindCommentSigns(comment_signs_CV,stringC, FoundCommentL,position_in_string)
!------------------------------------------------------------------------------
!
!++s* mod_push/FindCommentSigns
!
! NAME
!   SUBROUTINE FindCommentSigns
!
! PURPOSE
!   This subroutine searchs for comment signs and returns the leftmost position of a comment sign in a string.
! 
! USAGE
!   CALL FindCommentSigns(comment_signs_CV,stringC, FoundCommentL,position_in_string)
! 
! INPUT
!   o comment_signs_CV: comment signs, e.g. '!', '#', '<'
!   o StringC:
!
! OUTPUT
!   o FoundCommentL:       .TRUE. if comment was found, else .FALSE.
!   o position_in_string:  smallest position of any comment sign; 0 if no comment sign was found
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*) ,DIMENSION(:),INTENT(in)  :: comment_signs_CV
 CHARACTER(len=*)              ,INTENT(in)  :: stringC
 LOGICAL                       ,INTENT(out) :: FoundCommentL
 INTEGER                       ,INTENT(out) :: position_in_string

 INTEGER                          :: i
 INTEGER                          :: position
 INTEGER                          :: position_smallest

 !--------------------------------------------------------------------------
 ! Scan for position of comment signs. No comment sign results in ipos = 0.
 !--------------------------------------------------------------------------

 FoundCommentL = .FALSE.
 position_smallest = 0  ! Initialize with zero value

 DO i=1,SIZE(comment_signs_CV)
   position = INDEX( stringC, TRIM(comment_signs_CV(i)) )
   IF ( position > 0 ) THEN
      FoundCommentL = .TRUE.
      IF (position_smallest == 0) THEN                        ! no comment sign has been found yet
          position_smallest =     position                    ! Store position of first comment sign detected
      ELSE
          position_smallest = MIN(position,position_smallest) ! Take the left most position of the two comment signs if e.g. both '!' and '#' were present.
      END IF
      CYCLE ! Search for next comment sign.
   END IF
 END DO

 position_in_string = position_smallest ! smallest position of any comment sign; 0 if no comment sign was found

!------------------------------------------------------------------------------
 END SUBROUTINE FindCommentSigns
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Replace_Comment_with_blanks(position_in_string,stringC)
!------------------------------------------------------------------------------
!
!++s* mod_push/Replace_Comment_with_blanks
!
! NAME
!   SUBROUTINE Replace_Comment_with_blanks
!
! PURPOSE
!   This subroutine replaces in a string the comment with blanks.
! 
! USAGE
!   CALL Replace_Comment_with_blanks(position_in_string,stringC)
! 
! INPUT
!   o position_in_string: position of comment sign
!   o stringC:            (also output)   Example: ' debug-level = 0   ! debug level is set to zero'
!
! OUTPUT
!   o stringC:            (also input)    Example: ' debug-level = 0                               '
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER                       ,INTENT(in)    :: position_in_string
 CHARACTER(len=*)              ,INTENT(inout) :: stringC

 INTEGER                                      :: i

 DO i=position_in_string,LEN(stringC)    ! starting at comment sign, replace all characters by blanks
      stringC(i:i) = " "                 ! replace comment text by blanks
 END DO

!------------------------------------------------------------------------------
 END SUBROUTINE Replace_Comment_with_blanks
!-----------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_push
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_get_line
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE get_next_line (s,bufferC,line_number,start_at_topL,found_endL,act_node)
!------------------------------------------------------------------------------
 USE queue_type_def,ONLY:queue_type
 USE node_type_def ,ONLY:node_type

 IMPLICIT NONE

 TYPE(queue_type),INTENT (IN OUT) :: s                               !
 CHARACTER(len=*),INTENT (OUT)    :: bufferC                         !
 INTEGER         ,INTENT (OUT)    :: line_number                     !
 LOGICAL         ,INTENT (IN OUT) :: start_at_topL                   !
 LOGICAL         ,INTENT (OUT)    :: found_endL                      !
 TYPE(node_type) ,POINTER         :: act_node                        !

 INTEGER                          :: line_length,ichar

   found_endL = .FALSE.                                                !
   IF (start_at_topL) THEN                                             !
     start_at_topL = .FALSE.                                           !
     IF(ASSOCIATED(s%top)) THEN                                        !
      act_node => s%top                                                !
     ELSE                                                              !
      found_endL = .TRUE.                                              !
     END IF                                                            !
   ELSE                                                                !
     IF(ASSOCIATED(act_node%successor)) THEN                           !
      act_node => act_node%successor                                   !
     ELSE                                                              !
      found_endL = .TRUE.                                              !
     END IF                                                            !
   END IF                                                              !
                                                                       !
   IF (.NOT. found_endL) THEN                                          !
    line_length = act_node%input_line%length                           !
    line_number = act_node%input_line%number                           !
    bufferC = ""                                                       !
    DO ichar=1,line_length                                             !
     bufferC(ichar:ichar) = act_node%input_line%text(ichar)            !
    END DO                                                             !
   ELSE                                                                !
     bufferC = ""                                                      !
   END IF                                                              !

!------------------------------------------------------------------------------
 END SUBROUTINE get_next_line
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE get_prev_line (s,bufferC,line_number,start_at_endL,found_topL,act_node)
!------------------------------------------------------------------------------
 USE queue_type_def,ONLY:queue_type
 USE node_type_def ,ONLY:node_type

 IMPLICIT NONE

 TYPE(queue_type),INTENT (IN OUT) :: s                              !
 CHARACTER(len=*),INTENT (OUT)    :: bufferC                        !
 INTEGER         ,INTENT (OUT)    :: line_number                    !
 LOGICAL         ,INTENT (IN OUT) :: start_at_endL                  !
 LOGICAL         ,INTENT (OUT)    :: found_topL                     !
 TYPE(node_type) ,POINTER         :: act_node                       !

 INTEGER                           :: line_length,ichar
                                                                       !
   found_topL = .FALSE.                                                !
   IF (start_at_endL) THEN                                             !
     start_at_endL = .FALSE.                                           !
     IF(ASSOCIATED(s%rear)) THEN                                       !
      act_node => s%rear                                               !
     ELSE                                                              !
      found_topL = .TRUE.                                              !
     END IF                                                            !
   ELSE                                                                !
     IF(ASSOCIATED(act_node%previous)) THEN                            !
      act_node => act_node%previous                                    !
     ELSE                                                              !
      found_topL = .TRUE.                                              !
     END IF                                                            !
   END IF                                                              !
                                                                       !
   IF(.NOT.found_topL)THEN                                             !
   line_length = act_node%input_line%length                            !
   line_number = act_node%input_line%number                            !
   bufferC = ""                                                         !
   DO ichar=1,line_length                                              !
   bufferC(ichar:ichar) = act_node%input_line%text(ichar)               !
   END DO                                                              !
   ELSE                                                                !
   bufferC = ""                                                         !
   END IF                                                              !

!------------------------------------------------------------------------------
 END SUBROUTINE get_prev_line
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_get_line
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_init_keyword_queue
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE init_keyword_queue (s)                                   ! Initialize an empty keyword_queue
!------------------------------------------------------------------------------
 USE keyword_queue_def,ONLY:keyword_queue

 IMPLICIT NONE

 TYPE(keyword_queue), INTENT (OUT) :: s                              !
 NULLIFY (s%top,s%rear)                                              ! Disassociate pointer to top and rear of keyword queue

!------------------------------------------------------------------------------
 END SUBROUTINE init_keyword_queue
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
END MODULE mod_init_keyword_queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_get_keyword
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE get_keyword(s,bufferC,start_at_topL,found_last_keywordL,a1)
!------------------------------------------------------------------------------
! Returns all keywords, i.e. not only 'start' but also '$end_' keywords.
! Example: 'Returns '$electric-field and '$end_electric-field'.
!------------------------------------------------------------------------------
 USE keyword_queue_def,ONLY:keyword_queue
 USE keyword_node_def ,ONLY:keyword_node

 IMPLICIT NONE

 TYPE(keyword_queue),POINTER       :: s                               !
 CHARACTER(len=*)   ,INTENT(out)   :: bufferC                          !
 LOGICAL            ,INTENT(inout) :: start_at_topL
 LOGICAL            ,INTENT(out)   :: found_last_keywordL
 TYPE(keyword_node) ,POINTER       :: a1                              !

 INTEGER                           :: line_length,ichar

 found_last_keywordL = .FALSE.
                                                                       !
 IF (start_at_topL) THEN                                             !
    a1 => s%top                                                        !
    start_at_topL = .FALSE.                                            !
 ELSE                                                                !
    a1 => a1%next_keyword                                              !
 END IF                                                              !

 line_length = a1%length                                             !
 bufferC = ""                                                         !
 DO ichar=1,line_length                                              !
   bufferC(ichar:ichar) =  a1%keyword(ichar)                            !
 END DO                                                              !

 IF( .NOT. ASSOCIATED(a1%next_keyword) ) found_last_keywordL = .TRUE.

!------------------------------------------------------------------------------
 END SUBROUTINE get_keyword
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_get_keyword
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_add_keyword
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE add_keyword(Data_len,key_char,comment_signs_CV,required_key,not_required_key, &
                                                       required_input,not_required_input,s, bufferC)
!------------------------------------------------------------------------------
 USE keyword_queue_def        ,ONLY:keyword_queue
 USE mod_push                 ,ONLY:FindCommentSigns, &
                                    Replace_Comment_with_blanks

 IMPLICIT NONE

 INTEGER                      ,INTENT(in)     :: Data_len
 CHARACTER(len=1)             ,INTENT(in)     :: key_char
 CHARACTER(len=*),DIMENSION(:),INTENT(in)     :: comment_signs_CV
 CHARACTER(len=*)             ,INTENT(in)     :: required_key
 CHARACTER(len=*)             ,INTENT(in)     :: not_required_key
 CHARACTER(len=*)             ,INTENT(in)     :: required_input
 CHARACTER(len=*)             ,INTENT(in)     :: not_required_input
 TYPE(keyword_queue)          ,INTENT(IN OUT) :: s
 CHARACTER(len=*)             ,INTENT(IN OUT) :: bufferC

 CHARACTER(len=1),DIMENSION(:),POINTER :: local_line                     !
 LOGICAL                               :: input_required                 !
 INTEGER                               :: ipos,line_length,ichar,iloc    !
 LOGICAL                               :: everything_fine                !
 INTEGER                               :: length_test
 LOGICAL                               :: CommentSign_foundL
                                                                       !
 bufferC = TRIM(ADJUSTL(bufferC))                                      ! remove leading and trailing blanks from line

 !--------------------------------------------------------------------------
 ! Scan for position of comment signs. No comment sign results in ipos = 0.
 !--------------------------------------------------------------------------
 CALL FindCommentSigns(comment_signs_CV,bufferC, CommentSign_foundL,ipos)

 IF ( CommentSign_foundL ) THEN
    !-----------------------------------------------------------
    ! Comment sign was found, ipos is position of comment sign.
    !-----------------------------------------------------------
    CALL Replace_Comment_with_blanks(ipos,bufferC)
 END IF

   bufferC = TRIM(ADJUSTL(bufferC))                                      ! remove leading and trailing blanks from line
   line_length = LEN_TRIM(bufferC)                                      ! get length of input line
                                                                       !
   IF (line_length == 0)                                        RETURN ! if line is empty, don't generate queue entry ====>>> EXIT POINT
                                                                       !
   IF (bufferC(1:1)==key_char) THEN                                     !
                                                                       !
    line_length = SCAN(bufferC," ")-1                                   ! scan for first blank after keyword (separator to "optional")
    IF(line_length<1) CALL ERROR(1)                                    ! scan for separating character after keyword
                                                                       !
    ALLOCATE(local_line(line_length))                                  !
    DO ichar=1,line_length                                             !
    local_line(ichar) = bufferC(ichar:ichar)                            ! store data in new node
    END DO                                                             !
                                                                       !
    input_required = .FALSE.                                           ! default initialization
     DO ichar=line_length+1,LEN_TRIM(bufferC)                           !
      IF(bufferC(ichar:ichar) /= " ") EXIT                              !
     END DO                                                            !
     IF(ichar > LEN_TRIM(bufferC)) CALL ERROR(2)                        ! check whether some input appears after the keyword
                                                                       !
     length_test = LEN_TRIM(bufferC)-ichar+1                            !
     length_test = length_test - LEN_TRIM(required_key)                !
     IF (length_test==0) THEN                                          !
     everything_fine = .TRUE.                                          !
     DO ipos=ichar,LEN_TRIM(bufferC)                                    !
      iloc = ipos-ichar+1                                              !
      IF(bufferC(ipos:ipos) /= required_key(iloc:iloc)) THEN            !
       everything_fine = .FALSE.                                       !
      END IF                                                           !
     END DO                                                            !
     ELSE                                                              !
       everything_fine = .FALSE.                                       !
     END IF                                                            !
     IF (everything_fine) input_required = .TRUE.                      !
                                                                       !
     IF (.NOT. everything_fine) THEN                                   !
      length_test = LEN_TRIM(bufferC)-ichar+1                           !
      length_test = length_test - LEN_TRIM(not_required_key)           !
      IF (length_test==0) THEN                                         !
       everything_fine = .TRUE.                                        !
       DO ipos=ichar,LEN_TRIM(bufferC)                                  !
        iloc = ipos-ichar+1                                            !
        IF(bufferC(ipos:ipos) /= not_required_key(iloc:iloc)) THEN      !
         everything_fine = .FALSE.                                     !
        END IF                                                         !
       END DO                                                          !
       IF (everything_fine) input_required = .FALSE.                   !
      END IF                                                           !
     END IF                                                            !
                                                                       !
     IF (.NOT. everything_fine) CALL ERROR(3)                          !
                                                                       !
   END IF                                                              ! end if its a keyword entry
                                                                       !
                                                                       !
   IF (bufferC(1:1)==key_char) THEN                                     !
     IF (.NOT. ASSOCIATED (s%top)) THEN                                !
       ALLOCATE(s%top)                                                 !
       ALLOCATE(s%top%specify)                                         !
       NULLIFY (s%top%specify%top)                                     !
       NULLIFY (s%top%specify%rear)                                    !
       s%top%keyword => local_line                                     !
       s%top%length  =  line_length                                    !
       s%top%required=  input_required                                 !
       NULLIFY(s%top%next_keyword)                                     !
       s%rear => s%top                                                 !
     ELSE                                                              !
       ALLOCATE(s%rear%next_keyword)                                   !
       ALLOCATE(s%rear%next_keyword%specify)                           !
       NULLIFY (s%rear%next_keyword%specify%top)                       !
       NULLIFY (s%rear%next_keyword%specify%rear)                      !
       s%rear%next_keyword%keyword => local_line                       !
       s%rear%next_keyword%length  =  line_length                      !
       s%rear%next_keyword%required=  input_required                   !
       NULLIFY(s%rear%next_keyword%next_keyword)                       !
       s%rear => s%rear%next_keyword                                   !
     END IF                                                            !
   ELSE                                                                !
     CALL add_specifier(bufferC,Data_len,required_input,not_required_input, s)
   END IF                                                              !
       NULLIFY(s%rear%next_keyword)                                    !


 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE ERROR(ierr)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

   integer :: ierr

   IF(ierr==1) THEN                                                    !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE add_keyword.                     '!
    WRITE(my_output_unit,*)"Couldn't find separating blank between keyword and     "!
    WRITE(my_output_unit,*)'to control strings = ', TRIM(required_key),' or ',     &
               TRIM(not_required_key),' respectively!'                 !
    WRITE(my_output_unit,*)'What I got to analyze is the following:'                !
    WRITE(my_output_unit,*) TRIM(bufferC)                                            !
    STOP                                                               !
   ELSE IF(ierr==2) THEN                                               !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE add_keyword.                     '!
    WRITE(my_output_unit,*)"Couldn't find control string after keyword             "!
    WRITE(my_output_unit,*)'Required control strings are = ', TRIM(required_key),  &
               ' or ',TRIM(not_required_key),' respectively!'          !
    WRITE(my_output_unit,*)'What I got to analyze is the following:'                !
    WRITE(my_output_unit,*) TRIM(bufferC)                                            !
    STOP                                                               !
   ELSE IF(ierr==3) THEN                                               !
    WRITE(my_output_unit,*)                                                         !
    WRITE(my_output_unit,*)'>>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
    WRITE(my_output_unit,*)'Occured in SUBROUTINE add_keyword.                     '!
    WRITE(my_output_unit,*)"Couldn't find valid control string after keyword       "!
    WRITE(my_output_unit,*)'Valid control strings are = ', TRIM(required_key),     &
               ' or ',TRIM(not_required_key),' respectively!'          !
    WRITE(my_output_unit,*)'What I got to analyze is the following:'                !
    WRITE(my_output_unit,*) TRIM(bufferC)                                            !
    STOP                                                               !
   END IF                                                              !
   END SUBROUTINE ERROR                                                !

!------------------------------------------------------------------------------
 END SUBROUTINE add_keyword
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE add_specifier(bufferC,Data_len,required_input,not_required_input, s)
!------------------------------------------------------------------------------
!
!++s* mod_add_keyword/add_specifier
!
! NAME
!   SUBROUTINE add_specifier
!
! PURPOSE
!   Add specifier and related specifier information to queue.
!
! USAGE
!   CALL add_specifier(bufferC,Data_len,required_input,not_required_input, s)
! 
! INPUT
!   o bufferC:
!   o Data_len:
!   o required_input:
!   o not_required_input:
!   o s (also output)
!
! OUTPUT
!   o s (also input)
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE keyword_queue_def        ,ONLY:keyword_queue
 USE input_type_names         ,ONLY:DelimiterChoice_leftC, &
                                    DelimiterChoice_rightC

 IMPLICIT NONE

 CHARACTER(len=*)    ,INTENT(in)       :: bufferC
 INTEGER             ,INTENT(in)       :: Data_len
 CHARACTER(len=*)    ,INTENT(in)       :: required_input
 CHARACTER(len=*)    ,INTENT(in)       :: not_required_input
 TYPE(keyword_queue) ,INTENT(inout)    :: s                           !

 CHARACTER(len=1),DIMENSION(:),POINTER :: new_line1C                  !
 CHARACTER(len=1),DIMENSION(:),POINTER :: new_line2C                  !
 CHARACTER(len=1),DIMENSION(:),POINTER :: new_line4C                  !
 CHARACTER(len=:),ALLOCATABLE          :: tempC
 CHARACTER(len=Data_len/3)             :: temp1C                      ! usually not so long
 CHARACTER(len=Data_len/3)             :: temp2C                      ! usually not so long
 CHARACTER(len=Data_len/3)             :: temp3C                      ! usually not so long
 CHARACTER(len=Data_len)               :: temp4C                      ! can be very long
 LOGICAL                               :: optional_inputL             !
 INTEGER                               :: length1,length2,length4     !
 INTEGER                               :: i
 LOGICAL                               :: ChoiceL
 LOGICAL                               :: BracketsClosedL             = .TRUE.

   tempC = bufferC

   temp1C = ''
   temp2C = ''
   temp3C = ''
   temp4C = ''

   !-----------------------------
   ! Collect data for specifier.
   !-----------------------------
   i=1
   DO 
      IF (tempC(i:i)==" ") EXIT       ! Loop until a 'blank' was found.
      temp1C(i:i) = tempC(i:i)
      tempC (i:i) = " "               ! Replace read character with blank.
      i=i+1
   END DO
   tempC = ADJUSTL(tempC)             ! Remove trailing blanks.

   !-----------------------------
   ! Collect data for data type.
   !-----------------------------
   i=1
   DO 
      IF (tempC(i:i)==" ") EXIT       ! Loop until a 'blank' was found.
      temp2C(i:i) = tempC(i:i)
      tempC (i:i) = " "               ! Replace read character with blank.
      i=i+1
   END DO
   tempC = ADJUSTL(tempC)

   !-------------------------------------
   ! Collect data for required/optional.
   !-------------------------------------
   i=1
   DO 
      IF (tempC(i:i)==" ") EXIT       ! Loop until a 'blank' was found.
      temp3C(i:i) = tempC(i:i)
      tempC(i:i)  = " "               ! Replace read character with blank.
      i=i+1
   END DO
   tempC = ADJUSTL(tempC)

   !--------------------------
   ! Collect data for choice.
   !--------------------------
   BracketsClosedL = .TRUE.
   i=1

   DO 
      !----------------------------------------------------------------------------------
      ! Loop until a 'blank' was found but take into account open brackets like "[...]".
      ! This allows blanks to be included in a CHOICE[...,1 0 0,...] option.
      !----------------------------------------------------------------------------------
      IF (tempC(i:i)==" " .AND. BracketsClosedL) EXIT

      IF (          tempC(i:i)==DelimiterChoice_leftC ) THEN
         ! WRITE(my_output_unit,*) "tempC(i:i) = ",tempC(i:i), " ==> found '['"
           BracketsClosedL = .FALSE.
      ELSE IF (     tempC(i:i)==DelimiterChoice_rightC) THEN
         ! WRITE(my_output_unit,*) "tempC(i:i) = ",tempC(i:i), " ==> found ']'"
           BracketsClosedL = .TRUE.
      ELSE
         ! WRITE(my_output_unit,*) "tempC(i:i) = ",tempC(i:i)
      END IF
      temp4C(i:i) = tempC(i:i)

      tempC(i:i)  = " "               ! Replace read character with blank.
      i=i+1
   END DO

   temp1C = TRIM(temp1C); temp1C = ADJUSTL(temp1C)
   temp2C = TRIM(temp2C); temp2C = ADJUSTL(temp2C)
   temp3C = TRIM(temp3C); temp3C = ADJUSTL(temp3C)
   temp4C = TRIM(temp4C); temp4C = ADJUSTL(temp4C)

   !------------------------------
   ! Determine required/optional.
   !------------------------------
      optional_inputL = .FALSE.
   IF     (TRIM(ADJUSTL(temp3C)) == TRIM(    required_input)) THEN
      optional_inputL = .FALSE.
   ELSE IF(TRIM(ADJUSTL(temp3C)) == TRIM(not_required_input)) THEN
      optional_inputL = .TRUE.                                         !
   ELSE                                                                !
      WRITE(my_output_unit,*) "Error add_specifier:"
      WRITE(my_output_unit,*) "Unknown control string in keyword definition file!"  !
      WRITE(my_output_unit,*) "Don't know what you mean with -> ",TRIM(ADJUSTL(temp3C))
      WRITE(my_output_unit,*) "I expected -> ",TRIM(    required_input)    !
      WRITE(my_output_unit,*) "or         -> ",TRIM(not_required_input)    !
      WRITE(my_output_unit,*) "bufferC  = ",TRIM(bufferC)
      WRITE(my_output_unit,*) "temp1C   = ",TRIM(temp1C)
      WRITE(my_output_unit,*) "temp2C   = ",TRIM(temp2C)
      WRITE(my_output_unit,*) "temp3C   = ",TRIM(temp3C)
      WRITE(my_output_unit,*) "temp4C   = ",TRIM(temp4C)
      WRITE(my_output_unit,*) "This error can happen if you work on Linux/Unix and"
      WRITE(my_output_unit,*) "if your input files (*.in) are formatted with DOS like line breaks."
      WRITE(my_output_unit,*) "Solution: Use a text editor to convert your DOS formatted"
      WRITE(my_output_unit,*) "          input files to UNIX format."
      STOP                                                             !
   END IF                                                              !
                                                                       !
   !--------------------------------------------
   ! Determine specifier, data type and choice.
   !--------------------------------------------
   temp1C = TRIM(ADJUSTL(temp1C))                                      !
   temp2C = TRIM(ADJUSTL(temp2C))                                      !
   temp4C = TRIM(ADJUSTL(temp4C))                                      !
                                                                       !
   length1 = LEN_TRIM(temp1C)                                          !
   length2 = LEN_TRIM(temp2C)                                          !
   length4 = LEN_TRIM(temp4C)                                          !

   IF ( length4 > 0 ) THEN
    !------------------------------------
    ! Check if choice entry was present.
    !------------------------------------
    ChoiceL = .TRUE.
   ELSE
    ChoiceL = .FALSE.
   END IF

   ALLOCATE(new_line1C(length1))                                       !
   ALLOCATE(new_line2C(length2))                                       !
   ALLOCATE(new_line4C(length4))                                       !

   DO i=1,length1                                                      !
    new_line1C(i) = temp1C(i:i)                                        !
   END DO                                                              !
   DO i=1,length2                                                      !
    new_line2C(i) = temp2C(i:i)                                        !
   END DO                                                              !
   DO i=1,length4                                                      !
    new_line4C(i) = temp4C(i:i)                                        !
   END DO                                                              !

                                                                       !
   IF(.NOT. ASSOCIATED (s%rear%specify%top)) THEN                      !
     ALLOCATE(s%rear%specify%top)                                      !
     s%rear%specify%top%specifier                     => new_line1C                    !
     s%rear%specify%top%length                        =  length1                       !
     s%rear%specify%top%data_type                     => new_line2C                    !
     s%rear%specify%top%length_type                   =  length2                       !
     s%rear%specify%top%optionalL                     =  optional_inputL               !
     s%rear%specify%top%ChoiceL                       =  ChoiceL
     s%rear%specify%top%ChoiceC                       => new_line4C                    !
     s%rear%specify%top%length_Choice                 =  length4                       !
     NULLIFY(s%rear%specify%top%next_specifier)
     s%rear%specify%rear => s%rear%specify%top
   ELSE
     ALLOCATE(s%rear%specify%rear%next_specifier)
     s%rear%specify%rear%next_specifier%specifier     => new_line1C
     s%rear%specify%rear%next_specifier%length        =  length1
     s%rear%specify%rear%next_specifier%data_type     => new_line2C
     s%rear%specify%rear%next_specifier%length_type   =  length2
     s%rear%specify%rear%next_specifier%optionalL     =  optional_inputL
     s%rear%specify%rear%next_specifier%ChoiceL       =  ChoiceL
     s%rear%specify%rear%next_specifier%ChoiceC       => new_line4C
     s%rear%specify%rear%next_specifier%length_Choice =  length4
     NULLIFY(s%rear%specify%rear%next_specifier%next_specifier)
     s%rear%specify%rear => s%rear%specify%rear%next_specifier
   END IF

!------------------------------------------------------------------------------
 END SUBROUTINE add_specifier
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_add_keyword
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_get_specifier
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE get_specifier(start_at_topL, specifierC,data_typeC,optional_specifierL, &
                          ChoiceL,ChoiceC,found_endL,a1)
!------------------------------------------------------------------------------
 USE keyword_node_def  ,ONLY:keyword_node
 USE specifier_node_def,ONLY:specifier_node

 IMPLICIT NONE

 LOGICAL           ,INTENT(inout)  :: start_at_topL
 CHARACTER(len=*)  ,INTENT(out)    :: specifierC                     !
 CHARACTER(len=*)  ,INTENT(out)    :: data_typeC                     !
 LOGICAL           ,INTENT(out)    :: optional_specifierL
 LOGICAL           ,INTENT(out)    :: ChoiceL
 CHARACTER(len=*)  ,INTENT(out)    :: ChoiceC
 LOGICAL           ,INTENT(out)    :: found_endL
 TYPE(keyword_node)    ,POINTER    :: a1                             !

 TYPE(specifier_node),POINTER,SAVE :: a2                             !
 INTEGER                           :: line_length,ichar
                                                                     !
 found_endL = .FALSE.                                                !

   IF (start_at_topL) THEN                                              !

     start_at_topL = .FALSE.                                            !
     IF(ASSOCIATED(a1%specify%top)) THEN                               !
      a2 => a1%specify%top                                             !
     ELSE                                                              !
      found_endL = .TRUE.                                              !
     END IF                                                            !

   ELSE                                                                !

     IF(ASSOCIATED(a2%next_specifier)) THEN                            !
      a2 => a2%next_specifier                                          !
     ELSE                                                              !
      found_endL = .TRUE.                                              !
     END IF                                                            !

   END IF                                                              !
                                                                       !
   specifierC = ""
   data_typeC = ""
   ChoiceC    = ""

   IF (.NOT. found_endL) THEN                                          !

    line_length = a2%length
    DO ichar=1,line_length
     specifierC(ichar:ichar) =  a2%specifier(ichar) ! Get specifier.
    END DO

    line_length = a2%length_type
    DO ichar=1,line_length
     data_typeC(ichar:ichar) =  a2%data_type(ichar) ! Get data type.
    END DO

    line_length = a2%length_Choice
    DO ichar=1,line_length
     ChoiceC(ichar:ichar)    =  a2%ChoiceC(ichar)   ! Get choice.
    END DO

    optional_specifierL      =  a2%optionalL
    ChoiceL                  =  a2%ChoiceL

   ELSE
    specifierC = ""
    data_typeC = ""
    ChoiceC    = ""
    optional_specifierL      =  .FALSE. 
    ChoiceL                  =  .FALSE.
   END IF

!------------------------------------------------------------------------------
 END SUBROUTINE get_specifier
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_get_specifier
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_check_keyword
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE check_keyword(keywords,keyC, found_keyL,a1)
!------------------------------------------------------------------------------
!
!++s* mod_check_keyword/check_keyword
!
! NAME
!   SUBROUTINE check_keyword
!
! PURPOSE
!   Check if keyword 'keyC' is found within list of keywords.
!
! USAGE
!   CALL check_keyword(keywords,keyC, found_keyL,a1)
! 
! INPUT
!   o keywords:
!   o keyC:
!
! OUTPUT
!   o found_keyL:
!   o a1:
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE mod_get_keyword      ,ONLY:get_keyword
 USE keyword_queue_def    ,ONLY:keyword_queue
 USE keyword_node_def     ,ONLY:keyword_node
 USE input_type_names     ,ONLY:char_length_keyword_name

 IMPLICIT NONE

 TYPE(keyword_queue),POINTER     :: keywords                          !
 CHARACTER(len=*)   ,INTENT(in)  :: keyC                               !
 LOGICAL            ,INTENT(out) :: found_keyL                         !
 TYPE(keyword_node), POINTER     :: a1                                ! Pointer to keyword node in keyword queue

 CHARACTER(len=char_length_keyword_name) :: bufferC                            !
 LOGICAL                                 :: start_at_topL,found_last_keywordL

 start_at_topL       = .TRUE.                                          !
 found_last_keywordL = .FALSE.                                         !
 found_keyL          = .FALSE.                                         !
 DO 
  IF (found_last_keywordL) EXIT
  CALL get_keyword (keywords, bufferC,start_at_topL,found_last_keywordL,a1)       !
  IF( TRIM(bufferC) == TRIM(keyC) ) found_keyL = .TRUE.                   !
  IF(found_keyL) EXIT                                                   ! at exit, a1 points to node with keyword, to be found
 END DO

 IF(.NOT.found_keyL) bufferC =""                                         !

!------------------------------------------------------------------------------
 END SUBROUTINE check_keyword
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_check_keyword
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_check_specifier
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE check_specifier(spec,Data_len, found_specL,data_typeC,optional_specifierL,ChoiceL,ChoiceC,a2)
!------------------------------------------------------------------------------
!
!++s* mod_check_specifier/check_specifier
!
! NAME
!   SUBROUTINE check_specifier
!
! PURPOSE
!   Check if specifier 'spec' is found within keyword.
!
! USAGE
!   CALL check_specifier(spec,Data_len, found_specL,data_typeC,optional_specifierL,ChoiceL,ChoiceC,a2)
! 
! INPUT
!   o spec:
!   o Data_len:
!
! OUTPUT
!   o found_specL:
!   o data_typeC:
!   o optional_specifierL:
!   o ChoiceL:
!   o ChoiceC:
!   o a2:
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE mod_get_specifier      ,ONLY:get_specifier
 USE keyword_node_def       ,ONLY:keyword_node

 IMPLICIT NONE

 CHARACTER(len=*)  ,INTENT(in)  :: spec                              !
 INTEGER           ,INTENT(in)  :: Data_len
 LOGICAL           ,INTENT(out) :: found_specL
 CHARACTER(len=*)  ,INTENT(out) :: data_typeC                        !
 LOGICAL           ,INTENT(out) :: optional_specifierL
 LOGICAL           ,INTENT(out) :: ChoiceL
 CHARACTER(len=*)  ,INTENT(out) :: ChoiceC
 TYPE(keyword_node),POINTER     :: a2                                ! Pointer to keyword node in keyword queue

 CHARACTER(Data_len)            :: specifierC
 LOGICAL                        :: start_at_topL,found_endL
                                                                       !
 start_at_topL = .TRUE.                                                !
 found_endL    = .FALSE.                                               !
 found_specL   = .FALSE.                                               !
 DO 
  IF (found_endL) EXIT
  CALL get_specifier (start_at_topL, specifierC,data_typeC,optional_specifierL,ChoiceL,ChoiceC,found_endL,a2)
  IF (found_endL) EXIT                                                 !
  IF ( TRIM(specifierC) == TRIM(spec) ) THEN
          found_specL = .TRUE.
      IF (found_specL) EXIT                                            ! at exit, a1 points to node with keyword, to be found
  END IF
 END DO

!------------------------------------------------------------------------------
 END SUBROUTINE check_specifier
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_check_specifier
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_get_key_info
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE get_key_info(keywords,keyC, spec,data_type,optional_specifierL,ChoiceL,ChoiceC,found_no_specifierL)
!------------------------------------------------------------------------------
!
!++s* mod_get_key_info/get_key_info
!
! NAME
!   SUBROUTINE get_key_info
!
! PURPOSE
!   Get information on keyword and specifier.
!
! USAGE
!   CALL get_key_info(keywords,keyC, spec,data_type,optional_specifierL,ChoiceL,ChoiceC,found_no_specifierL)
! 
! INPUT
!   o keywords:
!   o keyC:
!
! OUTPUT
!   o spec:                  Returns empty string if no specifier is present for this keyword.
!   o data_type:
!   o optional_specifierL:
!   o ChoiceL:
!   o ChoiceC:
!   o found_no_specifierL:   .TRUE. if no specifier was found for this keywords. If last specifier was found, found_no_specifierL is .FALSE.
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE mod_check_keyword      ,ONLY:check_keyword
 USE mod_get_specifier      ,ONLY:get_specifier
 USE keyword_queue_def      ,ONLY:keyword_queue
 USE keyword_node_def       ,ONLY:keyword_node

 IMPLICIT NONE

 TYPE(keyword_queue) ,POINTER     :: keywords                       !
 CHARACTER(len=*)    ,INTENT(in)  :: keyC                            !
 CHARACTER(len=*)    ,INTENT(out) :: spec                           !
 CHARACTER(len=*)    ,INTENT(out) :: data_type                      !
 LOGICAL             ,INTENT(out) :: optional_specifierL
 LOGICAL             ,INTENT(out) :: ChoiceL
 CHARACTER(len=*)    ,INTENT(out) :: ChoiceC
 LOGICAL             ,INTENT(out) :: found_no_specifierL

 TYPE(keyword_node) ,POINTER,SAVE :: a1
 LOGICAL                          :: found_keyL
 LOGICAL,SAVE                     :: start_at_topL
 LOGICAL,SAVE                     :: first_entryL = .TRUE.

 IF (first_entryL) THEN
   !----------------------------------------------------------
   ! Check if keyword 'keyC' is found within list of keywords.
   !----------------------------------------------------------
   CALL check_keyword(keywords,keyC, found_keyL,a1)
   first_entryL  = .FALSE.
   start_at_topL = .TRUE.
 END IF
   CALL get_specifier(start_at_topL, spec,data_type,optional_specifierL,ChoiceL,ChoiceC,found_no_specifierL,a1)
   IF (found_no_specifierL) first_entryL  = .TRUE.

!------------------------------------------------------------------------------
 END SUBROUTINE get_key_info
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_get_key_info
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_Get_Separation_Specifier
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Get_Separation_Specifier(keywords,keyC, &
                           SpecifierC,data_typeC_out,optional_specifierL_out,ChoiceL_out,ChoiceC_out,found_endL_out)
!------------------------------------------------------------------------------
 USE mod_check_keyword      ,ONLY:check_keyword
 USE mod_get_specifier      ,ONLY:get_specifier
 USE keyword_queue_def      ,ONLY:keyword_queue
 USE keyword_node_def       ,ONLY:keyword_node
 USE input_type_names       ,ONLY:char_length_type_name, &
                                  char_length_choice_name

 IMPLICIT NONE

 TYPE(keyword_queue),POINTER              :: keywords                       !
 CHARACTER(len=*)   ,INTENT(in)           :: keyC                           !
 CHARACTER(len=*)   ,INTENT(out)          :: SpecifierC                     !
 CHARACTER(len=*)   ,INTENT(out),OPTIONAL :: data_typeC_out                 !
 LOGICAL            ,INTENT(out),OPTIONAL :: optional_specifierL_out        !
 LOGICAL            ,INTENT(out),OPTIONAL :: ChoiceL_out
 CHARACTER(len=*)   ,INTENT(out),OPTIONAL :: ChoiceC_out
 LOGICAL            ,INTENT(out),OPTIONAL :: found_endL_out                 !

 CHARACTER(len=char_length_type_name)     :: data_typeC                     !
 LOGICAL                                  :: optional_specifierL            !
 LOGICAL                                  :: ChoiceL
 CHARACTER(len=char_length_choice_name)   :: ChoiceC
 LOGICAL                                  :: found_endL                     !

 TYPE(keyword_node),POINTER               :: a1                             !
 LOGICAL                                  :: found_keywordL                 !
 LOGICAL                                  :: start_at_topL                  !

 !-----------------------------------------------------------
 ! Check if keyword 'keyC' is found within list of keywords.
 !-----------------------------------------------------------
 CALL check_keyword(keywords,keyC, found_keywordL,a1)

 start_at_topL = .TRUE.

 CALL get_specifier(start_at_topL, SpecifierC,data_typeC,optional_specifierL,ChoiceL,ChoiceC,found_endL,a1)

 IF ( PRESENT(         data_typeC_out) )          data_typeC_out = data_typeC
 IF ( PRESENT(optional_specifierL_out) ) optional_specifierL_out = optional_specifierL
 IF ( PRESENT(            ChoiceL_out) )             ChoiceL_out = ChoiceL
 IF ( PRESENT(            ChoiceC_out) )             ChoiceC_out = ChoiceC
 IF ( PRESENT(         found_endL_out) )          found_endL_out = found_endL

!------------------------------------------------------------------------------
 END SUBROUTINE Get_Separation_Specifier
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_Get_Separation_Specifier
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_Print_Keywords_Queue
!------------------------------------------------------------------------------
 
 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Print_Keywords(kind_of_fileC,keyword_filenameC,keywords)
!------------------------------------------------------------------------------
!
!++s* mod_Print_Keywords_Queue/Print_Keywords
!
! NAME
!   SUBROUTINE Print_Keywords
!
! PURPOSE
!   This subroutine prints out 2 files.
!     keywords_nn3.xml: This file is used by nextnanomat for Auto Complete feature.
!     keywords.val:     This file is equivalent to the file keywords.val that was read in but here, it is generated automatically.
!
! USAGE
!   CALL Print_Keywords(kind_of_fileC,keyword_filenameC,keywords)
! 
! INPUT
!   o kind_of_fileC:         'inputfile', 'database'
!   o keyword_filenameC:     'keywords.val' or 'database_nn3_keywords.val'
!   o keywords:              list of keywords
!
! OUTPUT
!   none
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units   ,ONLY:my_output_unit
!USE system_specific_parser      ,ONLY:DebugLevel
 USE mod_FileExtensions_parser   ,ONLY:XML_C,ValidatorC
 USE DirectoryFileExist          ,ONLY:GetGlobalDirectoryName
 USE input_type_names            ,ONLY:char_length_type_name, &
                                       char_length_required_name, &
                                       char_length_keyword_name, &
                                       char_length_specifier_name, &
                                       char_length_choice_name, &
                                       name_xs,name_xd,name_in,name_ca,name_lo, &
                                       name_inar,name_xsar,name_xdar, &
                                       required_key  ,not_required_key, &
                                       required_input,not_required_input, &
                                       ChoiceSeparatorC
 USE mod_get_keyword             ,ONLY:get_keyword
 USE mod_get_key_info            ,ONLY:get_key_info
 USE mod_Get_Separation_Specifier,ONLY:Get_Separation_Specifier
 USE keyword_queue_def           ,ONLY:keyword_queue
 USE keyword_node_def            ,ONLY:keyword_node
 USE mod_syntax_validator        ,ONLY:InputSyntax

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)  :: kind_of_fileC
 CHARACTER(len=*)             ,INTENT(in)  :: keyword_filenameC
 TYPE(keyword_queue)          ,POINTER     :: keywords

 TYPE(keyword_node)           ,POINTER     :: a1                                ! Pointer to keyword node in keyword queue

 LOGICAL                                   :: start_at_topL,found_last_keywordL
 LOGICAL                                   :: found_no_specifierL
 LOGICAL                                   :: AtLeastOneSpecifierWasFoundL
 LOGICAL                                   :: WriteKeywordOnlyOnceL
 LOGICAL                                   :: optional_specifierL
 CHARACTER(len=char_length_type_name)      :: data_type
 CHARACTER(len=60)                         :: PropertyC
 CHARACTER(len=30)                         :: OptionalString_XML_C
 CHARACTER(len=char_length_required_name)  :: OptionalString_VAL_C
 CHARACTER(len=30)                         :: SeparatorStringC
 CHARACTER(len=char_length_keyword_name)   :: bufferC
 CHARACTER(len=char_length_keyword_name)   :: keywordC
 CHARACTER(len=char_length_keyword_name)   :: KeywordXMLC
 CHARACTER(len=char_length_specifier_name) :: specifierC
 CHARACTER(len=char_length_specifier_name) :: SpecifierXMLC
 CHARACTER(len=char_length_specifier_name) :: SeparationSpecifierC
 CHARACTER(len=:),ALLOCATABLE              :: TypeXMLC
 CHARACTER(len=char_length_choice_name),DIMENSION(:),ALLOCATABLE :: ChoiceCV
 CHARACTER(len=:),ALLOCATABLE              :: filename_XML_C
 CHARACTER(len=:),ALLOCATABLE              :: filename_VAL_C
 CHARACTER(len=:),ALLOCATABLE              :: filename_VALIDATOR_C
 INTEGER                                   :: output_unit_XML
 INTEGER                                   :: output_unit_VAL
 LOGICAL                                   :: ChoiceL
 CHARACTER(len=char_length_choice_name)    :: ChoiceC
 CHARACTER(len=char_length_choice_name)    :: ChoiceStringC
!CHARACTER(len=:),ALLOCATABLE              :: ChoiceStringC
 INTEGER                                   :: i,j
 INTEGER                                   :: number_of_choices
 CHARACTER(len=*),PARAMETER                ::  KeywordsLabelC = 'Keywords'
 CHARACTER(len=*),PARAMETER                ::  SoftwareLabelC = 'nextnano3'
 CHARACTER(len=*),PARAMETER                ::     BlockLabelC = 'block'
 CHARACTER(len=*),PARAMETER                :: AttributeLabelC = 'attribute'
 CHARACTER(len=*),PARAMETER                ::  TRUE_XML_C = 'true'
 CHARACTER(len=*),PARAMETER                :: FALSE_XML_C = 'false'
 CHARACTER(len=:),ALLOCATABLE              :: tempC
 INTEGER                                   :: relevant_length

 output_unit_XML  = 10
 output_unit_VAL  = 11

 relevant_length = LEN_TRIM(keyword_filenameC) - LEN_TRIM(ValidatorC)

 filename_VAL_C = TRIM(GetGlobalDirectoryName(''))//TRIM(keyword_filenameC)                         ! //ValidatorC ! The extension '.val' is already included.
 filename_XML_C = TRIM(GetGlobalDirectoryName(''))//TRIM(keyword_filenameC(1:relevant_length))//"_nn3"//XML_C      ! We get rid of extension '.val' here by using relevant length.
                                                                             
! PRINT *,"filename_XML_C = ",TRIM(filename_XML_C)
! PRINT *,"filename_VAL_C = ",TRIM(filename_VAL_C)
 OPEN(output_unit_XML,file = filename_XML_C)
 OPEN(output_unit_VAL,file = filename_VAL_C)

 WRITE(output_unit_XML,'(A)')    '<'//TRIM(KeywordsLabelC)//'>'
 WRITE(output_unit_XML,'(3x,A)') '<'//TRIM(SoftwareLabelC)//'>' ! Indent by 3 blanks, i.e. '3x'.
 WRITE(output_unit_XML,'(A)')    ''

 keywordC = ''
 start_at_topL       = .TRUE.
 found_last_keywordL = .FALSE.

 DO 
  IF (found_last_keywordL) EXIT
  !------------------------------------------------------
  ! Loop over all keywords until last keyword was found.
  !------------------------------------------------------
  CALL get_keyword(keywords, bufferC,start_at_topL,found_last_keywordL,a1)

  keywordC = TRIM(bufferC)

  IF (a1%required) THEN
      OptionalString_VAL_C =     required_key
      OptionalString_XML_C =    'required="'//TRIM(TRUE_XML_C)//'"'
  ELSE
      OptionalString_VAL_C = not_required_key
      OptionalString_XML_C =    'required="'//TRIM(FALSE_XML_C)//'"'
  END IF
! WRITE(my_output_unit,*) "found ",TRIM(keywordC),"   ",TRIM(OptionalString_VAL_C) ! Print keyword and 'required'/'optional' information. Also '$end_'keyword is printed, not only start keywords.

  WriteKeywordOnlyOnceL        = .TRUE.
  found_no_specifierL          = .FALSE.
  AtLeastOneSpecifierWasFoundL = .FALSE.

  !------------------------------------------------
  ! Now get separation specifier for this keyword.
  !------------------------------------------------
  CALL Get_Separation_Specifier(keywords,keywordC, SeparationSpecifierC)

  DO
   !-----------------------------------------------------------------------------------------------------------------
   ! Loop over all specifiers for this keyword.
   ! Note: The sum of all specifiers corresponding to one keyword is referred to as input sequence in the following.
   !-----------------------------------------------------------------------------------------------------------------
   CALL get_key_info(keywords,keywordC, &
                     specifierC,data_type,optional_specifierL,ChoiceL,ChoiceC,found_no_specifierL)
   ! found_no_specifierL is .TRUE. if no specifier was found for this keywords. If last specifier was found, found_no_specifierL is .FALSE.
   ! WRITE(my_output_unit,*) "Get for ",TRIM(keywordC),", spec = ",TRIM(specifierC),": end = ",found_no_specifierL

   IF (found_no_specifierL) THEN
    !----------------------------
    ! A specifier was not found.
    !----------------------------
     IF (AtLeastOneSpecifierWasFoundL) THEN
      WRITE(output_unit_XML,'(6x,A)')        '</'//TRIM(BlockLabelC)//'>'
      WRITE(output_unit_XML,'(A)')           ''
     ELSE
      WRITE(output_unit_VAL,'(A67,A12,A1)')            keywordC , OptionalString_VAL_C ,                                    '!'
      WRITE(output_unit_VAL,'(A80)')         '!------------------------------------------------------------------------------!'
    ! WRITE(output_unit_VAL,'(A)')           '                                                                                ' ! Option A): Empty line with blanks.
      WRITE(output_unit_VAL,'(A)')           ''                                                                                 ! Option B): Empty line with no characters seems more reasonable than Option A).
     END IF

     EXIT  ! If no specifier was found, we move on to the next keyword.

   ELSE
    !------------------------
    ! A specifier was found.
    !------------------------
    AtLeastOneSpecifierWasFoundL = .TRUE.

    IF (WriteKeywordOnlyOnceL) THEN
     !----------------------------------------------------------------------------
     ! Write start keyword only once at the beginning of each list of specifiers.
     !----------------------------------------------------------------------------
     KeywordXMLC = '<'//TRIM(BlockLabelC)//'   text="'//TRIM(keywordC)//'"'

      IF ( LEN(TRIM(KeywordXMLC)) > 73 ) THEN
       WRITE(my_output_unit,*) "Error Print_Keywords: Length of keyword is too long. Please adjust source code ('A73')."
       WRITE(my_output_unit,*) "KeywordXMLC = ",TRIM(KeywordXMLC)
       STOP
      END IF

     TypeXMLC = 'type="group"'
     WRITE(output_unit_XML,'(6x,A73,3x,A26,3x,A,A)') KeywordXMLC,TypeXMLC,TRIM(OptionalString_XML_C),'>'
     WRITE(output_unit_VAL,'(A80)')        '!------------------------------------------------------------------------------!'
     WRITE(output_unit_VAL,'(A67,A12,A1)')   keywordC , OptionalString_VAL_C ,                                            '!'
     WriteKeywordOnlyOnceL = .FALSE.

     !------------------------------------------------------------------------------
     ! Note: The first specifier after the starting keyword must be required input!
     ! It seems that this is NOT true!!! (CHECK: This might be okay but then adjust input parser manual online documentation.
     !------------------------------------------------------------------------------
   ! IF (optional_specifierL) THEN
   !   WRITE(my_output_unit,'(A)')   'Error Print_Keywords: '// &
   !                                 'The first specifier after the starting keyword must be required input!'
   !   WRITE(my_output_unit,'(A,A)') 'keywordC      = ',TRIM(keywordC)
   !   WRITE(my_output_unit,'(A,A)') 'specifierC    = ',TRIM(specifierC)
   !   WRITE(my_output_unit,*)       'optional_specifierL = ',optional_specifierL
   !   STOP
   !  END IF
    END IF

    IF (optional_specifierL) THEN
       OptionalString_VAL_C =  not_required_input        ! 'optional'
       OptionalString_XML_C =     'required="'//TRIM(FALSE_XML_C)//'"'
    ELSE
       OptionalString_VAL_C =      required_input        ! 'required'
       OptionalString_XML_C =     'required="'//TRIM(TRUE_XML_C)//'"'
    END IF

    !------------------------------------------------------------------------------------------------
    ! The first specifier after a keyword is a special specifier. It marks the beginning of a group.
    !------------------------------------------------------------------------------------------------
    IF ( TRIM(specifierC) == TRIM(SeparationSpecifierC) .AND. .NOT. optional_specifierL) THEN
     ! SeparatorStringC = ''
       SeparatorStringC = '   function="separator"'     ! We use 3 blanks in front of 'function'.
    ELSE
       SeparatorStringC = ''
    END IF

    PropertyC           = TRIM(OptionalString_XML_C)//TRIM(SeparatorStringC)//'>'

    ! CHECK: A type 'choice' might be interesting, i.e. all possible entries that are allowed could be provided, e.g. 'yes' or 'no'.
	!        nextnano++: <format2D type="choice" number_of_values="6">
	!                 	   AvsBinary AvsAscii VTKAscii_AVSAscii VTKAscii_AVSBinary VTKAscii Origin
	!                    </format2D>

   SpecifierXMLC = '<'//TRIM(AttributeLabelC)//'   text="'//TRIM(specifierC)//'"'

    !-----------------------------------------------------------------------
    ! Now check which data type is expected.
    ! The length of a specifier string can be very long, thus we use 'A49'.
    ! The legth '49' is actually checked further below.
    !-----------------------------------------------------------------------
    IF      ( TRIM(data_type) == TRIM(name_xs) .OR. &   ! real
              TRIM(data_type) == TRIM(name_xd) ) THEN   ! double
      TypeXMLC = 'type="real"'                          ! real
    ELSE IF ( TRIM(data_type) == TRIM(name_in)   ) THEN
      TypeXMLC = 'type="integer"'                       ! integer
    ELSE IF ( TRIM(data_type) == TRIM(name_ca)   ) THEN
      TypeXMLC = 'type="string"'                        ! character
    ELSE IF ( TRIM(data_type) == TRIM(name_lo)   ) THEN
      TypeXMLC = 'type="logical"'                       ! logical
    ELSE IF ( TRIM(data_type) == TRIM(name_inar) ) THEN
      TypeXMLC = 'type="integer_vector"'                ! integer_array
    ELSE IF ( TRIM(data_type) == TRIM(name_xsar) .OR. & ! real_array
              TRIM(data_type) == TRIM(name_xdar)) THEN  ! double_array
      TypeXMLC = 'type="real_vector"'                   ! real_array
    ELSE
      WRITE(my_output_unit,'(A,A)') "Error Print_Keywords: data_type ill-defined. data_type = ",TRIM(data_type)
      STOP
    END IF

   IF (ChoiceL) THEN
      TypeXMLC = 'type="choice"'                        ! choice
   END IF

      IF ( LEN(TRIM(SpecifierXMLC)) > 70 ) THEN
       WRITE(my_output_unit,*) "Error Print_Keywords: Length of specifier is too long. Please adjust source code ('A70')."
       WRITE(my_output_unit,*) "SpecifierXMLC = ",TRIM(SpecifierXMLC)
       STOP
      END IF
      WRITE(output_unit_XML,'(9x,A70,3x,A26,3x,A)') SpecifierXMLC,TypeXMLC,TRIM(PropertyC)

   IF (ChoiceL) THEN
      ChoiceStringC = TRIM(ChoiceC)

      !-----------------------------------------------------------------------------------------
      ! Eliminate the substrings 'CHOICE[' (begin of string) and ']' character (end of string).
      ! Example: "CHOICE[yes,no,yes[A/m^2],yes[A/cm^2]]"  (before)
      !           ==>   "yes,no,yes[A/m^2],yes[A/cm^2]"   (after)
      !-----------------------------------------------------------------------------------------
      CALL Replace_Choice_and_Delimiters(ChoiceStringC)

      !------------------------------------------------------
      ! Count number of commas (ChoiceSeparatorC) in string.
      !------------------------------------------------------
      number_of_choices = 1
      DO i=1,LEN_TRIM(ChoiceStringC)
         IF ( ChoiceStringC(i:i) == TRIM(ChoiceSeparatorC) ) number_of_choices = number_of_choices + 1
      END DO

      ALLOCATE(ChoiceCV(number_of_choices))
      ChoiceCV = ''

      number_of_choices = 1
      j=1
      DO i=1,LEN_TRIM(ChoiceStringC)
         IF ( ChoiceStringC(i:i) == TRIM(ChoiceSeparatorC) ) THEN
          number_of_choices = number_of_choices + 1
          j = 1
         ELSE
          ChoiceCV(number_of_choices)(j:j) = ChoiceStringC(i:i)
          j = j + 1
         END IF
      END DO

     DO i=1,SIZE(ChoiceCV)
      WRITE(output_unit_XML,'(12x,A,A,A)')      '<option   value="',TRIM(ChoiceCV(i)),'"></option>'
     END DO
      DEALLOCATE(ChoiceCV)
   END IF

    ! WRITE(output_unit_XML,'(8x,A)')               '</'//TRIM(AttributeLabelC)//'>'
      WRITE(output_unit_XML,'(9x,A)')               '</'//TRIM(AttributeLabelC)//'>'
    ! WRITE(output_unit_XML,'(A)')           ''

     IF (ChoiceL) THEN
      WRITE(output_unit_VAL,'(1x,A49,A17,A12,A2,A)')   specifierC,data_type,OptionalString_VAL_C,'  ',TRIM(ChoiceC)
     ELSE
      WRITE(output_unit_VAL,'(1x,A49,A17,A12,A1)')     specifierC,data_type,OptionalString_VAL_C,'!'
     END IF

   END IF  ! End: A specifier was found/not found.

  END DO   ! End: Loop over all specifiers for this keyword.

 END DO    ! End: Loop over all keywords until last keyword was found.

 WRITE(output_unit_XML,'(3x,A)') '</'//TRIM(SoftwareLabelC)//'>' ! Indent by 3 blanks, i.e. '3x'.
 WRITE(output_unit_XML,'(A)')    '</'//TRIM(KeywordsLabelC)//'>'

 CLOSE(output_unit_XML)
 CLOSE(output_unit_VAL)

 !-------------------------------------------------------------------------------
 ! Write the same information again, i.e. same information as 'output_unit_VAL'.
 ! To distinguish, we use here the file extension '.syntax'.
 !-------------------------------------------------------------------------------
 filename_VALIDATOR_C = TRIM(GetGlobalDirectoryName(''))//TRIM(keyword_filenameC)//'.syntax'
 tempC = ''
 WRITE(my_output_unit,'(A)') ' Write file: '//filename_VALIDATOR_C
 CALL InputSyntax(kind_of_fileC,.FALSE.,'write-to-file', tempC, filenameC = filename_VALIDATOR_C)

!------------------------------------------------------------------------------
 END SUBROUTINE Print_Keywords
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Replace_Choice_and_Delimiters(choicesC)
!------------------------------------------------------------------------------
! Eliminate the substrings 'CHOICE[' (begin of string) and ']' character (end of string).
! Example: "CHOICE[yes,no,yes[A/m^2],yes[A/cm^2]]"  (before)
!           ==>   "yes,no,yes[A/m^2],yes[A/cm^2]"   (after)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
!USE system_specific_parser   ,ONLY:DebugLevel
 USE CharacterManipulation    ,ONLY:CharacterReplace
 USE mod_chrpak               ,ONLY:StringReplace
 USE input_type_names         ,ONLY:String_ChoiceC       , &
                                    DelimiterChoice_leftC, &
                                    DelimiterChoice_rightC

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(inout) :: choicesC

 INTEGER                        :: ii

 ! IF (DebugLevel > 500) THEN
 !  WRITE(my_output_unit,*) " choicesC = ",TRIM(choicesC)
 ! END IF

 ! Special example: Here, several brackets of type '[' and ']' could be included: CHOICE[test,yes,no,yes[A/m^2],yes[A/cm^2],yes[A/m],yes[A/cm],yes[A]]
 CALL StringReplace   (choicesC , TRIM(String_ChoiceC)//TRIM(DelimiterChoice_leftC)  , '')  ! Replace 'CHOICE[' entirely by shifting remaining string to the left.
 
 ii = LEN_TRIM(choicesC) ! Determine length of string ''.
 ! IF (DebugLevel > 500) THEN
 !  WRITE(my_output_unit,*) " choicesC = ",TRIM(choicesC),"ii = ",ii
 ! END IF

 !--------------------------------------------------------------------
 ! Assume that last character is identical to DelimiterChoice_rightC.
 !--------------------------------------------------------------------
 IF ( choicesC(ii:ii) /= TRIM(DelimiterChoice_rightC) ) THEN
    WRITE(my_output_unit,'(A,A)') " Error Replace_Choice_and_Delimiters: Last character of string must be ", &
                                    TRIM(DelimiterChoice_rightC)
    WRITE(my_output_unit,*)       " choicesC        = ",TRIM(choicesC),"ii = ",ii
    WRITE(my_output_unit,*)       " choicesC(ii:ii) = ",     choicesC(ii:ii)
    STOP
 END IF

 CALL CharacterReplace(choicesC(ii:ii) , TRIM(DelimiterChoice_rightC) , ' ')  ! Replace ']' with blank character.  <== Replaces only the last ']' in case ']' occurs several times.

 choicesC = TRIM( ADJUSTL(choicesC) )

 ! IF (DebugLevel > 500) THEN
 !  WRITE(my_output_unit,*) " choicesC = ",TRIM(choicesC)
 ! END IF

!------------------------------------------------------------------------------
 END SUBROUTINE Replace_Choice_and_Delimiters
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_Print_Keywords_Queue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_keyword_queue_built_up
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 FUNCTION keyword_queue_empty (s) RESULT (empty) 
!------------------------------------------------------------------------------
! CHECK: This function is not used at all.
!------------------------------------------------------------------------------
 USE keyword_queue_def,ONLY:keyword_queue

 IMPLICIT NONE
                                                                       !
 LOGICAL :: empty                                                      ! result variable
 TYPE(keyword_queue), INTENT (IN) :: s                                 !
                                                                       !
 empty = .NOT. ASSOCIATED (s%top)                                      ! return .TRUE. if s%top is disassociated
                                                                       !
!------------------------------------------------------------------------------
 END FUNCTION keyword_queue_empty
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE keyword_queue_built_up(type_of_fileC,keyword_filenameC,Use_Keywords_Validator_FileL, &
                                   Data_len,key_char,comment_signs_CV,required_key,not_required_key, &
                                   required_input,not_required_input, queue_1)
!------------------------------------------------------------------------------
!
!++s* mod_keyword_queue_built_up/keyword_queue_built_up
!
! NAME
!   SUBROUTINE keyword_queue_built_up
!
! PURPOSE
!   This subroutine generates the queue of keywords, specifiers, ...,
!   i.e. it reads
!     o Option A): the file        of the the syntax definition (old version until 2021).
!     o Option B): the source code of the the syntax definition (old version from 2021).
! 
! USAGE
!   CALL keyword_queue_built_up(type_of_fileC,keyword_filenameC,Use_Keywords_Validator_FileL, &
!                               Data_len,key_char,comment_signs_CV,required_key,not_required_key, &
!                               required_input,not_required_input, queue_1)
! 
! INPUT
!   o type_of_fileC:                  'inputfile', 'database'
!   o keyword_filenameC:
!   o Use_Keywords_Validator_FileL:   If .TRUE., then the syntax definition is read from a file (Option A) rather than taken from the source code (Option B).
!   o Data_len:         
!   o key_char:         
!   o comment_sign:     
!   o required_key:
!   o not_required_key:
!   o required_input:
!   o not_required_input:
!
! OUTPUT
!   o queue_1
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units      ,ONLY:my_output_unit
 USE system_specific_parser         ,ONLY:DebugLevel
 USE mod_SyntaxFolder               ,ONLY:GetFilenameIncludingSyntaxFolder
 USE DirectoryFileExist             ,ONLY:FileExistREAD
 USE CharacterManipulation          ,ONLY:CountCharacters
 USE mod_syntax_validator           ,ONLY:InputSyntax
 USE keyword_queue_def              ,ONLY:keyword_queue
 USE mod_init_keyword_queue         ,ONLY:init_keyword_queue

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)  :: type_of_fileC
 CHARACTER(len=*)             ,INTENT(in)  :: keyword_filenameC
 LOGICAL                      ,INTENT(in)  :: Use_Keywords_Validator_FileL
 INTEGER                      ,INTENT(in)  :: Data_len
 CHARACTER(len=1)             ,INTENT(in)  :: key_char
 CHARACTER(len=*),DIMENSION(:),INTENT(in)  :: comment_signs_CV
 CHARACTER(len=*)             ,INTENT(in)  :: required_key
 CHARACTER(len=*)             ,INTENT(in)  :: not_required_key
 CHARACTER(len=*)             ,INTENT(in)  :: required_input
 CHARACTER(len=*)             ,INTENT(in)  :: not_required_input
 TYPE(keyword_queue)                       :: queue_1

 INTEGER                                   :: ios
 INTEGER                                   :: line_number
 INTEGER                                   :: number_of_lines
 INTEGER                                   :: i
 INTEGER                                   :: ii
 CHARACTER(Data_len)                       :: bufferC
 CHARACTER(len=:),ALLOCATABLE              :: filenameC
 CHARACTER(len=:),ALLOCATABLE              :: filecontentC
 LOGICAL                                   :: NEW_LINE_foundL
 LOGICAL                                   :: DebugL
 INTEGER                                   :: StringLength

 IF ( DebugLevel == 10000 ) THEN
      DebugL = .TRUE.
 ELSE
      DebugL = .FALSE.
 END IF

 !----------------------------------------------------------
 ! Disassociate pointer 'queue_1' to top and rear of queue.
 !----------------------------------------------------------
 CALL init_keyword_queue (queue_1)                                   ! create a new queue
 !--------------------------------------------------------------------
 ! 'queue_1%top' and 'queue_1%rear' are now nullified.
 !--------------------------------------------------------------------

 line_number = 0

 IF ( Use_Keywords_Validator_FileL ) THEN
    !------------------------------------------------
    ! Option A): Read syntax definition from a file.
    !------------------------------------------------

    filenameC = GetFilenameIncludingSyntaxFolder('',keyword_filenameC)

    WRITE(my_output_unit,'(A)') ""
    WRITE(my_output_unit,'(A)') " Reading in syntax validator file: "//TRIM(keyword_filenameC)

    CALL FileExistREAD(filenameC)

    OPEN(10,STATUS='OLD',file=filenameC)                                 ! Open file.

    DO                                                                   ! Loop over every line of syntax definition file.
     READ (10,"(A)",IOSTAT = ios) bufferC                                ! Store line.
     IF (ios < 0) EXIT                                                   ! If end of file, exit loop.
     CALL AddKeyword(bufferC)                                            ! Add keyword.
    END DO

    CLOSE(10)                                                            ! Close file.

 ELSE
    !---------------------------------------------------------
    ! Option B): Read syntax definition from the source code.
    !---------------------------------------------------------

    filecontentC = '' ! allocates string so that it is at least allocated when calling InputSyntax_Database.

    CALL InputSyntax(type_of_fileC,DebugL,'syntax-definition', filecontentC)

    !-----------------------------------------------------------------------
    ! There will be a problem if the last line does not contain a new line.
    ! So we check this here.
    !-----------------------------------------------------------------------
    StringLength = LEN_TRIM( filecontentC )
    IF ( filecontentC(StringLength:StringLength) /= NEW_LINE('n') ) THEN
       WRITE(my_output_unit,'(A)') " Error keyword_queue_built_up: Line must end with a new line symbol."
       STOP
    END IF

    !------------------------------------------------------------------------------------------
    ! Count number of lines in string, i.e. count number of "new line" entries in this string.
    !------------------------------------------------------------------------------------------
    number_of_lines = CountCharacters(filecontentC,NEW_LINE('n'))
 
    IF (DebugL) THEN
       !------------------------------------------------------
       ! Print content of this string for debugging purposes.
       !------------------------------------------------------
       WRITE(my_output_unit,'(A)')    "================================================================================"
       WRITE(my_output_unit,'(A)')    " Syntax definition file = "//TRIM(keyword_filenameC)
       WRITE(my_output_unit,'(A)')    "================================================================================"
       WRITE(my_output_unit,'(A)')     filecontentC
       WRITE(my_output_unit,'(A,I8)') " number of lines = ",number_of_lines
       WRITE(my_output_unit,'(A)')    "================================================================================"
    END IF

    ii      = 0
    bufferC = ''

    DO i=1,LEN(filecontentC)                                             ! Loop over all characters in string.

      NEW_LINE_foundL = .FALSE.

      IF ( filecontentC(i:i) /= NEW_LINE('n') ) THEN                     ! Only loop until line break is found.
       ii = ii + 1
       bufferC(ii:ii) = filecontentC(i:i)                                ! Store line, i.e. all characters before line break
      ELSE
       NEW_LINE_foundL = .TRUE.
      END IF


      IF ( NEW_LINE_foundL ) THEN
       CALL AddKeyword(bufferC)                                          ! Add keyword.
       !----------------------------------------
       ! Reset buffer and ii for each new line.
       !----------------------------------------
       ii      = 0
       bufferC = ''
       NEW_LINE_foundL = .FALSE.
     END IF

    END DO

 END IF

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE AddKeyword(bufferC)
!------------------------------------------------------------------------------
 USE mod_add_keyword                ,ONLY:add_keyword

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(inout) :: bufferC

 line_number = line_number + 1  ! Track line of current bufferC (line_number is not used)

 CALL add_keyword(Data_len,key_char,comment_signs_CV,required_key,not_required_key, &
                  required_input,not_required_input,queue_1,bufferC) ! push node onto top of queue_1

!------------------------------------------------------------------------------
 END SUBROUTINE AddKeyword
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE keyword_queue_built_up
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_keyword_queue_built_up
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE Parser_Tools
!------------------------------------------------------------------------------
!
!++m* parser.f90/Parser_Tools
!
! NAME 
!   MODULE Parser_Tools
!
! CONTAINS
!   o FUNCTION is_yes
!   o FUNCTION is_no
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
 FUNCTION is_yes(StringC)
!------------------------------------------------------------------------------
!
!++f* Parser_Tools/is_yes
!
! NAME
!   FUNCTION is_yes
!
! PURPOSE
!   This function checks if a string equals 'yes'.
!   This function is not case sensitive.
! 
! USAGE
!   is_yes(StringC)
! 
! INPUT
!   o StringC:
!
! OUTPUT
!   o is_yes:     .TRUE. if 'yes', else .FALSE. 
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE String_Utility           ,ONLY:StringLowerCase

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)          :: StringC
 LOGICAL                              :: is_yes   ! RESULT

 IF ( StringLowerCase( TRIM(StringC) ) == 'yes' ) THEN
      is_yes = .TRUE.
 ELSE
      is_yes = .FALSE.
 END IF

!------------------------------------------------------------------------------
 END FUNCTION is_yes
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION is_no(StringC)
!------------------------------------------------------------------------------
!
!++f* Parser_Tools/is_no
!
! NAME
!   FUNCTION is_no
!
! PURPOSE
!   This function checks if a string equals 'no'.
!   This function is not case sensitive.
! 
! USAGE
!   is_no(StringC)
! 
! INPUT
!   o StringC:
!
! OUTPUT
!   o is_no:     .TRUE. if 'no', else .FALSE. 
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE String_Utility           ,ONLY:StringLowerCase

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)          :: StringC
 LOGICAL                              :: is_no   ! RESULT

 IF ( StringLowerCase( TRIM(StringC) ) == 'no' ) THEN
      is_no = .TRUE.
 ELSE
      is_no = .FALSE.
 END IF

!------------------------------------------------------------------------------
 END FUNCTION is_no
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE Parser_Tools
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE Parser_Errors
!------------------------------------------------------------------------------
!
!++m* parser.f90/Parser_Errors
!
! NAME 
!   MODULE Parser_Errors
!
! CONTAINS
!   o SUBROUTINE Print_Keyword_Specifier_Line
!   o SUBROUTINE STOP_UnexpectedNumbers
!   o SUBROUTINE STOP_UnexpectedNumbers_modulo
!   o SUBROUTINE STOP_Yes_or_No_is_required
!   o SUBROUTINE Error_WrongValue
!   o SUBROUTINE Error_WrongEntry
!   o SUBROUTINE Error_MissingEntry
!   o SUBROUTINE Error_MultipleEntries
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

 PRIVATE STOP_UnexpectedNumbers_integer
 PRIVATE STOP_UnexpectedNumbers_real

 INTERFACE STOP_UnexpectedNumbers
     MODULE PROCEDURE STOP_UnexpectedNumbers_integer
     MODULE PROCEDURE STOP_UnexpectedNumbers_real
 END INTERFACE

 INTERFACE Error_SmallerThanZero
     MODULE PROCEDURE  Error_SmallerThanZero_integer
     MODULE PROCEDURE  Error_SmallerThanZero_real
 END INTERFACE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/Print_Keyword_Specifier_Line
!
! NAME
!   SUBROUTINE Print_Keyword_Specifier_Line
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
! 
! USAGE
!   CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L)
! 
! INPUT
!   o keywordC:                   keyword
!   o specifierC:                 specifier
!   o line:        (optional)     If present, the line number will be printed.
!   o STOP_L:      (optional)     If present and .TRUE., the STOP statement will be executed.
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

 CHARACTER(len=*),INTENT(in)          :: keywordC
 CHARACTER(len=*),INTENT(in)          :: specifierC
 INTEGER         ,INTENT(in),OPTIONAL :: line
 LOGICAL         ,INTENT(in),OPTIONAL :: STOP_L

  WRITE(my_output_unit,'(A,A)')   "  keyword   = " ,TRIM(keywordC)
  WRITE(my_output_unit,'(A,A,A)') "  specifier =  ",TRIM(specifierC)," = ..."
 IF ( PRESENT(line) ) THEN
  WRITE(my_output_unit,'(A,I10)') "  line      = " ,line
 END IF

 IF ( PRESENT(STOP_L) ) THEN
         IF ( STOP_L ) STOP ! Stop execution of code.
 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE Print_Keyword_Specifier_Line
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE STOP_UnexpectedNumbers_integer(keywordC,specifierC,line,ExpectedNumber,ArrayV)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/STOP_UnexpectedNumbers_integer
!
! NAME
!   SUBROUTINE STOP_UnexpectedNumbers_integer
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
! 
! USAGE
!   CALL STOP_UnexpectedNumbers_integer(keywordC,specifierC,line,ExpectedNumber,ArrayV)
! 
! INPUT
!   o keywordC:                   keyword
!   o specifierC:                 specifier
!   o line:                       line
!   o ExpectedNumber:             expected number
!   o ArrayV:
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

 CHARACTER(len=*)    ,INTENT(in)          :: keywordC
 CHARACTER(len=*)    ,INTENT(in)          :: specifierC
 INTEGER             ,INTENT(in)          :: line
 INTEGER             ,INTENT(in)          :: ExpectedNumber
 INTEGER,DIMENSION(:),INTENT(in)          :: ArrayV

 WRITE(my_output_unit,*) " Unexpected number of values for given specifier."
 WRITE(my_output_unit,*) " You specified ",SIZE(ArrayV)," values."
 WRITE(my_output_unit,*) ArrayV
 WRITE(my_output_unit,*) " However, I need exactly ",ExpectedNumber," numbers - nothing else!"

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE STOP_UnexpectedNumbers_integer
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE STOP_UnexpectedNumbers_real(keywordC,specifierC,line,ExpectedNumber,ArrayV)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/STOP_UnexpectedNumbers_real
!
! NAME
!   SUBROUTINE STOP_UnexpectedNumbers_real
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
! 
! USAGE
!   CALL STOP_UnexpectedNumbers_real(keywordC,specifierC,line,ExpectedNumber,ArrayV)
! 
! INPUT
!   o keywordC:                   keyword
!   o specifierC:                 specifier
!   o line:                       line
!   o ExpectedNumber:             expected number
!   o ArrayV:
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

 CHARACTER(len=*)    ,INTENT(in)          :: keywordC
 CHARACTER(len=*)    ,INTENT(in)          :: specifierC
 INTEGER             ,INTENT(in)          :: line
 INTEGER             ,INTENT(in)          :: ExpectedNumber
 REAL(8),DIMENSION(:),INTENT(in)          :: ArrayV

 WRITE(my_output_unit,*) " Unexpected number of values for given specifier."
 WRITE(my_output_unit,*) " You specified ",SIZE(ArrayV)," values."
 WRITE(my_output_unit,*) ArrayV
 WRITE(my_output_unit,*) " However, I need exactly ",ExpectedNumber," numbers - nothing else!"

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE STOP_UnexpectedNumbers_real
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE STOP_UnexpectedNumbers_modulo(keywordC,specifierC,line,ExpectedModuloNumber,ArrayV)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/STOP_UnexpectedNumbers_modulo
!
! NAME
!   SUBROUTINE STOP_UnexpectedNumbers_modulo
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
!   Here the error is an unexpected size of the array.
! 
! USAGE
!   CALL STOP_UnexpectedNumbers_modulo(keywordC,specifierC,line,ExpectedModuloNumber,ArrayV)
! 
! INPUT
!   o keywordC:                   keyword
!   o specifierC:                 specifier
!   o line:                       line
!   o ExpectedModuloNumber:       expected modulo number
!   o ArrayV:
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

 CHARACTER(len=*)    ,INTENT(in)          :: keywordC
 CHARACTER(len=*)    ,INTENT(in)          :: specifierC
 INTEGER             ,INTENT(in)          :: line
 INTEGER             ,INTENT(in)          :: ExpectedModuloNumber
 REAL(8),DIMENSION(:),INTENT(in)          :: ArrayV

 WRITE(my_output_unit,*) " Unexpected number of values for given specifier."
 WRITE(my_output_unit,*) " You specified ",SIZE(ArrayV)," values."
 WRITE(my_output_unit,*) ArrayV
 WRITE(my_output_unit,*) " However, the expected number must be a multiple of ",ExpectedModuloNumber,"."

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE STOP_UnexpectedNumbers_modulo
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE STOP_Yes_or_No_is_required(keywordC,specifierC,stringC,line,FurtherOptionsCV)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/STOP_Yes_or_No_is_required
!
! NAME
!   SUBROUTINE STOP_Yes_or_No_is_required
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
!   Here, the error is a wrong string value provided.
!   The expected string value is either 'yes' or 'no',
!   or if the optional argument FurtherOptionsCV is present,
!   then further options are allowed.
! 
! USAGE
!   CALL STOP_Yes_or_No_is_required(keywordC,specifierC,stringC,line,FurtherOptionsCV)
! 
! INPUT
!   o keywordC:                     keyword
!   o specifierC:                   specifier
!   o stringC:                      string
!   o line:                         line
!   o FurtherOptionsCV: (optional)  further strings that are possible
!
! OUTPUT
!   none
! 
! NOTES
!   Each string contained in FurtherOptionsCV must have the same string length when using the
!   array constructor ['...','...'].
!   Example:
!     CALL STOP_Yes_or_No_is_required(keyword,specifier,caval,line,['donor   ', &
!                                                                   'acceptor'])
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)          :: keywordC
 CHARACTER(len=*)             ,INTENT(in)          :: specifierC
 CHARACTER(len=*)             ,INTENT(in)          :: stringC
 INTEGER                      ,INTENT(in)          :: line
 CHARACTER(len=*),DIMENSION(:),INTENT(in),OPTIONAL :: FurtherOptionsCV

 INTEGER                                           :: i

 IF ( PRESENT(FurtherOptionsCV) ) THEN
   WRITE(my_output_unit,'(A)')     " Input for this flag should be either"
   WRITE(my_output_unit,'(A)')     "    'yes'"
   WRITE(my_output_unit,'(A)')     "    'no'"
  DO i=1,SIZE(FurtherOptionsCV)
   WRITE(my_output_unit,'(A,A,A)') "    '",TRIM(FurtherOptionsCV(i)),"'"
  END DO
 ELSE
   WRITE(my_output_unit,'(A)')     " Input for this flag should be either 'yes' or 'no'."
 END IF

   WRITE(my_output_unit,'(A,A)')   " Your wrong entry was: ",TRIM(stringC)

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE STOP_Yes_or_No_is_required
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Error_SmallerThanZero_integer(keywordC,specifierC,line,value,FileTypeC)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/Error_SmallerThanZero
!
! NAME
!   SUBROUTINE Error_SmallerThanZero
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
!   Here, the error is a value that is outside the bounds,
!   i.e. x < 0 but only x >= 0 is allowed.
! 
! USAGE
!   CALL Error_SmallerThanZero(keywordC,specifierC,line,value,FileTypeC)
! 
! INPUT
!   o keywordC:                      keyword
!   o specifierC:                    specifier
!   o line:                          line
!   o value:                         value
!   o FileTypeC:                     'input' or 'database' file
!
! OUTPUT
!   none
! 
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)          :: keywordC
 CHARACTER(len=*)             ,INTENT(in)          :: specifierC
 INTEGER                      ,INTENT(in)          :: line
 INTEGER                      ,INTENT(in)          :: value
 CHARACTER(len=*)             ,INTENT(in),OPTIONAL :: FileTypeC

 IF ( PRESENT(FileTypeC) ) THEN
  WRITE(my_output_unit,'(A)')    " ERROR detected in "//TRIM(FileTypeC)//" file."       ! input file or database file
 ELSE
  WRITE(my_output_unit,'(A)')    " ERROR detected in "//"input"        //" file."       ! input file or database file
 END IF
 
  WRITE(my_output_unit,'(A)')        " A positive number >= 0 is expected."
  WRITE(my_output_unit,*)            " You specified: ",TRIM(specifierC)," = ",value

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE Error_SmallerThanZero_integer
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Error_SmallerThanZero_real(keywordC,specifierC,line,value,FileTypeC)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/Error_SmallerThanZero
!
! NAME
!   SUBROUTINE Error_SmallerThanZero
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
!   Here, the error is a value that is outside the bounds,
!   i.e. x < 0 but only x >= 0 is allowed.
! 
! USAGE
!   CALL Error_SmallerThanZero(keywordC,specifierC,line,value,FileTypeC)
! 
! INPUT
!   o keywordC:                      keyword
!   o specifierC:                    specifier
!   o line:                          line
!   o value:                         value
!   o FileTypeC:                     'input' or 'database' file
!
! OUTPUT
!   none
! 
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)          :: keywordC
 CHARACTER(len=*)             ,INTENT(in)          :: specifierC
 INTEGER                      ,INTENT(in)          :: line
 REAL(8)                      ,INTENT(in)          :: value
 CHARACTER(len=*)             ,INTENT(in),OPTIONAL :: FileTypeC

 IF ( PRESENT(FileTypeC) ) THEN
  WRITE(my_output_unit,'(A)')    " ERROR detected in "//TRIM(FileTypeC)//" file."       ! input file or database file
 ELSE
  WRITE(my_output_unit,'(A)')    " ERROR detected in "//"input"        //" file."       ! input file or database file
 END IF
 
  WRITE(my_output_unit,'(A)')        " A positive number >= 0 is expected."
  WRITE(my_output_unit,*)            " You specified: ",TRIM(specifierC)," = ",value

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE Error_SmallerThanZero_real
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Error_WrongValue(keywordC,specifierC,line,FileTypeC,integer_value,AllowedValuesV)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/Error_WrongValue
!
! NAME
!   SUBROUTINE Error_WrongValue
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
!   Here, the error is an integer value that is outside the bounds.
! 
! USAGE
!   CALL Error_WrongValue(keywordC,specifierC,line,FileTypeC,integer_value,AllowedValuesV)
! 
! INPUT
!   o keywordC:                      keyword
!   o specifierC:                    specifier
!   o line:                          line
!   o FileTypeC:                     'input' or 'database' file
!   o integer_value:                 integer_value
!   o AllowedValuesV:                allowed integer values
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

 CHARACTER(len=*)             ,INTENT(in)          :: keywordC
 CHARACTER(len=*)             ,INTENT(in)          :: specifierC
 INTEGER                      ,INTENT(in)          :: line
 CHARACTER(len=*)             ,INTENT(in)          :: FileTypeC
 INTEGER                      ,INTENT(in)          :: integer_value
 INTEGER,DIMENSION(:)         ,INTENT(in)          :: AllowedValuesV

 WRITE(my_output_unit,'(A,A,A)')    " ERROR detected in ",TRIM(FileTypeC)," file."       ! input file or database file
 WRITE(my_output_unit,'(A)')        " The entered value is not within the range of allowed values."
 WRITE(my_output_unit,'(A,A,A,I8)') " You specified: ",TRIM(specifierC)," = ",integer_value
 WRITE(my_output_unit,*)            " However, the allowed values are: ",AllowedValuesV

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE Error_WrongValue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Error_WrongEntry(keywordC,specifierC,line,WrongStringC)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/Error_WrongEntry
!
! NAME
!   SUBROUTINE Error_WrongEntry
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
!   Here, the error is a string value that is unexpected.
! 
! USAGE
!   CALL Error_WrongEntry(keywordC,specifierC,line,WrongStringC)
! 
! INPUT
!   o keywordC:                      keyword
!   o specifierC:                    specifier
!   o line:                          line
!   o WrongStringC:                  wrong string entry
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

 CHARACTER(len=*)             ,INTENT(in)          :: keywordC
 CHARACTER(len=*)             ,INTENT(in)          :: specifierC
 INTEGER                      ,INTENT(in)          :: line
 CHARACTER(len=*)             ,INTENT(in)          :: WrongStringC

 WRITE(my_output_unit,'(A,A,A,A)') " You specified: ",TRIM(specifierC)," = ",TRIM(WrongStringC)
 WRITE(my_output_unit,'(A,A)')     " Your wrong entry was: ",WrongStringC

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE Error_WrongEntry
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Error_MissingEntry(keywordC,specifierC,FileTypeC,stringC,FurtherCommentsCV)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/Error_MissingEntry
!
! NAME
!   SUBROUTINE Error_MissingEntry
!
! PURPOSE
!   This subroutine prints out the keyword and specifier of the input file
!   where an error was encountered.
!   Here, the error is a missing specifier which is required.
!   If the optional argument FurtherCommentsCV is present,
!   then further comments are printed out.
! 
! USAGE
!   CALL Error_MissingEntry(keywordC,specifierC,FileTypeC,stringC,FurtherCommentsCV)
! 
! INPUT
!   o keywordC:                      keyword
!   o specifierC:                    specifier
!   o FileTypeC:                     'input' or 'database' file
!   o stringC:                       string
!   o FurtherCommentsCV: (optional)  further comments
!
! OUTPUT
!   none
! 
! NOTES
!   If a specifier is missing, then the information in which line the specifier was not found is meaningless.
!   Thus we do not use the line as argument.
!   Each string contained in FurtherCommentsCV must have the same string length when using the
!   array constructor ['...','...'].
!
!   Example:
!     CALL Error_MissingEntry(keyword,specifier,FileTypeC,stringC,['Short Comment ', &
!                                                                  'Longer Comment'])
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)          :: keywordC
 CHARACTER(len=*)             ,INTENT(in)          :: specifierC
 CHARACTER(len=*)             ,INTENT(in)          :: FileTypeC
 CHARACTER(len=*)             ,INTENT(in)          :: stringC
 CHARACTER(len=*),DIMENSION(:),INTENT(in),OPTIONAL :: FurtherCommentsCV

 INTEGER                                           :: i

   WRITE(my_output_unit,'(A,A,A)') " ERROR detected in ",TRIM(stringC),":"                      ! e.g. 'ERROR in SUBROUTINE Test'
   WRITE(my_output_unit,'(A,A,A)') " There is a missing entry in the ",TRIM(FileTypeC)," file." ! input file or database file

 IF ( PRESENT(FurtherCommentsCV) ) THEN
  DO i=1,SIZE(FurtherCommentsCV)
   WRITE(my_output_unit,'(A)') TRIM(FurtherCommentsCV(i))
  END DO
 END IF

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE Error_MissingEntry
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Error_MultipleEntries(keywordC,specifierC,line,FileTypeC,stringC,FurtherCommentsCV)
!------------------------------------------------------------------------------
!
!++s* Parser_Errors/Error_MultipleEntries
!
! NAME
!   SUBROUTINE Error_MultipleEntries
!
! PURPOSE
!   This subroutine prints out the keyword, specifier and related line of the input file
!   where an error was encountered.
!   Here, the error is a multiple entry of a specifier where only one unique entry is allowed.
!   If the optional argument FurtherCommentsCV is present,
!   then further comments are printed out.
! 
! USAGE
!   CALL Error_MultipleEntries(keywordC,specifierC,line,FileTypeC,stringC,FurtherCommentsCV)
! 
! INPUT
!   o keywordC:                      keyword
!   o specifierC:                    specifier
!   o line:                          line
!   o FileTypeC:                     'input' or 'database' file
!   o stringC:                       string
!   o FurtherCommentsCV: (optional)  further comments
!
! OUTPUT
!   none
! 
! NOTES
!   Each string contained in FurtherCommentsCV must have the same string length when using the
!   array constructor ['...','...'].
!
!   Example:
!     CALL Error_MultipleEntries(keyword,specifier,FileTypeC,stringC,['Long Comment ', &
!                                                                     'Short Comment'])
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)          :: keywordC
 CHARACTER(len=*)             ,INTENT(in)          :: specifierC
 INTEGER                      ,INTENT(in)          :: line
 CHARACTER(len=*)             ,INTENT(in)          :: FileTypeC
 CHARACTER(len=*)             ,INTENT(in)          :: stringC
 CHARACTER(len=*),DIMENSION(:),INTENT(in),OPTIONAL :: FurtherCommentsCV

 INTEGER                                           :: i

   WRITE(my_output_unit,'(A,A,A)') " ERROR detected in ",TRIM(stringC),":"                        ! e.g. 'ERROR in SUBROUTINE Test'
   WRITE(my_output_unit,'(A)')     " There is a multiple entry for a specifier where only one unique entry is allowed."
   WRITE(my_output_unit,'(A,A,A)') " Only one entry should be in ",TRIM(FileTypeC)," file."       ! input file or database file

 IF ( PRESENT(FurtherCommentsCV) ) THEN
  DO i=1,SIZE(FurtherCommentsCV)
   WRITE(my_output_unit,'(A)') TRIM(FurtherCommentsCV(i))
  END DO
 END IF

 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE Error_MultipleEntries
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE Parser_Errors
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE MacroForInputFile
!------------------------------------------------------------------------------
!
!++m* parser.f90/MacroForInputFile
!
! NAME 
!   MODULE MacroForInputFile
!
! FILENAME
!   input_parser/parser.f90
!
! CONTAINS
!   o subroutine ApplyMacro
!   o subroutine Write_Variables_to_File
!   o subroutine CountGetVariables
!   o subroutine ReplaceVariables
!   o subroutine EvaluateFunction
!   o subroutine ReplaceVariableWithValue
!   o subroutine Treat_IfStatement_and_Comment
!   o subroutine Check_if_VariableValue_is_TRUE
!   o subroutine Check_if_variable_name_is_unique
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!-----------------------------------------------------------------------
 SUBROUTINE ApplyMacro(SpecialMacroCharacterC,CommentSigns_CV,IF_STATEMENT_CV,DebugLevel,NumberOfLines,max_string_length, &
                       StringsV,MacroActiveL)
!-----------------------------------------------------------------------
!
!++s* MacroForInputFile/ApplyMacro
!
! NAME
!   SUBROUTINE ApplyMacro
!
! PURPOSE
!   Implementation of macro in input file.
!   Looks for a special text string ('macro variable definition and value assignment')
!   in an array of strings (StringsV, e.g. obtained from an ASCII file),
!   and replaces all other occurences of this macro variable (including multiple entries in one line / one string).
!   For an example, see further below.
!
! USAGE
!   CALL ApplyMacro(SpecialMacroCharacterC,CommentSigns_CV,IF_STATEMENT_CV,NumberOfLines,max_string_length,StringsV,MacroActiveL)
! 
! INPUT
!   o SpecialMacroCharacterC:  e.g. '%' for '%width'
!   o CommentSigns_CV:         e.g. '!' for nextnano3
!   o IF_STATEMENT_CV:         e.g. '!IF'
!   o NumberOfLines:           usually equal to SIZE(StringsV)
!   o max_string_length:       length of characters of longest line (i.e. string)
!   o StringsV  (also output): Array of strings. If a macro variable is contained in this string, it has to be replaced.
!
! OUTPUT
!   o StringsV  (also input)
!   o MacroActiveL: .TRUE. if macro has been executed, else .FALSE.
! 
! NOTES
!   Examples:
!
!     %width = 10d0          ! width in 10 nm
!     ==> Replaces all variables %width with "10d0".
!
!     %material = GaAs       ! material name
!     ==> Replaces all variables %material with "GaAs".
!
!     %string = '10d0 20d0'  !
!     ==> Replaces all variables %string with "10d0 20d0".
!
!##
!
!-----------------------------------------------------------------------
 use My_Input_and_Output_Units, only: my_output_unit
 use mod_int_to_char999       , only: int_2_char ! converts integer to character
 use mod_Brackets             , only: CompletelyEnclosed
 use CharacterManipulation    , only: CharacterReplace
 use mod_chrpak               , only: StringReplace
 use mod_Array_of_Strings     , only: String_in_Line
 use String_Utility           , only: StringLowerCase, &
                                      StringUpperCase
 use input_type_names         , only: QuotationMarkC, &
                                      ApostropheC

 IMPLICIT NONE

 CHARACTER(len=*)                 ,INTENT(in)    :: SpecialMacroCharacterC
 CHARACTER(len=*),DIMENSION(:)    ,INTENT(in)    :: CommentSigns_CV
 CHARACTER(len=*),DIMENSION(:)    ,INTENT(in)    :: IF_STATEMENT_CV
 INTEGER                          ,INTENT(in)    :: DebugLevel
 INTEGER                          ,INTENT(in)    :: NumberOfLines
 INTEGER                          ,INTENT(in)    :: max_string_length
 TYPE(String_in_Line),DIMENSION(:),INTENT(inout) :: StringsV
 LOGICAL                          ,INTENT(out)   :: MacroActiveL

 INTEGER                                               :: i
 INTEGER                                               :: NumberOfVariables
 INTEGER                     ,DIMENSION(:),ALLOCATABLE :: Line_of_VariableDefinitionV
 CHARACTER(max_string_length),DIMENSION(:),ALLOCATABLE :: VariableNameCV
 CHARACTER(len=:)                         ,ALLOCATABLE :: VariableNameC
 CHARACTER(max_string_length),DIMENSION(:),ALLOCATABLE :: VariableValueCV
 CHARACTER(len=:)                         ,ALLOCATABLE :: VariableValueC
 LOGICAL                                               :: EvaluateVariableL
 INTEGER                                               :: EvaluateVariableLowerBound
!INTEGER                                               :: EvaluateVariableUpperBound

 CHARACTER(max_string_length),DIMENSION(:),ALLOCATABLE :: functionCV
!CHARACTER(len=14)           ,DIMENSION(:),ALLOCATABLE :: FunctionResultCV           ! must be at least 14 characters long
 CHARACTER(len=17)           ,DIMENSION(:),ALLOCATABLE :: FunctionResultCV           ! must be at least 17 characters long
 LOGICAL                     ,DIMENSION(:),ALLOCATABLE :: Real8FunctionSuccessLV
 LOGICAL                                               :: ReturnIntegerL
!CHARACTER(len=14)           ,DIMENSION(1)             :: temp_FunctionResultCV      ! must be at least 14 characters long
 CHARACTER(len=17)           ,DIMENSION(1)             :: temp_FunctionResultCV      ! must be at least 17 characters long
 LOGICAL                     ,DIMENSION(1)             :: temp_Real8FunctionSuccessLV

 INTEGER                                               :: NumberOfFunctions
 INTEGER                                               :: ifunc
 CHARACTER(len=10)                                     :: CounterC
 LOGICAL                                               :: TreatFunctionAsStringL
 INTEGER                                               :: length_of_variable_value
 INTEGER                                               :: position_INT
 INTEGER                                               :: position_bracket
 INTEGER                                               :: ii
 INTEGER                                               :: m,n
 LOGICAL                                               :: FoundBracketFollowing_INT_L

 IF ( LEN(SpecialMacroCharacterC) /= 1 ) THEN
    WRITE(my_output_unit,'(A)')   " Error ApplyMacro: Special character is expected to consist only of one character."
    WRITE(my_output_unit,'(A,A)') " LEN(SpecialMacroCharacterC) = ",LEN(SpecialMacroCharacterC)
    WRITE(my_output_unit,'(A,A)') "     SpecialMacroCharacterC  = ",    SpecialMacroCharacterC
    STOP
 END IF

 !----------------------------------
 ! 1) Count variables.
 !----------------------------------
 CALL CountGetVariables('count',NumberOfLines,SpecialMacroCharacterC,CommentSigns_CV,StringsV, &
                                NumberOfVariables) ! Note: 'NumberOfVariables' is output.

 if (NumberOfVariables > 0) then
   MacroActiveL = .TRUE.
 else
   MacroActiveL = .FALSE.
 end if

 MACRO_ACTIVE: if (MacroActiveL) then

  WRITE(my_output_unit,'(A,I5)') " Macro status: Macro definition found in input file. # variables defined = ",NumberOfVariables
  WRITE(my_output_unit,'(A)')    " "

  ! %variable_name = variable_value
  ALLOCATE(VariableNameCV             (NumberOfVariables)) ! store the name  of the variable: variable_name
  ALLOCATE(VariableValueCV            (NumberOfVariables)) ! store the value of the variable: variable_value
  ALLOCATE(Line_of_VariableDefinitionV(NumberOfVariables)) ! store the line where the definition of the variable name occurs

  !----------------------------------
  ! 2) Get variables and its values.
  !    e.g. %variable = <value>
  !----------------------------------
  CALL CountGetVariables('get'  ,NumberOfLines,SpecialMacroCharacterC,CommentSigns_CV,StringsV, &
                                 NumberOfVariables,VariableNameCV,VariableValueCV,Line_of_VariableDefinitionV) ! Note: 'NumberOfVariables' is input.
  WRITE(my_output_unit,'(A)')    " "

  !----------------------------------------------------------------------------------
  ! Now check if there are any variables that should be evaluated (function parser).
  !----------------------------------------------------------------------------------

  EvaluateVariableL = .FALSE.
  EvaluateVariableLowerBound = NumberOfVariables + 1 ! Initialize variable so that it is out of range.
  VARIABLES: do i = 1, NumberOfVariables

      !!!   CHECK: It might be useful to have '$FunctionParser = no' to avoid variables to be evaluated, e.g. '%DOMAIN-coordinates = 0d0 5d0'.
      !!!          Here, the minus sign and the '=' sign are interpreted as equations.
      !!!   RESTRICTION: A minus sign in a variable name is NOT allowed, such as '%DOMAIN-coordinates'. Use '%DOMAIN_coordinates' instead.
      !!!   IF ( TRIM(VariableNameCV (i)) == '%FunctionParser' .AND. &
      !!!        TRIM(VariableValueCV(i)) == 'no') THEN

         !--------------------------------------------------------------------------------------------------------------
         ! Search for '%FunctionParser = yes' in input file and detect which variable number this is (Determine ==> i).
         ! All variables < i are not evaluated in the following.
         ! All variables > i are     evaluated in the following. Variable #i = '%FunctionParser = yes'.
         !--------------------------------------------------------------------------------------------------------------
         IF ( StringUpperCase( TRIM(VariableNameCV (i)) ) == '%FUNCTIONPARSER' .AND. &
              StringLowerCase( TRIM(VariableValueCV(i)) ) == 'yes') THEN
            !--------------------------------------------------------------
            ! This means that all variables with index >= i are evaluated.
            ! All other are treated as strings.
            !--------------------------------------------------------------
            EvaluateVariableLowerBound = i + 1 ! 'i+1' because the i'th function '%FunctionParser = yes' does not have to be evaluated.
            !---------------------------------------------------------
            ! Check if there is in input file: '%FunctionParser = yes'
            !---------------------------------------------------------
            EvaluateVariableL = .TRUE.
            EXIT
          END IF
  
  end do VARIABLES

  EVALUATE_VARIABLE: if (EvaluateVariableL) then
   !-------------------------------------------------------
   ! Now evaluate variables if they consist of a function.
   !-------------------------------------------------------

!  IF (DebugLevel > 3) THEN
    WRITE(my_output_unit,'(A,A,A,A,A)') " Macro function parser: The following variables are evaluated: (", &
                             int_2_char(EvaluateVariableLowerBound),",...,",int_2_char(NumberOfVariables),")"
!  END IF

    !------------------------------------------------
    ! Calculate number of functions to be evaluated.
    !------------------------------------------------
    NumberOfFunctions = NumberOfVariables - ( EvaluateVariableLowerBound - 1 )

    ALLOCATE(functionCV            (NumberOfFunctions))
    ALLOCATE(FunctionResultCV      (NumberOfFunctions))
    ALLOCATE(Real8FunctionSuccessLV(NumberOfFunctions))
             Real8FunctionSuccessLV = .FALSE.

    ifunc = 0

   LOOP_EVALUATE_VARIABLE: do i = EvaluateVariableLowerBound, NumberOfVariables
    !------------------------------------------------
    ! Here, this loop evaluates all variables first.
    !------------------------------------------------

    CounterC = int_2_char(i)
    IF (DebugLevel > 3) THEN
     WRITE(my_output_unit,'(A)')     " "
     WRITE(my_output_unit,'(A,A,A,I8,A)') " Evaluate variable #",TRIM(CounterC)," of ",NumberOfVariables,":"
    END IF

    ifunc = ifunc + 1

    !-------------------------------------------------------------------
    ! Check if macro variable should be treated as string.
    ! This is the case if it is within quotation marks or apostrophes.
    !-------------------------------------------------------------------
    !------------------------------------------
    ! Get rid of leading and trailing blanks.
    !------------------------------------------
    VariableNameC  = TRIM( ADJUSTL(VariableNameCV(i))  )
    VariableValueC = TRIM( ADJUSTL(VariableValueCV(i)) )
    length_of_variable_value = LEN(VariableValueC)

  !  IF ( LEN(VariableValueC) == 0 ) THEN ! Check empty string.
  !   TreatFunctionAsStringL = .TRUE.
    !----------------------------------------------------------------
    ! Now we have to get rid of '"' or "'" in case they are present.
    ! We check if first and last character correspond to '"' or "'".
    !----------------------------------------------------------------
    IF      (VariableValueC(1:1) == '"' .AND. VariableValueC(length_of_variable_value:length_of_variable_value) == '"') THEN
  ! ELSE IF (VariableValueC(1:1) == '"' .AND. VariableValueC(length_of_variable_value:length_of_variable_value) == '"') THEN
             !-----------------------------------------------------
             ! Replace in string 'VariableValueC' all '"' with ' ' (blank).
             !-----------------------------------------------------
             ! CALL CharacterReplace (VariableValueC,      '"'       , ' ' )
               CALL CharacterReplace (VariableValueC, QuotationMarkC , ' ' )
     TreatFunctionAsStringL = .TRUE.
    ELSE IF (VariableValueC(1:1) == "'" .AND. VariableValueC(length_of_variable_value:length_of_variable_value) == "'") THEN
             !-----------------------------------------------------
             ! Replace in string 'VariableValueC' all '"' with ' ' (blank).
             !-----------------------------------------------------
             ! CALL CharacterReplace (VariableValueC,      "'"       , ' ' )
               CALL CharacterReplace (VariableValueC, ApostropheC    , ' ' )
     TreatFunctionAsStringL = .TRUE.
    ELSE
     TreatFunctionAsStringL = .FALSE.
    END IF

   TREAT_FUNCTION_AS_STRING: if (TreatFunctionAsStringL) then
        !-------------------------------
        ! Replace variable with string.
        !-------------------------------
        VariableValueCV(i) =  TRIM( ADJUSTL(VariableValueC) )
   else
    
    !-------------------------------------------------------------------
    ! Check if macro variable should be treated as an integer variable.
    !-------------------------------------------------------------------
    ReturnIntegerL = .FALSE.
     
    !-------------------------------------------------------------------------------------------------------------
    ! Possibility a)
    ! ==> Take into account 'INT(' in variable value.
    !                                          -----
    ! This is similar as having a function 'int' such as 'exp' or 'abs'.
    ! It is not allowed to have several occurences of INT.
    ! It is only allowed to have  'INT('  in the beginning and  ')'  at the end of the string.
    !
    ! Attention: If a variable name is called 'point', the letters 'int' are contained in 'point' as a substring.
    !-------------------------------------------------------------------------------------------------------------

    !-----------------------------------------
    ! Test if number of brackets makes sense.
    !-----------------------------------------
    IF ( .NOT. CompletelyEnclosed(VariableValueC,'()') ) THEN
       WRITE(my_output_unit,'(A)')       " Error ApplyMacro: The number of brackets seems to be odd. It must be even."
       WRITE(my_output_unit,'(A,A)')     "                   variable value = ",TRIM(VariableValueC)
       STOP
    END IF
    
    position_INT = INDEX ( StringUpperCase(VariableValueC) , 'INT' )
    IF      ( position_INT == 0 ) THEN ! This is the default case, i.e. 'INT' does not occur in variable value.
       CONTINUE
    ELSE IF ( position_INT == 1 ) THEN ! Here, 'INT' is at the beginning of the string.
       !---------------------------------------------------------------------------------------------------------
       ! The variable value starts with 'INT...'. This could be 'INTERBAND' or 'INT(...)'.
       ! In order to consider 'INT' as an expression, we require that 'INT' has to be followed by a bracket '('.
       ! There could be blanks between 'INT' and '(' but obviously no other character is allowed.
       ! Let's check if the next (nonblank) character following 'INT' is a '('.
       ! Example: %nodes = INT  ( 10 / 3 )   , i.e. with a 'blank' between 'INT' and '('
       !---------------------------------------------------------------------------------------------------------
       FoundBracketFollowing_INT_L = .FALSE.
       ii = position_INT + LEN('INT') ! Start from character following 'INT'.
       DO n=ii,LEN(VariableValueC)
         IF      ( VariableValueC(n:n) == ' ') THEN ! A blank is allowed.
          CONTINUE
         ELSE IF ( VariableValueC(n:n) == '(') THEN ! Search for '('.
          FoundBracketFollowing_INT_L = .TRUE.
          position_bracket = n
          !-----------------------------
          ! Replace 'INT(' with blanks.
          !-----------------------------
          DO m=position_INT,position_bracket
             VariableValueC(m:m) = ' '
          END DO
          EXIT ! Exit do loop.
         ELSE                                       ! Exit if any other character has been found.
          FoundBracketFollowing_INT_L = .FALSE.
          EXIT ! Exit do loop.
         END IF
       END DO

       IF (FoundBracketFollowing_INT_L) THEN
        CALL StringReplace (    VariableValueC(LEN_TRIM(VariableValueC): &
                                               LEN_TRIM(VariableValueC)), ')' , ' ' )  ! Replace last bracket in string, assuming that the last character is a ')'.
       END IF

      !-----------------------------------------
      ! Test if number of brackets makes sense.
      !-----------------------------------------
      IF ( .NOT. CompletelyEnclosed(VariableValueC,'()') ) THEN
       WRITE(my_output_unit,'(A)')       " Error ApplyMacro: The number of brackets seems to be odd. It must be even."
       WRITE(my_output_unit,'(A,A)')     "                   variable name  = ",TRIM(VariableNameC)
       WRITE(my_output_unit,'(A,A)')     "                   variable value = ",TRIM(VariableValueC)
       STOP
      END IF

      IF (FoundBracketFollowing_INT_L) THEN
       ReturnIntegerL = .TRUE.
      END IF

    ELSE ! 'INT' has been found elsewehere in variable value.
         !-------------------------------------------------------------------------------------------------------------------------------
         ! Be careful! The string '%point' includes 'int' as a substring. In this case, this is not meant to be an integer conversion!!!
         !-------------------------------------------------------------------------------------------------------------------------------
     CONTINUE
    END IF

         
    !----------------------------------------------------------------------------------
    ! Possibility b)
    ! INTEGER is the case, if the macro variable name starts with '%INT('.
    ! e.g. %INT(nodes) = 10 / 5   (or)
    !      %INT(nodes) = nodes
    ! This is the old implementation which is deprecated because it is not intuitive.
    !---------------------------------------------------------------------------------
    !-------------------------------------------------
    ! ==> Take into account '%INT(' in variable name.
    !                                           ----
    !-------------------------------------------------
    IF      ( INDEX ( StringUpperCase(VariableNameC) , 'INT(' ) == 2 ) THEN ! e.g. %INT(nodes) = 10 / 3   or 
                                                                            ! e.g. %INT(nodes) = %nodes
     ReturnIntegerL = .TRUE.
    END IF 
    
    !-------------------------------------------------------------------------
    ! The function should have the expression as defined in 'VariableValueC'.
    !-------------------------------------------------------------------------
    functionCV(ifunc) =  TRIM( ADJUSTL(VariableValueC) )

    CALL EvaluateFunction([functionCV(ifunc)],[ReturnIntegerL],VariableNameCV,VariableValueCV,DebugLevel, &
                          temp_FunctionResultCV,temp_Real8FunctionSuccessLV)
        Real8FunctionSuccessLV(ifunc) = temp_Real8FunctionSuccessLV(1)
    REAL8: if (Real8FunctionSuccessLV(ifunc)) then
        
        FunctionResultCV(ifunc) = temp_FunctionResultCV(1)

        IF (.NOT. ReturnIntegerL) THEN
           !-----------------------------------------------------------
           ! Here, we want to add 'e0' (exponent) to the real variable
           ! if it does not yet contain 'e', 'E', 'd' or 'D'.
           ! In case it is an integer variable, we leave it as it is.
           !-----------------------------------------------------------
           IF ( SCAN ( FunctionResultCV(ifunc) , 'eEdD') == 0 ) THEN      ! The FUNCTION SCAN scans a string for any character in a set of characters.
            FunctionResultCV(ifunc) = TRIM(FunctionResultCV(ifunc))//'e0'
           END IF
        END IF
        
        !-----------------------------------------
        ! Replace variable with evaluated result.
        !-----------------------------------------
        VariableValueCV(i) =  TRIM(FunctionResultCV(ifunc))
        WRITE(my_output_unit,'(A,A,A,A,A,A,A,A,A)') &
        " variable #",TRIM(CounterC),": ",TRIM(VariableNameCV(i)) ," = ",TRIM(functionCV(ifunc)) ," = ", &
                                          TRIM(VariableValueCV(i))," (evaluated and replaced)"
    else
      IF (DebugLevel > 3) THEN
        WRITE(my_output_unit,'(A,A,A,A,A,A,A)') &
        " variable #",TRIM(CounterC),": ",TRIM(VariableNameCV(i))," = ",TRIM(functionCV(ifunc)) ," (not evaluated)"
      END IF
    end if REAL8
    
   end if TREAT_FUNCTION_AS_STRING

   end do LOOP_EVALUATE_VARIABLE

    DEALLOCATE(functionCV)
    DEALLOCATE(FunctionResultCV)
    DEALLOCATE(Real8FunctionSuccessLV)
    
  else
   WRITE(my_output_unit,'(A)') " Macro function parser: No variables are evaluated because '%FunctionParser = yes'"// &
                               " was not present."
  end if EVALUATE_VARIABLE

  !-----------------------------------------------------------------------------------
  ! 3) Replace variables either with its value, or with its evaluated function value.
  !-----------------------------------------------------------------------------------
  CALL ReplaceVariables(NumberOfVariables,VariableNameCV,VariableValueCV,Line_of_VariableDefinitionV,CommentSigns_CV, &
                        IF_STATEMENT_CV, &
                        StringsV)
 
  WRITE(my_output_unit,'(A)') " "

  !---------------------------------------------------
  ! Write all variables and their values into a file.
  !---------------------------------------------------
  call Write_Variables_to_File( VariableNameCV, VariableValueCV )

  DEALLOCATE(VariableNameCV)
  DEALLOCATE(VariableValueCV)
  DEALLOCATE(Line_of_VariableDefinitionV)

 end if MACRO_ACTIVE

!------------------------------------------------------------------------------
 END SUBROUTINE ApplyMacro
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 subroutine Write_Variables_to_File( VariableNameCV, VariableValueCV )
!------------------------------------------------------------------------------
! PURPOSE
!   Write all variables and their values into a file.
!------------------------------------------------------------------------------
 use My_Input_and_Output_Units, only: my_output_unit
 use Filenames_mod_parser     , only: FILENAME_VARIABLES_C
 use DirectoryFileExist       , only: SetGlobalDirectoryName

 implicit none

 character(len=*), dimension(:), intent(in)  :: VariableNameCV
 character(len=*), dimension(:), intent(in)  :: VariableValueCV

 integer                                     :: i

 if ( size( VariableNameCV ) /= size( VariableValueCV ) ) then
    write(my_output_unit, '(A)')    "Error Write_Variables_to_File: size( VariableNameCV ) /= size( VariableValueCV )"
    write(my_output_unit, '(A,I5)') "                               size( VariableNameCV )  = ", size( VariableNameCV )
    write(my_output_unit, '(A,I5)') "Error Write_Variables_to_File: size( VariableValueCV ) = ", size( VariableValueCV )
    stop
 end if

 !------------------------------------------------------------------------------------------------------
 ! We use SetGlobalDirectoryName() to make sure that the directory is created if it does not exist yet.
 !------------------------------------------------------------------------------------------------------
 open( UNIT = 12, FILE = TRIM( SetGlobalDirectoryName('') ) // FILENAME_VARIABLES_C )

  !-----------------------------------------------------------------------------------
  ! Loop over the variables that are before the definition of 'FunctionParser = yes'.
  !-----------------------------------------------------------------------------------
  do i = 1, size( VariableNameCV )
    !---------------------------------------------------
    ! Write all variables and their values into a file.
    !---------------------------------------------------
    write( UNIT = 12, FMT = '(A,A,A)' ) trim( VariableNameCV(i) )," = ",trim( VariableValueCV(i) )
  end do

 close( UNIT = 12 )

!------------------------------------------------------------------------------
 end subroutine Write_Variables_to_File
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE CountGetVariables(actionC,NumberOfLines,SpecialMacroCharacterC,CommentSigns_CV,StringsV, &
                              NumberOfVariables, &
                              VariableNameCV   , &
                              VariableValueCV  , &
                              Line_of_VariableDefinitionV)
!------------------------------------------------------------------------------
!
!++s* MacroForInputFile/CountGetVariables
!
! NAME
!   SUBROUTINE CountGetVariables
!
! PURPOSE
!   Implementation of macro in input file.
!   Looks for a special text string ('macro variable definition and value assignment')
!   in an array of strings (StringsV, e.g. obtained from an ASCII file) and returns 
!     - the variable name or
!     - the variable value.
!   For an example, see further below.
!
! USAGE
!   CALL CountGetVariables(actionC,NumberOfLines,SpecialMacroCharacterC,CommentSigns_CV,StringsV, NumberOfVariables, VariableNameCV,VariableValueCV,Line_of_VariableDefinitionV)
! 
! INPUT
!   o actionC = 'count':   Count variables only.
!               'get':     Get variables: Assign all variables and its values.
!   o NumberOfLines
!   o SpecialMacroCharacterC
!   o CommentSigns_CV:
!   o StringsV
!   o NumberOfVariables       (can be also output)
!
! OUTPUT
!   o NumberOfVariables       (can be also input)
!   o VariableNameCV                               (optional)
!   o VariableValueCV                              (optional)
!   o Line_of_VariableDefinitionV                  (optional)
! 
! NOTES
!   SYNTAX
!
!   A variable name starts with a '%' symbol and is followed by one or several characters, digits or symbols,
!   e.g. %Variable_Name (Example: %Temperature).
!   A variable name is allowed to contain the substring of another variable, e.g.
!     - "%x   = 1"
!     - "%x1  = 2"   ! (contains %x)
!     - "%x11 = 3"   ! (contains %x1)
!   A variable name is not allowed to contain the '!' sign. (The reason is nextnano3 specific because everything that is followed by the '!' symbol is treated as a comment.)
!   A variable name is not allowed to contain the '-' sign. (The reason is nextnano3 specific, see explanation further below.)
!   A variable name is case sensitive.
!
!   A variable assignment is indicated with an '=' symbol.       (Example: %Temperature = 10.5)
!   A necessary requirement for a variable assignment is that
!     - the '%' sign is the first nonblank character in this line AND that
!     - the variable name is followed by an '=' symbol.
!   For that reason, there can only be one variable assignment in each line, i.e. "%x = 5.0    %y = 3.0" is not allowed. (CHECK: Can we generalize this?)
!
!   A variable assignment is allowed to include blanks, e.g. 
!     - "%growth_direction = 1 0 0" (CHECK: Maybe we should change this to ... = '1 0 0' or ... = "1 0 0"?)
!     - "%region_1 = region-number = 1  base-geometry = line  region-priority = 1"
!     - "%conduction_band_masses   =  0.067  0.067  0.067"
!  CHECK: How shall we treat here quotation marks or apostrophes? ('"' or "'")
!  Option 1: Replace them with a blank.
!  Option 2: Keep them.
!
!  It is not allowed to assign a variable twice. (CHECK: Maybe this statement has to be relaxed, e.g. nextnano++ supports:
!    %y = 4.0
!    %y = sqrt(%y)*%x
!
!   Variable replacement: A variable is replaced if
!     - the '%' symbol is not followed by an "=" symbol.
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_int_to_char999       ,ONLY:int_2_char ! converts integer to character
 USE mod_Array_of_Strings     ,ONLY:String_in_Line
 USE mod_push                 ,ONLY:FindCommentSigns, &
                                    Replace_Comment_with_blanks

 IMPLICIT NONE

 CHARACTER(len=*)                 ,INTENT(in)             :: actionC
 INTEGER                          ,INTENT(in)             :: NumberOfLines
 CHARACTER(len=*)                 ,INTENT(in)             :: SpecialMacroCharacterC
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)             :: CommentSigns_CV
 TYPE(String_in_Line),DIMENSION(:),INTENT(in)             :: StringsV
 INTEGER                          ,INTENT(inout)          :: NumberOfVariables
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(out)  ,OPTIONAL :: VariableNameCV
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(out)  ,OPTIONAL :: VariableValueCV
 INTEGER             ,DIMENSION(:),INTENT(out)  ,OPTIONAL :: Line_of_VariableDefinitionV

 CHARACTER(LEN=:),ALLOCATABLE                             :: LineC                   ! String of a particular line in input file without leading and trailing blanks.

 INTEGER                                                  :: line_count
 INTEGER                                                  :: variable_counter
 INTEGER                                                  :: i
 LOGICAL                                                  :: CommentSign_foundL

 INTEGER                                                  :: position
 INTEGER                                                  :: position_minus

 SELECT CASE ( TRIM(actionC) )
  CASE('count')
   NumberOfVariables = 0                  ! Initialize default value.
 END SELECT

 variable_counter = 0                     ! By default, zero variables are defined.
 line_count     = 1                       ! 'line_count' points to current line

 DO 
  IF ( line_count > NumberOfLines ) EXIT ! 'NumberOfLines' is total number of lines in strings to be examined.

  !---------------------------------------------------------------------------------------------------------
  ! ADJUSTL: Adjusts a character string to the left, removing leading blanks and inserting trailing blanks.
  !---------------------------------------------------------------------------------------------------------
  LineC = ADJUSTL( StringsV(line_count)%StringC ) ! StringsV(line_count)%StringC is line number 'line_count'. Remove leading blanks.

  !---------------------------------------------------------------------------
  ! TRIM:    Returns the argument with trailing blanks removed.
  ! ==> Use 'TRIM(LineC)' instead of 'LineC'. TRIM() removes trailing blanks.
  !---------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------
  ! LEN_TRIM: Returns the length of the character argument without counting trailing blank characters.
  !----------------------------------------------------------------------------------------------------
  IF ( LEN_TRIM(LineC) > 3 ) THEN  ! At least 4 characters are needed, e.g. "%x=5".

    !-----------------------------------------------------------------------------------------------------------
    ! Check if first character is '%'. This is a requirement for a variable definition and variable assignment.
    !-----------------------------------------------------------------------------------------------------------
    IF (LineC(1:1) == SpecialMacroCharacterC) THEN

       !------------------------------------------------------------------------------
       ! Scan for position of comment signs. No comment sign results in position = 0.
       !------------------------------------------------------------------------------
       CALL FindCommentSigns(CommentSigns_CV,LineC, CommentSign_foundL,position)
       IF ( CommentSign_foundL ) THEN
          !---------------------------------------------------------------
          ! Comment sign was found, position is position of comment sign.
          !---------------------------------------------------------------
          CALL Replace_Comment_with_blanks(position,LineC)
       END IF
       LineC = TRIM(LineC)                               ! Remove trailing blanks.

       !----------------------------------------------------------------
       ! SCAN: Scans a string for any character in a set of characters.
       !----------------------------------------------------------------

       !-----------------------------
       ! Get position of '=' symbol.
       !-----------------------------
       position = SCAN (LineC, '=')

       IF (position > 0) THEN ! A variable is only initialized if an '=' symbol is present.

          !-----------------------------
          ! Get position of '-' symbol.
          !-----------------------------
          position_minus = SCAN (LineC, '-')

          !-------------------------------------------------------------------------------------------------------------------------------------------------
          ! Why are we interested in the position of the minus sign?
          ! Because we want to allow for such a syntax.
          !   %DIRECTION             = z                       ! This means a variable assignment.
          !   %DIRECTION-coordinates = 0.0d0 10d0              ! This should be regarded as a variable which has to be replaced by its value, which is 'z'.
          !   %DIRECTION-grid-lines  = 0d0  10d0               ! This should be regarded as a variable which has to be replaced by its value, which is 'z'.
          ! If a variable name contains a '-' in front of the '=' sign, we do NOT consider it as a variable name.
          !-------------------------------------------------------------------------------------------------------------------------------------------------
          IF (.NOT. ( position_minus > 0 .AND. position_minus < position ) ) THEN ! A variable is taken into account only if a '-' sign is not contained in a variable name.
             variable_counter = variable_counter + 1

           SELECT CASE ( TRIM(actionC) )
            CASE('count')
             NumberOfVariables = variable_counter
            CASE('get')

             !------------------------------------------------------------------------------
             ! ADJUSTL is necessary to allow for blanks before and after the '=' delimiter.
             ! TRIM    is necessary to allow for blanks before and after the '=' delimiter.
             !------------------------------------------------------------------------------
! WRITE(*,'(A,A)') "              LineC                                          =",              LineC
! WRITE(*,'(A,A)') "              LineC(1:position-1)                            =",              LineC(1:position-1)
! WRITE(*,'(A,A)') "      ADJUSTL(LineC(1:position-1)                            =",      ADJUSTL(LineC(1:position-1))
! WRITE(*,'(A,A)') "TRIM( ADJUSTL(LineC(1:position-1)) )                         =",TRIM( ADJUSTL(LineC(1:position-1)) )
! WRITE(*,'(A,A)') "              LineC(position+1:position+1+LEN_TRIM(LineC)   )=",              LineC(position+1:position+1+LEN_TRIM(LineC))
! WRITE(*,'(A,A)') "      ADJUSTL(LineC(position+1:position+1+LEN_TRIM(LineC)))  =",      ADJUSTL(LineC(position+1:position+1+LEN_TRIM(LineC)))
! WRITE(*,'(A,A)') "TRIM( ADJUSTL(LineC(position+1:position+1+LEN_TRIM(LineC))) )=",TRIM( ADJUSTL(LineC(position+1:position+1+LEN_TRIM(LineC))) )
             VariableNameCV             (variable_counter) = TRIM( ADJUSTL(LineC(1:position-1)              ) )  ! VariableNameCV:  string before '=' (variable)
             VariableValueCV            (variable_counter) = TRIM( ADJUSTL(LineC(position+1:LEN_TRIM(LineC))) )  ! VariableValueCV: string after  '=' (value)    ! According to this definition everything following the "=" symbol is the variable value.
             Line_of_VariableDefinitionV(variable_counter) = line_count

             !--------------------------------------------------------
             ! Print information of variable and its value to screen.
             !--------------------------------------------------------
             WRITE(my_output_unit,'(A,A,A,A,A,A)') " definition variable #",int_2_char(variable_counter),": ", &
                                                                  TRIM(VariableNameCV (variable_counter))," <== ", &
                                                                  TRIM(VariableValueCV(variable_counter))

             !-----------------------------------------------------------------------------
             ! Check if 'variable name' is identical to 'variable value'.
             ! This is not allowed as such a statement does not make sense.
             ! Such a statement is very likely an error made by the user.
             ! Example: "%y_min = %y_min"
             !-----------------------------------------------------------------------------
             IF ( TRIM( VariableNameCV( variable_counter) ) == &
                  TRIM( VariableValueCV(variable_counter) ) ) THEN
              WRITE(my_output_unit,'(A)') " Error input file: Variable name is equal to its value."
              WRITE(my_output_unit,'(A,A,A,A,A,I10,A)') &
                    " variable: ",  TRIM(VariableNameCV( variable_counter))," = ", &
                                    TRIM(VariableValueCV(variable_counter))      , &
                    "  (line ", Line_of_VariableDefinitionV(variable_counter) ,")"
              WRITE(my_output_unit,'(A)') " I believe this was not done on purpose."
              STOP
             END IF

             !-----------------------------------------------------------------------------
             ! Check if two variable names are identical. This is currently not allowed.
             ! This means that the following is not supported (in contrast to nextnano++):
             ! "%y = 4.0"
             ! "%y = sqrt(%y) * %x"
             !-----------------------------------------------------------------------------
             DO i=1,variable_counter-1
                IF ( TRIM( VariableNameCV(i)            ) == &
                     TRIM( VariableNameCV(variable_counter) ) ) THEN
                WRITE(my_output_unit,'(A)') " Error input file: Variable is defined more than once."
                WRITE(my_output_unit,'(A,A,A,A,A,I10,A)') &
                      " variable #1: ",  TRIM(VariableNameCV( i))               ," = ", &
                                         TRIM(VariableValueCV(i))                   , &
                      "  (line ", Line_of_VariableDefinitionV(i)                ,")"
                WRITE(my_output_unit,'(A,A,A,A,A,I10,A)') &
                      " variable #2: ",  TRIM(VariableNameCV( variable_counter))," = ", &
                                         TRIM(VariableValueCV(variable_counter))      , &
                      "  (line ", Line_of_VariableDefinitionV(variable_counter),")"
                STOP
                END IF
             END DO
           END SELECT
          END IF

       END IF
    
    END IF ! End: '=' symbol is present.

  END IF

  !---------------------------------------------------
  ! Increase counter that counts current line number.
  !---------------------------------------------------
  line_count = line_count + 1
 
 END DO

!-----------------------------------------------------------------------
 END SUBROUTINE CountGetVariables
!-----------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ReplaceVariables(NumberOfVariables,VariableNameCV,VariableValueCV,Line_of_VariableDefinitionV,CommentSigns_CV, &
                             IF_STATEMENT_CV, &
                             StringsV)
!------------------------------------------------------------------------------
!
!++s* MacroForInputFile/ReplaceVariables
!
! NAME
!   SUBROUTINE ReplaceVariables
!
! PURPOSE
!   Implementation of macro in input file.
!   Replaces all variables in the input file with its (evaluated) value.
!   Looks for a special text string ('macro variable definition and value assignment')
!   in an array of strings (StringsV, e.g. obtained from an ASCII file)
!   and replaces all other occurences of this macro variable (including multiple entries in one line / one string).
!   The variable definition is not replaced, i.e. the line where the definition occurs is ignored.
!   Variables that occur comments are not replaced.
!   For an example, see further below.
!
! USAGE
!   CALL ReplaceVariables(NumberOfVariables,VariableNameCV,VariableValueCV,Line_of_VariableDefinitionV,CommentSigns_CV,IF_STATEMENT_CV, StringsV)
! 
! INPUT
!   o NumberOfVariables
!   o VariableNameCV
!   o VariableValueCV
!   o Line_of_VariableDefinitionV
!   o CommentSigns_CV:
!   o IF_STATEMENT_CV:
!   o StringsV                (also output)
!
! OUTPUT
!   o StringsV                (also input)
! 
! NOTES
!   SYNTAX
!     - "%xmin  %xmax"                    is replaced by the values of "%xmin" and "%xmax".
!     - "-%DeviceLength  +%DeviceLength"  is replaced by the value  of "%DeviceLength".
!     - "%cb_mass  %cb_mass  %cb_mass"    is replaced by the value  of "%cb_mass" multiple times.
!     - "%COORDINATE-coordinates"         is replaced by the value  of "%COORDINATE = z" resulting in "z-coordinates".
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_int_to_char999       ,ONLY:int_2_char ! converts integer to character
 USE mod_Array_of_Strings     ,ONLY:String_in_Line

 IMPLICIT NONE

 INTEGER                          ,INTENT(in)    :: NumberOfVariables
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)    :: VariableNameCV
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)    :: VariableValueCV
 INTEGER             ,DIMENSION(:),INTENT(in)    :: Line_of_VariableDefinitionV
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)    :: CommentSigns_CV
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)    :: IF_STATEMENT_CV
 TYPE(String_in_Line),DIMENSION(:),INTENT(inout) :: StringsV

 CHARACTER(len=:),ALLOCATABLE                    :: bufferC                 ! String of a particular line in input file.
 CHARACTER(len=:),ALLOCATABLE                    :: NumberOfReplacementsC
 CHARACTER(len=:),ALLOCATABLE                    :: ReplacementC
 INTEGER                                         :: line
 INTEGER                                         :: variable_counter
 INTEGER                                         :: NumberOfReplacements

 DO variable_counter=1,NumberOfVariables

       line = Line_of_VariableDefinitionV(variable_counter)

       bufferC = TRIM( ADJUSTL( StringsV(line)%StringC ) )      ! Store line in temporary array and remove leading and trailing blanks.

       WRITE(my_output_unit,'(A)')       " ================>"
       WRITE(my_output_unit,'(A,A,A,A)') " Replace variable '",TRIM(VariableNameCV (variable_counter)),"' with its value: ", &
                                                               TRIM(VariableValueCV(variable_counter))
       !----------------------------------
       ! Replace variable with its value.
       !----------------------------------
       CALL ReplaceVariableWithValue(VariableNameCV (variable_counter), &
                                     VariableValueCV(variable_counter),line,CommentSigns_CV,IF_STATEMENT_CV,VariableNameCV, &
                                     StringsV,NumberOfReplacements)

       !----------------------------------------------------------------------------------------------------------
       ! Now add a comment sign (CommentSigns_CV(1)) to this variable definition line
       ! so that the ordinary input parser does not complain.
       ! Example:    $width = 10d0 ! quantum well width
       !       ==> ! $width = 10d0 ! quantum well width
       ! CommentSigns_CV(1) is our default variable sign, i.e.
       ! comment_signsCV(1) = '!'. CHECK: This could be replaced later with
       ! comment_signsCV(2) = '#' to be more consistent with nextnano++.
       !----------------------------------------------------------------------------------------------------------
       IF (NumberOfReplacements == 1) THEN
        ReplacementC = 'replacement'
       ELSE
        ReplacementC = 'replacements'
       END IF
       NumberOfReplacementsC = int_2_char(NumberOfReplacements)
       StringsV(line)%StringC = TRIM(CommentSigns_CV(1))//'***macro*** ' &
                                //TRIM(bufferC)//' ('//NumberOfReplacementsC//' '//ReplacementC//')'
       WRITE(my_output_unit,'(A,A)') " Macro line: ",TRIM(StringsV(line)%StringC)
       WRITE(my_output_unit,'(A)')   " "

 END DO

!-----------------------------------------------------------------------
 END SUBROUTINE ReplaceVariables
!-----------------------------------------------------------------------
!
!
!
!-----------------------------------------------------------------------
 SUBROUTINE EvaluateFunction(functionCV,ReturnIntegerLV,VariableNameCV,VariableValueCV,DebugLevel, &
                             FunctionResultCV,Real8FunctionSuccessLV)
!-----------------------------------------------------------------------
! o ReturnIntegerLV: If true, the result of the evaluated function is converted to an integer.
!-----------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_chrpak               ,ONLY:int_to_s_left, &
                                    r8_to_s_left, &
                                    s_to_r8
 USE parameters               ,ONLY:rn
 USE fparser                  ,ONLY:initf, parsef, evalf, EvalErrType, EvalErrMsg

 IMPLICIT NONE

 CHARACTER(len=*),DIMENSION(:),INTENT(in)  :: functionCV
 LOGICAL         ,DIMENSION(:),INTENT(in)  :: ReturnIntegerLV
 CHARACTER(len=*),DIMENSION(:),INTENT(in)  :: VariableNameCV
 CHARACTER(len=*),DIMENSION(:),INTENT(in)  :: VariableValueCV
 INTEGER                      ,INTENT(in)  :: DebugLevel
 CHARACTER(len=*),DIMENSION(:),INTENT(out) :: FunctionResultCV       ! must be at least 17 characters long (previously it was 14)
 LOGICAL         ,DIMENSION(:),INTENT(out) :: Real8FunctionSuccessLV

 INTEGER                                   :: NumberOfVariables
 INTEGER                                   :: NumberOfFunctions
 REAL(rn)                                  :: real_value
 REAL(rn)        ,DIMENSION(:),ALLOCATABLE :: valueV
 REAL(rn)                                  :: FunctionResult
 INTEGER                                   :: i
 INTEGER                                   :: ierror
 INTEGER                                   :: length

   NumberOfFunctions = SIZE(functionCV)
   NumberOfVariables = SIZE(VariableNameCV)

   ALLOCATE(valueV(NumberOfVariables))
   valueV = 0d0

   DO i = 1,NumberOfVariables
      !----------------------------------------------------
      ! We try to replace the variable with a real number.
      !----------------------------------------------------
      CALL s_to_r8 ( TRIM(VariableValueCV(i)) , real_value , ierror , length)
      IF (ierror == 0 .AND. &
          length == LEN_TRIM(VariableValueCV(i))) THEN
         !-------------------------------------------
         ! Variable was replaced with a real number.
         !-------------------------------------------
       ! WRITE(my_output_unit,*) "length = ",length," == LEN_TRIM(VariableValueCV(i)) = ",LEN_TRIM(VariableValueCV(i))
         valueV(i) = real_value
       ! WRITE(my_output_unit,'(A,I5,A,ES26.12,A)') &
       IF (DebugLevel > 3) THEN
         WRITE(my_output_unit,'(A,I8,A,EN22.12,A,A,A)') &
                 " macro variable(",i,") = ",valueV(i)," (string replaced with real value)  [",TRIM(VariableNameCV(i)),"]"
       END IF
      ELSE
         !-----------------------------------------------
         ! Variable was not replaced with a real number.
         !-----------------------------------------------
       ! WRITE(my_output_unit,*) "length = ",length," /= LEN_TRIM(VariableValueCV(i)) = ",LEN_TRIM(VariableValueCV(i))
       ! WRITE(my_output_unit,'(A,I5,A,A,A)') &
       IF (DebugLevel > 3) THEN
         WRITE(my_output_unit,'(A,I8,A,A,A,A,A,A,A)') &
                 " macro variable(",i,") = ",TRIM(VariableValueCV(i))," (string not used within function parser)  [", &
                                                                                               TRIM(VariableNameCV(i)),"]"
       END IF
       ! WRITE(my_output_unit,*) "valueV(i) = cannot be assigned a real value."
         !------------------------------------------------------------------------------------------
         ! valueV(i) of variable(i) cannot be assigned a real value because it is a sort of string.
         !------------------------------------------------------------------------------------------
      END IF
   END DO

   !---------------------------------------------------------------
   ! Initialize function parser for 'NumberOfFunctions' functions.
   !---------------------------------------------------------------
   CALL initf (NumberOfFunctions)

   DO i=1,NumberOfFunctions
      !--------------------------------------------
      ! Parse and bytecompile ith function string.
      !--------------------------------------------
   !  IF (DebugLevel > 3) WRITE(my_output_unit,('(A,A)'))   "Parse function = ",TRIM(functionCV(i))
      CALL parsef(i,functionCV(i),VariableNameCV)
   !  IF (DebugLevel > 3) WRITE(my_output_unit,('(A,A,A)')) "Parse function = ",TRIM(functionCV(i))," (done)"
   END DO

 ! WRITE(my_output_unit,*)'==> Bytecode evaluation:'
   DO i=1,NumberOfFunctions
      !-----------------------------------------------------
      ! Interprete bytecode representation of ith function.
      !-----------------------------------------------------
      FunctionResult = evalf (i, valueV)
       IF (DebugLevel > 3) THEN
         WRITE(my_output_unit,*)            " function = ",TRIM(functionCV(i))," = ",FunctionResult," (REAL(8)   variable)" ! ### Write to .log file. ###
       END IF
      IF (EvalErrType > 0) THEN
         WRITE(my_output_unit,*) '*** Error: ',EvalErrMsg ()
         Real8FunctionSuccessLV(i) = .FALSE.
      ELSE
         IF (ReturnIntegerLV(i)) THEN
          !----------------------------------------------------
          ! Convert result to integer.
          ! NINT: Returns the nearest integer to the argument.
          ! ==> Negative numbers are considered correctly.
          !----------------------------------------------------
          IF ( ABS(FunctionResult) > DBLE( HUGE(0) ) ) THEN ! This check does not work if FunctionResult is NaN.
           WRITE(my_output_unit,*) " Error EvaluateFunction: FunctionResult  >  maximum integer value"
           WRITE(my_output_unit,*) "                    ABS( FunctionResult )= ",ABS(FunctionResult)
           WRITE(my_output_unit,*) "                         FunctionResult  = ",    FunctionResult
           WRITE(my_output_unit,*) "                         largest integer = ",HUGE(0)
           IF (DebugLevel < 10) STOP
          END IF
          CALL int_to_s_left ( NINT(FunctionResult), FunctionResultCV(i) )
         ELSE
          !------------------------------------------------------------------------------------
          ! (Check: A 'G14.6' format is used with a WRITE statement in subroutine r8_to_s_left.)
          !  Check: A 'G17.9' format is used with a WRITE statement in subroutine r8_to_s_left.
          !  This is probably not precise enough for double precision.
          !------------------------------------------------------------------------------------
          CALL r8_to_s_left (     FunctionResult,  FunctionResultCV(i) )
         END IF
       ! WRITE(my_output_unit,'(A,ES26.12,A)') &
       IF (DebugLevel > 3) THEN
         WRITE(my_output_unit,'(1x,A,A,A)') "          = ",TRIM(FunctionResultCV(i))               ," (character variable)" ! ### Write to .log file. ###
       END IF
         Real8FunctionSuccessLV(i) = .TRUE.
      END IF

   END DO

   DEALLOCATE(valueV)

!-----------------------------------------------------------------------
 END SUBROUTINE EvaluateFunction
!-----------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ReplaceVariableWithValue(VariableC,VariableValueC,Line_of_VariableDefinition,CommentSignsCV,IF_STATEMENT_CV, & 
                                     VariableNameCV, StringsV, &
                                     NumberOfReplacements)
!------------------------------------------------------------------------------
!
!++s* MacroForInputFile/ReplaceVariableWithValue
!
! NAME
!   SUBROUTINE ReplaceVariableWithValue
!
! PURPOSE
!   Replaces a variable ('VariableC' = '%width') with its value ('VariableValueC' = '10.0').
!   variable definition, e.g.: '%width = 10.0'
!   The variable can be defined several times in StringsV(i), i.e. several times in a line. Here, the line number is 'i'.
!   If there is a comment sign at the beginning of the line, no variable replacements are made in this line, i.e. we leave a commented line unchanged,
!   unless it is an "!IF ..." statement. In this case, we replace the variable if the IF statement is true.
!
! USAGE
!   CALL ReplaceVariableWithValue(VariableC,VariableValueC,Line_of_VariableDefinition,CommentSignsCV,IF_STATEMENT_CV,VariableNameCV, StringsV, NumberOfReplacements)
! 
! INPUT
!   o VariableC:                   e.g. '%width'
!   o VariableValueC:              e.g. '10.0'
!   o Line_of_VariableDefinition:  if the line containing the variable definition should be excluded (which is usually the case)
!   o CommentSignsCV:              e.g. '!'
!   o IF_STATEMENT_CV:             e.g. '!IF'
!   o VariableNameCV:              list of all variable names
!   o StringsV (also output):      Array of strings that have to replaced.
!
! OUTPUT
!   o StringsV (also input)
!   o NumberOfReplacements
! 
! NOTES
!   Examples:
!
!     %width = 10.0          ! width in 10 nm
!     ==> Replaces all variables %width with "10.0".
!
!     %material = GaAs       ! material name
!     ==> Replaces all variables %material with "GaAs".
!
!     %string = '10.0 20.0'  !
!     ==> Replaces all variables %string with "10.0 20.0".
!
!    Feature: "Conditional comment"
!       "!IF %TemperatureDependentBandGap  varshni-parameters-on = yes"
!        Such a statement has the following meaning:
!        If the value of %TemperatureDependentBandGap is either .TRUE.  or 1 or any other nonzero value, then the statement is included in the input file.
!        If the value of %TemperatureDependentBandGap is either .FALSE. or 0 or undefined, then the statement is simply ignored as if it were a comment.
!
! CONTAINS
!   o SUBROUTINE ReplaceVariableAtPositionInLine
!
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE mod_Array_of_Strings     ,ONLY:String_in_Line

 IMPLICIT NONE

 CHARACTER(len=*)                 ,INTENT(in)    :: VariableC
 CHARACTER(len=*)                 ,INTENT(in)    :: VariableValueC
 INTEGER                          ,INTENT(in)    :: Line_of_VariableDefinition
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)    :: CommentSignsCV
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)    :: IF_STATEMENT_CV
 CHARACTER(len=*)    ,DIMENSION(:),INTENT(in)    :: VariableNameCV
 TYPE(String_in_Line),DIMENSION(:),INTENT(inout) :: StringsV
 INTEGER                          ,INTENT(out)   :: NumberOfReplacements

 INTEGER                                         :: number_of_replacement
 INTEGER                                         :: j
 INTEGER                                         :: character_position
 INTEGER                                         :: position
 INTEGER                                         :: position_previous
!INTEGER                                         :: position_in_substring
 INTEGER                                         :: position_for_replacement
 INTEGER                                         :: position_other_variable
 LOGICAL                                         :: position_uniqueL
!INTEGER                                         :: position_of_other_variable_name
 INTEGER                                         :: length_of_variable_name
 INTEGER                                         :: length_of_other_variable_name
 LOGICAL                                         :: ReplaceL
 LOGICAL                                         :: uniqueL
 LOGICAL                                         :: IF_StatementL
 INTEGER                                         :: IF_Statement_length
 LOGICAL                                         :: Line_is_a_CommentL
 LOGICAL                                         :: Variable_belongs_to_IF_StatementL
!CHARACTER(len=:),ALLOCATABLE                    :: String_Left_C
!CHARACTER(len=:),ALLOCATABLE                    :: String_Middle_C
!CHARACTER(len=:),ALLOCATABLE                    :: String_Right_C
!CHARACTER(len=:),ALLOCATABLE                    :: RemainingStringC

 WRITE(my_output_unit,'(A)') " "

 !-------------------------------------------------------------------
 ! Counter for number of variables that have been replaced by value.
 !-------------------------------------------------------------------
 NumberOfReplacements = 0

 length_of_variable_name = LEN_TRIM( VariableC )

 !-------------------------------------------------------------------------------------------------
 ! Check for unique variable names: A variable name might be a substring of another variable name.
 ! Example: We can have similar variable names, e.g. %variable_name  = 1.0e3
 !                                                   %variable_name_test_a = ...
 !                                                   %variable_name_test_b = ...
 ! i.e. a variable name is contained in a longer variable name as a substring.
 ! We have to be careful to not replace              %variable_name_test_a with
 !                                                   %1.0e3_name_test_a
 ! because '%variable_name' is a substring of '%variable_name_test_a'.
 !-------------------------------------------------------------------------------------------------
 CALL Check_if_variable_name_is_unique(VariableC,VariableNameCV, uniqueL)

 !-----------------------------------------------------------------------------------------------------------
 ! Option a) Loop over all lines in input file, starting from the first line.
 ! Option b) Loop over all lines in input file, starting from the line after the variable definition.
 ! Which option should be implemented? a) or b)?
 ! b) seems to be more consistent to nextnano++ because a statement such as "%y = sqrt(%y) * %x" is allowed.
 !-----------------------------------------------------------------------------------------------------------
!STRINGS: do j =  1,                   size(StringsV)           ! a)
 STRINGS: do j =  Line_of_VariableDefinition+1, size(StringsV)  ! b)
!        IF (j /= Line_of_VariableDefinition) THEN              ! a) Exclude the line in which the variable value is defined.

   !---------------------------------------------------------
   ! Check if line begins with "!IF " or if it is a comment.
   !---------------------------------------------------------
   call Treat_IfStatement_and_Comment( IF_STATEMENT_CV, CommentSignsCV, VariableC, StringsV(j)%StringC, &
                                       Line_is_a_CommentL, IF_StatementL, &
                                                           IF_Statement_length, Variable_belongs_to_IF_StatementL )

   IF ( IF_StatementL .OR. (.NOT. Line_is_a_CommentL) ) THEN

        !----------------------
        ! Initialize variable.
        !----------------------
        position_previous = 0

      !----------------------------------------------------------------------------------------------
      ! This loop is necessary if the variable occurs several times in one line, i.e. in one string.
      !----------------------------------------------------------------------------------------------
    ! LINE: do ! Infinite Loop (Too unsecure)
      LINE: do character_position=1,LEN_TRIM( StringsV(j)%StringC ) ! Loop is limited to number of characters in line as an upper limit.

      !----------------------------------------------------------------------
      ! Search for variable name (e.g. '%x') in current line.
      ! Get position of 'VariableC' in string.
      ! If we have two variables called
      !  o '%x' and
      !  o '%xmax'
      ! we have to be careful, not to replace the substring '%x' in '%xmax'.
      !----------------------------------------------------------------------
           position = INDEX ( TRIM(StringsV(j)%StringC ), TRIM(VariableC) ) ! Get position of substring 'VariableC' in line.

!----------------------------------------------
! CHECK: (A) This does does not work yet.
! %Radius                      = 2.0
! %RadiusShell                 = 2.0 * %Radius    ! ==> Substring %Radius is contained in string %RadiusShell and afterwards it is used.
!----------------------------------------------
           IF (position <= position_previous) EXIT ! Exit infinite DO loop. ! This statement could be put into comments to allow for (A).

           position_previous = position

           !-----------------------------------------------------------------------------------
           ! If not found, 'position' has the value '0'.
           ! If found at position 'position', then replace it (unless it is only a substring).
           !-----------------------------------------------------------------------------------
           POSITION_GT_ZERO: if (position > 0) then

            UNIQUE: if (uniqueL) then
               ReplaceL = .TRUE.
               position_for_replacement = position
            else
               !---------------------------------------------------------------------
               ! If variable substring is not unique, we have to do a further check.
               !---------------------------------------------------------------------
!----------------------------------------------
! CHECK: (A) This does not work yet.
! %Radius                      = 2.0
! %RadiusShell                 = 2.0 * %Radius    ! ==> Substring %Radius is contained in string %RadiusShell and afterwards it is used.
!----------------------------------------------

            !----------------------------------------
            ! We divide the string into three parts.
            !----------------------------------------
         ! IF ( position == 1 ) THEN 
         !  String_Left_C   = ''
         ! ELSE
         !  String_Left_C   = TRIM( StringsV(j)%StringC( 1 : position-1 ) )
         ! END IF
         !  String_Middle_C = TRIM( StringsV(j)%StringC(     position : position+LEN_TRIM(VariableC)-1 ) )                                      ! This part contains 'TRIM(VariableC)'.
         !  String_Right_C  = TRIM( StringsV(j)%StringC(                position+LEN_TRIM(VariableC) : LEN_TRIM( TRIM(StringsV(j)%StringC)) ) )
         !  WRITE(*,'(A,A)') " hello String_Left_C   = ",TRIM(String_Left_C)
         !  WRITE(*,'(A,A)') " hello String_Middle_C = ",TRIM(String_Middle_C)
         !  WRITE(*,'(A,A)') " hello String_Right_C  = ",TRIM(String_Right_C)

            CALL CheckIfPositionIsUnique( TRIM(StringsV(j)%StringC),position, &
                                          position_uniqueL,position_other_variable,length_of_other_variable_name )

               IF (position_uniqueL) THEN
                    ReplaceL = .TRUE.
                    position_for_replacement = position
               ELSE
                  !------------------------------------------------------------------------------------------------------------
                  ! The variable substring is contained in another variable name.
                  ! There are two variables of different length. The longer string contains the shorter string as a substring.
                  ! We are not allowed to replace the longer string with the shorter string.
                  ! Only the opposite is allowed, i.e. LEN_TRIM(VariableC) must be larger than LEN_TRIM(VariableNameCV(i).
                  !------------------------------------------------------------------------------------------------------------

                  IF (length_of_variable_name >= length_of_other_variable_name) THEN
                    ReplaceL = .TRUE.
                    position_for_replacement = position ! CHECK: Somehow this does not make sense. Instead, the remaining string should be checked for a new position in case it appears again.
                  ELSE

                    ReplaceL = .FALSE.
                    position_for_replacement = 0
                    EXIT ! Exit DO loop.

                 ! RemainingStringC = TRIM( StringsV(j)%StringC( position_other_variable+length_of_other_variable_name : &
                 !                                               LEN_TRIM(StringsV(j)%StringC) ) )
                 ! position_in_substring = INDEX ( TRIM(RemainingStringC) , TRIM(VariableC) )      ! Get position of substring 'VariableNameCV(i)' in line.

                 ! CALL CheckIfPositionIsUnique( TRIM(RemainingStringC),position_in_substring, &
                 !                               position_uniqueL,position_other_variable,length_of_other_variable_name )

                 !  IF ( position_in_substring > 0 ) THEN
                 !   WRITE(*,'(A,A)') " hello RemainingStringC = ",TRIM(RemainingStringC)
                 !  WRITE(*,*)       " position_uniqueL       = ",position_uniqueL
                 !  IF (position_uniqueL) THEN
                 !    ReplaceL = .TRUE.
                 !    position_for_replacement = position_other_variable + length_of_other_variable_name + position_in_substring
                  !  WRITE(*,*) " hello position in substring / for replacement  = ",position_in_substring,position_for_replacement
                 !   ELSE
                 !   ReplaceL = .FALSE.
                 !  END IF
                 !  ELSE
                 !    ReplaceL = .FALSE.
                 !    position_for_replacement = 0
                 !    EXIT ! Exit DO loop.
                 !  END IF
                  END IF
               END IF
            end if UNIQUE

            IF (ReplaceL) THEN
               !----------------------------
               ! Replace variable by value.
               !----------------------------
               CALL ReplaceVariableAtPositionInLine(VariableC,VariableValueC,position_for_replacement,StringsV(j)%StringC, &
                                                             number_of_replacement)
               NumberOfReplacements = NumberOfReplacements + number_of_replacement
            END IF
           else       ! If not found, 'position' has the value '0'.
            EXIT      ! Exit do loop if not found.
           end if POSITION_GT_ZERO

      end do LINE ! End: Infinite loop over the line

   END IF         ! End: IF_StatementL .OR. (.NOT. Line_is_a_CommentL)

! END IF          ! End: Excluded line numbers.
 end do STRINGS   ! End: Loop over all lines

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE CheckIfPositionIsUnique(StringC,position_variable, &
                                    position_uniqueL,position_other_variable,length_of_other_variable_name)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: StringC
 INTEGER         ,INTENT(in)  :: position_variable
 LOGICAL         ,INTENT(out) :: position_uniqueL
 INTEGER         ,INTENT(out) :: position_other_variable
 INTEGER         ,INTENT(out) :: length_of_other_variable_name

 INTEGER                      :: i

 IF (position_variable <= 0 ) THEN
    WRITE(my_output_unit,'(A,I20)') " Error CheckIfPositionIsUnique: position_variable <= 0: ",position_variable
    STOP
 END IF

 position_uniqueL              = .TRUE.
 position_other_variable       = 0
 length_of_other_variable_name = 0

 !--------------------------
 ! Loop over all variables.
 !--------------------------
 DO i=1,SIZE(VariableNameCV)
    IF ( TRIM(VariableC) /=                               TRIM(VariableNameCV(i)) ) THEN ! Exclude variable itself from this test.
         !-------------------------------------------------------
         ! Find position of variable name (that is a substring).
         !-------------------------------------------------------
         position_other_variable = INDEX ( TRIM(StringC), TRIM(VariableNameCV(i)) )      ! Get position of substring 'VariableNameCV(i)' in line.
         !----------------------------------------------------------------------------------------------------
         ! If not found, 'position_other_variable' has the value '0', 'position_variable' is always > 0 here.
         !----------------------------------------------------------------------------------------------------
         IF ( position_other_variable == position_variable ) THEN
              !---------------------------------------------------
              ! Two variable substrings are at the same position.
              ! We have something like this:
              ! %Radius      = 2.0
              ! %RadiusShell = 2.0
              !---------------------------------------------------
              position_uniqueL = .FALSE.
              length_of_other_variable_name = LEN_TRIM( VariableNameCV(i) )
              EXIT ! Exit DO loop.
         END IF
    END IF
 END DO

!------------------------------------------------------------------------------
 END SUBROUTINE CheckIfPositionIsUnique
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ReplaceVariableAtPositionInLine(VariableC,VariableValueC,VariablePosition,LineC, NumberOfReplacements)
!------------------------------------------------------------------------------
! PURPOSE
!   Replace variable by value.
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 CHARACTER(len=*)            ,INTENT(in)    :: VariableC
 CHARACTER(len=*)            ,INTENT(in)    :: VariableValueC
 INTEGER                     ,INTENT(in)    :: VariablePosition
 CHARACTER(len=:),ALLOCATABLE,INTENT(inout) :: LineC
 INTEGER                     ,INTENT(out)   :: NumberOfReplacements

 INTEGER                                    :: i,ii
 LOGICAL                                    :: UndefinedVariableL
 CHARACTER(len=:),ALLOCATABLE               :: StringAfterReplacementC
 CHARACTER(len=:),ALLOCATABLE               :: tempC

 NumberOfReplacements = 0
 UndefinedVariableL = .FALSE.

 IF (IF_StatementL) THEN
     !------------------------------------------------------------------------------------------------
     ! Check for unique variable names.
     ! Example: We can have similar variable names, e.g.     %output_strain = yes
     !                                                   !IF %output_strain_sim output-strain = yes
     ! If '%output_strain' is defined but '%output_strain_sim' is not defined,
     ! we are not allowed to replace '%output_strain_sim' with the substring '%output_strain'.
     ! The rule is that if a variable is undefined, the '!IF ' statement is not replaced with blanks.
     !------------------------------------------------------------------------------------------------
     IF ( LEN(LineC) >= VariablePosition+LEN_TRIM(VariableC) ) THEN        ! Ensure that string to be tested is large enough.
         IF  (LineC(    VariablePosition+LEN_TRIM(VariableC): &
                        VariablePosition+LEN_TRIM(VariableC)) /= ' ') THEN ! We require a variable that comes after the '%IF ' statement to be followed by a blank, e.g. '!IF %output_strain_sim '
              UndefinedVariableL = .TRUE.
         END IF
     END IF
 END IF

 IF (.NOT. UndefinedVariableL) THEN
     WRITE(my_output_unit,'(A,A)') " I found:       ",TRIM(LineC)
     tempC = LineC

     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     ! IF IF IF IF IF IF IF
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     IF (IF_StatementL .AND. Variable_belongs_to_IF_StatementL) THEN
      IF ( Check_if_VariableValue_is_TRUE(VariableValueC) ) THEN ! Conditional comment is .TRUE. (default)
          !-----------------------------------------------------------------------
          ! If the variable value that belongs to the '!IF ' statement is .TRUE.,
          ! we have to uncomment the line, i.e. replace the '!IF ' statement.
          !-----------------------------------------------------------------------
          tempC = ADJUSTL(tempC) ! Shift string to left to make sure that line starts with '!IF '.
          !----------------------------- 
          ! Replace "!IF " with blanks.
          !----------------------------- 
          WRITE(my_output_unit,'(A,A)') "    ==> IF: <== ",TRIM(tempC)  ! Note: This line starts with '!IF ...'
          DO ii=1,IF_Statement_length
           tempC(ii:ii) = ' '                                           ! Replace '!IF'   with 3 blanks or
                                                                        ! replace '!WHEN' with 5 blanks.
          END DO
          tempC = ADJUSTL( tempC )                                      ! Remove leading blanks.
          DO i=1,LEN_TRIM(VariableC)
           tempC(i:i) = ' '                                             ! Replace variable name with ' '. (The '!IF ' string has been replaced already.)
          END DO
          StringAfterReplacementC = ' '//TRIM(tempC)                    ! Add a blank as the first character.
          NumberOfReplacements = NumberOfReplacements + 1
      ELSE                                                       ! Conditional comment is .FALSE., i.e. leave the '!IF ' comment in case it is there.
          StringAfterReplacementC = tempC
      END IF
     ELSE
          !------------------------------ 
          ! Replace variable with value.
          !------------------------------ 
          StringAfterReplacementC = LineC(1:VariablePosition-1) // TRIM(VariableValueC) // &
                                    LineC(  VariablePosition + LEN_TRIM(VariableC) : LEN_TRIM(LineC))
          NumberOfReplacements = NumberOfReplacements + 1
     END IF
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     ! IF IF IF IF IF IF IF
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     LineC = StringAfterReplacementC
     WRITE(my_output_unit,'(A,A)') " Replaced with: ",TRIM(LineC)
 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE ReplaceVariableAtPositionInLine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE ReplaceVariableWithValue
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 subroutine Treat_IfStatement_and_Comment( IF_STATEMENT_CV, COMMENT_SIGNS_CV, VariableC, stringC, &
                                           Line_is_a_CommentL, IF_StatementL, &
                                                               IF_Statement_length, Variable_belongs_to_IF_StatementL )
!------------------------------------------------------------------------------
!
!++s* MacroForInputFile/Treat_IfStatement_and_Comment
!
! NAME
!   SUBROUTINE Treat_IfStatement_and_Comment
!
! PURPOSE
!   Check if line is a comment and return .TRUE. if this is the case.
!   Check if line is a conditional IF and return .TRUE. if this is the case, and the length of the IF statement string.
!
! USAGE
!   CALL Treat_IfStatement_and_Comment( IF_STATEMENT_CV, COMMENT_SIGNS_CV, VariableC, stringC, &
!                                       Line_is_a_CommentL, IF_StatementL, IF_Statement_length, Variable_belongs_to_IF_StatementL )
! 
! INPUT
!   o IF_STATEMENT_CV:                     e.g. '!IF', '#IF', '!WHEN', '#WHEN'
!   o COMMENT_SIGNS_CV:                    e.g. '!', '#', '//', '/*' (Note: Comment sign can consist of any number of characters, e.g. 1 ('!') or 2 ('//') characters both work.
!   o VariableC:
!   o stringC:                             string that can include a comment sign or if statement in the beginning of the line
!
! OUTPUT
!   o Line_is_a_CommentL:                  .TRUE. if line is a comment, else .FALSE.
!   o IF_StatementL:                       .TRUE. if line is an if statement (Line_is_a_CommentL is still .TRUE.), else .FALSE.
!   o IF_Statement_length:                 length of 'IF' statement, e.g. 3 for '!IF' and '5' for '!WHEN'
!   o Variable_belongs_to_IF_StatementL:   .TRUE. if variable belongs to IF statement.
! 
! NOTES
!   Examples:
!
!     o  #IF %TemperatureDependentBandGap  varshni-parameters-on = yes
!     
!     o  !IF %IncludeHoles $quantum-model-holes
!
!##
!
!------------------------------------------------------------------------------
 use String_Utility           ,only:StringUpperCase

 implicit none

 character(len=*), dimension(:), intent(in)  :: IF_STATEMENT_CV
 character(len=*), dimension(:), intent(in)  :: COMMENT_SIGNS_CV
 character(len=*)              , intent(in)  :: VariableC
 character(len=*)              , intent(in)  :: stringC
 logical                       , intent(out) :: Line_is_a_CommentL
 logical                       , intent(out) :: IF_StatementL
 integer                       , intent(out) :: IF_Statement_length
 logical                       , intent(out) :: Variable_belongs_to_IF_StatementL

 character(len=:),allocatable                :: FirstRelevantCharactersInLineC
 character(len=:),allocatable                :: Line_without_IF_StatementC
 integer                                     :: i,ii
 integer                                     :: length_comment_sign
 integer                                     :: length_FirstRelChars
 integer                                     :: length_if_statement

 !------------------------
 ! Assign default values.
 !------------------------
 Line_is_a_CommentL                = .FALSE.
 IF_StatementL                     = .FALSE.
 Variable_belongs_to_IF_StatementL = .FALSE.
 IF_Statement_length               = 0

 !-------------------------------------------------------------------
 ! Get first relevant character of line ignoring any leading blanks.
 !-------------------------------------------------------------------
 FirstRelevantCharactersInLineC = adjustl( stringC )

 if ( len(FirstRelevantCharactersInLineC) /= 0 ) then ! Only do this if line is not empty.

  !--------------------------------
  ! A) Check if line is a comment.
  !--------------------------------

  !----------------------------------------------------------------------------------------------------
  ! Check if first character of line is comment sign.
  ! If this is the case, we do not replace the variable with its value and leave the comment as it is.
  !----------------------------------------------------------------------------------------------------
   length_FirstRelChars = LEN_TRIM(FirstRelevantCharactersInLineC)

  do i=1,SIZE( COMMENT_SIGNS_CV ) ! Loop over number of available comment signs.
   length_comment_sign  = LEN_TRIM(COMMENT_SIGNS_CV(i))
   if ( length_FirstRelChars >= length_comment_sign ) THEN
    if ( FirstRelevantCharactersInLineC(1:length_comment_sign) == &
                    COMMENT_SIGNS_CV(i)(1:length_comment_sign) ) then
     Line_is_a_CommentL = .TRUE.
     exit
    end if
   end if
  end do


  !---------------------------------------
  ! B) Check if line is a conditional IF.
  !---------------------------------------

  do i=1,size( IF_STATEMENT_CV ) ! Loop over number of available if statements.
   length_if_statement = LEN_TRIM(IF_STATEMENT_CV(i))

   if ( length_FirstRelChars >= length_if_statement ) THEN

    if ( StringUpperCase( FirstRelevantCharactersInLineC(1:length_if_statement) ) == &
                                      IF_STATEMENT_CV(i)(1:length_if_statement) ) then
      IF_StatementL = .TRUE.
      IF_Statement_length = length_if_statement

      IF_STATEMENT: if ( IF_StatementL ) then
           Line_without_IF_StatementC = adjustl( stringC )
          do ii=1,length_if_statement
           Line_without_IF_StatementC( ii:ii ) = ' '   ! Replace '!IF' with blanks.
          end do
           Line_without_IF_StatementC = adjustl( Line_without_IF_StatementC ) ! Remove leading blanks.
           !-------------------------------------------------------------------------------
           ! Now we check if the variable name directly follows the '!IF ' statement.
           ! If yes, then we know that this variable is relevant for the '!IF ' statement.
           !-------------------------------------------------------------------------------
         if ( len_trim( Line_without_IF_StatementC ) >= len_trim(VariableC) ) then ! Make sure that string is large enough so that the next line does not crash.
          if (          Line_without_IF_StatementC( 1 : len_trim(VariableC) ) == trim(VariableC) ) then
            Variable_belongs_to_IF_StatementL = .TRUE.
          else
            Variable_belongs_to_IF_StatementL = .FALSE.
          end if
         else
            Variable_belongs_to_IF_StatementL = .FALSE.
         end if
      end if IF_STATEMENT
     end if

    exit ! Exit do loop.

   end if
  end do

 end if

!------------------------------------------------------------------------------
 end subroutine Treat_IfStatement_and_Comment
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION Check_if_VariableValue_is_TRUE(VariableValueC) RESULT(is_TRUE_L)
!------------------------------------------------------------------------------
!
!++f* MacroForInputFile/Check_if_VariableValue_is_TRUE
!
! NAME
!   FUNCTION Check_if_VariableValue_is_TRUE
!
! PURPOSE
!   Checks if the value of the variable should be considered as .FALSE.
!   If the variable value equals
!     0, or
!     .FALSE. or .false.,
!     a very small number
!   this function returns .FALSE., else .TRUE.
!
! USAGE
!   Check_if_VariableValue_is_TRUE(VariableValueC)
! 
! INPUT 
!   o VariableValueC:     string containing value of variable
!
! OUTPUT
!   o is_TRUE_L:          .TRUE. or .FALSE.
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE String_Utility           ,ONLY:StringUpperCase
 USE mod_chrpak               ,ONLY:s_to_r8

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: VariableValueC
 LOGICAL                      :: is_TRUE_L       ! RESULT

 INTEGER                      :: length_of_variable_value
 INTEGER                      :: ierror,length
 REAL(8)         ,PARAMETER   :: epsilon = 1d-12
 REAL(8)                      :: VariableValue_real

 is_TRUE_L = .TRUE. ! By default, we assume that value is not .FALSE.

 length_of_variable_value = LEN_TRIM(VariableValueC)

 IF      ( length_of_variable_value == 1) THEN ! The integer '0' consists of 1 character.
                    IF (                     VariableValueC(1:1) == '0') THEN
                        is_TRUE_L = .FALSE.
                    END IF
 ELSE IF ( length_of_variable_value == 7) THEN ! .FALSE. consists of 7 characters.
                    IF (StringUpperCase(TRIM(VariableValueC))    == '.FALSE.' ) THEN
                        is_TRUE_L = .FALSE.
                    END IF
 ELSE
                    !----------------------------------------------------
                    ! We try to replace the variable with a real number.
                    !----------------------------------------------------
                    CALL s_to_r8 ( TRIM(VariableValueC) , VariableValue_real , ierror , length)
                    IF ( ierror == 0 .AND. &
                      length == LEN_TRIM(VariableValueC) ) THEN
                      !-------------------------------------------
                      ! Variable was replaced with a real number.
                      !-------------------------------------------
                      IF ( ABS(VariableValue_real) < epsilon) is_TRUE_L = .FALSE.
                    END IF
 END IF

!------------------------------------------------------------------------------
 END FUNCTION Check_if_VariableValue_is_TRUE
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Check_if_variable_name_is_unique(VariableC,VariableNameCV, uniqueL)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units ,ONLY:my_output_unit
 USE system_specific_parser    ,ONLY:DebugLevel

 IMPLICIT NONE

 CHARACTER(len=*)             ,INTENT(in)  :: VariableC
 CHARACTER(len=*),DIMENSION(:),INTENT(in)  :: VariableNameCV
 LOGICAL                      ,INTENT(out) :: uniqueL

 INTEGER                                   :: i

 INTEGER                                   :: length_of_variable
 CHARACTER(len=:),ALLOCATABLE              :: OtherVariableC

 uniqueL = .TRUE.

 length_of_variable = LEN_TRIM(VariableC)

 !-------------------------------
 ! Loop over all variable names.
 !-------------------------------
 DO i=1,SIZE(VariableNameCV)
    OtherVariableC = TRIM(VariableNameCV(i))
    IF ( TRIM(VariableC) /= TRIM(OtherVariableC) ) THEN         ! Exclude variable itself from this test.
     IF (length_of_variable < LEN_TRIM(OtherVariableC) ) THEN   ! If variable name has smaller length, this substring could potentially be contained, e.g. '%width' is contained in '%width_well'

      IF ( TRIM(VariableC     (1:length_of_variable)) == &
           TRIM(OtherVariableC(1:length_of_variable)) ) THEN
           !--------------------------------------------------------------------------------------------
           ! There are two variables of different length.
           ! The longer string 'OtherVariableC' contains the shorter string 'VariableC' as a substring.
           ! Thus the variable string is not "unique".
           !--------------------------------------------------------------------------------------------
           uniqueL = .FALSE.
           IF ( DebugLevel >= 1000 ) THEN
              WRITE(my_output_unit,'(A,A,A)') " Variable name is not unique: ",TRIM(VariableC), &
                                              "(Variable name is contained in another variable name as a substring.)"
           END IF
           EXIT ! Exit DO loop.
      END IF
     END IF
    END IF
 END DO

!------------------------------------------------------------------------------
 END SUBROUTINE Check_if_variable_name_is_unique
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE MacroForInputFile
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_InputFileName
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE GetInputFileNameFromValidatorFile(keywords_definition_filenameC,comment_signs_CV,key_char,spec_char, &
                                              input_filenameC)
!------------------------------------------------------------------------------
!USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE DirectoryFileExist       ,ONLY:FileExistREAD
 USE mod_push                 ,ONLY:FindCommentSigns, &
                                    Replace_Comment_with_blanks

 IMPLICIT NONE
  
 CHARACTER(len=*)             ,INTENT(in)  :: keywords_definition_filenameC  ! name of file where the syntax of the keywords is defined
 CHARACTER(len=*),DIMENSION(:),INTENT(in)  :: comment_signs_CV
 CHARACTER(len=1)             ,INTENT(in)  :: key_char
 CHARACTER(len=1)             ,INTENT(in)  :: spec_char
 CHARACTER(len=*)             ,INTENT(out) :: input_filenameC

 INTEGER                                   :: ios
 INTEGER                                   :: ipos
 INTEGER                                   :: i
 INTEGER                                   :: line_length
 LOGICAL                                   :: CommentSign_foundL 
                                                                      !
 CALL FileExistREAD(keywords_definition_filenameC, &
                    ErrorMessageC = 'For Linux: Check if environment variable NEXTNANO is set correctly '// &
                                    'so that definition files can be found.')

 OPEN(10,STATUS='OLD', IOSTAT=ios, file=keywords_definition_filenameC)
  ! IF (ios /= 0) THEN
  !  WRITE(my_output_unit,*)"ERROR in OPEN file = ",TRIM(keywords_definition_filenameC)
  !  WRITE(my_output_unit,*)"File doesn't exist or unit 10 is already connected!"   !
  !  STOP                                                              !
  ! END IF                                                             !
    input_filenameC = ""                                               !
    DO 
    IF (LEN_TRIM(ADJUSTL(input_filenameC)) /= 0) EXIT
    input_filenameC = ""                                               !
    READ (10,"(A)") input_filenameC                                    !

    !--------------------------------------------------------------------------
    ! Scan for position of comment signs. No comment sign results in ipos = 0.
    !--------------------------------------------------------------------------
    CALL FindCommentSigns(comment_signs_CV,input_filenameC, CommentSign_foundL,ipos)

    IF ( CommentSign_foundL ) THEN
      !-----------------------------------------------------------
      ! Comment sign was found, ipos is position of comment sign.
      !-----------------------------------------------------------
      CALL Replace_Comment_with_blanks(ipos,input_filenameC)
    END IF

     line_length = LEN_TRIM(ADJUSTL(input_filenameC))                  !
     input_filenameC = TRIM(ADJUSTL(input_filenameC))                  !
     DO i=line_length+1,LEN_TRIM(input_filenameC)                      ! starting at comment sign, replace all characters by blanks
       input_filenameC(i:i) =" "                                       ! replace comment text by blanks
     END DO                                                            !
    input_filenameC = TRIM(ADJUSTL(input_filenameC))                   !
    ipos           = SCAN(input_filenameC,spec_char)                   ! scan for spec_char sign (blank). No comment sign result: ipos=0.
     DO i=ipos,LEN(input_filenameC)                                    ! starting at comment sign, replace all characters by blanks
       if (ipos == 0) EXIT                                             ! no comment sign found, all text is input
       input_filenameC(i:i) =" "                                       ! replace comment text by blanks
     END DO                                                            !
    IF(input_filenameC(1:1) == key_char) input_filenameC = " "         !
    input_filenameC = TRIM(ADJUSTL(input_filenameC))                   !
    END DO                                                             !
 CLOSE(10)

!------------------------------------------------------------------------------
 END SUBROUTINE GetInputFileNameFromValidatorFile
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_InputFileName
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_string_in_list
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE string_in_list(Data_len,key_char,keywords,keyC_in,found_keyL,specC_in,found_specL, &
                           data_typeC_out,optional_specifierL_out,choiceL_out,choiceC_out)
!------------------------------------------------------------------------------
!
!++s* mod_string_to_value/string_in_list
!
! NAME
!   SUBROUTINE string_in_list
!
! PURPOSE
!   Checks if a particular keyword or a particular specifier for a keyword is possible.
!
! USAGE
!   CALL string_in_list(Data_len,key_char,keywords,keyC_in,found_keyL,specC_in,found_specL,data_type,optional_specifierL,choiceL_out,choiceC_out)
! 
! INPUT
!   o Data_len:
!   o key_char:
!   o keywords:
!   o keyC_in:
!   o specC_in:
!
! OUTPUT
!   o found_keyL:
!   o found_specL:         (optional)
!   o data_type:           (optional)
!   o optional_specifierL: (optional)
!   o choiceL_out:         (optional)
!   o choiceC_out:         (optional)
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE input_type_names       ,ONLY:char_length_type_name, &
                                  char_length_choice_name
 USE mod_check_keyword      ,ONLY:check_keyword
 USE mod_check_specifier    ,ONLY:check_specifier
 USE keyword_queue_def      ,ONLY:keyword_queue
 USE keyword_node_def       ,ONLY:keyword_node

 IMPLICIT NONE

 INTEGER            ,INTENT(in)           :: Data_len
 CHARACTER(len=1)   ,INTENT(in)           :: key_char
 TYPE(keyword_queue),POINTER              :: keywords
 CHARACTER(len=*)   ,INTENT(in)           :: keyC_in
 LOGICAL            ,INTENT(out)          :: found_keyL
 CHARACTER(len=*)   ,INTENT(in) ,OPTIONAL :: specC_in
 LOGICAL            ,INTENT(out),OPTIONAL :: found_specL
 CHARACTER(len=*)   ,INTENT(out),OPTIONAL :: data_typeC_out                    !
 LOGICAL            ,INTENT(out),OPTIONAL :: optional_specifierL_out           !
 LOGICAL            ,INTENT(out),OPTIONAL :: choiceL_out
 CHARACTER(len=*)   ,INTENT(out),OPTIONAL :: choiceC_out

 CHARACTER(len=:)   ,ALLOCATABLE          :: keyC
 CHARACTER(len=:)   ,ALLOCATABLE          :: specC
 CHARACTER(len=char_length_type_name)     :: data_typeC
 LOGICAL                                  :: optional_specifierL
 LOGICAL                                  :: ChoiceL
 CHARACTER(len=char_length_choice_name)   :: ChoiceC

 TYPE(keyword_node) ,POINTER,SAVE         :: a1                                !
 INTEGER,PARAMETER                        :: Data_len_temp =  267
 CHARACTER(Data_len_temp),SAVE            :: last_keyC                         ! The SAVE attribute does not accept 'Data_len' as input to this subroutine.
 LOGICAL,SAVE                             :: first_call = .TRUE.

 IF (first_call) last_keyC  = ""                                       !
 IF (first_call) first_call = .FALSE.                                  !

 keyC = TRIM( ADJUSTL(keyC_in) )

 !----------------------------------------------------------------------------------
 ! Check for invalid input, i.e. check first character of keyword which is special.
 !----------------------------------------------------------------------------------
 IF (keyC(1:1) /= key_char) CALL ERROR(1)                           !


   IF (.NOT.PRESENT(specC_in) .AND. .NOT.PRESENT(found_specL)) THEN
      !--------------------------------------------------------------------------------------
      ! If these two optional arguments are not present, then we only check for the keyword.
      !--------------------------------------------------------------------------------------

      !--------------------------
      ! Check for invalid input.
      !--------------------------
      IF (PRESENT(data_typeC_out) .OR. PRESENT(optional_specifierL_out) .OR. &
          PRESENT(choiceL_out)    .OR. PRESENT(choiceC_out)) CALL ERROR(3)

       !-----------------------------------------------------------
       ! Check if keyword 'keyC' is found within list of keywords.
       !-----------------------------------------------------------
       CALL check_keyword(keywords,keyC, found_keyL,a1)
       IF (found_keyL) THEN
           last_keyC = keyC                               !
       ELSE 
           last_keyC = ""                                 !
       END IF

   ELSE IF (PRESENT(specC_in) .AND.      PRESENT(found_specL)) THEN         !
     specC = TRIM( ADJUSTL(specC_in) )

      !--------------------------
      ! Check for invalid input.
      !--------------------------
      IF (keyC == last_keyC) THEN                                         !
         found_keyL = .TRUE.
                            !----------------------------------------------------
                            ! Check if specifier 'spec' is found within keyword.
                            !----------------------------------------------------
                            CALL check_specifier(specC,Data_len, &
                                                 found_specL,data_typeC,optional_specifierL,ChoiceL,ChoiceC,a1)
      ELSE                                                              !
       !-----------------------------------------------------------
       ! Check if keyword 'keyC' is found within list of keywords.
       !-----------------------------------------------------------
       CALL check_keyword(keywords,keyC, found_keyL,a1)
       IF (     found_keyL) THEN
          last_keyC = keyC
                            !----------------------------------------------------
                            ! Check if specifier 'spec' is found within keyword.
                            !----------------------------------------------------
                            CALL check_specifier(specC,Data_len, &
                                                 found_specL,data_typeC,optional_specifierL,ChoiceL,ChoiceC,a1)
       ELSE
          last_keyC = ""
       END IF
      END IF                                                            !
   ELSE                                                                !
     CALL ERROR(2)                                                     !
   END IF                                                              !

 IF ( PRESENT(         data_typeC_out) )          data_typeC_out = data_typeC
 IF ( PRESENT(optional_specifierL_out) ) optional_specifierL_out = optional_specifierL
 IF ( PRESENT(            choiceL_out) )             choiceL_out = choiceL
 IF ( PRESENT(            choiceC_out) )             choiceC_out = choiceC

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE ERROR(ierr)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

 INTEGER,INTENT(in)  :: ierr

    IF (ierr == 1) THEN                                                 !
     WRITE(my_output_unit,*)'>>> - ERROR - <<< >>> - ERROR - <<< >>> - ERROR - <<<' !
     WRITE(my_output_unit,*)'>>> invalid usage of subroutine string_in_list    <<<' !
     WRITE(my_output_unit,*)'>>> dummy argument >>keyC<< must start with a ',       &
                key_char,' sign <<<'                                   !
     STOP                                                              !
    ELSE IF (ierr == 2) THEN                                            !
     WRITE(my_output_unit,*)'>>> - ERROR - <<< >>> - ERROR - <<< >>> - ERROR -  <<<'!
     WRITE(my_output_unit,*)'>>> invalid usage of subroutine string_in_list     <<<'!
     WRITE(my_output_unit,*)'>>> the first two or all four optional aruments    <<<'!
     WRITE(my_output_unit,*)'>>> have to be present or not<<<'                      !
     STOP                                                              !
    ELSE IF (ierr == 3) THEN                                            !
     WRITE(my_output_unit,*)'>>> - ERROR - <<< >>> - ERROR - <<< >>> - ERROR -  <<<'!
     WRITE(my_output_unit,*)'>>> invalid usage of subroutine string_in_list     <<<'!
     WRITE(my_output_unit,*)'>>> if optional arguments for data_type and logical<<<'!
     WRITE(my_output_unit,*)'>>> flag for optionality of specifier are used,    <<<'!
     WRITE(my_output_unit,*)'>>> also both dummy arguments for specifier and    <<<'!
     WRITE(my_output_unit,*)'>>> and its appearance in the queue must be present<<<'!
     STOP                                                              !
    ELSE                                                               !
     WRITE(my_output_unit,*)'>>> - ERROR - <<< >>> - ERROR - <<< >>> - ERROR -  <<<'!
     WRITE(my_output_unit,*)'>>> invalid call to error subroutine occured in    <<<'!
     WRITE(my_output_unit,*)'>>> subroutine string_in_list                      <<<'!
     WRITE(my_output_unit,*)'>>> do not know which error you mean with ierr > 3  <<<'!
     STOP                                                              !
    END IF                                                             !

!------------------------------------------------------------------------------
 END SUBROUTINE ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE string_in_list
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_string_in_list
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE array_type_specifiers                                           ! define derived data type for one node in queue of specifiers
!------------------------------------------------------------------------------
 USE input_type_names       ,ONLY:char_length_type_name, &
                                  char_length_specifier_name

 IMPLICIT NONE

   TYPE :: in_array_node                                               !
     CHARACTER(char_length_specifier_name) :: spinar                   ! specifier names for integer numbers
     CHARACTER(char_length_type_name)      :: tyinar                   ! data type expected for specifiers
     LOGICAL               :: opinar                                   ! logical values for is_it_an_optional_input_specifier
     LOGICAL               :: prinar                                   ! logical values for is_input_specifier_present_in_current_entry
     INTEGER               :: liinar                                   ! value found in line number of input file
     INTEGER,DIMENSION(:),POINTER :: inar                              ! array with integer values
     TYPE(in_array_node) ,POINTER :: next_inar                         ! pointer to successor node in queue
   END TYPE in_array_node                                              !
   TYPE :: in_array_queue                                              !
     TYPE(in_array_node), POINTER :: top                               ! pointer to top  node of specifier queue
     TYPE(in_array_node), POINTER :: rear                              ! pointer to top  node of specifier queue
   END TYPE in_array_queue                                             !
!----------------------------------------------------------------------!
   TYPE :: xs_array_node                                               !
     CHARACTER(char_length_specifier_name) :: spxsar                   ! specifier names for integer numbers
     CHARACTER(char_length_type_name)      :: tyxsar                   ! data type expected for specifiers
     LOGICAL               :: opxsar                                   ! logical values for is_it_an_optional_input_specifier
     LOGICAL               :: prxsar                                   ! logical values for is_input_specifier_present_in_current_entry
     INTEGER               :: lixsar                                   ! value found in line number of input file
     REAL(4),DIMENSION(:),POINTER :: xsar                     ! array with integer values
     TYPE(xs_array_node) ,POINTER :: next_xsar                         ! pointer to successor node in queue
   END TYPE xs_array_node                                              !
   TYPE :: xs_array_queue                                              !
     TYPE(xs_array_node), POINTER :: top                               ! pointer to top  node of specifier queue
     TYPE(xs_array_node), POINTER :: rear                              ! pointer to top  node of specifier queue
   END TYPE xs_array_queue                                             !
!----------------------------------------------------------------------!
   TYPE :: xd_array_node                                               !
     CHARACTER(char_length_specifier_name) :: spxdar                   ! specifier names for integer numbers
     CHARACTER(char_length_type_name) :: tyxdar                        ! data type expected for specifiers
     LOGICAL               :: opxdar                                   ! logical values for is_it_an_optional_input_specifier
     LOGICAL               :: prxdar                                   ! logical values for is_input_specifier_present_in_current_entry
     INTEGER               :: lixdar                                   ! value found in line number of input file
     REAL(8),DIMENSION(:),POINTER :: xdar                     ! array with integer values
     TYPE(xd_array_node) ,POINTER :: next_xdar                         ! pointer to successor node in queue
   END TYPE xd_array_node                                              !
   TYPE :: xd_array_queue                                              !
     TYPE(xd_array_node), POINTER :: top                               ! pointer to top  node of specifier queue
     TYPE(xd_array_node), POINTER :: rear                              ! pointer to top  node of specifier queue
   END TYPE xd_array_queue                                             !
!------------------------------------------------------------------------------
END MODULE array_type_specifiers                                       ! define derived data types for queues of array type specifiers
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE generic_add
!------------------------------------------------------------------------------

  INTERFACE add_array_element
     MODULE PROCEDURE  add_inar_element    ! integer array
     MODULE PROCEDURE  add_xsar_element    ! real (single) array
     MODULE PROCEDURE  add_xdar_element    ! double array
  END INTERFACE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE add_inar_element(x,in_t)
!------------------------------------------------------------------------------
 USE array_type_specifiers

 IMPLICIT NONE

 TYPE(in_array_node) ,POINTER    :: x
 INTEGER             ,INTENT(in) :: in_t

 INTEGER,DIMENSION(:),POINTER    :: y
 INTEGER                         :: i,isize

 IF (ASSOCIATED(x%inar)) THEN                                          !
  isize = SIZE(x%inar)+1                                               !
  ALLOCATE(y(isize))                                                   !
  DO i=1,isize-1                                                       !
  y(i) = x%inar(i)                                                     !
  END DO                                                               !
 ELSE                                                                  !
  isize = 1                                                            !
  ALLOCATE(y(isize))                                                   !
 END IF                                                                !
 y(isize) = in_t                                                       !
 NULLIFY(x%inar)                                                       !
 ALLOCATE(x%inar(isize))                                               !
 x%inar = y                                                            !
 DEALLOCATE(y)                                                         !

!------------------------------------------------------------------------------
END SUBROUTINE add_inar_element
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE add_xsar_element(x,xs_t)
!------------------------------------------------------------------------------
 USE array_type_specifiers

 IMPLICIT NONE

 TYPE(xs_array_node) ,POINTER    :: x
 REAL(4)             ,INTENT(in) :: xs_t

 REAL(4),DIMENSION(:),POINTER    :: y
 INTEGER                         :: i,isize

 IF (ASSOCIATED(x%xsar)) THEN                                          !
  isize = SIZE(x%xsar)+1                                               !
  ALLOCATE(y(isize))                                                   !
  DO i=1,isize-1                                                       !
  y(i) = x%xsar(i)                                                     !
  END DO                                                               !
 ELSE                                                                  !
  isize = 1                                                            !
  ALLOCATE(y(isize))                                                   !
 END IF                                                                !
 y(isize) = xs_t                                                       !
 NULLIFY(x%xsar)                                                       !
 ALLOCATE(x%xsar(isize))                                               !
 x%xsar = y                                                            !
 DEALLOCATE(y)                                                         !

!------------------------------------------------------------------------------
END SUBROUTINE add_xsar_element
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE add_xdar_element(x,xd_t)
!------------------------------------------------------------------------------
 USE array_type_specifiers

 IMPLICIT NONE

 TYPE(xd_array_node) ,POINTER    :: x
 REAL(8)             ,INTENT(in) :: xd_t

 REAL(8),DIMENSION(:),POINTER    :: y
 INTEGER                         :: i,isize

 IF (ASSOCIATED(x%xdar)) THEN                                          !
  isize = SIZE(x%xdar)+1                                               !
  ALLOCATE(y(isize))                                                   !
  DO i=1,isize-1                                                       !
  y(i) = x%xdar(i)                                                     !
  END DO                                                               !
 ELSE                                                                  !
  isize = 1                                                            !
  ALLOCATE(y(isize))                                                   !
 END IF                                                                !
 y(isize) = xd_t                                                       !
 NULLIFY(x%xdar)                                                       !
 ALLOCATE(x%xdar(isize))                                               !
 x%xdar = y                                                            !
 DEALLOCATE(y)                                                         !

!------------------------------------------------------------------------------
 END SUBROUTINE add_xdar_element
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE generic_add
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_key_positions
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE key_positions(key_char,end_key_char,bufferC,key_pos,end_key_pos,NumberOfKeywordsInLine)
!------------------------------------------------------------------------------
!
!++s* mod_key_positions/key_positions
!
! NAME
!   SUBROUTINE key_positions
!
! PURPOSE
!   This subroutine determines the keyword positions in input line.
!
! USAGE
!   CALL key_positions(key_char,end_key_char,bufferC, key_pos,end_key_pos,NumberOfKeywordsInLine)
! 
! INPUT
!   o key_char:                  '$'
!   o end_key_char:              ' '
!   o bufferC:                   input line
!
! OUTPUT
!   o key_pos:                   key_pos(i) gives the starting position which is determined by the separating character key_char '$'.
!   o end_key_pos:               end_key_pos(i) is last character position of keyword i (Note: end_key_char = ' ')
!   o NumberOfKeywordsInLine:
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=1)    ,INTENT(in)             :: key_char
 CHARACTER(len=1)    ,INTENT(in)             :: end_key_char
 CHARACTER(len=*)    ,INTENT(in)             :: bufferC
 INTEGER,DIMENSION(:),POINTER                :: key_pos                             ! OUTPUT
 INTEGER,DIMENSION(:),POINTER                :: end_key_pos                         ! OUTPUT
 INTEGER             ,INTENT(out)            :: NumberOfKeywordsInLine              !

 INTEGER                            :: icount, i, j, length                ! local variables

   length = LEN_TRIM(bufferC)                                           !
   icount = 0                                                          !
   NumberOfKeywordsInLine = 0                                          !
   DO i=1,length                                                       !
    IF (bufferC(i:i)==key_char) icount = icount + 1                     ! Count number of '$' signs in line.
   END DO                                                              !
   NumberOfKeywordsInLine = icount
                                                                       !
   IF (NumberOfKeywordsInLine > 0) THEN
    !----------------------------------------------------
    ! Pointers can become disassociated by deallocation.
    !----------------------------------------------------
    IF (ASSOCIATED(key_pos))     DEALLOCATE(key_pos)                   !
    IF (ASSOCIATED(end_key_pos)) DEALLOCATE(end_key_pos)               !
    ALLOCATE(    key_pos(NumberOfKeywordsInLine))                      !
    ALLOCATE(end_key_pos(NumberOfKeywordsInLine))                      !
     icount = 0                                                        !
     DO i=1,length                                                     !
      IF(bufferC(i:i)==key_char) THEN                                   !
       icount = icount + 1                                             !
       key_pos(icount) = i                                             !
      END IF                                                           !
     END DO                                                            !
   END IF                                                              !
                                                                       !
   DO i=1,NumberOfKeywordsInLine
    DO j=key_pos(i),length                                             !
     IF(bufferC(j:j)==end_key_char) EXIT                                !
    END DO                                                             !
    end_key_pos(i) = j-1                                               !
   END DO                                                              !

!------------------------------------------------------------------------------
 END SUBROUTINE key_positions
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE spec_positions(spec_char,end_spec_char,bufferC,buffera,spec_pos,end_spec_pos,NumOfSpecifiersInLine)
!------------------------------------------------------------------------------
!
!++s* mod_key_positions/spec_positions
!
! NAME
!   SUBROUTINE spec_positions
!
! PURPOSE
!   This subroutine determines the specifier positions in input line.
!
! USAGE
!   CALL spec_positions(spec_char,end_spec_char,bufferC,buffera, spec_pos,end_spec_pos,NumOfSpecifiersInLine)
! 
! INPUT
!   o spec_char:                  ' '
!   o end_spec_char:              '='
!   o bufferC:                    input line
!   o buffera:                   It is tested if the line 'buffera(1:1)' begins with '='.
!
! OUTPUT
!   o spec_pos:                  spec_pos(i) gives the starting position
!   o end_spec_pos:              end_spec_pos(i) is last character position of specifier i (Note: end_spec_char = '=')
!   o NumOfSpecifiersInLine:
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   CHARACTER(len=*),INTENT(in)    :: spec_char                         ! ' '   Character which separates a specifier (must be a blank)
   CHARACTER(len=*),INTENT(in)    :: end_spec_char                     ! '='   Character which specifies end of specifier (can be chosen)
   CHARACTER(len=*),INTENT(in)    :: bufferC                            !
   CHARACTER(len=*),INTENT(in)    :: buffera                           !
   INTEGER,DIMENSION(:),POINTER   :: spec_pos                          ! OUTPUT
   INTEGER,DIMENSION(:),POINTER   :: end_spec_pos                      ! OUTPUT
   INTEGER         ,INTENT(out)   :: NumOfSpecifiersInLine
                                                                       !
   INTEGER                      :: icount, i, j, length                ! local variables
   INTEGER                      :: k                                   ! local variable
   INTEGER                      :: one_more                            ! local variables

   length  = LEN_TRIM(bufferC)                                          !
   icount  = 0                                                         !
   NumOfSpecifiersInLine = 0
   DO i=1,length                                                       !
    IF (bufferC(i:i)==end_spec_char) icount = icount + 1                ! Count number of '=' signs in line 'bufferC'
   END DO                                                              !
   NumOfSpecifiersInLine = icount
                                                                       !
   one_more = 0                                                        !
   IF (buffera(1:1)==end_spec_char) one_more = 1                       ! Count number of '=' signs in line 'buffera(1:1)'
   NumOfSpecifiersInLine = NumOfSpecifiersInLine + one_more
                                                                       !
   IF (NumOfSpecifiersInLine > 0) THEN
    !----------------------------------------------------
    ! Pointers can become disassociated by deallocation.
    !----------------------------------------------------
    IF(ASSOCIATED(    spec_pos)) DEALLOCATE(    spec_pos)              !
    IF(ASSOCIATED(end_spec_pos)) DEALLOCATE(end_spec_pos)              !
    ALLOCATE(    spec_pos(NumOfSpecifiersInLine))
    ALLOCATE(end_spec_pos(NumOfSpecifiersInLine))
     icount = 0                                                        !
     DO i=1,length                                                     !
      IF(bufferC(i:i)==end_spec_char) THEN                              !
       icount = icount + 1                                             !
       end_spec_pos(icount) = i                                        !
      END IF                                                           !
     END DO                                                            !
   END IF                                                              !
   DO i=1,NumOfSpecifiersInLine-one_more
    DO j=end_spec_pos(i)-1,0,-1                                        !
     IF (j==0) THEN                                                    !
      spec_pos(i) = 0                                                  ! if specifier string is in previous line, set position to zero
      EXIT                                                             !
     END IF                                                            !
      IF (bufferC(j:j)/=" ") EXIT                                       ! scan for first nonblank character in front of specifier end separator
    ENDDO                                                              !
    DO k=j,1,-1                                                        !
      spec_pos(i) = k                                                  ! position start of specifier at line start, if line starts with specifier
     IF (bufferC(k:k)==spec_char) THEN                                  !
      spec_pos(i) = k+1                                                !
      EXIT                                                             !
     END IF                                                            !
    END DO                                                             !
   END DO                                                              !
                                                                       !
   IF (one_more == 1) THEN                                             !
    end_spec_pos(NumOfSpecifiersInLine) = length
    DO k=length,1,-1                                                   !
      spec_pos(NumOfSpecifiersInLine) = k                              ! position start of specifier at line start, if line starts with specifier
     IF (bufferC(k:k)==spec_char) THEN                                  !
      spec_pos(NumOfSpecifiersInLine) = k+1                            !
      EXIT                                                             !
     END IF                                                            !
    END DO                                                             !
   END IF                                                              !

!------------------------------------------------------------------------------
 END SUBROUTINE spec_positions
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_key_positions
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_string_to_value
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE convert_string_to_value(keyword_filetypeC,data_typeC,SpecifierValueC_in,line_number,specifierC, &
                                    xs_t,xd_t,in_t,ca_t,lo_t)
!------------------------------------------------------------------------------
!
!++s* mod_string_to_value/convert_string_to_value
!
! NAME
!   SUBROUTINE convert_string_to_value
!
! PURPOSE
!   Converts the string 'SpecifierValueC' of specifier 'specifierC'
!   in line number 'line_number' to data type 'data_typeC'.
!
! USAGE
!   CALL convert_string_to_value(keyword_filetypeC,data_typeC,SpecifierValueC_in,line_number,specifierC, xs_t,xd_t,in_t,ca_t,lo_t)
! 
! INPUT
!   o keyword_filetypeC:
!   o data_typeC:
!   o SpecifierValueC_in:
!   o line_number:
!   o specifierC:
!   o line_number:
!
! OUTPUT
!   o xs_t
!   o xd_t
!   o in_t
!   o ca_t
!   o lo_t
! 
! NOTES
!
!##
!
!------------------------------------------------------------------------------
 USE CharacterManipulation    ,ONLY:CharacterReplace
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE input_type_names         ,ONLY:name_xs,name_xd,name_in,name_ca, &
                                    name_lo,name_inar,name_xsar,name_xdar

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)    :: keyword_filetypeC
 CHARACTER(len=*),INTENT(in)    :: data_typeC
 CHARACTER(len=*),INTENT(in)    :: SpecifierValueC_in
 INTEGER         ,INTENT(in)    :: line_number
 CHARACTER(len=*),INTENT(in)    :: specifierC
 REAL(4)         ,INTENT(out)   :: xs_t
 REAL(8)         ,INTENT(out)   :: xd_t
 INTEGER         ,INTENT(out)   :: in_t
 LOGICAL         ,INTENT(out)   :: lo_t
 CHARACTER(len=*),INTENT(out)   :: ca_t   ! long strings are supported, e.g. long directory names

 CHARACTER(len=:),ALLOCATABLE   :: SpecifierValueC
 CHARACTER(len=:),ALLOCATABLE   :: SpecifierValue_d0_C
 CHARACTER(len=10),PARAMETER    :: DigitStringC = "0123456789"
 CHARACTER(len=6) ,PARAMETER    :: true   = ".true."                                  !
 CHARACTER(len=6) ,PARAMETER    :: trueC  = ".TRUE."                                  !
 CHARACTER(len=7) ,PARAMETER    :: false  = ".false."                                 !
 CHARACTER(len=7) ,PARAMETER    :: falseC = ".FALSE."                                 !
 INTEGER                        :: ip,ipp,ipe,ipce,ipd,ipcd,mpp                       !
 INTEGER                        :: ipost,iposf
 INTEGER                        :: position
 CHARACTER(len=2),PARAMETER     :: DoubleStringC = 'd0'

 SpecifierValueC = TRIM(SpecifierValueC_in)

 !-----------------------------------------
 ! Check if data type is single precision.
 ! Comment: Sooner or later, 'single precision can probably be eliminated.
 !-----------------------------------------
 IF (TRIM(data_typeC)==TRIM(name_xs) .OR. &
     TRIM(data_typeC)==TRIM(name_xsar)) THEN
     !-----------------------------------------------------------------------
     ! Check if first character is 'e' or 'E'. This is not a correct format.
     !-----------------------------------------------------------------------
     IF (SpecifierValueC(1:1)=='e')                               CALL TYPE_ERROR !
     IF (SpecifierValueC(1:1)=='E')                               CALL TYPE_ERROR !
     !------------------------------------------------------------------------------------
     ! Check if last character is 'e' or 'E' or '+' or '-'. This is not a correct format.
     !------------------------------------------------------------------------------------
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='e') CALL TYPE_ERROR !
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='E') CALL TYPE_ERROR !
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='-') CALL TYPE_ERROR !
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='+') CALL TYPE_ERROR !
     ipp=0; ipe=0; ipce=0; mpp=0
     DO ip = 1,LEN_TRIM(SpecifierValueC)                                         !
      !-------------------------------------
      ! Loop over all characters in string.
      !-------------------------------------
      IF      (SpecifierValueC(ip:ip)=='.') THEN  ! Count number of '.'
       ipp = ipp+1                                                     !
       mpp = ip                                   ! Store location of '.'
      ELSE IF (SpecifierValueC(ip:ip)=='e') THEN  ! Count number of 'e'
       ipe = ipe+1                                                     !
       IF (ipp > 0) THEN                                               !
        IF (mpp > ip) CALL TYPE_ERROR                                  !
       END IF                                                          !
      ELSE IF (SpecifierValueC(ip:ip)=='E') THEN  ! Count number of 'E'
       ipce = ipce+1                                                   !
       IF (ipp > 0) THEN                                               !
        IF (mpp > ip) CALL TYPE_ERROR                                  !
       END IF                                                          !
      ELSE IF ( (SpecifierValueC(ip:ip)=='+') .OR. &
                (SpecifierValueC(ip:ip)=='-') ) THEN
        IF (ip /= 1) THEN ! If ip == 0, then '+' or '-' sign is the first character which is fine.
         !-----------------------------------------------------
         ! The first character following the 'E' or 'e' symbol
         ! is allowed to be a '-' or '+' sign.
         !-----------------------------------------------------
         IF( (SpecifierValueC(ip-1:ip-1)/='e') .AND. &
             (SpecifierValueC(ip-1:ip-1)/='E'))THEN
          CALL TYPE_ERROR                                              !
         END IF                                                        !
        END IF                                                         !
      ELSE                                                             !
       IF (SCAN(DigitStringC,SpecifierValueC(ip:ip))==0) CALL TYPE_ERROR ! Error if character is neither '+', '-', a digit nor 'E' or 'e'.
      END IF                                                           !
     END DO                                                            !
     !--------------------------
     ! Produce an error if ...
     !--------------------------
     IF ( ipe+ipce      > 1 ) CALL TYPE_ERROR  ! i.e. two or more occurences of 'E'/'e'
     IF ( ipp           > 1 ) CALL TYPE_ERROR  ! i.e. two or more occurences of '.'
     IF ( ipp+ipe+ipce == 0 ) THEN
      !----------------------------------------------------
      ! Produce an error if no '.', 'e' or 'E' is present.
      !----------------------------------------------------
                              CALL TYPE_ERROR  ! i.e. no '.', no 'e' and no 'E' in string. (This could be an integer.) 
     END IF
     READ( SpecifierValueC , * ) xs_t

 !-----------------------------------------
 ! Check if data type is double precision.
 !-----------------------------------------
 ELSE IF (TRIM(data_typeC)==TRIM(name_xd) .OR. &
          TRIM(data_typeC)==TRIM(name_xdar)) THEN

     !****************************************************************
     !----------------------------------------------------------------
     ! Here we add a new feature.
     ! Namely, we now allow the single precision format
     !        1.0E0, 1.0e0, ...
     ! to be a valid format for the required double precision format:
     !        1.0D0, 1.0d0, ...
     ! We simply convert all 'e'/'E' characters into 'd'/'D'.
     !----------------------------------------------------------------

       !----------------------------------------------------------------
       ! SCAN: Scans a string for any character in a set of characters.
       !----------------------------------------------------------------

       !----------------------------------------------------------------------------------
       ! Get position of 'e' (e0) or 'E' (E0) and replace it appropriately with 'd' (d0).
       !----------------------------------------------------------------------------------
       position = SCAN (SpecifierValueC, 'e')
       IF (position /= 0) CALL CharacterReplace (SpecifierValueC, 'e', 'd' )

       position = SCAN (SpecifierValueC, 'E')
       IF (position /= 0) CALL CharacterReplace (SpecifierValueC, 'E', 'D' )

     !****************************************************************


     !-----------------------------------------------------------------------
     ! Check if first character is 'd' or 'D'. This is not a correct format.
     !-----------------------------------------------------------------------
     IF (SpecifierValueC(1:1)=='d')                               CALL TYPE_ERROR !
     IF (SpecifierValueC(1:1)=='D')                               CALL TYPE_ERROR !
     !------------------------------------------------------------------------------------
     ! Check if last character is 'd' or 'D' or '+' or '-'. This is not a correct format.
     !------------------------------------------------------------------------------------
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='d') CALL TYPE_ERROR !
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='D') CALL TYPE_ERROR !
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='-') CALL TYPE_ERROR !
     IF (SpecifierValueC(LEN_TRIM(SpecifierValueC):LEN_TRIM(SpecifierValueC))=='+') CALL TYPE_ERROR !
     ipp=0; ipd=0; ipcd=0; mpp=0
     DO ip = 1,LEN_TRIM(SpecifierValueC)                                         !
      !-------------------------------------
      ! Loop over all characters in string.
      !-------------------------------------
      IF      (  SpecifierValueC(ip:ip)=='.' )   THEN  ! Count number of '.'
       ipp = ipp+1                                                     !
       mpp = ip                                        ! Store location of '.'
      ELSE IF (  SpecifierValueC(ip:ip)=='d' )   THEN  ! Count number of 'd'
       ipd = ipd+1                                                     !
       IF (ipp > 0) THEN                                               !
        IF (mpp > ip) CALL TYPE_ERROR                                  !
       END IF                                                          !
      ELSE IF (  SpecifierValueC(ip:ip)=='D' )   THEN  ! Count number of 'D'
       ipcd = ipcd+1                                                   !
       IF (ipp > 0) THEN                                               !
        IF (mpp > ip) CALL TYPE_ERROR                                  !
       END IF                                                          !
      ELSE IF (( SpecifierValueC(ip:ip)=='+' ) .OR. &
               ( SpecifierValueC(ip:ip)=='-' ) ) THEN
        IF (ip /= 1 ) THEN ! If ip == 0, then '+' or '-' sign is the first character which is fine.
         !-----------------------------------------------------
         ! The first character following the 'D' or 'd' symbol
         ! is allowed to be a '-' or '+' sign.
         !-----------------------------------------------------
         IF (( SpecifierValueC(ip-1:ip-1) /= 'd') .AND. &
             ( SpecifierValueC(ip-1:ip-1) /= 'D')) THEN
          CALL TYPE_ERROR                                              !
         END IF                                                        !
        END IF                                                         !
      ELSE                                                             !
       IF (SCAN(DigitStringC,SpecifierValueC(ip:ip)) == 0) CALL TYPE_ERROR ! Error if character is neither '+', '-', a digit nor 'D' or 'd'.
      END IF                                                           !
     END DO                                                            !
     !--------------------------
     ! Produce an error if ...
     !--------------------------
     IF (      ipd+ipcd  > 1 ) CALL TYPE_ERROR  ! i.e. two or more occurences of 'D'/'d'
     IF (  ipp           > 1 ) CALL TYPE_ERROR  ! i.e. two or more occurences of '.'

     IF (  ipp+ipd+ipcd == 0 ) THEN
    ! !----------------------------------------------------
    ! ! Produce an error if no '.', 'd' or 'D' is present.
    ! !----------------------------------------------------
    ! CALL TYPE_ERROR  ! i.e. no '.', no 'd' and no 'D' in string. (This could be an integer.)
      SpecifierValue_d0_C = TRIM(SpecifierValueC)//DoubleStringC  ! Add 'd0' to integer string to convert it to double precision.
      READ( SpecifierValue_d0_C , * ) xd_t

     ELSE IF ( (ipd<1) .AND. (ipcd<1)) THEN
    !  !-----------------------------------------------
    !  ! Produce an error if no 'd' or 'D' is present.
    !  !-----------------------------------------------
    !  CALL TYPE_ERROR  ! i.e. no occurence of 'd'/'D'. (This could be a single precision number, e.g. '1.0' instead of '1.0d0'.)
      SpecifierValue_d0_C = TRIM(SpecifierValueC)//DoubleStringC  ! Add 'd0' to integer string to convert it to double precision.
      READ( SpecifierValue_d0_C , * ) xd_t

     ELSE
      READ(      SpecifierValueC                 , * ) xd_t
     END IF

 !-----------------------------------------
 ! Check if data type is integer.
 !-----------------------------------------
 ELSE IF (TRIM(data_typeC)==TRIM(name_in) .OR.                      &
          TRIM(data_typeC)==TRIM(name_inar)    ) THEN                  !
     DO ip = 1,LEN_TRIM(SpecifierValueC)
       !-------------------------------------
       ! Loop over all characters in string.
       !-------------------------------------
       IF ( SCAN(DigitStringC,SpecifierValueC(ip:ip)) == 0 ) THEN
        !------------------------------------------
        ! Check if 'character' is not an integer.
        !------------------------------------------
        IF (ip /= 1) THEN
         WRITE(my_output_unit,*) "                ip     = ",ip
         WRITE(my_output_unit,*) "SpecifierValueC(ip:ip) = ",SpecifierValueC(ip:ip)
         WRITE(my_output_unit,*) "SpecifierValueC(1:1)   = ",SpecifierValueC(1:1)
         WRITE(my_output_unit,*) "SpecifierValueC        = ",SpecifierValueC
         CALL TYPE_ERROR
        ELSE IF ( (SpecifierValueC(ip:ip) /= '-') .AND. &
                  (SpecifierValueC(ip:ip) /= '+') ) THEN
         !---------------------------------------------------------------------
         ! The first character is allowed to be a '-' or '+' sign.
         ! If it is neither '-' nor '+', nor an integer, an error is returned.
         !---------------------------------------------------------------------
         WRITE(my_output_unit,*) "SpecifierValueC(ip:ip) = ",SpecifierValueC(ip:ip)
         WRITE(my_output_unit,*) "SpecifierValueC(1:1)   = ",SpecifierValueC(1:1)
         WRITE(my_output_unit,*) "SpecifierValueC        = ",SpecifierValueC
         CALL TYPE_ERROR                                               !
        END IF                                                         !
       END IF                                                          !
     END DO                                                            !
     READ(SpecifierValueC,*) in_t  ! Note: in_t is an integer. It is not an integer array.

 !-----------------------------------------
 ! Check if data type is character.
 !-----------------------------------------
 ELSE IF (TRIM(data_typeC)==TRIM(name_ca)) THEN
     ca_t = TRIM(SpecifierValueC)

 !-----------------------------------------
 ! Check if data type is logical.
 !-----------------------------------------
 ELSE IF (TRIM(data_typeC)==TRIM(name_lo)) THEN
     DO ip = 1,LEN_TRIM(SpecifierValueC)                                         !
       ipost = SCAN(truec ,SpecifierValueC(ip:ip))                               !
       iposf = SCAN(falsec,SpecifierValueC(ip:ip))                               !
       IF((ipost>0).AND.(ip<=6)) SpecifierValueC(ip:ip) = true (ipost:ipost)     !
       IF((iposf>0).AND.(ip<=7)) SpecifierValueC(ip:ip) = false(iposf:iposf)     !
     END DO
       IF ( ( TRIM(SpecifierValueC) /= true   ).AND. &
            ( TRIM(SpecifierValueC) /= false) ) CALL TYPE_ERROR
     READ(SpecifierValueC,*) lo_t
 END IF

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE TYPE_ERROR
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE

  WRITE(my_output_unit,'(A)')   ' >>>>>>> ERROR <<<<< >>>>> ERROR <<<<< >>>>>ERROR<<<<<<<'!
  WRITE(my_output_unit,'(A,A)') ' In internal SUBROUTINE convert_string_to_value called by: ',TRIM(keyword_filetypeC)
  WRITE(my_output_unit,'(A)')   ' SUBROUTINE value_to_queue. Inconsistency of value for  '!
  WRITE(my_output_unit,'(A,A,A,A,A,A)' ) ' specifier ',TRIM(specifierC),' = ',TRIM(SpecifierValueC), &
                                         ' with expected data type = ',TRIM(data_typeC)
  WRITE(my_output_unit,*) 'Input was found in line number = ',line_number,' of ',TRIM(keyword_filetypeC),'.'
 IF ( TRIM(data_typeC) == TRIM(name_inar) ) THEN
  WRITE(my_output_unit,'(A)') ' In fact, input to this subroutine must be each element of the integer array individually,'
  WRITE(my_output_unit,'(A)') ' and not the whole integer array at once.'
 END IF
 STOP

!------------------------------------------------------------------------------
 END SUBROUTINE TYPE_ERROR
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE convert_string_to_value
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_string_to_value
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_Error_control_variables
!------------------------------------------------------------------------------
!  o SUBROUTINE Error_control_variables
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE Error_control_variables(get_data_subroutineC,keyword_filetypeC, &
                                    keywordC,specifierC,line,last_keywordC,newL,contL)
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit
 USE Parser_Errors            ,ONLY:Print_Keyword_Specifier_Line

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: get_data_subroutineC
 CHARACTER(len=*),INTENT(in)  :: keyword_filetypeC
 CHARACTER(len=*),INTENT(in)  :: keywordC
 CHARACTER(len=*),INTENT(in)  :: specifierC
 INTEGER         ,INTENT(in)  :: line
 CHARACTER(len=*),INTENT(in)  :: last_keywordC
 LOGICAL         ,INTENT(in)  :: newL
 LOGICAL         ,INTENT(in)  :: contL

 WRITE(my_output_unit,'(A)') " Error in SUBROUTINE "//TRIM(get_data_subroutineC)//" for "//TRIM(keyword_filetypeC)//"."
 WRITE(my_output_unit,'(A)') " You specified to reuse old keyword = "//TRIM(last_keywordC)
 WRITE(my_output_unit,'(A)') " However, the keyword = "//TRIM(keywordC)//" in the dummy"// &
                             " argument list differs from the keyword previously used."
 WRITE(my_output_unit,'(A)') " Do you really know what you want?"
 WRITE(my_output_unit,'(A)') " Control variables are set as follows:"
 WRITE(my_output_unit,*)     " newL = ",newL,"; contL = ",contL
 CALL Print_Keyword_Specifier_Line(keywordC,specifierC,line,STOP_L=.TRUE.)

!------------------------------------------------------------------------------
 END SUBROUTINE Error_control_variables
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_Error_control_variables
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_log_file
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE GenerateLogFile(ActionC)
!------------------------------------------------------------------------------
!
!++s* DirectoryFileExist/GenerateLogFile
!
! NAME
!   SUBROUTINE GenerateLogFile
!
! PURPOSE
!   Opens, reopens and closes the log file that is generated when using the command line argument '-log', i.e.
!   my_output_unit corresponds to *.log file's output unit.
!   If the command line argument '-log' is not present, standard output is written to
!   my_output_unit = standard output = 6.
!   To be precise, the output of 'my_output_unit' is redirected to a .log file which has the name:
!      <outputfolder>/<inputfilename>.log
!   Code statements like "PRINT *," and "WRITE(*,*)" are written to standard output (=6) in all cases and not to a '.log file.
!   (Note: nextnanomat catches the standard output ('6') and writes it to a different .log file.)
!
! USAGE
!   CALL GenerateLogFile(ActionC)
! 
! INPUT
!   o ActionC:       action that should be done ('open', 'reopen', 'close')
!
! OUTPUT
!   none
! 
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE,INTRINSIC :: ISO_FORTRAN_ENV,ONLY:   output_unit
 USE My_Input_and_Output_Units   ,ONLY:my_output_unit, &
                                          output_unit_log_file
 USE system_specific_parser      ,ONLY:DebugLeveL      , &
                                       GenerateLogFileL, &
                                       LogFilenameC    , &
                                       InputFileName_NoDirectoryNoExtensionC
 USE mod_FileExtensions_parser   ,ONLY:LogC
 USE Input_and_Output_Units      ,ONLY:Redefine_Input_and_Output_Units
 USE DirectoryFileExist          ,ONLY:SetGlobalDirectoryName, &
                                       GetGlobalDirectoryName, &
                                       CopyFile, &
                                       DeleteFile

 CHARACTER(len=*),INTENT(in)  :: ActionC

 CHARACTER(len=:),ALLOCATABLE :: NewLogFilenameC
 INTEGER                      :: error_number
 CHARACTER(len=:),ALLOCATABLE :: LogDirectoryC

 IF (GenerateLogFileL) THEN
  !----------------------------------------------------------------------------
  ! The following only makes sense if a .log file should be written to a file.
  !----------------------------------------------------------------------------

  SELECT CASE( TRIM(ActionC) )
   CASE ('open')   ! This is the filename as passed via command line arguments.

      !-------------------------------------------------------------------------------------
      ! Note: FUNCTION GetGlobalDirectoryName needs 'my_output_unit' which is not open yet.
      !       If the directory does not exist, a message is printed to the standard output.
      !       So we first have to define the 'LogFilenameC',
      !       and then we can call SUBROUTINE Redefine_Input_and_Output_Units.
      !-------------------------------------------------------------------------------------

      !--------------------------------
      ! 1) Define filename of logfile.
      !--------------------------------
      IF ( TRIM(InputFileName_NoDirectoryNoExtensionC) == '') THEN     ! <== If .TRUE., then the name of the input file is not known yet.
          LogDirectoryC = TRIM(SetGlobalDirectoryName(''))
          LogFilenameC  = TRIM(LogDirectoryC)//'log_file'//TRIM(LogC)  ! Therefore we take a dummy file 'log_file.log' that we delete later.
      ELSE
          !------------------------------------------------------------------------
          ! Get log file name including output directory name and input file name.
          !------------------------------------------------------------------------
          LogFilenameC = Get_LogFile_Filename(IncludeDirectoryL=.TRUE.)
      END IF
      IF (DebugLevel > 0) THEN
          !---------------------------------------------------------------------------------------------------------------
          ! Here, the debug level of the input file is not know yet but the command line argument '-debuglevel' is known.
          !---------------------------------------------------------------------------------------------------------------
          !-----------------------------------------------------------------------------------------
          ! Here, 'my_output_unit' is still the default standard output, i.e. not the '*.log' file.
          !-----------------------------------------------------------------------------------------
          WRITE(my_output_unit,'(A,A)') " Writing screen output (standard output) to log file: ",TRIM(LogFilenameC) ! Write out this line which helps debugging if .log file cannot be opened.
      END IF

      !----
      ! 2)
      !----
      !-----------------------------------------------------------------------------------------------------------
      ! We choose unit number 'output_unit_log_file'. Hopefully this number is not defined elsewhere in the code.
      !-----------------------------------------------------------------------------------------------------------
      CALL Redefine_Input_and_Output_Units(new_output_unit = output_unit_log_file)    ! Use modified standard output unit. Note: This number must be UNIQUE. It must not be used elsewhere in the code!!!
    ! CALL Redefine_Input_and_Output_Units(new_output_unit = output_unit)             ! Use default  standard output unit.
      !-------------------------------------------------------------
      ! Check if my standard output unit is different from default.
      !-------------------------------------------------------------
      ! IF (my_output_unit /= output_unit) THEN
          !----------------------------------------------------
          ! The order is important:
          !   1) Open '*.log' file as 'my_output_unit'.
          !   2) Write to 'my_output_unit', i.e. '*.log' file.
          !----------------------------------------------------
          OPEN(UNIT=my_output_unit,FILE=LogFilenameC,IOSTAT=error_number) ! This works nicely if --outputdirectory AND --inputfile is specified via the command line but if it is specified via input file, it is not taken into account here.
          IF (error_number /= 0) THEN
            WRITE(*,'(A,A)') " Error in opening .log file: ",TRIM(LogFilenameC) ! Write to standard output '*' so that the error message can be read.
            STOP
          END IF
         IF ( TRIM(InputFileName_NoDirectoryNoExtensionC) /= '') THEN
           WRITE(my_output_unit,'(A,A)') " Writing screen output (standard output) to log file: ",TRIM(LogFilenameC)
         ELSE
          !--------------------------------------------------------------------------------------------
          ! Here, the name of the input file is not known yet, i.e. dummy file 'log_file.log' is used.
          !--------------------------------------------------------------------------------------------
          IF (DebugLevel > 10) THEN
           WRITE(my_output_unit,'(A,A)') " Writing screen output (standard output) to log file: ",TRIM(LogFilenameC)
          END IF
         END IF
      ! END IF

   CASE ('reopen') ! This is the filename as passed via command line arguments and/or input file.
       !-----------------------------------------------------------------------------------------------
       ! Now, the information of input file name and output directory is defined in any case,
       ! i.e. either via command line or via input file.
       ! Even if it was NOT passed via command line arguments ('--outputdirectory' and '--inputfile').
       !-----------------------------------------------------------------------------------------------
          !------------------------------------------------------------------------
          ! Get log file name including output directory name and input file name.
          !------------------------------------------------------------------------
          NewLogFilenameC = Get_LogFile_Filename(IncludeDirectoryL=.TRUE.)

       IF ( TRIM(NewLogFilenameC) /= &
            TRIM(   LogFilenameC) ) THEN
          !------------------------------------------------------------------
          ! Close old and open new '*.log' file if the names are different.
          !------------------------------------------------------------------
          CLOSE(UNIT=my_output_unit)                                          ! This is the filename as passed via command line arguments.

          !-------------------------------------------------
          ! Reset 'my_output_unit' to standard output unit.
          !-------------------------------------------------
          CALL Redefine_Input_and_Output_Units(new_output_unit=output_unit)   ! Use standard output unit because the .log file has been closed.

          CALL CopyFile(LogFilenameC, &
                        GetGlobalDirectoryName(''),Get_LogFile_Filename(), &
                        ExactCopyL_in=.TRUE.,PrintInfoL_in=.FALSE.) ! PrintInfoL_in must be .FALSE. because my_output_unit is closed.

          !--------------------------------------------------
          ! Reset 'my_output_unit' to .log file output unit.
          !--------------------------------------------------
          CALL Redefine_Input_and_Output_Units(new_output_unit=output_unit_log_file) ! Use customized output unit because the .log file will be opened again.

          OPEN (UNIT=my_output_unit,FILE=NewLogFilenameC,position = 'append',IOSTAT=error_number) ! This is the filename as passed via command line arguments and/or input file.
          IF (error_number /= 0) THEN
            WRITE(*,'(A,A)') " Error in opening log file: ",TRIM(NewLogFilenameC)//'  (reopen)' ! Write to standard output '*' so that the error message can be read.
            STOP
          END IF
          WRITE(my_output_unit,'(A,A)') " Writing screen output (standard output) to log file: ",TRIM(NewLogFilenameC)

          !---------------------------------------------------------
          ! Delete old .log file because it is not needed any more.
          !---------------------------------------------------------
          CALL DeleteFile(LogFilenameC) ! Note: Unit 'my_output_unit' must be open!

       ELSE
          !------------------------------------------------------------------
          ! If the names are identical, we keep the previous .log file open.
          !------------------------------------------------------------------
       END IF

   CASE ('close')
    !---------------------
    ! Close '*.log' file.
    !---------------------
    CLOSE(UNIT=my_output_unit)
    CALL Redefine_Input_and_Output_Units(new_output_unit = output_unit) ! Use default standard output unit again.

   CASE DEFAULT
    WRITE(my_output_unit,*) " ActionC ill-defined. ActionC = ",TRIM(ActionC)
  END SELECT

 END IF

 CONTAINS

!------------------------------------------------------------------------------
 FUNCTION Get_LogFile_Filename(IncludeDirectoryL) RESULT(LogFileC)
!------------------------------------------------------------------------------
! PURPOSE
!   Returns log file name including output directory name and input file name.
!------------------------------------------------------------------------------

 IMPLICIT NONE

 LOGICAL                     ,INTENT(in),OPTIONAL :: IncludeDirectoryL
 CHARACTER(len=:),ALLOCATABLE                     :: LogFileC ! RESULT

 LOGICAL                                          :: include_directoryL

 IF ( PRESENT(IncludeDirectoryL) ) THEN
    include_directoryL = IncludeDirectoryL
 ELSE
    include_directoryL = .FALSE.
 END IF

 !------------------------------------------------------------------------------------
 ! Note: nextnanomat automatically generates a file <input file>.log.
 !       If nextnanomat is executed using '-log' as additional command line argument,
 !       then the executable and nextnanomat are writing to the same filename.
 !       This can be avoided by using '_log.log'. Then the filenames are different.
 !------------------------------------------------------------------------------------
 IF ( include_directoryL ) THEN
  ! LogFileC = TRIM(GetGlobalDirectoryName(''))//TRIM(InputFileName_NoDirectoryNoExtensionC)        //TRIM(LogC) ! This is not good as this is the same filename as the one used by nextnanomat.
    LogFileC = TRIM(GetGlobalDirectoryName(''))//TRIM(InputFileName_NoDirectoryNoExtensionC)//'_log'//TRIM(LogC) ! Here, we make sure that if the argument '-log' is passed via nextnanomat, then the filenames must be different.
 ELSE
    LogFileC =                                   TRIM(InputFileName_NoDirectoryNoExtensionC)//'_log'//TRIM(LogC)
 END IF
!------------------------------------------------------------------------------
 END FUNCTION Get_LogFile_Filename
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END SUBROUTINE GenerateLogFile
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_log_file
!------------------------------------------------------------------------------
