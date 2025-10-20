!------------------------------------------------------------------------------
 MODULE mod_chrpak
!------------------------------------------------------------------------------
!
!++m* convert_int_character.f90/mod_chrpak
!
! NAME 
!   MODULE mod_chrpak
!
! CONTAINS
!   o SUBROUTINE s_chop
!   o SUBROUTINE s_blanks_insert
!   o SUBROUTINE StringReplace
!   o SUBROUTINE s_to_r8
!   o SUBROUTINE CheckIfStringCharacters
!   o SUBROUTINE ch_to_digit
!   o SUBROUTINE digit_to_ch
!   o SUBROUTINE int_to_s_left
!   o SUBROUTINE r8_to_s_left
!   o FUNCTION ch_eqi
!   o SUBROUTINE ch_cap
!
! FILENAME
!   FortranInputParser/convert_int_character.f90
!
! NOTES
!   see e.g.
!   https://people.sc.fsu.edu/~jburkardt/f_src/chrpak/chrpak.html
!   CHRPAK - Characters and Strings
!   CHRPAK is a collection of FORTRAN subroutines and functions for dealing
!   with characters and strings.
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 
 PRIVATE

 PUBLIC StringReplace            ! SUBROUTINE s_replace
 PUBLIC s_to_r8
 PUBLIC CheckIfStringCharacters  ! SUBROUTINE s_to_r8vec
 PUBLIC int_to_s_left
 PUBLIC r8_to_s_left

 CONTAINS

!------------------------------------------------------------------------------
 SUBROUTINE s_chop ( s, ilo, ihi )
!------------------------------------------------------------------------------

!*****************************************************************************80
!
!! S_CHOP "chops out" a portion of a string, and closes up the hole.
!
!  Example:
!
!    S = 'Fred is not a jerk!' 
!
!    call s_chop ( S, 9, 12 ) 
!
!    S = 'Fred is a jerk!    '
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:  
!
!    06 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, integer ( kind = 4 ) ILO, IHI, the locations of the first and last
!    characters to be removed.  
!
  implicit none

  integer   ( kind = 4 ) ihi
  integer   ( kind = 4 ) ihi2
  integer   ( kind = 4 ) ilo
  integer   ( kind = 4 ) ilo2
  character ( len = * )  s
  integer   ( kind = 4 ) s_length

  s_length = len ( s )

  ilo2 = max ( ilo, 1 )
  ihi2 = min ( ihi, s_length )

  if ( ihi2 < ilo2 ) then
    return
  end if

  s(ilo2:s_length+ilo2-ihi2-1) = s(ihi2+1:s_length)
  s(s_length+ilo2-ihi2:s_length) = ' '

!------------------------------------------------------------------------------
 END SUBROUTINE s_chop
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE s_blanks_insert ( s, ilo, ihi )
!------------------------------------------------------------------------------
!*****************************************************************************80
!
!! S_BLANKS_INSERT inserts blanks into a string, sliding old characters over.
!
!  Discussion:
!
!    Characters at the end of the string "drop off" and are lost.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, integer ( kind = 4 ) ILO, the location where the first blank 
!    is to be inserted.
!
!    Input, integer ( kind = 4 ) IHI, the location where the last blank 
!    is to be inserted.
!
  implicit none

  character              ch
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) get
  integer   ( kind = 4 ) ihi
  integer   ( kind = 4 ) ilo
  integer   ( kind = 4 ) imax
  integer   ( kind = 4 ) imin
  integer   ( kind = 4 ) put
  integer   ( kind = 4 ) nmove
  character ( len = * )  s
  integer   ( kind = 4 ) s_length

  s_length = len ( s )
 
  if ( ihi < ilo .or. s_length < ilo ) then
    return
  end if
 
  if ( ihi <= s_length ) then
    imax = ihi
  else
    imax = s_length
  end if
 
  if ( 1 <= ilo ) then
    imin = ilo
  else
    imin = 1
  end if
 
  nmove = s_length - imax
 
  do i = 1, nmove
    put = s_length + 1 - i
    get = s_length - imax + imin - i
    ch = s(get:get)
    s(put:put) = ch
  end do
 
  do i = imin, imax
    s(i:i) = ' '
  end do
 
!------------------------------------------------------------------------------
 END SUBROUTINE s_blanks_insert
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
!SUBROUTINE s_replace     ( s, sub1, sub2, irep_out )
 SUBROUTINE StringReplace ( s, sub1, sub2, irep_out )
!------------------------------------------------------------------------------

!*****************************************************************************80
!
!! S_REPLACE replaces all occurrences of SUB1 by SUB2 in a string.
!
!  Discussion:
!
!    This is not always true if SUB2 is longer than SUB1.  The
!    replacement is NOT recursive.  In other words, replacing all
!    occurrences of "ab" by "a" in "abbbbb" will return "abbbb"
!    rather than "a".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S.  On input,
!    the string in which occurrences are to be replaced.
!    On output, the revised string.
!
!    Input, character ( len = * ) SUB1, the string which is to be replaced.
!    Trailing blank characters are ignored.  The routine is case sensitive.  
!
!    Input, character ( len = * ) SUB2, the replacement string.
!
!    Output, integer ( kind = 4 ) IREP, the number of replacements made.
!    If IREP is negative, then its absolute value is the
!    number of replacements made, and SUB2 is longer than
!    SUB1, and at least one substring SUB1 could not be
!    replaced by SUB2 because there was no more space.
!    (If S = 'aab' and SUB1 = 'a' and SUB2 = 'cc'
!    then the result would be S = 'cca'.  The first 'a'
!    was replaced, the 'b' fell off the end, the second 'a'
!    was not replaced because the replacement 'cc' would have
!    fallen off the end)
!
 USE My_Input_and_Output_Units,ONLY:my_output_unit

  implicit none

  integer   ( kind = 4 ) ilo
  integer   ( kind = 4 ),INTENT(out),OPTIONAL :: irep_out

  integer   ( kind = 4 ) irep
  integer   ( kind = 4 ) len1
  integer   ( kind = 4 ) len2
  integer   ( kind = 4 ) lens
  integer   ( kind = 4 ) loc
  character ( len = * )  s
  character ( len = * )  sub1
  character ( len = * )  sub2

  irep = 0
  IF ( PRESENT(irep_out) ) irep_out = irep ! Initialize.

  lens = len ( s )
 
  if ( lens <= 0 ) then
    WRITE(my_output_unit,'(a)' ) ' '
    WRITE(my_output_unit,'(a)' ) 'S_REPLACE - Serious error!'
    WRITE(my_output_unit,'(a)' ) '  Null string not allowed!'
    return
  end if
 
  len1 = len_trim ( sub1 )
 
  if ( len1 <= 0 ) then
    WRITE(my_output_unit,'(a)' ) ' '
    WRITE(my_output_unit,'(a)' ) 'S_REPLACE - Serious error!'
    WRITE(my_output_unit,'(a)' ) '  Null SUB1 not allowed!'
    return
  end if
 
  len2 = len_trim ( sub2 )
 
  if ( len2 == len1 ) then
 
    if ( sub1(1:len1) == sub2(1:len2) ) then
      WRITE(my_output_unit,'(a)' ) ' '
      WRITE(my_output_unit,'(a)' ) 'S_REPLACE - Warning!'
      WRITE(my_output_unit,'(a)' ) '  Replacement = original!'
      return
    end if
 
    ilo = 1

    do

      loc = index ( s(ilo:lens), sub1(1:len1) )

      if ( loc == 0 ) then
        exit
      end if

      loc = loc + ilo - 1
      irep = irep + 1
      s(loc:loc+len1-1) = sub2(1:len2)
      ilo = loc + len1

      if ( lens < ilo ) then
        exit
      end if

    end do
 
  else if ( len2 < len1 ) then
 
    ilo = 1

    do

      loc = index ( s(ilo:lens), sub1(1:len1) )

      if ( loc == 0 ) then
        exit
      end if

      irep = irep + 1
      loc = loc + ilo - 1
      s(loc:loc+len2-1) = sub2(1:len2)
      call s_chop ( s, loc+len2, loc+len1-1 )
      ilo = loc + len2

      if ( lens < ilo ) then
        exit
      end if

    end do
 
  else
 
    ilo = 1

    do

      loc = index ( s(ilo:lens), sub1(1:len1) )

      if ( loc == 0 ) then
        exit
      end if

      loc = loc + ilo - 1
      irep = irep + 1
 
      if ( lens < loc + len2 - 1 ) then
        irep = -irep
        WRITE(my_output_unit,'(a)' ) ' '
        WRITE(my_output_unit,'(a)' ) 'S_REPLACE - Warning!'
        WRITE(my_output_unit,'(a)' ) '  Some replaceable elements remain!'
        exit
      end if
 
      call s_blanks_insert ( s, loc, loc+len2-1-len1 )

      s(loc:loc+len2-1) = sub2(1:len2)
      ilo = loc + len2

    end do

  end if

 IF ( PRESENT(irep_out) ) irep_out = irep

!------------------------------------------------------------------------------
 END SUBROUTINE StringReplace
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE s_to_r8 ( s, dval, ierror, length )
!------------------------------------------------------------------------------

!*****************************************************************************80
!
!! S_TO_R8 reads an R8 value from a string.
!
!  Discussion:
!
!    An "R8" value is simply a real number to be stored as a
!    variable of type "real ( kind = 8 )".
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 blanks
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon,
!
!    with most quantities optional.
!
!  Example:
!
!    S                 DVAL
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = 8 ) DVAL, the value read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read
!    to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
 USE My_Input_and_Output_Units,ONLY:my_output_unit

  implicit none

  character              c
  real      ( kind = 8 ) dval
  integer   ( kind = 4 ) ierror
  integer   ( kind = 4 ) ihave
  integer   ( kind = 4 ) isgn
  integer   ( kind = 4 ) iterm
  integer   ( kind = 4 ) jbot
  integer   ( kind = 4 ) jsgn
  integer   ( kind = 4 ) jtop
  integer   ( kind = 4 ) length
  integer   ( kind = 4 ) ndig
  real      ( kind = 8 ) rbot
  real      ( kind = 8 ) rexp
  real      ( kind = 8 ) rtop
  character ( len = * )  s
  integer   ( kind = 4 ) s_length
  character           :: TAB = achar ( 9 )

  LOGICAL :: debugL

  debugL = .FALSE.
! debugL = .TRUE.

! WRITE(my_output_unit,*) "s = ",s

  s_length = len_trim ( s )

! WRITE(my_output_unit,*) "s_length = ",s_length

  ierror = 0
  dval = 0.0D+00
  length = -1
  isgn = 1
  rtop = 0
  rbot = 1
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

  do

    length = length + 1

    if ( s_length < length+1 ) then
      exit
    end if

    c = s(length+1:length+1)
!
!  Blank character.
!
    if ( c == ' ' .or. c == TAB ) then

      if ( ihave == 2 ) then

      else if ( ihave == 6 .or. ihave == 7 ) then
        iterm = 1
      else if ( 1 < ihave ) then
        ihave = 11
      end if
!
!  Comma.
!
    else if ( c == ',' .or. c == ';' ) then

      if ( ihave /= 1 ) then
        iterm = 1
        ihave = 12
        length = length + 1
      end if
!
!  Minus sign.
!
    else if ( c == '-' ) then

      if ( ihave == 1 ) then
        ihave = 2
        isgn = -1
      else if ( ihave == 6 ) then
        ihave = 7
        jsgn = -1
      else
        iterm = 1
      end if
!
!  Plus sign.
!
    else if ( c == '+' ) then

      if ( ihave == 1 ) then
        ihave = 2
      else if ( ihave == 6 ) then
        ihave = 7
      else
        iterm = 1
      end if
!
!  Decimal point.
!
    else if ( c == '.' ) then

      if ( ihave < 4 ) then
        ihave = 4
      else if ( 6 <= ihave .and. ihave <= 8 ) then
        ihave = 9
      else
        iterm = 1
      end if
!
!  Scientific notation exponent marker.
!
    else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

      if ( ihave < 6 ) then
        ihave = 6
      else
        iterm = 1
      end if
!
!  Digit.
!
    else if (  ihave < 11 .and. lle ( '0', c ) .and. lle ( c, '9' ) ) then

      if ( ihave <= 2 ) then
        ihave = 3
      else if ( ihave == 4 ) then
        ihave = 5
      else if ( ihave == 6 .or. ihave == 7 ) then
        ihave = 8
      else if ( ihave == 9 ) then
        ihave = 10
      end if

      call ch_to_digit ( c, ndig )

      if ( ihave == 3 ) then
        rtop = 10.0D+00 * rtop + real ( ndig, kind = 8 )
      else if ( ihave == 5 ) then
        rtop = 10.0D+00 * rtop + real ( ndig, kind = 8 )
        rbot = 10.0D+00 * rbot
      else if ( ihave == 8 ) then
        jtop = 10 * jtop + ndig
      else if ( ihave == 10 ) then
        jtop = 10 * jtop + ndig
        jbot = 10 * jbot
      end if
!
!  Anything else is regarded as a terminator.
!
    else
      iterm = 1
    end if
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    if ( iterm == 1 ) then
      exit
    end if

  end do
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to S_LENGTH.
!
  if ( iterm /= 1 .and. length+1 == s_length ) then
    length = s_length
  end if
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  if ( ihave == 1 .or. ihave == 2 .or. ihave == 6 .or. ihave == 7 ) then
    ierror = ihave
    IF (debugL) THEN
     !------------------------------------------------------------------------
     ! Comment S. Birner:
     ! I added the debugL flag because if the number is nonnumeric input,
     ! this is okay, and the "serious error" error message should not pop up.
     !------------------------------------------------------------------------
     WRITE(my_output_unit,'(a)' ) ' '
     WRITE(my_output_unit,'(a)' ) 'S_TO_R8 - Serious error!'
     WRITE(my_output_unit,'(a)' ) '  Illegal or nonnumeric input:'
     WRITE(my_output_unit,'(a,I5)' ) '  ierror = ',ierror
     WRITE(my_output_unit,'(a)' ) '    ' // trim ( s )
    END IF
    return
  end if
!
!  Number seems OK.  Form it.
!
  if ( jtop == 0 ) then
    rexp = 1.0D+00
  else
    if ( jbot == 1 ) then
      rexp = 10.0D+00 ** ( jsgn * jtop )
    else
      rexp = 10.0D+00 ** ( real ( jsgn * jtop, kind = 8 ) &
        / real ( jbot, kind = 8 ) )
    end if
  end if

  dval = real ( isgn, kind = 8 ) * rexp * rtop / rbot

!------------------------------------------------------------------------------
 END SUBROUTINE s_to_r8
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE CheckIfStringCharacters ( s, n, r8vec, ierror )
!subroutine s_to_r8vec              ( s, n, r8vec, ierror )
!------------------------------------------------------------------------------

!*****************************************************************************80
!
!! S_TO_R8VEC reads an R8VEC from a string.
!
!  Discussion:
!
!    An R8VEC is a vector of real values, of type "real ( kind = 8 )".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be read.
!
!    Input, integer ( kind = 4 ) N, the number of values expected.
!
!    Output, real ( kind = 8 ) R8VEC(N), the values read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    -K, could not read data for entries -K through N.
!
  implicit none

  integer   ( kind = 4 ) n

  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) ierror
  integer   ( kind = 4 ) ilo
  integer   ( kind = 4 ) lchar
  real      ( kind = 8 ) r8vec(n)
  character ( len = * )  s

  i = 0
  ierror = 0
  ilo = 1

  do
    IF (i >= n) EXIT

    i = i + 1

    call s_to_r8 ( s(ilo:), r8vec(i), ierror, lchar )

    if ( ierror /= 0 ) then
      ierror = -i
      exit
    end if

    ilo = ilo + lchar

  end do

!------------------------------------------------------------------------------
 END SUBROUTINE CheckIfStringCharacters
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ch_to_digit ( c, digit )
!------------------------------------------------------------------------------
!
!*******************************************************************************
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal. 
!
!    Output, integer DIGIT, the corresponding integer value.  If C was
!    'illegal', then DIGIT is -1.
!
  implicit none
!
  character c
  integer digit
!
  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then
 
    digit = ichar ( c ) - 48
 
  else if ( c == ' ' ) then
 
    digit = 0
 
  else

    digit = -1

  end if
 
!------------------------------------------------------------------------------
 END SUBROUTINE ch_to_digit
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE digit_to_ch ( digit, ch )
!------------------------------------------------------------------------------
!
!! DIGIT_TO_CH returns the character representation of a decimal digit.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Example:
!
!    DIGIT   CH 
!    -----  ---
!      0    '0'
!      1    '1'
!    ...    ...
!      9    '9'
!     17    '*'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 1999
!    08 April 2020, Stefan Birner
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIGIT, the digit value between 0 and 9.
!
!    Output, character CH, the corresponding character.
!
  implicit none

  character              ch
! integer   ( kind = 4 ) digit ! Stefan Birner
  integer, intent(in) :: digit ! Stefan Birner

  if ( 0 <= digit .and. digit <= 9 ) then

    ch = achar ( digit + 48 )

  else

    ch = '*'

  end if
 
!------------------------------------------------------------------------------
 END SUBROUTINE digit_to_ch
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
!SUBROUTINE i4_to_s_left ( i4, s )
 SUBROUTINE int_to_s_left ( i4, s )
!------------------------------------------------------------------------------
!
!! I4_TO_S_LEFT converts an I4 to a left-justified string.
! int_TO_S_LEFT converts an integer to a left-justified string. ! Stefan Birner
!
!  Discussion:
!
!!   An I4 is an integer ( kind = 4 ).
!    An I4 is an integer.  ! S. Birner
!
!  Example:
!
!    Assume that S is 6 characters long:
!
!        I4  S
!
!         1  1
!        -1  -1
!         0  0
!      1952  1952
!    123456  123456
!   1234567  ******  <-- Not enough room!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 July 2000
!    08 April 2020 Stefan Birner
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, an integer to be converted.
!
!    Output, character ( len = * ) S, the representation of the integer.
!    The integer will be left-justified.  If there is not enough space,
!    the string will be filled with stars.
!
  implicit none

  character              c
! integer   ( kind = 4 ) i
! integer   ( kind = 4 ) i4
! integer   ( kind = 4 ) idig
! integer   ( kind = 4 ) ihi
! integer   ( kind = 4 ) ilo
! integer   ( kind = 4 ) ipos
! integer   ( kind = 4 ) ival
  integer             :: i      ! S. Birner
  integer, intent(in) :: i4     ! S. Birner
  integer             :: idig   ! S. Birner
  integer             :: ihi    ! S. Birner
  integer             :: ilo    ! S. Birner
  integer             :: ipos   ! S. Birner
  integer             :: ival   ! S. Birner
  character ( len = * )  s

  s = ' '

  ilo = 1
  ihi = len ( s )

  if ( ihi <= 0 ) then
    return
  end if
!
!  Make a copy of the integer.
!
  ival = i4
!
!  Handle the negative sign.
!
  if ( ival < 0 ) then

    if ( ihi <= 1 ) then
      s(1:1) = '*'
      return
    end if

    ival = -ival
    s(1:1) = '-'
    ilo = 2

  end if
!
!  The absolute value of the integer goes into S(ILO:IHI).
!
  ipos = ihi
!
!  Find the last digit of IVAL, strip it off, and stick it into the string.
!
  do

    idig = mod ( ival, 10 )
    ival = ival / 10

    if ( ipos < ilo ) then
      do i = 1, ihi
        s(i:i) = '*'
      end do
      return
    end if

    call digit_to_ch ( idig, c )

    s(ipos:ipos) = c
    ipos = ipos - 1

    if ( ival == 0 ) then
      exit
    end if

  end do
!
!  Shift the string to the left.
!
  s(ilo:ilo+ihi-ipos-1) = s(ipos+1:ihi)
  s(ilo+ihi-ipos:ihi) = ' '
 
!------------------------------------------------------------------------------
 END SUBROUTINE int_to_s_left
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
!SUBROUTINE r8_to_s_left ( r8, s )
 SUBROUTINE r8_to_s_left ( r8, s , formatC_in )
!------------------------------------------------------------------------------

!*****************************************************************************80
!
!! R8_TO_S_LEFT writes an R8 into a left justified string.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!  ! A 'G14.6' format is used with a WRITE statement.
!    A 'G17.9' format is used with a WRITE statement. ! modified by S. Birner
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R8, the number to be written into the string.
!
!    Output, character ( len = * ) S, the string into which
!  ! the real number is to be written.  If the string is less than 14
!    the real number is to be written.  If the string is less than 17 ! modified by S. Birner
!    characters long, it will will be returned as a series of asterisks.
!
  implicit none

  integer   ( kind = 4 ) i
  real      ( kind = 8 ) r8
  character ( len = * )  s
  character(len=*),intent(in), optional :: formatC_in  ! modified by S. Birner

  character(len=:),allocatable          :: formatC     ! modified by S. Birner
  integer   ( kind = 4 ) s_length
! character ( len = 14 ) s2
  character ( len = 17 ) s2                            ! modified by S. Birner

  if ( present(formatC_in) ) then                      ! modified by S. Birner
      formatC = formatC_in                             ! modified by S. Birner
  else                                                 ! modified by S. Birner
      formatC = ''                                     ! modified by S. Birner
  end if                                               ! modified by S. Birner

  s_length = len ( s )

  s = '' ! <== Added by S. Birner to initialize the string. This is necessary if the input string is longer than 17 characters.

! if ( s_length < 14 ) then
  if ( s_length < 17 ) then ! modified by S. Birner

    do i = 1, s_length
      s(i:i) = '*'
    end do

  else if ( r8 == 0.0D+00 ) then
  ! s(1:14) = '     0.0      '
    s(1:17) = '     0.0         ' ! modified by S. Birner
  else

   SELECT CASE( TRIM(formatC) )
    CASE('(f17.9)')
     write ( s2, '(f17.9)' ) r8    ! modified by S. Birner
    CASE DEFAULT
   ! write ( s2, '(g14.6)' ) r8
     write ( s2, '(g17.9)' ) r8    ! modified by S. Birner
   END SELECT
   ! s(1:14) = s2
     s(1:17) = s2                  ! modified by S. Birner
  end if
!
!  Shift the string left.
!
  s = adjustl ( s )

!------------------------------------------------------------------------------
 END SUBROUTINE r8_to_s_left
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION ch_eqi ( c1, c2 )
!------------------------------------------------------------------------------
!
!*******************************************************************************
!
!! CH_EQI is a case insensitive comparison of two characters for equality.  
!
!
!  Examples:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical CH_EQI, the result of the comparison.
!
  implicit none
!
  character c1
  character c1_cap
  character c2
  character c2_cap
  logical ch_eqi
!
  c1_cap = c1
  c2_cap = c2

  call ch_cap ( c1_cap )
  call ch_cap ( c2_cap )

  if ( c1_cap == c2_cap ) then
    ch_eqi = .true.
  else
    ch_eqi = .false.
  end if

!------------------------------------------------------------------------------
 END FUNCTION ch_eqi
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ch_cap ( c )
!------------------------------------------------------------------------------
!
!*******************************************************************************
!
!! CH_CAP capitalizes a single character.
!
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none
!
  character c
  integer itemp
!
  itemp = ichar ( c )
 
  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if
 
!------------------------------------------------------------------------------
 END SUBROUTINE ch_cap
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_chrpak
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE CharacterManipulation
!------------------------------------------------------------------------------
!
!++m* convert_int_character.f90/CharacterManipulation
!
! NAME 
!   MODULE CharacterManipulation
!
! CONTAINS
!   o FUNCTION CountCharacters
!   o SUBROUTINE CharacterReplace
!   o SUBROUTINE ReplaceTAB
!   o SUBROUTINE Replace_NonBreakingSpace
!   o SUBROUTINE ReplaceXMLTag
!   o SUBROUTINE TEST_ReplaceXMLTag
!   o SUBROUTINE Replace_BackslashBlank_withBlank
!
! FILENAME
!   FortranInputParser/convert_int_character.f90
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE
 
 PRIVATE

 PUBLIC CountCharacters
 PUBLIC CharacterReplace
 PUBLIC ReplaceTAB
 PUBLIC Replace_NonBreakingSpace
 PUBLIC ReplaceXMLTag
 PUBLIC TEST_ReplaceXMLTag
 PUBLIC Replace_BackslashBlank_withBlank

 CONTAINS

!------------------------------------------------------------------------------
 FUNCTION CountCharacters(StringC,SpecialCharactersC) RESULT (NumberOfOccurences)
!------------------------------------------------------------------------------
!
!++f* CharacterManipulation/CountCharacters
!
! NAME
!   FUNCTION CountCharacters
!
! PURPOSE
!   Counts how often a character or substring occurs in a string.
!
! USAGE
!   CountCharacters(StringC,SpecialCharactersC)
! 
! INPUT 
!   o StringC
!   o SpecialCharactersC
!
! OUTPUT
!   o NumberOfOccurences
!
! NOTES
!   
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(in)  :: StringC
 CHARACTER(len=*),INTENT(in)  :: SpecialCharactersC
 INTEGER                      :: NumberOfOccurences

 INTEGER                      :: i
 INTEGER                      :: length_of_string
 INTEGER                      :: length_of_substring
 
 NumberOfOccurences = 0

 length_of_string    = LEN(StringC)
 length_of_substring = LEN(SpecialCharactersC)

 DO i=1,length_of_string-length_of_substring+1
    IF (StringC(i:i+length_of_substring-1) == SpecialCharactersC ) THEN
       NumberOfOccurences = NumberOfOccurences + 1
    END IF
 END DO
    
!------------------------------------------------------------------------------
 END FUNCTION CountCharacters
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE CharacterReplace (StringC, CharacterBeforeC, CharacterAfterC )
!------------------------------------------------------------------------------

  IMPLICIT NONE

  CHARACTER(len=*),INTENT(inout) :: StringC
  CHARACTER(len=1),INTENT(in)    :: CharacterBeforeC
  CHARACTER(len=1),INTENT(in)    :: CharacterAfterC

  INTEGER                        :: i
! INTEGER                        :: count

! count = 0
! WRITE(my_output_unit,*)                        " StringC = ",TRIM(StringC)

  !-----------------------------------------------
  ! Loop over all characters in string 'StringC'.
  !-----------------------------------------------
  DO i = 1, LEN ( StringC )

    IF ( StringC(i:i) == CharacterBeforeC ) THEN
         StringC(i:i) =  CharacterAfterC
 !       count = count + 1
    END IF

  END DO
 
 !WRITE(my_output_unit,*)                        " StringC = ",TRIM(StringC)
 !WRITE(my_output_unit,FMT = '(A,A,A,A,A,I5,A)') " Replaced '",CharacterBeforeC,"' with '",CharacterAfterC,"' ",count," times."
 !PAUSE
 
!------------------------------------------------------------------------------
 END SUBROUTINE CharacterReplace
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ReplaceTAB(StringC)
!------------------------------------------------------------------------------
!
!++s* CharacterManipulation/ReplaceTAB
!
! NAME
!   SUBROUTINE ReplaceTAB
!
! PURPOSE
!   Replaces a tab with a blank.
!
! USAGE
!   CALL ReplaceTAB(StringC)
! 
! INPUT 
!   o StringC (also output)
!
! OUTPUT
!   o StringC (also input)
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(inout) :: StringC

 CHARACTER(len=*),PARAMETER     :: TAB = ACHAR ( 9 )  ! The length of this character is 'len=1'.
 CHARACTER(len=1),PARAMETER     :: BLANK = ' '

 !-------------------------------
 ! Replace the TAB with a blank.
 !-------------------------------
 CALL CharacterReplace (StringC, TAB, BLANK )

!------------------------------------------------------------------------------
 END SUBROUTINE ReplaceTAB
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Replace_NonBreakingSpace(StringC)
!------------------------------------------------------------------------------
!
!++s* CharacterManipulation/Replace_NonBreakingSpace
!
! NAME
!   SUBROUTINE Replace_NonBreakingSpace
!
! PURPOSE
!   Replaces the nonbreaking space character with a blank.
!
! USAGE
!   CALL Replace_NonBreakingSpace(StringC)
! 
! INPUT 
!   o StringC (also output)
!
! OUTPUT
!   o StringC (also input)
!
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(inout) :: StringC

 CHARACTER(len=*),PARAMETER     :: NonBreakingSpace = ACHAR ( 160 )  ! The length of this character is 'len=1'.
 CHARACTER(len=1),PARAMETER     :: BLANK = ' '

 !-------------------------------------------------------
 ! Replace the nonbreaking space character with a blank.
 ! https://en.wikipedia.org/wiki/Non-breaking_space
 ! HTML code: &#160; or &nbsp;
 !-------------------------------------------------------
 CALL CharacterReplace (StringC, NonBreakingSpace, BLANK )

!------------------------------------------------------------------------------
 END SUBROUTINE Replace_NonBreakingSpace
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE ReplaceXMLTag(StringC)
!------------------------------------------------------------------------------
!
!++s* CharacterManipulation/ReplaceXMLTag
!
! NAME
!   SUBROUTINE ReplaceXMLTag
!
! PURPOSE
!   Replaces an XML tag <...> with blanks in an arbitrary string.
!     o <test>          (start tag)
!     o </test>         (end tag)
!     o <test></test>   (empty tag)
!     o <test />        (empty element tag)
!     o <test/>
!     o </>
!     o <>
!     o <i|x|j>         is also replaced
!     o <ij>            is also replaced
!     o <f_ij>          is also replaced
!
! USAGE
!   CALL ReplaceXMLTag(StringC)
! 
! INPUT 
!   o StringC (also output)
!
! OUTPUT
!   o StringC (also input)
!
! NOTES
!   written by S. Birner
!   Rules
!     o XML tags are case-sensitive.
!     o XML tags must be closed in an appropriate order, i.e. an XML tag opened inside another element must be closed before the outer element is closed.
!   
!##
!
!------------------------------------------------------------------------------
 USE system_specific_parser   ,ONLY:OutputDir_default_IndicatorC

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(inout) :: StringC

 CHARACTER(len=1),PARAMETER     :: BLANK = ' '

 CHARACTER(len=1),PARAMETER     :: XML_BEGIN = '<'
 CHARACTER(len=1),PARAMETER     :: XML_END   = '>'

 INTEGER                        :: i
 INTEGER                        :: j
 INTEGER                        :: i_begin
 INTEGER                        :: i_end
 INTEGER                        :: length
 LOGICAL                        :: FOUND_XML_BEGIN_L
 LOGICAL                        :: FOUND_XML_END_L
 LOGICAL                        :: ReplaceL

 length = LEN_TRIM(StringC) ! Determine length of string ignoring blanks on the right.

 FOUND_XML_BEGIN_L = .FALSE.
 FOUND_XML_END_L   = .FALSE.
 ReplaceL          = .FALSE.

 !-------------------------------------
 ! Initialize with unrealistic values.
 !-------------------------------------
 i_begin = length
 i_end   = 0

 !-------------------------------------
 ! Loop over all characters in string.
 !-------------------------------------
 DO i=1,length

    IF ( .NOT. FOUND_XML_BEGIN_L ) THEN    ! Search for "begin XML tag".
    
     IF ( StringC(i:i) == XML_BEGIN ) THEN ! Search for "begin XML tag".
          i_begin           = i
          FOUND_XML_BEGIN_L = .TRUE.
     END IF

    END IF


    IF ( FOUND_XML_BEGIN_L ) THEN              ! Now search for "end XML tag".

         IF ( StringC(i:i) == XML_END   ) THEN
          i_end             = i
          FOUND_XML_END_L   = .TRUE.
         END IF

    END IF

    IF ( FOUND_XML_BEGIN_L .AND. FOUND_XML_END_L ) THEN
     
     ReplaceL = .TRUE.

     !--------------------------------------------------
     ! Check for special string '<name_of_input_file>'.
     !--------------------------------------------------
     IF ( LEN_TRIM(OutputDir_default_IndicatorC) == i_end-i_begin+1 ) THEN        ! Check if length is equal to special string.
      IF (    TRIM(OutputDir_default_IndicatorC) == StringC(i_begin:i_end) ) THEN ! Check if string is equal to special string.
          ReplaceL = .FALSE.
      END IF
     END IF

     IF ( ReplaceL ) THEN
      DO j=i_begin,i_end
          StringC(j:j) = BLANK
      END DO
     END IF

     FOUND_XML_BEGIN_L = .FALSE.
     FOUND_XML_END_L   = .FALSE.
     i_begin = length
     i_end   = 0
    END IF

 END DO

!------------------------------------------------------------------------------
 END SUBROUTINE ReplaceXMLTag
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE TEST_ReplaceXMLTag
!------------------------------------------------------------------------------
!
!++s* CharacterManipulation/TEST_ReplaceXMLTag
!
! NAME
!   SUBROUTINE TEST_ReplaceXMLTag
!
! PURPOSE
!   Automatic unit test routine for SUBROUTINE ReplaceXMLTag.
!
! USAGE
!   CALL TEST_ReplaceXMLTag
! 
! INPUT 
!   none
!
! OUTPUT
!   none
!
! NOTES
!   written by S. Birner
!   
!##
!
!------------------------------------------------------------------------------
 USE My_Input_and_Output_Units,ONLY:my_output_unit

 IMPLICIT NONE
 CHARACTER(len=987),DIMENSION(2,15) :: StringCM

 CHARACTER(len=:)  ,ALLOCATABLE     :: StringTestC
 INTEGER                            :: i

 StringCM(1,1)  = "<test>"
 StringCM(2,1)  = "      "

 StringCM(1,2)  = "</test>"
 StringCM(2,2)  = "       "

 StringCM(1,3)  = "<variable> %Width = 5.0 </variable>     # quantum well width"
 StringCM(2,3)  = "           %Width = 5.0                 # quantum well width"

 StringCM(1,4)  = "<>"
 StringCM(2,4)  = "  "

 StringCM(1,5)  = "<test/>"
 StringCM(2,5)  = "       "

 StringCM(1,6)  = " ! <variable> %Width = 5.0 </variable>     # quantum well width"
 StringCM(2,6)  = " !            %Width = 5.0                 # quantum well width"

 StringCM(1,7)  = "</unit >"
 StringCM(2,7)  = "        "

 StringCM(1,8)  = "  >   <>   <   !  #"
 StringCM(2,8)  = "  >        <   !  #"

 StringCM(1,9)  = '<?xml version = "1.0" encoding = "UTF-8"?>'
 StringCM(2,9)  = '                                          '

 StringCM(1,10) = "<element-name attribute1 attribute2 > test"
 StringCM(2,10) = "                                      test"

 StringCM(1,11) = "< element-name attribute1 attribute2 > test"
 StringCM(2,11) = "                                       test"

 StringCM(1,12) = "<variable> %Width = 5.0 </variable> <variable2> %Length = 5.0 </variable2>     # quantum well width"
 StringCM(2,12) = "           %Width = 5.0                         %Length = 5.0                  # quantum well width"

 StringCM(1,13) = ' output-directory = <name_of_input_file> ! Here, the placeholder string "<name_of_input_file>" ...'
 StringCM(2,13) = ' output-directory = <name_of_input_file> ! Here, the placeholder string "<name_of_input_file>" ...' ! no replacement in this special case!

 StringCM(1,14) = ' test << 5 < test > sf '
 StringCM(2,14) = ' test               sf '

 StringCM(1,15) = ' test >< 5 > t<e>st<sad  sf '
 StringCM(2,15) = ' test >      t   st<sad  sf '

 DO i=1,SIZE(StringCM(2,:))
       StringTestC  = StringCM(1,i)
  WRITE(my_output_unit,'(A)') "StringCM(1,i) = "//TRIM(StringCM(1,i))
  CALL ReplaceXMLTag(StringTestC)
  WRITE(my_output_unit,'(A)') "StringCM(2,i) = "//TRIM(StringCM(2,i))
  IF ( StringTestC /= StringCM(2,i) ) THEN
     WRITE(my_output_unit,'(A)') "Error TEST_ReplaceXMLTag: XML tag string replacement did not work."
     STOP
  END IF
 END DO

!------------------------------------------------------------------------------
 END SUBROUTINE TEST_ReplaceXMLTag
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 SUBROUTINE Replace_BackslashBlank_withBlank(StringC)
!------------------------------------------------------------------------------
!
!++s* CharacterManipulation/Replace_BackslashBlank_withBlank
!
! NAME
!   SUBROUTINE Replace_BackslashBlank_withBlank
!
! PURPOSE
!   Replaces '\ ' with ' ' (needed for Linux).
!
! USAGE
!   CALL Replace_BackslashBlank_withBlank(StringC)
! 
! INPUT 
!   o StringC (also output)
!
! OUTPUT
!   o StringC (also output)
!
! NOTES
!   
!##
!
!------------------------------------------------------------------------------
 USE SpecialCharacters     ,ONLY:DoubleBackSlashC, &
                                       BackSlashC
 USE system_specific_parser,ONLY:OperatingSystemC
 USE mod_chrpak            ,ONLY:StringReplace

 IMPLICIT NONE

 CHARACTER(len=*),INTENT(inout) :: StringC

 SELECT CASE( TRIM(OperatingSystemC) )
  CASE('windows')
   !-------------------------------------------------------------------------------------
   ! CHECK: It seems that this does not work on Windows, so let's do nothing and return.
   !-------------------------------------------------------------------------------------
  CASE DEFAULT
   !-----------------------------------------------------------------------------------------------------------------------------------------------------
   ! On Linux, an input file name can look like like the following where '\ ' indicates a blank in Linux.
   ! nextnano3.exe -inputfile  /home/nextnano/nextnano_exe/nextnano/nextnano3\ sample\ files/1DGreg_Snider_MANUAL.nn3  <== This works.
   ! nextnano3.exe -inputfile '/home/nextnano/nextnano_exe/nextnano/nextnano3\ sample\ files/1DGreg_Snider_MANUAL.nn3' <== We have to CALL StringReplace.
   ! nextnano3.exe -inputfile "/home/nextnano/nextnano_exe/nextnano/nextnano3\ sample\ files/1DGreg_Snider_MANUAL.nn3" <== We have to CALL StringReplace.
   !-----------------------------------------------------------------------------------------------------------------------------------------------------
   CALL StringReplace( StringC, DoubleBackSlashC//' ' , ' ') ! ==> '\\ ' ! It is important that '\\ ' comes before '\ '.
   CALL StringReplace( StringC,       BackSlashC//' ' , ' ') ! ==> '\ '
 END SELECT

!------------------------------------------------------------------------------
 END SUBROUTINE Replace_BackslashBlank_withBlank
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE CharacterManipulation
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 MODULE mod_int_to_char999
!------------------------------------------------------------------------------
!
!++m* convert_int_character.f90/mod_int_to_char999
!
! NAME 
!   MODULE mod_int_to_char999
!
! CONTAINS
!   o FUNCTION int_to_char99
!   o FUNCTION int_to_char999
!   o FUNCTION int_to_char
!   o FUNCTION int_2_char            ==> This should be the prefered function. ==> The length of the string depends on the integer. It is variable.
!   o FUNCTION int_to_char000
!
! FILENAME
!   FortranInputParser/convert_int_character.f90
!
! NOTES
!   Are negative numbers treated correctly? If not, use SUBROUTINE int_to_s_left for negative numbers.
!
!##
!
!------------------------------------------------------------------------------
 USE system_specific_parser,ONLY:DebugLevel

 IMPLICIT NONE

 PRIVATE

 PUBLIC int_to_char99       !     Exactly 2 digits (e.g MONTH=04, DAY=02)
!PUBLIC int_to_char999      !     Exactly 3 digits.
!PUBLIC int_to_char           ==> should not be used. Use FUNCTION int_2_char instead.
 PUBLIC int_2_char          !     As many digits as needed.
 PUBLIC int_to_char000      !     At least 3 digits but can be more.

 CONTAINS

!------------------------------------------------------------------------------
 FUNCTION int_to_char99(i) RESULT(ch)
!------------------------------------------------------------------------------
!
!++f* mod_int_to_char999/int_to_char99
!
! NAME
!   FUNCTION int_to_char99
!
! PURPOSE
!   Converts i=1..99 to a character of length 2, e.g.  5 --> '05'.
!
! USAGE
!   int_to_char99(i)
! 
! INPUT 
!   o i: integer number  0 <= i <= 99
!
! OUTPUT
!   o ch: character of length 2
! 
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER           ,INTENT(in) :: i
 INTEGER           ,PARAMETER  :: TWO = 2
 CHARACTER(len=TWO)            :: ch

 ch = ''

  IF (i < 0 .OR. &
      i > 99) THEN
    WRITE (*,'(A,I12)') " Error int_to_char99: Only integers 0 <= x <= 99 are allowed. i = ",i
    CALL Convert_int_to_char(i, ch)
    IF (DebugLevel < 1) STOP
  ELSE
    CALL Convert_int_to_char(i, ch,minimum_number_of_characters=TWO)
  END IF

!------------------------------------------------------------------------------
 END FUNCTION int_to_char99
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION int_to_char999(i) RESULT(ch)
!------------------------------------------------------------------------------
!
!++f* mod_int_to_char999/int_to_char999
!
! NAME
!   FUNCTION int_to_char999
!
! PURPOSE
!   Converts i=1..999 to a character of length 3, e.g.  5 --> '005'.
!
! USAGE
!   int_to_char999(i)
! 
! INPUT 
!   o i: integer number  0 <= i <= 999
!
! OUTPUT
!   o ch: character of length 3
! 
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER           ,INTENT(in) :: i
 INTEGER           ,PARAMETER  :: THREE = 3
 CHARACTER(len=THREE)          :: ch

 ch = ''

  IF (i < 0 .OR. &
      i > 999) THEN
    WRITE (*,'(A,I12)') " Error int_to_char999: Only integers 0 <= x <= 999 are allowed. i = ",i
    CALL Convert_int_to_char(i, ch)
    IF (DebugLevel < 1) STOP
  ELSE
    CALL Convert_int_to_char(i, ch,minimum_number_of_characters=THREE)
  END IF

!------------------------------------------------------------------------------
 END FUNCTION int_to_char999
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 RECURSIVE SUBROUTINE Convert_int_to_char(i, integer_as_stringC,minimum_number_of_characters)
!------------------------------------------------------------------------------
!
!++s* mod_int_to_char999/Convert_int_to_char
!
! NAME
!   SUBROUTINE Convert_int_to_char
!
! PURPOSE
!   Converts i=1..9999...9 to a character of length 'NumberOfDigits', e.g. 12345 --> '12345'.
!
! USAGE
!   CALL Convert_int_to_char(i, integer_as_stringC,minimum_number_of_characters)
! 
! INPUT 
!   o i:                             integer number  0 <= i <= 9999...9
!   o minimum_number_of_characters:  If e.g. minimum_number_of_characters = 4, then the number 23 is written as '0023'.
!
! OUTPUT
!   o integer_as_stringC:            character of length 'NumberOfDigits'
!
! NOTES
!   The number 2,147,483,647 (or hexadecimal 7FFF,FFFF16) is the maximum positive value for a 32-bit signed binary integer in computing.
!   This number has 10 digits, so 10 should be enough.
!
!
!##
!
!------------------------------------------------------------------------------
 USE mod_chrpak ,ONLY:int_to_s_left

 IMPLICIT NONE

 INTEGER         ,INTENT(in)           :: i
 CHARACTER(len=*),INTENT(out)          :: integer_as_stringC
 INTEGER         ,INTENT(in) ,OPTIONAL :: minimum_number_of_characters

 INTEGER                               :: NumberOfDigits
 CHARACTER(len=10)                     :: StringC
!------------------------------------

 IF      ( i < 0 ) THEN
  ! PRINT *,"Error Convert_int_to_char: Only integers >= 0 are allowed. i = ",i
    CALL int_to_s_left ( i, stringC )
    integer_as_stringC = TRIM(stringC)                   ! includes the minus within the string
  ! CALL Convert_int_to_char(ABS(i), integer_as_stringC) ! Taking the absolute value of this integer, i.e. making the negative number positive.

 ELSE ! if i >= 0

  IF      ( i <= 9 ) THEN
   NumberOfDigits = 1
  ELSE IF ( i <= 99 ) THEN
   NumberOfDigits = 2
  ELSE IF ( i <= 999 ) THEN
   NumberOfDigits = 3
  ELSE IF ( i <= 9999 ) THEN
   NumberOfDigits = 4
  ELSE IF ( i <= 99999 ) THEN
   NumberOfDigits = 5
  ELSE IF ( i <= 999999 ) THEN
   NumberOfDigits = 6
  ELSE IF ( i <= 9999999 ) THEN
   NumberOfDigits = 7
  ELSE IF ( i <= 99999999 ) THEN
   NumberOfDigits = 8
  ELSE IF ( i <= 999999999 ) THEN
   NumberOfDigits = 9
! ELSE IF ( i <= 9999999999 ) THEN
  ELSE IF ( i <= 2147483647 ) THEN
   NumberOfDigits = 10
  ELSE
    WRITE (*,'(A,I12)') " Error Convert_int_to_char:&
                        & Numbers larger than 2,147,483,647 were not thought to be relevant for this routine. i = ",i
    STOP
  END IF

 NumberOfDigits = MAX(NumberOfDigits,minimum_number_of_characters)

  SELECT CASE( NumberOfDigits )
   CASE(1)
    WRITE (integer_as_stringC,"(I1.1)")   i
   CASE(2)
    WRITE (integer_as_stringC,"(I2.2)")   i
   CASE(3)
    WRITE (integer_as_stringC,"(I3.3)")   i
   CASE(4)
    WRITE (integer_as_stringC,"(I4.4)")   i
   CASE(5)
    WRITE (integer_as_stringC,"(I5.5)")   i
   CASE(6)
    WRITE (integer_as_stringC,"(I6.6)")   i
   CASE(7)
    WRITE (integer_as_stringC,"(I7.7)")   i
   CASE(8)
    WRITE (integer_as_stringC,"(I8.8)")   i
   CASE(9)
    WRITE (integer_as_stringC,"(I9.9)")   i
   CASE(10)
    WRITE (integer_as_stringC,"(I10.10)") i
   CASE DEFAULT
    WRITE (*,'(A,I12)') " Error Convert_int_to_char:&
                        & Numbers larger than 2,147,483,647 were not thought to be relevant for this routine. i = ",i
    WRITE (*,'(A,I12)') " NumberOfDigits = ",NumberOfDigits
    STOP
  END SELECT

 END IF

!------------------------------------------------------------------------------
 END SUBROUTINE Convert_int_to_char
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION int_to_char( i )
!------------------------------------------------------------------------------
!
!++f* mod_int_to_char999/int_to_char
!
! NAME
!   FUNCTION int_to_char
!
! PURPOSE
!   Converts integer i to a character string of length 10.
!   CHECK: The idea is to have a variable length instead of the fixed value of 10.
!
! USAGE
!   int_to_char(i)
! 
! INPUT 
!   o i:           integer number
!
! OUTPUT
!   o int_to_char: character of length 10
!
! NOTES
!   The number 2,147,483,647 (or hexadecimal 7FFF,FFFF16) is the maximum positive value for a 32-bit signed binary integer in computing.
!   This number has 10 digits, so 10 should be enough.
! 
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER          ,INTENT(in) :: i
!CHARACTER(len=:),ALLOCATABLE :: int_to_char ! RESULT    (Did not work for this particular algorithm.)
 CHARACTER(len=10)            :: int_to_char ! RESULT    (CHECK: How about variable length?)

 INTEGER                      :: factor, i_digit, i_counter

 int_to_char = ''

 factor = 1

 DO
    IF ( factor > i ) EXIT
    factor = factor * 10
 END DO

 i_digit   = i
 i_counter = 1

 DO
    IF ( factor <= 1 ) EXIT
       
    factor = factor / 10
    int_to_char( 1 : i_counter ) = &
         TRIM( int_to_char ) // CHAR( 48 + i_digit / factor )
    i_digit   = i_digit - ( i_digit / factor ) * factor
    i_counter = i_counter + 1

 END DO

 !---------------------------------------------------------------------------
 ! It seems that 'int_to_char( 0 )' does not return '0' but an empty string.
 ! Therefore we put this additional statement.
 !---------------------------------------------------------------------------
 IF (i == 0) THEN
    int_to_char = '0'
 END IF

!------------------------------------------------------------------------------
 END FUNCTION int_to_char
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION int_2_char( i )
!------------------------------------------------------------------------------
!
!++f* mod_int_to_char999/int_2_char
!
! NAME
!   FUNCTION int_2_char
!
! PURPOSE
!   Converts integer i to a character string of variable length.
!
! USAGE
!         int_2_char(i)  should be the prefered option           if ALLOCATABLE character length is supported.
!  [ TRIM(int_2_char(i)) should be the recommended function call if ALLOCATABLE character length is not supported. ]
! 
! INPUT 
!   o i:           integer number
!
! OUTPUT
!   o int_2_char:  character of variable length
! 
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER          ,INTENT(in) :: i
 CHARACTER(len=:),ALLOCATABLE :: int_2_char ! RESULT
!CHARACTER(len=3)             :: int_2_char ! RESULT ! (if allocatable character is not supported, e.g. gfortran compiler on CentOS) CHECK: This probably has implications on the software!!!)

 CHARACTER(len=10)            :: stringC    ! We use an intermediate step here. No sure if this is necessary.
                                            ! It seems that the gfortran compilers needs TRIM(int_2_char(i)) (gfortran 4.7.1). But this intermediate step does not work.
                                            ! specifier = 'ion-'//TRIM(int_2_char(ion))//'-name'  <== This should have worked.
                                            ! specifier = 'ion-'//int_2_char(ion)//'-name'        <== This does not work with gfortran 4.7.1.
                                            ! Error message: Couldn't find requested specifier = ion-1         -name
                                            !                Actual keyword = $atomic-layers
 stringC = int_to_char( i )

 int_2_char = TRIM( stringC )

!------------------------------------------------------------------------------
 END FUNCTION int_2_char
!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------
 FUNCTION int_to_char000( i )
!------------------------------------------------------------------------------
!
!++f* mod_int_to_char999/int_to_char000
!
! NAME
!   FUNCTION int_to_char000
!
! PURPOSE
!   Converts integer i to a character string of a fixed length starting with zeros.
!   Example:    1 ==>  001
!   Example:   14 ==>  014
!   Example:  145 ==>  145
!   Example: 1145 ==> 1145
!   ... 
!
! USAGE
!        int_to_char000( i )
!   TRIM(int_to_char000( i ))  <== This is recommended in order to avoid trailing blanks, i.e. '14' instead of '14 '.
! 
! INPUT 
!   o i:                integer number
!
! OUTPUT
!   o int_to_char000:  character of variable length but at least the length is 3 starting with zeros.
! 
!##
!
!------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER          ,INTENT(in)  :: i
 CHARACTER(len=:) ,ALLOCATABLE :: int_to_char000 ! RESULT
!CHARACTER(len=10)             :: int_to_char000 ! RESULT ! (if allocatable character is not supported, e.g. gfortran compiler on CentOS) CHECK: This probably has implications on the software!!!)

 !----------------------------------------------------------------------------------------------------------------------
 ! By default, we want to return an index with at least 3 digits like this: 000, 001, 002, 003, ..., 009, 010, 011, ...
 ! In all other cases where the number needs at least 4 digits, we return more, e.g. 1241, ...
 !----------------------------------------------------------------------------------------------------------------------
 ! CHECK: How about negative numbers?
 IF      ( i <= 999 ) THEN
    int_to_char000 = int_to_char999( i ) ! Return exactly 3 digits.
 ELSE
    int_to_char000 = int_2_char( i )     ! Return as many digits as needed.
 END IF

!------------------------------------------------------------------------------
 END FUNCTION int_to_char000
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 END MODULE mod_int_to_char999
!------------------------------------------------------------------------------
