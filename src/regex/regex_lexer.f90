module regex_lexer
  !> Regex pattern tokenizer for FERP
  !> Handles both BRE (Basic) and ERE (Extended) regex dialects
  use regex_types
  use ferp_kinds, only: pattern_len
  implicit none
  private

  public :: tokenize

contains

  subroutine tokenize(pattern, tokens, is_ere, ierr)
    !> Tokenize a regex pattern
    character(len=*), intent(in) :: pattern
    type(token_list_t), intent(out) :: tokens
    logical, intent(in) :: is_ere           ! True for ERE, false for BRE
    integer, intent(out) :: ierr

    integer :: i, n
    character(len=1) :: c, next_c
    type(token_t) :: tok
    logical :: in_bracket

    ierr = 0
    call tokens%init()
    n = pattern_len(pattern)  ! Use pattern_len to preserve whitespace patterns
    i = 1
    in_bracket = .false.

    do while (i <= n)
      c = pattern(i:i)
      next_c = ' '
      if (i < n) next_c = pattern(i+1:i+1)

      tok = token_t()
      tok%pos = i

      ! Handle character class separately
      if (c == '[') then
        call parse_char_class(pattern, i, n, tok, ierr)
        if (ierr /= 0) return
        call tokens%append(tok)
        cycle
      end if

      ! Handle escape sequences
      if (c == '\') then
        if (i >= n) then
          ! Trailing backslash - treat as literal
          tok%ttype = TOK_LITERAL
          tok%char_val = '\'
          i = i + 1
        else
          call parse_escape(pattern, i, n, tok, is_ere, ierr)
          if (ierr /= 0) return
        end if
        call tokens%append(tok)
        cycle
      end if

      ! Handle metacharacters based on BRE vs ERE
      select case (c)
        case ('.')
          tok%ttype = TOK_DOT
          i = i + 1

        case ('*')
          tok%ttype = TOK_STAR
          i = i + 1

        case ('^')
          tok%ttype = TOK_CARET
          i = i + 1

        case ('$')
          tok%ttype = TOK_DOLLAR
          i = i + 1

        case ('+')
          if (is_ere) then
            tok%ttype = TOK_PLUS
          else
            tok%ttype = TOK_LITERAL
            tok%char_val = '+'
          end if
          i = i + 1

        case ('?')
          if (is_ere) then
            tok%ttype = TOK_QUESTION
          else
            tok%ttype = TOK_LITERAL
            tok%char_val = '?'
          end if
          i = i + 1

        case ('|')
          if (is_ere) then
            tok%ttype = TOK_PIPE
          else
            tok%ttype = TOK_LITERAL
            tok%char_val = '|'
          end if
          i = i + 1

        case ('(')
          if (is_ere) then
            tok%ttype = TOK_LPAREN
          else
            tok%ttype = TOK_LITERAL
            tok%char_val = '('
          end if
          i = i + 1

        case (')')
          if (is_ere) then
            tok%ttype = TOK_RPAREN
          else
            tok%ttype = TOK_LITERAL
            tok%char_val = ')'
          end if
          i = i + 1

        case ('{')
          if (is_ere) then
            tok%ttype = TOK_LBRACE
          else
            tok%ttype = TOK_LITERAL
            tok%char_val = '{'
          end if
          i = i + 1

        case ('}')
          if (is_ere) then
            tok%ttype = TOK_RBRACE
          else
            tok%ttype = TOK_LITERAL
            tok%char_val = '}'
          end if
          i = i + 1

        case default
          ! Literal character
          tok%ttype = TOK_LITERAL
          tok%char_val = c
          i = i + 1
      end select

      call tokens%append(tok)
    end do

    ! Add end token
    tok = token_t()
    tok%ttype = TOK_END
    tok%pos = n + 1
    call tokens%append(tok)

  end subroutine tokenize

  subroutine parse_escape(pattern, pos, n, tok, is_ere, ierr)
    !> Parse an escape sequence starting at pos (which points to \)
    character(len=*), intent(in) :: pattern
    integer, intent(inout) :: pos
    integer, intent(in) :: n
    type(token_t), intent(out) :: tok
    logical, intent(in) :: is_ere
    integer, intent(out) :: ierr

    character(len=1) :: c
    integer :: ref_num

    ierr = 0
    pos = pos + 1  ! Skip the backslash
    c = pattern(pos:pos)

    select case (c)
      ! BRE special escapes (become metacharacters)
      case ('(')
        if (is_ere) then
          tok%ttype = TOK_LITERAL
          tok%char_val = '('
        else
          tok%ttype = TOK_LPAREN
        end if
        pos = pos + 1

      case (')')
        if (is_ere) then
          tok%ttype = TOK_LITERAL
          tok%char_val = ')'
        else
          tok%ttype = TOK_RPAREN
        end if
        pos = pos + 1

      case ('{')
        if (is_ere) then
          tok%ttype = TOK_LITERAL
          tok%char_val = '{'
        else
          tok%ttype = TOK_LBRACE
        end if
        pos = pos + 1

      case ('}')
        if (is_ere) then
          tok%ttype = TOK_LITERAL
          tok%char_val = '}'
        else
          tok%ttype = TOK_RBRACE
        end if
        pos = pos + 1

      ! GNU extensions for BRE (also work in ERE)
      case ('+')
        if (.not. is_ere) then
          tok%ttype = TOK_PLUS
        else
          tok%ttype = TOK_LITERAL
          tok%char_val = '+'
        end if
        pos = pos + 1

      case ('?')
        if (.not. is_ere) then
          tok%ttype = TOK_QUESTION
        else
          tok%ttype = TOK_LITERAL
          tok%char_val = '?'
        end if
        pos = pos + 1

      case ('|')
        if (.not. is_ere) then
          tok%ttype = TOK_PIPE
        else
          tok%ttype = TOK_LITERAL
          tok%char_val = '|'
        end if
        pos = pos + 1

      ! Backreferences \1-\9
      case ('1', '2', '3', '4', '5', '6', '7', '8', '9')
        tok%ttype = TOK_BACKREF
        read(c, '(I1)') ref_num
        tok%int_val = ref_num
        pos = pos + 1

      ! Word boundaries
      case ('<')
        tok%ttype = TOK_WORD_BOUNDARY
        tok%int_val = 1  ! word start
        pos = pos + 1

      case ('>')
        tok%ttype = TOK_WORD_BOUNDARY
        tok%int_val = 2  ! word end
        pos = pos + 1

      case ('b')
        tok%ttype = TOK_WORD_BOUNDARY
        tok%int_val = 3  ! word boundary (either)
        pos = pos + 1

      case ('B')
        tok%ttype = TOK_WORD_BOUNDARY
        tok%int_val = 4  ! not word boundary
        pos = pos + 1

      ! Character escapes
      case ('n')
        tok%ttype = TOK_LITERAL
        tok%char_val = char(10)  ! newline
        pos = pos + 1

      case ('t')
        tok%ttype = TOK_LITERAL
        tok%char_val = char(9)   ! tab
        pos = pos + 1

      case ('r')
        tok%ttype = TOK_LITERAL
        tok%char_val = char(13)  ! carriage return
        pos = pos + 1

      ! Escape metacharacters to make them literal
      case ('.', '*', '^', '$', '[', ']', '\')
        tok%ttype = TOK_LITERAL
        tok%char_val = c
        pos = pos + 1

      case default
        ! Unknown escape - treat as literal
        tok%ttype = TOK_LITERAL
        tok%char_val = c
        pos = pos + 1
    end select

  end subroutine parse_escape

  subroutine parse_char_class(pattern, pos, n, tok, ierr)
    !> Parse a character class [...] starting at pos (which points to [)
    character(len=*), intent(in) :: pattern
    integer, intent(inout) :: pos
    integer, intent(in) :: n
    type(token_t), intent(out) :: tok
    integer, intent(out) :: ierr

    integer :: j, start_char, end_char
    character(len=1) :: c, prev_c
    logical :: negated, first

    ierr = 0
    tok%ttype = TOK_LITERAL  ! Will be set properly at end
    tok%char_class = .false.
    tok%negated = .false.

    pos = pos + 1  ! Skip [

    if (pos > n) then
      ierr = 1
      return
    end if

    ! Check for negation
    negated = .false.
    if (pattern(pos:pos) == '^') then
      negated = .true.
      pos = pos + 1
    end if

    ! Handle ] at start (it's literal)
    first = .true.
    if (pos <= n .and. pattern(pos:pos) == ']') then
      tok%char_class(ichar(']')) = .true.
      pos = pos + 1
      first = .false.
    end if

    ! Handle - at start (it's literal)
    if (pos <= n .and. pattern(pos:pos) == '-') then
      tok%char_class(ichar('-')) = .true.
      pos = pos + 1
    end if

    prev_c = char(0)
    do while (pos <= n)
      c = pattern(pos:pos)

      if (c == ']') then
        ! End of character class
        pos = pos + 1
        tok%negated = negated
        tok%ttype = TOK_LBRACKET  ! Indicate this is a char class token
        return
      end if

      if (c == '-' .and. pos + 1 <= n .and. pattern(pos+1:pos+1) /= ']') then
        ! Range: prev_c - next_c
        if (prev_c /= char(0)) then
          pos = pos + 1
          if (pos > n) then
            ierr = 1
            return
          end if
          c = pattern(pos:pos)

          ! Handle escape in range end
          if (c == '\' .and. pos + 1 <= n) then
            pos = pos + 1
            c = pattern(pos:pos)
          end if

          start_char = ichar(prev_c)
          end_char = ichar(c)
          if (start_char > end_char) then
            ! Invalid range, but we'll be lenient
            tok%char_class(start_char) = .true.
            tok%char_class(ichar('-')) = .true.
            tok%char_class(end_char) = .true.
          else
            do j = start_char, end_char
              tok%char_class(j) = .true.
            end do
          end if
          prev_c = char(0)  ! Reset after range
          pos = pos + 1
          cycle
        else
          ! - at start after ] or as first char
          tok%char_class(ichar('-')) = .true.
          pos = pos + 1
          cycle
        end if
      end if

      if (c == '\' .and. pos + 1 <= n) then
        ! Escape sequence in character class
        pos = pos + 1
        c = pattern(pos:pos)
        select case (c)
          case ('n')
            c = char(10)
          case ('t')
            c = char(9)
          case ('r')
            c = char(13)
          ! Otherwise take the character literally
        end select
      end if

      if (c == '[' .and. pos + 1 <= n .and. pattern(pos+1:pos+1) == ':') then
        ! POSIX character class [:alpha:] etc
        call parse_posix_class(pattern, pos, n, tok%char_class, ierr)
        if (ierr /= 0) return
        prev_c = char(0)
        cycle
      end if

      ! Regular character
      tok%char_class(ichar(c)) = .true.
      prev_c = c
      pos = pos + 1
    end do

    ! Unterminated character class
    ierr = 1

  end subroutine parse_char_class

  subroutine parse_posix_class(pattern, pos, n, char_class, ierr)
    !> Parse POSIX character class [:name:]
    character(len=*), intent(in) :: pattern
    integer, intent(inout) :: pos
    integer, intent(in) :: n
    logical, intent(inout) :: char_class(0:255)
    integer, intent(out) :: ierr

    integer :: end_pos, j
    character(len=16) :: class_name

    ierr = 0

    ! Find closing :]
    end_pos = index(pattern(pos:n), ':]')
    if (end_pos == 0) then
      ierr = 1
      return
    end if
    end_pos = pos + end_pos - 1  ! Adjust to absolute position (index is 1-based)

    ! Extract class name (skip [: at start, stop before :]
    class_name = pattern(pos+2:end_pos-1)

    select case (trim(class_name))
      case ('alnum')
        do j = ichar('a'), ichar('z')
          char_class(j) = .true.
        end do
        do j = ichar('A'), ichar('Z')
          char_class(j) = .true.
        end do
        do j = ichar('0'), ichar('9')
          char_class(j) = .true.
        end do

      case ('alpha')
        do j = ichar('a'), ichar('z')
          char_class(j) = .true.
        end do
        do j = ichar('A'), ichar('Z')
          char_class(j) = .true.
        end do

      case ('digit')
        do j = ichar('0'), ichar('9')
          char_class(j) = .true.
        end do

      case ('lower')
        do j = ichar('a'), ichar('z')
          char_class(j) = .true.
        end do

      case ('upper')
        do j = ichar('A'), ichar('Z')
          char_class(j) = .true.
        end do

      case ('space')
        char_class(ichar(' ')) = .true.
        char_class(9) = .true.   ! tab
        char_class(10) = .true.  ! newline
        char_class(11) = .true.  ! vertical tab
        char_class(12) = .true.  ! form feed
        char_class(13) = .true.  ! carriage return

      case ('blank')
        char_class(ichar(' ')) = .true.
        char_class(9) = .true.   ! tab

      case ('punct')
        ! Punctuation characters
        do j = 33, 47
          char_class(j) = .true.
        end do
        do j = 58, 64
          char_class(j) = .true.
        end do
        do j = 91, 96
          char_class(j) = .true.
        end do
        do j = 123, 126
          char_class(j) = .true.
        end do

      case ('xdigit')
        do j = ichar('0'), ichar('9')
          char_class(j) = .true.
        end do
        do j = ichar('a'), ichar('f')
          char_class(j) = .true.
        end do
        do j = ichar('A'), ichar('F')
          char_class(j) = .true.
        end do

      case ('word')
        ! GNU extension: word characters
        do j = ichar('a'), ichar('z')
          char_class(j) = .true.
        end do
        do j = ichar('A'), ichar('Z')
          char_class(j) = .true.
        end do
        do j = ichar('0'), ichar('9')
          char_class(j) = .true.
        end do
        char_class(ichar('_')) = .true.

      case default
        ! Unknown class - ignore silently
    end select

    pos = end_pos + 2  ! Skip past :] (end_pos points to ':', so +2 to skip both)

  end subroutine parse_posix_class

end module regex_lexer
