  !> @brief Functions to parse command line arguments
  !>
  !> Module to read command line arguments to a program
  !> We assume they are of the form name=value (spaces around '=' are ignored) or are a flag
  !> Value can be extracted as a string, integer, a long-integer
  !> or a single or double-precision real, according to the
  !> type passed in.
  !> Argument names are limited to 20 chars, and values
  !> to 30 chars as read.
  !>
  !> Fortran 2008 is required for ISO_FORTRAN_ENV
  !>
  !> Note that the only functions you should call from outside are
  !> get_arg and get_arg_value for 'key=value' arguments, arg_present
  !> for flag arguments, and arg_count to get total count
  !> NOTE: total count may not match COMMAND_ARGUMENT_COUNT due to
  !> parsing spaces out of key( )=()value syntax!
  !> A complete example code is:
  ! @snippet command_line_snippet.f90 Cmd eg
  !> @include command_line_snippet.f90

  !> @author H Ratcliffe

MODULE command_line

  USE ISO_FORTRAN_ENV

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: get_arg, get_arg_value, arg_present, arg_count
  !> Length of character value string
  INTEGER, PARAMETER :: vlen = 30

  LOGICAL :: initial_parse_done = .FALSE.

  !> Type containing a key-value pair
  ! Init. to default values
  TYPE cmd_arg
    CHARACTER(LEN=20) :: name = "NULL"
    CHARACTER(LEN=vlen) :: value = ""
  END TYPE

  !> @brief Read arguments by name or number
  !>
  !> Get arguments. If the first parameter is a string,
  !> this is interpreted as the name of the parameter, if
  !> an integer, it is the position of the argument in the input list.
  !> @param name Name to look up (supply this OR num)
  !> @param num Arg. number to look up (supply this OR name)
  !> @param val Value to read into, with type matching that to parse as
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  INTERFACE get_arg
    MODULE PROCEDURE get_arg_num_int, get_arg_name_int
    MODULE PROCEDURE get_arg_num_long, get_arg_name_long
    MODULE PROCEDURE get_arg_num_float, get_arg_name_float
    MODULE PROCEDURE get_arg_num_dbl, get_arg_name_dbl
    MODULE PROCEDURE get_arg_num_str, get_arg_name_str
  END INTERFACE

  !> The argument list
  TYPE(cmd_arg), DIMENSION(:), ALLOCATABLE :: all_args
  !> The number of arguments
  INTEGER :: num_args = 0
  PRIVATE :: all_args, num_args

  CONTAINS

 ! /TODO regroup on = sign in case of spaces
 ! if = not present, then thing is a flag...
  !> @brief Parse out command line args
  ! This function can be called multiple times
  ! and will freshly parse ALL arguments each time
  ! We assume these are entered as 'name=value'. If there is
  ! no '=' sign, we set an empty value
  SUBROUTINE parse_args()

    ! Strictly we can't be sure 50 chars is enough
    ! but for command-line args it's enough if we're sensible
    ! We wont overflow, but our strings may get truncated
    INTEGER :: i_arg, i_tok, indx
    TYPE(cmd_arg), DIMENSION(:), ALLOCATABLE :: all_args_tmp
    CHARACTER(LEN=51) :: arg, tmp

    num_args = COMMAND_ARGUMENT_COUNT()
    IF(num_args > 0) THEN

      ! If this is not the first call, all_args may be already allocated
      ! Deallocate if needed, and allocate to correct size
      IF(ALLOCATED(all_args)) DEALLOCATE(all_args)
      ALLOCATE(all_args(num_args))

      i_arg = 1 !Index of current arg
      i_tok = 1 ! Index of current input token
      ! Loop over all arguments
      DO WHILE (i_tok <= num_args)
        CALL GET_COMMAND_ARGUMENT(i_tok, arg)
        i_tok = i_tok + 1

        ! Location of the '=' sign
        ! If not found, return value of this is 0
        indx = INDEX(arg, '=')

        !Look at next chars - remove all whitespace
        tmp = TRIM(ADJUSTL(arg(indx+1:)))
        IF(indx > 1 .AND. LEN(TRIM(ADJUSTL(arg(indx+1:)))) > 0) THEN
          ! All characters up to '=', not including it
          ! but with any leading spaces removed
          all_args(i_arg)%name = ADJUSTL(arg(1:indx-1))
          ! All characters after '='
          all_args(i_arg)%value = tmp
        ELSE IF(indx > 1) THEN
          ! Have an '=' but no following value
          ! Consume next token
          CALL GET_COMMAND_ARGUMENT(i_tok, tmp)
          i_tok = i_tok + 1
          all_args(i_arg)%name = ADJUSTL(arg(1:indx-1))
          all_args(i_arg)%value = tmp
        ELSE   ! Have not yet found the equals!
          ! Set name, then hunt value...
          all_args(i_arg)%name = TRIM(ADJUSTL(arg))

          !Peek next token - will need either 0, 1 or 2 more
          CALL GET_COMMAND_ARGUMENT(i_tok, tmp)
          indx = INDEX(ADJUSTL(tmp), '=')
          IF(indx /= 1) THEN
            ! Next token does not lead with '=', assume this is a flag and
            ! DO NOT consume next. Set value for clarity
            all_args(i_arg)%value = ""
          ELSE
            ! Consume this one and possibly one more
            i_tok = i_tok + 1
            IF(LEN(TRIM(ADJUSTL(tmp))) > 1) THEN
              !This token has content
              all_args(i_arg)%value = ADJUSTL(tmp(2:))
            ELSE
              ! Consume another
              CALL GET_COMMAND_ARGUMENT(i_tok, tmp)
              i_tok = i_tok + 1
              all_args(i_arg)%value = ADJUSTL(tmp)
            END IF
          END IF
        END IF
        i_arg = i_arg + 1
      END DO
    ENDIF

    !i_arg is now the actual parsed count
    !Shrink array to get rid of excess unfilled space
    num_args = i_arg-1
    CALL MOVE_ALLOC(all_args, all_args_tmp)
    ALLOCATE(all_args(num_args))
    all_args = all_args_tmp(1:num_args)
    DEALLOCATE(all_args_tmp)

  END SUBROUTINE parse_args

  !> Helper function - do a parse if it hasn't been done yet
  SUBROUTINE initial_parse

    IF(.NOT. initial_parse_done) THEN
      CALL parse_args
      initial_parse_done = .TRUE.
    END IF

  END SUBROUTINE initial_parse

  !> Get the number of arguments
  !> NOTE: total count may not match COMMAND_ARGUMENT_COUNT due to
  !> parsing key=value syntax!
  FUNCTION arg_count()
    INTEGER :: arg_count

    CALL initial_parse
    arg_count = num_args
  END FUNCTION

!------------------------------------------------------------------

  !> @brief Read by number for double precision values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_dbl(num, val, exists)

    LOGICAL :: get_arg_num_dbl
    INTEGER, INTENT(IN) :: num
    REAL(KIND=REAL64), INTENT(OUT) :: val
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    CALL initial_parse

    found = .FALSE.
    ! Check requested number is in range
    IF(num <= num_args .AND. num > 0) THEN
      ! READ it from string into value
      READ(all_args(num)%value, *, IOSTAT=ierr) val
      found = .TRUE.
    END IF

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_num_dbl = (found .AND. (ierr == 0))

  END FUNCTION get_arg_num_dbl

  !> @brief Read by name for double precision values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_dbl(name, val, exists)

    LOGICAL :: get_arg_name_dbl
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(KIND=REAL64), INTENT(OUT) :: val
    INTEGER :: i
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    CALL initial_parse

    found = .FALSE.
    ! Our cmd_arg type is already initialised to the sentinel
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        READ(all_args(i)%value, *, IOSTAT=ierr) val
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_name_dbl = (found .AND. (ierr == 0))

  END FUNCTION get_arg_name_dbl

  ! Command line parsing should be avoided in performance critical code
  ! so extra overhead from double call and downcast is not a problem

! \TODO Use this approach, or just dupe. all the code?

  !> @brief Read by number for single precision (float) values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_float(num, val, exists)
    LOGICAL :: get_arg_num_float
    INTEGER, INTENT(IN) :: num
    REAL(KIND=REAL32), INTENT(OUT) :: val
    REAL(KIND=REAL64) :: tmp
    LOGICAL, INTENT(OUT), OPTIONAL :: exists

    get_arg_num_float = get_arg_num_dbl(num, tmp, exists)
    IF( ABS(tmp) < HUGE(val)) THEN
      !Value in range. Convert. Note: there may be precision loss
      val = REAL(tmp, KIND=REAL32)
    ELSE
      !Value out of range, can't be parsed
      get_arg_num_float  = .FALSE.
    END IF

  END FUNCTION

  !> @brief Read by name for single precision (float) values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_float(name, val, exists)
    LOGICAL :: get_arg_name_float
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(KIND=REAL32), INTENT(OUT) :: val
    REAL(KIND=REAL64) :: tmp
    LOGICAL, INTENT(OUT), OPTIONAL :: exists

    get_arg_name_float = get_arg_name_dbl(name, tmp, exists)
    IF( ABS(tmp) < HUGE(val)) THEN
      !Value in range. Convert. Note: there may be precision loss
      val = REAL(tmp, KIND=REAL32)
    ELSE
      !Value out of range, can't be parsed
      get_arg_name_float  = .FALSE.
    END IF

  END FUNCTION


  !> @brief Read by number for integer values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_int(num, val, exists)

    LOGICAL :: get_arg_num_int
    INTEGER, INTENT(IN) :: num
    INTEGER(KIND=INT32), INTENT(OUT) :: val
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    CALL initial_parse

    found = .FALSE.
    ! Check requested number is in range
    IF(num <= num_args .AND. num > 0) THEN
      ! READ it from string into value
      ! We don't need to specify the format in general
      READ(all_args(num)%value, *, IOSTAT=ierr) val
      found = .TRUE.
    END IF

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_num_int = (found .AND. (ierr == 0))

  END FUNCTION get_arg_num_int

  !> @brief Read by name for integer values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_int(name, val, exists)

    LOGICAL :: get_arg_name_int
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(KIND=INT32), INTENT(OUT) :: val
    INTEGER :: i
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    CALL initial_parse

    found = .FALSE.
    ! Our cmd_arg type is already initialised to the sentinel
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        READ(all_args(i)%value, *, IOSTAT=ierr) val
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_name_int = (found .AND. (ierr == 0))

  END FUNCTION get_arg_name_int

  !> @brief Read by number for long integer values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_long(num, val, exists)

    LOGICAL :: get_arg_num_long
    INTEGER, INTENT(IN) :: num
    INTEGER(KIND=INT64), INTENT(OUT) :: val
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    CALL initial_parse

    found = .FALSE.
    ! Check requested number is in range
    IF(num <= num_args .AND. num > 0) THEN
      ! READ it from string into value
      ! We don't need to specify the format in general
      READ(all_args(num)%value, *, IOSTAT=ierr) val
      found = .TRUE.
    END IF

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_num_long = (found .AND. (ierr == 0))

  END FUNCTION get_arg_num_long

  !> @brief Read by name for long integer values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_long(name, val, exists)

    LOGICAL :: get_arg_name_long
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(KIND=INT64), INTENT(OUT) :: val
    INTEGER :: i
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

   CALL initial_parse

    found = .FALSE.
    ! Our cmd_arg type is already initialised to the sentinel
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        READ(all_args(i)%value, *, IOSTAT=ierr) val
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_name_long = (found .AND. (ierr == 0))

  END FUNCTION get_arg_name_long

  !> @brief Read by number for string/character values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found - this is already contained in
  !> the return value, but is given for consistency with the other members
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_str(num, val, exists)

    LOGICAL :: get_arg_num_str
    INTEGER, INTENT(IN) :: num
    CHARACTER(LEN=*), INTENT(OUT) :: val
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found

    CALL initial_parse

    found = .FALSE.
    ! Check requested number is in range
    IF(num <= num_args .AND. num > 0) THEN
      ! READ it from string into value
      ! We don't need to specify the format in general
      val = all_args(num)%value
      found = .TRUE.
    END IF

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_num_str = found

  END FUNCTION get_arg_num_str

  !> @brief Read by name for string values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found - this is already contained in
  !> the return value, but is given for consistency with the other members
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_str(name, val, exists)

    LOGICAL :: get_arg_name_str
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*), INTENT(OUT) :: val
    INTEGER :: i
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found

    CALL initial_parse

    found = .FALSE.
    ! Our cmd_arg type is already initialised to the sentinel
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        val = all_args(i)%value
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_name_str = found

  END FUNCTION get_arg_name_str


!--------------------------------------------------------------------

  FUNCTION arg_present(name) RESULT(found)

   LOGICAL :: found
   CHARACTER(LEN=*), INTENT(IN) :: name
   INTEGER :: i

    CALL initial_parse

    found = .FALSE.
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        EXIT
      ENDIF
    END DO

  END FUNCTION arg_present

  !> @brief Lookup an argument by name and return the value as a string
  !> @param name Argument name to look up
  !> @param exists Whether the name was found
  !> @return The string value associated with the given name
  FUNCTION get_arg_value(name, exists)

    CHARACTER(LEN=vlen) :: get_arg_value
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    TYPE(cmd_arg) :: tmp
    INTEGER :: i
    LOGICAL :: found

    CALL initial_parse

    ! Initialise to the default value
    ! Use a temporary to get the default values
    ! This makes sure we match the expected sentinel
    get_arg_value = tmp%value
    found = .FALSE.

    DO i = 1, num_args
      IF(all_args(i)%name .EQ. TRIM(ADJUSTL(name))) THEN
        get_arg_value = all_args(i)%value
        found = .TRUE.
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

  END FUNCTION get_arg_value


END MODULE command_line
