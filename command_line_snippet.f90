! An example of how to use the command_line library

! Uses command_line.f90
! gfortran --std=f2008 command_line.f90 command_line_eg.f90
!
! Example input: ./a.out val=abc val1=10 val2=hi
! There are            3 arguments
! Arg val1 is present
! Got arg val1 of    10.000000000000000       as a REAL
! Got arg val1 of    10.0000000      as a REAL32
! Got arg val2 as hi                        as a string
! Failed to get arg val3 as an int
!   Value was not found in input
! Failed to get first arg as a REAL
! Failed to get thrid arg as an INT

! Example Input ./a.out val1=10 val2=hi val3=10
! There are            3 arguments
! Arg val1 is present
! Got arg val1 of    10.000000000000000       as a REAL
! Got arg val1 of    10.0000000      as a REAL32
! Got arg val2 as hi                        as a string
! Got arg val3 as           10  as an int
! Got first arg of    10.000000000000000       as a REAL
! Got thrid arg of           10  as an INT

! Example Input ./a.out val1=1e50
! There are            1 arguments
! Arg val1 is present
! Got arg val1 of    1.0000000000000001E+050  as a REAL
! Failed to get arg val1 as a REAL32
! Failed to get arg val2 as a string
! Failed to get arg val3 as an int
!   Value was not found in input
! Got first arg of    1.0000000000000001E+050  as a REAL
! Failed to get thrid arg as an INT


PROGRAM MAIN

  USE ISO_FORTRAN_ENV
  USE command_line

  IMPLICIT NONE
  LOGICAL :: success, exists
  REAL(KIND=REAL64) :: real_val
  REAL(KIND=REAL32) :: real32_val
  CHARACTER(LEN=25) :: str_val
  INTEGER(KIND=INT32) :: int_val
  INTEGER :: total

  ! Checking for presence and count
  success = arg_present("val1")
  total = arg_count()
  PRINT*, "There are ", total, "arguments"
  IF(success) THEN
    PRINT*, "Arg val1 is present"
  ELSE
    PRINT*, "Arg val1 is not present"
  END IF

  ! Try to get arg named val1 as a REAL
  success = get_arg("val1", real_val)

  IF(success) THEN
    PRINT*, "Got arg val1 of ", real_val, " as a REAL"
  ELSE
    PRINT*, "Failed to get arg val1 as a REAL"
  END IF

  success = get_arg("val1", real32_val)

  IF(success) THEN
    PRINT*, "Got arg val1 of ", real32_val, " as a REAL32"
  ELSE
    PRINT*, "Failed to get arg val1 as a REAL32"
  END IF


  ! Try to get arg named val2 as a String
  success = get_arg("val2", str_val)

  IF(success) THEN
    PRINT*, "Got arg val2 as ", str_val, " as a string"
  ELSE
    PRINT*, "Failed to get arg val2 as a string"
  END IF

  ! Try to get arg named val3 as an Int
  success = get_arg("val3", int_val, exists=exists)

  IF(success) THEN
    PRINT*, "Got arg val3 as ", int_val, " as an int"
  ELSE
    PRINT*, "Failed to get arg val3 as an int"
    IF(exists) THEN
      PRINT*, "  Value could not be parsed"
    ELSE
      PRINT*, "  Value was not found in input"
    END IF
  END IF


  ! Try to get the first argument given as a REAL
  success = get_arg(1, real_val)

  IF(success) THEN
    PRINT*, "Got first arg of ", real_val, " as a REAL"
  ELSE
    PRINT*, "Failed to get first arg as a REAL"
  END IF

  ! Try to get the third argument given as a INT
  success = get_arg(3, int_val)

  IF(success) THEN
    PRINT*, "Got thrid arg of ", int_val, " as an INT"
  ELSE
    PRINT*, "Failed to get thrid arg as an INT"
  END IF


END PROGRAM
!> [Cmd eg]
