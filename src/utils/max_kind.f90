!----------------------------------------------------------------------------
!    F90 code to investigate meaning of KIND parameters on current
!    machine architecture.
!
!    (C)Copyright 2002 The University of Manchester, United Kingdom,
!    all rights reserved.
!
!    This software is produced by the Supercomputing, Visualization &
!    e-Science Group, Manchester Computing, the Victoria University of
!    Manchester as part of the RealityGrid project.
!
!    This software has been tested with care but is not guaranteed for
!    any particular purpose. Neither the copyright holder, nor the
!    University of Manchester offer any warranties or representations,
!    nor do they accept any liabilities with respect to this software.
!
!    This software must not be used for commercial gain without the
!    written permission of the authors.
!    
!    This software must not be redistributed without the written
!    permission of the authors.
!
!    Permission is granted to modify this software, provided any
!    modifications are made freely available to the original authors.
! 
!    Supercomputing, Visualization & e-Science Group
!    Manchester Computing
!    University of Manchester
!    Manchester M13 9PL
!    
!    WWW:    http://www.sve.man.ac.uk  
!    email:  sve@man.ac.uk
!    Tel:    +44 161 275 6095
!    Fax:    +44 161 275 6800    
!
!    Initial version by: A Porter
!    
!---------------------------------------------------------------------------
PROGRAM max_kind
  IMPLICIT none

  INTEGER :: i
  INTEGER :: ikind
  REAL    :: log10_2
  INTEGER :: KIND_FROM_BITS
  INTEGER :: nbits
  CHARACTER (LEN=10) :: type

! Prints the kind parameter of the largest integer the machine
! can represent.  Also finds the KIND parameter that corresponds to each
! C type.
! ARP, 9/8/2002

  log10_2 = LOG10(2.)

  DO i=128,2,-1

    ! n = SELECTED_IND_KIND(R) returns the KIND parameter of an integer 
    ! that covers -10**R < n < 10**R or -1 if none exists.
    ! Max. signed integer, n, represented by nbits is n = 2**(nbits - 1) - 1
    !ikind = SELECTED_INT_KIND(INT((LOG10(2.0**(i-1) - 1.0))))
    ikind = KIND_FROM_BITS(i)

    IF (ikind .ne. -1) EXIT
  END DO

  IF (ikind .ne. -1) THEN

    WRITE(*,*) 'Max. kind param = ', ikind, ' for +/-',(2.0**(i-1) - 1.0), &
               'or ', i, 'bits'
  ELSE
    WRITE(*,*) 'Didn''t find any valid KIND parameters!'
  END IF

  WRITE (*,*) 'KIND of 1     = ', KIND(1)
  WRITE (*,*) 'KIND of 1.0   = ', KIND(1.)
  WRITE (*,*) 'KIND of 1.0d0 = ', KIND(1.0D0)

  type = " "
  type = "short"
  type(6:6) = CHAR(0)

  CALL sizeof_f(type, nbits)
  IF(nbits .gt. 0)THEN
    nbits = 8*nbits
    WRITE (*,*) 'short (', nbits, ' bits) corresponds to KIND = ', KIND_FROM_BITS(nbits)
  END IF

  type = " "
  type = "int"
  type(4:4) = CHAR(0)

  CALL sizeof_f(type, nbits)
  IF(nbits .gt. 0)THEN
    nbits = 8*nbits
    WRITE (*,*) 'int (', nbits, ' bits) corresponds to KIND = ', KIND_FROM_BITS(nbits)
  END IF

  type = " "
  type = "long"
  type(5:5) = CHAR(0)

  CALL sizeof_f(type, nbits)
  IF(nbits .gt. 0)THEN
    nbits = 8*nbits
    WRITE (*,*) 'long (', nbits, ' bits) corresponds to KIND = ', KIND_FROM_BITS(nbits)
  END IF

  type = " "
  type = "float"
  type(6:6) = CHAR(0)

  CALL sizeof_f(type, nbits)
  IF(nbits .gt. 0)THEN
    nbits = 8*nbits
    WRITE (*,*) 'float (', nbits, ' bits) corresponds to KIND = ', KIND_FROM_BITS(nbits)
  END IF

  type = " "
  type = "double"
  type(7:7) = CHAR(0)

  CALL sizeof_f(type, nbits)
  IF(nbits .gt. 0)THEN
    nbits = 8*nbits
    WRITE (*,*) 'double (', nbits, ' bits) corresponds to KIND = ', KIND_FROM_BITS(nbits)
  END IF

END PROGRAM max_kind

FUNCTION KIND_FROM_BITS(nbits)
  IMPLICIT none

  INTEGER :: KIND_FROM_BITS
  INTEGER :: nbits

  ! n = SELECTED_IND_KIND(R) returns the KIND parameter of an integer 
  ! that covers -10**R < n < 10**R or -1 if none exists.
  ! Max. signed integer, n, represented by nbits is n = 2**(nbits - 1) - 1

  KIND_FROM_BITS = SELECTED_INT_KIND(INT((LOG10(2.0**(nbits-1) - 1.0))))

END FUNCTION KIND_FROM_BITS
