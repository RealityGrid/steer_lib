!---------------------------------------------------------------------
! F90 include file intended to give F90 applications access to 
! the RealityGrid computational-steering API.
!
! (C)Copyright 2002 The University of Manchester, United Kingdom,
! all rights reserved.
!
! This software is produced by the Supercomputing, Visualization &
! e-Science Group, Manchester Computing, the Victoria University of
! Manchester as part of the RealityGrid project.
!
! This software has been tested with care but is not guaranteed for
! any particular purpose. Neither the copyright holder, nor the
! University of Manchester offer any warranties or representations,
! nor do they accept any liabilities with respect to this software.
!
! This software must not be used for commercial gain without the
! written permission of the authors.
! 
! This software must not be redistributed without the written
! permission of the authors.
!
! Permission is granted to modify this software, provided any
! modifications are made freely available to the original authors.
! 
! Supercomputing, Visualization & e-Science Group
! Manchester Computing
! University of Manchester
! Manchester M13 9PL
! 
! WWW:    http://www.sve.man.ac.uk  
! email:  sve@man.ac.uk
! Tel:    +44 161 275 6095
! Fax:    +44 161 275 6800    
!
! Initial version by:  A Porter.
!---------------------------------------------------------------------

! Declarations of RealityGrid steering functions - these are actually
! wrappers for the library routines (in C)

  EXTERNAL steering_enable_f
  EXTERNAL steering_initialize_f
  EXTERNAL steering_finalize_f
  EXTERNAL register_iotypes_f
  EXTERNAL register_chktypes_f
  EXTERNAL register_params_f
  EXTERNAL record_chkpt_f
  EXTERNAL consume_start_f
  EXTERNAL consume_stop_f
  EXTERNAL steering_control_f
  EXTERNAL steering_pause_f
  EXTERNAL steering_char_to_ptr_f

! KIND parameters to ensure we match types when passing between
! C and F90
  INTEGER, PARAMETER :: REG_SP_KIND = KIND(1)
  INTEGER, PARAMETER :: REG_DP_KIND = KIND(1.0D0)

! These are variables, not parameters so as to allow call by
! reference when passing into C routines

  INTEGER (KIND=4) :: reg_true  = 1
  INTEGER (KIND=4) :: reg_false = 0

! Return values

  INTEGER (KIND=4), PARAMETER :: REG_SUCCESS = 0
  INTEGER (KIND=4), PARAMETER :: REG_FAILURE = 1
  INTEGER (KIND=4), PARAMETER :: REG_EOD     = 2
  INTEGER (KIND=4), PARAMETER :: REG_MEM_FAIL= 3

! Limit on number of commands steerer can send at any one time

  INTEGER (KIND=4), PARAMETER :: REG_MAX_NUM_STR_CMDS = 50

! Limit on number of parameters steerer can modify at any one time

  INTEGER (KIND=4), PARAMETER :: REG_MAX_NUM_STR_PARAMS = 100

! Initial sizes for internal tables of registered IO types 
! and parameters

  INTEGER (KIND=4), PARAMETER :: REG_INITIAL_NUM_IOTYPES = 20
  INTEGER (KIND=4), PARAMETER :: REG_INITIAL_NUM_PARAMS = 20

! Initial limit on no. of registered supported commands

  INTEGER (KIND=4), PARAMETER :: REG_INITIAL_NUM_CMDS = 20

! Initial limit on no. of sims being steered at any one time

  INTEGER (KIND=4), PARAMETER :: REG_MAX_NUM_STEERED_SIM = 10

! Limit on number of files used in communication - filenames
!  wrap back on themselves once this limit reached

  INTEGER (KIND=4), PARAMETER :: REG_MAX_NUM_FILES = 100

! Maximum length for any string

  INTEGER (KIND=4), PARAMETER :: REG_MAX_STRING_LENGTH = 80

! Steering commands

  INTEGER (KIND=4), PARAMETER :: REG_STR_STOP             = 1
  INTEGER (KIND=4), PARAMETER :: REG_STR_PAUSE            = 2
  INTEGER (KIND=4), PARAMETER :: REG_STR_RESUME           = 3
  INTEGER (KIND=4), PARAMETER :: REG_STR_DETACH           = 4
  INTEGER (KIND=4), PARAMETER :: REG_STR_SET_EMIT_FREQ    = 5
  INTEGER (KIND=4), PARAMETER :: REG_STR_SET_CONSUME_FREQ = 6

! Coding scheme for data types

  INTEGER (KIND=4), PARAMETER :: REG_INT   = 0
  INTEGER (KIND=4), PARAMETER :: REG_FLOAT = 1
  INTEGER (KIND=4), PARAMETER :: REG_DBL   = 2
  INTEGER (KIND=4), PARAMETER :: REG_CHAR  = 3

! Coding scheme for IOType types

  INTEGER (KIND=4), PARAMETER :: REG_IO_IN    = 0
  INTEGER (KIND=4), PARAMETER :: REG_IO_OUT   = 1
  INTEGER (KIND=4), PARAMETER :: REG_IO_INOUT = 2
