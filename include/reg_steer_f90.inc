!---------------------------------------------------------------------
!  F90 include file intended to give F90 applications access to 
!  the RealityGrid computational-steering API.
!
!  (C) Copyright 2006, University of Manchester, United Kingdom,
!  all rights reserved.
!
!  This software was developed by the RealityGrid project
!  (http://www.realitygrid.org), funded by the EPSRC under grants
!  GR/R67699/01 and GR/R67699/02.
!
!  LICENCE TERMS
!
!  Redistribution and use in source and binary forms, with or without
!  modification, are permitted provided that the following conditions
!  are met:
!  1. Redistributions of source code must retain the above copyright
!     notice, this list of conditions and the following disclaimer.
!  2. Redistributions in binary form must reproduce the above copyright
!     notice, this list of conditions and the following disclaimer in the
!     documentation and/or other materials provided with the distribution.
!
!  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
!  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
!  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
!  CORRECTION.  
!
!  Authors........: Andrew Porter, Robert Haines
!---------------------------------------------------------------------

! KIND parameters to ensure we match types when passing between
! C and F90
  INTEGER, PARAMETER :: REG_INT_KIND = KIND(1)
  INTEGER, PARAMETER :: REG_SP_KIND = KIND(1.0)
  INTEGER, PARAMETER :: REG_DP_KIND = KIND(1.0D0)

! These are variables, not parameters so as to allow call by
! reference when passing into C routines

  INTEGER (KIND=REG_SP_KIND) :: reg_true  = 1
  INTEGER (KIND=REG_SP_KIND) :: reg_false = 0

! Return values

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_SUCCESS = 0
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_FAILURE = 1
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_EOD     = 2
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_MEM_FAIL= 3

! Limit on number of commands steerer can send at any one time

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_MAX_NUM_STR_CMDS = 20

! Limit on number of parameters steerer can modify at any one time

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_MAX_NUM_STR_PARAMS = 40

! Initial sizes for internal tables of registered IO types 
! and parameters

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_INITIAL_NUM_IOTYPES = 20
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_INITIAL_NUM_PARAMS = 40

! Initial limit on no. of registered supported commands

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_INITIAL_NUM_CMDS = 20

! Initial limit on no. of sims being steered at any one time

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_MAX_NUM_STEERED_SIM = 5

! Limit on number of files used in communication - filenames
!  wrap back on themselves once this limit reached

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_MAX_NUM_FILES = 500

! Maximum length for any string

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_MAX_STRING_LENGTH = 256

! Steering commands

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_STR_STOP             = 1
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_STR_PAUSE            = 2
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_STR_RESUME           = 3
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_STR_DETACH           = 4
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_STR_EMIT_PARAM_LOG   = 5
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_STR_PAUSE_INTERNAL   = 6


! Coding scheme for data types

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_INT   = 0
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_FLOAT = 1
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_DBL   = 2
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_CHAR  = 3
  ! Values 4-6 are used for internal (XDR-related) types in the lib
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_BIN   = 7

! Coding scheme for IOType types

  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_IO_IN    = 0
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_IO_OUT   = 1
  INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_IO_INOUT = 2





