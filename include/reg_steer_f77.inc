c---------------------------------------------------------------------
c  F77 include file intended to give F77 applications access to 
c  the RealityGrid computational-steering API.
c
c  (C) Copyright 2006, University of Manchester, United Kingdom,
c  all rights reserved.
c
c  This software was developed by the RealityGrid project
c  (http://www.realitygrid.org), funded by the EPSRC under grants
c  GR/R67699/01 and GR/R67699/02.
c
c  LICENCE TERMS
c
c  Redistribution and use in source and binary forms, with or without
c  modification, are permitted provided that the following conditions
c  are met:
c  1. Redistributions of source code must retain the above copyright
c     notice, this list of conditions and the following disclaimer.
c  2. Redistributions in binary form must reproduce the above copyright
c     notice, this list of conditions and the following disclaimer in the
c     documentation and/or other materials provided with the distribution.
c
c  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
c  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
c  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
c  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
c  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
c  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
c  CORRECTION.  
c
c  Authors........: Andrew Porter, Robert Haines
c---------------------------------------------------------------------

c Declarations of RealityGrid steering functions - these are actually
c wrappers for the library routines (in C)

      EXTERNAL steering_enable_f
      EXTERNAL steering_initialize_f
      EXTERNAL steering_finalize_f
      EXTERNAL register_iotypes_f
      EXTERNAL register_chktypes_f
      EXTERNAL register_param_f
      EXTERNAL register_string_param_f
      EXTERNAL record_chkpt_f
      EXTERNAL consume_start_f
      EXTERNAL consume_stop_f
      EXTERNAL steering_control_f
      EXTERNAL steering_pause_f
      EXTERNAL steering_char_to_ptr_f

c KIND parameters to ensure we match types when passing between
c C and F90

c  INTEGER, PARAMETER :: REG_SP_KIND = KIND(1)
c  INTEGER, PARAMETER :: REG_DP_KIND = KIND(1.0D0)
  
c      INTEGER REG_SP_KIND
c      PARAMETER(REG_SP_KIND = KIND(1))
c      INTEGER REG_DP_KIND
c      PARAMETER(REG_DP_KIND = KIND(1.0D0))

c These are variables, not parameters so as to allow call by
c reference when passing into C routines
      INTEGER  reg_true
      PARAMETER (reg_true  = 1)
      INTEGER reg_false
      PARAMETER (reg_false = 0)
  
  
c Return values

      INTEGER REG_SUCCESS
      PARAMETER (REG_SUCCESS = 0)
      INTEGER REG_FAILURE
      PARAMETER (REG_FAILURE = 1)  
      INTEGER REG_EOD
      PARAMETER (REG_EOD = 3)  
      INTEGER REG_MEM_FAIL
      PARAMETER (REG_MEM_FAIL = 3)  
  
  
c Limit on number of commands steerer can send at any one time

      INTEGER REG_MAX_NUM_STR_CMDS
      PARAMETER (REG_MAX_NUM_STR_CMDS = 20)
  
c Limit on number of parameters steerer can modify at any one time
  
      INTEGER REG_MAX_NUM_STR_PARAMS
      PARAMETER (REG_MAX_NUM_STR_PARAMS = 40)

c Initial sizes for internal tables of registered IO types 
c and parameters

      INTEGER  REG_INITIAL_NUM_IOTYPES
      PARAMETER (REG_INITIAL_NUM_IOTYPES = 20)
      INTEGER REG_INITIAL_NUM_PARAMS
      PARAMETER (REG_INITIAL_NUM_PARAMS = 40)
  
c Initial limit on no. of registered supported commands

      INTEGER  REG_INITIAL_NUM_CMDS
      PARAMETER (REG_INITIAL_NUM_CMDS = 20)

  
c Initial limit on no. of sims being steered at any one time

      INTEGER  REG_MAX_NUM_STEERED_SIM
      PARAMETER(REG_MAX_NUM_STEERED_SIM = 5)
  

c Limit on number of files used in communication - filenames
c  wrap back on themselves once this limit reached

      INTEGER  REG_MAX_NUM_FILES
      PARAMETER (REG_MAX_NUM_FILES = 500)

      
c Maximum length for any string

      INTEGER REG_MAX_STRING_LENGTH
      PARAMETER (REG_MAX_STRING_LENGTH = 256)

      
c Steering commands
  
      INTEGER  REG_STR_STOP
      PARAMETER (REG_STR_STOP = 1)
      INTEGER  REG_STR_PAUSE
      PARAMETER (REG_STR_PAUSE = 2)
      INTEGER  REG_STR_RESUME
      PARAMETER (REG_STR_RESUME = 3)
      INTEGER  REG_STR_DETACH
      PARAMETER (REG_STR_DETACH = 4)
  

c Coding scheme for data types
  
      INTEGER  REG_INT
      PARAMETER (REG_INT = 0)
      INTEGER  REG_FLOAT
      PARAMETER (REG_FLOAT = 1)
      INTEGER  REG_DBL
      PARAMETER (REG_DBL = 2)
      INTEGER  REG_CHAR
      PARAMETER (REG_CHAR = 3)
  
      
c Coding scheme for IOType types

      INTEGER  REG_IO_IN
      PARAMETER (REG_IO_IN = 0)
      INTEGER  REG_IO_OUT
      PARAMETER (REG_IO_OUT = 1)
      INTEGER  REG_IO_INOUT
      PARAMETER (REG_IO_INOUT = 2)
  



