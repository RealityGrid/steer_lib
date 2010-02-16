!
!  The RealityGrid Steering Library
!
!  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
!  All rights reserved.
!
!  This software is produced by Research Computing Services, University
!  of Manchester as part of the RealityGrid project and associated
!  follow on projects, funded by the EPSRC under grants GR/R67699/01,
!  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
!  EP/F00561X/1.
!
!  LICENCE TERMS
!
!  Redistribution and use in source and binary forms, with or without
!  modification, are permitted provided that the following conditions
!  are met:
!
!    * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!
!    * Redistributions in binary form must reproduce the above
!      copyright notice, this list of conditions and the following
!      disclaimer in the documentation and/or other materials provided
!      with the distribution.
!
!    * Neither the name of The University of Manchester nor the names
!      of its contributors may be used to endorse or promote products
!      derived from this software without specific prior written
!      permission.
!
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
!  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
!  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
!  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
!  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
!  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!  POSSIBILITY OF SUCH DAMAGE.
!
!  Author: Andrew Porter
!          Robert Haines

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

      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_MAX_NUM_STR_PARAMS = 40

! Initial sizes for internal tables of registered IO types
! and parameters

      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_INITIAL_NUM_IOTYPES = 20
      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_INITIAL_NUM_PARAMS = 40

! Initial limit on no. of registered supported commands

      INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_INITIAL_NUM_CMDS = 20

! Initial limit on no. of sims being steered at any one time

      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_MAX_NUM_STEERED_SIM = 5

! Limit on number of files used in communication - filenames
!  wrap back on themselves once this limit reached

      INTEGER (KIND=REG_SP_KIND), PARAMETER :: REG_MAX_NUM_FILES = 500

! Maximum length for any string

      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_MAX_STRING_LENGTH = 256

! Steering commands

      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_STR_STOP             = 1
      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_STR_PAUSE            = 2
      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_STR_RESUME           = 3
      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_STR_DETACH           = 4
      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_STR_EMIT_PARAM_LOG   = 5
      INTEGER (KIND=REG_SP_KIND), PARAMETER ::
     +REG_STR_PAUSE_INTERNAL   = 6


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
