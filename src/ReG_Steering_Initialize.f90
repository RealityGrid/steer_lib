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

SUBROUTINE steering_initialize_f(AppName, NumSupportedCmds, &
                               SupportedCmds, Status)
  IMPLICIT none
  INCLUDE 'reg_steer_f90.inc'

  CHARACTER (LEN=*), INTENT(in)                         :: AppName
  INTEGER (KIND=REG_INT_KIND), INTENT(in)               :: NumSupportedCmds
  INTEGER (KIND=REG_INT_KIND), DIMENSION(*), INTENT(in) :: SupportedCmds
  INTEGER (KIND=REG_INT_KIND), INTENT(out)              :: Status

  INTEGER (KIND=REG_INT_KIND), DIMENSION(2) :: int_array
  REAL (KIND=REG_SP_KIND), DIMENSION(2)    :: real_array
  REAL (KIND=REG_DP_KIND), DIMENSION(2)    :: dbl_array

  ! These calls are the reason this F90 wrapper exists - allows us to
  ! calculate the sizeof(each ReG type).  This info. is used in
  ! Register_bin_param_f(...)
  CALL set_type_size(REG_CHAR, AppName(1:1), AppName(2:2), Status)
  CALL set_type_size(REG_INT, int_array(1), int_array(2), Status)
  CALL set_type_size(REG_FLOAT, real_array(1), real_array(2), Status)
  CALL set_type_size(REG_DBL, dbl_array(1), dbl_array(2), Status)

  CALL steering_initialize_wrapper(AppName, NumSupportedCmds, &
                                   SupportedCmds, Status)
  RETURN
END SUBROUTINE steering_initialize_f
