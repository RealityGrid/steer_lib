!--------------------------------------------------------------------------
!  This file contains routines providing an F90 interface layer
!  to the (C) steering library.
!
!  (C) Copyright 2005, University of Manchester, United Kingdom,
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
!  Authors........: Andrew Porter
!-------------------------------------------------------------------------

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
