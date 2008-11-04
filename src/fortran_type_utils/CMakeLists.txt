#
#  CMake build machinery for the RealityGrid Steering Library.
#
#  (C) Copyright 2004-2008, University of Manchester, United Kingdom,
#  all rights reserved.
#
#  This software is produced by the Supercomputing, Visualization and
#  e-Science Group, Manchester Computing, University of Manchester
#  as part of the RealityGrid project (http://www.realitygrid.org),
#  funded by the EPSRC under grants GR/R67699/01 and GR/R67699/02.
#
#  LICENCE TERMS
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#
#  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
#  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
#  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
#  CORRECTION.
#
#  Author.........: Robert Haines
#----------------------------------------------------------------------

# Build sizeof_f.c from ReG_Steer_f_macros.m4 and sizeof_f.m4
add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/sizeof_f.c
  COMMAND ${M4}
  ARGS ${PROJECT_BINARY_DIR}/ReG_Steer_f_macros.m4 ${CMAKE_CURRENT_SOURCE_DIR}/sizeof_f.m4 > ${CMAKE_CURRENT_BINARY_DIR}/sizeof_f.c
  COMMENT "Generating Fortran type utilities bindings"
)

# build the executables
add_executable(max_kind max_kind.f90 ${CMAKE_CURRENT_BINARY_DIR}/sizeof_f.c)
add_executable(c_types c_types.c)
