#
#  CMake build machinery for the RealityGrid Steering Library.
#
#  (C) Copyright 2004-2009, University of Manchester, United Kingdom,
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

if(REG_BUILD_MODULAR_LIBS)
  set(wsrf_srcs ${Steering_WSRF_SRCS})
else(REG_BUILD_MODULAR_LIBS)
  set(wsrf_srcs ${SRCS_Steering_WSRF})
endif(REG_BUILD_MODULAR_LIBS)

get_source_file_property(
  flags
  ReG_Steer_Steering_Transport_WSRF.c
  COMPILE_FLAGS
)

if(NOT ${flags})
  set(flags "")
endif(NOT ${flags})

set_source_files_properties(
  ${wsrf_srcs}
  PROPERTIES COMPILE_FLAGS "${flags} -DWITH_CDATA -DWITH_OPENSSL"
)
