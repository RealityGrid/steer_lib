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

# need to link the rpc lib for xdr stuff
find_library(LIB_RPC rpc)
mark_as_advanced(LIB_RPC)

if(NOT LIB_RPC STREQUAL "LIB_RPC-NOTFOUND")
  set(REG_EXTERNAL_LIBS ${REG_EXTERNAL_LIBS} ${LIB_RPC})
endif(NOT LIB_RPC STREQUAL "LIB_RPC-NOTFOUND")

# this could be done more properly using the more windowsy
# __declspec(dllexport) and __declspec(dllimport) stuff but
# this will suffice until a full windows port is attempted
set(CMAKE_C_FLAGS
  "-enable-auto-import"
  CACHE STRING
  "Flags used by the compiler during all build types."
  FORCE
)
