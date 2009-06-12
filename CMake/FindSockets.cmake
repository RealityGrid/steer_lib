#
#  CMake rules to find libsocket and friends.
#
#  (C) Copyright 2009, University of Manchester, United Kingdom,
#  all rights reserved.
#
#  This software was developed by the RealityGrid project
#  (http://www.realitygrid.org), funded by the EPSRC under grants
#  GR/R67699/01 and GR/R67699/02.
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
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
#  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
#  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
#  CORRECTION.
#
#  Author.........: Robert Haines
#
#------------------------------------------------------------------------

if(LIBSOCKET_LIBRARIES)
  # already in cache so be quiet
  set(LIBSOCKET_FIND_QUIETLY TRUE)
endif(LIBSOCKET_LIBRARIES)

# find the various bits as they are split
# differently on different platforms
find_library(LIBRESOLV_LIB resolv)
find_library(LIBSOCKET_LIB socket)
find_library(LIBNSL_LIB nsl)

# build up a link line with any or none of the three libraries
if(NOT LIBRESOLV_LIB STREQUAL "LIBRESOLV_LIB-NOTFOUND")
  set(LIBSOCKET_LIBRARIES ${LIBRESOLV_LIB})
endif(NOT LIBRESOLV_LIB STREQUAL "LIBRESOLV_LIB-NOTFOUND")

if(NOT LIBSOCKET_LIB STREQUAL "LIBSOCKET_LIB-NOTFOUND")
  set(LIBSOCKET_LIBRARIES ${LIBSOCKET_LIBRARIES} ${LIBSOCKET_LIB})
endif(NOT LIBSOCKET_LIB STREQUAL "LIBSOCKET_LIB-NOTFOUND")

if(NOT LIBNSL_LIB STREQUAL "LIBNSL_LIB-NOTFOUND")
  set(LIBSOCKET_LIBRARIES ${LIBSOCKET_LIBRARIES} ${LIBNSL_LIB})
endif(NOT LIBNSL_LIB STREQUAL "LIBNSL_LIB-NOTFOUND")

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(LIBSOCKET DEFAULT_MSG LIBSOCKET_LIBRARIES)

mark_as_advanced(LIBRESOLV_LIB LIBSOCKET_LIB LIBNSL_LIB)
