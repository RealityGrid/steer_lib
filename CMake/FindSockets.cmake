#
#  The RealityGrid Steering Library
#
#  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
#  All rights reserved.
#
#  This software is produced by Research Computing Services, University
#  of Manchester as part of the RealityGrid project and associated
#  follow on projects, funded by the EPSRC under grants GR/R67699/01,
#  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
#  EP/F00561X/1.
#
#  LICENCE TERMS
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#    * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials provided
#      with the distribution.
#
#    * Neither the name of The University of Manchester nor the names
#      of its contributors may be used to endorse or promote products
#      derived from this software without specific prior written
#      permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
#  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
#  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
#  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.
#
#  Author: Robert Haines

if(LIBSOCKET_LIBRARIES)
  # already in cache so be quiet
  set(LIBSOCKET_FIND_QUIETLY TRUE)
endif(LIBSOCKET_LIBRARIES)

# find the various bits as they are split
# differently on different platforms
find_library(LIBRESOLV_LIB resolv)
find_library(LIBSOCKET_LIB socket)
find_library(LIBNSL_LIB nsl)
find_library(LIBWS2_LIB Ws2_32)

# build up a link line with any or none of the three unix libraries
# or Winsock2
if(NOT LIBRESOLV_LIB STREQUAL "LIBRESOLV_LIB-NOTFOUND")
  set(LIBSOCKET_LIBRARIES ${LIBRESOLV_LIB})
endif(NOT LIBRESOLV_LIB STREQUAL "LIBRESOLV_LIB-NOTFOUND")

if(NOT LIBSOCKET_LIB STREQUAL "LIBSOCKET_LIB-NOTFOUND")
  set(LIBSOCKET_LIBRARIES ${LIBSOCKET_LIBRARIES} ${LIBSOCKET_LIB})
endif(NOT LIBSOCKET_LIB STREQUAL "LIBSOCKET_LIB-NOTFOUND")

if(NOT LIBNSL_LIB STREQUAL "LIBNSL_LIB-NOTFOUND")
  set(LIBSOCKET_LIBRARIES ${LIBSOCKET_LIBRARIES} ${LIBNSL_LIB})
endif(NOT LIBNSL_LIB STREQUAL "LIBNSL_LIB-NOTFOUND")

if(NOT LIBWS2_LIB STREQUAL "LIBWS2_LIB-NOTFOUND")
  set(LIBSOCKET_LIBRARIES ${LIBWS2_LIB})
endif(NOT LIBWS2_LIB STREQUAL "LIBWS2_LIB-NOTFOUND")

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(LIBSOCKET DEFAULT_MSG LIBSOCKET_LIBRARIES)

mark_as_advanced(LIBRESOLV_LIB LIBSOCKET_LIB LIBNSL_LIB LIBWS2_LIB)
