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

# Java compile stuff we have overridden to compensate for CMake bugs
set(CMAKE_USER_MAKE_RULES_OVERRIDE "JavaCompileOverride")

enable_language(Java)
mark_as_advanced(CMAKE_Java_ARCHIVE)
mark_as_advanced(CMAKE_Java_RUNTIME)

set(IOPROXY_SRCS
  HybridSwitch.java
  HybridThread.java
)

# build library
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_library(ReG_Steer_ioProxy ${IOPROXY_SRCS})

# update manifest so jar file can be run
add_custom_command(TARGET ReG_Steer_ioProxy POST_BUILD
  COMMAND jar ARGS -umf ${CMAKE_CURRENT_SOURCE_DIR}/manifest.txt ${CMAKE_BINARY_DIR}/lib/ReG_Steer_ioProxy.jar
  COMMENT "Updating ReG_Steer_ioProxy.jar manifest"
)

# build runnable script in two forms
set(REG_PROXY_DEFAULT_PORT 65000 CACHE STRING "The port that the proxy should listen on if none is provided on the command line. Default 65000.")
mark_as_advanced(REG_PROXY_DEFAULT_PORT)

# build tree version
set(RealityGrid_INSTALL_PREFIX_CONFIG ${PROJECT_BINARY_DIR}/lib)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/reg-proxy.in
  ${PROJECT_BINARY_DIR}/bin/reg-proxy
  @ONLY IMMEDIATE
)

# install tree version
set(RealityGrid_INSTALL_PREFIX_CONFIG ${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/java)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/reg-proxy.in
  ${PROJECT_BINARY_DIR}/CMakeFiles/reg-proxy
  @ONLY IMMEDIATE
)

#
# install
#

install(TARGETS ReG_Steer_ioProxy ARCHIVE DESTINATION lib/RealityGrid/java)
install(PROGRAMS ${PROJECT_BINARY_DIR}/CMakeFiles/reg-proxy DESTINATION bin)
