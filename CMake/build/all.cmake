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

# need to make sure everything is built with PIC in case
# the static libs are to be embedded in a shared object
# but not for Cygwin or MSVC
if(NOT REG_BUILD_SHARED_LIBS AND NOT WIN32)
  foreach(type ${REG_MODULES_TYPES})
    foreach(service ${REG_MODULES_PROVIDES})
      if(REG_BUILD_MODULAR_LIBS)
	set_source_files_properties(
	  ${${type}_${service}_SRCS}
	  ${REG_MODULE_COMMON_SRCS}
	  PROPERTIES COMPILE_FLAGS -fPIC
	)
      else(REG_BUILD_MODULAR_LIBS)
	set_source_files_properties(
	  ${SRCS_${type}_${service}}
	  PROPERTIES COMPILE_FLAGS -fPIC
	)
      endif(REG_BUILD_MODULAR_LIBS)
    endforeach(service ${REG_MODULES_PROVIDES})
  endforeach(type ${REG_MODULES_TYPES})
endif(NOT REG_BUILD_SHARED_LIBS AND NOT WIN32)

#
# Go through the various "types" of module
# and do any extra specific build steps for them.
#
# Files should be in the build directory and named "Service.cmake"
# where Service is what the module provides, eg Sockets, Files
#
if(REG_BUILD_MODULAR_LIBS)
  foreach(service ${REG_MODULES_PROVIDES})
    if(EXISTS ${CMAKE_SOURCE_DIR}/CMake/build/${service}.cmake)
      include(build/${service})
    endif(EXISTS ${CMAKE_SOURCE_DIR}/CMake/build/${service}.cmake)
  endforeach(service ${REG_MODULES_PROVIDES})
else(REG_BUILD_MODULAR_LIBS)
  foreach(type ${REG_MODULES_TYPES})
    set(default_mod ${REG_USE_MODULE_${type}})
    if(EXISTS ${CMAKE_SOURCE_DIR}/CMake/build/${default_mod}.cmake)
      include(build/${default_mod})
    endif(EXISTS ${CMAKE_SOURCE_DIR}/CMake/build/${default_mod}.cmake)
  endforeach(type ${REG_MODULES_TYPES})
endif(REG_BUILD_MODULAR_LIBS)
