#
#  The RealityGrid Steering Library
#
#  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
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

#
# register_module macro
#
# parameters are:
# rm_type     - the type of module (Samples, Steering, etc)
# rm_provides - what is provided (Sockets, Files, etc)
# rm_srcs     - source files unique to this module
# rm_common   - source files common to other modules
#
macro(register_module rm_type rm_provides rm_srcs rm_common)

# add to the list of what modules are providing if unique, error otherwise
list(FIND REG_MODULES_${rm_type} ${rm_provides} found)
if(${found} EQUAL -1)
  list(APPEND REG_MODULES_${rm_type} ${rm_provides})
else(${found} EQUAL -1)
  message(FATAL_ERROR "A module is already registered that provides ${rm_provides}! Please edit CMake/Modules.cmake and remove the duplicate entry.")
endif(${found} EQUAL -1)

# add to the lists of module types and remove duplicates
list(APPEND REG_MODULES_TYPES ${rm_type})
list(APPEND REG_MODULES_PROVIDES ${rm_provides})
list(REMOVE_DUPLICATES REG_MODULES_TYPES)
list(REMOVE_DUPLICATES REG_MODULES_PROVIDES)

if(REG_BUILD_MODULAR_LIBS)
  # add common files to the main build and remove duplicates
  if(NOT "${rm_common}" STREQUAL "")
    list(APPEND REG_MODULE_COMMON_SRCS ${rm_common})
    list(REMOVE_DUPLICATES REG_MODULE_COMMON_SRCS)
  endif(NOT "${rm_common}" STREQUAL "")

  # create lists of source files to build into modules
  set(rm_target "${rm_type}_${rm_provides}")
  list(APPEND REG_MODULES ${rm_target})
  set(${rm_target}_SRCS ${rm_srcs})
else(REG_BUILD_MODULAR_LIBS)
  # keep track of each modules individual source files
  set(SRCS_${rm_type}_${rm_provides} ${rm_srcs} ${rm_common})
endif(REG_BUILD_MODULAR_LIBS)

endmacro(register_module)
