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

# Generate the RealityGridConfig.cmake file.
# This file tells external projects which modules are available
# for this build of the RealityGrid library.

# first gather a list of built-in modules if were a monolithic build
if(NOT REG_BUILD_MODULAR_LIBS)
  foreach(type ${REG_MODULES_TYPES})
    list(APPEND
      RealityGrid_MODULES_LIST_CONFIG
      ${type}_${REG_USE_MODULE_${type}}
    )
    set(REG_MODULES ${RealityGrid_MODULES_LIST_CONFIG})
  endforeach(type ${REG_MODULES_TYPES})
endif(NOT REG_BUILD_MODULAR_LIBS)

# write file header
file(WRITE ${PROJECT_BINARY_DIR}/RealityGridModulesList.cmake
  "#\n# RealityGrid Modules list\n#\n"
  "# This is a generated file, do not edit!\n#\n\n"
)

# populate rest of file with a line for each module
foreach(type ${REG_MODULES_TYPES})
  foreach(mod ${REG_MODULES_${type}})
    if(REG_BUILD_MODULAR_LIBS)
      file(APPEND ${PROJECT_BINARY_DIR}/RealityGridModulesList.cmake
	"set(REG_MODULE_${type}_${mod} ReG_Steer_${type}_${mod})\n"
      )
    else(REG_BUILD_MODULAR_LIBS)
      list(FIND REG_MODULES "${type}_${mod}" found)
      if(${found} EQUAL -1)
	set(found OFF)
      else(${found} EQUAL -1)
	set(found ON)
      endif(${found} EQUAL -1)
      file(APPEND ${PROJECT_BINARY_DIR}/RealityGridModulesList.cmake
	"set(REG_MODULE_${type}_${mod} ${found})\n"
      )
    endif(REG_BUILD_MODULAR_LIBS)
  endforeach(mod ${REG_MODULES_${type}})
endforeach(type ${REG_MODULES_TYPES})
