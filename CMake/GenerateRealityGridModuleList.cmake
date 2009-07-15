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
