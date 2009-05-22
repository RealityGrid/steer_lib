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

# need to be built PIC if going to be added to shared objects
if(NOT REG_BUILD_SHARED_LIBS)
  set_source_files_properties(
    ${rm_srcs}
    PROPERTIES COMPILE_FLAGS -fPIC
  )
endif(NOT REG_BUILD_SHARED_LIBS)

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
  if(NOT ${rm_common} STREQUAL "")
    list(APPEND REG_MODULE_COMMON_SRCS ${rm_common})
    list(REMOVE_DUPLICATES REG_MODULE_COMMON_SRCS)
  endif(NOT ${rm_common} STREQUAL "")

  # create lists of source files to build into modules
  set(rm_target "ReG_Steer_${rm_type}_${rm_provides}")
  set(REG_MODULES ${REG_MODULES} ${rm_target})
  set(${rm_target}_SRCS ${rm_srcs})
else(REG_BUILD_MODULAR_LIBS)
  # keep track of each modules individual source files
  set(SRCS_${rm_type}_${rm_provides} ${rm_srcs} ${rm_common})
endif(REG_BUILD_MODULAR_LIBS)

endmacro(register_module)
