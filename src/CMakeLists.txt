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

# library sources
set(REG_LIB_SRCS
  Base64.c
  ReG_Steer_Appside.c
  ReG_Steer_Steerside.c
  ReG_Steer_Common.c
  ReG_Steer_XML.c
  ReG_Steer_Logging.c
  ReG_Steer_Browser.c
)

# header files and definitions
include_directories(${PROJECT_SOURCE_DIR}/include)

#
# Build the Fortran bits and include them in the main library
#
if(REG_BUILD_FORTRAN_WRAPPERS)
  # Fortran source files
  set(REG_LIB_FORTRAN_SRCS
    ${CMAKE_CURRENT_BINARY_DIR}/ReG_Steer_Appside_f.c
    ReG_Steering_Initialize.f90
    reg_steer_mod.f90
  )

  # Configure m4 macros
  if(DEFINED FC_NAME_MANGLE)
    configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/ReG_Steer_f_macros.m4.in"
      "ReG_Steer_f_macros.m4"
      @ONLY
    )
  endif(DEFINED FC_NAME_MANGLE)

  # Build ReG_Steer_Appside_f.c from
  # ReG_Steer_f_macros.m4 and ReG_Steer_Appside_f.m4
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ReG_Steer_Appside_f.c
    COMMAND ${M4}
    ARGS ${PROJECT_BINARY_DIR}/ReG_Steer_f_macros.m4 ${CMAKE_CURRENT_SOURCE_DIR}/ReG_Steer_Appside_f.m4 > ${CMAKE_CURRENT_BINARY_DIR}/ReG_Steer_Appside_f.c
    COMMENT "Generating Fortran language bindings"
  )

  # Add the Fortran sources to the main library build
  set(REG_LIB_SRCS ${REG_LIB_SRCS} ${REG_LIB_FORTRAN_SRCS})

  # offer to build the Fortran type investigation utilities
  option(REG_BUILD_FORTRAN_TYPE_UTILS "Build two utility programs to investigate how Fortran and C types map to each other on this hardware. These are for debug purposes only and are not installed with the rest of the example applications." OFF)
  mark_as_advanced(REG_BUILD_FORTRAN_TYPE_UTILS)
  if(REG_BUILD_FORTRAN_TYPE_UTILS)
    add_subdirectory(fortran_type_utils)
  endif(REG_BUILD_FORTRAN_TYPE_UTILS)
endif(REG_BUILD_FORTRAN_WRAPPERS)

# build the modules and add the common code to the main lib
# or add the selected module code to the main lib if monolithic
if(REG_BUILD_MODULAR_LIBS)
  foreach(target ${REG_MODULES})
    add_library(ReG_Steer_${target} ${REG_MOD_TYPE} ${${target}_SRCS})
    list(APPEND ReG_Steer_INSTALL_TARGETS ReG_Steer_${target})
  endforeach(target ${REG_MODULES})
  set(REG_LIB_SRCS ${REG_LIB_SRCS} ${REG_MODULE_COMMON_SRCS})
else(REG_BUILD_MODULAR_LIBS)
  foreach(type ${REG_MODULES_TYPES})
    set(REG_LIB_SRCS ${REG_LIB_SRCS} ${SRCS_${type}_${REG_USE_MODULE_${type}}})
    list(REMOVE_DUPLICATES REG_LIB_SRCS)
  endforeach(type ${REG_MODULES_TYPES})
endif(REG_BUILD_MODULAR_LIBS)

# need to make sure everything is built with PIC in case
# the static libs are to be embedded in a shared object
if(NOT REG_BUILD_SHARED_LIBS)
  set_source_files_properties(
    ${REG_LIB_SRCS}
    PROPERTIES COMPILE_FLAGS -fPIC
  )
endif(NOT REG_BUILD_SHARED_LIBS)

add_library(ReG_Steer ${REG_LIB_TYPE} ${REG_LIB_SRCS})
list(APPEND ReG_Steer_INSTALL_TARGETS ReG_Steer)

# link to external libs
target_link_libraries(ReG_Steer ${REG_EXTERNAL_LIBS})

# set shared library version numbers
if(REG_BUILD_SHARED_LIBS)
  set_target_properties(${ReG_Steer_INSTALL_TARGETS}
    PROPERTIES ${REG_LIBRARY_VERSION}
  )
endif(REG_BUILD_SHARED_LIBS)

#
# Any other custom build steps needed? 
#
include(build/All)

#
# install libs and modules
#

install(TARGETS ${ReG_Steer_INSTALL_TARGETS}
  ARCHIVE DESTINATION lib/RealityGrid
  LIBRARY DESTINATION lib/RealityGrid
)
