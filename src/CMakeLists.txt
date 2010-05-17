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

# MSVC support files
if(WIN32 AND NOT CYGWIN)
  set(REG_LIB_SRCS ${REG_LIB_SRCS} win32/dirent.c win32/snprintf.c)
endif(WIN32 AND NOT CYGWIN)

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
      "${PROJECT_BINARY_DIR}/ReG_Steer_f_macros.m4"
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

# add the XML schema to the build if required
if(REG_VALIDATE_XML)
  find_package(XXD REQUIRED)

  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/reg_steer_comm.c
    COMMAND ${XXD}
    ARGS -i reg_steer_comm.xsd ${CMAKE_CURRENT_BINARY_DIR}/reg_steer_comm.c
    DEPENDS ${PROJECT_SOURCE_DIR}/xml_schema/reg_steer_comm.xsd
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/xml_schema
    COMMENT "Compiling XML schema"
  )

  set(REG_LIB_SRCS ${REG_LIB_SRCS} ${CMAKE_CURRENT_BINARY_DIR}/reg_steer_comm.c)
endif(REG_VALIDATE_XML)

# Add the dynamic loader code if required
if(REG_DYNAMIC_MOD_LOADING)
  set(REG_LIB_SRCS ${REG_LIB_SRCS} ReG_Steer_Dynamic_Loader.c)
endif(REG_DYNAMIC_MOD_LOADING)

# build the modules and add the common code to the main lib
# or add the selected module code to the main lib if monolithic
if(REG_BUILD_MODULAR_LIBS)
  foreach(target ${REG_MODULES})
    set(TARGET "ReG_Steer_${target}")
    add_library(${TARGET} ${REG_MOD_TYPE} ${${target}_SRCS})
    list(APPEND ReG_Steer_INSTALL_TARGETS ${TARGET})

    # no lib prefix needed for dynamic loading
    if(REG_DYNAMIC_MOD_LOADING)
      set_target_properties(${TARGET} PROPERTIES PREFIX "")
      target_link_libraries(${TARGET} ReG_Steer)
    endif(REG_DYNAMIC_MOD_LOADING)
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
# but not for Cygwin or MSVC
if(NOT REG_BUILD_SHARED_LIBS AND NOT WIN32)
  set_source_files_properties(
    ${REG_LIB_SRCS}
    PROPERTIES COMPILE_FLAGS -fPIC
  )
endif(NOT REG_BUILD_SHARED_LIBS AND NOT WIN32)

add_library(ReG_Steer ${REG_LIB_TYPE} ${REG_LIB_SRCS})
list(APPEND ReG_Steer_INSTALL_TARGETS ReG_Steer)

# link to external libs
target_link_libraries(ReG_Steer ${REG_EXTERNAL_LIBS})

# set shared library version numbers
if(REG_DYNAMIC_MOD_LOADING)
  set_target_properties(ReG_Steer
      PROPERTIES ${REG_LIBRARY_VERSION}
    )
else(REG_DYNAMIC_MOD_LOADING)
  if(REG_BUILD_SHARED_LIBS)
    set_target_properties(${ReG_Steer_INSTALL_TARGETS}
      PROPERTIES ${REG_LIBRARY_VERSION}
    )
  endif(REG_BUILD_SHARED_LIBS)
endif(REG_DYNAMIC_MOD_LOADING)

#
# Any other custom build steps needed?
#
include(build/all)

#
# install libs and modules
#

install(TARGETS ${ReG_Steer_INSTALL_TARGETS}
  ARCHIVE DESTINATION lib/RealityGrid
  LIBRARY DESTINATION lib/RealityGrid
)
