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

project(REG)

# cmake setup boiler-plate
cmake_minimum_required(VERSION 2.6)
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")

# version numbers
set(REG_LIB_VERSION_MAJ 3)
set(REG_LIB_VERSION_MIN 5)
set(REG_LIB_VERSION_PAT 0)
set(REG_STEER_LIB_VERSION
    "${REG_LIB_VERSION_MAJ}.${REG_LIB_VERSION_MIN}.${REG_LIB_VERSION_PAT}")

# shared library version numbers
set(REG_LIBRARY_VERSION
  VERSION "${REG_STEER_LIB_VERSION}"
  SOVERSION "${REG_LIB_VERSION_MAJ}"
)

# set output directories.
if(NOT LIBRARY_OUTPUT_PATH)
  set(LIBRARY_OUTPUT_PATH ${REG_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
endif(NOT LIBRARY_OUTPUT_PATH)
if(NOT EXECUTABLE_OUTPUT_PATH)
  set(EXECUTABLE_OUTPUT_PATH ${REG_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
endif(NOT EXECUTABLE_OUTPUT_PATH)

#
# build shared or static libs
#
set(REG_LIB_TYPE STATIC)
if(UNIX)
  option(REG_BUILD_SHARED_LIBS "Turn on to build shared libraries. Build static libraries by default." OFF)
  if(REG_BUILD_SHARED_LIBS)
    set(REG_LIB_TYPE SHARED)
  endif(REG_BUILD_SHARED_LIBS)
endif(UNIX)

#
# build a monolithic or modular library
#
option(REG_BUILD_MODULAR_LIBS "Turn on to build a set of modular libs rather than one monolithic one. Monolithic by default." OFF)

# set the type of modules to build
if(REG_BUILD_MODULAR_LIBS)
  set(REG_MOD_TYPE STATIC)
  if(REG_BUILD_SHARED_LIBS)
    set(REG_MOD_TYPE SHARED)
    option(REG_DYNAMIC_MOD_LOADING "Turn on to build dynamically loadable modules that are linked at run time instead of build time. OFF by default." OFF)
    if(REG_DYNAMIC_MOD_LOADING)
      set(REG_MOD_TYPE MODULE)
    endif(REG_DYNAMIC_MOD_LOADING)
  else(REG_BUILD_SHARED_LIBS)
    set(REG_DYNAMIC_MOD_LOADING OFF)
  endif(REG_BUILD_SHARED_LIBS)
endif(REG_BUILD_MODULAR_LIBS)

# set up modules
include(ModulesMacro)
include(Modules)

# do dependency checks
include(deps/all)

# do platform compatibility checks
include(platform/all)

# set up configure header file
configure_file(
  "${PROJECT_SOURCE_DIR}/ReG_Steer_Config.h.in"
  "${PROJECT_BINARY_DIR}/ReG_Steer_Config.h"
  @ONLY
)

# add binary tree so that we can find ReG_Steer_Config.h
include_directories("${PROJECT_BINARY_DIR}")

#
# configure options
#
option(REG_LOG_STEERING "Whether or not to log steering commands. Default is ON." ON)
set(REG_SCRATCH_DEFAULT "./" CACHE PATH "Directory for library to use as general scratch space. Default is the current directory (./).")
option(REG_VALIDATE_XML "Should the library validate every XML message it receives? Default is OFF" OFF)
mark_as_advanced(REG_LOG_STEERING)
mark_as_advanced(REG_SCRATCH_DEFAULT)
mark_as_advanced(REG_VALIDATE_XML)

#
# library debug options
#
option(REG_DEBUG "Enable debugging output from the library." OFF)
if(REG_DEBUG)
  option(REG_DEBUG_FULL "Enable extended debugging output." OFF)
  mark_as_advanced(REG_DEBUG_FULL)
else(REG_DEBUG)
  if(REG_DEBUG_FULL)
    set(REG_DEBUG_FULL OFF CACHE BOOL "Enable extended debugging output." FORCE)
  endif(REG_DEBUG_FULL)
endif(REG_DEBUG)
option(REG_USE_TIMING "Enable internal timing routines - not very portable!" OFF)
mark_as_advanced(REG_USE_TIMING)

#
# build Fortran wrappers?
#
option(REG_BUILD_FORTRAN_WRAPPERS "Build FORTRAN wrappers for the Steering Library code." OFF)
if(REG_BUILD_FORTRAN_WRAPPERS)
  # this will fail if Fortran is not installed
  enable_language(Fortran)
  find_package(M4 REQUIRED)
  include(fortran-c-abi/Fortran-C-ABI)

  # if Fortran is available we can build the MPI example
  find_package(MPIF)
  if(MPIF_FOUND)
    option(REG_BUILD_MPI_EXAMPLE "Build Fortran MPI example application." ON)
  endif(MPIF_FOUND)
endif(REG_BUILD_FORTRAN_WRAPPERS)

#
# find the required external libraries and
# keep a track of them to help with configuring
# other projects that link against this one
#
if(UNIX)
  find_library(LIBM_LIB m)
  mark_as_advanced(LIBM_LIB)
  set(REG_EXTERNAL_LIBS ${REG_EXTERNAL_LIBS} ${LIBM_LIB})
endif(UNIX)
find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIR})
set(REG_EXTERNAL_LIBS ${ZLIB_LIBRARY} ${REG_EXTERNAL_LIBS})

find_package(LibXml2 REQUIRED)
include_directories("${LIBXML2_INCLUDE_DIR}")
set(REG_EXTERNAL_LIBS ${LIBXML2_LIBRARIES} ${REG_EXTERNAL_LIBS})

if(REG_DYNAMIC_MOD_LOADING)
  find_library(LIBDL_LIB dl)
  mark_as_advanced(LIBDL_LIB)
  set(REG_EXTERNAL_LIBS ${REG_EXTERNAL_LIBS} ${LIBDL_LIB})
endif(REG_DYNAMIC_MOD_LOADING)

# ReG libs to link examples against
set(REG_LINK_LIBRARIES ReG_Steer)

# build and install library, headers and support files
add_subdirectory(src)
add_subdirectory(include)
add_subdirectory(xml_schema)

# build examples?
option(REG_BUILD_EXAMPLES "Build the example applications?" ON)
if(REG_BUILD_EXAMPLES)
  add_subdirectory(examples)
endif(REG_BUILD_EXAMPLES)

# build documentation?
option(REG_BUILD_DOCUMENTATION "Build the Doxygen documentation?" OFF)
if(REG_BUILD_DOCUMENTATION)
  add_subdirectory(doc)
endif(REG_BUILD_DOCUMENTATION)

# build cmake packaging config and build settings
# for use by FIND_PACKAGE files and so on
include(CMakeExportBuildSettings)
cmake_export_build_settings(
  ${REG_BINARY_DIR}/RealityGridBuildSettings.cmake
)
export_library_dependencies(
  ${REG_BINARY_DIR}/RealityGridLibraryDepends.cmake
)

configure_file(
  ${PROJECT_SOURCE_DIR}/UseRealityGrid.cmake.in
  ${PROJECT_BINARY_DIR}/UseRealityGrid.cmake
  COPYONLY IMMEDIATE
)

configure_file(
  ${PROJECT_SOURCE_DIR}/RealityGridMacros.cmake.in
  ${PROJECT_BINARY_DIR}/RealityGridMacros.cmake
  @ONLY
)

# create module list file
include(GenerateRealityGridModuleList)

# create build- and install-tree packaging info
include(GenerateRealityGridConfig)

# install packaging config files
install(PROGRAMS ${PROJECT_BINARY_DIR}/CMakeFiles/reg-config DESTINATION bin)
install(FILES
  "${PROJECT_BINARY_DIR}/UseRealityGrid.cmake"
  "${PROJECT_BINARY_DIR}/CMakeFiles/RealityGridConfig.cmake"
  "${PROJECT_BINARY_DIR}/RealityGridBuildSettings.cmake"
  "${PROJECT_BINARY_DIR}/RealityGridLibraryDepends.cmake"
  "${PROJECT_BINARY_DIR}/RealityGridMacros.cmake"
  "${PROJECT_BINARY_DIR}/RealityGridModulesList.cmake"
  DESTINATION lib/RealityGrid
)
