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

project(REG)

# cmake setup boiler-plate
cmake_minimum_required(VERSION 2.6)
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")

# version numbers
set(REG_LIB_VERSION_MAJ 2)
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

# set up modules
include(ModulesMacro)
include(Modules)

# do platform checks
include(PlatformFeatures)

# set up configure header file
configure_file(
  "${PROJECT_SOURCE_DIR}/ReG_Steer_Config.h.in"
  "${PROJECT_BINARY_DIR}/ReG_Steer_Config.h"
  @ONLY
)

# add binary tree so that we can find ReG_Steer_Config.h
include_directories("${PROJECT_BINARY_DIR}")

#
# build shared or static libs
#
set(REG_LIB_TYPE STATIC)
option(REG_BUILD_SHARED_LIBS "Turn on to build shared libraries. Build static libraries by default." OFF)
if(REG_BUILD_SHARED_LIBS)
  set(REG_LIB_TYPE SHARED)
endif(REG_BUILD_SHARED_LIBS)

#
# build a monolithic or modular library
#
option(REG_BUILD_MODULAR_LIBS "Turn on to build a set of modular libs rather than one monolithic one. Modular by default." ON)

# set the type of modules to build
if(REG_BUILD_MODULAR_LIBS)
  set(REG_MOD_TYPE STATIC)
  if(REG_BUILD_SHARED_LIBS)
    # THE BELOW SHOULD BE CHANGED TO "MODULE" WHEN LD STUFF IS COMPLETE
    set(REG_MOD_TYPE SHARED)
  endif(REG_BUILD_SHARED_LIBS)
endif(REG_BUILD_MODULAR_LIBS)

#
# transport options
#
option(REG_SOCKET_STEERING "Use plain sockets for the transfer of steering messages. Default is ON." ON)
option(REG_SOAP_STEERING "Use SOAP for the transfer of steering messages. Default is OFF." OFF)

# quick hack to stop both socket and soap steering from being selected
if(REG_SOCKET_STEERING AND REG_SOAP_STEERING)
  message(SEND_ERROR "You can not use both Sockets and SOAP for your steering transport. Please switch one of them off!")
endif(REG_SOCKET_STEERING AND REG_SOAP_STEERING)

#
# feature options
#
option(REG_USE_OPENSSL "Use SSL for steering connections. Default is OFF." OFF)
option(REG_BUILD_SEPARATE_SOAP_LIB "The SOAP code can sometimes clash with other applications if linked in to the main library. Use this toggle to move it in or out of the main library. Default is to build a separate library." ON)
option(REG_USE_WSRF "If set, use WSRF otherwise use OGSI for the SOAP middleware. Default is WSRF." ON)
option(REG_LOG_STEERING "Whether or not to log steering commands. Default is ON." ON)
mark_as_advanced(REG_BUILD_SEPARATE_SOAP_LIB)
mark_as_advanced(REG_USE_WSRF)
mark_as_advanced(REG_LOG_STEERING)

#
# library debug options
#
option(REG_DEBUG "Enable debugging output from the library." OFF)
if(REG_DEBUG)
  option(REG_DEBUG_FULL "Enable extended debugging output." OFF)
  mark_as_advanced(REG_DEBUG_FULL)
endif(REG_DEBUG)
option(REG_USE_TIMING "Enable internal timing routines - not very portable!" OFF)
option(REG_KEEP_XML_MESSAGES "Keep xml messages for debugging purposes. Default is OFF." OFF)
mark_as_advanced(REG_USE_TIMING)
mark_as_advanced(REG_KEEP_XML_MESSAGES)

#
# build Fortran wrappers?
#
option(REG_BUILD_FORTRAN_WRAPPERS "Build FORTRAN wrappers for the Steering Library code." OFF)
if(REG_BUILD_FORTRAN_WRAPPERS)
  # this will fail if Fortran is not installed
  enable_language(Fortran)
  find_package(M4 REQUIRED)
  include(fortran-c-abi/Fortran-C-ABI)
endif(REG_BUILD_FORTRAN_WRAPPERS)

# keep a track of external libs used for configuring
# other projects that link against this one
set(REG_EXTERNAL_LIBS "-lz -lm")

#
# find libxml2
#
find_package(LibXml2 REQUIRED)
include_directories("${LIBXML2_INCLUDE_DIR}")
set(REG_EXTERNAL_LIBS "-lxml2 ${REG_EXTERNAL_LIBS}")

#
# find openssl
#
if(REG_USE_OPENSSL)
  find_package(OpenSSL REQUIRED)
  include_directories("${OPENSSL_INCLUDE_DIR}")
  set(REG_EXTERNAL_LIBS "-lssl ${REG_EXTERNAL_LIBS}")
endif(REG_USE_OPENSSL)

# libs to link examples against
set(REG_LINK_LIBRARIES ReG_Steer_Utils ReG_Steer)
if(REG_BUILD_SEPARATE_SOAP_LIB)
  set(REG_LINK_LIBRARIES ${REG_LINK_LIBRARIES} ReG_Steer_SOAP)
endif(REG_BUILD_SEPARATE_SOAP_LIB)

# build and install library and headers
add_subdirectory(src)
add_subdirectory(include)

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

# create build- and install-tree packaging info
include(GenerateRealityGridConfig)

# install packaging config files
install(PROGRAMS ${PROJECT_BINARY_DIR}/CMakeFiles/reg-config DESTINATION bin)
install(FILES
  "${PROJECT_BINARY_DIR}/UseRealityGrid.cmake"
  "${PROJECT_BINARY_DIR}/CMakeFiles/RealityGridConfig.cmake"
  "${PROJECT_BINARY_DIR}/RealityGridBuildSettings.cmake"
  "${PROJECT_BINARY_DIR}/RealityGridLibraryDepends.cmake"
  DESTINATION lib/RealityGrid
)
