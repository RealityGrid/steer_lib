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

# Generate the RealityGridConfig.cmake file in the build tree.
# Also configure one for installation. The file tells external
# projects how to use RealityGrid.

#
# build tree config
#

# install prefix
set(RealityGrid_INSTALL_PREFIX_CONFIG ${PROJECT_BINARY_DIR})

# header, library and documentation locations
set(RealityGrid_INCLUDE_DIRS_CONFIG
  ${PROJECT_BINARY_DIR}
  ${PROJECT_SOURCE_DIR}/include
)
set(RealityGrid_LIBRARY_DIRS_CONFIG ${PROJECT_BINARY_DIR}/lib)
set(RealityGrid_DOCS_DIR_CONFIG ${PROJECT_BINARY_DIR})

# build setting, library dependencies and use file locations
set(RealityGrid_BUILD_SETTINGS_FILE_CONFIG
  ${PROJECT_BINARY_DIR}/RealityGridBuildSettings.cmake
)

set(RealityGrid_USE_FILE_CONFIG
  ${PROJECT_BINARY_DIR}/UseRealityGrid.cmake
)

set(RealityGrid_DEPENDS_FILE_CONFIG
  ${PROJECT_BINARY_DIR}/RealityGridLibraryDepends.cmake
)

# Configure RealityGridConfig.cmake for the build tree.
configure_file(
  ${PROJECT_SOURCE_DIR}/RealityGridConfig.cmake.in
  ${PROJECT_BINARY_DIR}/RealityGridConfig.cmake
  @ONLY IMMEDIATE
)

# set up config shell script for the build tree.
configure_file(
  "${PROJECT_SOURCE_DIR}/reg-config.in"
  "${PROJECT_BINARY_DIR}/bin/reg-config"
  @ONLY IMMEDIATE
)

#
# install tree config
#

# install prefix
set(RealityGrid_INSTALL_PREFIX_CONFIG ${CMAKE_INSTALL_PREFIX})

# header, library and documentation locations
set(RealityGrid_INCLUDE_DIRS_CONFIG "${CMAKE_INSTALL_PREFIX}/include/RealityGrid")
set(RealityGrid_LIBRARY_DIRS_CONFIG "${CMAKE_INSTALL_PREFIX}/lib/RealityGrid")
set(RealityGrid_DOCS_DIR_CONFIG "${CMAKE_INSTALL_PREFIX}/share/doc/RealityGrid")

# build setting, library dependencies and use file locations
set(RealityGrid_BUILD_SETTINGS_FILE_CONFIG
  "${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/RealityGridBuildSettings.cmake"
)

set(RealityGrid_USE_FILE_CONFIG
  "${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/UseRealityGrid.cmake"
)

set(RealityGrid_DEPENDS_FILE_CONFIG 
  "${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/RealityGridLibraryDepends.cmake"
)

# Configure RealityGridConfig.cmake for the install tree.
configure_file(
  ${PROJECT_SOURCE_DIR}/RealityGridConfig.cmake.in
  ${PROJECT_BINARY_DIR}/CMakeFiles/RealityGridConfig.cmake
  @ONLY IMMEDIATE
)

# set up config shell script for the install tree.
configure_file(
  "${PROJECT_SOURCE_DIR}/reg-config.in"
  "${PROJECT_BINARY_DIR}/CMakeFiles/reg-config"
  @ONLY IMMEDIATE
)
