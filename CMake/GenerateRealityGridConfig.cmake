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

# Generate the RealityGridConfig.cmake file in the build tree.
# Also configure one for installation. The file tells external
# projects how to use RealityGrid.

#
# build tree config
#

# install type
set(RealityGrid_INSTALL_TYPE_CONFIG "build")

# install prefix
set(RealityGrid_INSTALL_PREFIX_CONFIG ${PROJECT_BINARY_DIR})

# header, library and documentation locations
set(RealityGrid_INCLUDE_DIRS_CONFIG ${PROJECT_SOURCE_DIR}/include)
set(RealityGrid_EXT_INCLUDE_DIRS_CONFIG
  ${ZLIB_INCLUDE_DIR}
  ${LIBXML2_INCLUDE_DIR}
)
set(RealityGrid_LIBRARY_DIRS_CONFIG ${PROJECT_BINARY_DIR}/lib)
set(RealityGrid_BIN_DIR_CONFIG ${PROJECT_BINARY_DIR}/bin)
set(RealityGrid_DOCS_DIR_CONFIG ${PROJECT_BINARY_DIR})

# build setting, library dependencies, use file and modules list locations
set(RealityGrid_BUILD_SETTINGS_FILE_CONFIG
  ${PROJECT_BINARY_DIR}/RealityGridBuildSettings.cmake
)

set(RealityGrid_USE_FILE_CONFIG
  ${PROJECT_BINARY_DIR}/UseRealityGrid.cmake
)

set(RealityGrid_DEPENDS_FILE_CONFIG
  ${PROJECT_BINARY_DIR}/RealityGridLibraryDepends.cmake
)

set(RealityGrid_MACROS_FILE_CONFIG
  ${PROJECT_BINARY_DIR}/RealityGridMacros.cmake
)

set(RealityGrid_MODULES_LIST_CONFIG
  ${PROJECT_BINARY_DIR}/RealityGridModulesList.cmake
)

# Configure RealityGridConfig.cmake for the build tree.
configure_file(
  ${PROJECT_SOURCE_DIR}/RealityGridConfig.cmake.in
  ${PROJECT_BINARY_DIR}/RealityGridConfig.cmake
  @ONLY IMMEDIATE
)

# set up config shell script for the build tree.
configure_file(
  ${PROJECT_SOURCE_DIR}/reg-config.in
  ${PROJECT_BINARY_DIR}/bin/reg-config
  @ONLY IMMEDIATE
)

#
# install tree config
#

# install type
set(RealityGrid_INSTALL_TYPE_CONFIG "install")

# install prefix
set(RealityGrid_INSTALL_PREFIX_CONFIG ${CMAKE_INSTALL_PREFIX})

# header, library and documentation locations
set(RealityGrid_INCLUDE_DIRS_CONFIG ${CMAKE_INSTALL_PREFIX}/include/RealityGrid)
set(RealityGrid_EXT_INCLUDE_DIRS_CONFIG
  ${ZLIB_INCLUDE_DIR}
  ${LIBXML2_INCLUDE_DIR}
)
set(RealityGrid_LIBRARY_DIRS_CONFIG ${CMAKE_INSTALL_PREFIX}/lib/RealityGrid)
set(RealityGrid_BIN_DIR_CONFIG ${CMAKE_INSTALL_PREFIX}/bin)
set(RealityGrid_DOCS_DIR_CONFIG ${CMAKE_INSTALL_PREFIX}/share/doc/RealityGrid)

# build setting, library dependencies, use file and modules list locations
set(RealityGrid_BUILD_SETTINGS_FILE_CONFIG
  ${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/RealityGridBuildSettings.cmake
)

set(RealityGrid_USE_FILE_CONFIG
  ${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/UseRealityGrid.cmake
)

set(RealityGrid_DEPENDS_FILE_CONFIG
  ${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/RealityGridLibraryDepends.cmake
)

set(RealityGrid_MACROS_FILE_CONFIG
  ${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/RealityGridMacros.cmake
)

set(RealityGrid_MODULES_LIST_CONFIG
  ${CMAKE_INSTALL_PREFIX}/lib/RealityGrid/RealityGridModulesList.cmake
)

# Configure RealityGridConfig.cmake for the install tree.
configure_file(
  ${PROJECT_SOURCE_DIR}/RealityGridConfig.cmake.in
  ${PROJECT_BINARY_DIR}/CMakeFiles/RealityGridConfig.cmake
  @ONLY IMMEDIATE
)

# set up config shell script for the install tree.
configure_file(
  ${PROJECT_SOURCE_DIR}/reg-config.in
  ${PROJECT_BINARY_DIR}/CMakeFiles/reg-config
  @ONLY IMMEDIATE
)
