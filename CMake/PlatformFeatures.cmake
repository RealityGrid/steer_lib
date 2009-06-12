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

include(CheckIncludeFiles)
include(CheckSymbolExists)
include(CheckFunctionExists)
include(CheckLibraryExists)

# check where malloc and friends are defined
CHECK_SYMBOL_EXISTS(malloc stdlib.h MALLOC_IN_STDLIB)
if(NOT MALLOC_IN_STDLIB)
  CHECK_INCLUDE_FILES(malloc.h REG_NEED_MALLOC_H)
endif(NOT MALLOC_IN_STDLIB)

#
# Go through the various "types" of module
# and do any specific checks for them.
#
# Files should be named "PlatformFeaturesService.cmake"
# where Service is what the module provides, eg Sockets, Files
#
if(REG_BUILD_MODULAR_LIBS)
  foreach(service ${REG_MODULES_PROVIDES})
    if(EXISTS ${CMAKE_SOURCE_DIR}/CMake/PlatformFeatures${service}.cmake)
      include(PlatformFeatures${service})
    endif(EXISTS ${CMAKE_SOURCE_DIR}/CMake/PlatformFeatures${service}.cmake)
  endforeach(service ${REG_MODULES_PROVIDES})
else(REG_BUILD_MODULAR_LIBS)
  foreach(type ${REG_MODULES_TYPES})
    set(default_mod ${REG_USE_MODULE_${type}})
    if(EXISTS ${CMAKE_SOURCE_DIR}/CMake/PlatformFeatures${default_mod}.cmake)
      include(PlatformFeatures${default_mod})
    endif(EXISTS ${CMAKE_SOURCE_DIR}/CMake/PlatformFeatures${default_mod}.cmake)
  endforeach(type ${REG_MODULES_TYPES})
endif(REG_BUILD_MODULAR_LIBS)
