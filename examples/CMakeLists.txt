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

find_package(MPI)

if(REG_BUILD_MODULAR_LIBS)
  foreach(type ${REG_MODULES_TYPES})
    set(REG_EXAMPLES_MODULES_LINK ${REG_EXAMPLES_MODULES_LINK} "ReG_Steer_${type}_${REG_USE_MODULE_${type}}")
  endforeach(type ${REG_MODULES_TYPES})
endif(REG_BUILD_MODULAR_LIBS)

add_subdirectory(mini_app)
add_subdirectory(mini_steerer)
add_subdirectory(sink)
add_subdirectory(simple)

if(REG_BUILD_FORTRAN_WRAPPERS)
  add_subdirectory(mini_app_f90)
  if(MPI_FOUND)
    add_subdirectory(mini_app_f90_parallel)
  endif(MPI_FOUND)
endif(REG_BUILD_FORTRAN_WRAPPERS)
