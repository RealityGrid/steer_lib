#
#  Common CMake build rules for the RealityGrid example MPI applications.
#
#  (C) Copyright 2008, University of Manchester, United Kingdom,
#  all rights reserved.
#
#  This software was developed by the RealityGrid project
#  (http://www.realitygrid.org), funded by the EPSRC under grants
#  GR/R67699/01 and GR/R67699/02.
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
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
#  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
#  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
#  CORRECTION.
#
#  Author.........: Robert Haines
#
#------------------------------------------------------------------------

include_directories(${PROJECT_SOURCE_DIR}/include ${MPI_INCLUDE_PATH})
string(REGEX MATCH "^[^\\.]+" EX_APP_NAME ${EX_SRC_NAME})
add_executable(${EX_APP_NAME} ${EX_SRC_NAME})

# Set MPI flags for the source file and target while building
if(${MPI_COMPILE_FLAGS})
  set_source_files_properties(
    ${EX_SRC_NAME} PROPERTIES COMPILE_FLAGS ${MPI_COMPILE_FLAGS}
  )
endif(${MPI_COMPILE_FLAGS})
if(${MPI_LINK_FLAGS})
  set_target_properties(
    ${EX_APP_NAME} PROPERTIES LINK_FLAGS ${MPI_LINK_FLAGS}
  )
endif(${MPI_LINK_FLAGS})

# if building static libraries the link order needs
# to be repeated to resolve circular dependencies
set(EXAMPLES_LINK_LINE ${REG_EXAMPLES_MODULES_LINK} ${REG_LINK_LIBRARIES})
if(NOT REG_BUILD_SHARED_LIBS)
  set(EXAMPLES_LINK_LINE ${EXAMPLES_LINK_LINE} ${EXAMPLES_LINK_LINE})
endif(NOT REG_BUILD_SHARED_LIBS)

target_link_libraries(
  ${EX_APP_NAME}
  ${MPI_LIBRARY}
  ${MPI_EXTRA_LIBRARY}
  ${EXAMPLES_LINK_LINE}
  ${REG_EXTERNAL_LIBS}
)

#
# install
#

install(TARGETS ${EX_APP_NAME} RUNTIME DESTINATION bin)
