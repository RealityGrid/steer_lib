#
#  The RealityGrid Steering Library
#
#  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
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

find_package(Doxygen REQUIRED)

# build HTML documentation
option(REG_BUILD_HTML_DOCS "Build the HTML documentation." ON)
mark_as_advanced(REG_BUILD_HTML_DOCS)
if(REG_BUILD_HTML_DOCS)
  set(REG_BUILD_HTML_DOCS YES)
else(REG_BUILD_HTML_DOCS)
  set(REG_BUILD_HTML_DOCS NO)
endif(REG_BUILD_HTML_DOCS)

# build LaTeX documentation
option(REG_BUILD_LATEX_DOCS "Build the LaTeX documentation." OFF)
mark_as_advanced(REG_BUILD_LATEX_DOCS)
if(REG_BUILD_LATEX_DOCS)
  set(REG_BUILD_LATEX_DOCS YES)
  find_package(LATEX REQUIRED)
else(REG_BUILD_LATEX_DOCS)
  set(REG_BUILD_LATEX_DOCS NO)
endif(REG_BUILD_LATEX_DOCS)

# use short names (for eg CD filesystems)
option(DOXYGEN_SHORT_NAMES
  "Use short file names for generated documentation." OFF)
mark_as_advanced(DOXYGEN_SHORT_NAMES)
if(DOXYGEN_SHORT_NAMES)
  set(DOXYGEN_SHORT_NAMES YES)
else(DOXYGEN_SHORT_NAMES)
  set(DOXYGEN_SHORT_NAMES NO)
endif(DOXYGEN_SHORT_NAMES)

# keep doxygen quiet
option(DOXYGEN_QUIET "Keep Doxygen quiet while it works." ON)
mark_as_advanced(DOXYGEN_QUIET)
if(DOXYGEN_QUIET)
  set(DOXYGEN_QUIET YES)
else(DOXYGEN_QUIET)
  set(DOXYGEN_QUIET NO)
endif(DOXYGEN_QUIET)

# configure the doxygen driver file
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/doxyfile.in"
  "${CMAKE_CURRENT_BINARY_DIR}/doxyfile"
)

# build it
add_custom_target(Documentation ALL
  ${DOXYGEN_EXECUTABLE}
  "${CMAKE_CURRENT_BINARY_DIR}/doxyfile"
)

# install the documentation
if(REG_BUILD_HTML_DOCS)
  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/
    DESTINATION share/doc/RealityGrid/html
  )
endif(REG_BUILD_HTML_DOCS)

if(REG_BUILD_LATEX_DOCS)
  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/latex/
    DESTINATION share/doc/RealityGrid/latex
  )
endif(REG_BUILD_LATEX_DOCS)
