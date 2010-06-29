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

# build mangler tester
if(NOT DEFINED FC_NAME_MANGLE)
  message(STATUS "Determining Fortran name mangling required")
  set(mangle_doc "The Fortran-C name mangling m4 macro")

  try_compile(MANGLE_COMPILE_RESULT
    ${PROJECT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/mangle
    ${PROJECT_SOURCE_DIR}/CMake/fortran-c-abi/mangle mangle
  )

  if(MANGLE_COMPILE_RESULT)
    exec_program(
      ${PROJECT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/mangle/mangle
      OUTPUT_VARIABLE MANGLE_OUTPUT
    )

    foreach(under ${MANGLE_OUTPUT})
      if(${under} LESS 10)
	#message("Lower case")
	if(${under} EQUAL 0)
	  #message("No underscores")
	  set(FC_NAME_MANGLE "$1" CACHE STRING ${mangle_doc} FORCE)
	  set(FC_STRING_FUNC "sf")
	elseif(${under} EQUAL 1)
          #message("One underscore")
	  set(FC_NAME_MANGLE "$1_" CACHE STRING ${mangle_doc} FORCE)
	  set(FC_STRING_FUNC "sf_")
	else(${under} EQUAL 1)
	  #message("Two underscores")
	  set(FC_NAME_MANGLE "ifelse(`-1',index($1, `_'),`$1_',`$1__')"
	    CACHE STRING ${mangle_doc} FORCE)
	  set(FC_STRING_FUNC "sf_")
	endif(${under} EQUAL 0)
      else(${under} LESS 10)
	#message("Upper case")
	if(${under} EQUAL 10)
	  #message("No underscores")
	  set(FC_NAME_MANGLE "translit($1,abcdefghijklmnopqrstuvwxyz,ABCDEFGHIJKLMNOPQRSTUVWXYZ)"
	    CACHE STRING ${mangle_doc} FORCE)
	  set(FC_STRING_FUNC "SF")
        elseif(${under} EQUAL 11)
          #message("One underscore")
	  set(FC_NAME_MANGLE "translit($1_,abcdefghijklmnopqrstuvwxyz,ABCDEFGHIJKLMNOPQRSTUVWXYZ)"
	    CACHE STRING ${mangle_doc} FORCE)
	  set(FC_STRING_FUNC "SF_")
	else(${under} EQUAL 12)
	  #message("Two underscores")
	  set(FC_NAME_MANGLE "translit(ifelse(`-1',index($1, `_'),`$1_',`$1__'),abcdefghijklmnopqrstuvwxyz,ABCDEFGHIJKLMNOPQRSTUVWXYZ)"
	    CACHE STRING ${mangle_doc} FORCE)
	  set(FC_STRING_FUNC "SF_")
	endif(${under} EQUAL 10)
      endif(${under} LESS 10)
    endforeach(under ${MANGLE_OUTPUT})

    mark_as_advanced(FC_NAME_MANGLE)

  endif(MANGLE_COMPILE_RESULT)

endif(NOT DEFINED FC_NAME_MANGLE)
