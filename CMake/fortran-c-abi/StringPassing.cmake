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

macro(fc_string_test TYPE FUNC_NAME)
  try_compile(STRING_COMPILE_RESULT
    ${PROJECT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/strings-${TYPE}
    ${PROJECT_SOURCE_DIR}/CMake/fortran-c-abi/strings strings
    CMAKE_FLAGS "-DSTRING_FUNC:STRING=-DSF=${FUNC_NAME}"
  )

  if(STRING_COMPILE_RESULT)
    exec_program(
      ${PROJECT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/strings-${TYPE}/strings
      OUTPUT_VARIABLE FC_STRING_OUTPUT
    )

  endif(STRING_COMPILE_RESULT)
endmacro(fc_string_test TYPE FUNC_NAME)


# build string function tester
if(NOT DEFINED FC_STRING_PASSING)
  if(DEFINED FC_STRING_FUNC)
  message(STATUS "Determining how to pass strings between C and Fortran")
  set(STRING_FUNC_TRAIL "\"${FC_STRING_FUNC}(str_ptr, val, str_len)\"")
  set(STRING_FUNC_PAIR "\"${FC_STRING_FUNC}(str_ptr, str_len, val)\"")
  set(string_doc "Part of the Fortran-C string conversion m4 macro")

  fc_string_test(trail ${STRING_FUNC_TRAIL})
  if(FC_STRING_OUTPUT)
    #message("Trailing")
    set(FC_STRING_HEADER "" CACHE STRING ${string_doc} FORCE)
    set(FC_STRING_SAVE "divert(1)$1`'divert(0)"
      CACHE STRING ${string_doc} FORCE)
    set(FC_STRING_ARGS "($1`'undivert(1))" CACHE STRING ${string_doc} FORCE)
    set(FC_STRING_STRING_ARG "$1_ptr`'SAVE(`, $1_len')"
      CACHE STRING ${string_doc} FORCE)
    set(FC_STRING_STRING_ARG_DECL "char * $1_ptr; int $1_len"
      CACHE STRING ${string_doc} FORCE)
    set(FC_STRING_STRING_LEN "$1_len" CACHE STRING ${string_doc} FORCE)
    set(FC_STRING_STRING_PTR "$1_ptr" CACHE STRING ${string_doc} FORCE)
  else(FC_STRING_OUTPUT)
  fc_string_test(pair ${STRING_FUNC_PAIR})
    if(FC_STRING_OUTPUT)
      #message("Paired")
      set(FC_STRING_HEADER "" CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_SAVE "" CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_ARGS "($1)" CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_ARG "$1_ptr, $1_len"
	CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_ARG_DECL "char * $1_ptr; int * $1_len"
	CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_LEN "(*$1_len)" CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_PTR "$1_ptr" CACHE STRING ${string_doc} FORCE)
    else(FC_STRING_OUTPUT)
      #message("Strings are complicated - probably Cray")
      set(FC_STRING_HEADER "#include <fortran.h>"
	CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_SAVE "" CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_ARGS "($1)" CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_ARG "$1_fcd" CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_ARG_DECL "_fcd $1_fcd"
	CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_LEN "_fcdlen($1_fcd)"
	CACHE STRING ${string_doc} FORCE)
      set(FC_STRING_STRING_PTR "_fcdtocp($1_fcd)"
	CACHE STRING ${string_doc} FORCE)
    endif(FC_STRING_OUTPUT)
  endif(FC_STRING_OUTPUT)

  mark_as_advanced(
    FC_STRING_HEADER FC_STRING_SAVE FC_STRING_ARGS FC_STRING_STRING_ARG
    FC_STRING_STRING_ARG_DECL FC_STRING_STRING_LEN FC_STRING_STRING_PTR
  )

  endif(DEFINED FC_STRING_FUNC)
endif(NOT DEFINED FC_STRING_PASSING)
