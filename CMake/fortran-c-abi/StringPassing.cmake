#
#  The Fortran/C ABI wrangler - This file works out how strings are passed.
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
