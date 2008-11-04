#
#  The Fortran/C ABI wrangler - This file works out how names are mangled.
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
	endif(${under} EQUAL 0)
      endif(${under} LESS 10)
    endforeach(under ${MANGLE_OUTPUT})

    mark_as_advanced(FC_NAME_MANGLE)

  endif(MANGLE_COMPILE_RESULT)

endif(NOT DEFINED FC_NAME_MANGLE)
