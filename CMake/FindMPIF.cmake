# - Message Passing Interface (MPI) for Fortran module.
#
# The Message Passing Interface (MPI) is a library used to write
# high-performance parallel applications that use message passing, and
# is typically deployed on a cluster. MPI is a standard interface
# (defined by the MPI forum) for which many implementations are
# available. All of these implementations have somewhat different
# compilation approaches (different include paths, libraries to link
# against, etc.), and this module tries to smooth out those differences.
#
# This module will set the following variables:
#   MPIF_FOUND                 TRUE if we have found MPI for Fortran
#   MPIF_COMPILE_FLAGS         Compilation flags for Fortran MPI programs
#   MPIF_INCLUDE_PATH          Include path(s) for Fortran MPI header
#   MPIF_LINK_FLAGS            Linking flags for Fortran MPI programs
#   MPIF_LIBRARY               First Fortran MPI library to link (cached)
#   MPIF_EXTRA_LIBRARY         Extra Fortran MPI libraries to link (cached)
#   MPIF_LIBRARIES             All libraries to link Fortran MPI programsd
#
# This module will attempt to auto-detect these settings, first by looking
# for a MPI Fortran compiler, which many MPI implementations provide as a
# pass-through to the native compiler to simplify the compilation of MPI
# programs. The MPI compiler is stored in the cache variable MPIF_COMPILER,
# and will attempt to look for commonly-named drivers mpifortran, mpif95,
# mpif90, or mpif77.  If the compiler driver is found and recognized, it
# will be used to set all of the module variables. To skip this
# auto-detection, set MPIF_LIBRARY and MPIF_INCLUDE_PATH in the CMake
# cache.
#
# If no compiler driver is found or the compiler driver is not
# recognized, this module will then search for common include paths
# and library names to try to detect MPI.
#
# If CMake initially finds a different MPI than was intended, and you
# want to use the MPI compiler auto-detection for a different MPI
# implementation, set MPIF_COMPILER to the MPI compiler driver you want
# to use (e.g., mpif77) and then set MPIF_LIBRARY to the string
# MPIF_LIBRARY-NOTFOUND. When you re-configure, auto-detection of MPI
# will run again with the newly-specified MPIF_COMPILER.
#
# This module implicitly also loads the FindMPI module (the one for C).
# Thus, useful variables like MPIEXEC, MPIEXEC_NUMPROC_FLAG,
# MPIEXEC_PREFLAGS, and MPIEXEC_POSTFLAGS are also defined.  See the
# documentation for the FindMPI module for more information.
#
# When using MPIEXEC to execute MPI applications, you should typically
# use all of the MPIEXEC flags as follows:
#   ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} PROCS ${MPIEXEC_PREFLAGS} EXECUTABLE
#     ${MPIEXEC_POSTFLAGS} ARGS
# where PROCS is the number of processors on which to execute the program,
# EXECUTABLE is the MPI program, and ARGS are the arguments to pass to the
# MPI program.

#=============================================================================
# Copyright 2001-2009 Kitware, Inc.
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distributed this file outside of CMake, substitute the full
#  License text for the above reference.)

# This module is maintained by David Partyka <dave.partyka@kitware.com>.

# A set of directories to search through in addition to the standard system paths
# that find_program will search through.
# Microsoft HPC SDK is automatically added to the system path
# Argonne National Labs MPICH2 sets a registry key that we can use.

find_package(MPI QUIET)

set(_MPIF_PACKAGE_DIR
  mpi
  mpich
  openmpi
  lib/mpi
  lib/mpich
  lib/openmpi
  "MPICH/SDK"
  "Microsoft Compute Cluster Pack"
  )

set(_MPIF_PREFIX_PATH)
if(WIN32)
  list(APPEND _MPIF_PREFIX_PATH "[HKEY_LOCAL_MACHINE\\SOFTWARE\\MPICH\\SMPD;binary]/..")
endif()

foreach(SystemPrefixDir ${CMAKE_SYSTEM_PREFIX_PATH})
  foreach(MpiPackageDir ${_MPIF_PREFIX_PATH})
    if(EXISTS ${SystemPrefixDir}/${MpiPackageDir})
      list(APPEND _MPIF_PREFIX_PATH "${SystemPrefixDir}/${MpiPackageDir}")
    endif()
  endforeach(MpiPackageDir)
endforeach(SystemPrefixDir)

# call get_filename_component twice to remove mpiexec and the directory it exists in (typically bin).
# This gives us a fairly reliable base directory to search for /bin /lib and /include from.
get_filename_component(_MPIF_BASE_DIR "${MPIEXEC}" PATH)
get_filename_component(_MPIF_BASE_DIR "${_MPIF_BASE_DIR}" PATH)

# If there is an mpi compiler find it and interogate (farther below) it for the include
# and lib dirs otherwise we will continue to search from ${_MPIF_BASE_DIR}.
find_program(MPIF_COMPILER
  NAMES mpifortran openmpif90 openmpif77 mpif95 mpif90 mpif77
  HINTS "${_MPIF_BASE_DIR}"
  PATH_SUFFIXES bin
  DOC "MPI Fortran compiler. Used only to detect MPI compilation flags.")
mark_as_advanced(MPIF_COMPILER)

if (MPIF_INCLUDE_PATH AND MPIF_LIBRARY)
  # Do nothing: we already have MPIF_INCLUDE_PATH and MPIF_LIBRARY in
  # the cache, and we don't want to override those settings.
elseif (MPIF_COMPILER)
  # Check whether the -showme:compile option works. This indicates
  # that we have either Open MPI or a newer version of LAM-MPI, and
  # implies that -showme:link will also work.
  # Note that Windows distros do not have an mpi compiler to interogate.
  exec_program(${MPIF_COMPILER}
    ARGS -showme:compile
    OUTPUT_VARIABLE MPIF_COMPILE_CMDLINE
    RETURN_VALUE MPIF_COMPILER_RETURN)

  if (MPIF_COMPILER_RETURN EQUAL 0)
    # If we appear to have -showme:compile, then we should also have
    # -showme:link. Try it.
    exec_program(${MPIF_COMPILER}
      ARGS -showme:link
      OUTPUT_VARIABLE MPIF_LINK_CMDLINE
      RETURN_VALUE MPIF_COMPILER_RETURN)

    # Note that we probably have -showme:incdirs and -showme:libdirs
    # as well.
    set(MPIF_COMPILER_MAY_HAVE_INCLIBDIRS TRUE)
  endif (MPIF_COMPILER_RETURN EQUAL 0)

  if (MPIF_COMPILER_RETURN EQUAL 0)
    # Do nothing: we have our command lines now
  else (MPIF_COMPILER_RETURN EQUAL 0)
    # Older versions of LAM-MPI have "-showme". Try it.
    exec_program(${MPIF_COMPILER}
      ARGS -showme
      OUTPUT_VARIABLE MPIF_COMPILE_CMDLINE
      RETURN_VALUE MPIF_COMPILER_RETURN)
  endif (MPIF_COMPILER_RETURN EQUAL 0)

  if (MPIF_COMPILER_RETURN EQUAL 0)
    # Do nothing: we have our command lines now
  else (MPIF_COMPILER_RETURN EQUAL 0)
    # MPICH uses "-show". Try it.
    exec_program(${MPIF_COMPILER}
      ARGS -show
      OUTPUT_VARIABLE MPIF_COMPILE_CMDLINE
      RETURN_VALUE MPIF_COMPILER_RETURN)
  endif (MPIF_COMPILER_RETURN EQUAL 0)

  if (MPIF_COMPILER_RETURN EQUAL 0)
    # We have our command lines, but we might need to copy
    # MPIF_COMPILE_CMDLINE into MPIF_LINK_CMDLINE, if the underlying
    if (NOT MPIF_LINK_CMDLINE)
      SET(MPIF_LINK_CMDLINE ${MPIF_COMPILE_CMDLINE})
    endif (NOT MPIF_LINK_CMDLINE)
  else (MPIF_COMPILER_RETURN EQUAL 0)
    message(STATUS "Unable to determine MPI from MPI driver ${MPIF_COMPILER}")
  endif (MPIF_COMPILER_RETURN EQUAL 0)
endif (MPIF_INCLUDE_PATH AND MPIF_LIBRARY)

if (MPIF_INCLUDE_PATH AND MPIF_LIBRARY)
  # Do nothing: we already have MPIF_INCLUDE_PATH and MPIF_LIBRARY in
  # the cache, and we don't want to override those settings.
elseif (MPIF_COMPILE_CMDLINE)
  # Extract compile flags from the compile command line.
  string(REGEX MATCHALL "-D([^\" ]+|\"[^\"]+\")" MPIF_ALL_COMPILE_FLAGS "${MPIF_COMPILE_CMDLINE}")
  set(MPIF_COMPILE_FLAGS_WORK)
  foreach(FLAG ${MPIF_ALL_COMPILE_FLAGS})
    if (MPIF_COMPILE_FLAGS_WORK)
      set(MPIF_COMPILE_FLAGS_WORK "${MPIF_COMPILE_FLAGS_WORK} ${FLAG}")
    else(MPIF_COMPILE_FLAGS_WORK)
      set(MPIF_COMPILE_FLAGS_WORK ${FLAG})
    endif(MPIF_COMPILE_FLAGS_WORK)
  endforeach(FLAG)

  # Extract include paths from compile command line
  string(REGEX MATCHALL "-I([^\" ]+|\"[^\"]+\")" MPIF_ALL_INCLUDE_PATHS "${MPIF_COMPILE_CMDLINE}")
  set(MPIF_INCLUDE_PATH_WORK)
  foreach(IPATH ${MPIF_ALL_INCLUDE_PATHS})
    string(REGEX REPLACE "^-I" "" IPATH ${IPATH})
    string(REGEX REPLACE "//" "/" IPATH ${IPATH})
    list(APPEND MPIF_INCLUDE_PATH_WORK ${IPATH})
  endforeach(IPATH)

  if (NOT MPIF_INCLUDE_PATH_WORK)
    if (MPIF_COMPILER_MAY_HAVE_INCLIBDIRS)
      # The compile command line didn't have any include paths on it,
      # but we may have -showme:incdirs. Use it.
      exec_program(${MPIF_COMPILER}
        ARGS -showme:incdirs
        OUTPUT_VARIABLE MPIF_INCLUDE_PATH_WORK
        RETURN_VALUE MPIF_COMPILER_RETURN)
      separate_arguments(MPIF_INCLUDE_PATH_WORK)
    endif (MPIF_COMPILER_MAY_HAVE_INCLIBDIRS)
  endif (NOT MPIF_INCLUDE_PATH_WORK)

  if (NOT MPIF_INCLUDE_PATH_WORK)
    # If all else fails, just search for mpif.h in the normal include
    # paths.
    find_path(MPIF_INCLUDE_PATH mpif.h
  HINTS ${_MPIF_BASE_DIR} ${_MPIF_PREFIX_PATH}
  PATH_SUFFIXES include
    )
    set(MPIF_INCLUDE_PATH_WORK ${MPIF_INCLUDE_PATH})
  endif (NOT MPIF_INCLUDE_PATH_WORK)

  # Extract linker paths from the link command line
  string(REGEX MATCHALL "-L([^\" ]+|\"[^\"]+\")" MPIF_ALL_LINK_PATHS "${MPIF_LINK_CMDLINE}")
  set(MPIF_LINK_PATH)
  foreach(LPATH ${MPIF_ALL_LINK_PATHS})
    string(REGEX REPLACE "^-L" "" LPATH ${LPATH})
    string(REGEX REPLACE "//" "/" LPATH ${LPATH})
    list(APPEND MPIF_LINK_PATH ${LPATH})
  endforeach(LPATH)

  if (NOT MPIF_LINK_PATH)
    if (MPIF_COMPILER_MAY_HAVE_INCLIBDIRS)
      # The compile command line didn't have any linking paths on it,
      # but we may have -showme:libdirs. Use it.
      exec_program(${MPIF_COMPILER}
        ARGS -showme:libdirs
        OUTPUT_VARIABLE MPIF_LINK_PATH
        RETURN_VALUE MPIF_COMPILER_RETURN)
      separate_arguments(MPIF_LINK_PATH)
    endif (MPIF_COMPILER_MAY_HAVE_INCLIBDIRS)
  endif (NOT MPIF_LINK_PATH)

  # Extract linker flags from the link command line
  string(REGEX MATCHALL "-Wl,([^\" ]+|\"[^\"]+\")" MPIF_ALL_LINK_FLAGS "${MPIF_LINK_CMDLINE}")
  set(MPIF_LINK_FLAGS_WORK)
  foreach(FLAG ${MPIF_ALL_LINK_FLAGS})
    if (MPIF_LINK_FLAGS_WORK)
      set(MPIF_LINK_FLAGS_WORK "${MPIF_LINK_FLAGS_WORK} ${FLAG}")
    else(MPIF_LINK_FLAGS_WORK)
      set(MPIF_LINK_FLAGS_WORK ${FLAG})
    endif(MPIF_LINK_FLAGS_WORK)
  endforeach(FLAG)

  # Extract the set of libraries to link against from the link command
  # line
  string(REGEX MATCHALL "-l([^\" ]+|\"[^\"]+\")" MPIF_LIBNAMES "${MPIF_LINK_CMDLINE}")

  # Determine full path names for all of the libraries that one needs
  # to link against in an MPI program
  set(MPIF_LIBRARIES)
  foreach(LIB ${MPIF_LIBNAMES})
    string(REGEX REPLACE "^-l" "" LIB ${LIB})
    set(MPIF_LIB "MPIF_LIB-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
    find_library(MPIF_LIB ${LIB} HINTS ${MPIF_LINK_PATH})
    if (MPIF_LIB)
      list(APPEND MPIF_LIBRARIES ${MPIF_LIB})
    else (MPIF_LIB)
      message(SEND_ERROR "Unable to find MPI library ${LIB}")
    endif (MPIF_LIB)
  endforeach(LIB)
  set(MPIF_LIB "MPIF_LIB-NOTFOUND" CACHE INTERNAL "Scratch variable for MPI detection" FORCE)

  # Chop MPIF_LIBRARIES into the old-style MPIF_LIBRARY and
  # MPIF_EXTRA_LIBRARY.
  list(LENGTH MPIF_LIBRARIES MPIF_NUMLIBS)
  list(LENGTH MPIF_LIBNAMES MPIF_NUMLIBS_EXPECTED)
  if (MPIF_NUMLIBS EQUAL MPIF_NUMLIBS_EXPECTED)
    list(GET MPIF_LIBRARIES 0 MPIF_LIBRARY_WORK)
    set(MPIF_LIBRARY ${MPIF_LIBRARY_WORK} CACHE FILEPATH "Fortran MPI library to link against" FORCE)
  else (MPIF_NUMLIBS EQUAL MPIF_NUMLIBS_EXPECTED)
    set(MPIF_LIBRARY "MPIF_LIBRARY-NOTFOUND" CACHE FILEPATH "Fortran MPI library to link against" FORCE)
  endif (MPIF_NUMLIBS EQUAL MPIF_NUMLIBS_EXPECTED)
  if (MPIF_NUMLIBS GREATER 1)
    set(MPIF_EXTRA_LIBRARY_WORK ${MPIF_LIBRARIES})
    list(REMOVE_AT MPIF_EXTRA_LIBRARY_WORK 0)
    set(MPIF_EXTRA_LIBRARY ${MPIF_EXTRA_LIBRARY_WORK} CACHE STRING "Extra MPI libraries to link against" FORCE)
  else (MPIF_NUMLIBS GREATER 1)
    set(MPIF_EXTRA_LIBRARY "MPIF_EXTRA_LIBRARY-NOTFOUND" CACHE STRING "Extra MPI libraries to link against" FORCE)
  endif (MPIF_NUMLIBS GREATER 1)

  # Set up all of the appropriate cache entries
  set(MPIF_COMPILE_FLAGS ${MPIF_COMPILE_FLAGS_WORK} CACHE STRING "MPI Fortran compilation flags" FORCE)
  set(MPIF_INCLUDE_PATH ${MPIF_INCLUDE_PATH_WORK} CACHE STRING "MPI Fortran include path" FORCE)
  set(MPIF_LINK_FLAGS ${MPIF_LINK_FLAGS_WORK} CACHE STRING "MPI Fortran linking flags" FORCE)
else (MPIF_COMPILE_CMDLINE)
# No MPI compiler to interogate so attempt to find everything with find functions.
  find_path(MPIF_INCLUDE_PATH mpif.h
    HINTS ${_MPIF_BASE_DIR} ${_MPIF_PREFIX_PATH}
    PATH_SUFFIXES include
    )

  # Decide between 32-bit and 64-bit libraries for Microsoft's MPI
  if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
    set(MS_MPIF_ARCH_DIR amd64)
  else()
    set(MS_MPIF_ARCH_DIR i386)
  endif()

  find_library(MPIF_LIBRARY
    NAMES mpi mpich msmpi
    HINTS ${_MPIF_BASE_DIR} ${_MPIF_PREFIX_PATH}
    PATH_SUFFIXES lib lib/${MS_MPIF_ARCH_DIR} Lib Lib/${MS_MPIF_ARCH_DIR}
    )

  find_library(MPIF_EXTRA_LIBRARY
    NAMES mpi_f77
    HINTS ${_MPIF_BASE_DIR} ${_MPIF_PREFIX_PATH}
    PATH_SUFFIXES lib
    DOC "Extra MPI libraries to link against.")

  set(MPIF_COMPILE_FLAGS "" CACHE STRING "MPI compilation flags")
  set(MPIF_LINK_FLAGS "" CACHE STRING "MPI linking flags")
endif (MPIF_INCLUDE_PATH AND MPIF_LIBRARY)

# on BlueGene/L the MPI lib is named libmpich.rts.a, there also these additional libs are required
if("${MPIF_LIBRARY}" MATCHES "mpich.rts")
   set(MPIF_EXTRA_LIBRARY ${MPIF_EXTRA_LIBRARY} msglayer.rts devices.rts rts.rts devices.rts)
   set(MPIF_LIBRARY ${MPIF_LIBRARY}  msglayer.rts devices.rts rts.rts devices.rts)
endif("${MPIF_LIBRARY}" MATCHES "mpich.rts")

# Set up extra variables to conform to
if (MPIF_EXTRA_LIBRARY)
  set(MPIF_LIBRARIES ${MPIF_LIBRARY} ${MPIF_EXTRA_LIBRARY})
else (MPIF_EXTRA_LIBRARY)
  set(MPIF_LIBRARIES ${MPIF_LIBRARY})
endif (MPIF_EXTRA_LIBRARY)

if (MPIF_INCLUDE_PATH AND MPIF_LIBRARY)
  set(MPIF_FOUND TRUE)
else (MPIF_INCLUDE_PATH AND MPIF_LIBRARY)
  set(MPIF_FOUND FALSE)
endif (MPIF_INCLUDE_PATH AND MPIF_LIBRARY)

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments
find_package_handle_standard_args(MPIF DEFAULT_MSG MPIF_LIBRARY MPIF_INCLUDE_PATH)

mark_as_advanced(
  MPIF_INCLUDE_PATH
  MPIF_COMPILE_FLAGS
  MPIF_LINK_FLAGS
  MPIF_LIBRARY
  MPIF_EXTRA_LIBRARY)

# unset to cleanup namespace
unset(_MPIF_PACKAGE_DIR)
unset(_MPIF_PREFIX_PATH)
unset(_MPIF_BASE_DIR)
