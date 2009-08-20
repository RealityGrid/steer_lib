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

#
# Register modules in this file.
#
# To use the register_module macro:
#
# register_module(
#   the type of module (Samples, Steering, etc)
#   what is provided (Sockets, Files, etc)
#   source files unique to this module (filenames ; separated)
#   source files common to other modules (filenames ; separated)
# )
#

register_module(
  Samples
  Sockets
  "ReG_Steer_Samples_Transport_Sockets_Shared.c;ReG_Steer_Samples_Transport_Sockets.c"
  "ReG_Steer_Sockets_Common.c"
)

register_module(
  Samples
  Files
  "ReG_Steer_Samples_Transport_Files.c"
  "ReG_Steer_Files_Common.c"
)

register_module(
  Samples
  Proxy
  "ReG_Steer_Samples_Transport_Sockets_Shared.c;ReG_Steer_Samples_Transport_Proxy.c"
  "ReG_Steer_Sockets_Common.c"
)

register_module(
  Steering
  Sockets
  "ReG_Steer_Steering_Transport_Sockets.c"
  "ReG_Steer_Sockets_Common.c"
)

register_module(
  Steering
  WSRF
  "stdsoap2.c;soapC.c;soapClient.c;ReG_Steer_WSRF_Utilities.c;ReG_Steer_Steering_Transport_WSRF.c"
  ""
)

register_module(
  Steering
  Files
  "ReG_Steer_Steering_Transport_Files.c"
  "ReG_Steer_Files_Common.c"
)

##
## DO NOT REGISTER ANY MODULES BELOW HERE
##

# choose a module to link the examples with
# or build into the main lib if monolithic
foreach(type ${REG_MODULES_TYPES})
  # init doc strings
  if(REG_BUILD_MODULAR_LIBS)
    set(doc "The module you would like to link the examples with to provide ${type}. Available modules:")
  else(REG_BUILD_MODULAR_LIBS)
    set(doc "The module you would like to build into your monolithic library to provide ${type}. Available modules:")
  endif(REG_BUILD_MODULAR_LIBS)

  # complete doc string
  foreach(mod ${REG_MODULES_${type}})
    set(doc "${doc} ${mod}")
  endforeach(mod ${REG_MODULES_${type}})

  # set default module
  list(GET REG_MODULES_${type} 0 default)
  set(REG_USE_MODULE_${type} "${default}" CACHE STRING ${doc})

  # check that a registered module has been chosen
  # by the user for each type of module available
  list(FIND REG_MODULES_${type} ${REG_USE_MODULE_${type}} exists)
  if(exists EQUAL -1)
    message(SEND_ERROR
      "You cannot specify '${REG_USE_MODULE_${type}}' for "
      "REG_USE_MODULE_${type} as a module of that name has not been registered "
      "as providing ${type} transport. Please check the help for "
      "REG_USE_MODULE_${type} to see the list of modules you can use."
    )
  endif(exists EQUAL -1)
endforeach(type ${REG_MODULES_TYPES})
