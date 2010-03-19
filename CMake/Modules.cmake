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
  "ReG_Steer_Sockets_Common.c"
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
