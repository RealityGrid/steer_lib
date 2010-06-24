/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Robert Haines
 */

/** @internal
    @file ReG_Steer_Dynamic_Loader.h
    @brief Header file defining methods for working with dynamically
    loaded libraries.
    @author Robert Haines
    This header file contains routines for loading dynamic libraries
    and mapping symbols for use by the core library.
*/

#ifndef __REG_STEER_DYNAMIC_LOADER_H__
#define __REG_STEER_DYNAMIC_LOADER_H__

/** @internal
    Load the samples transport module specified by the
    REG_SAMPLES_TRANSPORT environment variable and map the symbols
    contained within to the core library function pointers.
 */
int Load_samples_transport_api();

/** @internal
    Load the steering transport module specified by the
    REG_STEERING_TRANSPORT environment variable and map the symbols
    contained within to the core library function pointers.
 */
int Load_steering_transport_api();

/** @internal
    @param name The name of the symbol to load.
    @param suffix The suffix to be added to the symbol name before loading.
    @param module The pointer to the dynamically loaded library from which
    the symbol is to be loaded.
    @param func_ptr The function pointer to which the loaded symbol is to
    be mapped.

    Load the named symbol from the provided module and map it to the
    function pointer indicated.
 */
int Load_symbol(const char* name, const char* suffix,
		void* module, void (**func_ptr)());

#endif /* __REG_STEER_DYNAMIC_LOADER_H__ */
