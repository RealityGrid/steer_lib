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
    @file ReG_Steer_Dynamic_Loader.c
    @brief Methods for working with dynamically loaded libraries.
    @author Robert Haines
*/

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Dynamic_Loader.h"

#include "ReG_Steer_Samples_Transport_API.h"
#include "ReG_Steer_Steering_Transport_API.h"

/** Basic library config - declared in ReG_Steer_Common */
extern Steer_lib_config_type Steer_lib_config;

/*----------------------------------------------------------*/

int Load_samples_transport_api() {
  void* mod_handle;
  char* env;
  char mod_name[REG_MAX_STRING_LENGTH];

  env = getenv("REG_SAMPLES_TRANSPORT");
  if(env == NULL) {
    fprintf(stderr, "Cannot load samples transport module: "
	    "REG_SAMPLES_TRANSPORT not set.\n");
    return REG_FAILURE;
  }
  snprintf(mod_name, REG_MAX_STRING_LENGTH, "ReG_Steer_Samples_%s.so", env);

  mod_handle = dlopen(mod_name, RTLD_NOW | RTLD_GLOBAL);
  if(!mod_handle) {
    fprintf(stderr, "Cannot open module: %s\n", dlerror());
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, " * Loaded samples transport module: %s *\n", mod_name);
#endif

  Load_symbol("Finalize_IOType_transport", env, mod_handle, (void*) &Finalize_IOType_transport_impl);
  Load_symbol("Initialize_samples_transport", env, mod_handle, (void*) &Initialize_samples_transport_impl);
  Load_symbol("Finalize_samples_transport", env, mod_handle, (void*) &Finalize_samples_transport_impl);
  Load_symbol("Initialize_IOType_transport", env, mod_handle, (void*) &Initialize_IOType_transport_impl);
  Load_symbol("Enable_IOType", env, mod_handle, (void*) &Enable_IOType_impl);
  Load_symbol("Disable_IOType", env, mod_handle, (void*) &Disable_IOType_impl);
  Load_symbol("Get_communication_status", env, mod_handle, (void*) &Get_communication_status_impl);
  Load_symbol("Emit_data_non_blocking", env, mod_handle, (void*) &Emit_data_non_blocking_impl);
  Load_symbol("Emit_header", env, mod_handle, (void*) &Emit_header_impl);
  Load_symbol("Emit_data", env, mod_handle, (void*) &Emit_data_impl);
  Load_symbol("Consume_msg_header", env, mod_handle, (void*) &Consume_msg_header_impl);
  Load_symbol("Emit_msg_header", env, mod_handle, (void*) &Emit_msg_header_impl);
  Load_symbol("Consume_start_data_check", env, mod_handle, (void*) &Consume_start_data_check_impl);
  Load_symbol("Consume_data_read", env, mod_handle, (void*) &Consume_data_read_impl);
  Load_symbol("Emit_ack", env, mod_handle, (void*) &Emit_ack_impl);
  Load_symbol("Consume_ack", env, mod_handle, (void*) &Consume_ack_impl);
  Load_symbol("Get_IOType_address", env, mod_handle, (void*) &Get_IOType_address_impl);
  Load_symbol("Emit_start", env, mod_handle, (void*) &Emit_start_impl);
  Load_symbol("Emit_stop", env, mod_handle, (void*) &Emit_stop_impl);
  Load_symbol("Consume_stop", env, mod_handle, (void*) &Consume_stop_impl);

  Steer_lib_config.samples_mod_handle = mod_handle;

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Load_steering_transport_api() {
  void* mod_handle;
  char* env;
  char mod_name[REG_MAX_STRING_LENGTH];

  env = getenv("REG_STEERING_TRANSPORT");
  if(env == NULL) {
    fprintf(stderr, "Cannot load samples transport module: "
	    "REG_STEERING_TRANSPORT not set.\n");
    return REG_FAILURE;
  }
  snprintf(mod_name, REG_MAX_STRING_LENGTH, "ReG_Steer_Steering_%s.so", env);

  mod_handle = dlopen(mod_name, RTLD_NOW | RTLD_GLOBAL);
  if(!mod_handle) {
    fprintf(stderr, "Cannot open module: %s\n", dlerror());
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, " * Loaded steering transport module: %s *\n", mod_name);
#endif

  Load_symbol("Initialize_log", env, mod_handle, (void*) &Initialize_log_impl);
  Load_symbol("Get_control_msg", env, mod_handle, (void*) &Get_control_msg_impl);
  Load_symbol("Get_status_msg", env, mod_handle, (void*) &Get_status_msg_impl);
  Load_symbol("Detach_from_steerer", env, mod_handle, (void*) &Detach_from_steerer_impl);
  Load_symbol("Steerer_connected", env, mod_handle, (void*) &Steerer_connected_impl);
  Load_symbol("Send_status_msg", env, mod_handle, (void*) &Send_status_msg_impl);
  Load_symbol("Initialize_steering_connection", env, mod_handle, (void*) &Initialize_steering_connection_impl);
  Load_symbol("Finalize_steering_connection", env, mod_handle, (void*) &Finalize_steering_connection_impl);
  Load_symbol("Get_data_io_address", env, mod_handle, (void*) &Get_data_io_address_impl);
  Load_symbol("Record_checkpoint_set", env, mod_handle, (void*) &Record_checkpoint_set_impl);
  Load_symbol("Save_log", env, mod_handle, (void*) &Save_log_impl);
  Load_symbol("Initialize_steerside_transport", env, mod_handle, (void*) &Initialize_steerside_transport_impl);
  Load_symbol("Finalize_steerside_transport", env, mod_handle, (void*) &Finalize_steerside_transport_impl);
  Load_symbol("Sim_attach", env, mod_handle, (void*) &Sim_attach_impl);
  Load_symbol("Sim_attach_security", env, mod_handle, (void*) &Sim_attach_security_impl);
  Load_symbol("Send_control_msg", env, mod_handle, (void*) &Send_control_msg_impl);
  Load_symbol("Send_detach_msg", env, mod_handle, (void*) &Send_detach_msg_impl);
  Load_symbol("Finalize_connection", env, mod_handle, (void*) &Finalize_connection_impl);
  Load_symbol("Get_param_log", env, mod_handle, (void*) &Get_param_log_impl);

  Load_symbol("Get_registry_entries", env, mod_handle, (void*) &Get_registry_entries_impl);

  Steer_lib_config.steering_mod_handle = mod_handle;

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Load_symbol(const char* name, const char* suffix, void* mod, void (**func)()) {
  char* dl_error;
  char sym_name[REG_MAX_STRING_LENGTH];

  snprintf(sym_name, REG_MAX_STRING_LENGTH, "%s_%s", name, suffix);

  dlerror();
  *func = dlsym(mod, sym_name);
  dl_error = dlerror();
  if(dl_error) {
    fprintf(stderr, "Cannot load symbol '%s': %s\n", sym_name, dl_error);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "  - Loaded symbol '%s'\n", sym_name);
#endif

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/
