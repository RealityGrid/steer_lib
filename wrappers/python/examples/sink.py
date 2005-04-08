#!/usr/bin/env python
#  This file is part of the RealityGrid Steering Library Python Wrappers.
# 
#  (C) Copyright 2005, University of Manchester, United Kingdom,
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
#  Author........: Robert Haines

from ReG_Steer import *
import sys, time

# Some global variables
nloops = 5000

finished = 0
status = 0
sleep_time = 1

# init and enable the steering library

Steering_enable(REG_TRUE)

status = Steering_initialize("Python Sink v1.0", [REG_STR_STOP])

if status != REG_SUCCESS:
    print "Steering library initialization failed"
    sys.exit(0)

# Register IO channel

status, iotype_handle = Register_IOType("VTK_STRUCTURED_POINTS", REG_IO_IN, 1)

if status != REG_SUCCESS:
    print "Failed to register IO type"
    sys.exit(0)

# Register param(s)

bytes_read = intp()
status = Register_param("Bytes_read", REG_FALSE, bytes_read, REG_INT, "", "")

# main loop waiting for data
i = 0
while i < nloops:
    time.sleep(sleep_time)
    print "\ni = %d" % i
    status, changed_param_labels, recvd_cmds, recvd_cmd_params = Steering_control(i)
    num_params_changed = len(changed_param_labels)
    num_recvd_cmds = len(recvd_cmds)

    if status != REG_SUCCESS:
        continue

    # zero count of bytes read this time
    bytes_read.assign(0)

    if num_recvd_cmds > 0:
        for icmd in range(0, num_recvd_cmds):
            if recvd_cmds[icmd] == REG_STR_STOP:
                finished = 1
                break
            else:
                if recvd_cmds[icmd] == iotype_handle:
                    status, iohandle = Consume_start(iotype_handle)
                    if status == REG_SUCCESS:
                        status, data_type, data_count = Consume_data_slice_header(iohandle)
                        while status == REG_SUCCESS:
                            print "Got data: type = %d, count = %d" % (data_type, data_count)
                            status, outdata = Consume_data_slice(iohandle,
                                                                 data_type, 
                                                                 data_count)
                            bytes_read.assign(bytes_read.value()
                                              + (data_count * Sizeof(data_type)))
                            
                            status, data_type, data_count = Consume_data_slice_header(iohandle)
                    status, iohandle = Consume_stop(iohandle)
                break
            if finished == 1:
                break
        if finished == 1:
            break
    
    i = i + 1
    
status = Steering_finalize()
