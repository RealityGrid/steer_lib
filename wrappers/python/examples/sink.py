#!/usr/bin/env python

# sink.py
# Robert Haines Feb 2005
#

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
