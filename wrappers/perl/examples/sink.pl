#!/usr/bin/env perl
#  This file is part of the RealityGrid Steering Library Perl Wrappers.
# 
#  (C) Copyright 2006, University of Manchester, United Kingdom,
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

use ReG_Steer;
use strict;

# Some global variables
my $nloops = 5000;

my $finished = 0;
my $status = 0;
my $sleep_time = 1;

# init and enable the steering library
#&ReG_Steer::Steering_enable($ReG_Steer::REG_TRUE);
&Steering_enable($REG_TRUE);

my @cmds = ($REG_STR_STOP);
$status = &Steering_initialize("Perl Sink v1.0", \@cmds);
if($status != $REG_SUCCESS) {
    die "Steering library initialization failed\n";
}

# Register IO channel
my $iotype_handle;
($status, $iotype_handle) = &Register_IOType("VTK_STRUCTURED_POINTS", $REG_IO_IN, 1);
if($status != $REG_SUCCESS) {
    die "Failed to register IO type\n";
}

# Register param(s)
my $bytes_read = new ReG_Steer::intp();
my $test_steer = new ReG_Steer::floatp();
$status = &Register_param("Bytes_read", $REG_FALSE, $bytes_read, $REG_INT, "", "");
$status = &Register_param("Test_Steer", $REG_TRUE, $test_steer, $REG_FLOAT, "-10.0", "10.0");
if($status != $REG_SUCCESS) {
    die "Failed to register parameter(s)\n";
}
$test_steer->assign(5.0);

# main loop waiting for data
for(my $i = 0; $i < $nloops; $i++) {
    sleep $sleep_time;
    print "\ni = $i\nTEST_STEER = ",$test_steer->value(),"\n";
    my ($changed_param_labels, $recvd_cmds, $recvd_cmd_params);
    ($status, $changed_param_labels, $recvd_cmds, $recvd_cmd_params) = &Steering_control($i);
    if($status != $REG_SUCCESS) {
	next;
    }
    my $num_params_changed = @$changed_param_labels;
    my $num_recvd_cmds = @$recvd_cmds;

    # zero count of bytes read this time
    $bytes_read->assign(0);

    if($num_recvd_cmds > 0) {
	for(my $icmd = 0; $icmd < $num_recvd_cmds; $icmd++) {
	    if(@$recvd_cmds[$icmd] == $REG_STR_STOP) {
		$finished = 1;
		last;
	    }
	    else {
		if(@$recvd_cmds[$icmd] == $iotype_handle) {
		    my $iohandle;
		    ($status, $iohandle) = &Consume_start($iotype_handle);
		    if($status == $REG_SUCCESS) {
			my ($data_type, $data_count);
			($status, $data_type, $data_count) = &Consume_data_slice_header($iohandle);
			while($status == $REG_SUCCESS) {
			    print "Got data: type = $data_type, count = $data_count\n";
			    my $outdata;
			    ($status, $outdata) = &Consume_data_slice($iohandle,
								      $data_type,
								      $data_count);
			    if($data_type == $REG_CHAR) {
				print $$outdata,"\n";
			    }
			    $bytes_read->assign($bytes_read->value() +
						($data_count * &Sizeof($data_type)));

			    ($status, $data_type, $data_count) = &Consume_data_slice_header($iohandle);
			} # while REG_SUCCESS
		    }
		    ($status, $iohandle) = &Consume_stop($iohandle);
		} #
		last;
	    } # if recvd_cmds[icmd]
	    if($finished == 1) {
		last;
	    }
	} # for icmd
	if($finished == 1) {
	    last;
	}
    } # if num_recvd_cmds > 0
} # for nloops

$status = &Steering_finalize();
