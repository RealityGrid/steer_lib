#!/usr/bin/perl 
use SOAP::Transport::HTTP;
use SGS;

#---------------------------------------------------------------------
#
#    This Perl script launches the Steering Grid Service (as 
#    defined by SGS.pm) as a daemon.
#
#    (C)Copyright 2003, The University of Manchester, United Kingdom,
#    all rights reserved.
#
#    This software is produced by the Supercomputing, Visualization &
#    e-Science Group, Manchester Computing, the Victoria University of
#    Manchester as part of the RealityGrid project.
#
#    This software has been tested with care but is not guaranteed for
#    any particular purpose. Neither the copyright holder, nor the
#    University of Manchester offer any warranties or representations,
#    nor do they accept any liabilities with respect to this software.
#
#    This software must not be used for commercial gain without the
#    written permission of the authors.
#    
#    This software must not be redistributed without the written
#    permission of the authors.
#
#    Permission is granted to modify this software, provided any
#    modifications are made freely available to the original authors.
# 
#    Supercomputing, Visualization & e-Science Group
#    Manchester Computing
#    University of Manchester
#    Manchester M13 9PL
#    
#    WWW:    http://www.sve.man.ac.uk  
#    email:  sve@man.ac.uk
#    Tel:    +44 161 275 6095
#    Fax:    +44 161 275 6800    
#
#    Initial version by:  M McKeown and A Porter, 23.4.2003       0.1
#
#---------------------------------------------------------------------

# Set timeout to 8 hours
$TIMEOUT = (8 * 60 * 60);
$PORT = 50005;

$SIG{ALRM} = \&timed_out;

# don't want to die on 'Broken pipe' or Ctrl-C
  $SIG{PIPE} = $SIG{INT} = 'IGNORE';

my $daemon = SOAP::Transport::HTTP::Daemon
    -> new (LocalPort => $PORT)
    -> dispatch_to('SGS');

$Contact=$daemon->url;

print "Contact to SOAP server at ", $daemon->url, "\n";

eval{
 alarm($TIMEOUT);
 $daemon->handle;
};

print "Service Shutting Down\n";
exit;
 
sub timed_out {
    die "TIME HAS RUN OUT!!!";
}
