#!/usr/bin/perl -w

#use SOAP::Lite +trace => debug => sub {};
use SOAP::Lite;


$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> SetTerminationTime("20")
       -> result;

print "Termination time set: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> SetServiceData("Supp_cmds","PAUSE")
       -> result;

print "SDE set: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> Detach()
       -> result;

print "Detach returned: \n$ans\n";

# Steerer attaches

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> Attach()
       -> result;

print "Attach returned: \n$ans\n";

# A 2nd steerer attempts to attach (should be rejected)

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> Attach()
       -> result;

print "2nd attach returned: \n$ans\n";

# Application registers some parameters etc.

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> SetServiceData("Param_defs","A_bit_of_xml_for_params")
       -> result;

print "SDE set: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> SetServiceData("IOType_defs","Some_IOType_definitions")
       -> result;

print "SDE set: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> SetServiceData("ChkType_defs","Some_ChkType_definitions")
       -> result;

print "SDE set: \n$ans\n";

# Application starts generating status messages

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> PutStatus("Burble")
       -> result;

print "Status set: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> PutStatus("moreBurble")
       -> result;

print "Status set: \n$ans\n";

# Steerer sends a bunch of control messages

for ($i = 0; $i < 12; $i++){

    $ans=  SOAP::Lite
	   -> uri('SGS')
	   -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
	   -> PutControl("herewiggo"."$i")
	   -> result;

    print "Control set: \n$ans\n";
}

# Attempt to set an SDE that doesn't exist

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> SetServiceData("rubbish", "STOP")
       -> result;

print "Service data set: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> FindServiceData("Supp_cmds")
       -> result;

print "Service data found: \n$ans\n";

# Get status messages back from SGS

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> GetStatus()
       -> result;

print "Status data found: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> GetStatus()
       -> result;

print "Status data found: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> GetStatus()
       -> result;

print "Status data found: \n$ans\n";

# Play with control messages

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> GetControl()
       -> result;

print "Control data found: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> PutControl("stopnow")
       -> result;

print "Control set: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> GetControl()
       -> result;

print "Control data found: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> GetNotifications()
       -> result;

print "Got notifications: \n$ans\n";

# Pause application

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> Pause()
       -> result;

print "Pause returned: \n$ans\n";

# Resume application

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> Resume()
       -> result;

print "Resume returned: \n$ans\n";

for ($i = 0; $i < 15; $i++){

    $ans=  SOAP::Lite
	-> uri('SGS')
	-> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
	-> GetControl()
	-> result;

    print "Control data found: \n$ans\n";
}

# Steerer requests detach...

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> Detach()
       -> result;

print "Detach returned: \n$ans\n";

# Application responds

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> AppDetach()
       -> result;

print "AppDetach returned: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> GetNotifications()
       -> result;

print "Got notifications: \n$ans\n";

$ans=  SOAP::Lite
       -> uri('SGS')
       -> proxy('http://vermont.mvc.mcc.ac.uk:50005/')
       -> Destroy()
       -> result;


