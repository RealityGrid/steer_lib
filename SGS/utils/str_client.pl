#!/usr/bin/perl

#uncoment to get debug info
#use SOAP::Lite +trace => debug => sub {};
use SOAP::Lite;

$URL="http://vermont.mvc.mcc.ac.uk/Andrew/SGS.wsdl";
$service  = SOAP::Lite -> service( $URL );

$ans = $service->AppStart();

$ans = $service->SetServiceData("Supp_cmds", "STOP", "some_rubbish");

print "Set service data: ", $ans, "\n";

$ans = $service->Attach();

print "Supported cmds: ", $ans, "\n";

$ans = $service->PutControl("Some_cmds_here");

print "PutControl returned: ", $ans, "\n";

$ans = $service->PutStatus(@ARGV);
    
print "Set status: ", $ans, "\n";

$ans = $service->Detach();

print "Detach returned: ", $ans, "\n";

$ans = $service->AppStop();

print "AppStop returned: ", $ans, "\n";

#$ans = $service->AppDetach();

#print "AppDetach returned: ", $ans, "\n";

exit;
