#!/usr/bin/perl

#uncoment to get debug info
use SOAP::Lite +trace => debug => sub {};
#use SOAP::Lite;

$URL="http://vermont.mvc.mcc.ac.uk/Andrew/SGS.wsdl";
#$URL="http://vermont.mvc.mcc.ac.uk/WSDL/MiM.wsdl";
$service  = SOAP::Lite -> service( $URL );

$ans = $service->PutStatus(@ARGV);
    
print "Set status: ", $ans, "\n";

$ans = $service->SetServiceData("Supp_cmds", "STOP", "some_rubbish");

print "Set service data: ", $ans, "\n";

$ans = $service->FindServiceData("Supp_cmds");

print "Get service data: ", $ans, "\n";

exit;
