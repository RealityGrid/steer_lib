#!/usr/bin/perl -w

use SOAP::Lite;

# Script takes address of SGS as command line argument and
# calls its Destroy method.

if(($#ARGV + 1) != 1){

    print "Usage: destroy_SGS.pl <address_of_SGS>\n";
    exit;
}

$address = $ARGV[0];

SOAP::Lite
    -> uri('SGS')
    -> proxy($address)
    -> Destroy()
    -> result;

