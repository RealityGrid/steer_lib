#!/usr/bin/perl

    use WSDL::Generator;

    my $init = {

            'schema_namesp' => 'http://vermont.mvc.mcc.ac.uk/WSDL/SGS.xsd',

            'services'      => 'SGS',

            'service_name'  => 'SGS',

            'target_namesp' => 'SGS',

            'documentation' => 'SGS SOAP Service.',

            'location'      => 'http://vermont.mvc.mcc.ac.uk:50005/'

    };

    my $w = WSDL::Generator->new($init);
    
    SGS->Attach();

    SGS->Detach();

    SGS->AppAttach();

    SGS->AppDetach();

    SGS->Pause();

    SGS->Resume();

    SGS->Stop();

    SGS->PutStatus("a_string");

    SGS->GetStatus();

    SGS->PutControl("a_string");

    SGS->GetControl();

    SGS->FindServiceData("sde_name");

    SGS->SetServiceData("sde_name", "sde_value");

    SGS->SetTerminationTime("a_string");

    SGS->GetNotifications();

    SGS->Destroy();

    print $w->get(SGS);
