#!/usr/bin/perl 

#---------------------------------------------------------------------
#
#    This Perl module contains the methods and service data elements
#    of the Steering Grid Service.  This Grid Service exposes the
#    steering interface of a RealityGrid application in an OGSA-
#    compliant fashion. 
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

package SGS;

$numStatusMsg = 20;
$lastStatusMsg = -1;
@statusMsgArray = ();
@statusMsgArrayFlags = ();

for($i=0; $i < $numStatusMsg; $i++){
    # Set flag to show each buffer is empty
    $statusMsgArrayFlags[$i] = 0;
}

$numCtrlMsg = 20;
$lastCtrlMsg = -1;
@ctrlMsgArray = ();
@ctrlMsgArrayFlags = ();

for($i=0; $i < $numCtrlMsg; $i++){
    # Set flag to show each buffer is empty
    $ctrlMsgArrayFlags[$i] = 0;
}

# Store SDE names as variables to save typo's - these MUST MATCH
# those declared in ReG_Steer_Steerside_Soap.h
$suppCmdsSDE = "Supp_cmds";
$paramDefsSDE = "Param_defs";
$ioDefsSDE = "IOType_defs";
$chkDefsSDE = "ChkType_defs";
$appStatusSDE = "Application_status";
$steerStatusSDE = "Steerer_status";

# The names of my service data elements
@serviceDataElements = ($suppCmdsSDE, $paramDefsSDE, $ioDefsSDE, 
			$chkDefsSDE, $steerStatusSDE, 
			$appStatusSDE);

# Create hash table to actually hold SDEs
%serviceData = ("$serviceDataElements[0]", "", 
		"$serviceDataElements[1]", "", 
		"$serviceDataElements[2]", "",
		"$serviceDataElements[3]", "",
		"$serviceDataElements[4]", "",
		"$serviceDataElements[5]", "");

# Set initial states (no steering client attached & application not
# running - i.e. it hasn't contacted us yet)
# Possible values for steerer status are: 
#      DETACHED, ATTACHED, DETACHING
$serviceData{$steerStatusSDE} = "DETACHED";
# Possible values for application status are: 
#      NOT_STARTED, RUNNING, STOPPING, STOPPED, PAUSED
$serviceData{$appStatusSDE} = "NOT_STARTED";

# For storing notifications that need sending to steerer
$numNotifications = 0;

# Order that notifications are received in is important so
# we use this array to store that information
@notificationOrder = ();
for($i=0; $i < @{serviceDataElements}; $i++){
    $notificationOrder[$i] = 0;
}

@serviceDataChanged = ();
for($i=0; $i < @{serviceDataElements}; $i++){
    $serviceDataChanged[$i] = 0;
}

# Cludge to deal with case where application stops before
# acting on a steerer's detach command...
# Set this flag to 1 to signal that we are free to shutdown
# but only after waiting $delayBeforeDeath seconds.
$dieAfterDelay = 0;
$delayBeforeDeath = 30;

#---------------------------------------------------------

sub Attach {

    # Check that we're not already connected to a steerer
    # and that application is up and running
    if($SGS::serviceData{$steerStatusSDE} eq "DETACHED" &&
       $SGS::serviceData{$appStatusSDE} eq "RUNNING"){

        # Clean up any old data in our buffers
	for($i=0; $i < $SGS::numStatusMsg; $i++){
	    # Set flag to show each buffer is empty
	    $SGS::statusMsgArrayFlags[$i] = 0;
	    $SGS::statusMsgArray[$i] = "";
	}

	for($i=0; $i < $SGS::numCtrlMsg; $i++){
	    # Set flag to show each buffer is empty
	    $SGS::ctrlMsgArrayFlags[$i] = 0;
	    $SGS::ctrlMsgArray[$i] = "";
	}

	# Clean up all notifications
        $SGS::numNotifications = 0;
	for($i=0; $i < @{SGS::serviceDataElements}; $i++){
	    $SGS::serviceDataChanged[$i] = 0;
	}

        # Delete all data in SDEs apart from Supp_cmds
	$SGS::serviceData{$paramDefsSDE} = "";
	$SGS::serviceData{$ioDefsSDE} = "";
	$SGS::serviceData{$chkDefsSDE} = "";

        # Set steerer-status SDE to signal that a steerer is attached
	$SGS::serviceData{$steerStatusSDE} = "ATTACHED";

	# Return the supported commands
	return $SGS::serviceData{$suppCmdsSDE};

    } else {

        return "SGS_ERROR";
    }
}

#------------------------------

sub Detach {
    
    my $detachMsg = "<?xml version=\"1.0\"?>" . 
     "<ReG_steer_message xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " . 
     "xmlns=\"http://www.realitygrid.org/xml/steering\" " . 
     "xsi:SchemaLocation=\"http://www.realitygrid.org/xml/steering " . 
     "/home/zzcguap/reg_steer_lib/xml_schema/reg_steer_comm.xsd\">" . 
     "<Steer_control>" . 
     "<Command>" . 
     "<Cmd_name>DETACH</Cmd_name>" . 
     "</Command>" . 
     "</Steer_control>" . 
     "</ReG_steer_message>";

    print "Detach: Steerer_status = $SGS::serviceData{$steerStatusSDE}\n";
    print "Detach: Application_status = $SGS::serviceData{$appStatusSDE}\n";

    # This method can only be called by an attached steering client
    if($SGS::serviceData{$steerStatusSDE} eq "ATTACHED"){

	if($SGS::serviceData{$appStatusSDE} eq "STOPPED"){

	    # Record change of state
	    $SGS::serviceData{$steerStatusSDE} = "DETACHED";
	    
            # App. has stopped and steerer is detached so quit
	    Destroy();

	    return "SGS_SUCCESS";

	}
	elsif($SGS::serviceData{$appStatusSDE} eq "RUNNING"){

	    # Tell application to detach
	    PutControl(" ", "$detachMsg");

	    # Record change of state
	    $SGS::serviceData{$steerStatusSDE} = "DETACHING";

	    return "SGS_SUCCESS";
	}
    }
    return "SGS_ERROR";
}

#------------------------------
# Called by application when it starts up

sub AppStart {

    # Check that application not already attached
    if($SGS::serviceData{$SGS::appStatusSDE} eq "NOT_STARTED"){

	SetServiceData(" ", $SGS::appStatusSDE, "RUNNING");
	return "SGS_SUCCESS";

    } else {

        return "SGS_ERROR";
    }
}

#------------------------------
# Called by application to confirm or instigate a detach from the
# steering client

sub AppDetach {

    print "AppDetach: Steerer_status = $SGS::serviceData{$steerStatusSDE}\n";
    print "AppDetach: Application_status = $SGS::serviceData{$appStatusSDE}\n";

    if($SGS::serviceData{$SGS::steerStatusSDE} eq "DETACHING"){

	SetServiceData(" ", $SGS::steerStatusSDE, "DETACHED");	

        # If app. isn't running and no steerer is attached then it's
        # time we died
	if($SGS::serviceData{$SGS::appStatusSDE} ne "RUNNING"){

	    if($SGS::dieAfterDelay == 1){
		# In theory we are now free to shutdown but actually, since
		# app. has stopped while steerer is waiting for confirmation
		# of a detach, we have to allow the steerer some time
		# to get the confirmation (which we have just generated by
		# changing the steerer status to DETACHED).
                print "Service will shutdown in $SGS::delayBeforeDeath " .
		      "seconds\n";
		SetTerminationTime(" ", $SGS::delayBeforeDeath);
	    }
	    else{
		Destroy();
	    }
	}

	return "SGS_SUCCESS";
    } else {

        return "SGS_ERROR";
    }
}

#------------------------------
# Called by application to confirm or notify steering client
# that it has stopped

sub AppStop {

    print "AppStop: Steerer_status = $SGS::serviceData{$steerStatusSDE}\n";
    print "AppStop: Application_status = $SGS::serviceData{$appStatusSDE}\n";

    if($SGS::serviceData{$SGS::appStatusSDE} eq "NOT_STARTED" ||
       $SGS::serviceData{$SGS::appStatusSDE} eq "PAUSED"){

	return "SGS_ERROR";
    }

    SetServiceData(" ", $SGS::appStatusSDE, "STOPPED");

    # If no steerer is attached then we're now free to die
    if($SGS::serviceData{$SGS::steerStatusSDE} eq "DETACHED"){
	print "AppStop: calling Destroy\n";
	Destroy();
    }
    elsif($SGS::serviceData{$SGS::steerStatusSDE} eq "DETACHING"){

        # Steerer is in process of detaching but application has finished
        # - call AppDetach ourselves to complete the detach process
        # Set flag to say we are free to die after allowing some delay to 
        # give steerer time to get confirmation of Detach.
        $SGS::dieAfterDelay = 1;
	AppDetach();
    }

    return "SGS_SUCCESS";
}

#------------------------------

sub GetStatus {

    # Search for next buffer to return, starting from the last one 
    # we returned
    for($i=($SGS::lastStatusMsg+1); $i < $SGS::numStatusMsg; $i++){
	if($SGS::statusMsgArrayFlags[$i] == 1){
	    $SGS::lastStatusMsg = $i;
	    $SGS::statusMsgArrayFlags[$i] = 0;
	    return "$SGS::statusMsgArray[$i]";
	}
    }
    for($i=0; $i <= $SGS::lastStatusMsg; $i++){
	if($SGS::statusMsgArrayFlags[$i] == 1){
	    $SGS::lastStatusMsg = $i;
	    $SGS::statusMsgArrayFlags[$i] = 0;
	    return "$SGS::statusMsgArray[$i]";
	}
    }

    return "";     
  }

#------------------------------

sub PutStatus {
    # Pull out first two parameters given in call
    my($class,$NEW_STATE) = @_;

    # Check that we have a free buffer to store message in
    for($i=0; $i < $SGS::numStatusMsg; $i++){

	if($SGS::statusMsgArrayFlags[$i] == 0){

	    print "PutStatus: putting msg ", $NEW_STATE, " in buffer ", 
	          $i, "\n";

            # Found one - store msg and set flag on buffer
	    $SGS::statusMsgArray[$i] = $NEW_STATE;
	    $SGS::statusMsgArrayFlags[$i] = 1;
	    return $NEW_STATE;
	}
    }

    # No free buffer found
    return "";
  }

#------------------------------

sub GetControl {

    print "\n" . "GetControl: last ctrl msg was $SGS::lastCtrlMsg" . "\n";

    # Search for next buffer to return, starting from the last one 
    # we returned
    for($i=($SGS::lastCtrlMsg+1); $i < $SGS::numCtrlMsg; $i++){
	if($SGS::ctrlMsgArrayFlags[$i] == 1){
	    $SGS::lastCtrlMsg = $i;
	    $SGS::ctrlMsgArrayFlags[$i] = 0;
	    print "GetControl: returning buffer $i";
	    return "$SGS::ctrlMsgArray[$i]";
	}
    }
    for($i=0; $i <= $SGS::lastCtrlMsg; $i++){
	if($SGS::ctrlMsgArrayFlags[$i] == 1){
	    $SGS::lastCtrlMsg = $i;
	    $SGS::ctrlMsgArrayFlags[$i] = 0;
	    print "GetControl: returning buffer $i";
	    return "$SGS::ctrlMsgArray[$i]";
	}
    }

    return "";     
  }

#------------------------------

sub PutControl {
    my($class,$NEW_STATE) = @_;

    # Check that we have a free buffer to store message in
    for($i=0; $i < $SGS::numCtrlMsg; $i++){

	if($SGS::ctrlMsgArrayFlags[$i] == 0){

	    print "PutControl: putting msg ", $NEW_STATE, " in buffer ", 
	          $i, "\n";

            # Found one - store msg and set flag on buffer
	    $SGS::ctrlMsgArray[$i] = $NEW_STATE;
	    $SGS::ctrlMsgArrayFlags[$i] = 1;
	    return "SGS_SUCCESS";
	}
    }

    # No free buffer found but that's not really an error
    return " ";
  }

#------------------------------

sub FindServiceData {
    my $element_value = "";

    my($class,$element_name) = @_;
    print "FindServiceData: name = ", $element_name, "\n";

    # Check that we know about this service data
    for ($i=0; $i<@{SGS::serviceDataElements}; $i++) {  

	if($SGS::serviceDataElements[$i] eq  $element_name){
	    $element_value = $SGS::serviceData{"$element_name"};

            # Unset any pending notification on this SDE
	    if($SGS::serviceDataChanged[$i] == 1){

		# Re-jig the ordered list of notifications
		for ($j=0; $j<@{SGS::serviceDataElements}; $j++) {  
		    if($SGS::notificationOrder[$j] == $i){

                        # Found the one we're returning - delete it and 
                        # shuffle up remaining notifications to close gap
			for ($k=$j; $k<($SGS::numNotifications-$j-1); $k++) {  

			    $SGS::notificationOrder[$k] = $SGS::notificationOrder[$k+1];
			}
			last;
		    }
		}
		$SGS::serviceDataChanged[$i] = 0;
		$SGS::numNotifications--;
	    }
	}
    }

    print "FindServiceData: value = ", $element_value, "\n";
    return "$element_value";
  }

#------------------------------

sub SetServiceData {

    my $return_val = "";
    my($class, $element_name, $element_value) = @_;

    # Check that we know about this service data
    for ($i=0; $i<@{SGS::serviceDataElements}; $i++) {  

	if($SGS::serviceDataElements[$i] eq  $element_name){

            # We do - set new value and return it too
	    $SGS::serviceData{"$element_name"} = "$element_value";
	    $return_val = $SGS::serviceData{"$element_name"};

            # Set notification flag & increment count - we don't generate
	    # notifications for supported commands because these can only
	    # be set once and are returned to the steering client as
	    # part of the attaching procedure
	    if($element_name ne $SGS::suppCmdsSDE){
		# Can only have as many notifications as SDEs so check that we 
		# haven't already got a notification for this SDE
		if($SGS::numNotifications < @{SGS::serviceDataElements}){

		    if($SGS::serviceDataChanged[$i] != 1){
			$SGS::serviceDataChanged[$i] = 1;
			$SGS::notificationOrder[$SGS::numNotifications] = $i;
			$SGS::numNotifications++;
		    }
		}
	    }
            print "SetServiceData: setting ",$element_name, " = ", 
                  $element_value, "\n";
	    return "$return_val";
	}
    }

    return "$return_val";
  }

#------------------------------

sub SetTerminationTime{
     local($class,$NewTimeToDie) = @_;
     $TimeLeft = alarm(100000000);
     $ans="";
     if ( $NewTimeToDie eq "" )
     {
        alarm($TimeLeft);
	$ans=$TimeLeft;
     }else{
        alarm($NewTimeToDie);
	$ans=$NewTimeToDie;
     }	
     return $ans;
  }

#------------------------------

sub GetNotifications{

    my $return_val = "";

    print "GetNotifications: have ", $SGS::numNotifications, " notifications\n";
    # Loop over no. of serviceDataElements to make sure we get everything
    for($i=0; $i<@{serviceDataElements}; $i++){

	$index = $SGS::notificationOrder[$i];
	if($SGS::serviceDataChanged[$index] == 1){
	    
	    $return_val = $return_val . "$serviceDataElements[$index]" . " ";
	    $SGS::serviceDataChanged[$index] = 0;
	}
    }

    # Reset notifications - ASSUMES only one notification sink
    $SGS::numNotifications = 0;

    return $return_val;
  }

#------------------------------

sub Pause{
    my $pauseMsg = "<?xml version=\"1.0\"?>" . 
     "<ReG_steer_message xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " . 
     "xmlns=\"http://www.realitygrid.org/xml/steering\" " . 
     " xsi:SchemaLocation=\"http://www.realitygrid.org/xml/steering " . 
     "/home/zzcguap/reg_steer_lib/xml_schema/reg_steer_comm.xsd\">" . 
     "<Steer_control>" . 
     "<Command>" . 
     "<Cmd_name>PAUSE</Cmd_name>" . 
     "</Command>" . 
     "</Steer_control>" . 
     "</ReG_steer_message>";

    # Check that we are connected to a steerer
    if($SGS::serviceData{$SGS::steerStatusSDE} eq "ATTACHED"){

	if($SGS::serviceData{$SGS::appStatusSDE} eq "RUNNING"){
	    # Tell application to pause
	    PutControl(" ", "$pauseMsg");

	    # Record change of state
	    $SGS::serviceData{$SGS::appStatusSDE} = "PAUSED";

	    return "SGS_SUCCESS";
	}
    }
    return "SGS_ERROR";
}

#------------------------------

sub Resume{
    my $resumeMsg = "<?xml version=\"1.0\"?>" . 
     "<ReG_steer_message xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " . 
     "xmlns=\"http://www.realitygrid.org/xml/steering\" " . 
     "xsi:SchemaLocation=\"http://www.realitygrid.org/xml/steering " . 
     "/home/zzcguap/reg_steer_lib/xml_schema/reg_steer_comm.xsd\">" . 
     "<Steer_control>" . 
     "<Command>" . 
     "<Cmd_name>RESUME</Cmd_name>" . 
     "</Command>" . 
     "</Steer_control>" . 
     "</ReG_steer_message>";

    # Check that we are connected to a steerer
    if($SGS::serviceData{$SGS::steerStatusSDE} eq "ATTACHED"){

	if($SGS::serviceData{$SGS::appStatusSDE} eq "PAUSED"){
	    # Tell application to resume
	    PutControl(" ", "$resumeMsg");

	    # Record change of state
	    $SGS::serviceData{$SGS::appStatusSDE} = "RUNNING";

	    return "SGS_SUCCESS";
	}
    }
    return "SGS_ERROR";
}

#------------------------------

sub Stop{
    my $stopMsg = "<?xml version=\"1.0\"?>" . 
     "<ReG_steer_message xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " . 
     "xmlns=\"http://www.realitygrid.org/xml/steering\" " . 
     "xsi:SchemaLocation=\"http://www.realitygrid.org/xml/steering " . 
     "/home/zzcguap/reg_steer_lib/xml_schema/reg_steer_comm.xsd\">" . 
     "<Steer_control>" . 
     "<Command>" . 
     "<Cmd_name>STOP</Cmd_name>" . 
     "</Command>" . 
     "</Steer_control>" . 
     "</ReG_steer_message>";

    $status = $SGS::serviceData{$SGS::steerStatusSDE};
    print "Stop: Steerer_status = $status"."\n";
    $status = $SGS::serviceData{$SGS::appStatusSDE};
    print "Stop: Application_status = $status"."\n";

    # Check that we are connected to a steerer
    if($SGS::serviceData{$SGS::steerStatusSDE} eq "ATTACHED"){

	if($SGS::serviceData{$SGS::appStatusSDE} eq "RUNNING" ||
	   $SGS::serviceData{$SGS::appStatusSDE} eq "PAUSED"){

	    # Tell application to stop
	    $ans = PutControl(" ", "$stopMsg");

	    # Record change of state
	    $SGS::serviceData{$SGS::appStatusSDE} = "STOPPING";

	    return "SGS_SUCCESS";
	}
    }

    return "SGS_ERROR";
}

#------------------------------

sub Destroy {

# Destroy does just that - I don't think it's supposed to be careful
# about the consequences and thus it isn't...
    print "Destroy: about to trigger alarm\n";
    alarm 1;
    return;
}

#------------------------------

# Perl module has to return 1 (== true)
1;
