/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Andrew Porter
 */

/** @file mainpage_doc.h
    @author Andrew Porter
    @author Robert Haines
    @brief Separate header file for doxygen documentation only
*/

/**
    @mainpage

    @section Contents

    @li @ref starting_sec @n
    @li @ref intro_sec @n
    @li @ref sec_background @n
    @li @ref sec_f90 @n
    @li @ref cmds_sec @n
    @li @ref sec_example @n
    @li @ref sec_IO @n
    @li @ref sec_ChkPt @n
    @li @ref sec_EnvVars @n
    @li @ref sec_Acks @n

    @section starting_sec Getting Started

    If you are impatient to actually use the steering library rather
    than just read about it then we recommend that you read at least
    the @ref intro_sec and @ref sec_background sections. After that,
    take a look at the source code of the simple.c example or one of
    the others from the @ref sec_example section.  The code generated
    by Doxygen includes links to the documentation for each of the
    steering-library routines used by this example.

    @section intro_sec Introduction

    A number of people contributed to the initial design of the
    RealityGrid steering API and thus were authors of the document
    that this Doxygen-generated documentation replaces.  They are:
    Robert Haines, Stephen Pickles, Robin Pinning, Andrew Porter,
    Graham Riley, Rupert Ford, Ken Mayes, David Snelling, Jim Stanton,
    Steven Kenny and Shantenu Jha.

    @latexonly
    \begin{figure}
    \centerline{\includegraphics[width=\textwidth]{steer_arch_symm.png}}
    \caption{Basic architecture for RealityGrid steering.  Each ellipse
    represents a distinct application, the location of which is not
    constrained.  The client can dynamically attach to the running
    application, perform some steering activity and then detach again. The
    application may also use the steering library to send data sets to
    some other component, {\it e.g}. a visualization package.}
    \end{figure}
    @endlatexonly

    <img src="steer_arch_symm.png" alt="Illustrative architecture diagram"
         title="Illustrative architecture of a steering scenario">

    The figure shows an example use case for computational steering.
    The simulation component has been instrumented using the steering
    API.  This enables it to receive control messages from the
    steering client and return status messages to it. In addition to
    interacting with the steering client, the simulation in the figure
    also uses the API to transfer (potentially large) data sets to a
    second component.  Although this second component is labelled
    'visualization,' it could in fact be any other type of component
    including a second form of simulation (as in a coupled-model
    scenario).

    The API has been designed with the assumption that the control and
    status links between a steering client (labelled 'Steering
    connection' in the figure) and simulation will involve the
    exchange of small quantities of data.  For cases where the
    steerable 'parameter' is a large array of data, it will best be
    treated as a form of 'sample data' with the steering client
    issuing instructions for it to be emitted and consumed as
    required.

    All routines will provide integer return values with a return
    value of REG_SUCCESS (zero) indicating success and a return value
    of REG_FAILURE (unity) indicating failure.  (These quantities are
    defined in ReG_Steer_types.h.)  Some routines use this return
    value to specify more detailed information on the nature of the
    success/failure - see individual interface specifications for
    details.

    @section sec_background Background Assumptions

    Application scientists have pre-existing codes (hereafter referred
    to as simulations), which are to be made steerable. These codes
    are written in Fortran, C, C++ or a mixture of these, and can be
    compiled using standards-compliant Fortran90 and C/C++
    compilers. These codes will in general be parallel, but we avoid
    assuming or prescribing any particular paradigm (e.g. message
    passing or shared memory) or harness (e.g. MPI, PVM, SHMEM).  As a
    consequence of this, the API presented here is designed to be
    called on a single thread only.  In a parallel code therefore, the
    application programmer assumes the responsibility of communicating
    any changes resulting from steering activity to the other
    threads/processes.

    We assume that the logical structure within the simulation is such
    that there exists a point (breakpoint) within a larger control
    loop at which it is feasible to insert new functionality intended
    to:
    @li accept a change to one or more of the parameters of the
    simulation (steerable parameters);
    @li emit a consistent representation of the current state of
    both the steerable parameters and other variables (monitored
    quantities);
    @li emit a consistent representation (provisionally called
    outsample) of part of the system being simulated that may be
    required by a downstream component (@e e.g. a visualisation system or
    another simulation).

    We also assume that it is feasible, at the same point in the control
    loop, to:
    @li output a consistent representation of the system (checkpoint)
    containing sufficient information to enable a subsequent restart of
    the simulation from its current state;
    @li (in the case that the steered component is itself downstream
    of another component), to accept a sample emitted by an upstream
    component (provisionally called insample).

    @section sec_f90 F90 bindings

    The API itself also provides F90 bindings for the simulation-side
    routines in ReG_Steer_Appside_f.c.  All of the routines making up
    the F90 bindings are implemented as F90 subroutines and hence
    their return value is passed back to the caller by an additional
    @p status argument (c.f. MPI).  Where an argument to an interface
    in the API is an array, the first element of the array should be
    supplied by the calling routine. The KIND parameters used here,
    REG_INT_KIND, REG_SP_KIND and REG_DP_KIND, are defined in
    reg_steer_f90.inc and correspond to KIND(1), KIND(1.0) and
    KIND(1.0D0), respectively.

    @section cmds_sec Pre-defined steering commands

    The API defines and uses four pre-defined commands: 'stop',
    'pause', 'resume' and 'detach'.  An application that is steering
    enabled using the API supports `detach' by default.  It is the
    responsibility of the application programmer to decide whether or
    not their application will support 'stop' and 'pause' and, if so,
    to implement this functionality.  (Note that the steering library
    will automatically handle the 'pause' command - the call to
    Steering_control() will block - if the user passes
    REG_STR_PAUSE_INTERNAL rather than REG_STR_PAUSE to
    Steering_initialize().) An application that supports 'pause' is
    automatically assumed to support `resume.'  The relevant constants
    are defined in ReG_Steer_types.h

    @section sec_example Example usage

    Included with the RealityGrid steering library are a number of
    example applications:

    @li simple.c provides a very simple example of the basic
    instrumentation required in order to make a simulation
    steerable.
    @li mini_steerer.c provides a fairly simple example of a command-line
    steering client.
    @li mini_app.c illustrates more of the library's functionality
    @li mini_app2.f90 similar to mini_app.c but using the F90 bindings
    @li mini_app_para.f90 An example of using the steering library in a
    parallel (MPI), F90 code
    @li sink.c Example of using steering library to receive data (@e e.g.
    for visualization)

    @section sec_IO IO Control

    The API contains a number of routines  intended to allow an
    application to emit and consume potentially large quantities of
    data ('Sample data').  An application may emit or consume various
    sorts of data set and we term each of these an 'IOType.'  The
    library is designed to cope with parallel applications that are
    unable to collect the complete data set to be emitted on a single
    processor.  Such a data set must therefore be emitted piece by
    piece - this corresponds to several calls to Emit_data_slice() in
    the steering library.  Multiple calls to this routine must also be
    made if emitting data of different types.
    @see Emit_start(), Emit_data_slice(), Emit_stop() @n Consume_start(),
    Consume_data_slice_header, Consume_data_slice(), Consume_stop()

    @subsection IO with the example codes

    The steering library offers file- and sockets-based
    implementations of IOTypes; which to use is set at compile
    time by editing the Makefile.include file. The file-based
    implementation writes and reads data from the directory specified
    by the REG_DATA_DIRECTORY environment variable or, if that is
    unset, the current working directory. In order to match emitter
    with consumer when using the file-based implementation, the labels
    given to the IOType at registration in both the emitter and
    consumer must be identical (since the label is used to generate
    the filenames used).

    With the library and examples compiled to use file-based IO
    (@e i.e. the REG_SOCKET_SAMPLES_FLAG and REG_PROXY_SAMPLES_FLAG flags
    in Makefile.include set to zero) and file-based steering, open two
    terminal windows. Ensure that REG_STEER_DIRECTORY is set to a
    different value in each terminal but that REG_DATA_DIRECTORY is
    set to the same value in both. (Alternatively, you could leave
    REG_DATA_DIRECTORY unset in both terminals and then run the two
    executables in the same directory.) Launch the mini_app binary in
    one terminal and the sink binary in the other. You should see
    messages indicating that mini_app is writing-out data while sink
    reads it in. If you don't, examine the messages produced by the
    library at start-up and check that the directories it is using are
    what you expect; in particular, that the directory being used for
    data IO (as opposed to steering) is the same for both mini_app and
    sink.

    The size of the data set being emitted by mini_app is set to 16
    cubed. This may be steered by launching a steering client (with
    REG_STEER_DIRECTORY set to match that given to mini_app) and
    adjusting the values of NX, NY and NZ. NX is constrained to be a
    multiple of four because of the way mini_app simulates the
    emission of decomposed data.

    Although file-based IO is attractive due to its simplicity, the
    use of sockets allows the code producing the data to be running on
    a separate machine from that consuming it. e.g. You may want to
    run a large simulation on a supercomputer at some remote site but
    visualize the results on another machine that has more suitable
    hardware/software for that task. To test this functionality, the
    library and examples must first be recompiled to use direct socket
    connections (set the REG_SOCKET_SAMPLES_FLAG flag in
    Makefile.include to 1 and do a 'make clean' followed by 'make' and
    'make install').

    Unlike file-based IO, the label assigned to the IO channel is no
    longer used to match emitter with consumer. Instead, the data
    source or destination consists of a port number and an IP
    address. The application (e.g. mini_app) that wishes to emit data
    on a given IO channel listens for connections on a particular
    port. Thus, the application that wishes to consume this data
    (e.g. sink) must be told the IP address of the machine on which
    the data source is running and the port on which it is listening.

    If file-based steering is being used then this information is
    provided to the steering library by way of the REG_CONNECTOR_PORT
    and REG_CONNECTOR_HOSTNAME environment variables. (Hence the
    library is limited to connecting to a single data source.) On
    start-up, the library prints which port each output IO channel is
    listening on. Therefore, as a first test, launch mini_app in the
    first terminal and cut-and-paste the port number it prints out
    into the REG_CONNECTOR_PORT variable in the second terminal. Set
    REG_CONNECTOR_HOSTNAME to the IP address of the machine on which
    mini_app is running (to avoid any possible firewall issues it is
    best to run both mini_app and sink on your local machine for this
    test) and then launch sink. Data should then be transferred from
    mini_app to sink, as before.

    The problem with the approach just outlined is that it requires
    that you study the output of the first application to discover
    which port it is listening on. However, the ports used by the
    steering library may be controlled by setting the
    GLOBUS_TCP_PORT_RANGE environment variable to
    e.g. 2500,2600. (Although the steering library does not use
    globus, it checks for this environment variable as it is often set
    on Grid resources that are protected by firewalls.) Then, the
    first listener to be created will be on port 2500 (provided that
    no other application on the machine is using that port), the
    second will be on 2501 and so on. Therefore, in the example given
    in the last paragraph, if we set GLOBUS_TCP_PORT_RANGE to
    2500,2600 in the first terminal then we should set
    REG_CONNECTOR_PORT to 2500 in the second. (Note that on machines
    running Linux, the command 'netstat -tlp' should allow you to see
    which applications are listening on which ports.)

    The data IO functionality of the library is at its most flexible
    when the web-services framework is being used for steering. In
    that case, the process of setting up a data connection between an
    application and visualization (say) is tackled during the launch
    of the visualization. Although it is no longer strictly necessary
    to set REG_CONNECTOR_PORT, REG_CONNECTOR_HOSTNAME or
    GLOBUS_TCP_PORT_RANGE, the latter may still be required if there
    are firewalls in place around the machines running the application
    and visualization. It is important to note that
    GLOBUS_TCP_PORT_RANGE also affects the ports used for the steering
    connections themselves and therefore, if it is set at all,
    GLOBUS_TCP_PORT_RANGE must contain several ports (ten or more).

    When the web-services framework is being used for steering, the
    library provides a third option for data IO - that of an indirect
    sockets connection via an "ioProxy". This is particularly useful
    when applications are running on machines that (for whatever
    reason) do not permit incoming socket connections. To use this
    option the steering library must be built with
    REG_SOCKET_SAMPLES_FLAG set to zero and REG_PROXY_SAMPLES_FLAG set
    to one (in Makefile.include). The web service representing each
    application must be initialised with the IP address and port of
    the ioProxy. That representing the data sink must also be
    initialised with the label of the data source (as used when
    registering the IOType). The command-line tools (reg_wsrf_tools
    in the RSS CVS repository) can be used to do this initialisation
    when creating a Steering Web Service.

    @section sec_ChkPt Checkpointing Control

    When requesting that the simulation perform a restart,
    Steering_control() returns the ChkType from which to restart and a
    ChkTag identifying which checkpoint set.  Currently, no information
    is supplied on where to find the checkpoint or on how the ChkTag
    should be used to construct the necessary filenames; this is all
    assumed to be carried out by the simulation.  This situation is not
    satisfactory but is difficult to resolve since it impinges on the
    more general area of checkpoint management.
    @see Register_ChkTypes(), Add_checkpoint_file(), Record_checkpoint_set()

    @section sec_EnvVars Environment Variables

    Some of the behaviour of the steering library, particularly on the
    application/simulation side can be configured using certain
    environment variables.  These can be particularly important when
    using socket-based IOTypes on machines with complex network
    configurations. Each of these environment variables
    is described below:

    @verbinclude Environment_variables.txt

    @section sec_Acks Acknowledgements

    This work was funded by the Engineering and Physical Sciences
    Research Council (http://www.epsrc.ac.uk) as part of the
    RealityGrid project (http://www.realitygrid.org) under grants
    GR/R67699/01, GR/R67699/02, GR/T27488/01, EP/C536452/1,
    EP/D500028/1 and EP/F00561X/1.  More documentation and the library
    itself are available from
    http://code.google.com/p/computational-steering/.

*/
