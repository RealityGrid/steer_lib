!----------------------------------------------------------------------
!    Example of a basic steerable parallel F90 application that uses 
!    the RealityGrid steering library.
!
!    (C)Copyright 2002 The University of Manchester, United Kingdom,
!    all rights reserved.
!
!    This software is produced by the Supercomputing, Visualization &
!    e-Science Group, Manchester Computing, the Victoria University of
!    Manchester as part of the RealityGrid project.
!
!    This software has been tested with care but is not guaranteed for
!    any particular purpose. Neither the copyright holder, nor the
!    University of Manchester offer any warranties or representations,
!    nor do they accept any liabilities with respect to this software.
!
!    This software must not be used for commercial gain without the
!    written permission of the authors.
!    
!    This software must not be redistributed without the written
!    permission of the authors.
!
!    Permission is granted to modify this software, provided any
!    modifications are made freely available to the original authors.
! 
!    Supercomputing, Visualization & e-Science Group
!    Manchester Computing
!    University of Manchester
!    Manchester M13 9PL
!    
!    WWW:    http://www.sve.man.ac.uk  
!    email:  sve@man.ac.uk
!    Tel:    +44 161 275 6095
!    Fax:    +44 161 275 6800    
!
!    Initial version by:  Andrew Porter
!----------------------------------------------------------------------

PROGRAM para_mini_app
  IMPLICIT none

  INCLUDE 'reg_steer_f90.inc'
  INCLUDE 'mpif.h'

  INTEGER (KIND=REG_SP_KIND) :: status

  ! For supported commands
  INTEGER (KIND=REG_SP_KIND) :: num_cmds
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_INITIAL_NUM_CMDS)    :: commands

  ! For IO types
  INTEGER (KIND=REG_SP_KIND)                                     :: num_types
  CHARACTER(LEN=40), DIMENSION(REG_INITIAL_NUM_IOTYPES)          :: io_labels
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_INITIAL_NUM_IOTYPES) :: iotype_handles
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_INITIAL_NUM_IOTYPES) :: io_dirn

  ! For Chk types
  INTEGER (KIND=REG_SP_KIND)                                  :: num_chk_types
  CHARACTER(LEN=40), DIMENSION(REG_INITIAL_NUM_IOTYPES)       :: chk_labels
  INTEGER (KIND=REG_SP_KIND), &
                           DIMENSION(REG_INITIAL_NUM_IOTYPES) :: chk_handles
  INTEGER (KIND=REG_SP_KIND), &
                           DIMENSION(REG_INITIAL_NUM_IOTYPES) :: chk_dirn

  INTEGER (KIND=REG_SP_KIND) :: input_freq  = 0
  INTEGER (KIND=REG_SP_KIND) :: output_freq = 5

  ! For parameters
  CHARACTER(LEN=40), DIMENSION(REG_MAX_NUM_STR_PARAMS) :: param_labels
  INTEGER (KIND=REG_SP_KIND)                           :: param_type
  INTEGER (KIND=REG_SP_KIND)                           :: param_strbl

  ! Parameters for steering
  INTEGER (KIND=REG_SP_KIND)           :: dum_int, dum_int2
  REAL (KIND=REG_SP_KIND)              :: dum_real, dum_real2
  REAL (KIND=REG_DP_KIND)              :: dum_dbl
  CHARACTER(LEN=REG_MAX_STRING_LENGTH) :: dum_str
  INTEGER (KIND=REG_SP_KIND)           :: str_len

  ! Ensure that we have enough storage for a ptr returned from C - needs
  ! to be large enough to store a C 'long' on whatever architecture is
  ! being used.
  INTEGER (KIND=REG_DP_KIND) :: ptr

  INTEGER (KIND=REG_SP_KIND) :: num_recvd_cmds
  INTEGER (KIND=REG_SP_KIND) :: num_params_changed
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_MAX_NUM_STR_CMDS) :: recvd_cmds
  CHARACTER(LEN=REG_MAX_STRING_LENGTH), &
              DIMENSION(REG_MAX_NUM_STR_CMDS)   :: recvd_cmd_params
  CHARACTER(LEN=REG_MAX_STRING_LENGTH), &
              DIMENSION(REG_MAX_NUM_STR_PARAMS) :: changed_param_labels

  INTEGER (KIND=4) :: iloop, icmd, iparam
  INTEGER (KIND=4) :: finished = 0
  INTEGER (KIND=4) :: parse_finished = 0

  ! MPI-related variables
  INTEGER (KIND=4) :: my_id
  INTEGER (KIND=4) :: IERROR

  CALL MPI_INIT(IERROR)

  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, IERROR)

! Initialise dummy steered/monitored variables on all processes
  dum_int2  = 123
  dum_dbl   = 123.123d0
  dum_real2 = -23.456
  dum_real  = 123.45
  dum_int   = 5

  dum_str = "hello"//CHAR(0)

  ! Enable steering - we only ever call the steering library
  ! from the master px
  IF(my_id .eq. 0)THEN

    CALL steering_enable_f(reg_true)
  END IF

  ! Specify which commands we support and initialise the library
  num_cmds = 2
  commands(1) = REG_STR_STOP
  commands(2) = REG_STR_PAUSE

  IF(my_id .eq. 0)THEN
    CALL steering_initialize_f(num_cmds, commands, status)

    IF(status .ne. REG_SUCCESS)THEN

      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Call to steering_initialize_f failed!'
    END IF
  END IF

  ! Register the input IO channel

  num_types    = 1
  io_labels(1) = "VTK_STRUCTURED_POINTS_INPUT"
  io_dirn(1)   = REG_IO_IN
    
  IF(my_id .eq. 0)THEN
    CALL register_iotypes_f(num_types, io_labels, io_dirn, &
                            input_freq, iotype_handles(1), status)
  
    IF(status .ne. REG_SUCCESS)THEN
  
      CALL steering_finalize_f(status)
      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Failed to register IO type'
    END IF
  
    WRITE(*,*) 'Returned IOtype = ', iotype_handles(1)
  END IF

  io_labels(1) = "SOME_OUTPUT"
  io_dirn(1)   = REG_IO_OUT
    
  IF(my_id .eq. 0)THEN
    CALL register_iotypes_f(num_types, io_labels, io_dirn, &
                            output_freq, iotype_handles(2), status)
  
    IF(status .ne. REG_SUCCESS)THEN
  
      CALL steering_finalize_f(status)
      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Failed to register IO type'
    END IF
  
    WRITE(*,*) 'Returned IOtype = ', iotype_handles(2)
  END IF

  num_chk_types = 1
  chk_labels(1) = "SOME_CHECKPOINT"
  chk_dirn(1)   = REG_IO_INOUT
  
  IF(my_id .eq. 0)THEN
    CALL register_chktypes_f(num_chk_types, chk_labels, chk_dirn, &
                             output_freq, chk_handles(1), status)

    IF(status .ne. REG_SUCCESS)THEN
  
      CALL steering_finalize_f(status)
      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Failed to register IO type'
    END IF
  
    WRITE(*,*) 'Returned IOtype = ', iotype_handles(3)
  END IF

  ! Register some parameters
  
  param_labels(1) = "test_integer"
  param_type  = REG_INT
  param_strbl = reg_true
  
  IF(my_id .eq. 0)THEN
     ! This variable restricted to 0 <= dum_int <= 100
    CALL register_param_f(param_labels(1), param_strbl, dum_int, &
                          param_type, "0", "100", status)
  END IF

  ! Registration uses address of variable so use second 'dum_int' here
  ! rather than simply changing value of first one
  param_labels(2) = "2nd_test_integer"
  param_strbl = reg_false
  
  IF(my_id .eq. 0)THEN
    CALL register_param_f(param_labels(2), param_strbl, dum_int2, &
                          param_type, "-50", "50", status)
  END IF

  param_labels(3) = "test_real"
  param_type  = REG_FLOAT
  param_strbl = reg_false
  
  IF(my_id .eq. 0)THEN
    CALL register_param_f(param_labels(3), param_strbl, dum_real, &
                          param_type, "-200.0", "200.0", status)
  END IF

  param_labels(4) = "2nd_test_real"
  param_type  = REG_FLOAT
  param_strbl = reg_true
    
  IF(my_id .eq. 0)THEN
    CALL register_param_f(param_labels(4), param_strbl, dum_real2, &
                          param_type, "-200.0", "200.0", status)
  END IF

  param_labels(5) = "test_string"
  param_type  = REG_CHAR
  param_strbl = reg_true
    
  IF(my_id .eq. 0)THEN
    ! Getting a pointer to a string from F90 is a bit tricky so use
    ! the following string-specific function to register a string for
    ! steering/monitoring  
    CALL register_string_param_f(param_labels(5), param_strbl, dum_str, status)
  END IF

  param_labels(6) = "test_double"
  param_type  = REG_DBL
  param_strbl = reg_true
  
  IF(my_id .eq. 0)THEN
    CALL register_param_f(param_labels(6), param_strbl, dum_dbl, &
                          param_type, "50.0d0", "150.0d0", status)
  
  
    IF(status .ne. REG_SUCCESS)THEN
  
      CALL steering_finalize_f(status)
      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Failed to register IO params'
    END IF

    ! Let workers know that initialisation was successful
    CALL MPI_BCAST(status, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)

  ELSE

    ! Check that initialisation was successful
    CALL MPI_BCAST(status, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)

    IF(status .eq. REG_FAILURE)THEN

      CALL MPI_FINALIZE(IERROR)
      WRITE (*,*) 'PX ',my_id,' stopping.'
      STOP
    END IF

  END IF ! my_id == 0

  ! Enter main 'simulation' loop

  WRITE(*,*) 'PX ', my_id,' entering main simulation loop...'

  iloop = 0
  DO WHILE(iloop<50 .AND. (finished .ne. 1))

    CALL MPI_BARRIER(MPI_COMM_WORLD, IERROR)

    ! Steering is all done on master px only...
    IF(my_id .eq. 0)THEN

      WRITE (*,*) 'Starting loop ',iloop

      CALL steering_control_f(iloop, num_params_changed, changed_param_labels,&
                              num_recvd_cmds, recvd_cmds, recvd_cmd_params, &

                              status)

      CALL MPI_BCAST(status, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)

      IF(status .ne. REG_SUCCESS)THEN
         WRITE (*,*) 'steering_control returned error'
         CYCLE
      END IF

      WRITE(*,*) 'Received ',num_recvd_cmds, 'commands and ', &
                 num_params_changed, 'params'

      ! Notify other px's of (possibly altered) parameter values
      CALL MPI_BCAST(num_params_changed, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, &
                     IERROR)

      DO iparam=1, num_params_changed, 1

        CALL MPI_BCAST(changed_param_labels(iparam), REG_MAX_STRING_LENGTH, &
                       MPI_CHARACTER, 0, MPI_COMM_WORLD, IERROR)

        ! It would be simpler to just broadcast all of the steerable parameters,
        ! irrespective of whether they've been edited...
        IF(changed_param_labels(iparam) == param_labels(1))THEN
          CALL MPI_BCAST(dum_int, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
        ELSE IF(changed_param_labels(iparam) == param_labels(4))THEN
          CALL MPI_BCAST(dum_real2, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERROR)
        ELSE IF(changed_param_labels(iparam) == param_labels(5))THEN
          str_len = LEN_TRIM(dum_str)
          CALL MPI_BCAST(str_len, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
          CALL MPI_BCAST(dum_str, str_len, MPI_CHARACTER, 0, MPI_COMM_WORLD, IERROR)
        ELSE IF(changed_param_labels(iparam) == param_labels(6))THEN
          CALL MPI_BCAST(dum_dbl, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, &
                         IERROR)
        END IF
  
        WRITE(*,FMT="(A, I2, A, I2, A, A)") 'Px ', my_id, ' changed param no. ', &
                                            iparam,' = ', &
                                            changed_param_labels(iparam)
      END DO

      IF(num_params_changed > 0)THEN
        WRITE(*,*) 'Px ', my_id,' test_integer = ', dum_int
        WRITE(*,*) 'Px ', my_id,' 2nd_test_real = ', dum_real2
        WRITE(*,*) 'Px ', my_id,' test_string = ', TRIM(dum_str)
        WRITE(*,*) 'Px ', my_id,' test_double = ', dum_dbl
      END IF

      ! Inform worker px's how many cmds were received
      CALL MPI_BCAST(num_recvd_cmds, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, &
                     IERROR)

      ! Process any commands sent by the steerer
  
      IF(num_recvd_cmds > 0)THEN
  
        icmd = 1
        DO
  
          WRITE (*,FMT='(A, I4, A, A)') "Received cmd: ", recvd_cmds(icmd),&
                                        " with params: ", &
                                        TRIM(recvd_cmd_params(icmd))

          SELECT CASE (recvd_cmds(icmd))
  
          CASE(REG_STR_PAUSE)
            WRITE (*,*) 'Received pause command from steerer'
  
            CALL steering_pause_f(num_params_changed, changed_param_labels, &
                                 num_recvd_cmds, recvd_cmds, &
                                 recvd_cmd_params, status)
  
            ! Worker processes are blocked on a BCAST and thus have 
            ! effectively been paused too
            CALL MPI_BCAST(REG_STR_PAUSE, 1, MPI_INTEGER, 0, &
                           MPI_COMM_WORLD, IERROR)

            IF(status .ne. REG_SUCCESS)THEN
  
              WRITE(*,*) 'steering_pause_f returned error'
              finished = 1
            ELSE
  
              WRITE(*,*) 'steering_pause_f: ',num_recvd_cmds,' cmds and ', &
                         num_params_changed, ' parameters'

              ! Notify other px's of (possibly altered) parameter values
              CALL MPI_BCAST(num_params_changed, 1, MPI_INTEGER, 0, &
                             MPI_COMM_WORLD, IERROR)

              DO iparam=1, num_params_changed, 1

                 CALL MPI_BCAST(changed_param_labels(iparam), REG_MAX_STRING_LENGTH, &
                                MPI_CHARACTER, 0, MPI_COMM_WORLD, IERROR)

                IF(changed_param_labels(iparam) == param_labels(1))THEN
                  CALL MPI_BCAST(dum_int, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
                ELSE IF(changed_param_labels(iparam) == param_labels(4))THEN
                  CALL MPI_BCAST(dum_real2, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERROR)
                ELSE IF(changed_param_labels(iparam) == param_labels(5))THEN
                  str_len = LEN_TRIM(dum_str)
                  CALL MPI_BCAST(str_len, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
                  CALL MPI_BCAST(dum_str, str_len, MPI_CHARACTER, 0, MPI_COMM_WORLD, &
                                 IERROR)
                ELSE IF(changed_param_labels(iparam) == param_labels(5))THEN
                  CALL MPI_BCAST(dum_dbl, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, &
                                 IERROR)
                END IF
  
                WRITE(*,*) 'Px ', my_id, ' changed param no. ', iparam,' = ', &
                           changed_param_labels(iparam)
              END DO

              IF(num_params_changed > 0)THEN           
                WRITE(*,*) 'Px ', my_id,' test_integer = ', dum_int
                WRITE(*,*) 'Px ', my_id,' 2nd_test_real = ', dum_real2
                WRITE(*,*) 'Px ', my_id,' test_string = ', TRIM(dum_str)
                WRITE(*,*) 'Px ', my_id,' test_double = ', dum_dbl
              END IF

              ! Reset loop counter so loop over new set of commands
              ! received
              icmd = 0
            END IF
  
          CASE(REG_STR_STOP)

            CALL MPI_BCAST(REG_STR_STOP, 1, MPI_INTEGER, 0, &
                             MPI_COMM_WORLD, IERROR)
            WRITE (*,*) 'Px ', my_id, ' received stop command from steerer'
            finished = 1           
  
          CASE DEFAULT

            ! Worker px always expects to be told what the command is, even
            ! though it doesn't always do anything with it...
            CALL MPI_BCAST(recvd_cmds(icmd), 1, MPI_INTEGER, 0, &
                           MPI_COMM_WORLD, IERROR)

          END SELECT
  
          icmd = icmd + 1
  
          IF ((icmd .gt. num_recvd_cmds) .OR. (finished .eq. 1)) THEN

            CALL MPI_BCAST(1, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, &
                           IERROR)
            EXIT
          ELSE
            CALL MPI_BCAST(finished, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, &
                           IERROR)              
          END IF
        END DO
     
      END IF ! num_recvd_cmds > 0

    ELSE ! We are a worker px...

      CALL MPI_BCAST(status, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)

      IF(status .ne. REG_SUCCESS)THEN
         WRITE (*,*) 'Px ', my_id, 'got error status'
         CYCLE
      END IF

      ! Worker px's receive steering info from master...
      CALL MPI_BCAST(num_params_changed, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, &
                     IERROR)

      WRITE (*,*) 'Px ', my_id, ' num_params_changed = ', num_params_changed

      DO iparam=1, num_params_changed, 1

        ! Find out which parameter has been changed
        CALL MPI_BCAST(changed_param_labels(iparam), REG_MAX_STRING_LENGTH, &
                       MPI_CHARACTER, 0, MPI_COMM_WORLD, IERROR)

        IF(changed_param_labels(iparam) == param_labels(1))THEN
          CALL MPI_BCAST(dum_int, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
        ELSE IF(changed_param_labels(iparam) == param_labels(4))THEN
          CALL MPI_BCAST(dum_real2, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERROR)
        ELSE IF(changed_param_labels(iparam) == param_labels(5))THEN
          str_len = LEN_TRIM(dum_str)
          CALL MPI_BCAST(str_len, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
          CALL MPI_BCAST(dum_str, str_len, MPI_CHARACTER, 0, MPI_COMM_WORLD, &
                         IERROR)
        ELSE IF(changed_param_labels(iparam) == param_labels(6))THEN
          CALL MPI_BCAST(dum_dbl, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, &
                         IERROR)
        END IF
  
        WRITE(*,*) 'Px ', my_id, ' changed param no. ', iparam,' = ', &
                   TRIM(changed_param_labels(iparam))
      END DO

      IF(num_params_changed > 0)THEN    
        WRITE(*,*) 'Px ', my_id,' test_integer = ', dum_int
        WRITE(*,*) 'Px ', my_id,' 2nd_test_real = ', dum_real2
        WRITE(*,*) 'Px ', my_id,' test_string = ', TRIM(dum_str)
        WRITE(*,*) 'Px ', my_id,' test_double = ', dum_dbl
      END IF

      CALL MPI_BCAST(num_recvd_cmds, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      
      WRITE (*,*) 'Px ', my_id, ' num_recvd_cmds = ', num_recvd_cmds

      IF(num_recvd_cmds > 0)THEN
      
        parse_finished = 0
        DO WHILE(parse_finished .ne. 1)

          CALL MPI_BCAST(icmd, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)

          IF(icmd .eq. REG_STR_STOP)THEN

            finished = 1
          ELSE IF(icmd .eq. REG_STR_PAUSE)THEN

            CALL MPI_BCAST(num_params_changed, 1, MPI_INTEGER, 0, &
                           MPI_COMM_WORLD, IERROR)

            DO iparam=1, num_params_changed, 1

              ! Find out which parameter has been changed
              CALL MPI_BCAST(changed_param_labels(iparam), REG_MAX_STRING_LENGTH, &
                             MPI_CHARACTER, 0, MPI_COMM_WORLD, IERROR)

              IF(changed_param_labels(iparam) == param_labels(1))THEN
                CALL MPI_BCAST(dum_int, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
              ELSE IF(changed_param_labels(iparam) == param_labels(4))THEN
                CALL MPI_BCAST(dum_real2, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERROR)
              ELSE IF(changed_param_labels(iparam) == param_labels(5))THEN
                str_len = LEN_TRIM(dum_str)
                CALL MPI_BCAST(str_len, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
                CALL MPI_BCAST(dum_str, str_len, MPI_CHARACTER, 0, MPI_COMM_WORLD, &
                               IERROR)
              ELSE IF(changed_param_labels(iparam) == param_labels(6))THEN
                CALL MPI_BCAST(dum_dbl, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, &
                               IERROR)
              END IF
  
              WRITE(*,*) 'Px ', my_id, ' changed param no. ', iparam,' = ', &
                         TRIM(changed_param_labels(iparam))
            END DO

            IF(num_params_changed > 0)THEN         
              WRITE(*,*) 'Px ', my_id,' test_integer = ', dum_int
              WRITE(*,*) 'Px ', my_id,' 2nd_test_real = ', dum_real2
              WRITE(*,*) 'Px ', my_id,' test_string = ', TRIM(dum_str)
              WRITE(*,*) 'Px ', my_id,' test_double = ', dum_dbl
            END IF

          END IF

          CALL MPI_BCAST(parse_finished, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, &
                           IERROR)
        END DO
      END IF

    END IF ! my_id == 0

    IF(finished .ne. 1)THEN

      ! Just calls 'sleep' command for a bit as we don't have
      ! a simulation here - this is where we would do 'real work' if
      ! we had any
      CALL steering_sleep_f()

      ! Adjust values of monitored variables
      dum_int2 = dum_int2 + 6
      dum_real = dum_real - 1.5

      ! Increment loop counter
      iloop = iloop + 1
    END IF

  END DO ! End of main loop


  IF(my_id .eq. 0) THEN

    CALL steering_finalize_f(status)

    IF(status .ne. REG_SUCCESS)THEN

      WRITE (*,*) 'Call to steering_finalize_f failed!'
    END IF
  END IF

  CALL MPI_FINALIZE(IERROR)

END PROGRAM para_mini_app
