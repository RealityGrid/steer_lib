!
!  The RealityGrid Steering Library
!
!  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
!  All rights reserved.
!
!  This software is produced by Research Computing Services, University
!  of Manchester as part of the RealityGrid project and associated
!  follow on projects, funded by the EPSRC under grants GR/R67699/01,
!  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
!  EP/F00561X/1.
!
!  LICENCE TERMS
!
!  Redistribution and use in source and binary forms, with or without
!  modification, are permitted provided that the following conditions
!  are met:
!
!    * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!
!    * Redistributions in binary form must reproduce the above
!      copyright notice, this list of conditions and the following
!      disclaimer in the documentation and/or other materials provided
!      with the distribution.
!
!    * Neither the name of The University of Manchester nor the names
!      of its contributors may be used to endorse or promote products
!      derived from this software without specific prior written
!      permission.
!
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
!  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
!  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
!  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
!  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
!  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!  POSSIBILITY OF SUCH DAMAGE.
!
!  Author: Andrew Porter
!          Robert Haines

PROGRAM para_mini_app

!  use reg_steer_module
  IMPLICIT none

  INCLUDE 'reg_steer_f90.inc'
  INCLUDE 'mpif.h'

  INTEGER (KIND=REG_SP_KIND) :: status

  ! For supported commands
  INTEGER (KIND=REG_SP_KIND) :: num_cmds
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_INITIAL_NUM_CMDS)    :: commands

  ! For IO types
  INTEGER (KIND=REG_SP_KIND), &
                           DIMENSION(REG_INITIAL_NUM_IOTYPES) :: iotype_handles

  ! For Chk types
  INTEGER (KIND=REG_SP_KIND), &
                           DIMENSION(REG_INITIAL_NUM_IOTYPES) :: chk_handles

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
    CALL steering_initialize_f("mini_app_para v.1.0", num_cmds, &
                               commands, status)

    IF(status .ne. REG_SUCCESS)THEN

      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Call to steering_initialize_f failed!'
    END IF
  END IF

  ! Register the input IO channel

  IF(my_id .eq. 0)THEN
    CALL register_iotype_f("VTK_STRUCTURED_POINTS_INPUT", REG_IO_IN, &
                           0, iotype_handles(1), status)

    IF(status .ne. REG_SUCCESS)THEN

      CALL steering_finalize_f(status)
      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Failed to register IO type'
    END IF

    WRITE(*,*) 'Returned IOtype = ', iotype_handles(1)
  END IF

  IF(my_id .eq. 0)THEN
    CALL register_iotype_f("SOME_OUTPUT", REG_IO_OUT, &
                           5, iotype_handles(2), status)

    IF(status .ne. REG_SUCCESS)THEN

      CALL steering_finalize_f(status)
      CALL MPI_BCAST(REG_FAILURE, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERROR)
      CALL MPI_FINALIZE(IERROR)
      STOP 'Failed to register IO type'
    END IF

    WRITE(*,*) 'Returned IOtype = ', iotype_handles(2)
  END IF

  IF(my_id .eq. 0)THEN
    CALL register_chktype_f("SOME_CHECKPOINT", REG_IO_INOUT, &
                            5, chk_handles(1), status)

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
