!----------------------------------------------------------------------
!    Example of a basic steerable serial F90 application that uses the 
!    RealityGrid steering library.
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

PROGRAM mini_app
  IMPLICIT none

  INCLUDE 'reg_steer_f90.inc'

  INTEGER (KIND=REG_SP_KIND) :: num_sim_loops = 1000
  INTEGER (KIND=REG_SP_KIND) :: status

  ! For supported commands
  INTEGER (KIND=REG_SP_KIND) :: num_cmds
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_INITIAL_NUM_CMDS) :: commands

  ! For IO types
  INTEGER (KIND=REG_SP_KIND) :: num_types
  CHARACTER(LEN=REG_MAX_STRING_LENGTH), DIMENSION(REG_INITIAL_NUM_IOTYPES) :: io_labels
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_INITIAL_NUM_IOTYPES) :: iotype_handles
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_INITIAL_NUM_IOTYPES) :: io_dirn

  INTEGER (KIND=REG_SP_KIND) :: input_freq  = 0

  INTEGER (KIND=REG_SP_KIND) :: output_freq = 5
  INTEGER (KIND=REG_SP_KIND) :: iohandle
  INTEGER (KIND=REG_SP_KIND) :: data_type
  INTEGER (KIND=REG_SP_KIND) :: data_count
  INTEGER (KIND=REG_SP_KIND), DIMENSION(:), ALLOCATABLE :: i_array
  REAL    (KIND=REG_SP_KIND), DIMENSION(:), ALLOCATABLE :: f_array
  REAL    (KIND=REG_DP_KIND), DIMENSION(:), ALLOCATABLE :: d_array

  ! For parameters
  INTEGER (KIND=REG_SP_KIND) :: num_params
  CHARACTER(LEN=REG_MAX_STRING_LENGTH) :: param_label
  INTEGER (KIND=REG_SP_KIND) :: param_type
  INTEGER (KIND=REG_SP_KIND) :: param_strbl

  ! Parameters for steering
  INTEGER (KIND=REG_SP_KIND)           :: dum_int, dum_int2
  REAL (KIND=REG_SP_KIND)              :: dum_real, dum_real2
  REAL (KIND=REG_DP_KIND)              :: dum_dbl
  CHARACTER(LEN=REG_MAX_STRING_LENGTH) :: dum_str

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

  INTEGER (KIND=4) :: iloop, icmd, iparam, hdr_len, j
  INTEGER (KIND=4) :: finished = 0
  INTEGER (KIND=4) :: NX = 32
  INTEGER (KIND=4) :: NY = 32
  INTEGER (KIND=4) :: NZ = 32
  INTEGER (KIND=4) :: veclen = 1
  CHARACTER(LEN=4096) :: header
  CHARACTER(LEN=128)  :: buf
  REAL (KIND=REG_DP_KIND) :: aaxis=1.5
  REAL (KIND=REG_DP_KIND) :: baxis=1.5
  REAL (KIND=REG_DP_KIND) :: caxis=1.5

  ! Enable steering
  CALL steering_enable_f(reg_true)

  ! Specify which commands we support and initialise the library
  num_cmds = 2
  commands(1) = REG_STR_STOP
  commands(2) = REG_STR_PAUSE

  WRITE (*,*) 'Calling steering_initialize_f'

  CALL steering_initialize_f(num_cmds, commands, status)

  IF(status .ne. REG_SUCCESS)THEN

    STOP 'Call to steering_initialize_f failed!'
  END IF

  ! Register the input IO channel

  num_types = 1
  io_labels(1) = "VTK_STRUCTURED_POINTS_INPUT"
  io_labels(1)(28:28) = CHAR(0)

  io_dirn(1) = REG_IO_IN
  
  CALL register_iotypes_f(num_types, io_labels, io_dirn, &
                          input_freq, iotype_handles(1), status)

  IF(status .ne. REG_SUCCESS)THEN

    CALL steering_finalize_f(status)
    STOP 'Failed to register IO type'
  END IF

  WRITE(*,*) 'Returned IOtype = ', iotype_handles(1)

  io_labels(1) = "VTK_STRUCTURED_POINTS_OUTPUT"
  io_labels(1)(29:29) = CHAR(0)

  io_dirn(1) = REG_IO_OUT
  
  CALL register_iotypes_f(num_types, io_labels, io_dirn, &
                          output_freq, iotype_handles(2), status)

  IF(status .ne. REG_SUCCESS)THEN

    CALL steering_finalize_f(status)
    STOP 'Failed to register IO type'
  END IF

  WRITE(*,*) 'Returned IOtype = ', iotype_handles(2)

  io_labels(1) = "SOME_CHECKPOINT"
  io_labels(1)(16:16) = CHAR(0)

  io_dirn(1) = REG_IO_CHKPT
  
  CALL register_iotypes_f(num_types, io_labels, io_dirn, &
                          output_freq, iotype_handles(3), status)

  IF(status .ne. REG_SUCCESS)THEN

    CALL steering_finalize_f(status)
    STOP 'Failed to register IO type'
  END IF

  WRITE(*,*) 'Returned IOtype = ', iotype_handles(3)

  ! Register some parameters

  num_params  = 1
  dum_int     = 5
  param_label = "test_integer"
  param_label(13:13) = CHAR(0)
  param_type  = REG_INT
  param_strbl = reg_true

  CALL register_params_f(num_params, param_label, param_strbl, dum_int, &
                         param_type, status)

  ! Registration uses address of variable so use second 'dum_int' here
  ! rather than simply changing value of first one
  dum_int2 = 123
  param_label = "2nd_test_integer"
  param_label(17:17) = CHAR(0)
  param_strbl = reg_false

  CALL register_params_f(num_params, param_label, param_strbl, dum_int2, &
                         param_type, status)

  dum_real = 123.45
  param_label = "test_real"
  param_label(10:10) = CHAR(0)
  param_type  = REG_FLOAT
  param_strbl = reg_false
  
  CALL register_params_f(num_params, param_label, param_strbl, dum_real, &
                         param_type, status)

  dum_real2 = -23.456
  param_label = "2nd_test_real"
  param_label(14:14) = CHAR(0)
  param_type  = REG_FLOAT
  param_strbl = reg_true
  
  CALL register_params_f(num_params, param_label, param_strbl, dum_real2, &
                         param_type, status)

  dum_str = "hello"
  dum_str(6:6) = CHAR(0)
  param_label = "test_string"
  param_label(12:12) = CHAR(0)
  param_type  = REG_CHAR
  param_strbl = reg_true
  
  ! Getting a pointer to a string from F90 is a bit tricky so use
  ! the following function to produce the quantity that is actually
  ! passed to register_params_f
  CALL steering_char_to_ptr_f(dum_str, ptr);

  CALL register_params_f(num_params, param_label, param_strbl, ptr, &
                         param_type, status)

  dum_dbl = 123.123d0
  param_label = "test_double"
  param_label(12:12) = CHAR(0)
  param_type  = REG_DBL
  param_strbl = reg_true

  CALL register_params_f(num_params, param_label, param_strbl, dum_dbl, &
                         param_type, status)

  param_label = "veclen"
  param_label(7:7) = CHAR(0)
  param_type  = REG_INT
  param_strbl = reg_true

  CALL register_params_f(num_params, param_label, param_strbl, veclen, &
                         param_type, status)

  param_label = "a_axis"
  param_label(7:7) = CHAR(0)
  param_type  = REG_DBL
  param_strbl = reg_true

  CALL register_params_f(num_params, param_label, param_strbl, aaxis, &
                         param_type, status)

  param_label = "b_axis"
  param_label(7:7) = CHAR(0)
  param_type  = REG_DBL
  param_strbl = reg_true

  CALL register_params_f(num_params, param_label, param_strbl, baxis, &
                         param_type, status)

  param_label = "c_axis"
  param_label(7:7) = CHAR(0)
  param_type  = REG_DBL
  param_strbl = reg_true

  CALL register_params_f(num_params, param_label, param_strbl, caxis, &
                         param_type, status)

  IF(status .ne. REG_SUCCESS)THEN

    CALL steering_finalize_f(status)
    STOP 'Failed to register IO params'
  END IF

  ! Enter main 'simulation' loop

  WRITE(*,*) 'Entering main simulation loop...'

  DO WHILE(iloop<num_sim_loops .AND. (finished .ne. 1))

!!$    CALL consume_start_f(iotype_handles(1), iohandle, status)
!!$
!!$    IF( status == REG_SUCCESS )THEN
!!$	
!!$      CALL consume_data_slice_header_f(iohandle, data_type, data_count, status)
!!$
!!$
!!$      DO WHILE(status == REG_SUCCESS)
!!$
!!$        SELECT CASE(data_type)
!!$
!!$        CASE (REG_INT)
!!$
!!$          ALLOCATE(i_array(data_count))
!!$
!!$          CALL consume_data_slice_f(iohandle, data_type, data_count, &
!!$                                    i_array, status)
!!$          DEALLOCATE(i_array)
!!$
!!$        CASE (REG_FLOAT)
!!$
!!$          ALLOCATE(f_array(data_count))
!!$
!!$          CALL consume_data_slice_f(iohandle, data_type, data_count, &
!!$                                    f_array, status)
!!$          DEALLOCATE(f_array)
!!$
!!$        CASE (REG_DBL)
!!$
!!$          ALLOCATE(d_array(data_count))
!!$
!!$          CALL consume_data_slice_f(iohandle, data_type, data_count, &
!!$                                    d_array, status)
!!$          DEALLOCATE(d_array)
!!$
!!$        END SELECT
!!$
!!$        CALL consume_data_slice_header_f(iohandle, data_type, &
!!$                                         data_count, status)
!!$
!!$      END DO
!!$
!!$      CALL consume_stop_f(iohandle)
!!$    END IF

    CALL steering_control_f(iloop, num_params_changed, changed_param_labels, &
                            num_recvd_cmds, recvd_cmds, recvd_cmd_params, &
                            status)

    IF(status .eq. REG_SUCCESS)THEN

      WRITE(*,*) 'Received ',num_recvd_cmds, 'commands and ', &
                 num_params_changed, 'params'
      WRITE(*,*) 'iloop = ', iloop
      WRITE(*,*) 'test_integer = ', dum_int
      WRITE(*,*) '2nd_test_real = ', dum_real2
      WRITE(*,*) 'test_string = ', TRIM(dum_str)
      WRITE(*,*) 'test_double = ', dum_dbl
      WRITE(*,*) 'output_freq = ', output_freq

      IF(num_params_changed > 0)THEN

         DO iparam=1, num_params_changed, 1

            WRITE(*,*) 'Changed param no. ', iparam,' = ', &
                       changed_param_labels(iparam)
         END DO
      END IF

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

            IF(status .ne. REG_SUCCESS)THEN

              WRITE(*,*) 'steering_pause_f returned error'
              EXIT
            ELSE

              WRITE(*,*) 'steering_pause_f: ',num_recvd_cmds,' cmds and ', &
                         num_params_changed, ' parameters'
              ! Reset loop counter so loop over new set of commands
              ! received
              icmd = 0
            END IF

          CASE(REG_STR_STOP)
            WRITE (*,*) 'Received stop command from steerer'
            finished = 1           
            EXIT

          CASE DEFAULT
             IF(recvd_cmds(icmd) .EQ. iotype_handles(2))THEN

               WRITE(*,*) 'Emitting data...'

! Example of how to make a vtk header in F90...
!!$               buf(1:128) = "# vtk DataFile Version 2.1"//ACHAR(10)
!!$               hdr_len = 0
!!$               header = buf
!!$
!!$               buf(1:128) = "Fake test data"//ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf(1:128) = "BINARY"//ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf(1:128) = "DATASET STRUCTURED_POINTS"//ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf = " "
!!$               WRITE(buf, FMT="('DIMENSIONS ',3(I3,1x),A1)") NX, NY, NZ, &
!!$                     ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf(1:128) = "ORIGIN  0.000   0.000   0.000"//ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf(1:128) = "SPACING  1  1  1"//ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf = " "
!!$               WRITE(buf, FMT="('POINT_DATA ', I8, A1)") NX*NY*NZ, ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf(1:128) = "SCALARS scalars double"//ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               buf(1:128) = "LOOKUP_TABLE default"//ACHAR(10)
!!$               hdr_len = hdr_len + 128
!!$               header = header(1:hdr_len)//buf
!!$
!!$               ! Terminate string nicely for the C routines...
!!$               hdr_len = hdr_len + 128
!!$               header(hdr_len+1:hdr_len+1) = ACHAR(0)
! ...end of example

               ALLOCATE(f_array(veclen*NX*NY*NZ))

               CALL make_vtk_buffer_f(header, NX, NY, NZ, veclen, aaxis, &
                                      baxis, caxis, f_array, status)

               CALL emit_start_f(iotype_handles(2), iloop, reg_true, iohandle,&
                                 status)

               IF( status .eq. REG_SUCCESS )THEN
	
                 !data_count = hdr_len
                 data_count = LEN(header)
                 data_type  = REG_CHAR
                 CALL emit_data_slice_f(iohandle, data_type, data_count, &
                                        header, status)

                 ! Test with int data
                 !ALLOCATE(i_array(NX*NY*NZ))
                 !data_type  = REG_INT
                 !DO j=1, data_count, 1
                 !  i_array(j) = NINT(f_array(j))
                 !END DO

                 ! Test with double data
                 !ALLOCATE(d_array(NX*NY*NZ))
                 !data_type  = REG_DBL
                 !DO j=1, data_count, 1
                 !  d_array(j) = REAL(f_array(j), REG_DP_KIND)
                 !END DO

                 data_count = veclen*NX*NY*NZ
                 data_type  = REG_FLOAT
                 CALL emit_data_slice_f(iohandle, data_type, data_count, &
                                        f_array, status)

                 CALL emit_stop_f(iohandle, status)
               END IF

               DEALLOCATE(f_array)
               !DEALLOCATE(i_array)
               !DEALLOCATE(d_array)

               WRITE (*,*) '...done'
             END IF

          END SELECT

          icmd = icmd + 1

          IF ((icmd .gt. num_recvd_cmds) .OR. (finished .eq. 1)) EXIT
        END DO

      END IF ! num_recvd_cmds > 0

    ELSE
       WRITE (*,*) 'steering_control returned error'
    END IF

    ! Just calls 'sleep' command for a bit as we don't have
    ! a simulation here
    CALL steering_sleep_f()

    ! Adjust values of monitored variables
    dum_int2 = dum_int2 + 6
    dum_real = dum_real - 1.5

    ! Increment loop counter
    iloop = iloop + 1

  END DO ! End of main loop


  CALL steering_finalize_f(status)

  IF(status .ne. REG_SUCCESS)THEN

    STOP 'Call to steering_finalize_f failed!'
  END IF

END PROGRAM mini_app
