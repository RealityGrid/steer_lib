SUBROUTINE steering_initialize_f(AppName, NumSupportedCmds, &
                               SupportedCmds, Status)
  IMPLICIT none
  INCLUDE 'reg_steer_f90.inc'

  CHARACTER (LEN=*), INTENT(in)                        :: AppName
  INTEGER (KIND=REG_SP_KIND), INTENT(in)               :: NumSupportedCmds
  INTEGER (KIND=REG_SP_KIND), DIMENSION(*), INTENT(in) :: SupportedCmds
  INTEGER (KIND=REG_SP_KIND), INTENT(out)              :: Status

  INTEGER (KIND=REG_SP_KIND), DIMENSION(2) :: int_array
  REAL (KIND=REG_SP_KIND), DIMENSION(2)    :: real_array
  REAL (KIND=REG_DP_KIND), DIMENSION(2)    :: dbl_array
  INTEGER (KIND=4)                         :: bytes

  ! These calls are the reason this F90 wrapper exists - allows us to 
  ! calculate the sizeof(each ReG type).  This info. is used in
  ! Register_bin_param_f(...)
  CALL set_type_size(REG_INT, AppName(1:1), AppName(2:2), Status)
  CALL set_type_size(REG_INT, int_array(1), int_array(2), Status)
  CALL set_type_size(REG_FLOAT, real_array(1), real_array(2), Status)
  CALL set_type_size(REG_DBL, dbl_array(1), dbl_array(2), Status)

  CALL steering_initialize_wrapper(AppName, NumSupportedCmds, &
                                   SupportedCmds, Status)
  RETURN
END SUBROUTINE steering_initialize_f
