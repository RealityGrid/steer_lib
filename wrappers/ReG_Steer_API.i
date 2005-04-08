/*-------------- Application-side function prototypes -------------*/

extern void Steering_enable(const int EnableSteer);

extern int Steering_initialize(char *AppName,
			       int   NumSupportedCmds,
			       int  *SupportedCmds);

extern int Register_IOType(char* IOLabel,
			   int   direction,
			   int   IOFrequency,
			   int   *iotypehandle);

extern int Enable_IOTypes_on_registration(int toggle);

extern int Disable_IOType(int IOType);

extern int Enable_IOType(int IOType);

extern int Enable_IOType_acks(int IOType);

extern int Disable_IOType_acks(int IOType);

#if 0
extern int Register_ChkType(char* ChkLabel,
			    int   direction,
			    int   ChkFrequency,
			    int   *ChkType);

extern int Record_Chkpt(int   ChkType,
			char *ChkTag);

extern int Add_checkpoint_file(int   ChkType,
			       char *filename);

extern int Record_checkpoint_set(int   ChkType,
				 char *ChkTag,
				 char *Path);
#endif

extern int Register_param(char* ParamLabel,
			  int   ParamSteerable,
			  void *ParamPtr,
			  int   ParamType,
			  char* ParamMinimum,
			  char* ParamMaximum);

extern int Steering_control(int     SeqNum,
			    int    *NumSteerParams,
			    char*  *SteerParamLabels,
			    int    *NumSteerCommands,
			    int    *SteerCommands,
			    char*  *SteerCmdParams);

#ifdef SWIGJAVA
extern int Emit_start(int  IOType,
		      int  SeqNum,
		      int *IOHandle);

extern int Emit_start_blocking(int    IOType,
			       int    SeqNum,
			       int   *IOHandle,
			       float  TimeOut);

extern int Emit_data_slice(int	            IOHandle,
			   int             DataTypeIN,
			   int             CountIN,
			   const void     *pDataIN);

extern int Emit_stop(int *IOHandleINOUT);
#endif

extern int Consume_start(int  IOType,
			 int *IOHandle);

extern int Consume_start_blocking(int   IOType,
				  int  *IOHandle,
				  float TimeOut);

extern int Consume_data_slice_header(int  IOTypeIndex,
				     int *DataType,
				     int *DataCount);

#ifndef SWIGJAVA
extern int Consume_data_slice(int     IOTypeIndex,
			      int     DataType,
			      int     Count,
			      void   *pDataOUT);
#endif

extern int Consume_stop(int     *IOHandleINOUT);

extern int Steering_finalize();

#if 0
extern int Steering_pause(int   *NumSteerParams,
			  char **SteerParamLabels,
			  int   *NumCommands,
			  int   *SteerCommands,
			  char **SteerCmdParams);

extern char **Alloc_string_array(int String_len,
				 int Array_len);

extern int Free_string_arrays();

extern int Make_vtk_buffer(int    nx,
			   int    ny,
			   int    nz,
			   int    veclen,
			   double a,
			   double b,
			   double c,
			   float *array);

extern int Make_vtk_header(char  *header,
			   char  *title,
			   int    nx,
			   int    ny,
			   int    nz,
			   int    veclen,
			   int    type);

extern int Make_chunk_header(char *header,
			     int   IOindex,
			     int   totx, int toty, int totz,
			     int   sx,   int sy,   int sz,
			     int   nx,   int ny,   int nz);

extern int Set_f90_array_ordering(int IOTypeIndex,
				  int flag);

extern int Called_from_f90(int flag);

extern int Reorder_array(int          ndims,
			 int         *tot_extent,
			 int         *sub_extent,
			 int         *origin,
			 int          type,
			 void        *pInData,
			 void        *pOutData,
			 int          to_f90);
#endif

/* It's useful to have a sizeof type thing... */
%inline %{
  int Sizeof(int type) {
    int result;
    switch(type) {
    case REG_INT:
      result = sizeof(int);
      break;
    case REG_CHAR:
      result = sizeof(char);
      break;
    case REG_FLOAT:
      result = sizeof(float);
      break;
    case REG_DBL:
      result = sizeof(double);
      break;
    case REG_XDR_INT:
      result = REG_SIZEOF_XDR_INT;
      break;
    case REG_XDR_FLOAT:
      result = REG_SIZEOF_XDR_FLOAT;
      break;
    case REG_XDR_DOUBLE:
      result = REG_SIZEOF_XDR_DOUBLE;
      break;
    default:
      result = 0;
      break;
    }

    return result;
  }
%}
int Sizeof(int type);
