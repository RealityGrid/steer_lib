package org.realitygrid.steering;

public class ReG_SteerAppside implements ReG_SteerConstants {

  private static ReG_SteerAppside instance = new ReG_SteerAppside();

  private int status;

  private String[] changedParamLabels;
  private int[] receivedCommands;
  private String[] receivedCommandParams;

  /**
   * Private constructor.
   */
  private ReG_SteerAppside() {
    changedParamLabels = new String[REG_MAX_NUM_STR_PARAMS];
    receivedCommands = new int[REG_MAX_NUM_STR_CMDS];
    receivedCommandParams = new String[REG_MAX_NUM_STR_CMDS];
  }

  public static ReG_SteerAppside getInstance() {
    return instance;
  }

  public String[] getChangedParamLabels() {
    return changedParamLabels;
  }

  public int[] getReceivedCommands() {
    return receivedCommands;
  }

  public String[] getReceivedCommandParams() {
    return receivedCommandParams;
  }

  public void steeringEnable(boolean b) {
    if(b)
      ReG_Steer.Steering_enable(REG_TRUE);
    else
      ReG_Steer.Steering_enable(REG_FALSE);
  }

  public void steeringInitialize(String s, int[] ia) throws ReG_SteerException {
    status = ReG_Steer.Steering_initialize(s, ia);
    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to initialize steering.", status);
    }
  }

  public int registerIOType(String name, int dir, int freq) throws ReG_SteerException {
    Intp iotype_handle = new Intp();

    status = ReG_Steer.Register_IOType(name, dir, freq, iotype_handle.cast());
    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to register IO type", status);
    }

    return iotype_handle.value();
  }

  public void registerParam(ReG_SteerParameter param) throws ReG_SteerException {
    int steered = REG_FALSE;
    if(param.isSteerable())
      steered = REG_TRUE;

    status = ReG_Steer.Register_param(param.getLabel(),
				      steered,
				      param.getVoidPointer(),
				      param.getType(),
				      param.getMinLabel(),
				      param.getMaxLabel());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to register parameter.", status);
    }   
  }

  public int[] steeringControl(int i) throws ReG_SteerException {
    int[] result = new int[2];
    Intp numParamsChanged = new Intp();
    Intp numReceivedCommands = new Intp();

    status = ReG_Steer.Steering_control(i,
					numParamsChanged.cast(),
					changedParamLabels,
					numReceivedCommands.cast(),
					receivedCommands,
					receivedCommandParams);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Steering control call failed.", status);
    }

    result[0] = numParamsChanged.value();
    result[1] = numReceivedCommands.value();
    return result;
  }

  public int consumeStart(int ioth) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Consume_start(ioth, iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("No data to consume.", status);
    }

    return iohandle.value();
  }

  public Object consumeDataSlice(int ioh) {
    Intp dataType = new Intp();
    Intp dataCount = new Intp();
    Object result;

    status = ReG_Steer.Consume_data_slice_header(ioh, dataType.cast(), dataCount.cast());

    if(status == REG_SUCCESS) {
      result = ReG_Steer.Consume_data_slice_j(ioh, dataType.value(), dataCount.value());
    }
    else
      result = null;
    
    return result;
  }

  public int consumeStop(int ioh) throws ReG_SteerException {
    Intp iohandle = new Intp();
    iohandle.assign(ioh);

    status = ReG_Steer.Consume_stop(iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to stop consuming!", status);
    }

    return iohandle.value();
  }

  public void steeringFinalize() throws ReG_SteerException {
    status = ReG_Steer.Steering_finalize();

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to finalize steering.", status);
    }
  }

}
