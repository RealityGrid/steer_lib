package org.realitygrid.steering;

public class ReG_SteerException extends Exception {

  private int errorCode;

  public ReG_SteerException() {
    super();
  }

  public ReG_SteerException(String s) {
    super(s);
  }

  public ReG_SteerException(int ec) {
    errorCode = ec;
  }

  public ReG_SteerException(String s, int ec) {
    super(s);
    errorCode = ec;
  }

  public int getErrorCode() {
    return errorCode;
  }

}
