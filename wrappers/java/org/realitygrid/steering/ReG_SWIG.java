package org.realitygrid.steering;

public abstract class ReG_SWIG {
  protected abstract long getPointer();

  public boolean equals(Object obj) {
    boolean equal = false;
    if (obj instanceof ReG_SWIG)
      equal = (((ReG_SWIG)obj).getPointer() == this.getPointer());
    return equal;
  }
  
  public SWIGTYPE_p_void getVoidPointer() {
    return new SWIGTYPE_p_void(getPointer(), false);
  }
}
