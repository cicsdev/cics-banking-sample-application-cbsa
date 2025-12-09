package com.ibm.cics.cip.cbsa.galasa.tests.manager;

import dev.galasa.ManagerException;

public class CbsaException extends ManagerException{
    private static final long serialVersionUID=1L;

    public CbsaException(){

    }

    public CbsaException(String message){
        super(message);
    }

    public CbsaException(Throwable cause){
        super(cause);
    }

    public CbsaException(String message, Throwable cause){
        super(message, cause);
    }

    public CbsaException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace){
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
