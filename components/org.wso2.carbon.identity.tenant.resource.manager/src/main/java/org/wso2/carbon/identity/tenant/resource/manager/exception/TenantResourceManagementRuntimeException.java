package org.wso2.carbon.identity.tenant.resource.manager.exception;

/**
 * This class is an {@link RuntimeException}.
 */
public class TenantResourceManagementRuntimeException extends RuntimeException {

    private String errorCode;

    public TenantResourceManagementRuntimeException() {

        super();
    }

    public TenantResourceManagementRuntimeException(String message, String errorCode) {

        super(message);
        this.errorCode = errorCode;
    }

    public TenantResourceManagementRuntimeException(String message, String errorCode, Throwable cause) {

        super(message, cause);
        this.errorCode = errorCode;
    }

    public TenantResourceManagementRuntimeException(Throwable cause) {

        super(cause);
    }

    protected String getErrorCode() {

        return errorCode;
    }

    protected void setErrorCode(String errorCode) {

        this.errorCode = errorCode;
    }
}
