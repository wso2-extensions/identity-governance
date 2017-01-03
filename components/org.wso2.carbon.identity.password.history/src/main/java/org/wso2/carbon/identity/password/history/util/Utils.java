package org.wso2.carbon.identity.password.history.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.common.base.exception.IdentityException;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.password.history.constants.PasswordHistoryConstants;
import org.wso2.carbon.identity.password.history.exeption.IdentityPasswordHistoryException;

/**
 * Utility methods.
 */
public class Utils {
    private static final Log log = LogFactory.getLog(Utils.class);

    public static EventException handleEventException(PasswordHistoryConstants.ErrorMessages
                                                            error, String data) throws EventException {
        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        EventException identityEventException = new EventException(errorDescription);
        IdentityException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityPasswordHistoryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(error.getCode());
        identityEventException.addErrorInfo(errorInfoBuilder.build());
        return identityEventException;
    }

    public static EventException handleEventException(PasswordHistoryConstants.ErrorMessages
                                            error, String data, Throwable throwable) throws EventException {
        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        EventException identityEventException = new EventException(errorDescription, throwable);
        IdentityException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityPasswordHistoryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(error.getCode());
        errorInfoBuilder.cause(throwable);
        identityEventException.addErrorInfo(errorInfoBuilder.build());
        return identityEventException;
    }
}
