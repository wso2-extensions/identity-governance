package org.wso2.carbon.identity.password.history.Util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.password.history.constants.PasswordHistoryConstants;

public class Utils {
    private static final Log log = LogFactory.getLog(Utils.class);

    public static IdentityEventException handleEventException(PasswordHistoryConstants.ErrorMessages
                                                            error, String data) throws IdentityEventException {
        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        return IdentityException.error(IdentityEventException.class, error.getCode(), errorDescription);
    }

    public static IdentityEventException handleEventException(PasswordHistoryConstants.ErrorMessages
                                            error, String data, Throwable throwable) throws IdentityEventException {
        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        return IdentityException.error(IdentityEventException.class, error.getCode(), errorDescription, throwable);
    }
}
