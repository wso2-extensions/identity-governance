package org.wso2.carbon.identity.password.policy.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.password.policy.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.policy.exeption.IdentityPasswordPolicyException;

public class Utils {
    private static final Log log = LogFactory.getLog(Utils.class);


    public static IdentityEventException handleEventException(PasswordPolicyConstants.ErrorMessages error,
                                                 String errorText, Throwable throwable) throws IdentityEventException {

        if (StringUtils.isBlank(errorText)) {
            errorText = error.getMessage();
        }

        IdentityEventException identityEventException = new IdentityEventException(errorText, throwable);
        IdentityException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityPasswordPolicyException
                .ErrorInfo.ErrorInfoBuilder(errorText);
        errorInfoBuilder.errorCode(error.getCode());
        errorInfoBuilder.cause(throwable);
        identityEventException.addErrorInfo(errorInfoBuilder.build());
        return identityEventException;
    }
}
