package org.wso2.carbon.identity.recovery.endpoint.Utils;

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.recovery.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.SecurityQuestionPasswordRecoveryManager;

public class RecoveryUtil {
    public static NotificationPasswordRecoveryManager getNotificationBasedPwdRecoveryManager() {
        return (NotificationPasswordRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(NotificationPasswordRecoveryManager.class);
    }

    public static SecurityQuestionPasswordRecoveryManager getSecurityQuestionBasedPwdRecoveryManager() {
        return (SecurityQuestionPasswordRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(SecurityQuestionPasswordRecoveryManager.class);
    }
}
