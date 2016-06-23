package org.wso2.carbon.identity.recovery.endpoint.Utils;

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;

public class RecoveryUtil {
    public static NotificationPasswordRecoveryManager getNotificationBasedPwdRecoveryManager() {
        return (NotificationPasswordRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(NotificationPasswordRecoveryManager.class);
    }

    public static SecurityQuestionPasswordRecoveryManager getSecurityQuestionBasedPwdRecoveryManager() {
        return (SecurityQuestionPasswordRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(SecurityQuestionPasswordRecoveryManager.class);
    }

    public static NotificationUsernameRecoveryManager getNotificationBasedUsernameRecoveryManager() {
        return (NotificationUsernameRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(NotificationUsernameRecoveryManager.class);
    }

    public static UserSelfRegistrationManager getUserSelfRegistrationManager() {
        return (UserSelfRegistrationManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(UserSelfRegistrationManager.class);
    }
}
