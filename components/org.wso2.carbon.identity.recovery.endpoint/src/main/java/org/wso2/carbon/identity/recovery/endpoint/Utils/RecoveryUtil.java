package org.wso2.carbon.identity.recovery.endpoint.Utils;

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.recovery.NotificationBasedPwdRecoveryManager;

public class RecoveryUtil {
    public static NotificationBasedPwdRecoveryManager getNotificationBasedPwdRecoveryManager() {
        return (NotificationBasedPwdRecoveryManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(NotificationBasedPwdRecoveryManager.class);
    }
}
