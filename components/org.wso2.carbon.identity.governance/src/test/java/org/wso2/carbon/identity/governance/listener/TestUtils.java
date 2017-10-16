package org.wso2.carbon.identity.governance.listener;

import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;

public class TestUtils {

    public static void startTenantFlow(String tenantDomain) {
        String carbonHome = TestUtils.class.getResource("/").getFile();
        System.setProperty(CarbonBaseConstants.CARBON_HOME, carbonHome);
        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(tenantDomain);
    }

}
