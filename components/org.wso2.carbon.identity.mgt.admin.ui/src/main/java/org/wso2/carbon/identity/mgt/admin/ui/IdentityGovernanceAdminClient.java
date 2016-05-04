package org.wso2.carbon.identity.mgt.admin.ui;

import org.apache.axis2.client.Options;
import org.apache.axis2.client.ServiceClient;
import org.apache.axis2.context.ConfigurationContext;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.governance.stub.bean.Property;
import org.wso2.carbon.identity.governance.stub.IdentityGovernanceAdminServiceIdentityGovernanceExceptionException;
import org.wso2.carbon.identity.governance.stub.IdentityGovernanceAdminServiceStub;
import org.wso2.carbon.identity.governance.stub.bean.ConnectorConfig;

import java.rmi.RemoteException;

public class IdentityGovernanceAdminClient {

    protected IdentityGovernanceAdminServiceStub stub = null;

    protected static Log log = LogFactory.getLog(IdentityGovernanceAdminClient.class);

    public IdentityGovernanceAdminClient(String cookie, String backendServerURL,
                                   ConfigurationContext configContext)
            throws Exception {
        try {
            stub = new IdentityGovernanceAdminServiceStub(configContext, backendServerURL +
                    IdentityMgtAdminUIConstants.IDENTITY_MGT_ADMIN_SERVICE_URL);
            ServiceClient client = stub._getServiceClient();
            Options option = client.getOptions();
            option.setManageSession(true);
            option.setProperty(org.apache.axis2.transport.http.HTTPConstants.COOKIE_STRING, cookie);
        } catch (Exception e) {
            throw new Exception("Error occurred while creating TenantIdentityMgtClient Object", e);
        }
    }

    public ConnectorConfig[] getConnectorList() throws RemoteException, IdentityGovernanceAdminServiceIdentityGovernanceExceptionException {
        return stub.getConnectorList();
    }

    public void updateConfigurations (Property[] properties) throws RemoteException, IdentityGovernanceAdminServiceIdentityGovernanceExceptionException {
        stub.updateConfigurations(properties);
    }

}
