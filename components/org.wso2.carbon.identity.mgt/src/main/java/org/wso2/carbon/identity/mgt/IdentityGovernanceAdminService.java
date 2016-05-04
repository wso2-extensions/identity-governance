package org.wso2.carbon.identity.mgt;

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.core.AbstractAdmin;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.mgt.bean.ConnectorConfig;
import org.wso2.carbon.identity.mgt.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.mgt.internal.IdentityMgtServiceDataHolder;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class IdentityGovernanceAdminService extends AbstractAdmin {

    IdentityGovernanceService identityGovernanceService;

    public ConnectorConfig[] getConnectorList() throws IdentityGovernanceException {

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        identityGovernanceService = new IdentityGovernanceServiceImpl();
        List<IdentityGovernanceConnector> list = IdentityMgtServiceDataHolder.getInstance()
                .getIdentityGovernanceConnectorList();
        Map<String, String> properties = identityGovernanceService.getConfiguration(tenantDomain);
        ConnectorConfig[] configs = new ConnectorConfig[list.size()];
        String[] connectorProperties;
        for (int i=0; i<list.size();i++) {
            ConnectorConfig config =new ConnectorConfig();
            config.setFriendlyName(list.get(i).getFriendlyName());
            connectorProperties = list.get(i).getPropertyNames();
            Property[] configProperties = new Property[connectorProperties.length];
            for (int j=0; j<connectorProperties.length;j++) {
                Property prop = new Property();
                prop.setName(connectorProperties[j]);
                prop.setValue(properties.get(connectorProperties[j]));
                configProperties[j] = prop;
            }
            config.setProperties(configProperties);
            configs[i] = config;
        }
        return configs;
    }

    public void updateConfigurations (Property[] configurations) throws IdentityGovernanceException {
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        identityGovernanceService = new IdentityGovernanceServiceImpl();
        Map<String, String> confMap = new HashMap<>();
        for (Property configuration : configurations) {
            confMap.put(configuration.getName(), configuration.getValue());
        }
        identityGovernanceService.updateConfiguration(tenantDomain, confMap);

    }

}
