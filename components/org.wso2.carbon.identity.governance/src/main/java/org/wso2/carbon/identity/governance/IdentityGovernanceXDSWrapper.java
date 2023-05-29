package org.wso2.carbon.identity.governance;

import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.xds.common.constant.XDSWrapper;

import java.util.List;
import java.util.Map;

public class IdentityGovernanceXDSWrapper implements XDSWrapper {

    private String tenantDomain;
    private Map<String, String> configurationDetails;

    private List<IdentityProviderProperty> properties;

    public IdentityGovernanceXDSWrapper(IdentityGovernanceXDSWrapperBuilder builder) {

        this.tenantDomain = builder.tenantDomain;
        this.configurationDetails = builder.configurationDetails;
        this.properties = builder.properties;
    }

    public String getTenantDomain() {

        return tenantDomain;
    }

    public Map<String, String> getConfigurationDetails() {

        return configurationDetails;
    }

    /**
     * Builder class for IdentityGovernanceXDSWrapper.
     */
    public static class IdentityGovernanceXDSWrapperBuilder {

        private String tenantDomain;
        private Map<String, String> configurationDetails;

        private List<IdentityProviderProperty> properties;

        public IdentityGovernanceXDSWrapperBuilder setTenantDomain(String tenantDomain) {

            this.tenantDomain = tenantDomain;
            return this;
        }

        public IdentityGovernanceXDSWrapperBuilder setConfigurationProperties(List<IdentityProviderProperty> properties) {

            this.properties = properties;
            return this;
        }

        public IdentityGovernanceXDSWrapperBuilder setConfigurationDetails(Map<String, String> configurationDetails) {

            this.configurationDetails = configurationDetails;
            return this;
        }

        public IdentityGovernanceXDSWrapper build() {

            return new IdentityGovernanceXDSWrapper(this);
        }
    }
}
