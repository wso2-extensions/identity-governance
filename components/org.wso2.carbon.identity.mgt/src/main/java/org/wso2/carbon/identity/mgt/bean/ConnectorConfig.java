package org.wso2.carbon.identity.mgt.bean;

import org.wso2.carbon.identity.application.common.model.Property;

public class ConnectorConfig {

    private String friendlyName;
    private Property[] properties;

    public String getFriendlyName() {
        return friendlyName;
    }

    public void setFriendlyName(String friendlyName) {
        this.friendlyName = friendlyName;
    }

    public Property[] getProperties() {
        return properties;
    }

    public void setProperties(Property[] properties) {
        this.properties = properties;
    }
}
