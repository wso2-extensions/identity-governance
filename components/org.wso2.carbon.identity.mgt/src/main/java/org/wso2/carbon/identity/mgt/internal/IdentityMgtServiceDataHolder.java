package org.wso2.carbon.identity.mgt.internal;

import org.wso2.carbon.identity.event.services.EventMgtService;
import org.wso2.carbon.identity.mgt.common.IdentityGovernanceConnector;
import org.wso2.carbon.idp.mgt.IdpManager;

import java.util.ArrayList;
import java.util.List;

public class IdentityMgtServiceDataHolder {

    private static IdentityMgtServiceDataHolder instance = new IdentityMgtServiceDataHolder();
    private EventMgtService eventMgtService;
    private IdpManager idpManager;
    private static volatile List<IdentityGovernanceConnector> identityGovernanceConnectorList = new ArrayList<>();

    public static IdentityMgtServiceDataHolder getInstance() {

        return instance;
    }

    public EventMgtService getEventMgtService() {
        return eventMgtService;
    }

    public void setEventMgtService(EventMgtService eventMgtService) {
        this.eventMgtService = eventMgtService;
    }

    protected void addIdentityGovernanceConnector(
            IdentityGovernanceConnector connector) {

        identityGovernanceConnectorList.add(connector);
    }

    protected void unsetIdentityGovernanceConnector(
            IdentityGovernanceConnector connector) {

        identityGovernanceConnectorList.remove(connector);
    }

    public List<IdentityGovernanceConnector> getIdentityGovernanceConnectorList () {
        return identityGovernanceConnectorList;
    }

    public IdpManager getIdpManager() {
        return idpManager;
    }

    public void setIdpManager(IdpManager idpManager) {
        this.idpManager = idpManager;
    }
}
