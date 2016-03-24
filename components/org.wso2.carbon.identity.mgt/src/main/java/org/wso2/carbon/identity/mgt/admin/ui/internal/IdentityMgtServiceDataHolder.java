package org.wso2.carbon.identity.mgt.admin.ui.internal;

import org.wso2.carbon.identity.event.services.EventMgtService;

public class IdentityMgtServiceDataHolder {

    private static IdentityMgtServiceDataHolder instance = new IdentityMgtServiceDataHolder();
    private EventMgtService eventMgtService;

    public static IdentityMgtServiceDataHolder getInstance() {

        return instance;
    }

    public EventMgtService getEventMgtService() {
        return eventMgtService;
    }

    public void setEventMgtService(EventMgtService eventMgtService) {
        this.eventMgtService = eventMgtService;
    }
}
