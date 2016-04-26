package org.wso2.carbon.identity.mgt.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.ComponentContext;
import org.wso2.carbon.identity.event.services.EventMgtService;
import org.wso2.carbon.identity.mgt.listener.IdentityMgtEventListener;
import org.wso2.carbon.identity.mgt.listener.IdentityStoreEventListener;
import org.wso2.carbon.user.core.listener.UserOperationEventListener;

/**
 * @scr.component name="org.wso2.carbon.identity.mgt.internal.IdentityMgtServiceComponent"
 * immediate="true
 * @scr.reference name="EventMgtService"
 * interface="org.wso2.carbon.identity.event.services.EventMgtService" cardinality="1..1"
 * policy="dynamic" bind="setEventMgtService" unbind="unsetEventMgtService"
 */
public class IdentityMgtServiceComponent {

    private static Log log = LogFactory.getLog(IdentityMgtServiceComponent.class);
    private static IdentityMgtEventListener listener = null;

    protected void activate(ComponentContext context) {

        listener = new IdentityMgtEventListener();
        context.getBundleContext().registerService(UserOperationEventListener.class,
                        listener, null);
        context.getBundleContext().registerService(UserOperationEventListener.class,
                new IdentityStoreEventListener(), null);
        if (log.isDebugEnabled()) {
            log.debug("Identity Management Listener is enabled");
        }
    }


    protected void deactivate(ComponentContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Identity Management bundle is de-activated");
        }
    }

    protected void unsetEventMgtService(EventMgtService eventMgtService) {
        IdentityMgtServiceDataHolder.getInstance().setEventMgtService(null);
    }

    protected void setEventMgtService(EventMgtService eventMgtService) {
        IdentityMgtServiceDataHolder.getInstance().setEventMgtService(eventMgtService);
    }
}
