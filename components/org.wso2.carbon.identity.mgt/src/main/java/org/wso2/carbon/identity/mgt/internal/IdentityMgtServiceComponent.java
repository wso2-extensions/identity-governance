package org.wso2.carbon.identity.mgt.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.ComponentContext;
import org.wso2.carbon.identity.event.services.EventMgtService;
import org.wso2.carbon.identity.mgt.IdentityGovernanceException;
import org.wso2.carbon.identity.mgt.IdentityGovernanceService;
import org.wso2.carbon.identity.mgt.IdentityGovernanceServiceImpl;
import org.wso2.carbon.identity.mgt.IdentityGovernanceUtil;
import org.wso2.carbon.identity.mgt.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.mgt.listener.IdentityMgtEventListener;
import org.wso2.carbon.identity.mgt.listener.IdentityStoreEventListener;;
import org.wso2.carbon.identity.mgt.listener.TenantCreationEventListener;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.stratos.common.listeners.TenantMgtListener;
import org.wso2.carbon.user.core.listener.UserOperationEventListener;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

/**
 * @scr.component name="org.wso2.carbon.identity.mgt.internal.IdentityMgtServiceComponent"
 * immediate="true
 * @scr.reference name="EventMgtService"
 * interface="org.wso2.carbon.identity.event.services.EventMgtService" cardinality="1..1"
 * policy="dynamic" bind="setEventMgtService" unbind="unsetEventMgtService"
 * @scr.reference name="idp.mgt.event.listener.service"
 * interface="org.wso2.carbon.identity.mgt.common.IdentityGovernanceConnector"
 * cardinality="0..n" policy="dynamic"
 * bind="setIdentityGovernanceConnector"
 * unbind="unsetIdentityGovernanceConnector"
 * @scr.reference name="IdentityProviderManager"
 * interface="org.wso2.carbon.idp.mgt.IdpManager" cardinality="1..1"
 * policy="dynamic" bind="setIdpManager" unbind="unsetIdpManager"
 */
public class IdentityMgtServiceComponent {

    private static Log log = LogFactory.getLog(IdentityMgtServiceComponent.class);
    private static IdentityMgtEventListener listener = null;

    protected void activate(ComponentContext context) {

        try {
            listener = new IdentityMgtEventListener();
            context.getBundleContext().registerService(UserOperationEventListener.class,
                    listener, null);
            context.getBundleContext().registerService(UserOperationEventListener.class,
                    new IdentityStoreEventListener(), null);
            context.getBundleContext().registerService(IdentityGovernanceService.class, new
                    IdentityGovernanceServiceImpl(), null);
            context.getBundleContext().registerService(TenantMgtListener.class.getName(),
                    new TenantCreationEventListener(), null);
            if (log.isDebugEnabled()) {
                log.debug("Identity Management Listener is enabled");
            }
        } catch (Exception e) {
            log.error("Error while activating identity governance component.", e);
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

    protected void unsetIdentityGovernanceConnector(IdentityGovernanceConnector identityGovernanceConnector) {
        IdentityMgtServiceDataHolder.getInstance().unsetIdentityGovernanceConnector(identityGovernanceConnector);
    }

    protected void setIdentityGovernanceConnector(IdentityGovernanceConnector identityGovernanceConnector) {
        IdentityMgtServiceDataHolder.getInstance().addIdentityGovernanceConnector(identityGovernanceConnector);
        try {
            IdentityGovernanceUtil.saveConnectorDefaultProperties(identityGovernanceConnector, MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        } catch (IdentityGovernanceException e) {
            log.error("Error while saving super tenant configurations for " + identityGovernanceConnector.getName() +
                    ".", e);
        }
    }

    protected void unsetIdpManager(IdpManager idpManager) {
        IdentityMgtServiceDataHolder.getInstance().setIdpManager(null);
    }

    protected void setIdpManager(IdpManager idpManager) {
        IdentityMgtServiceDataHolder.getInstance().setIdpManager(idpManager);
    }


}
