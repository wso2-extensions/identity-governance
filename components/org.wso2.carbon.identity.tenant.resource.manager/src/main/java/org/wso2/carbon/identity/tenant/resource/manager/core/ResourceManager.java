package org.wso2.carbon.identity.tenant.resource.manager.core;

import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementException;

/**
 * Tenant Resource manager service interface.
 */
public interface ResourceManager {

    /**
     * This API is used to add an EventPublisher for a particular tenant.
     *
     * @param eventPublisherName Event Publisher Name.
     * @throws TenantResourceManagementException
     */
    void addEventPublisherConfiguration(String eventPublisherName)
            throws TenantResourceManagementException;


}
