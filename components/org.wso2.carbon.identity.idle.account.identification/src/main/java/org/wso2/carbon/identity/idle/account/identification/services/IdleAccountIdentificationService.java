package org.wso2.carbon.identity.idle.account.identification.services;

import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationException;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;

import java.util.List;

/**
 * Service interface for idle account identification.
 */
public interface IdleAccountIdentificationService {

    /**
     * Get the list of inactive users.
     *
     * @param inactiveAfter Latest active date of login.
     * @param excludeBefore Date to exclude the oldest inactive users.
     * @param tenantDomain  Tenant domain.
     * @return List of inactive users.
     * @throws IdleAccIdentificationException Exception thrown when getting inactive users.
     */
    List<InactiveUserModel> getInactiveUsers(String inactiveAfter, String excludeBefore, String tenantDomain)
            throws IdleAccIdentificationException;
}
