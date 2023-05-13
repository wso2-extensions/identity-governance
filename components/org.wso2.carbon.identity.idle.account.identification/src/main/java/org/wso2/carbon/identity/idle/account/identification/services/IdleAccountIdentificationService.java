package org.wso2.carbon.identity.idle.account.identification.services;

import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationException;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Service interface for idle account identification.
 */
public interface IdleAccountIdentificationService {

    /**
     * Get inactive users from a specific date.
     *
     * @param inactiveAfter date after which the user should be inactive.
     * @param tenantDomain  tenant domain.
     * @return              list of inactive users.
     * @throws IdleAccIdentificationException Exception when retrieving inactive users from database.
     */
    List<InactiveUserModel> getInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, String tenantDomain)
            throws IdleAccIdentificationException;

    /**
     * Get inactive users from a specific date excluding the oldest inactive users.
     *
     * @param inactiveAfter date after which the user should be inactive.
     * @param excludeBefore date before which the user should be excluded.
     * @param tenantDomain  tenant domain.
     * @return              list of inactive users.
     * @throws IdleAccIdentificationException Exception when retrieving inactive users from database.
     */
    List<InactiveUserModel> getLimitedInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter,
            LocalDateTime excludeBefore, String tenantDomain) throws IdleAccIdentificationException;
}
