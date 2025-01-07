package org.wso2.carbon.identity.idle.account.identification.services;

import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccountIdentificationException;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

/**
 * Service interface for idle account identification.
 */
public interface IdleAccountIdentificationService {

    /**
     * Get inactive users from a specific date.
     * (Example: Providing date object of 2023-01-31 as value for 'inactiveAfter' parameter will return all users who
     * have not logged in since 2023-01-31 00:00:00.000)
     *
     * @param inactiveAfter date after which the user should be inactive.
     * @param tenantDomain  tenant domain.
     * @return              list of inactive users.
     * @throws IdleAccountIdentificationException Exception when retrieving inactive users from database.
     */
    List<InactiveUserModel> getInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, String tenantDomain)
            throws IdleAccountIdentificationException;

    /**
     * Get inactive users from a specific date excluding the oldest inactive users.
     * (Example: Providing date object of 2023-01-31 as value for 'inactiveAfter' parameter and date object of
     * 2023-01-01 as value for 'excludeBefore' parameter will return all users who have not logged in since
     * 2023-01-31 00:00:00.000 excluding users who have not logged in since 2023-01-01 00:00:00.000)
     *
     * @param inactiveAfter date after which the user should be inactive.
     * @param excludeBefore date before which the user should be excluded.
     * @param tenantDomain  tenant domain.
     * @return              list of inactive users.
     * @throws IdleAccountIdentificationException Exception when retrieving inactive users from database.
     */
    List<InactiveUserModel> getLimitedInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter,
            LocalDateTime excludeBefore, String tenantDomain) throws IdleAccountIdentificationException;

    /**
     * Get inactive users from a specific date or from a specific date excluding the oldest inactive users while
     * filtering the disabled users based on the value provided for the isDisabled.
     * If isDisabled is true, the method will return inactive and disabled users.
     * If isDisabled is false, the method will return inactive and non-disabled users filtering disabled users.
     * (Example: If isDisabled is true, the method will return all the inactive users who have not logged in since
     * 2023-01-31 00:00:00.000 excluding users who have not logged in since 2023-01-01 00:00:00.000 and who are
     * in the state of DISABLED)
     *
     * @param inactiveAfter Inactive after date.
     * @param excludeBefore Exclude before date.
     * @param tenantDomain  Tenant domain.
     * @param isDisabled    Filter based on the state DISABLED.
     * @return List of inactive users.
     * @throws IdleAccountIdentificationException Idle account identification exception.
     */
    default List<InactiveUserModel> filterInactiveUsersIfDisabled(LocalDateTime inactiveAfter,
                                                                  LocalDateTime excludeBefore, String tenantDomain,
                                                                  boolean isDisabled)
            throws IdleAccountIdentificationException {

        return Collections.emptyList();
    }
}
