package org.wso2.carbon.identity.password.expiry.services;


import org.wso2.carbon.identity.password.expiry.exceptions.ExpiredPasswordIdentificationException;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiredUserModel;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Service interface for expired password identification.
 */
public interface ExpiredPasswordIdentificationService {

    /**
     * Get password expired users from a specific date.
     * (Example: Providing date object of 2023-01-31 as value for 'expiredAfter' parameter will return all users whose
     * password will expire on or after 2023-01-31 00:00:00.000)
     *
     * @param expiredAfter  The date after which passwords will expire.
     * @param tenantDomain  Tenant domain.
     * @return List of password expired users.
     * @throws ExpiredPasswordIdentificationException Exception when retrieving password expired users from database.
     */
    List<PasswordExpiredUserModel> getPasswordExpiredUsersFromSpecificDate(LocalDateTime expiredAfter, String tenantDomain)
            throws ExpiredPasswordIdentificationException;

    /**
     * Get password expired users between specific dats.
     * (Example: Providing date object of 2023-01-01 as value for 'expiredAfter' parameter and date object of
     * 2023-01-31 as value for 'excludeAfter' parameter will return all users whose password will expire after
     * 2023-01-01 00:00:00.000, but excluding after 2023-01-31 00:00:00.000)
     *
     * @param expiredAfter  The date after which passwords will expire.
     * @param excludeBefore The date after which the user should be excluded.
     * @param tenantDomain  Tenant domain.
     * @return List of password expired users.
     * @throws ExpiredPasswordIdentificationException Exception when retrieving password expired users from database.
     */
    List<PasswordExpiredUserModel> getPasswordExpiredUsersBetweenSpecificDates(LocalDateTime expiredAfter,
            LocalDateTime excludeBefore, String tenantDomain) throws ExpiredPasswordIdentificationException;
}
