/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.account.suspension.notification.task.ldap;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrieval;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationConstants;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationReceiver;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.ldap.LDAPConnectionContext;
import org.wso2.carbon.user.core.ldap.LDAPConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.DirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class LDAPNotificationReceiversRetrieval implements NotificationReceiversRetrieval {

    private static final Log log = LogFactory.getLog(LDAPNotificationReceiversRetrieval.class);
    private RealmConfiguration realmConfiguration = null;

    @Override
    public void init(RealmConfiguration realmConfiguration) {
        this.realmConfiguration = realmConfiguration;
    }

    @Override
    public List<NotificationReceiver> getNotificationReceivers(long lookupMin, long lookupMax,
            long delayForSuspension, String tenantDomain) throws AccountSuspensionNotificationException {

        List<NotificationReceiver> users = new ArrayList<NotificationReceiver>();

        if (realmConfiguration != null) {
            String ldapSearchBase = realmConfiguration.getUserStoreProperty(LDAPConstants.USER_SEARCH_BASE);
            RealmService realmService = NotificationTaskDataHolder.getInstance().getRealmService();

            try {
                ClaimManager claimManager = (ClaimManager) realmService.getTenantUserRealm(IdentityTenantUtil.
                        getTenantId(tenantDomain)).getClaimManager();
                String userStoreDomain = realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.
                        PROPERTY_DOMAIN_NAME);
                if (StringUtils.isBlank(userStoreDomain)) {
                    userStoreDomain = IdentityUtil.getPrimaryDomainName();
                }

                String usernameMapAttribute = claimManager.getAttributeName(userStoreDomain, NotificationConstants.USERNAME_CLAIM);
                String firstNameMapAttribute  = claimManager.getAttributeName(userStoreDomain, NotificationConstants.FIRST_NAME_CLAIM);
                String emailMapAttribute = claimManager.getAttributeName(userStoreDomain, NotificationConstants.EMAIL_CLAIM);
                String lastLoginTimeAttribute = claimManager.getAttributeName(userStoreDomain, NotificationConstants.LAST_LOGIN_TIME);

                if (log.isDebugEnabled()) {
                    log.debug("Retrieving ldap user list for lookupMin: " + lookupMin + " - lookupMax: " + lookupMax);
                }

                LDAPConnectionContext ldapConnectionContext = new LDAPConnectionContext(realmConfiguration);
                DirContext ctx = ldapConnectionContext.getContext();

                //carLicense is the mapped LDAP attribute for LastLoginTime claim
                String searchFilter = "(&("+lastLoginTimeAttribute+">=" + lookupMin + ")("+lastLoginTimeAttribute+"<="
                        + lookupMax + "))";

                SearchControls searchControls = new SearchControls();
                searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);

                NamingEnumeration<SearchResult> results = ctx.search(ldapSearchBase, searchFilter, searchControls);

                if (log.isDebugEnabled()) {
                    log.debug("LDAP user list retrieved.");
                }

                while (results.hasMoreElements()) {
                    SearchResult result = results.nextElement();

                    NotificationReceiver receiver = new NotificationReceiver();
                    receiver.setEmail((String) result.getAttributes().get(emailMapAttribute).get());
                    receiver.setUsername((String) result.getAttributes().get(usernameMapAttribute).get());
                    receiver.setFirstName((String) result.getAttributes().get(firstNameMapAttribute).get());
                    receiver.setUserStoreDomain(userStoreDomain);

                    long lastLoginTime = Long.parseLong(result.getAttributes().get(lastLoginTimeAttribute).get().
                            toString());
                    long expireDate = lastLoginTime + TimeUnit.DAYS.toMillis(delayForSuspension);
                    receiver.setExpireDate(new SimpleDateFormat("dd-MM-yyyy").format(new Date(expireDate)));

                    if (log.isDebugEnabled()) {
                        log.debug("Expire date was set to: " + receiver.getExpireDate());
                    }
                    users.add(receiver);
                }
            } catch (NamingException e) {
                throw new AccountSuspensionNotificationException("Failed to filter users from LDAP user store.", e);
            } catch (UserStoreException e) {
                throw new AccountSuspensionNotificationException("Failed to load LDAP connection context.", e);
            } catch (org.wso2.carbon.user.api.UserStoreException e) {
                throw new AccountSuspensionNotificationException("Error occurred while getting tenant user realm for "
                        + "tenant:" + tenantDomain, e);
            }
        }
        return users;
    }

}
