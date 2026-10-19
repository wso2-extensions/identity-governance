/*
 * Copyright (c) 2026, WSO2 LLC. (https://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.model;

import org.wso2.carbon.identity.application.common.model.User;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Registration completion work retained across provisioning retries. Does not retain the add-user event,
 * credentials, or user-store manager.
 */
public record RegistrationCompletionData(String username, String tenantDomain, String userStoreDomain,
                                         String preferredChannel, boolean channelVerified,
                                         boolean notificationClaimAvailable, boolean confirmationEnabled,
                                         boolean effectiveAccountLock, boolean notificationsInternallyManaged,
                                         boolean completionNotificationEnabled, boolean publishSuccessEvent,
                                         Map<String, String> userClaims,
                                         Map<String, String> notificationProperties) implements Serializable {

    private static final long serialVersionUID = 1L;

    public RegistrationCompletionData {

        userClaims = userClaims == null ? null : Collections.unmodifiableMap(new HashMap<>(userClaims));
        notificationProperties = Collections.unmodifiableMap(new HashMap<>(notificationProperties));
    }

    public User toUser() {

        User user = new User();
        user.setUserName(username);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(userStoreDomain);
        return user;
    }

    public Property[] toNotificationProperties() {

        return notificationProperties.entrySet().stream()
                .map(entry -> new Property(entry.getKey(), entry.getValue())).toArray(Property[]::new);
    }
}
