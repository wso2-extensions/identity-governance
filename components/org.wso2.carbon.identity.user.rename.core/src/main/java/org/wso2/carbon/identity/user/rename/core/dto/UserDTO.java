/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.rename.core.dto;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.Optional;

/**
 * Object class that represents the User whose username needs to be updated.
 */
public class UserDTO {

    String existingUsername;
    String newUsername;
    String userStoreDomain;
    String tenantDomain;

    /**
     * Returns given existing username of the user.
     *
     * @return existing username of the user
     */
    public String getExistingUsername() {

        return existingUsername;
    }

    /**
     * Sets the given existing username of the user.
     *
     * @param existingUsername existing username value
     */
    public void setExistingUsername(String existingUsername) {

        this.existingUsername = existingUsername;
    }

    /**
     * Returns the given new username of the user.
     *
     * @return new username of the user
     */
    public String getNewUsername() {

        return newUsername;
    }

    /**
     * Sets the new username of the user.
     *
     * @param newUsername new username value
     */
    public void setNewUsername(String newUsername) {

        this.newUsername = newUsername;
    }

    /**
     * Returns the given user store domain of the user.
     *
     * @return user store domain of the user
     */
    public String getUserStoreDomain() {

        if (StringUtils.isNotBlank(userStoreDomain)) {
            return userStoreDomain.toUpperCase();
        }
        return UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME;
    }

    /**
     * Sets the user store domain of the user.
     *
     * @param userStoreDomain user store domain value
     */
    public void setUserStoreDomain(String userStoreDomain) {

        this.userStoreDomain = userStoreDomain;
    }

    /**
     * Returns the tenant domain of the user.
     *
     * @return tenant domain of the user
     */
    public String getTenantDomain() {

        return Optional.ofNullable(tenantDomain).orElse(MultitenantConstants
                .SUPER_TENANT_DOMAIN_NAME);
    }

    /**
     * Sets tenant domain of the user.
     *
     * @param tenantDomain tenant domain value
     */
    public void setTenantDomain(String tenantDomain) {

        this.tenantDomain = tenantDomain;
    }

    @Override
    public String toString() {

        return "UserDTO{" +
                "existingUsername='" + existingUsername + '\'' +
                ", newUsername='" + newUsername + '\'' +
                ", userStoreDomain='" + userStoreDomain + '\'' +
                ", tenantDomain='" + tenantDomain + '\'' +
                '}';
    }
}
