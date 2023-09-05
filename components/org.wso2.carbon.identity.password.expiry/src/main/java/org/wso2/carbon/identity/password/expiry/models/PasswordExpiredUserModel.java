/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.password.expiry.models;

/**
 * Object for password expired user model.
 */
public class PasswordExpiredUserModel {

    private String userId;
    private String username;
    private String userStoreDomain;

    /**
     * Method to get userId.
     *
     * @return userId.
     */
    public String getUserId() {

        return userId;
    }

    /**
     * Method to set userId.
     */
    public void setUserId(String userId) {

        this.userId = userId;
    }

    /**
     * Method to get username.
     *
     * @return username.
     */
    public String getUsername() {

        return username;
    }

    /**
     * Method to set username.
     */
    public void setUsername(String username) {

        this.username = username;
    }

    /**
     * Method to get userStore domain.
     *
     * @return userStore domain.
     */
    public String getUserStoreDomain() {

        return userStoreDomain;
    }

    /**
     * Method to set userStore domain.
     */
    public void setUserStoreDomain(String userStoreDomain) {

        this.userStoreDomain = userStoreDomain;
    }
}
