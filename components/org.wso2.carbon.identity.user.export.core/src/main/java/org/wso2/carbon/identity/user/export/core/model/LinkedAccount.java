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
package org.wso2.carbon.identity.user.export.core.model;

/**
 * Data model for linked accounts.
 */
public class LinkedAccount {

    private final String connection;
    private final String connectionId;
    private final String linkedAccountId;
    private final boolean isExternalConnection;

    /**
     * @param connectionName       Name of the IDP.
     * @param connectionId         Unique Id of the IDP
     * @param linkedAccountId      User's linked account ID.
     * @param isExternalConnection is social connection or not.
     */
    public LinkedAccount(String connectionName, String connectionId, String linkedAccountId,
                         boolean isExternalConnection) {

        this.connection = connectionName;
        this.connectionId = connectionId;
        this.linkedAccountId = linkedAccountId;
        this.isExternalConnection = isExternalConnection;
    }

    /**
     * Return the IdP name.
     *
     * @return connection Name;
     */
    public String getConnection() {

        return this.connection;
    }

    /**
     * Return the unique ID of the IdP connection.
     *
     * @return connection ID.
     */
    public String getConnectionId() {

        return this.connectionId;
    }

    /**
     * Return true if this is an external IdP connection.
     *
     * @return true if this is an external IdP connection.
     */
    public boolean getIsExternalConnection() {

        return this.isExternalConnection;
    }

    /**
     * Return the userId of the linked account.
     *
     * @return linkedAccountId
     */
    public String getLinkedAccountId() {

        return this.linkedAccountId;
    }
}
