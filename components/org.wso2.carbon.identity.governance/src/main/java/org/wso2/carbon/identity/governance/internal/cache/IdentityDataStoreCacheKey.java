/*
 * Copyright (c) 2021, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

package org.wso2.carbon.identity.governance.internal.cache;

import org.wso2.carbon.identity.core.cache.CacheKey;

/**
 * Key for the identityDataStoreCache, contains domain name and username.
 */
public class IdentityDataStoreCacheKey extends CacheKey {

    private static final long serialVersionUID = 7332751154827092685L;
    private final String domainName;
    private final String username;

    public IdentityDataStoreCacheKey(String domainName, String username) {

        this.domainName = domainName;
        this.username = username;
    }

    @Override
    public boolean equals(Object o) {

        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }

        IdentityDataStoreCacheKey that = (IdentityDataStoreCacheKey) o;

        return domainName.equals(that.domainName) && username.equals(that.username);
    }

    @Override
    public int hashCode() {

        int result = super.hashCode();
        result = 31 * result + domainName.hashCode();
        result = 31 * result + username.hashCode();
        return result;
    }
}
