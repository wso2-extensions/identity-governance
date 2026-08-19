/*
 * Copyright (c) 2026, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.breach.detection.mgt;

/**
 * The management contract, published as an OSGi service.
 * <p>
 * This is what an administrator API renders from. It is the only part of the core exported to anything outside
 * it apart from the SPI, and it deliberately exposes no way to submit a candidate password: no interface
 * introduced by this capability accepts a password from a caller.
 */
public interface BreachDetectionService {

    /**
     * @param tenantDomain the organization asking.
     * @return what the capability is doing for that organization, including installed and enabled sources.
     */
    BreachDetectionStatus getStatus(String tenantDomain);

    /**
     * Re-read operator configuration from identity.xml and re-hand it to every bound source. A source backed by
     * a file rebuilds its index; a failure leaves the previously loaded data in effect.
     *
     * @return a human-readable summary of what was reloaded.
     */
    String reloadSources();
}
