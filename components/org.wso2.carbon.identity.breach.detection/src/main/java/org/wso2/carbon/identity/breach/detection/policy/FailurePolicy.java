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

package org.wso2.carbon.identity.breach.detection.policy;

import org.wso2.carbon.identity.breach.detection.constants.BreachDetectionConstants;

/**
 * What to do about a source that could not answer.
 * <p>
 * The default is {@link #ALLOW} for remote sources, which is defensible only because the offline list keeps
 * enforcing when they are down. Fail-closed everywhere turns a third-party outage into an outage of every
 * password write in the deployment.
 */
public enum FailurePolicy {

    /** Proceed, and record the gap in telemetry. */
    ALLOW,

    /** Refuse, with a message distinguishable from a breached-password rejection. */
    DENY;

    public static FailurePolicy from(String value) {

        if (BreachDetectionConstants.ON_ERROR_DENY.equalsIgnoreCase(value)) {
            return DENY;
        }
        return ALLOW;
    }

    public String toConfigValue() {

        return this == DENY ? BreachDetectionConstants.ON_ERROR_DENY : BreachDetectionConstants.ON_ERROR_ALLOW;
    }
}
