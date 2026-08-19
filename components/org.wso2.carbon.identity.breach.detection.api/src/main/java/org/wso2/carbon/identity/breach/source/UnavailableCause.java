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

package org.wso2.carbon.identity.breach.source;

/**
 * Why a source returned {@link Outcome#UNAVAILABLE}.
 * <p>
 * These are distinguished rather than collapsed because the operator response differs entirely: an exhausted
 * quota is a billing action, a transport failure is a network one, and a source named in policy with no bundle
 * behind it is a deployment one.
 */
public enum UnavailableCause {

    /** The call did not complete inside the configured timeout. */
    TIMEOUT,

    /** The call failed to reach the service, or the service returned a transport-level failure. */
    TRANSPORT,

    /** A rate limit or quota was exhausted. */
    QUOTA,

    /** A response was received but could not be understood. */
    PARSE,

    /** The source is installed but not usable with the configuration it was given. */
    MISCONFIGURED,

    /** Repeated failure opened the circuit breaker; the source is not being called. */
    CIRCUIT_OPEN,

    /** Tenant policy names a source id with no bound service - usually a missing connector JAR. */
    SOURCE_NOT_REGISTERED,

    /** An unexpected error inside the source. Contained to that source. */
    INTERNAL
}
