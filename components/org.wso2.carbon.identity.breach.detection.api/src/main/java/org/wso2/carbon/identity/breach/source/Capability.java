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
 * What a source needs and what it is, declared so the engine and the administrator surface can treat it
 * correctly without knowing what it is.
 */
public enum Capability {

    /** Answers without crossing the deployment boundary. No timeout or circuit breaker applies. */
    OFFLINE,

    /** Calls a service outside the deployment. The engine bounds the call and breaks the circuit on failure. */
    REMOTE,

    /** Keyed on the password alone. The subject on the context is ignored. */
    PASSWORD_ONLY,

    /**
     * Keyed on the credential pair, so the subject identity is transmitted to the source. Drives the privacy
     * disclosure the administrator sees before the source can be enabled.
     */
    NEEDS_SUBJECT
}
