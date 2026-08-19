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
 * The credential-write operation a candidate password was submitted for.
 * <p>
 * Carried on {@link BreachContext} so a source can vary its answer by operation - refusing on registration
 * but only reporting on an administrative reset, for example. A source that does not care ignores it.
 */
public enum Operation {

    /** Self-registration, and administrative user creation. */
    REGISTER,

    /** Self-service password change, with the current password supplied. */
    SELF_UPDATE,

    /** Administrative password reset. */
    ADMIN_RESET,

    /** Password set while accepting an invitation. */
    INVITE,

    /** Password set through the recovery flow. */
    RECOVERY,

    /** A credential write that could not be attributed to one of the above. */
    UNKNOWN
}
