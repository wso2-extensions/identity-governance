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

package org.wso2.carbon.identity.breach.detection.engine;

/**
 * What the engine concluded the caller should do.
 * <p>
 * A refusal because the password is breached and a refusal because it could not be checked are separate
 * decisions, because the user-facing message and the operator response differ.
 */
public enum Decision {

    /** No enabled source reported the password, or the ones that could not answer are set to allow. */
    ACCEPT,

    /** A source reported the password as breached. */
    REFUSE_BREACHED,

    /** A source could not answer and is set to refuse. */
    REFUSE_UNVERIFIED
}
