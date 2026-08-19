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
 * What a source concluded about a candidate password.
 * <p>
 * {@link #UNAVAILABLE} is not {@link #NOT_FOUND}. A source that cannot reach its corpus, times out, exhausts a
 * quota, or fails to parse a response returns {@code UNAVAILABLE}. No source is permitted to invent
 * {@code NOT_FOUND}: collapsing the two is what makes a breach check silently stop enforcing while still
 * reporting itself as enabled.
 */
public enum Outcome {

    /** The source reports the password as present in its corpus. */
    FOUND,

    /** The source positively reports the password as absent from its corpus. */
    NOT_FOUND,

    /** The source could not produce a verdict. Resolved by the failure policy configured for it. */
    UNAVAILABLE
}
