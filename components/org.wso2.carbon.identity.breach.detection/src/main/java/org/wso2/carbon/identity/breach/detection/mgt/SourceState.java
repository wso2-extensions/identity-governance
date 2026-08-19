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
 * The state of one source as an administrator sees it.
 * <p>
 * {@link #NOT_INSTALLED} is distinct from {@link #UNAVAILABLE} because the fix is a deployment action - a
 * missing connector JAR - rather than a configuration one.
 */
public enum SourceState {

    /** Bound, configured, and answering. */
    READY,

    /** Bound but not set up: no file, or a required credential missing. */
    NOT_CONFIGURED,

    /** Bound and set up, but currently unable to answer. */
    UNAVAILABLE,

    /** Named in this organization's policy with no bound service behind it. */
    NOT_INSTALLED,

    /** Bound and installed, but not named in this organization's policy. */
    OFF
}
