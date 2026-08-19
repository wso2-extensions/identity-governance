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

package org.wso2.carbon.identity.breach.detection.constants;

/**
 * Names shared across the capability: configuration keys, governance property names, and the error codes a
 * caller sees. Exported so an administrator API can name the same things without duplicating literals.
 */
public class BreachDetectionConstants {

    private BreachDetectionConstants() {

    }

    /** Listener registration, mirroring the declaration in identity.xml. */
    public static final String LISTENER_TYPE = "org.wso2.carbon.user.core.listener.UserOperationEventListener";

    public static final String LISTENER_CLASS =
            "org.wso2.carbon.identity.breach.detection.listener.BreachDetectionListener";

    /**
     * After input validation at 3, so a password failing composition never reaches a breach source, and before
     * the service extension at 10000, so in-product policy resolves before any customer extension runs.
     */
    public static final int DEFAULT_LISTENER_ORDER = 420;

    /** The identity.xml element carrying operator configuration, rendered from [breach_detection]. */
    public static final String CONFIG_ELEMENT = "BreachDetection";
    public static final String CONFIG_SOURCES_ELEMENT = "Sources";
    public static final String CONFIG_SOURCE_ELEMENT = "Source";
    public static final String CONFIG_PROPERTY_ELEMENT = "Property";
    public static final String CONFIG_ATTRIBUTE_ID = "id";
    public static final String CONFIG_ATTRIBUTE_NAME = "name";
    public static final String CONFIG_ATTRIBUTE_SECRET_ALIAS = "secretAlias";

    /** The one source that ships in the core. */
    public static final String LOCAL_LIST_SOURCE_ID = "localList";

    /** Governance connector - per-organization policy. */
    public static final String CONNECTOR_NAME = "breachDetection";
    public static final String CONNECTOR_FRIENDLY_NAME = "Breached Password Detection";
    public static final String CONNECTOR_CATEGORY = "Password Policies";
    public static final String CONNECTOR_SUB_CATEGORY = "DEFAULT";
    public static final int CONNECTOR_ORDER = 0;

    public static final String PROPERTY_ENABLE = "breachDetection.enable";
    public static final String PROPERTY_SOURCES = "breachDetection.sources";
    public static final String PROPERTY_ON_ERROR_PREFIX = "breachDetection.";
    public static final String PROPERTY_ON_ERROR_SUFFIX = ".onError";

    public static final String ON_ERROR_ALLOW = "allow";
    public static final String ON_ERROR_DENY = "deny";

    /**
     * Deployment default is disabled. Upgrading with no configuration change must produce password-setting
     * behaviour identical to before the upgrade.
     */
    public static final String DEFAULT_ENABLE = "false";
    public static final String DEFAULT_SOURCES = "";

    /** Operator settings, read from identity.xml. */
    public static final String CONFIG_SOURCE_TIMEOUT_MS = "evaluation_timeout_ms";
    public static final int DEFAULT_SOURCE_TIMEOUT_MS = 1500;
    public static final String CONFIG_EXEMPT_BULK = "exempt_bulk_operations";
    public static final String CONFIG_WORKER_THREADS = "worker_threads";
    public static final int DEFAULT_WORKER_THREADS = 20;

    /**
     * Error codes. A policy rejection is a client error carrying the reason - never a server fault, which is
     * indistinguishable from an outage and stops portals rendering the cause.
     */
    public static final String ERROR_CODE_BREACHED_PASSWORD = "BRD-60001";
    public static final String ERROR_CODE_CANNOT_VERIFY = "BRD-60002";

    /** Message keys, resolved through the bundled resource bundle so they localize with everything else. */
    public static final String MESSAGE_KEY_BREACHED = "breach.detection.password.breached";
    public static final String MESSAGE_KEY_CANNOT_VERIFY = "breach.detection.password.unverifiable";
    public static final String MESSAGE_KEY_RECOVERY_STILL_VALID = "breach.detection.recovery.still.valid";

    public static final String RESOURCE_BUNDLE = "org.wso2.carbon.identity.breach.detection.i18n.Resources";
}
