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

package org.wso2.carbon.identity.breach.detection.util;

import org.wso2.carbon.identity.breach.detection.constants.BreachDetectionConstants;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Small helpers shared across the core. Nothing here ever accepts a candidate password.
 */
public class BreachDetectionUtils {

    private BreachDetectionUtils() {

    }

    /**
     * Source ids are compared leniently so that a deployment.toml namespace written as {@code local_list} and a
     * policy entry written as {@code localList} name the same source. The canonical form is what the source
     * itself returns from {@code getId()}.
     *
     * @param sourceId an id as written in configuration or policy.
     * @return a comparison key.
     */
    public static String normalizeSourceId(String sourceId) {

        if (sourceId == null) {
            return null;
        }
        return sourceId.replace("_", "").replace("-", "").toLowerCase(Locale.ROOT).trim();
    }

    /**
     * Resolve a user-facing message. Falls back to the supplied default so a missing translation never leaves a
     * user staring at a key.
     *
     * @param key            message key.
     * @param defaultMessage message to use when the bundle has no entry.
     * @return the message.
     */
    public static String getMessage(String key, String defaultMessage) {

        try {
            ResourceBundle bundle = ResourceBundle.getBundle(BreachDetectionConstants.RESOURCE_BUNDLE,
                    Locale.getDefault(), BreachDetectionUtils.class.getClassLoader());
            if (bundle.containsKey(key)) {
                return bundle.getString(key);
            }
        } catch (MissingResourceException ignored) {
            // The bundle is optional; the shipped default is the contract.
        }
        return defaultMessage;
    }

    /**
     * @param value a configured value.
     * @param fallback value to use when unset or unparseable.
     * @return the parsed integer.
     */
    public static int parseInt(String value, int fallback) {

        if (value == null || value.trim().isEmpty()) {
            return fallback;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            return fallback;
        }
    }

    /**
     * @param value a configured value.
     * @param fallback value to use when unset or unparseable.
     * @return the parsed long.
     */
    public static long parseLong(String value, long fallback) {

        if (value == null || value.trim().isEmpty()) {
            return fallback;
        }
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException e) {
            return fallback;
        }
    }

    /**
     * @param value a configured value.
     * @param fallback value to use when unset.
     * @return the parsed flag.
     */
    public static boolean parseBoolean(String value, boolean fallback) {

        if (value == null || value.trim().isEmpty()) {
            return fallback;
        }
        return Boolean.parseBoolean(value.trim());
    }
}
