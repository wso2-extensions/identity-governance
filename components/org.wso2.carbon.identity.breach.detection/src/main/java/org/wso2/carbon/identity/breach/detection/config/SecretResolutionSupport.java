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

package org.wso2.carbon.identity.breach.detection.config;

import org.apache.axiom.om.OMElement;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.securevault.SecretResolver;
import org.wso2.securevault.SecretResolverFactory;

/**
 * Resolves a secure-vault alias to its value.
 * <p>
 * Isolated in its own class and loaded reflectively by {@link BreachDetectionConfig} so that a deployment
 * without the secure vault bundle degrades to literal values rather than failing to load the configuration
 * layer outright.
 */
final class SecretResolutionSupport {

    private static final Log LOG = LogFactory.getLog(SecretResolutionSupport.class);

    private final SecretResolver resolver;

    SecretResolutionSupport(OMElement documentRoot) {

        SecretResolver created = null;
        try {
            created = SecretResolverFactory.create(documentRoot, false);
        } catch (Throwable t) {
            LOG.debug("Secure vault is not available for breach detection configuration. " +
                    "Secret properties will be read literally.", t);
        }
        this.resolver = created;
    }

    /**
     * @param alias secure vault alias.
     * @return the resolved value, or {@code null} when no resolver is initialised or the alias is unknown.
     */
    String resolve(String alias) {

        if (resolver == null || alias == null || !resolver.isInitialized()) {
            return null;
        }
        try {
            if (resolver.isTokenProtected(alias)) {
                return resolver.resolve(alias);
            }
        } catch (Throwable t) {
            // Never let a vault failure surface the alias or the value.
            LOG.error("Failed to resolve a secure vault alias for a breach detection source property.");
        }
        return null;
    }
}
