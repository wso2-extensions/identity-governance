/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.wso2.carbon.identity.claim.verification.core.util;

import org.apache.axiom.om.OMElement;
import org.apache.axiom.om.impl.builder.StAXOMBuilder;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants;
import org.wso2.carbon.utils.CarbonUtils;
import org.wso2.carbon.utils.ServerConstants;
import org.wso2.securevault.SecretResolver;
import org.wso2.securevault.SecretResolverFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Stack;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamException;

import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_READING_CONFIGURATION;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.VerificationConfigs.CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.VerificationConfigs.CLAIM_VERIFICATION_CONFIG_ELEMENT_ATTRIBUTE_CLAIM_VERIFIER_ID;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.VerificationConfigs.CLAIM_VERIFICATION_CONFIG_ELEMENT_ATTRIBUTE_TENANT_DOMAIN;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.VerificationConfigs.CLAIM_VERIFICATION_CONFIG_ELEMENT_NAME_CLAIM_VERIFIER;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.VerificationConfigs.CLAIM_VERIFICATION_CONFIG_ELEMENT_NAME_TENANT;

public class ClaimVerificationConfigParser {

    private static Map<String, Object> configuration = new HashMap<>();
    private static SecretResolver secretResolver;
    private static Log log = LogFactory.getLog(ClaimVerificationConfigParser.class);
    private OMElement rootElement;

    public ClaimVerificationConfigParser() {

        buildConfiguration();
    }

    /**
     * For the given configurationKey value, retrieve the corresponding config value, either an array list or a string
     * if exists. Null otherwise. The configurationKey value is a domain separated string to identify a configuration
     * . Following is the default email claim verifier configurations.
     *<pre>{@code
     * <ClaimVrificationConfigs>
     *   <Tenant domain="carbon.super">
     *     <ClaimVerifiers>
     *           <!-- Default email claim Verifier Configurations -->
     *           <ClaimVerifier id="EmailClaimVerifier">
     *               <config>
     *                 <ValidationUrl>www.google.com</ValidationUrl>
     *                <EmailTemplate>claimVerifier</EmailTemplate>
     *               </config>
     *           </ClaimVerifier>
     *     </ClaimVerifiers>
     *   </Tenant>
     * </ClaimVrificationConfigs>
     * }
     * {@code <Tenant>} tag is used to identify the tenant and requires to have the {@code domain={tenant-domain}}
     * attribute. Domain value for this element is resolved to the domain attribute.
     *
     * {@code <ClaimVerifier>} tag is used to identify the claim verifier and requires to have the {@code id={claim
     * -verifier-id}} attribute. Domain value for this element is resolved to the id attribute.
     * </pre>
     *
     * For above, configurationKey for ValidationUrl is resolved to, {@code carbon.super::ClaimVerifiers
     * ::EmailClaimVerifier::config::ValidationUrl}.
     *
     * @param configurationKey Key of the configuration wish to receive. This needs to be properly separated by the
     *                         domain.
     * @return Corresponding String value for the configuration key. Null otherwise.
     */
    public Object getConfigValue(String configurationKey) {

        return configuration.get(configurationKey);
    }

    /**
     * Retrieve all the available configurations for the given claim verifier in the given tenant domain.
     *
     * @param tenantDomain Tenant domain value.
     * @param claimVerifierId Id value for the claim verifier.
     * @return Configurations available for the given claim verifier in the given tenant domain.
     */
    public Map<String, Object> getClaimVerifierConfigs(String tenantDomain, String claimVerifierId) {

        String claimVerifierConfigDomain = getClaimVerifierConfigDomainKey(tenantDomain, claimVerifierId);

        Map<String, Object> claimVerifierConfigs = new HashMap<>();
        configuration.forEach((key, val) -> {
            if (key.startsWith(claimVerifierConfigDomain)) {
                claimVerifierConfigs.put(key, val);
            }
        });
        return claimVerifierConfigs;
    }

    private String getClaimVerifierConfigDomainKey(String tenantDomain, String claimVerifierId) {

        // key -> {tenant-domain}::ClaimVerifiers::{claim-verifier}::config::{config-name}
        StringBuilder sb = new StringBuilder();
        addNewClaimVerificationConfigDomain(sb, tenantDomain);
        addNewClaimVerificationConfigDomain(sb, "ClaimVerifiers");
        addNewClaimVerificationConfigDomain(sb, claimVerifierId);
        addNewClaimVerificationConfigDomain(sb, "config");
        return sb.toString();
    }

    private void addNewClaimVerificationConfigDomain(StringBuilder sb, String domain) {

        sb.append(domain);
        sb.append(CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR);
    }

    public Map<String, Object> getConfiguration() {

        return configuration;
    }

    private void buildConfiguration() {

        InputStream inStream = null;
        StAXOMBuilder builder;

        try {

            String configurationFilePath = CarbonUtils.getCarbonConfigDirPath()
                    + File.separator + ClaimVerificationCoreConstants.VerificationConfigs.CLAIM_VERIFICATION_CONFIG_FILE;
            File consentConfigXml = new File(configurationFilePath);
            if (consentConfigXml.exists()) {
                inStream = new FileInputStream(consentConfigXml);
            }

            if (inStream == null) {
                String message = "Consent configuration not found";
                if (log.isDebugEnabled()) {
                    log.debug(message);
                }
                throw new FileNotFoundException(message);
            }

            builder = new StAXOMBuilder(inStream);
            rootElement = builder.getDocumentElement();
            Stack<String> nameStack = new Stack<>();
            secretResolver = SecretResolverFactory.create(rootElement, true);
            readChildElements(rootElement, nameStack);
        } catch (IOException | XMLStreamException e) {
            throw ClaimVerificationCoreUtils.getClaimVerificationRuntimeException(
                    ERROR_MSG_READING_CONFIGURATION, e);
        } finally {
            try {
                if (inStream != null) {
                    inStream.close();
                }
            } catch (IOException e) {
                log.error("Error closing the input stream for consent-mgt-config.xml", e);
            }
        }
    }

    private void readChildElements(OMElement serverConfig, Stack<String> nameStack) {

        for (Iterator childElements = serverConfig.getChildElements(); childElements.hasNext(); ) {
            OMElement element = (OMElement) childElements.next();
            nameStack.push(getResolvedLocalName(element));
            if (elementHasText(element)) {
                String key = getKey(nameStack);
                Object currentObject = configuration.get(key);
                String value = replaceSystemProperty(element.getText());
                if (secretResolver != null && secretResolver.isInitialized() &&
                        secretResolver.isTokenProtected(key)) {
                    value = secretResolver.resolve(key);
                }
                if (currentObject == null) {
                    configuration.put(key, value);
                } else if (currentObject instanceof ArrayList) {
                    ArrayList list = (ArrayList) currentObject;
                    if (!list.contains(value)) {
                        list.add(value);
                        configuration.put(key, list);
                    }
                } else {
                    if (!value.equals(currentObject)) {
                        ArrayList arrayList = new ArrayList(2);
                        arrayList.add(currentObject);
                        arrayList.add(value);
                        configuration.put(key, arrayList);
                    }
                }
            }
            readChildElements(element, nameStack);
            nameStack.pop();
        }
    }

    private String getResolvedLocalName(OMElement element) {

        // Resolve config element local names in the claim verification config file.
        String localName = element.getLocalName();

        switch (localName) {
            case CLAIM_VERIFICATION_CONFIG_ELEMENT_NAME_TENANT:
                // Set 'domain' attribute as the Tenant local name.
                String tenantDomain = getElementAttributeValue(element,
                        CLAIM_VERIFICATION_CONFIG_ELEMENT_ATTRIBUTE_TENANT_DOMAIN);
                validateResolvedLocalName(localName, tenantDomain);
                localName = tenantDomain;
                break;
            case CLAIM_VERIFICATION_CONFIG_ELEMENT_NAME_CLAIM_VERIFIER:
                // Set 'id' attribute as the ClaimVerifier local name.
                String id = getElementAttributeValue(element,
                        CLAIM_VERIFICATION_CONFIG_ELEMENT_ATTRIBUTE_CLAIM_VERIFIER_ID);
                validateResolvedLocalName(localName, id);
                localName = id;
                break;
        }
        return localName;
    }

    private void validateResolvedLocalName(String elementIdentifier, String resolvedLocalName) {

        if (StringUtils.isBlank(resolvedLocalName)) {
            log.error("Error resolving attribute value: " + resolvedLocalName + " as the local name in the " +
                    "element: " + elementIdentifier);
            throw ClaimVerificationCoreUtils.getClaimVerificationRuntimeException(
                    ERROR_MSG_READING_CONFIGURATION);
        }

        // Domain separator value is not allowed in a resolved local name.
        if (resolvedLocalName.contains(CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR)) {
            log.error("Error resolving attribute value: " + resolvedLocalName + " as the local name in the " +
                    "element: " + elementIdentifier + ". Resolved local name cannot contain the " +
                    "domain separator value: " + CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR);
            throw ClaimVerificationCoreUtils.getClaimVerificationRuntimeException(
                    ERROR_MSG_READING_CONFIGURATION);
        }
    }

    private String getElementAttributeValue(OMElement element, String attributeName) {

        return element.getAttribute(new QName(attributeName)).getAttributeValue();
    }

    private String getKey(Stack<String> nameStack) {

        StringBuilder key = new StringBuilder();
        for (int i = 0; i < nameStack.size(); i++) {
            String name = nameStack.elementAt(i);
            key.append(name).append(CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR);
        }
        key.delete(key.lastIndexOf(CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR), key.length());

        return key.toString();
    }

    private boolean elementHasText(OMElement element) {

        String text = element.getText();
        return text != null && text.trim().length() != 0;
    }

    private String replaceSystemProperty(String text) {

        int indexOfStartingChars = -1;
        int indexOfClosingBrace;

        // The following condition deals with properties.
        // Properties are specified as ${system.property},
        // and are assumed to be System properties
        StringBuilder textBuilder = new StringBuilder(text);
        while (indexOfStartingChars < textBuilder.indexOf("${")
                && (indexOfStartingChars = textBuilder.indexOf("${")) != -1
                && (indexOfClosingBrace = textBuilder.indexOf("}")) != -1) { // Is a property used?
            String sysProp = textBuilder.substring(indexOfStartingChars + 2, indexOfClosingBrace);
            String propValue = System.getProperty(sysProp);
            if (propValue != null) {
                textBuilder = new StringBuilder(textBuilder.substring(0, indexOfStartingChars) + propValue
                        + textBuilder.substring(indexOfClosingBrace + 1));
            }
            if (sysProp.equals(ServerConstants.CARBON_HOME)) {
                if (System.getProperty(ServerConstants.CARBON_HOME).equals(".")) {
                    textBuilder.insert(0, new File(".").getAbsolutePath() + File.separator);
                }
            }
        }
        text = textBuilder.toString();
        return text;
    }
}
