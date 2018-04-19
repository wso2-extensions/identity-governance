/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.export.core.dto;

import java.util.ArrayList;
import java.util.List;

public class ConsentReceiptDTO {

    private String version = null;
    private String jurisdiction = null;
    private Long consentTimestamp = null;
    private String collectionMethod = null;
    private String consentReceiptID = null;
    private String publicKey = null;
    private String language = null;
    private String piiPrincipalId = null;
    private String tenantDomain = null;
    private String state = null;
    private List<PiiControllerDTO> piiControllers = new ArrayList<>();
    private String policyUrl = null;
    private List<ServiceDTO> services = new ArrayList<>();
    private Boolean sensitive = null;
    private List<String> spiCat = new ArrayList<>();

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getJurisdiction() {
        return jurisdiction;
    }

    public void setJurisdiction(String jurisdiction) {
        this.jurisdiction = jurisdiction;
    }

    public Long getConsentTimestamp() {
        return consentTimestamp;
    }

    public void setConsentTimestamp(Long consentTimestamp) {
        this.consentTimestamp = consentTimestamp;
    }

    public String getCollectionMethod() {
        return collectionMethod;
    }

    public void setCollectionMethod(String collectionMethod) {
        this.collectionMethod = collectionMethod;
    }

    public String getConsentReceiptID() {
        return consentReceiptID;
    }

    public void setConsentReceiptID(String consentReceiptID) {
        this.consentReceiptID = consentReceiptID;
    }

    public String getPublicKey() {
        return publicKey;
    }

    public void setPublicKey(String publicKey) {
        this.publicKey = publicKey;
    }

    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    public String getPiiPrincipalId() {
        return piiPrincipalId;
    }

    public void setPiiPrincipalId(String piiPrincipalId) {
        this.piiPrincipalId = piiPrincipalId;
    }

    public String getTenantDomain() {
        return tenantDomain;
    }

    public void setTenantDomain(String tenantDomain) {
        this.tenantDomain = tenantDomain;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public List<PiiControllerDTO> getPiiControllers() {
        return piiControllers;
    }

    public void setPiiControllers(List<PiiControllerDTO> piiControllers) {
        this.piiControllers = piiControllers;
    }

    public String getPolicyUrl() {
        return policyUrl;
    }

    public void setPolicyUrl(String policyUrl) {
        this.policyUrl = policyUrl;
    }

    public List<ServiceDTO> getServices() {
        return services;
    }

    public void setServices(List<ServiceDTO> services) {
        this.services = services;
    }

    public Boolean getSensitive() {
        return sensitive;
    }

    public void setSensitive(Boolean sensitive) {
        this.sensitive = sensitive;
    }

    public List<String> getSpiCat() {
        return spiCat;
    }

    public void setSpiCat(List<String> spiCat) {
        this.spiCat = spiCat;
    }

    @Override
    public String toString() {

        return "class ConsentReceiptDTO {\n" +
                "  version: " + version + "\n" +
                "  jurisdiction: " + jurisdiction + "\n" +
                "  consentTimestamp: " + consentTimestamp + "\n" +
                "  collectionMethod: " + collectionMethod + "\n" +
                "  consentReceiptID: " + consentReceiptID + "\n" +
                "  publicKey: " + publicKey + "\n" +
                "  language: " + language + "\n" +
                "  piiPrincipalId: " + piiPrincipalId + "\n" +
                "  tenantDomain: " + tenantDomain + "\n" +
                "  state: " + state + "\n" +
                "  piiControllers: " + piiControllers + "\n" +
                "  policyUrl: " + policyUrl + "\n" +
                "  services: " + services + "\n" +
                "  sensitive: " + sensitive + "\n" +
                "  spiCat: " + spiCat + "\n" +
                "}\n";
    }
}
