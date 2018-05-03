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

public class PurposeDTO {

    private String purpose = null;
    private List<String> purposeCategory = new ArrayList<>();
    private String consentType = null;
    private List<PiiCategoryDTO> piiCategory = new ArrayList<>();
    private Boolean primaryPurpose = null;
    private String termination = null;
    private Boolean thirdPartyDisclosure = null;
    private String thirdPartyName = null;

    public String getPurpose() {
        return purpose;
    }

    public void setPurpose(String purpose) {
        this.purpose = purpose;
    }

    public List<String> getPurposeCategory() {
        return purposeCategory;
    }

    public void setPurposeCategory(List<String> purposeCategory) {
        this.purposeCategory = purposeCategory;
    }

    public String getConsentType() {
        return consentType;
    }

    public void setConsentType(String consentType) {
        this.consentType = consentType;
    }

    public List<PiiCategoryDTO> getPiiCategory() {
        return piiCategory;
    }

    public void setPiiCategory(List<PiiCategoryDTO> piiCategory) {
        this.piiCategory = piiCategory;
    }

    public Boolean getPrimaryPurpose() {
        return primaryPurpose;
    }

    public void setPrimaryPurpose(Boolean primaryPurpose) {
        this.primaryPurpose = primaryPurpose;
    }

    public String getTermination() {
        return termination;
    }

    public void setTermination(String termination) {
        this.termination = termination;
    }

    public Boolean getThirdPartyDisclosure() {
        return thirdPartyDisclosure;
    }

    public void setThirdPartyDisclosure(Boolean thirdPartyDisclosure) {
        this.thirdPartyDisclosure = thirdPartyDisclosure;
    }

    public String getThirdPartyName() {
        return thirdPartyName;
    }

    public void setThirdPartyName(String thirdPartyName) {
        this.thirdPartyName = thirdPartyName;
    }

    @Override
    public String toString() {

        return "class PurposeDTO {\n" +
                "  purpose: " + purpose + "\n" +
                "  purposeCategory: " + purposeCategory + "\n" +
                "  consentType: " + consentType + "\n" +
                "  piiCategory: " + piiCategory + "\n" +
                "  primaryPurpose: " + primaryPurpose + "\n" +
                "  termination: " + termination + "\n" +
                "  thirdPartyDisclosure: " + thirdPartyDisclosure + "\n" +
                "  thirdPartyName: " + thirdPartyName + "\n" +
                "}\n";
    }
}
