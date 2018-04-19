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

public class PiiControllerDTO {

    private String piiController = null;
    private String contact = null;
    private AddressDTO address = null;
    private String email = null;
    private String phone = null;
    private Boolean onBehalf = null;
    private String piiControllerUrl = null;

    public String getPiiController() {
        return piiController;
    }

    public void setPiiController(String piiController) {
        this.piiController = piiController;
    }

    public String getContact() {
        return contact;
    }

    public void setContact(String contact) {
        this.contact = contact;
    }

    public AddressDTO getAddress() {
        return address;
    }

    public void setAddress(AddressDTO address) {
        this.address = address;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public Boolean getOnBehalf() {
        return onBehalf;
    }

    public void setOnBehalf(Boolean onBehalf) {
        this.onBehalf = onBehalf;
    }

    public String getPiiControllerUrl() {
        return piiControllerUrl;
    }

    public void setPiiControllerUrl(String piiControllerUrl) {
        this.piiControllerUrl = piiControllerUrl;
    }

    @Override
    public String toString() {

        return "class PiiControllerDTO {\n" +
                "  piiController: " + piiController + "\n" +
                "  contact: " + contact + "\n" +
                "  address: " + address + "\n" +
                "  email: " + email + "\n" +
                "  phone: " + phone + "\n" +
                "  onBehalf: " + onBehalf + "\n" +
                "  piiControllerUrl: " + piiControllerUrl + "\n" +
                "}\n";
    }
}
