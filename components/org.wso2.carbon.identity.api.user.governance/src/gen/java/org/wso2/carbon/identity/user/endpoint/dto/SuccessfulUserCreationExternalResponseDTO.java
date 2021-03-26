/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.user.endpoint.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;

import javax.validation.Valid;

/**
 * DTO which holds the attributes of the response for successful user self-registration via external channel.
 */
public class SuccessfulUserCreationExternalResponseDTO extends SuccessfulUserCreationDTO {

    @Valid
    private String confirmationCode = null;

    /**
     * Confirmation code to verify the user, when notifications are externally managed. In this scenario,
     * *notificationChannel* will be *EXTERNAL*. Use the confirmation code for confirm the user account.
     **/
    @ApiModelProperty(value = "Confirmation code to verify the user, when notifications are externally managed. " +
            "In this scenario, *notificationChannel* will be *EXTERNAL*. Use the confirmation code for confirm the " +
            "user account.")
    @JsonProperty("confirmationCode")
    public String getConfirmationCode() {

        return confirmationCode;
    }

    /**
     * Set the confirmation code to confirm the self registration flow externally.
     *
     * @param confirmationCode Confirmation code
     */
    public void setConfirmationCode(String confirmationCode) {

        this.confirmationCode = confirmationCode;
    }
}
