/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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
package org.wso2.carbon.identity.recovery.dto;

/**
 * Object that encapsulates data for a successful resend confirmation.
 */
public class ResendConfirmationDTO {

    /**
     * Success status message.
     */
    private String successMessage;

    /**
     * Recovery Info sent channel.
     */
    private String notificationChannel;

    /**
     * Resend code to resend recovery information.
     */
    private String resendCode;

    /**
     * Success status code.
     */
    private String successCode;

    /**
     * Code to confirm password recovery when the notifications are managed externally.
     */
    private String externalConfirmationCode;

    /**
     * Get external confirmation code.
     *
     * @return External confirmation code.
     */
    public String getExternalConfirmationCode() {

        return externalConfirmationCode;
    }

    /**
     * Set external confirmation code.
     *
     * @param externalConfirmationCode External confirmation code
     */
    public void setExternalConfirmationCode(String externalConfirmationCode) {

        this.externalConfirmationCode = externalConfirmationCode;
    }

    /**
     * Get resend code.
     *
     * @return Resend code
     */
    public String getResendCode() {

        return resendCode;
    }

    /**
     * Get resend code.
     *
     * @param resendCode Resend code
     */
    public void setResendCode(String resendCode) {

        this.resendCode = resendCode;
    }

    /**
     * Get success code.
     *
     * @return Success code
     */
    public String getSuccessCode() {

        return successCode;
    }

    /**
     * Set success code.
     *
     * @param successCode Success code
     */
    public void setSuccessCode(String successCode) {

        this.successCode = successCode;
    }

    /**
     * Get success message.
     *
     * @return Success message
     */
    public String getSuccessMessage() {

        return successMessage;
    }

    /**
     * Set success message.
     *
     * @param successMessage Success message.
     */
    public void setSuccessMessage(String successMessage) {

        this.successMessage = successMessage;
    }

    /**
     * Get notification channel.
     *
     * @return Notification channel
     */
    public String getNotificationChannel() {

        return notificationChannel;
    }

    /**
     * Set notification channel.
     *
     * @param notificationChannel Notification channel
     */
    public void setNotificationChannel(String notificationChannel) {

        this.notificationChannel = notificationChannel;
    }
}
