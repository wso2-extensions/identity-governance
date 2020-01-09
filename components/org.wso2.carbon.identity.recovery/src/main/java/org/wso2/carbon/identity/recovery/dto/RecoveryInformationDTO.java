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
 * Object that encapsulates the user account recovery details.
 */
public class RecoveryInformationDTO {

    /**
     * Recovered username of the user.
     */
    private String username;

    /**
     * Available Recovery channel Information.
     */
    private RecoveryChannelInfoDTO recoveryChannelInfoDTO;

    /**
     * Whether question based recovery is enabled for the user.
     */
    private boolean isQuestionBasedRecoveryEnabled;

    /**
     * Whether notification based recovery is enabled for the user.
     */
    private boolean isNotificationBasedRecoveryEnabled;

    /**
     * Get whether question based recovery is enabled for the user.
     *
     * @return True if question based recovery is enabled for the user
     */
    public boolean isQuestionBasedRecoveryEnabled() {

        return isQuestionBasedRecoveryEnabled;
    }

    /**
     * set whether question based recovery is enabled for the user.
     *
     * @param questionBasedRecoveryEnabled Question based recovery enabled
     */
    public void setQuestionBasedRecoveryEnabled(boolean questionBasedRecoveryEnabled) {

        isQuestionBasedRecoveryEnabled = questionBasedRecoveryEnabled;
    }

    /**
     * set whether notification based recovery is enabled for the user.
     *
     * @param isNotificationBasedRecoveryEnabled Notification based recovery enabled
     */
    public void setNotificationBasedRecoveryEnabled(boolean isNotificationBasedRecoveryEnabled) {

        this.isNotificationBasedRecoveryEnabled = isNotificationBasedRecoveryEnabled;
    }

    /**
     * Get whether notification based recovery is enabled for the user.
     *
     * @return True if notification based recovery is enabled for the user
     */
    public boolean isNotificationBasedRecoveryEnabled() {

        return isNotificationBasedRecoveryEnabled;
    }

    /**
     * Get recovered username.
     *
     * @return Username
     */
    public String getUsername() {

        return username;
    }

    /**
     * Set recovered username.
     *
     * @param userName Username
     */
    public void setUsername(String userName) {

        this.username = userName;
    }

    /**
     * Get RecoveryChannelInfoDTO.
     *
     * @return RecoveryChannelInfoDTO
     */
    public RecoveryChannelInfoDTO getRecoveryChannelInfoDTO() {

        return recoveryChannelInfoDTO;
    }

    /**
     * Set RecoveryChannelInfoDTO.
     *
     * @param recoveryChannelInfoDTO RecoveryChannelInfoDTO
     */
    public void setRecoveryChannelInfoDTO(RecoveryChannelInfoDTO recoveryChannelInfoDTO) {

        this.recoveryChannelInfoDTO = recoveryChannelInfoDTO;
    }
}
