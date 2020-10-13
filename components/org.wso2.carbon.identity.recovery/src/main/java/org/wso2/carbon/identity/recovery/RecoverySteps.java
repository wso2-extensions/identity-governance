/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.recovery;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.recovery.util.Utils;

/**
 * Enum which contains the recovery steps related to recovery scenarios.
 */
public enum RecoverySteps {
    NOTIFY,
    UPDATE_PASSWORD,
    VALIDATE_CHALLENGE_QUESTION,
    VALIDATE_ALL_CHALLENGE_QUESTION,
    CONFIRM_SIGN_UP,
    SEND_RECOVERY_INFORMATION,
    RESEND_CONFIRMATION_CODE,
    CONFIRM_LITE_SIGN_UP,
    VERIFY_EMAIL,
    VERIFY_MOBILE_NUMBER;

    /**
     * Get Recovery step which matches the given step name.
     *
     * @param stepName Name of the step
     * @return RecoverySteps
     * @throws IdentityRecoveryClientException Invalid step name
     */
    public static RecoverySteps getRecoveryStep(String stepName) throws IdentityRecoveryClientException {

        RecoverySteps[] recoverySteps = {
                NOTIFY, UPDATE_PASSWORD, VALIDATE_CHALLENGE_QUESTION, VALIDATE_ALL_CHALLENGE_QUESTION, CONFIRM_SIGN_UP,
                CONFIRM_LITE_SIGN_UP, SEND_RECOVERY_INFORMATION, RESEND_CONFIRMATION_CODE, VERIFY_EMAIL,
                VERIFY_MOBILE_NUMBER
        };
        if (StringUtils.isNotEmpty(stepName)) {
            for (RecoverySteps step : recoverySteps) {
                if (stepName.equals(step.name())) {
                    return step;
                }
            }
        }
        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_STEP,
                stepName);
    }
}
