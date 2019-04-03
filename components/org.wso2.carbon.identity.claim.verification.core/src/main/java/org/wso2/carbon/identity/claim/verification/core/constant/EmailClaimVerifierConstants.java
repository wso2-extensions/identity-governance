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

package org.wso2.carbon.identity.claim.verification.core.constant;

public class EmailClaimVerifierConstants {

    /**
     * Constants related to EmailClaimVerifier identity connector config.
     */
    public static class ConnectorConfig {

        public static final String VALIDATION_STEP_CODE_EXPIRY_TIME =
                "EmailClaimVerification.Validation.Step.Code.ExpiryTime";
        public static final String CONFIRMATION_STEP_EXPIRY_TIME =
                "EmailClaimVerification.Confirmation.Step.Code.ExpiryTime";
    }

    /**
     * Configurations for the e-mail claim verifier.
     */
    public static class ClaimVerifierConfig {

        public static final String EMAIL_VERIFIER_PROPERTY_TEMPLATE = "EmailTemplate";
        public static final String EMAIL_VERIFIER_PROPERTY_VALIDATION_URL = "ValidationUrl";
    }

    /**
     * Request/Response parameters for the e-mail claim verifier.
     */
    public static class MessageParameter {

        public static final String EMAIL_VERIFIER_PARAMETER_NONCE_VALUE = "NonceValue";
    }

    /**
     * Properties can be used as placeholders in the email template.
     */
    public static class NotificationProperty {

        public static final String NOTIFICATION_PROPERTY_SEND_TO = "send-to";
        public static final String NOTIFICATION_PROPERTY_NONCE_VALUE = "nonce-value";
        public static final String NOTIFICATION_PROPERTY_CLAIM_NAME = "claim-name";
        public static final String NOTIFICATION_PROPERTY_CLAIM_VALUE = "claim-value";
        public static final String NOTIFICATION_PROPERTY_TEMPLATE_TYPE = "TEMPLATE_TYPE";
        public static final String NOTIFICATION_PROPERTY_VALIDATION_URL = "validation-url";
    }
}
