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

package org.wso2.carbon.identity.claim.verification.endpoint.impl.util;

/**
 * Class that contains all the common constants for the claim verification endpoint.
 */
public class ClaimVerificationEndpointConstants {

    public static final String API_URI = "/api/identity/claim-verification/v1.0";
    public static final String API_URI_EP_VALIDATE = "validate";
    public static final String API_URI_EP_CONFIRM = "confirm";
    public static final String API_URI_EP_INIT_VERIFICATION = "init-verification";
    public static final String TENANT_NAME_FROM_CONTEXT = "TenantNameFromContext";

    public static final String STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT = "Internal server error";
    public static final String STATUS_BAD_REQUEST_MESSAGE_DEFAULT = "Bad Request";


    public static final String ERROR_CODE_UNEXPECTED_ERROR = "18013";
    public static final String ERROR_CODE_NO_MATCHING_USER_FOUND = "20014";
    public static final String ERROR_CODE_MULTIPLE_MATCHING_USERS_FOUND = "20015";
    public static final String ERROR_CODE_NO_MATCHING_VALIDATION_STATUS_FOUND = "20016";

    public static final String ERROR_WHILE_RETRIEVING_USER_DATA = "Error while retrieving user data.";
    public static final String ERROR_WHILE_RETRIEVING_CLAIM_DATA = "Error while retrieving claim data.";

    public static final String CLAIM_VALIDATION_SUCCESS = "SUCCESS";
    public static final String CLAIM_VALIDATION_FAILURE = "FAILURE";
    public static final String CLAIM_VALIDATION_PENDING = "PENDING";

}
