/*
 * Copyright (c) 2022, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.governance.service.otp;

public interface OTPGenerator {

    /**
     * Generates the OTP based on the provided charSet and length.
     *
     * @param charSet Character set allowed for OTP.
     * @param otpLength Length of OTP.
     */
    String generateOTP(String charSet, int otpLength);

    /**
     * Generates the OTP based on the OTP properties.
     *
     * @param isNumeric Whether numeric values are present in regex.
     * @param isUpperCase Whether upper case characters are present in regex.
     * @param isLowerCase Whether lower case characters are present in regex.
     * @param otpLength Length of OTP.
     */
    String generateOTP(boolean isNumeric, boolean isUpperCase, boolean isLowerCase, int otpLength);

}
