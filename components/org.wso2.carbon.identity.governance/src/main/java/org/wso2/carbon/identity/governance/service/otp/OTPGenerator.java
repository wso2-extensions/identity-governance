/*
 * Copyright (c) 2022, WSO2 Inc. (http://www.wso2.org).
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.governance.service.otp;

import org.wso2.carbon.identity.governance.exceptions.otp.OTPGeneratorClientException;
import org.wso2.carbon.identity.governance.exceptions.otp.OTPGeneratorServerException;

public interface OTPGenerator {

    /**
     * Generates the OTP based on the provided charSet and length.
     *
     * @param charSet Character set allowed for OTP.
     * @param otpLength Length of OTP.
     * @return String Value of OTP string.
     * @throws OTPGeneratorClientException OTP Generator Client Exception
     * @throws OTPGeneratorServerException OTP Generator Server Exception
     */
    String generateOTP(String charSet, int otpLength) throws OTPGeneratorClientException, OTPGeneratorServerException;

    /**
     * Generates the OTP based on the OTP properties.
     *
     * @param useNumeric Whether numeric values should be used.
     * @param useUppercaseLetters Whether upper case letters should be used.
     * @param useLowercaseLetters Whether lower case letters should be used.
     * @param otpLength Length of OTP.
     * @return String Value of OTP string.
     * @throws OTPGeneratorClientException OTP Generator Client Exception
     * @throws OTPGeneratorServerException OTP Generator Server Exception
     */
    String generateOTP(boolean useNumeric, boolean useUppercaseLetters, boolean useLowercaseLetters, int otpLength)
            throws OTPGeneratorClientException, OTPGeneratorServerException;

}
