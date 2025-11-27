/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.governance.internal.service.impl.otp;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.annotation.bundle.Capability;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.service.otp.OTPGenerator;

import java.security.SecureRandom;

/**
 * Default class to generate OTP.
 */
@Capability(
        namespace = "osgi.service",
        attribute = {
                "objectClass=org.wso2.carbon.identity.governance.service.otp.OTPGenerator",
                "service.scope=singleton"
        }
)
public class OTPGeneratorImpl implements OTPGenerator {

    private static final Log log = LogFactory.getLog(OTPGeneratorImpl.class);
    private static final String OTP_GENERATOR_IMPL = "OTPGeneratorImpl";

    /**
     * Generate the OTP according to given length and pattern. If pattern is not defined default pattern will be used.
     * Default pattern is to use all three type characters (uppercase characters, lowercase characters and numerics).
     *
     * @param useUppercaseLetters Whether uppercase characters should be used for OTP.
     * @param useLowercaseLetters Whether lowercase characters should be used for OTP.
     * @param useNumeric          Whether numeric characters should be used for OTP.
     * @param otpLength           OTP length.
     * @param recoveryScenario    Recovery scenario.
     * @return Secret key.
     */
    @Override
    public String generateOTP(boolean useNumeric, boolean useUppercaseLetters, boolean useLowercaseLetters,
                              int otpLength, String recoveryScenario) {

        if (otpLength < IdentityMgtConstants.OTPGeneratorConstants.OTP_CODE_MIN_LENGTH ||
                otpLength > IdentityMgtConstants.OTPGeneratorConstants.OTP_CODE_MAX_LENGTH) {
            otpLength = IdentityMgtConstants.OTPGeneratorConstants.OTP_CODE_DEFAULT_LENGTH;
            if (log.isDebugEnabled()) {
                log.debug("Configured OTP length is not in the range of " +
                        IdentityMgtConstants.OTPGeneratorConstants.OTP_CODE_MIN_LENGTH + "-" +
                        IdentityMgtConstants.OTPGeneratorConstants.OTP_CODE_MAX_LENGTH + ". Hence using default length for OTP");
            }
        }
        StringBuilder charSet = new StringBuilder();
        if (!useNumeric && !useUppercaseLetters && !useLowercaseLetters) {
            charSet.append(IdentityMgtConstants.OTPGeneratorConstants.OTP_GENERATE_ALPHABET_CHAR_SET_UPPERCASE);
            charSet.append(IdentityMgtConstants.OTPGeneratorConstants.OTP_GENERATE_ALPHABET_CHAR_SET_LOWERCASE);
            charSet.append(IdentityMgtConstants.OTPGeneratorConstants.OTP_GENERATE_NUMERIC_CHAR_SET_WITHOUT_ZERO);
            return generateOTP(charSet.toString(), otpLength, recoveryScenario);
        }
        if (useUppercaseLetters) {
            charSet.append(IdentityMgtConstants.OTPGeneratorConstants.OTP_GENERATE_ALPHABET_CHAR_SET_UPPERCASE);
        }
        if (useLowercaseLetters) {
            charSet.append(IdentityMgtConstants.OTPGeneratorConstants.OTP_GENERATE_ALPHABET_CHAR_SET_LOWERCASE);
        }
        if (useNumeric) {
            if (useUppercaseLetters || useLowercaseLetters) {
                charSet.append(IdentityMgtConstants.OTPGeneratorConstants.OTP_GENERATE_NUMERIC_CHAR_SET_WITHOUT_ZERO);
            } else {
                charSet.append(IdentityMgtConstants.OTPGeneratorConstants.OTP_GENERATE_NUMERIC_CHAR_SET_WITH_ZERO);
            }

        }
        return generateOTP(charSet.toString(), otpLength, recoveryScenario);
    }

    /**
     * Generates the OTP based on the provided charSet and length.
     *
     * @param charSet               Character set allowed for OTP.
     * @param otpLength             Length of OTP.
     * @param recoveryScenario      Recovery Scenario.
     * @return Value of OTP string.
     */
    @Override
    public String generateOTP(String charSet, int otpLength, String recoveryScenario) {

        SecureRandom secureRandom = new SecureRandom();
        StringBuilder stringBuilder = new StringBuilder();
        char[] otpCharacters = charSet.toCharArray();
        for (int otpCharacterIndex = 0; otpCharacterIndex < otpLength; otpCharacterIndex++) {
            stringBuilder.append(otpCharacters[secureRandom.nextInt(otpCharacters.length)]);
        }
        return stringBuilder.toString();
    }

    /**
     * Retrieve the OTP Generator name.
     */
    @Override
    public String getOTPGeneratorName() {

        return OTP_GENERATOR_IMPL;
    }
}
