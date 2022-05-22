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

package org.wso2.carbon.identity.governance.internal.service.impl.otp;

import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.service.otp.OTPGenerator;

import java.security.SecureRandom;

/**
 * Default class to generate OTP.
 */
public class DefaultOTPGenerator implements OTPGenerator {

    public DefaultOTPGenerator() {}

    @Override
    public String generateOTP(String charSet, int otpLength) {

        char[] chars = charSet.toCharArray();
        SecureRandom rnd = new SecureRandom();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < otpLength; i++) {
            sb.append(chars[rnd.nextInt(chars.length)]);
        }
        return sb.toString();
    }

    @Override
    public String generateOTP(boolean isNumeric, boolean isUpperCase, boolean isLowerCase, int otpLength) {

        StringBuilder charSet = new StringBuilder();
        if (isUpperCase) {
            charSet.append(IdentityMgtConstants.SMS_OTP_GENERATE_ALPHABET_CHAR_SET);
        }
        if (isLowerCase) {
            charSet.append(IdentityMgtConstants.SMS_OTP_GENERATE_ALPHABET_CHAR_SET.toLowerCase());
        }
        if (isNumeric) {
            charSet.append(IdentityMgtConstants.SMS_OTP_GENERATE_NUMERIC_CHAR_SET);
        }
        if (!isNumeric && !isUpperCase && !isLowerCase) {
            charSet.append(IdentityMgtConstants.SMS_OTP_GENERATE_CHAR_SET);
        }
        return generateOTP(charSet.toString(), otpLength);
    }
}
