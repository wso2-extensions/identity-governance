/*
 *
 *  Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.recovery.endpoint.impl;

import com.google.gson.JsonObject;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.recovery.endpoint.CaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaPropertiesDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaResponseTokenDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaVerificationResponseDTO;

import java.util.Properties;

import javax.ws.rs.core.Response;

/**
 * This class provides ReCaptcha details and verify the ReCaptcha response for the Captcha API.
 */
public class CaptchaApiServiceImpl extends CaptchaApiService {

    private static final Log log = LogFactory.getLog(CaptchaApiServiceImpl.class);
    private final String RECAPTCHA = "ReCaptcha";

    @Override
    public Response getCaptcha(String captchaType, String recoveryType, String tenantDomain) {

        if (!captchaType.equals(RECAPTCHA)) {
            RecoveryUtil.handleBadRequest(String.format("Invalid captcha type : %s", captchaType), Constants.INVALID);
        }

        Properties properties = RecoveryUtil.getValidatedCaptchaConfigs();
        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants.RE_CAPTCHA_ENABLED));
        boolean forcefullyEnabledRecaptchaForAllTenants =
                Boolean.valueOf(properties.getProperty(CaptchaConstants.FORCEFULLY_ENABLED_RECAPTCHA_FOR_ALL_TENANTS));
        ReCaptchaPropertiesDTO reCaptchaPropertiesDTO = new ReCaptchaPropertiesDTO();

        if (reCaptchaEnabled && (forcefullyEnabledRecaptchaForAllTenants ||
                RecoveryUtil.checkCaptchaEnabledResidentIdpConfiguration(tenantDomain, recoveryType))) {
            reCaptchaPropertiesDTO.setReCaptchaEnabled(true);
            reCaptchaPropertiesDTO.setReCaptchaKey(properties.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY));
            reCaptchaPropertiesDTO.setReCaptchaAPI(properties.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL));
            reCaptchaPropertiesDTO.setReCaptchaType(properties.getProperty(CaptchaConstants.RE_CAPTCHA_TYPE));
        } else {
            reCaptchaPropertiesDTO.setReCaptchaEnabled(false);
        }
        return Response.ok(reCaptchaPropertiesDTO).build();
    }

    @Override
    public Response verifyCaptcha(ReCaptchaResponseTokenDTO reCaptchaResponse, String captchaType, String tenantDomain) {

        if (!captchaType.equals(RECAPTCHA)) {
            RecoveryUtil.handleBadRequest(String.format("Invalid captcha type : %s", captchaType), Constants.INVALID);
        }

        Properties properties = RecoveryUtil.getValidatedCaptchaConfigs();
        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants.RE_CAPTCHA_ENABLED));
        String reCaptchaType = properties.getProperty(CaptchaConstants.RE_CAPTCHA_TYPE);

        if (!reCaptchaEnabled) {
            RecoveryUtil.handleBadRequest("ReCaptcha is disabled", Constants.INVALID);
        }

        ReCaptchaVerificationResponseDTO reCaptchaVerificationResponseDTO = new ReCaptchaVerificationResponseDTO();

        JsonObject verificationResponse = RecoveryUtil.makeCaptchaVerificationHttpClient5Request(
                reCaptchaResponse, properties);
        if (verificationResponse == null) {
            RecoveryUtil.handleBadRequest("ReCaptcha verification response is not received.",
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }
        if (CaptchaConstants.RE_CAPTCHA_TYPE_ENTERPRISE.equals(reCaptchaType)) {
            // For Recaptcha Enterprise.
            JsonObject tokenProperties = verificationResponse.get(CaptchaConstants.CAPTCHA_TOKEN_PROPERTIES)
                    .getAsJsonObject();
            boolean success = tokenProperties.get(CaptchaConstants.CAPTCHA_VALID).getAsBoolean();
            reCaptchaVerificationResponseDTO.setSuccess(success);
        } else {
            // For ReCaptcha v2 and v3.
            reCaptchaVerificationResponseDTO.setSuccess(verificationResponse.get(
                    CaptchaConstants.CAPTCHA_SUCCESS).getAsBoolean());
        }
        return Response.ok(reCaptchaVerificationResponseDTO).build();
    }
}
