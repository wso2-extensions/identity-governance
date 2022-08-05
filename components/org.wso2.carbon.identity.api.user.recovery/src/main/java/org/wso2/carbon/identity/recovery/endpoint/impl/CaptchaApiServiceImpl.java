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
import com.google.gson.JsonParser;
import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.recovery.endpoint.CaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaPropertiesDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaResponseTokenDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaVerificationResponseDTO;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import javax.ws.rs.core.Response;

/**
 * This class provides ReCaptcha details and verify the ReCaptcha response for the Captcha API.
 */
public class CaptchaApiServiceImpl extends CaptchaApiService {

    private static final String SUCCESS = "success";
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
            return Response.ok(reCaptchaPropertiesDTO).build();
        } else {
            reCaptchaPropertiesDTO.setReCaptchaEnabled(false);
            return Response.ok(reCaptchaPropertiesDTO).build();
        }
    }

    @Override
    public Response verifyCaptcha(ReCaptchaResponseTokenDTO reCaptchaResponse, String captchaType, String tenantDomain) {

        if (!captchaType.equals(RECAPTCHA)) {
            RecoveryUtil.handleBadRequest(String.format("Invalid captcha type : %s", captchaType), Constants.INVALID);
        }

        Properties properties = RecoveryUtil.getValidatedCaptchaConfigs();
        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants.RE_CAPTCHA_ENABLED));

        if (!reCaptchaEnabled) {
            RecoveryUtil.handleBadRequest("ReCaptcha is disabled", Constants.INVALID);
        }

        HttpResponse response = RecoveryUtil.makeCaptchaVerificationHttpRequest(reCaptchaResponse, properties);
        HttpEntity entity = response.getEntity();
        ReCaptchaVerificationResponseDTO reCaptchaVerificationResponseDTO = new ReCaptchaVerificationResponseDTO();

        try {
            if (entity == null) {
                RecoveryUtil.handleBadRequest("ReCaptcha verification response is not received.",
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            } else {
                try (InputStream in = entity.getContent()) {
                    JsonObject verificationResponse = new JsonParser().parse(IOUtils.toString(in)).getAsJsonObject();
                    reCaptchaVerificationResponseDTO.setSuccess(verificationResponse.get(SUCCESS).getAsBoolean());
                }
            }
        } catch (IOException e) {
            log.error("Unable to read the verification response.", e);
            RecoveryUtil.handleBadRequest("Unable to read the verification response.",
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }
        return Response.ok(reCaptchaVerificationResponseDTO).build();
    }
}
