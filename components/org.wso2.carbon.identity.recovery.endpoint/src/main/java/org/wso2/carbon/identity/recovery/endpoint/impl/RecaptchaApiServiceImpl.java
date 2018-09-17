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
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.RecaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.CaptchaResponseTokenDTO;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import javax.ws.rs.core.Response;

/**
 * This class provides ReCaptcha details and verify the ReCaptcha response for the ReCaptcha API.
 */
public class RecaptchaApiServiceImpl extends RecaptchaApiService {

    public static final String SUCCESS = "success";
    private static final Log log = LogFactory.getLog(RecaptchaApiServiceImpl.class);

    @Override
    public Response getRecaptcha(String recoveryType, String tenantDomain) {

        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = org.wso2.carbon.utils.multitenancy.MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        } else if (!RecoveryUtil.isValidTenantDomain(tenantDomain)) {
            RecoveryUtil.handleBadRequest(String.format("Invalid tenant domain : %s", tenantDomain),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_TENANT.getCode());
        }

        Properties properties = RecoveryUtil.getCaptchaConfigs();

        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants
                .RE_CAPTCHA_ENABLED));

        if (RecoveryUtil.checkResidentIdpConfiguration(tenantDomain, recoveryType) && reCaptchaEnabled) {
            String reCaptchaSiteKey = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY);
            if (StringUtils.isBlank(reCaptchaSiteKey)) {
                RecoveryUtil.handleBadRequest(String.format("%s is not found ", CaptchaConstants.RE_CAPTCHA_SITE_KEY),
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }

            String reCaptchaAPIUrl = properties.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL);
            if (StringUtils.isBlank(reCaptchaAPIUrl)) {
                RecoveryUtil.handleBadRequest(String.format("%s is not found ", CaptchaConstants.RE_CAPTCHA_API_URL),
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }
            return Response.ok().entity("ReCaptcha generated").header("reCaptcha", reCaptchaEnabled).
                    header("reCaptchaKey", reCaptchaSiteKey).
                    header("reCaptchaAPI", reCaptchaAPIUrl).build();
        } else {
            return Response.ok().entity("ReCaptcha is disabled").build();
        }
    }

    @Override
    public Response verifyRecaptcha(CaptchaResponseTokenDTO gRecaptchaResponse, String tenantDomain) {

        Properties properties = RecoveryUtil.getCaptchaConfigs();

        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants.RE_CAPTCHA_ENABLED));

        if (reCaptchaEnabled) {
            HttpResponse response = RecoveryUtil.makeCaptchaVerificationHttpRequest(gRecaptchaResponse, properties);

            HttpEntity entity = response.getEntity();
            if (entity == null) {
                RecoveryUtil.handleBadRequest(String.format("ReCaptcha verification response is not received."),
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }

            try {
                try (InputStream in = entity.getContent()) {
                    JsonObject verificationResponse = new JsonParser().parse(IOUtils.toString(in)).getAsJsonObject();
                    if (verificationResponse == null || verificationResponse.get("success") == null ||
                            !verificationResponse.get(SUCCESS).getAsBoolean()) {
                        return Response.ok().entity("ReCaptcha verification failed. Please try again.").
                                header(SUCCESS, false).build();
                    }
                    return Response.ok().entity("ReCaptcha is verified. ").header(SUCCESS, verificationResponse.
                            get(SUCCESS).getAsBoolean()).build();
                }
            } catch (IOException e) {
                log.error(String.format("Unable to read the verification response."), e);
                RecoveryUtil.handleBadRequest(String.format("Unable to read the verification response."),
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }
        }
        return Response.ok().entity("ReCaptcha is disabled").build();
    }
}
