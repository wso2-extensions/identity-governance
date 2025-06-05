/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.captcha.provider_mgt.service.impl;

import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.provider_mgt.provider.AbstractCaptchaProvider;
import org.wso2.carbon.identity.captcha.provider_mgt.service.CaptchaRuntimeService;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class CaptchaRuntimeServiceImpl implements CaptchaRuntimeService {

    @Override
    public boolean isCaptchaEnabled() {

        return CaptchaDataHolder.getInstance().getCaptchaConfigService().isCaptchaEnabled();
    }

    @Override
    public boolean verifyCaptcha(String captchaResponse) throws CaptchaException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.verifyCaptcha(captchaResponse);
            }

        }
        return false;
    }

    @Override
    public String getCaptchaSiteKey() throws CaptchaServerException {

        return CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaSiteKey();
    }

    @Override
    public String getCaptchaProjectID() throws CaptchaServerException {

        return CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProjectID();
    }

    @Override
    public String getCaptchaApiKey() throws CaptchaServerException {

        return CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaApiKey();
    }

    @Override
    public String getCaptchaSiteSecret() throws CaptchaServerException {

        return CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaSiteSecret();
    }

    @Override
    public Map<String, String> getWidgetAttributes() throws CaptchaServerException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.getWidgetAttributes();
            }

        }
        return Collections.emptyMap();
    }

    @Override
    public List<Map<String, String>> getScriptAttributes() throws CaptchaServerException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.getScriptAttributes();
            }
        }
        return Collections.emptyList();
    }

    @Override
    public String getCaptchaIdentifier() throws CaptchaServerException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.getCaptchaIdentifier();
            }
        }
        return "";
    }

    @Override
    public String getCaptchaResponseIdentifier() throws CaptchaServerException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.getCaptchaResponseIdentifier();
            }
        }
        return "";
    }

    @Override
    public Map<String, String> getCaptchaFunctions() throws CaptchaServerException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.getCaptchaFunctions();
            }
        }
        return Collections.emptyMap();
    }

    @Override
    public String getCaptchaApiUrl() throws CaptchaServerException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.getCaptchaApiUrl();
            }
        }
        return "";
    }

    @Override
    public String getCaptchaVerifyUrl() throws CaptchaServerException {

        if (isCaptchaEnabled()) {
            AbstractCaptchaProvider captchaProvider =
                    CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaProvider();

            if (captchaProvider != null) {
                return captchaProvider.getCaptchaVerifyUrl();
            }
        }
        return "";
    }

    @Override
    public String getCaptchaScoreThreshold() {

        return CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaScoreThreshold();
    }

    @Override
    public String getCaptchaWarnScoreThreshold() {

        return CaptchaDataHolder.getInstance().getCaptchaConfigService().getCaptchaWarnScoreThreshold();
    }
}
