package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.recovery.endpoint.GetRecaptchaApiService;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;
import javax.ws.rs.core.Response;

public class GetRecaptchaApiServiceImpl extends GetRecaptchaApiService {

    @Override
    public Response getRecaptchaGet(String tenantDomain) {

        Path path = Paths.get(getCarbonHomeDirectory().toString(), "repository",
                "conf", "identity", CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME);

        if (Files.exists(path)) {
            Properties properties = new Properties();
            try (Reader in = new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)) {
                properties.load(in);
            } catch (IOException e) {
                throw new RuntimeException("Error while loading '" + CaptchaConstants
                        .CAPTCHA_CONFIG_FILE_NAME + "' configuration file", e);
            }

            boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants
                    .RE_CAPTCHA_ENABLED));

            if (reCaptchaEnabled) {
                String reCaptchaSiteKey = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY);
                if (StringUtils.isBlank(reCaptchaSiteKey)) {
                    throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_SITE_KEY));
                }

                String reCaptchaAPIUrl = properties.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL);
                if (StringUtils.isBlank(reCaptchaAPIUrl)) {
                    throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_API_URL));
                }
                return Response.ok().entity("ReCaptcha generated").header("reCaptcha", reCaptchaEnabled).
                        header("reCaptchaKey", reCaptchaSiteKey).
                        header("reCaptchaAPI", reCaptchaAPIUrl).build();
            } else {
                CaptchaDataHolder.getInstance().setReCaptchaEnabled(false);
                return Response.ok().entity("ReCaptcha is not enabled").header("reCaptcha", reCaptchaEnabled).build();
            }
        }
        return Response.ok().entity("ReCaptcha path is not exists").build();
    }

    private Path getCarbonHomeDirectory() {

        return Paths.get(System.getProperty(CaptchaConstants.CARBON_HOME));
    }

    private String getValidationErrorMessage(String property) {

        return "Invalid value for " + property + " in the " + CaptchaConstants
                .CAPTCHA_CONFIG_FILE_NAME + " file.";
    }
}
