package org.wso2.carbon.identity.recovery.endpoint.impl;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.RecaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.CaptchaResponseTokenDTO;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import javax.ws.rs.core.Response;

public class RecaptchaApiServiceImpl extends RecaptchaApiService {

    public static final String PROPERTY_PASSWORD_RECOVERY_RECAPTCHA_ENABLE = "Recovery.Password.ReCaptcha.Enable";
    public static final String PROPERTY_USERNAME_RECOVERY_RECAPTCHA_ENABLE = "Recovery.Username.ReCaptcha.Enable";
    private static final Log LOG = LogFactory.getLog(RecaptchaApiServiceImpl.class);

    @Override
    public Response getRecaptcha(String recoveryType, String tenantDomain) {

        Properties properties = getCaptchaConfigs();

        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants
                .RE_CAPTCHA_ENABLED));

        if (checkConfiguration(tenantDomain, recoveryType) && reCaptchaEnabled) {
            String reCaptchaSiteKey = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY);
            if (StringUtils.isBlank(reCaptchaSiteKey)) {
                RecoveryUtil.handleBadRequest(CaptchaConstants.RE_CAPTCHA_SITE_KEY + " is not found ",
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }

            String reCaptchaAPIUrl = properties.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL);
            if (StringUtils.isBlank(reCaptchaAPIUrl)) {
                RecoveryUtil.handleBadRequest(CaptchaConstants.RE_CAPTCHA_API_URL + " is not found ",
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }
            return Response.ok().entity("ReCaptcha generated").header("reCaptcha", reCaptchaEnabled).
                    header("reCaptchaKey", reCaptchaSiteKey).
                    header("reCaptchaAPI", reCaptchaAPIUrl).build();
        } else {
            return Response.ok().entity("ReCaptcha is disabled").header("reCaptcha", reCaptchaEnabled).build();
        }
    }

    @Override
    public Response verifyRecaptcha(CaptchaResponseTokenDTO gRecaptchaResponse, String tenantDomain) {

        Properties properties = getCaptchaConfigs();

        boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants.RE_CAPTCHA_ENABLED));

        if (reCaptchaEnabled) {
            String reCaptchaSecretKey = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY);
            if (StringUtils.isBlank(reCaptchaSecretKey)) {
                RecoveryUtil.handleBadRequest(CaptchaConstants.RE_CAPTCHA_SECRET_KEY + " is not found ",
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }

            String reCaptchaVerifyUrl = properties.getProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL);
            if (StringUtils.isBlank(reCaptchaVerifyUrl)) {
                RecoveryUtil.handleBadRequest(CaptchaConstants.RE_CAPTCHA_VERIFY_URL + " is not found ",
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }

            CloseableHttpClient httpclient = HttpClientBuilder.create().useSystemProperties().build();
            HttpPost httppost = new HttpPost(reCaptchaVerifyUrl);

            List<BasicNameValuePair> params = Arrays.asList(
                    new BasicNameValuePair("secret", reCaptchaSecretKey),
                    new BasicNameValuePair("response", gRecaptchaResponse.getToken()));
            httppost.setEntity(new UrlEncodedFormEntity(params, StandardCharsets.UTF_8));

            HttpResponse response = null;
            try {
                response = httpclient.execute(httppost);
            } catch (IOException e) {
                RecoveryUtil.handleBadRequest("Unable to get the verification response." + e.getMessage(),
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }

            HttpEntity entity = response.getEntity();
            if (entity == null) {
                RecoveryUtil.handleBadRequest("reCaptcha verification response is not received.",
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }

            try {
                try (InputStream in = entity.getContent()) {
                    JsonObject verificationResponse = new JsonParser().parse(IOUtils.toString(in)).getAsJsonObject();
                    if (verificationResponse == null || verificationResponse.get("success") == null ||
                            !verificationResponse.get("success").getAsBoolean()) {
                        return Response.ok().entity("ReCaptcha verification failed. Please try again.").
                                header("VerifiedReCaptcha", false).build();
                    }
                    return Response.ok().entity("ReCaptcha is verified. ").header("VerifiedReCaptcha",
                            verificationResponse.get("success").getAsBoolean()).build();
                }
            } catch (IOException e) {
                RecoveryUtil.handleBadRequest("Unable to read the verification response. " + e,
                        Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }
        }
        return Response.ok().entity("ReCaptcha is disabled").header("reCaptcha", reCaptchaEnabled).build();
    }

    private Properties getCaptchaConfigs() {

        Path path = Paths.get(getCarbonHomeDirectory().toString(), "repository", "conf", "identity",
                CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME);

        Properties properties = new Properties();

        if (Files.exists(path)) {
            try (Reader in = new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)) {
                properties.load(in);
            } catch (IOException e) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug("Error while loading '" + CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME
                            + "' configuration file", e);
                }
                RecoveryUtil.handleBadRequest("Error while loading '" + CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME
                        + "' configuration file", Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
            }
        }
        return properties;
    }

    private Path getCarbonHomeDirectory() {

        return Paths.get(System.getProperty(CaptchaConstants.CARBON_HOME));
    }

    private boolean checkConfiguration(String tenantDomain, String recoveryType) {

        String RECOVERY_RECAPTCHA_ENABLE = null;

        if (recoveryType.equals("username-recovery")) {
            RECOVERY_RECAPTCHA_ENABLE = PROPERTY_USERNAME_RECOVERY_RECAPTCHA_ENABLE;
        } else if (recoveryType.equals("password-recovery")) {
            RECOVERY_RECAPTCHA_ENABLE = PROPERTY_PASSWORD_RECOVERY_RECAPTCHA_ENABLE;
        }

        Property[] connectorConfigs = new Property[0];

        IdentityGovernanceService identityGovernanceService = getIdentityGovernanceService();

        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{RECOVERY_RECAPTCHA_ENABLE},
                    tenantDomain);
        } catch (IdentityGovernanceException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Error while retrieving resident Idp configurations.", e);
            }
            RecoveryUtil.handleBadRequest("Error while retrieving resident Idp configurations.",
                    Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT);
        }

        String enable = null;
        for (Property connectorConfig : connectorConfigs) {
            if ((RECOVERY_RECAPTCHA_ENABLE).equals(connectorConfig.getName())) {
                enable = connectorConfig.getValue();
            }
        }
        return Boolean.parseBoolean(enable);
    }

    protected IdentityGovernanceService getIdentityGovernanceService() {

        PrivilegedCarbonContext privilegedCarbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
        IdentityGovernanceService identityGovernanceService = (IdentityGovernanceService) privilegedCarbonContext.
                getOSGiService(IdentityGovernanceService.class, null);
        return identityGovernanceService;
    }
}
