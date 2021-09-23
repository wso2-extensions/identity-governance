package org.wso2.carbon.identity.captcha.connector.kaptcha;

import com.google.code.kaptcha.Producer;
import com.google.code.kaptcha.util.Config;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Properties;
import javax.imageio.ImageIO;
import javax.servlet.http.HttpSession;

public class KaptchaImageProvider {

    private static final Log LOG = LogFactory.getLog(KaptchaImageProvider.class);

    public String generateImageForSSO(String sessionDataKey) {
       // String sessionDataKey = "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";
        Config config = new Config(new Properties());
        Producer captchaProducer = config.getProducerImpl();
        String captchaText = captchaProducer.createText();
        try {
            byte[] encodedCaptchaImage = generateBase64EncodedCaptchaImage(captchaProducer, captchaText);
            setCaptchaTextToAuthenticationContext(sessionDataKey, captchaText);
            return new String(encodedCaptchaImage);
        } catch (IOException e) {
            LOG.error("Error when creating captcha image for session data key: " + sessionDataKey);
            return null;
        } catch (CaptchaException e) {
            LOG.error("Error when setting captcha text to context " + sessionDataKey);
            return null;
        }
    }

    /**
     * Generate a captcha image and set the text value in the session as a attribute
     * @param session The session object to set the captcha answer as an attribute
     * @return
     */
    public String generateImageForSession(HttpSession session) {

        Config config = new Config(new Properties());
        Producer captchaProducer = config.getProducerImpl();
        String captchaText = captchaProducer.createText();
        try {
            byte[] encodedCaptchaImage = generateBase64EncodedCaptchaImage(captchaProducer, captchaText);
            setCaptchaTextToSession(session, captchaText);
            return new String(encodedCaptchaImage);
        } catch (IOException e) {
            LOG.error("Error when creating captcha image for session");
            return null;
        }
    }

    private byte[] generateBase64EncodedCaptchaImage(Producer captchaProducer, String captchaText) throws IOException {

        BufferedImage image = captchaProducer.createImage(captchaText);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(image, "jpg", baos);
        return Base64.encodeBase64(baos.toByteArray());

    }

    private void setCaptchaTextToAuthenticationContext(String sessionDataKey, String captchaText) throws
            CaptchaException {

        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (context != null) {
            context.setProperty("captchaAnswer", captchaText);
            if (LOG.isDebugEnabled()) {
                LOG.debug(String.format("Captcha text set for session data key : %s, captcha text: %s",
                        sessionDataKey, captchaText));
            }
            return;
        }
        if (LOG.isDebugEnabled()) {
            LOG.debug("Invalid authentication context Id: " + sessionDataKey);
        }
        throw new CaptchaException("Invalid authentication context id.");
    }

    private void setCaptchaTextToSession(HttpSession session, String captchaText) {

        session.setAttribute("captchaAnswer", captchaText);
    }
}
