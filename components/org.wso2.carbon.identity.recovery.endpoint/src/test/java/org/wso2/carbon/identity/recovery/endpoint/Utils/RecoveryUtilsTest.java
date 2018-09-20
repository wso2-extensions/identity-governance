package org.wso2.carbon.identity.recovery.endpoint.Utils;

import org.powermock.api.mockito.PowerMockito;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.recovery.endpoint.Exceptions.BadRequestException;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.testng.Assert.assertEquals;

public class RecoveryUtilsTest {

    @Test
    public void testGetValidatedCaptchaConfigs() throws IOException {

        Path path = Paths.get("src/test/resources", "repository", "conf", "identity",
                CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME);
        Properties sampleProperties = new Properties();

        if (Files.exists(path)) {
            try (Reader in = new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)) {
                sampleProperties.load(in);
            } catch (IOException e) {
                throw new IOException(e);
            }
        }

        System.setProperty("carbon.home", "src/test/resources");
        Properties properties = RecoveryUtil.getValidatedCaptchaConfigs();
        assertEquals(properties, sampleProperties);
        assertEquals(properties.size(), sampleProperties.size());
    }

//    @Test(expectedExceptions = {BadRequestException})
//    public void throw

}
