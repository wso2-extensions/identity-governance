package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.doReturn;

public class RecaptchaApiServiceImplTest {

    @Test
    public void testGetRecaptcha() throws IdentityGovernanceException {

        System.setProperty("carbon.home", "src/test/resources");
        Property property = new Property();
        property.setName("Recovery.Username.ReCaptcha.Enable");
        property.setValue(String.valueOf(true));
        Property[] properties;
        properties = new Property[]{property};

        IdentityGovernanceService identityGovernanceService = Mockito.mock(IdentityGovernanceService.class);
        RecaptchaApiServiceImpl recaptchaApiService = Mockito.mock(RecaptchaApiServiceImpl.class);
        doReturn(identityGovernanceService).when(recaptchaApiService).getIdentityGovernanceService();
        doReturn(properties).when(identityGovernanceService).getConfiguration(Mockito.any(String[].class), anyString());
        Mockito.doCallRealMethod().when(recaptchaApiService).getRecaptcha(Mockito.anyString(), Mockito.anyString());
        Assert.assertEquals(recaptchaApiService.getRecaptcha("username-recovery", null).getStatus(),
                200);

    }
}
