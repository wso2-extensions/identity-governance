package org.wso2.carbon.identity.recovery.listener;

import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.registration.engine.model.RegisteringUser;
import org.wso2.carbon.identity.user.registration.engine.model.RegistrationContext;
import org.wso2.carbon.identity.user.registration.engine.model.RegistrationStep;

import java.util.Collections;
import java.util.Map;

import static org.mockito.Mockito.*;
import static org.testng.Assert.*;

public class SelfRegistrationCompletionListenerTest {

    private SelfRegistrationCompletionListener listener;

    @Mock
    private RegistrationStep step;
    @Mock
    private RegistrationContext context;
    @Mock
    private RegisteringUser user;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        listener = spy(new SelfRegistrationCompletionListener());
    }

    @DataProvider(name = "postContinueScenarios")
    public Object[][] postContinueScenarios() {
        return new Object[][]{
                {"COMPLETE", true, true, true, true, true},   // Verified channel → return true
                {"COMPLETE", false, true, false, true, false}, // Not verified, lock + notify internally
                {"INCOMPLETE", false, false, false, false, false}, // Skipped flow
                {"COMPLETE", false, false, false, false, false},   // No lock, no notify
        };
    }

    @Test(dataProvider = "postContinueScenarios")
    public void testDoPostContinue(String flowStatus, boolean channelVerified,
                                   boolean lockOnCreation, boolean confirmationNotify,
                                   boolean notifyInternally, boolean expectedResult) throws Exception {

        when(step.getFlowStatus()).thenReturn(flowStatus);
        when(context.getRegisteringUser()).thenReturn(user);
        when(context.getTenantDomain()).thenReturn("carbon.super");
        when(user.getUsername()).thenReturn("john@wso2.com");
        when(user.getClaims()).thenReturn(Collections.emptyMap());

        doReturn("PRIMARY").when(listener).resolveUserStoreDomain(any());
        doReturn(mock(org.wso2.carbon.user.core.UserStoreManager.class)).when(listener).getUserStoreManager(any(), any());
        doReturn("EMAIL").when(listener).resolveNotificationChannel(any(), any(), any());
        doReturn(mock(org.wso2.carbon.identity.governance.service.notification.NotificationChannels.class))
                .when(listener).getNotificationChannel(any(), any());
        doReturn(channelVerified).when(listener).isNotificationChannelVerified(any(), any());

        // Connector Configs
        mockStatic(Utils.class);
        when(Utils.getConnectorConfig(eq("account.lock.on.creation"), any())).thenReturn(Boolean.toString(lockOnCreation));
        when(Utils.getConnectorConfig(eq("self.registration.notification.send"), any())).thenReturn(Boolean.toString(confirmationNotify));
        when(Utils.getConnectorConfig(eq("signup.notification.manage"), any())).thenReturn(Boolean.toString(notifyInternally));

        boolean result = listener.doPostContinue(step, context);
        assertEquals(result, expectedResult);
    }
} 
