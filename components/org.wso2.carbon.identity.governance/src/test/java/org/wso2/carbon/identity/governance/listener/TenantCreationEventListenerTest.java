package org.wso2.carbon.identity.governance.listener;

import org.mockito.Mock;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;

import static org.testng.Assert.assertEquals;

public class TenantCreationEventListenerTest {

    TenantCreationEventListener tenantCreationEventListener;
    @Mock
    IdentityMgtServiceDataHolder identityMgtServiceDataHolder;

    @BeforeMethod
    public void setUp() {
        tenantCreationEventListener = new TenantCreationEventListener();
    }

    @Test
    public void testGetListenerOrder() {
        assertEquals(tenantCreationEventListener.getListenerOrder(), 31, "Error returing listener order.");
    }
}
