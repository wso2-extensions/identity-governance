/*
 * Copyright (c) 2026, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.account.suspension.notification.task.util;

import org.testng.annotations.AfterMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import java.text.SimpleDateFormat;
import java.util.Date;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

public class NotificationReceiversRetrievalUtilTest {

    private static final String TENANT_DOMAIN = "carbon.super";

    @AfterMethod
    public void tearDown() {

        NotificationTaskDataHolder.getInstance().setIdentityGovernanceService(null);
    }

    @Test
    public void testResolveSuspensionDateFormatReturnsDefaultWhenGovernanceServiceNull() {

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
        assertEquals(NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT, "dd-MM-yyyy");
    }

    @Test
    public void testResolveSuspensionDateFormatReturnsDefaultWhenPropertyNotSet() throws Exception {

        mockGovernanceProperty(null);

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
    }

    @Test
    public void testResolveSuspensionDateFormatReturnsDefaultWhenPropertyEmpty() throws Exception {

        mockGovernanceProperty("");

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
    }

    @Test
    public void testResolveSuspensionDateFormatReturnsDefaultWhenPropertyWhitespace() throws Exception {

        mockGovernanceProperty("   ");

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
    }

    @Test
    public void testResolveSuspensionDateFormatFallsBackWhenGovernanceThrows() throws Exception {

        IdentityGovernanceService service = mock(IdentityGovernanceService.class);
        when(service.getConfiguration(any(String[].class), any(String.class)))
                .thenThrow(new IdentityGovernanceException("test failure"));
        NotificationTaskDataHolder.getInstance().setIdentityGovernanceService(service);

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
    }

    @DataProvider(name = "validPatterns")
    public Object[][] validPatterns() {

        return new Object[][] {
                {"MM-dd-yyyy"},
                {"yyyy/MM/dd"},
                {"yyyy-MM-dd"},
                {"dd MMM yyyy"},
                {"EEEE, MMMM dd, yyyy"}
        };
    }

    @Test(dataProvider = "validPatterns")
    public void testResolveSuspensionDateFormatReturnsConfiguredPattern(String configuredPattern) throws Exception {

        mockGovernanceProperty(configuredPattern);

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN), configuredPattern);
    }

    @DataProvider(name = "invalidPatterns")
    public Object[][] invalidPatterns() {

        return new Object[][] {
                {"invalid-[[pattern"},
                {"yyyy-MM-dd 'unterminated"}
        };
    }

    @Test(dataProvider = "invalidPatterns")
    public void testResolveSuspensionDateFormatFallsBackOnInvalidPattern(String invalidPattern) throws Exception {

        mockGovernanceProperty(invalidPattern);

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
    }

    // Date(126, 4, 17) = 2026-05-17 anchored at local midnight (timezone-stable).
    @Test
    public void testConfiguredPatternIsAppliedWhenFormattingExpireDate() throws Exception {

        mockGovernanceProperty("MM-dd-yyyy");

        Date someDate = new Date(126, 4, 17);
        String rendered = new SimpleDateFormat(
                NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN))
                .format(someDate);

        assertEquals(rendered, "05-17-2026");
    }

    @Test
    public void testDefaultPatternIsAppliedWhenFormattingExpireDate() throws Exception {

        mockGovernanceProperty(null);

        Date someDate = new Date(126, 4, 17);
        String rendered = new SimpleDateFormat(
                NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(TENANT_DOMAIN))
                .format(someDate);

        assertEquals(rendered, "17-05-2026");
    }

    // Pin customer-facing governance property name + default value — patch contract surface.
    @Test
    public void testConstantsForCustomerFacingContract() {

        assertEquals(NotificationConstants.SUSPENSION_NOTIFICATION_DATE_FORMAT,
                "suspension.notification.date.format");
        assertEquals(NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT, "dd-MM-yyyy");
    }

    private void mockGovernanceProperty(String value) throws IdentityGovernanceException {

        IdentityGovernanceService service = mock(IdentityGovernanceService.class);
        Property property = new Property();
        property.setName(NotificationConstants.SUSPENSION_NOTIFICATION_DATE_FORMAT);
        property.setValue(value);
        when(service.getConfiguration(any(String[].class), any(String.class)))
                .thenReturn(new Property[]{property});
        NotificationTaskDataHolder.getInstance().setIdentityGovernanceService(service);
    }
}
