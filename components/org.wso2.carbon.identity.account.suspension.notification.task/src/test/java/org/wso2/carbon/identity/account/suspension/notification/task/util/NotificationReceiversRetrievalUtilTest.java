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

import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.core.util.IdentityUtil;

import java.text.SimpleDateFormat;
import java.util.Date;

import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

public class NotificationReceiversRetrievalUtilTest {

    private MockedStatic<IdentityUtil> identityUtilMock;

    @BeforeMethod
    public void setUp() {

        identityUtilMock = mockStatic(IdentityUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        identityUtilMock.close();
    }

    @Test
    public void testResolveSuspensionDateFormatReturnsDefaultWhenPropertyNotSet() {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG)).thenReturn(null);

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
        assertEquals(NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT, "dd-MM-yyyy");
    }

    @Test
    public void testResolveSuspensionDateFormatReturnsDefaultWhenPropertyEmpty() {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG)).thenReturn("");

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
    }

    @Test
    public void testResolveSuspensionDateFormatReturnsDefaultWhenPropertyWhitespace() {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG)).thenReturn("   ");

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(),
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
    public void testResolveSuspensionDateFormatReturnsConfiguredPattern(String configuredPattern) {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG))
                .thenReturn(configuredPattern);

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(), configuredPattern);
    }

    @DataProvider(name = "invalidPatterns")
    public Object[][] invalidPatterns() {

        return new Object[][] {
                {"invalid-[[pattern"},
                {"yyyy-MM-dd 'unterminated"}
        };
    }

    @Test(dataProvider = "invalidPatterns")
    public void testResolveSuspensionDateFormatFallsBackOnInvalidPattern(String invalidPattern) {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG))
                .thenReturn(invalidPattern);

        assertEquals(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat(),
                NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);
    }

    // Date(126, 4, 17) = 2026-05-17 anchored at local midnight (timezone-stable for the assertion).
    @Test
    public void testConfiguredPatternIsAppliedWhenFormattingExpireDate() {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG))
                .thenReturn("MM-dd-yyyy");

        Date someDate = new Date(126, 4, 17);
        String rendered = new SimpleDateFormat(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat())
                .format(someDate);

        assertEquals(rendered, "05-17-2026");
    }

    @Test
    public void testDefaultPatternIsAppliedWhenFormattingExpireDate() {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG)).thenReturn(null);

        Date someDate = new Date(126, 4, 17);
        String rendered = new SimpleDateFormat(NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat())
                .format(someDate);

        assertEquals(rendered, "17-05-2026");
    }

    // Pin customer-facing TOML/XML key + default value — patch contract surface.
    @Test
    public void testConstantsForCustomerFacingContract() {

        assertEquals(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG,
                "AccountSuspension.SuspensionDateFormat");
        assertEquals(NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT, "dd-MM-yyyy");
    }
}
