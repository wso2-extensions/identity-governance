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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

public class EmailUtilTest {

    private MockedStatic<IdentityUtil> identityUtilMock;
    private static final int DAY_DELTA_TOLERANCE = 1;

    @BeforeMethod
    public void setUp() {

        identityUtilMock = mockStatic(IdentityUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        identityUtilMock.close();
    }

    @Test
    public void testCalculateRemainingDaysWithDefaultPattern() throws Exception {

        int offsetDays = 9;
        String pattern = "dd-MM-yyyy";
        String suspensionDate = futureDateAsString(offsetDays, pattern);

        long actual = invokeCalculateRemainingDays(suspensionDate, pattern);

        assertWithinTolerance(actual, offsetDays);
    }

    @Test
    public void testCalculateRemainingDaysWithMMddyyyy() throws Exception {

        int offsetDays = 9;
        String pattern = "MM-dd-yyyy";
        String suspensionDate = futureDateAsString(offsetDays, pattern);

        long actual = invokeCalculateRemainingDays(suspensionDate, pattern);

        assertWithinTolerance(actual, offsetDays);
    }

    @DataProvider(name = "round-trip-patterns")
    public Object[][] roundTripPatterns() {

        return new Object[][] {
                {"dd-MM-yyyy"},
                {"MM-dd-yyyy"},
                {"yyyy/MM/dd"},
                {"yyyy-MM-dd"},
                {"dd MMM yyyy"},
        };
    }

    @Test(dataProvider = "round-trip-patterns")
    public void testCalculateRemainingDaysRoundTrip(String pattern) throws Exception {

        int offsetDays = 7;
        String suspensionDate = futureDateAsString(offsetDays, pattern);

        long actual = invokeCalculateRemainingDays(suspensionDate, pattern);

        assertWithinTolerance(actual, offsetDays);
    }

    @Test
    public void testCalculateRemainingDaysWithPastSuspensionDate() throws Exception {

        int offsetDays = -3;
        String pattern = "dd-MM-yyyy";
        String suspensionDate = futureDateAsString(offsetDays, pattern);

        long actual = invokeCalculateRemainingDays(suspensionDate, pattern);

        assertWithinTolerance(actual, offsetDays);
    }

    @Test(expectedExceptions = ParseException.class)
    public void testCalculateRemainingDaysThrowsOnUnparseable() throws Throwable {

        try {
            invokeCalculateRemainingDays("not-a-date-at-all", "dd-MM-yyyy");
        } catch (InvocationTargetException e) {
            throw e.getCause();
        }
    }

    @Test
    public void testResolverProducesSamePatternForFormatterAndParser() {

        String configuredPattern = "MM-dd-yyyy";
        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG))
                .thenReturn(configuredPattern);

        String resolved = NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat();
        assertEquals(resolved, configuredPattern);

        try {
            String suspensionDate = futureDateAsString(9, resolved);
            long days = invokeCalculateRemainingDays(suspensionDate, resolved);
            assertWithinTolerance(days, 9);
        } catch (Exception e) {
            fail("Round-trip with resolver-provided pattern threw: " + e, e);
        }
    }

    @Test
    public void testResolverDefaultFallbackKeepsParserOnDdMMyyyy() throws Exception {

        when(IdentityUtil.getProperty(NotificationConstants.SUSPENSION_DATE_FORMAT_CONFIG))
                .thenReturn(null);

        String resolved = NotificationReceiversRetrievalUtil.resolveSuspensionDateFormat();
        assertEquals(resolved, NotificationConstants.DEFAULT_SUSPENSION_DATE_FORMAT);

        String suspensionDate = futureDateAsString(5, resolved);
        long days = invokeCalculateRemainingDays(suspensionDate, resolved);
        assertWithinTolerance(days, 5);
    }

    private long invokeCalculateRemainingDays(String suspensionDate, String dateFormat) throws Exception {

        Method m = EmailUtil.class.getDeclaredMethod("calculateRemainingDays", String.class, String.class);
        m.setAccessible(true);
        return Long.parseLong((String) m.invoke(new EmailUtil(), suspensionDate, dateFormat));
    }

    private String futureDateAsString(int offsetDays, String pattern) {

        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_YEAR, offsetDays);
        return new SimpleDateFormat(pattern).format(cal.getTime());
    }

    private void assertWithinTolerance(long actual, int expected) {

        long diff = Math.abs(actual - expected);
        assertTrue(diff <= DAY_DELTA_TOLERANCE,
                "actual=" + actual + " expected=~" + expected + " diff=" + diff);
    }
}
