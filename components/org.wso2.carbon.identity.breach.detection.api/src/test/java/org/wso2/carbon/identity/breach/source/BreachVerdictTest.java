/*
 * Copyright (c) 2026, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.breach.source;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Unavailable is not not-found. Collapsing the two is the defect the whole design is organised against, so it
 * gets a test rather than a comment.
 */
public class BreachVerdictTest {

    @Test
    public void unavailableIsItsOwnOutcomeAndCarriesACause() {

        BreachVerdict verdict = BreachVerdict.unavailable("hibp", UnavailableCause.TIMEOUT, "no answer");
        assertEquals(verdict.getOutcome(), Outcome.UNAVAILABLE);
        assertEquals(verdict.getCause().orElse(null), UnavailableCause.TIMEOUT);
        assertFalse(verdict.getOccurrences().isPresent());
    }

    @Test
    public void anUnavailableVerdictWithNoCauseStillHasOne() {

        assertEquals(BreachVerdict.unavailable("x", null, null).getCause().orElse(null),
                UnavailableCause.INTERNAL);
    }

    @Test
    public void foundCarriesOccurrencesOnlyWhenTheSourceCounts() {

        assertEquals(BreachVerdict.found("hibp", 612953).getOccurrences().getAsLong(), 612953L);
        assertFalse(BreachVerdict.found("localList").getOccurrences().isPresent());
        assertFalse(BreachVerdict.found("localList", -1).getOccurrences().isPresent());
    }

    @Test
    public void notFoundIsPositiveAndCarriesNoCause() {

        BreachVerdict verdict = BreachVerdict.notFound("localList");
        assertEquals(verdict.getOutcome(), Outcome.NOT_FOUND);
        assertFalse(verdict.getCause().isPresent());
    }

    @Test
    public void theStringFormCarriesNoCredentialAndNoCount() {

        String text = BreachVerdict.found("hibp", 612953).toString();
        assertTrue(text.contains("hibp"));
        assertFalse(text.contains("612953"));
    }
}
