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

package org.wso2.carbon.identity.breach.detection.mgt;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Counters for one source in one organization. Carries no password, no digest, and no fragment of either.
 */
public final class SourceStats {

    private final long evaluations;
    private final long found;
    private final long notFound;
    private final long unavailable;
    private final Map<String, Long> unavailableByCause;
    private final long todayEvaluations;
    private final long todayFound;
    private final long todayUnavailable;
    private final long maxLatencyMs;
    private final long averageLatencyMs;

    public SourceStats(long evaluations, long found, long notFound, long unavailable,
                       Map<String, Long> unavailableByCause, long todayEvaluations, long todayFound,
                       long todayUnavailable, long maxLatencyMs, long averageLatencyMs) {

        this.evaluations = evaluations;
        this.found = found;
        this.notFound = notFound;
        this.unavailable = unavailable;
        this.unavailableByCause = Collections.unmodifiableMap(new LinkedHashMap<>(unavailableByCause));
        this.todayEvaluations = todayEvaluations;
        this.todayFound = todayFound;
        this.todayUnavailable = todayUnavailable;
        this.maxLatencyMs = maxLatencyMs;
        this.averageLatencyMs = averageLatencyMs;
    }

    public long getEvaluations() {

        return evaluations;
    }

    public long getFound() {

        return found;
    }

    public long getNotFound() {

        return notFound;
    }

    public long getUnavailable() {

        return unavailable;
    }

    /**
     * @return unavailable counts broken out by cause, so a quota is distinguishable from a transport failure.
     */
    public Map<String, Long> getUnavailableByCause() {

        return unavailableByCause;
    }

    public long getTodayEvaluations() {

        return todayEvaluations;
    }

    public long getTodayFound() {

        return todayFound;
    }

    public long getTodayUnavailable() {

        return todayUnavailable;
    }

    public long getMaxLatencyMs() {

        return maxLatencyMs;
    }

    public long getAverageLatencyMs() {

        return averageLatencyMs;
    }
}
