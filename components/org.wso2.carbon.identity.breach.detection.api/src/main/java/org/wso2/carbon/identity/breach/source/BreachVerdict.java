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

import java.util.Optional;
import java.util.OptionalLong;

/**
 * What one source concluded, and - when it could not conclude anything - why.
 * <p>
 * Occurrence counts are operator- and telemetry-facing only. They are deliberately never shown to an end user:
 * a count invites treating a smaller number as safer and offers nothing actionable.
 */
public final class BreachVerdict {

    private final Outcome outcome;
    private final String sourceId;
    private final Long occurrences;
    private final UnavailableCause cause;
    private final String detail;

    private BreachVerdict(Outcome outcome, String sourceId, Long occurrences, UnavailableCause cause,
                          String detail) {

        this.outcome = outcome;
        this.sourceId = sourceId;
        this.occurrences = occurrences;
        this.cause = cause;
        this.detail = detail;
    }

    /**
     * The password is in this source's corpus.
     *
     * @param sourceId    reporting source.
     * @param occurrences how many records, or a negative value when the source does not count.
     * @return the verdict.
     */
    public static BreachVerdict found(String sourceId, long occurrences) {

        return new BreachVerdict(Outcome.FOUND, sourceId, occurrences < 0 ? null : occurrences, null, null);
    }

    /**
     * The password is in this source's corpus, with no count available.
     *
     * @param sourceId reporting source.
     * @return the verdict.
     */
    public static BreachVerdict found(String sourceId) {

        return new BreachVerdict(Outcome.FOUND, sourceId, null, null, null);
    }

    /**
     * The source positively determined the password is absent. Never returned for "could not check".
     *
     * @param sourceId reporting source.
     * @return the verdict.
     */
    public static BreachVerdict notFound(String sourceId) {

        return new BreachVerdict(Outcome.NOT_FOUND, sourceId, null, null, null);
    }

    /**
     * The source could not produce a verdict.
     *
     * @param sourceId reporting source.
     * @param cause    why, so telemetry can distinguish a quota from an outage.
     * @param detail   operator-facing detail. Must never contain the credential or a source credential.
     * @return the verdict.
     */
    public static BreachVerdict unavailable(String sourceId, UnavailableCause cause, String detail) {

        return new BreachVerdict(Outcome.UNAVAILABLE, sourceId, null,
                cause == null ? UnavailableCause.INTERNAL : cause, detail);
    }

    public Outcome getOutcome() {

        return outcome;
    }

    public String getSourceId() {

        return sourceId;
    }

    public OptionalLong getOccurrences() {

        return occurrences == null ? OptionalLong.empty() : OptionalLong.of(occurrences);
    }

    public Optional<UnavailableCause> getCause() {

        return Optional.ofNullable(cause);
    }

    /**
     * @return operator-facing detail; never contains any part of the candidate password.
     */
    public Optional<String> getDetail() {

        return Optional.ofNullable(detail);
    }

    @Override
    public String toString() {

        return "BreachVerdict{sourceId=" + sourceId + ", outcome=" + outcome
                + (cause == null ? "" : ", cause=" + cause) + '}';
    }
}
