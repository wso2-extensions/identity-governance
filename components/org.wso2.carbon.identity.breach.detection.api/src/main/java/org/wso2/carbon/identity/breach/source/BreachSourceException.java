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

/**
 * Thrown by a source that could not reach a verdict.
 * <p>
 * The engine treats this identically to a returned {@link Outcome#UNAVAILABLE}: contained to the source that
 * raised it, resolved by that source's failure policy, and never allowed to fail the other sources. The
 * message must carry no part of the candidate password and no source credential, including in the cause
 * chain.
 */
public class BreachSourceException extends Exception {

    private static final long serialVersionUID = 1L;

    private final UnavailableCause cause;

    public BreachSourceException(UnavailableCause cause, String message) {

        super(message);
        this.cause = cause == null ? UnavailableCause.INTERNAL : cause;
    }

    public BreachSourceException(UnavailableCause cause, String message, Throwable throwable) {

        super(message, throwable);
        this.cause = cause == null ? UnavailableCause.INTERNAL : cause;
    }

    public UnavailableCause getUnavailableCause() {

        return cause;
    }
}
