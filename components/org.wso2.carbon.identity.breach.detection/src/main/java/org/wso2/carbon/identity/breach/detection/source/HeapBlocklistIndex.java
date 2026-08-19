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

package org.wso2.carbon.identity.breach.detection.source;

import java.util.Set;

/**
 * A curated list, held in heap.
 * <p>
 * Chosen when the file is at or below the memory-map threshold. Tens of thousands of entries cost little and
 * answer in O(1); the same structure for a full offline corpus is how a feature ends up in a customer's heap
 * dump, which is what the other regime exists to prevent.
 */
public class HeapBlocklistIndex implements BlocklistIndex {

    private final Set<String> digests;

    HeapBlocklistIndex(Set<String> digests) {

        this.digests = digests;
    }

    @Override
    public boolean contains(String uppercaseHexDigest) {

        return digests.contains(uppercaseHexDigest);
    }

    @Override
    public long size() {

        return digests.size();
    }

    @Override
    public String getRegime() {

        return "in-heap hash set";
    }

    @Override
    public void close() {

        digests.clear();
    }
}
