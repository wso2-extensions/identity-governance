/*
 * Copyright (c) 2024, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.password.expiry.models;

import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class PasswordExpiryRule {

    private int priority;
    private int expiryDays;
    private AttributeEnum attribute;
    private OperatorEnum operator;
    private List<String> values;

    public PasswordExpiryRule(int priority, int expiryDays, AttributeEnum attribute, OperatorEnum operator,
                              List<String> values) {

        this.priority = priority;
        this.expiryDays = expiryDays;
        this.attribute = attribute;
        this.operator = operator;
        this.values = values;
    }

    public PasswordExpiryRule(String rule) {

        // Using regex to split by comma but not within quotes.
        String[] parts = rule.split(",(?=(?:[^']*'[^']*')*[^']*$)");

        // Ensure that the rule has at least the basic required parts
        if (parts.length < 4) {
            throw new IllegalArgumentException("Invalid rule format: not enough parts in the rule definition.");
        }

        this.priority = Integer.parseInt(parts[0].trim());
        this.expiryDays = Integer.parseInt(parts[1].trim());
        this.attribute = AttributeEnum.fromString(parts[2].trim());
        this.operator = OperatorEnum.fromString(parts[3].trim());
        this.values = new ArrayList<>();

        for (int i = 4; i < parts.length; i++) {
            String value = parts[i].trim();
            if ((StringUtils.startsWith(value, "'") && StringUtils.endsWith(value, "'")) ||
                    (StringUtils.startsWith(value, "\"") && StringUtils.endsWith(value, "\""))) {
                value = value.substring(1, value.length() - 1).trim();
            }
            this.values.add(value);
        }

        if (this.values.isEmpty()) {
            throw new IllegalArgumentException("Invalid rule format: no valid values provided.");
        }
    }

    public int getPriority() {

        return priority;
    }

    public void setPriority(int priority) {

        this.priority = priority;
    }

    public int getExpiryDays() {

        return expiryDays;
    }

    public void setExpiryDays(int expiryDays) {

        this.expiryDays = expiryDays;
    }

    public AttributeEnum getAttribute() {

        return attribute;
    }

    public void setAttribute(AttributeEnum attribute) {

        this.attribute = attribute;
    }

    public OperatorEnum getOperator() {

        return operator;
    }

    public void setOperator(OperatorEnum operator) {

        this.operator = operator;
    }

    public List<String> getValues() {

        return values;
    }

    public void setValues(List<String> values) {

        this.values = values;
    }
}
