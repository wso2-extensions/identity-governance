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

/**
 * Class to represent a password expiry rule.
 */
public class PasswordExpiryRule {

    private int priority;
    private int expiryDays;
    private PasswordExpiryRuleAttributeEnum attribute;
    private PasswordExpiryRuleOperatorEnum operator;
    private List<String> values = new ArrayList<>();
    private static final String RULE_SPLIT_REGEX = ",(?=(?:[^']*'[^']*')*[^']*$)";

    public PasswordExpiryRule(String rule) throws IllegalArgumentException{

        try {
            // Rule format: "priority,expiryDays,attribute,operator,value1,value2, ...".
            // At least 5 parts are required in the rule definition.
            int ruleSectionLength = 4;

            String[] ruleSections = rule.split(RULE_SPLIT_REGEX);
            if (ruleSections.length < ruleSectionLength) {
                throw new IllegalArgumentException("Invalid rule format: not enough parts in the rule definition.");
            }

            this.priority = Integer.parseInt(ruleSections[0].trim());
            this.expiryDays = Integer.parseInt(ruleSections[1].trim());
            this.attribute = PasswordExpiryRuleAttributeEnum.fromString(ruleSections[2].trim());
            this.operator = PasswordExpiryRuleOperatorEnum.fromString(ruleSections[3].trim());

            // Extract values from the rule removing quotes if present.
            // Eg: "1,40,roles,eq,'12ec01e1-aa45,8a485d10c8fa',cc40ad49-8435-75fa1b627332".
            for (int i = 4; i < ruleSections.length; i++) {
                String value = ruleSections[i].trim();
                if ((StringUtils.startsWith(value, "'") && StringUtils.endsWith(value, "'")) ||
                        (StringUtils.startsWith(value, "\"") && StringUtils.endsWith(value, "\""))) {
                    value = value.substring(1, value.length() - 1).trim();
                }
                this.values.add(value);
            }

            if (this.values.isEmpty()) {
                throw new IllegalArgumentException("Invalid rule format: no valid values provided.");
            }
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid rule format: " + e.getMessage());
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

    public PasswordExpiryRuleAttributeEnum getAttribute() {

        return attribute;
    }

    public void setAttribute(PasswordExpiryRuleAttributeEnum attribute) {

        this.attribute = attribute;
    }

    public PasswordExpiryRuleOperatorEnum getOperator() {

        return operator;
    }

    public void setOperator(PasswordExpiryRuleOperatorEnum operator) {

        this.operator = operator;
    }

    public List<String> getValues() {

        return values;
    }

    public void setValues(List<String> values) {

        this.values = values;
    }
}
