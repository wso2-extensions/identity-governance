/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
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

package org.wso2.carbon.identity.auth.attribute.handler.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Model class that represent a validation result.
 */
public class ValidationResult {

    private boolean isValid;
    private List<ValidationFailureReason> validationFailureReasons = new ArrayList<>();

    public ValidationResult() {

    }

    public ValidationResult(boolean isValid) {

        this.isValid = isValid;
    }

    public ValidationResult(boolean isValid, List<ValidationFailureReason> validationFailureReasons) {

        this.isValid = isValid;
        this.validationFailureReasons = validationFailureReasons;
    }

    public boolean isValid() {

        return isValid;
    }

    public void setValid(boolean valid) {

        isValid = valid;
    }

    public List<ValidationFailureReason> getValidationFailureReasons() {

        return validationFailureReasons;
    }

    public void setValidationFailureReasons(List<ValidationFailureReason> validationFailureReasons) {

        this.validationFailureReasons = validationFailureReasons;
    }
}
