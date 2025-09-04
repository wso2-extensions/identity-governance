/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.listener;


import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.listener.AbstractFlowExecutionListener;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.UserStoreManager;

import java.util.Map;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.ERROR_CODE_LISTENER_FAILURE;
import static org.wso2.carbon.identity.flow.execution.engine.util.FlowExecutionEngineUtils.handleServerException;


public class AttributeOnboardListener extends AbstractFlowExecutionListener {

    private static final Log log = LogFactory.getLog(AttributeOnboardListener.class);

    @Override
    public boolean isEnabled() {

        return true;
    }

    public boolean doPostExecute(FlowExecutionStep step, FlowExecutionContext context) throws FlowEngineException {

        if (!Constants.COMPLETE.equals(step.getFlowStatus())) {
            return true;
        }

        User user = Utils.resolveUserFromContext(context);
        if (user == null) {

            return false;
        }

        try{

            Map<String, String> userClaims = context.getFlowUser().getClaims();
            int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance()
                    .getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            userStoreManager.setUserClaimValues(domainQualifiedName, userClaims, null);
        } catch ( Exception e) {
            log.error(ERROR_CODE_LISTENER_FAILURE.getMessage(), e);
            throw handleServerException(ERROR_CODE_LISTENER_FAILURE, this.getClass().getSimpleName(),
                    context.getFlowType(), context.getContextIdentifier());
        }
        return true;
    }
}
