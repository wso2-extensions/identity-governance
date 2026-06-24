/*
 * Copyright (c) 2026, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.PIICategory;
import org.wso2.carbon.consent.mgt.core.model.PurposePIICategoryBinding;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.consent.mgt.core.util.ConsentReceiptUtils;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.central.log.mgt.utils.LogConstants;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.context.IdentityContext;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineServerException;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.execution.engine.util.FlowExecutionEngineUtils;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.DiagnosticLog;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.wso2.carbon.identity.application.authentication.framework.handler.request.impl.consent.constant.SSOConsentConstants.RESIDENT_IDP;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.ERROR_CODE_INVALID_USER_INPUT;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.ERROR_CODE_POLICY_CONSENT_FAILURE;

/**
 * Utility class for consent processing in executor flows.
 */
public class ExecutorConsentUtils {

    private static final Log LOG = LogFactory.getLog(ExecutorConsentUtils.class);

    private ExecutorConsentUtils() {
    }

    /**
     * Processes user consent for all purposes declared on the flow user.
     * No-op if Consent V2 API is not enabled.
     *
     * @param componentId         Executor component identifier used in diagnostic logs.
     * @param context             Current flow execution context.
     * @param user                Flow user whose consents are to be processed.
     * @param userStoreDomainName User store domain of the user.
     * @throws FlowEngineException if consent processing fails.
     */
    public static void processUserConsent(String componentId, FlowExecutionContext context, FlowUser user,
                                          String userStoreDomainName) throws FlowEngineException {

        if (!FrameworkUtils.isConsentV2APIEnabled() || user.getUserConsents().isEmpty()) {
            return;
        }

        String usernameWithUserStoreDomain = UserCoreUtil.addDomainToName(user.getUsername(), userStoreDomainName);
        if (LOG.isDebugEnabled()) {
            LOG.debug("Processing consent for user: " + user.getUsername() + " in tenant: " +
                    context.getTenantDomain());
        }
        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(context.getTenantDomain(), true);
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setUsername(usernameWithUserStoreDomain);
        try {
            for (FlowUser.UserConsent userConsent : user.getUserConsents()) {
                for (FlowUser.ConsentPurpose purpose : userConsent.getPurposes()) {
                    createPurposeConsent(usernameWithUserStoreDomain, context.getTenantDomain(),
                            userConsent.getPurposeType(), purpose);
                }
            }
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("Consent processing completed for user: " + user.getUsername() + " in tenant: " +
                    context.getTenantDomain());
        }
        if (LoggerUtils.isDiagnosticLogsEnabled()) {
            LoggerUtils.triggerDiagnosticLogEvent(
                    consentDiagnosticLogBuilder(componentId, user.getUsername(), context.getTenantDomain())
                    .resultMessage("Consent processing completed for user.")
                    .resultStatus(DiagnosticLog.ResultStatus.SUCCESS));
        }
    }

    private static void createPurposeConsent(String username, String tenantDomain, String purposeType,
                                             FlowUser.ConsentPurpose purpose) throws FlowEngineException {

        if (StringUtils.isBlank(purpose.getId())) {
            throw FlowExecutionEngineUtils.handleClientException(ERROR_CODE_INVALID_USER_INPUT,
                    purposeType + " consent");
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("Processing consent type: " + purposeType + ", User: " +
                    Utils.maskIfRequired(username) + ", Purpose: " + purpose.getId() +
                    ", Tenant: " + tenantDomain);
        }

        try {
            // Attributes list contains only accepted attribute IDs when purpose is accepted,
            // and only rejected attribute IDs when purpose is not accepted.
            List<String> attributes = purpose.getAttributes();
            List<PIICategory> piiCategories = new ArrayList<>();

            PIICategory defaultPiiCategory = ConsentReceiptUtils.getDefaultPiiCategory(purposeType,
                    IdentityRecoveryServiceDataHolder.getInstance().getConsentManager());
            if (defaultPiiCategory != null) {
                piiCategories.add(defaultPiiCategory);
            }

            if (attributes != null && !attributes.isEmpty()) {
                piiCategories.addAll(getPiiCategories(attributes));
            }

            addConsentReceipt(username, tenantDomain, purposeType,
                    Collections.singletonList(new PurposePIICategoryBinding(purpose.getId(), piiCategories)),
                    !purpose.isAccepted());
        } catch (ConsentManagementException e) {
            throw FlowExecutionEngineUtils.handleServerException(ERROR_CODE_POLICY_CONSENT_FAILURE, e, purposeType,
                    Utils.maskIfRequired(username));
        }

        if (LoggerUtils.isDiagnosticLogsEnabled()) {
            LoggerUtils.triggerDiagnosticLogEvent(
                    consentDiagnosticLogBuilder(null, username, tenantDomain)
                    .inputParam("consent_type", purposeType)
                    .inputParam("purpose_id", purpose.getId())
                    .resultMessage("Policy consent successfully processed.")
                    .resultStatus(DiagnosticLog.ResultStatus.SUCCESS));
        }
    }

    private static List<PIICategory> getPiiCategories(List<String> attributes) throws FlowEngineException {

        org.wso2.carbon.consent.mgt.core.ConsentManager consentManager =
                IdentityRecoveryServiceDataHolder.getInstance().getConsentManager();
        List<PIICategory> piiCategories = new ArrayList<>();
        try {
            for (String attributeUuid : attributes) {
                PIICategory piiCategory = consentManager.getPIICategoryByUuid(attributeUuid);
                if (piiCategory != null) {
                    piiCategories.add(piiCategory);
                }
            }
        } catch (ConsentManagementException e) {
            throw FlowExecutionEngineUtils.handleServerException(ERROR_CODE_POLICY_CONSENT_FAILURE, e,
                    String.join(",", attributes), null);
        }
        return piiCategories;
    }

    private static void addConsentReceipt(String subjectId, String tenantDomain, String purposeType,
                                          List<PurposePIICategoryBinding> purposeBindings, boolean isRejected)
            throws FlowEngineServerException {

        Flow.InitiatingPersona initiatingPersona;
        Flow existingFlow = IdentityContext.getThreadLocalIdentityContext().getCurrentFlow();
        if (existingFlow != null) {
            initiatingPersona = existingFlow.getInitiatingPersona();
        } else {
            initiatingPersona = Flow.InitiatingPersona.ADMIN;
        }
        Flow consentFlow = new Flow.Builder()
                .name(Flow.Name.CONSENT_ADD)
                .initiatingPersona(initiatingPersona)
                .build();
        IdentityContext.getThreadLocalIdentityContext().enterFlow(consentFlow);
        try {
            org.wso2.carbon.consent.mgt.core.ConsentManager consentManager =
                    IdentityRecoveryServiceDataHolder.getInstance().getConsentManager();
            ReceiptInput receiptInput = ConsentReceiptUtils.buildReceiptInput("en", subjectId, tenantDomain,
                    null, isRejected, null, null, RESIDENT_IDP, purposeBindings, consentManager);
            consentManager.addConsent(receiptInput);

            if (LoggerUtils.isDiagnosticLogsEnabled()) {
                LoggerUtils.triggerDiagnosticLogEvent(
                        consentDiagnosticLogBuilder(null, subjectId, tenantDomain)
                        .inputParam("consent_type", purposeType)
                        .inputParam("purpose_count", purposeBindings.size())
                        .resultMessage("Consent receipt successfully added.")
                        .resultStatus(DiagnosticLog.ResultStatus.SUCCESS));
            }
        } catch (ConsentManagementException e) {
            if (LoggerUtils.isDiagnosticLogsEnabled()) {
                LoggerUtils.triggerDiagnosticLogEvent(
                        consentDiagnosticLogBuilder(null, subjectId, tenantDomain)
                        .inputParam("consent_type", purposeType)
                        .inputParam(LogConstants.InputKeys.ERROR_MESSAGE, e.getMessage())
                        .resultMessage("Failed to add consent receipt.")
                        .resultStatus(DiagnosticLog.ResultStatus.FAILED));
            }
            throw FlowExecutionEngineUtils.handleServerException(ERROR_CODE_POLICY_CONSENT_FAILURE, e, purposeType,
                    Utils.maskIfRequired(subjectId));
        } finally {
            IdentityContext.getThreadLocalIdentityContext().exitFlow();
        }
    }

    /**
     * Builds a DiagnosticLog builder for consent-related events.
     *
     * @param componentId  Executor component identifier, or null to omit it from the builder.
     * @param subjectId    Username or subject identifier.
     * @param tenantDomain Tenant domain.
     * @return Configured DiagnosticLog builder.
     */
    public static DiagnosticLog.DiagnosticLogBuilder consentDiagnosticLogBuilder(String componentId,
                                                                                  String subjectId,
                                                                                  String tenantDomain) {

        String component = StringUtils.isNotBlank(componentId) ? componentId : "ExecutorConsentUtils";
        return new DiagnosticLog.DiagnosticLogBuilder(
                component, FrameworkConstants.LogConstants.ActionIDs.PROCESS_POLICY_CONSENT)
                .inputParam(LogConstants.InputKeys.USER, LoggerUtils.isLogMaskingEnable ?
                        LoggerUtils.getMaskedContent(subjectId) : subjectId)
                .inputParam(LogConstants.InputKeys.TENANT_DOMAIN, tenantDomain)
                .logDetailLevel(DiagnosticLog.LogDetailLevel.APPLICATION);
    }
}
