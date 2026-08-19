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

package org.wso2.carbon.identity.breach.detection.listener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.breach.detection.config.BreachDetectionConfig;
import org.wso2.carbon.identity.breach.detection.constants.BreachDetectionConstants;
import org.wso2.carbon.identity.breach.detection.engine.BreachEvaluationEngine;
import org.wso2.carbon.identity.breach.detection.engine.Decision;
import org.wso2.carbon.identity.breach.detection.engine.EvaluationResult;
import org.wso2.carbon.identity.breach.detection.internal.BreachDetectionDataHolder;
import org.wso2.carbon.identity.breach.detection.policy.BreachPolicy;
import org.wso2.carbon.identity.breach.detection.policy.BreachPolicyResolver;
import org.wso2.carbon.identity.breach.detection.util.BreachDetectionUtils;
import org.wso2.carbon.identity.breach.source.BreachContext;
import org.wso2.carbon.identity.breach.source.Credential;
import org.wso2.carbon.identity.breach.source.Operation;
import org.wso2.carbon.identity.breach.source.Subject;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.context.IdentityContext;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.context.model.Organization;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.utils.Secret;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.Arrays;
import java.util.Map;

/**
 * The single interception point.
 * <p>
 * Every path that sets a password converges on {@code AbstractUserStoreManager}, which drives an ordered
 * listener chain before writing to the store. Sitting in that chain gives uniform coverage for free: portals,
 * management APIs, recovery and invitation acceptance all pass through it, and so does any path added later,
 * including one against a secondary user store.
 * <p>
 * Order 420 places this after composition rules at 3 - so a password failing length or character class never
 * reaches a breach source - and before the service extension at 10000, so in-product policy resolves before any
 * customer extension runs.
 */
public class BreachDetectionListener extends AbstractIdentityUserOperationEventListener {

    private static final Log LOG = LogFactory.getLog(BreachDetectionListener.class);

    @Override
    public int getExecutionOrderId() {

        int order = getOrderId();
        return order == IdentityCoreConstantsOrderPlaceholder.UNDEFINED
                ? BreachDetectionConstants.DEFAULT_LISTENER_ORDER : order;
    }

    @Override
    public boolean doPreAddUser(String userName, Object credential, String[] roleList,
                                Map<String, String> claims, String profile, UserStoreManager userStoreManager)
            throws UserStoreException {

        // Self-registration, administrative user creation, and invitation completion all arrive here.
        return check(userName, null, credential, userStoreManager, Operation.REGISTER);
    }

    @Override
    public boolean doPreUpdateCredential(String userName, Object newCredential, Object oldCredential,
                                         UserStoreManager userStoreManager) throws UserStoreException {

        return check(userName, null, newCredential, userStoreManager, Operation.SELF_UPDATE);
    }

    @Override
    public boolean doPreUpdateCredentialWithID(String userID, Object newCredential, Object oldCredential,
                                               UserStoreManager userStoreManager) throws UserStoreException {

        return check(null, userID, newCredential, userStoreManager, Operation.SELF_UPDATE);
    }

    @Override
    public boolean doPreUpdateCredentialByAdmin(String userName, Object newCredential,
                                                UserStoreManager userStoreManager) throws UserStoreException {

        // Administrative reset, and the reset that completes a recovery flow.
        return check(userName, null, newCredential, userStoreManager, Operation.ADMIN_RESET);
    }

    @Override
    public boolean doPreUpdateCredentialByAdminWithID(String userID, Object newCredential,
                                                      UserStoreManager userStoreManager)
            throws UserStoreException {

        return check(null, userID, newCredential, userStoreManager, Operation.ADMIN_RESET);
    }

    private boolean check(String userName, String userId, Object credential, UserStoreManager userStoreManager,
                          Operation fallbackOperation) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }
        BreachEvaluationEngine engine = BreachDetectionDataHolder.getInstance().getEvaluationEngine();
        BreachPolicyResolver resolver = BreachDetectionDataHolder.getInstance().getPolicyResolver();
        if (engine == null || resolver == null) {
            LOG.debug("Breach detection is not fully started yet. The credential write proceeds unchanged.");
            return true;
        }

        char[] chars = extract(credential);
        if (chars == null || chars.length == 0) {
            // Nothing to check. Composition rules own empty and malformed input.
            return true;
        }

        Operation operation = resolveOperation(fallbackOperation);
        if (operation == null) {
            // Exempted by configuration - a bulk import or migration-time write.
            return true;
        }

        String tenantDomain = resolveTenantDomain(userStoreManager);
        BreachPolicy policy = resolver.resolve(tenantDomain);
        if (!policy.isEnabled()) {
            return true;
        }

        // A copy, so clearing it after evaluation cannot corrupt the write that follows.
        Credential candidate = new Credential(Arrays.copyOf(chars, chars.length));
        BreachContext context = BreachContext.builder()
                .credential(candidate)
                .subject(Subject.builder(userName)
                        .userId(userId)
                        .userStoreDomain(resolveUserStoreDomain(userStoreManager))
                        .build())
                .tenantDomain(tenantDomain)
                .organizationId(resolveOrganizationId())
                .operation(operation)
                .build();

        EvaluationResult result;
        try {
            result = engine.evaluate(context, policy);
        } catch (Throwable t) {
            // A defect in our own engine is a server fault, and must not masquerade as a policy decision.
            LOG.error("Breached password detection failed unexpectedly. The credential write is refused.", t);
            throw new UserStoreException("An internal error occurred while checking the password.");
        } finally {
            if (!candidate.isCleared()) {
                try {
                    candidate.clear();
                } catch (RuntimeException ignored) {
                    // Already cleared by the engine.
                }
            }
        }

        if (result.getDecision() == Decision.REFUSE_BREACHED) {
            throw policyRejection(BreachDetectionConstants.ERROR_CODE_BREACHED_PASSWORD,
                    BreachDetectionUtils.getMessage(BreachDetectionConstants.MESSAGE_KEY_BREACHED,
                            "This password has appeared in a known data breach. Choose a different one - even "
                                    + "a small change to it is likely to be found too."));
        }
        if (result.getDecision() == Decision.REFUSE_UNVERIFIED) {
            throw policyRejection(BreachDetectionConstants.ERROR_CODE_CANNOT_VERIFY,
                    BreachDetectionUtils.getMessage(BreachDetectionConstants.MESSAGE_KEY_CANNOT_VERIFY,
                            "This password could not be checked against the breached password sources right "
                                    + "now. Try again shortly."));
        }
        return true;
    }

    /**
     * A policy decision is a client error carrying its reason - never a server fault, which is
     * indistinguishable from an outage and stops portals rendering the cause. The policy violation in the cause
     * chain is what the recovery and self-registration paths recognise.
     */
    private UserStoreClientException policyRejection(String errorCode, String message) {

        return new UserStoreClientException(message, errorCode, new PolicyViolationException(message));
    }

    private Operation resolveOperation(Operation fallback) {

        Flow flow = currentFlow();
        if (flow == null || flow.getName() == null) {
            return fallback;
        }
        if (flow.getName() == Flow.Name.BULK_RESOURCE_UPDATE
                && BreachDetectionConfig.getInstance().isBulkExempt()) {
            return null;
        }
        boolean administrative = flow.getInitiatingPersona() == Flow.InitiatingPersona.ADMIN;
        switch (flow.getName()) {
            case REGISTER:
            case JUST_IN_TIME_PROVISION:
                return Operation.REGISTER;
            case INVITE:
            case INVITED_USER_REGISTRATION:
                return Operation.INVITE;
            case PASSWORD_RESET:
            case CREDENTIAL_RESET:
                return administrative ? Operation.ADMIN_RESET : Operation.RECOVERY;
            case CREDENTIAL_UPDATE:
            case CREDENTIAL_ENROLL:
                return administrative ? Operation.ADMIN_RESET : Operation.SELF_UPDATE;
            default:
                return fallback;
        }
    }

    private Flow currentFlow() {

        try {
            IdentityContext context = IdentityContext.getThreadLocalIdentityContext();
            return context == null ? null : context.getFlow();
        } catch (Throwable t) {
            LOG.debug("No identity context is available; falling back to the listener hook for the operation.");
            return null;
        }
    }

    private String resolveOrganizationId() {

        try {
            IdentityContext context = IdentityContext.getThreadLocalIdentityContext();
            Organization organization = context == null ? null : context.getOrganization();
            return organization == null ? null : organization.getId();
        } catch (Throwable t) {
            return null;
        }
    }

    private String resolveTenantDomain(UserStoreManager userStoreManager) {

        try {
            int tenantId = userStoreManager.getTenantId();
            String domain = IdentityTenantUtil.getTenantDomain(tenantId);
            if (domain != null) {
                return domain;
            }
        } catch (Throwable t) {
            LOG.debug("Could not resolve the tenant from the user store manager.", t);
        }
        return MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
    }

    private String resolveUserStoreDomain(UserStoreManager userStoreManager) {

        try {
            return userStoreManager.getRealmConfiguration()
                    .getUserStoreProperty("DomainName");
        } catch (Throwable t) {
            return null;
        }
    }

    /**
     * The credential arrives as a {@link Secret} for listeners that handle secrets and as a character sequence
     * otherwise. Neither is turned into a {@code String} here.
     */
    private char[] extract(Object credential) {

        if (credential == null) {
            return null;
        }
        if (credential instanceof Secret) {
            return ((Secret) credential).getChars();
        }
        if (credential instanceof char[]) {
            return (char[]) credential;
        }
        if (credential instanceof CharSequence) {
            CharSequence sequence = (CharSequence) credential;
            char[] chars = new char[sequence.length()];
            for (int i = 0; i < sequence.length(); i++) {
                chars[i] = sequence.charAt(i);
            }
            return chars;
        }
        LOG.debug("The credential arrived in an unrecognised form and was not evaluated.");
        return null;
    }

    /**
     * The sentinel {@code getOrderId} returns when identity.xml carries no declaration for this listener.
     */
    private static final class IdentityCoreConstantsOrderPlaceholder {

        private static final int UNDEFINED = -1;

        private IdentityCoreConstantsOrderPlaceholder() {

        }
    }
}
