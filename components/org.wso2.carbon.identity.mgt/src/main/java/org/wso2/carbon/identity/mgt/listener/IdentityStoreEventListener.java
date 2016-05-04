package org.wso2.carbon.identity.mgt.listener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.mgt.model.UserIdentityClaim;
import org.wso2.carbon.identity.mgt.store.UserIdentityDataStore;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Iterator;
import java.util.Map;

public class IdentityStoreEventListener extends AbstractIdentityUserOperationEventListener {

    private static final Log log = LogFactory.getLog(IdentityMgtEventListener.class);
    private static final String DO_PRE_SET_USER_CLAIM_VALUES = "doPreSetUserClaimValues";
    private static final String USER_OPERATION_EVENT_LISTENER_TYPE = "org.wso2.carbon.user.core.listener" +
            ".UserOperationEventListener";
    private static final String DATA_STORE_PROPERTY_NAME = "Data.Store";
    private UserIdentityDataStore store;

    public IdentityStoreEventListener() throws IllegalAccessException, InstantiationException, ClassNotFoundException {
        String storeClassName = IdentityUtil.readEventListenerProperty(USER_OPERATION_EVENT_LISTENER_TYPE, this
                .getClass().getName()).getProperties().get(DATA_STORE_PROPERTY_NAME).toString();
        Class clazz = Thread.currentThread().getContextClassLoader().loadClass(storeClassName);
        store = (UserIdentityDataStore) clazz.newInstance();
    }


    @Override
    public int getExecutionOrderId() {
        int orderId = getOrderId();
        if (orderId != IdentityCoreConstants.EVENT_LISTENER_ORDER_ID) {
            return orderId;
        }
        return 100;
    }

    /**
     * As in the above method the user account lock claim, primary challenges
     * claim will be separately handled. Identity claims will be removed from
     * the claim set before adding claims to the user store.
     */
    @Override
    public boolean doPreSetUserClaimValues(String userName, Map<String, String> claims,
                                           String profileName, UserStoreManager userStoreManager)
            throws UserStoreException {

        if (!isEnable()) {
            return true;
        }
        IdentityUtil.clearIdentityErrorMsg();
        boolean accountLocked = Boolean.parseBoolean(claims.get(UserIdentityDataStore.ACCOUNT_LOCK));
        if (accountLocked) {
            IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(UserCoreConstants
                    .ErrorCode.USER_IS_LOCKED);
            IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
        }

        // Top level try and finally blocks are used to unset thread local variables
        try {
            if (!IdentityUtil.threadLocalProperties.get().containsKey(DO_PRE_SET_USER_CLAIM_VALUES)) {
                IdentityUtil.threadLocalProperties.get().put(DO_PRE_SET_USER_CLAIM_VALUES, true);

                UserIdentityDataStore identityDataStore = this.store;
                UserIdentityClaim identityDTO = identityDataStore.load(userName, userStoreManager);
                if (identityDTO == null) {
                    identityDTO = new UserIdentityClaim(userName);
                }

                Iterator<Map.Entry<String, String>> it = claims.entrySet().iterator();
                while (it.hasNext()) {

                    Map.Entry<String, String> claim = it.next();

                    if (claim.getKey().contains(UserCoreConstants.ClaimTypeURIs.CHALLENGE_QUESTION_URI)
                            || claim.getKey().contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)) {
                        String key = claim.getKey();
                        String value = claim.getValue();

                        identityDTO.setUserIdentityDataClaim(key, value);
                        it.remove();
                    }
                }

                // storing the identity claims and security questions
                try {
                    identityDataStore.store(identityDTO, userStoreManager);
                } catch (IdentityException e) {
                    throw new UserStoreException(
                            "Error while saving user store data for user : " + userName, e);
                } finally {
                    // Remove thread local variable
                    IdentityUtil.threadLocalProperties.get().remove(DO_PRE_SET_USER_CLAIM_VALUES);
                }
            }
            return true;
        } finally {

        }
    }

}
