package org.wso2.carbon.identity.recovery;

/**
 * Recovery Steps constants.
 */
public enum RecoverySteps {
    NOTIFY,
    UPDATE_PASSWORD,
    VALIDATE_CHALLENGE_QUESTION,
    VALIDATE_ALL_CHALLENGE_QUESTION,
    CONFIRM_SIGN_UP
}
