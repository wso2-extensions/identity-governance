package org.wso2.carbon.identity.idle.account.identification.models;

/**
 * Object for inactive user model.
 */
public class InactiveUserModel {

    private String userId;
    private String username;
    private String userStoreDomain;

    /**
     * Method to get userId.
     *
     * @return userId.
     */
    public String getUserId() {

        return userId;
    }

    /**
     * Method to set userId.
     */
    public void setUserId(String userId) {

        this.userId = userId;
    }

    /**
     * Method to get username.
     *
     * @return username.
     */
    public String getUsername() {

        return username;
    }

    /**
     * Method to set username.
     */
    public void setUsername(String username) {

        this.username = username;
    }

    /**
     * Method to get userStore domain.
     *
     * @return userStore domain.
     */
    public String getUserStoreDomain() {

        return userStoreDomain;
    }

    /**
     * Method to set userStore domain.
     */
    public void setUserStoreDomain(String userStoreDomain) {

        this.userStoreDomain = userStoreDomain;
    }
}
