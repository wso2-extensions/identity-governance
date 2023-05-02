package org.wso2.carbon.identity.idle.account.identification.models;

/**
 * Object for inactive user model.
 */
public class InactiveUserModel {

    private String username;
    private String userStoreDomain;
    private String email;

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

    /**
     * Method to get email.
     *
     * @return email.
     */
    public String getEmail() {

        return email;
    }

    /**
     * Method to set email.
     */
    public void setEmail(String email) {

        this.email = email;
    }
}
