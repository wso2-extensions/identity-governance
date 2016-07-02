package org.wso2.carbon.identity.recovery.endpoint.bean;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.model.UserClaim;
import org.wso2.carbon.user.api.Claim;

import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = {
        "user",
        "password",
        "claims",
        "properties"
})
@XmlRootElement(name = "selfRegistrationRequest")
public class SelfRegistrationRequest {
    @XmlElement(required = true)
    private User user;

    @XmlElement(required = true)
    private Claim[] claims;

    @XmlElement(required = false)
    private Property[] properties;

    @XmlElement(required = true)
    private String password;

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public Claim[] getClaims() {
        return claims;
    }

    public void setClaims(Claim[] claims) {
        this.claims = claims;
    }

    public Property[] getProperties() {
        return properties;
    }

    public void setProperties(Property[] properties) {
        this.properties = properties;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}