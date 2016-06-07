package org.wso2.carbon.identity.recovery.endpoint.bean;

import org.wso2.carbon.identity.application.common.model.User;

import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = {
        "user",
        "code",
        "password"
})
@XmlRootElement(name = "userPassword")
public class UserPassword {
    @XmlElement(required = true)
    private User user;

    @XmlElement(required = true)
    private String password;

    @XmlElement(required = true)
    private String code;

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }
}