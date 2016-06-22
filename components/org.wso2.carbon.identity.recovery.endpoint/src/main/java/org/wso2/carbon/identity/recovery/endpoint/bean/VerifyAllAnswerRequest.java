package org.wso2.carbon.identity.recovery.endpoint.bean;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;

import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = {
        "user",
        "answers",
        "code"
})
@XmlRootElement(name = "verifyAllAnswerRequest")
public class VerifyAllAnswerRequest {
    @XmlElement(required = true)
    private User user;

    @XmlElement(required = true)
    private UserChallengeAnswer[] answers;

    @XmlElement(required = true)
    private String code;


    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public void setAnswers(UserChallengeAnswer[] answers) {
        this.answers = answers;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public UserChallengeAnswer[] getAnswers() {
        return answers;
    }
}