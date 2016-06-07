package org.wso2.carbon.identity.recovery.endpoint.bean;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.model.UserChallengeAnswer;
import org.wso2.carbon.identity.recovery.model.UserChallengeQuestion;

import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = {
        "user",
        "userChallengeAnswer"
})
@XmlRootElement(name = "verifyAnswer")
public class VerifyAnswer {
    @XmlElement(required = true)
    private User user;

    @XmlElement(required = true)
    private UserChallengeAnswer userChallengeAnswer;

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public UserChallengeAnswer getUserChallengeAnswer() {
        return userChallengeAnswer;
    }

    public void setUserChallengeAnswer(UserChallengeQuestion userChallengeQuestion) {
        this.userChallengeAnswer = userChallengeAnswer;
    }
}