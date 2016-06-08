package org.wso2.carbon.identity.recovery.endpoint.bean;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;

import javax.xml.bind.annotation.*;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = {
        "question",
        "code"
})
@XmlRootElement(name = "verifyAnswer")
public class ChallengeQuestionResponse {

    @XmlElement(required = true)
    private ChallengeQuestion question;

    @XmlElement(required = true)
    private String code;

    public ChallengeQuestion getQuestion() {
        return question;
    }

    public void setQuestion(ChallengeQuestion question) {
        this.question = question;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }
}