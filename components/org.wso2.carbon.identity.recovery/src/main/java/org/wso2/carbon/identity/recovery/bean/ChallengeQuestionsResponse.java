package org.wso2.carbon.identity.recovery.bean;

import org.wso2.carbon.identity.recovery.model.ChallengeQuestion;

public class ChallengeQuestionsResponse {

    private ChallengeQuestion[] question;
    private String code;
    private String status;

    public ChallengeQuestionsResponse() {
        //Default constructor
    }

    public ChallengeQuestionsResponse(ChallengeQuestion[] question) {
        this.question = question;
    }

    public ChallengeQuestion[] getQuestion() {
        return question;
    }

    public void setQuestion(ChallengeQuestion[] question) {
        this.question = question;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }
}