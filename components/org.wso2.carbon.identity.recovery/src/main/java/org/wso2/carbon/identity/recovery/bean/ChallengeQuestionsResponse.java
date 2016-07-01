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
        if (question != null) {
            this.question = question.clone();
        }
    }

    public ChallengeQuestion[] getQuestion() {
        if (question == null) {
            return new ChallengeQuestion[0];
        }
        return question;
    }

    public void setQuestion(ChallengeQuestion[] question) {
        if (question != null) {
            this.question = question.clone();
        }
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