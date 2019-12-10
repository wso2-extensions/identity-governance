<%--
  ~ Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
  ~
  ~ Licensed under the Apache License, Version 2.0 (the "License");
  ~ you may not use this file except in compliance with the License.
  ~ You may obtain a copy of the License at
  ~
  ~ http://www.apache.org/licenses/LICENSE-2.0
  ~
  ~ Unless required by applicable law or agreed to in writing, software
  ~ distributed under the License is distributed on an "AS IS" BASIS,
  ~ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  ~ See the License for the specific language governing permissions and
  ~ limitations under the License.
  --%>

<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib uri="http://wso2.org/projects/carbon/taglibs/carbontags.jar" prefix="carbon" %>
<jsp:include page="../dialog/display_messages.jsp"/>

<%@page import="org.apache.axis2.context.ConfigurationContext" %>
<%@page import="org.apache.commons.collections.CollectionUtils" %>
<%@ page import="org.apache.commons.lang.StringUtils" %>
<%@ page import="org.wso2.carbon.CarbonConstants" %>
<%@ page import="org.wso2.carbon.context.CarbonContext" %>
<%@ page import="org.wso2.carbon.identity.recovery.stub.model.ChallengeQuestion" %>
<%@ page import="org.wso2.carbon.identity.recovery.ui.IdentityManagementAdminClient" %>
<%@ page import="org.wso2.carbon.identity.recovery.ui.Utils" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIMessage" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIUtil" %>
<%@ page import="org.wso2.carbon.utils.ServerConstants" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="java.util.Arrays" %>
<%@ page import="java.util.Collections" %>
<%@ page import="java.util.List" %>

<%
    String httpMethod = request.getMethod();
    if (!"post".equalsIgnoreCase(httpMethod)) {
        response.sendError(HttpServletResponse.SC_METHOD_NOT_ALLOWED);
        return;
    }

    String removeSetId = request.getParameter("removeSetId");

    // params coming from a request to create a new question set
    String setId = request.getParameter("setName");
    String questionId = request.getParameter("questionId0");
    String question = request.getParameter("question0");
    String questionLocale = request.getParameter("questionLocale0");


    // flags to identity whether questions are to be deleted or updated at the backend.
    boolean deleteQuestions = false;
    boolean updateQuestions = false;

    List<ChallengeQuestion> questionToBeDeleted = new ArrayList<ChallengeQuestion>();
    List<ChallengeQuestion> questionToBeUpdated = new ArrayList<ChallengeQuestion>();
    List<ChallengeQuestion> retrievedQuestions = null;

    Object toBeDeleted = session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE);
    if (toBeDeleted != null) {
        questionToBeDeleted = (List<ChallengeQuestion>) toBeDeleted;
    }

    Object toBeUpdated = session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_UPDATE);
    if (toBeUpdated != null) {
        questionToBeUpdated = (List<ChallengeQuestion>) toBeUpdated;
    }

    try {
        String cookie = (String) session
                .getAttribute(ServerConstants.ADMIN_SERVICE_COOKIE);
        String backendServerURL = CarbonUIUtil.getServerURL(config.getServletContext(),
                session);
        ConfigurationContext configContext = (ConfigurationContext) config
                .getServletContext()
                .getAttribute(CarbonConstants.CONFIGURATION_CONTEXT);
        IdentityManagementAdminClient proxy =
                new IdentityManagementAdminClient(cookie, backendServerURL, configContext);

        String tenantDomain = CarbonContext.getThreadLocalCarbonContext().getTenantDomain();


        ChallengeQuestion[] retrievedChallengeQuestions = proxy.getChallengeQuestionsForTenant(tenantDomain);
        if (retrievedChallengeQuestions != null) {
            retrievedQuestions = Arrays.asList(retrievedChallengeQuestions);
        } else {
            retrievedQuestions = Collections.emptyList();
        }

        if (removeSetId != null && removeSetId.trim().length() > 0) {
            for (ChallengeQuestion challengeQuestion : retrievedQuestions) {
                if (StringUtils.equals(challengeQuestion.getQuestionSetId(), removeSetId)) {
                    questionToBeDeleted.add(challengeQuestion);
                }
            }
        }

        // create a new questions set along with its first question.
        if (StringUtils.isNotBlank(setId) && StringUtils.isNotBlank(questionId) && StringUtils.isNotBlank(question) &&
                StringUtils.isNotBlank(questionLocale)) {
            ChallengeQuestion challengeQuestion = new ChallengeQuestion();

            if (!setId.startsWith(Utils.WSO2_CLAIM_DIALECT)) {
                setId = Utils.WSO2_CLAIM_DIALECT + setId;
            }
            challengeQuestion.setQuestionSetId(setId);
            challengeQuestion.setQuestionId(questionId);
            challengeQuestion.setQuestion(question);
            challengeQuestion.setLocale(questionLocale);

            questionToBeUpdated.add(challengeQuestion);
        }

        if (CollectionUtils.isNotEmpty(questionToBeUpdated)) {
            updateQuestions = true;
        }

        if (CollectionUtils.isNotEmpty(questionToBeDeleted)) {
            deleteQuestions = true;
        }

        if (deleteQuestions) {
            int size = questionToBeDeleted.size();
            // call admin client to delete the questions.
            proxy.deleteChallengeQuestions(
                    questionToBeDeleted.toArray(new ChallengeQuestion[size]), tenantDomain);
            session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE, new ArrayList<ChallengeQuestion>());
        }

        if (updateQuestions) {
            int size = questionToBeUpdated.size();
            proxy.setChallengeQuestions(questionToBeUpdated.toArray(new ChallengeQuestion[size]), tenantDomain);
            session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE, new ArrayList<ChallengeQuestion>());
        }
%>
<script type="text/javascript">
    location.href = "challenge-set-list.jsp";
</script>
<%
} catch (Exception e) {
    CarbonUIMessage.sendCarbonUIMessage(e.getMessage(), CarbonUIMessage.ERROR, request);
%>
<script type="text/javascript">
    location.href = "challenge-set-list.jsp";
</script>
<%
        return;
    }
%>
        

