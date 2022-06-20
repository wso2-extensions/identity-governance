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
<%@ page import="org.apache.axis2.context.ConfigurationContext" %>
<%@page import="org.apache.commons.lang.ArrayUtils" %>

<%@page import="org.apache.commons.lang.StringUtils" %>
<jsp:include page="../dialog/display_messages.jsp"/>


<%@ page import="org.owasp.encoder.Encode" %>
<%@ page import="org.wso2.carbon.CarbonConstants" %>
<%@ page import="org.wso2.carbon.context.CarbonContext" %>
<%@ page import="org.wso2.carbon.identity.core.util.IdentityUtil" %>
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

    List<ChallengeQuestion> updatedChallenges;
    List<ChallengeQuestion> deletedChallenges;
    List<ChallengeQuestion> challenges;
    String deleteRowId = request.getParameter("deleteRowId");
    String editRowId = request.getParameter("editRowId");
    String updateRowId = request.getParameter("updateRowId");
    String updatedQuestion = request.getParameter("updatedQuestion");

    String clear = request.getParameter("clear");

    String addRowId = request.getParameter("addRowId");
    String setName = request.getParameter("setName");
    String questionId = request.getParameter("questionId");
    String questionLocale = request.getParameter("questionLocale");

    challenges = (List<ChallengeQuestion>) session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION);
    deletedChallenges = (List<ChallengeQuestion>) session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE);
    updatedChallenges = (List<ChallengeQuestion>) session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_UPDATE);

    if (challenges == null) {
        try {
            String cookie = (String) session
                    .getAttribute(ServerConstants.ADMIN_SERVICE_COOKIE);
            String backendServerURL = CarbonUIUtil.getServerURL(config.getServletContext(),
                    session);
            ConfigurationContext configContext = (ConfigurationContext) config
                    .getServletContext()
                    .getAttribute(CarbonConstants.CONFIGURATION_CONTEXT);
            IdentityManagementAdminClient client =
                    new IdentityManagementAdminClient(cookie, backendServerURL, configContext);

            String tenantDomain = CarbonContext.getThreadLocalCarbonContext().getTenantDomain();
            ChallengeQuestion[] challengeQuestionsForUser = client.getChallengeQuestionsForTenant(tenantDomain);

            // retrieve challenge questions for user.
            if (ArrayUtils.isNotEmpty(challengeQuestionsForUser)) {
                challenges = new ArrayList<ChallengeQuestion>(Arrays.asList(challengeQuestionsForUser));
                session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION, challenges);
            }
        } catch (Exception e) {
            CarbonUIMessage.sendCarbonUIMessage(e.getMessage(), CarbonUIMessage.ERROR,
                    request);
%>
<script type="text/javascript">
    location.href = "../admin/error.jsp";
</script>
<%
            return;
        }
    }

    if (deletedChallenges == null || StringUtils.isNotBlank(clear)) {
        deletedChallenges = new ArrayList<ChallengeQuestion>();
    }

    if (updatedChallenges == null || StringUtils.isNotBlank(clear)) {
        updatedChallenges = new ArrayList<ChallengeQuestion>();
    }

    if (challenges != null) {
        // delete questions
        if (deleteRowId != null) {
            int rowNo = Integer.parseInt(deleteRowId);
            ChallengeQuestion removed = challenges.remove(rowNo);
            deletedChallenges.add(removed);
        }

        // edited questions
        if (updateRowId != null) {
            int rowNo = Integer.parseInt(updateRowId);
            ChallengeQuestion selected = challenges.get(rowNo);
            if (StringUtils.isNotBlank(updatedQuestion)) {
                selected.setQuestion(updatedQuestion);
            }
            updatedChallenges.add(selected);
        }

    } else {
        challenges = new ArrayList<ChallengeQuestion>();
    }

    // adding new questions
    if (addRowId != null) {
        ChallengeQuestion dto = new ChallengeQuestion();
        dto.setQuestion(addRowId);
        dto.setQuestionSetId(setName);
        dto.setQuestionId(questionId);
        dto.setLocale(questionLocale);
        challenges.add(dto);
        updatedChallenges.add(dto);
    }

    session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE, deletedChallenges);
    session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_UPDATE, updatedChallenges);

    Collections.sort(challenges, Utils.questionComparator);
    session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION, challenges);
%>

<fmt:bundle basename="org.wso2.carbon.identity.recovery.ui.i18n.Resources">
    <carbon:breadcrumb label="challenge.add"
                       resourceBundle="org.wso2.carbon.identity.recovery.ui.i18n.Resources"
                       topPage="true" request="<%=request%>"/>

    <script type="text/javascript">


        function removeRow(row) {
            function doDelete() {
                var setName = document.getElementsByName("setName")[0].value;
                location.href = 'challenges-mgt.jsp?deleteRowId=' + encodeURIComponent(row) + '&setName=' +
                        encodeURIComponent(setName);
            }

            CARBON.showConfirmationDialog("<fmt:message key="confirm.delete.challenge.question"/> ?", doDelete, null);
        }

        function editRow(row) {
            var setName = document.getElementsByName("setName")[0].value;
            location.href = 'challenges-mgt.jsp?editRowId=' + encodeURIComponent(row) + '&setName=' +
                    encodeURIComponent(setName);
        }

        function updateRow(row) {
            var setName = document.getElementsByName("setName")[0].value;
            var questionId = 'questionTD' + row;
            var question = document.getElementById(questionId).innerText;

            if (question == null || question == "") {
                CARBON.showWarningDialog('Please enter a valid security question', null, null);
                location.href = '#';
            } else {
                question = question.trim();
                location.href = 'challenges-mgt.jsp?updateRowId=' + encodeURIComponent(row) + '&setName=' +
                        encodeURIComponent(setName) + '&updatedQuestion=' + encodeURIComponent(question);
            }
        }

        function cancelForm() {
            location.href = 'challenge-set-list.jsp';
        }

    </script>

    <div id="middle">
        <h2><fmt:message key="challenge.add"/></h2>

        <form id="questionForm" name="questionForm" method="post" action="challenges-mgt-finish-ajaxprocessor.jsp">
            <div id="workArea">
                <table class="styledLeft" id="internal" name="internal" width="100%">
                    <tr class="tableEvenRow">
                        <td style="width: 30px;">
                            <div style="height:30px;">
                                <a href="challenge-question-add.jsp?setName=<%=Encode.forUriComponent(setName)%>"
                                   style='background-image:url(images/add.gif);'
                                   type="button" class="icon-link">
                                    <fmt:message key='challenge.question.add'/>
                                </a>
                            </div>
                        </td>
                    </tr>
                </table>

                <!-- Table displaying current challenge questions in the set. -->
                <p>&nbsp;</p>
                <table class="styledLeft">
                    <tr>
                        <td class="nopadding">
                            <table cellspacing="0" id="mainTable" style="width:100%;border:none !important">
                                <thead>
                                <th class="leftCol-small"><fmt:message key="challenge.question.questionId"/></th>
                                <th class="leftCol-small"><fmt:message key="challenge.question.question"/></th>
                                <th class="leftCol-small"><fmt:message key="challenge.question.questionLocale"/></th>
                                <th><fmt:message key="challenge.question.actions"/></th>
                                </thead>
                                <tbody>

                                <%
                                    if (challenges.size() > 0) {
                                        for (int i = 0; i < challenges.size(); i++) {
                                            ChallengeQuestion question = challenges.get(i);
                                            if (StringUtils.isNotBlank(setName) && StringUtils.equalsIgnoreCase(question.getQuestionSetId(), setName)) {

                                %>
                                <tr>
                                    <td width="10%"><%=Encode.forHtmlContent(question.getQuestionId())%>
                                    </td>
                                    <% if (IdentityUtil.isNotBlank(editRowId) && (Integer.parseInt(editRowId) == i)) {%>
                                    <td id="questionTD<%=i%>" width="60%"
                                        contenteditable="true" bgcolor="#ffffb3">
                                        <i><%=Encode.forHtmlContent(question.getQuestion())%>
                                        </i>
                                    </td>
                                    <%
                                    } else {
                                    %>
                                    <td id="questionTD<%=i%>"
                                        width="60%"><%=Encode.forHtmlContent(question.getQuestion())%>
                                    </td>
                                    <%
                                        }%>

                                    <td width="10%"><%=Encode.forHtmlContent(question.getLocale())%>
                                    </td>
                                    <td width="20%">
                                        <%
                                            if (IdentityUtil.isNotBlank(editRowId) && (Integer.parseInt(editRowId) == i)) {
                                        %>
                                        <a onclick="updateRow('<%=i%>')"
                                           style='background-image:url(images/edit.gif);'
                                           type="button" class="icon-link">
                                            Update
                                        </a>
                                        <%
                                        } else { %>
                                        <a onclick="editRow('<%=i%>')"
                                           style='background-image:url(images/edit.gif);'
                                           type="button" class="icon-link">
                                            Edit
                                        </a>
                                        <%}%>
                                        <a onclick="removeRow('<%=i%>')"
                                           style='background-image:url(images/delete.gif);' type="button"
                                           class="icon-link">
                                            Delete
                                        </a>
                                    </td>
                                </tr>

                                <%
                                            }
                                        }
                                    }
                                %>

                                </tbody>
                            </table>
                        </td>

                    </tr>
                    <tr>
                        <td class="buttonRow">
                            <input type="submit" value="Finish" class="button"/>
                            <input type="button" value="Cancel" onclick="cancelForm();" class="button"/>
                        </td>
                    </tr>
                    <input type="hidden" name="setName" id="setName"
                           size="60"
                           value="<%=Encode.forHtmlAttribute(setName)%>"/>

                </table>


            </div>
        </form>
    </div>
</fmt:bundle>

