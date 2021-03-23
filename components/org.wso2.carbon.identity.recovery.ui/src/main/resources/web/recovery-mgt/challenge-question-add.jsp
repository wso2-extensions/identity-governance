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
<script type="text/javascript" src="../identity/validation/js/identity-validate.js"></script>

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
<%@ page import="java.util.Locale" %>


<%

    List<ChallengeQuestion> updatedChallenges;
    List<ChallengeQuestion> deletedChallenges;
    List<ChallengeQuestion> tempChallenges;
    List<ChallengeQuestion> challenges;
    List<String> challengeURIs;

    // add/edit/delete within the set
    String deleteRowId = request.getParameter("deleteRowId");
    String editRowId = request.getParameter("editRowId");
    String addRowId = request.getParameter("addRowId");

    String updateRowId = request.getParameter("updateRowId");
    String updatedQuestion = request.getParameter("updatedQuestion");


    String setName = request.getParameter("setName");
    String question = request.getParameter("question");
    String questionId = request.getParameter("questionId");
    String questionLocale = request.getParameter("questionLocale");

    String localeMapping = request.getParameter("localeMapping");

    challenges = (List<ChallengeQuestion>) session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION);
    tempChallenges = (List<ChallengeQuestion>) session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_SET_TEMP);
    deletedChallenges = (List<ChallengeQuestion>) session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE);
    updatedChallenges = (List<ChallengeQuestion>) session.getAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_UPDATE);

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

        challengeURIs = Utils.getChallengeSetUris(challengeQuestionsForUser);
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


    if (deletedChallenges == null) {
        deletedChallenges = new ArrayList<ChallengeQuestion>();
    }

    if (updatedChallenges == null) {
        updatedChallenges = new ArrayList<ChallengeQuestion>();
    }

    if (challenges == null) {
        challenges = new ArrayList<ChallengeQuestion>();
    }

    if (tempChallenges != null) {
        // delete questions
        if (deleteRowId != null) {
            int rowNo = Integer.parseInt(deleteRowId);
            ChallengeQuestion removed = tempChallenges.remove(rowNo);
            updatedChallenges.remove(rowNo);
            deletedChallenges.add(removed);
        }

        // edited questions
        if (updateRowId != null) {
            int rowNo = Integer.parseInt(updateRowId);
            ChallengeQuestion selected = tempChallenges.get(rowNo);
            if (StringUtils.isNotBlank(updatedQuestion)) {
                selected.setQuestion(updatedQuestion);
                updatedChallenges.get(rowNo).setQuestion(updatedQuestion);
            }
        }

    } else {
        tempChallenges = new ArrayList<ChallengeQuestion>();
    }

    // adding new questions
    if (addRowId != null) {
        ChallengeQuestion dto = new ChallengeQuestion();
        dto.setQuestion(addRowId);
        dto.setQuestionSetId(setName);
        dto.setQuestionId(questionId);
        dto.setLocale(questionLocale);
        tempChallenges.add(dto);
        updatedChallenges.add(dto);
    }

    session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE, deletedChallenges);
    session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_UPDATE, updatedChallenges);

    Collections.sort(tempChallenges, Utils.questionComparator);
    Collections.sort(challenges, Utils.questionComparator);

    session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_SET_TEMP, tempChallenges);
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
                location.href = 'challenge-question-add.jsp?deleteRowId=' + encodeURIComponent(row) + '&setName=' +
                        encodeURIComponent(setName);
            }

            CARBON.showConfirmationDialog("<fmt:message key="confirm.delete.challenge.question"/> ?", doDelete, null);
        }

        function editRow(row) {
            var setName = document.getElementsByName("setName")[0].value;
            location.href = 'challenge-question-add.jsp?editRowId=' + encodeURIComponent(row) + '&setName=' +
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
                location.href = 'challenge-question-add.jsp?updateRowId=' + encodeURIComponent(row) + '&setName=' +
                        encodeURIComponent(setName) + '&updatedQuestion=' + encodeURIComponent(question);
            }
        }

        function addRow() {
            var setIndex = document.getElementsByName("setName0").selectedIndex;
            var setName = document.getElementsByName("setName0")[0].value;

            var question = document.getElementsByName("question0")[0].value;
            var questionId = document.getElementsByName("questionId0")[0].value;

            var questionLocale = document.getElementsByName("questionLocale0")[0].value;
            var localeIndex = document.getElementsByName("questionLocale0")[0].selectedIndex;

            var localeMapping = $("input[name=localeMapping]:checked").val();

            if (setIndex == 0) {
                CARBON.showWarningDialog('Please select a Question Set Id', null, null);
                location.href = '#';
            } else if (questionId == null || questionId == "") {
                CARBON.showWarningDialog('Please enter a valid question id', null, null);
                location.href = '#';
            } else if (localeIndex == 0) {
                CARBON.showWarningDialog('Please select a valid locale for the question', null, null);
                location.href = '#';
            } else if (question == null || question == "") {
                CARBON.showWarningDialog('Please enter a valid security question', null, null);
                location.href = '#';
            }
            // validate questionId
            else {
                if (!doValidateInput(document.getElementsByName("questionId0")[0], "Provided Question ID is invalid. Only {1} allowed.")) {
                    location.href = '#';
                } else {
                    question = question.trim();
                    location.href = 'challenge-question-add.jsp?addRowId=' + encodeURIComponent(question) +
                            '&setName=' + encodeURIComponent(setName) +
                            '&questionId=' + encodeURIComponent(questionId) +
                            '&questionLocale=' + encodeURIComponent(questionLocale) +
                            '&localeMapping=' + encodeURIComponent(localeMapping);
                }

            }
        }

        // function to handle the question set ID select event
        function onChangeSelect() {
            var index = document.getElementById("setName0").selectedIndex;
            var setName = document.getElementsByName("setName0")[0].value;

            var question = document.getElementsByName("question0")[0].value;
            var questionId = document.getElementsByName("questionId0")[0].value;

            var questionLocale = document.getElementsByName("questionLocale0")[0].value;
            var localeIndex = document.getElementsByName("questionLocale0")[0].selectedIndex;

            var localeMapping = $("input[name=localeMapping]:checked").val();

            if (index != 0) {
                disableInputEdit(false);

                var redirectTo = 'challenge-question-add.jsp?setName=' + encodeURIComponent(setName);

                if (question != null && question != "") {
                    redirectTo = redirectTo + '&question=' + encodeURIComponent(question);
                }

                if (questionId != null && questionId != "") {
                    redirectTo = redirectTo + '&questionId=' + encodeURIComponent(questionId);
                }

                if (localeIndex != 0) {
                    redirectTo = redirectTo + '&questionLocale=' + encodeURIComponent(questionLocale);
                }

                if (localeMapping != null && localeMapping != "") {
                    redirectTo = redirectTo + '&localeMapping=' + encodeURIComponent(localeMapping);
                }
                location.href = redirectTo;
            }
            else {
                // disable input fields when a questionSetId is not selected.
                disableInputEdit(true);
            }
        }

        function disableInputEdit(disabled) {
            var status = (disabled == true);

            document.getElementsByName("question0")[0].disabled = status;
            document.getElementsByName("questionId0")[0].disabled = status;
            document.getElementsByName("questionLocale0")[0].disabled = status;
            document.getElementsByName("localeMapping")[0].disabled = status;
            document.getElementsByName("localeMapping")[1].disabled = status;
            document.getElementById("addButton").disabled = status;
        }


        $(document).ready(function(){
            var index = document.getElementById("setName0").selectedIndex;
            var disabled = (index == 0);
            if(disabled) {
                setLocaleMappingCheckBox("no");
            }
            disableInputEdit(disabled)
        });


        function getQuestionIDs() {
            var questionSetIndex = document.getElementById("setName0").selectedIndex;
            var questionSetName = document.getElementsByName("setName0")[0].value;

            var localeMapping = $("input[name=localeMapping]:checked").val();

            if (questionSetIndex == 0) {
                CARBON.showWarningDialog('Please select a Question Set Id', null, null);
                location.href = '#';
            } else {
                var redirectTo = 'challenge-question-add.jsp?setName=' + encodeURIComponent(questionSetName)
                        + '&localeMapping=' + encodeURIComponent(localeMapping);
                location.href = redirectTo;
            }
        }

        function disableSetIDSelection(selectElementID) {
            document.getElementById(selectElementID).setAttribute("disabled", "disabled");
        }

        function setLocaleMappingCheckBox(option) {
            if (option != null && option == "no") {
                document.getElementById("localeNo").checked = true;
            } else {
                document.getElementById("localeYes").checked = true;
            }

        }

        function cancelForm() {
            location.href = 'challenges-mgt-add.jsp?clear=true';
        }

    </script>

    <div id="middle">
        <h2><fmt:message key="challenge.add"/></h2>

        <form id="questionForm" name="questionForm" method="post" action="challenges-mgt-finish-ajaxprocessor.jsp">
            <div id="workArea">
                <table class="styledLeft">
                    <tr>
                        <td class="formRow">
                            <table class="normal" cellspacing="0">
                                <tr>
                                    <td class="leftCol-med labelField">
                                        <fmt:message key="challenge.question.add.questionSet"/>
                                        <span class="required">*</span>
                                    </td>
                                    <td>
                                        <select id="setName0" name="setName0" onchange="onChangeSelect()"
                                                class="leftCol-med">
                                            <option value="selectID">--- Select Set Id ---</option>

                                            <%
                                                for (String setURI : challengeURIs) {
                                                    if (StringUtils.isNotBlank(setName) && StringUtils.equalsIgnoreCase(setURI, setName)) {
                                            %>
                                            <option value="<%=Encode.forHtmlAttribute(setURI)%>" selected="selected">
                                                <%=Encode.forHtmlContent(setURI)%>
                                            </option>
                                            <%
                                            } else {
                                            %>
                                            <option value="<%=Encode.forHtmlAttribute(setURI)%>">
                                                <%=Encode.forHtmlContent(setURI)%>
                                            </option>
                                            <%
                                                    }
                                                }
                                            %>
                                        </select>
                                    </td>
                                </tr>
                                <td class="leftCol-med labelField">
                                    <fmt:message key="challenge.question.add.locale.mapping"/>
                                    <span class="required">*</span>
                                </td>
                                <td>
                                    <input type="radio" name="localeMapping" id="localeYes" value="yes"
                                           onchange="getQuestionIDs()">Yes</input>
                                    <input type="radio" name="localeMapping" id="localeNo" value="no"
                                           onchange="getQuestionIDs()">No</input>

                                    <% if (StringUtils.isNotBlank(localeMapping) && StringUtils.equalsIgnoreCase("yes", localeMapping)) {%>
                                    <script>
                                        setLocaleMappingCheckBox("yes");
                                    </script>
                                    <%
                                        } else  {
                                    %>
                                    <script>
                                        setLocaleMappingCheckBox("no");
                                    </script>
                                    <%}%>

                                </td>
                                </tr>
                                <tr>
                                    <td class="leftCol-med labelField">
                                        <fmt:message key="challenge.question.add.questionId"/>
                                        <span class="required">*</span>
                                    </td>
                                    <td>
                                        <% if (StringUtils.isBlank(localeMapping) || StringUtils.equalsIgnoreCase("no", localeMapping)) {%>
                                        <input size="70" name="questionId0" id="questionId0" class="text-box-big"
                                               white-list-patterns="^[a-zA-Z0-9]+$"/>
                                        <%} else {%>
                                        <select id="questionId0" name="questionId0" class="leftCol-med">
                                            <%
                                                List<String> questionIds = Utils.getUniqueQuestionIds(challenges, setName);
                                                for (String qID : questionIds) {
                                                    if (StringUtils.isNotBlank(qID) && StringUtils.equalsIgnoreCase(questionId, qID)) {
                                            %>
                                            <option value="<%=Encode.forHtmlAttribute(qID)%>" selected="selected">
                                                <%=Encode.forHtmlContent(qID)%>
                                            </option>
                                            <%
                                            } else {
                                            %>
                                            <option value="<%=Encode.forHtmlAttribute(qID)%>">
                                                <%=Encode.forHtmlContent(qID)%>
                                            </option>
                                            <%
                                                    }
                                                }
                                            %>
                                        </select>

                                        <%}%>
                                    </td>
                                </tr>
                                <tr>
                                    <td class="leftCol-med labelField">
                                        <fmt:message key="challenge.question.add.questionLocale"/>
                                        <span class="required">*</span>
                                    </td>
                                    <td>
                                        <select id="questionLocale0" name="questionLocale0" class="leftCol-med">
                                            <%
                                                for (Locale locale : Locale.getAvailableLocales()) {
                                            %>
                                            <option value="<%=Encode.forHtmlAttribute(Utils.getLocaleString(locale))%>">
                                                <%=Encode.forHtmlContent(locale.getDisplayName())%>
                                            </option>
                                            <%
                                                }
                                            %>
                                        </select>
                                    </td>
                                </tr>
                                <tr>
                                    <td class="leftCol-med labelField">
                                        <fmt:message key="challenge.question.add.question"/>
                                        <span class="required">*</span>
                                    </td>
                                    <td class="leftCol-big">
                                        <% if (StringUtils.isNotBlank(question)) { %>
                                        <input size="100" name="question0" id="question0" class="text-box-big"
                                               value='<%=Encode.forHtmlAttribute(question)%>'/>
                                        <%} else { %>
                                        <input size="100" name="question0" id="question0" class="text-box-big"/>
                                        <% }%>
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        <button id="addButton" onclick="addRow()" type="button"
                                                class="button">Add
                                        </button>
                                    </td>
                                </tr>
                            </table>
                        </td>
                    </tr>
                </table>

                <!-- Table displaying currently modified challenge questions in the set. -->
                <p>&nbsp;</p>
                <% if (!updatedChallenges.isEmpty()) { %>
                <table class="styledLeft">
                    <tr>
                        <td class="nopadding">
                            <table cellspacing="0" id="mainTable0" style="width:100%;border:none !important">
                                <thead>
                                <th class="leftCol-small"><fmt:message key="challenge.question.questionId"/></th>
                                <th class="leftCol-small"><fmt:message key="challenge.question.question"/></th>
                                <th class="leftCol-small"><fmt:message key="challenge.question.questionLocale"/></th>
                                <th><fmt:message key="challenge.question.actions"/></th>
                                </thead>
                                <tbody>

                                <%
                                    if (updatedChallenges.size() > 0) {
                                        for (int i = 0; i < updatedChallenges.size(); i++) {
                                            ChallengeQuestion tempQuestion = updatedChallenges.get(i);
                                            if (StringUtils.isNotBlank(setName) && StringUtils.equalsIgnoreCase(tempQuestion.getQuestionSetId(), setName)) {

                                %>
                                <tr>
                                    <td width="10%"><%=Encode.forHtmlContent(tempQuestion.getQuestionId())%>
                                    </td>
                                    <% if (IdentityUtil.isNotBlank(editRowId) && (Integer.parseInt(editRowId) == i)) {%>
                                    <td id="questionTD<%=i%>" width="60%" bgcolor="#ffffb3"
                                        contenteditable="true"><i><%=Encode.forHtmlContent(tempQuestion.getQuestion())%>
                                    </i>
                                    </td>
                                    <%
                                    } else {
                                    %>
                                    <td id="questionTD<%=i%>"
                                        width="60%"><%=Encode.forHtmlContent(tempQuestion.getQuestion())%>
                                    </td>
                                    <%
                                        }%>

                                    <td width="10%"><%=Encode.forHtmlContent(tempQuestion.getLocale())%>
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
                <%}%>

                <!-- Table displaying persisted challenge questions in the set. -->
                <p>&nbsp;</p>
                <p>&nbsp;</p>
                <p>&nbsp;</p>
                <table class="styledLeft">
                    <tr>
                        <td class="nopadding">
                            <table cellspacing="0" id="mainTable" style="width:100%;border:none !important">
                                <thead>
                                <th class="leftCol-small"><fmt:message key="challenge.question.questionId"/></th>
                                <th class="leftCol-small"><fmt:message key="challenge.question.question"/></th>
                                <th class="leftCol-small"><fmt:message key="challenge.question.questionLocale"/></th>
                                </thead>
                                <tbody>

                                <%
                                    if (challenges.size() > 0) {
                                        for (ChallengeQuestion challengeQuestion : challenges) {
                                            if (StringUtils.isNotBlank(setName) && StringUtils.equalsIgnoreCase(challengeQuestion.getQuestionSetId(), setName)) {

                                %>
                                <tr>
                                    <td width="20%"><%=Encode.forHtmlContent(challengeQuestion.getQuestionId())%>
                                    </td>
                                    <td width="60%"><%=Encode.forHtmlContent(challengeQuestion.getQuestion())%>
                                    </td>
                                    <td width="20%"><%=Encode.forHtmlContent(challengeQuestion.getLocale())%>
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


                </table>

            </div>
        </form>
    </div>
</fmt:bundle>

