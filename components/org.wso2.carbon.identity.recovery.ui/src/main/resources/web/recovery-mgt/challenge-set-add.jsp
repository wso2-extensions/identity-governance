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
<%@page import="org.owasp.encoder.Encode" %>
<%@page import="org.wso2.carbon.CarbonConstants" %>
<script type="text/javascript" src="../identity/validation/js/identity-validate.js"></script>
<jsp:include page="../dialog/display_messages.jsp"/>

<%@ page import="org.wso2.carbon.context.CarbonContext" %>
<%@ page import="org.wso2.carbon.identity.recovery.stub.model.ChallengeQuestion" %>
<%@ page import="org.wso2.carbon.identity.recovery.ui.IdentityManagementAdminClient" %>
<%@ page import="org.wso2.carbon.identity.recovery.ui.Utils" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIMessage" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIUtil" %>
<%@ page import="org.wso2.carbon.utils.ServerConstants" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.Locale" %>


<%
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
        ChallengeQuestion[] challengeQuestionsForTenant = client.getChallengeQuestionsForTenant(tenantDomain);

        List<String> challengeSetUris = Utils.getChallengeSetUris(challengeQuestionsForTenant);

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


%>

<fmt:bundle basename="org.wso2.carbon.identity.recovery.ui.i18n.Resources">
    <carbon:breadcrumb label="challenge.add"
                       resourceBundle="org.wso2.carbon.identity.recovery.ui.i18n.Resources"
                       topPage="true" request="<%=request%>"/>

    <script type="text/javascript">

        function addRow() {
            var setName = document.getElementsByName("setName")[0].value;
            var question = document.getElementsByName("question0")[0].value;
            var questionId = document.getElementsByName("questionId0")[0].value;

            var sel = document.getElementsByName("questionLocale0")[0].selectedIndex;


            if (setName == "") {
                CARBON.showWarningDialog('Please provide the question set id');
                location.href = '#';
            } else if (question == null || question == "") {
                CARBON.showWarningDialog('Please enter a valid security question', null, null);
                location.href = '#';
            } else if (questionId == null || questionId == "") {
                CARBON.showWarningDialog('Please enter a valid question id', null, null);
                location.href = '#';
            } else if (sel == 0) {
                CARBON.showWarningDialog('Please select a locale for the question', null, null);
                location.href = '#';
            } else {
                if (!doValidateInput(document.getElementById("setName"), "Provided Question Set ID is invalid. Only {1} allowed.")) {
                    location.href = '#';
                } else if (!doValidateInput(document.getElementById("questionId0"), "Provided Question ID  is invalid. Only {1} allowed.")) {
                    location.href = '#';
                } else {
                    $("#questionForm").submit();
                    return true;
                }
            }
        }

        function cancelForm() {
            location.href = 'challenges-mgt-add.jsp';
        }

    </script>

    <div id="middle">
        <h2><fmt:message key="challenge.add"/></h2>

        <form id="questionForm" name="questionForm" method="post" action="challenges-mgt-finish-ajaxprocessor.jsp">
            <div id="workArea">
                <table class="styledLeft">
                    <thead>
                    <tr>
                        <th colspan="2"><fmt:message key='challenge.set.add.details'/></th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr>
                        <td class="formRow">
                            <table class="normal" cellspacing="0">
                                <tr>
                                    <td class="leftCol-med labelField">
                                        <fmt:message key="challenge.question.add.questionSet"/>
                                        <span class="required">*</span>
                                    </td>
                                    <td class="leftCol-big">
                                        <input name="setName" class="text-box-big" id="setName" size="100"
                                               white-list-patterns="^[a-zA-Z0-9]*$"/>
                                    </td>
                                </tr>
                                <tr>
                                    <td class="leftCol-med labelField">
                                        <fmt:message key="challenge.question.add.questionId"/>
                                        <span class="required">*</span>
                                    </td>
                                    <td class="leftCol-big"><input size="70" name="questionId0" id="questionId0"
                                                                   class="text-box-big"
                                                                   white-list-patterns="^[a-zA-Z0-9]*$"/>
                                    </td>
                                </tr>
                                <tr>
                                    <td class="leftCol-med labelField">
                                        <fmt:message key="challenge.question.add.questionLocale"/>
                                        <span class="required">*</span>
                                    </td>
                                    <td class="leftCol-big">
                                        <select id="questionLocale0" name="questionLocale0" class="leftCol-med">
                                            <% for (Locale locale : Locale.getAvailableLocales()) { %>
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
                                        <input size="100" name="question0" id="question0" class="text-box-big"/>
                                    </td>
                                </tr>
                                <tr>
                                    <td colspan="2" class="buttonRow">
                                        <button onclick="addRow()" type="button" class="button">Add</button>
                                        <button onclick="cancelForm()" type="button" class="button">Cancel</button>
                                    </td>
                                </tr>
                                </tbody>
                            </table>
                        </td>
                    </tr>
                </table>
            </div>
        </form>
    </div>
</fmt:bundle>

