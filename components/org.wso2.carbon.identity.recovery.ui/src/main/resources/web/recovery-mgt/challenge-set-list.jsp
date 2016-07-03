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

<jsp:include page="../dialog/display_messages.jsp"/>

<%@ page import="org.wso2.carbon.context.CarbonContext" %>
<%@ page import="org.wso2.carbon.identity.recovery.stub.model.ChallengeQuestion" %>
<%@ page import="org.wso2.carbon.identity.recovery.ui.IdentityManagementAdminClient" %>
<%@ page import="org.wso2.carbon.identity.recovery.ui.Utils" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIMessage" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIUtil" %>
<%@ page import="org.wso2.carbon.utils.ServerConstants" %>
<%@ page import="java.util.Collections" %>
<%@ page import="java.util.List" %>


<%
    session.removeAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION);
    session.removeAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE);
    session.removeAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_UPDATE);
    List<ChallengeQuestion> challenges = null;
    List<String> questionSetNamesList = null;

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
        // retrieve all available challenge questions for user's tenant and locale.
        ChallengeQuestion[] challengeQuestions = client.getChallengeQuestionsForTenant(tenantDomain);
        questionSetNamesList = Utils.getChallengeSetUris(challengeQuestions);
        Collections.sort(questionSetNamesList);

    } catch (Exception e) {
        CarbonUIMessage.sendCarbonUIMessage(e.getMessage(), CarbonUIMessage.ERROR, request);
%>
<script type="text/javascript">
    location.href = "../admin/error.jsp";
</script>
<%
        return;
    }


%>

<fmt:bundle basename="org.wso2.carbon.identity.recovery.ui.i18n.Resources">
    <carbon:breadcrumb label="challenge.questions.mgt"
                       resourceBundle="org.wso2.carbon.identity.recovery.ui.i18n.Resources"
                       topPage="false" request="<%=request%>"/>

    <script type="text/javascript">
        function removeSet(row) {
            function doDelete() {
                $.ajax({
                    type: 'POST',
                    url: 'challenges-mgt-finish-ajaxprocessor.jsp',
                    headers: {
                        Accept: "text/html"
                    },
                    data: 'removeSetId=' + encodeURIComponent(row),
                    async: false,
                    success: function (responseText, status) {
                        if (status == "success") {
                            location.assign("challenge-set-list.jsp");
                        }
                    }
                });
            }

            CARBON.showConfirmationDialog("<fmt:message key="confirm.delete.challenge.set"/> " + row + " ?", doDelete, null);
        }

    </script>

    <div id="middle">
        <h2><fmt:message key='challenge.questions.mgt'/></h2>
        <div id="workArea">
                <%--<table class="normal">--%>
                <%--<tr>--%>
                <%--<td><a href="challenges-mgt.jsp" style="background-image: url(images/add.gif);"--%>
                <%--class="icon-link">Add new challenge questions set</a></td>--%>
                <%--</tr>--%>
                <%--</table>--%>

            <p>&nbsp;</p>

            <table class="styledLeft" style="width: 100%;">
                <thead>
                <th colspan="2" class="leftCol-small"><fmt:message key="challenge.question.set"/></th>
                </thead>
                <tbody>

                <% if (questionSetNamesList.size() > 0) {
                    for (String questionSetName : questionSetNamesList) {
                %>
                <tr>
                    <td width="50%">
                        <%=Encode.forHtmlContent(questionSetName)%>
                    </td>
                    <td width="20%">
                        <a href="challenges-mgt.jsp?setName=<%=Encode.forUriComponent(questionSetName)%>"
                           style='background-image:url(images/edit.gif);'
                           type="button" class="icon-link">
                            <fmt:message key="edit"/>
                        </a>
                        <a onclick="removeSet('<%=Encode.forJavaScriptAttribute(questionSetName)%>')"
                           style='background-image:url(images/delete.gif);'
                           type="button" class="icon-link">
                            <fmt:message key="delete"/>
                        </a>
                    </td>
                </tr>

                <%
                    }
                } else {
                %>
                <tr>
                    <td colspan="2">
                        <i><fmt:message key="challenge.set.none"/></i>
                    </td>
                </tr>
                <%
                    }
                %>
                </tbody>
            </table>
        </div>
    </div>
</fmt:bundle>
