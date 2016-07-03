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
<%@page import="org.apache.commons.lang.StringUtils" %>

<%@page import="org.wso2.carbon.CarbonConstants" %>
<jsp:include page="../dialog/display_messages.jsp"/>


<%@ page import="org.wso2.carbon.context.CarbonContext" %>
<%@ page import="org.wso2.carbon.identity.recovery.stub.model.ChallengeQuestion" %>
<%@ page import="org.wso2.carbon.identity.recovery.ui.IdentityManagementAdminClient" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIMessage" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIUtil" %>
<%@ page import="org.wso2.carbon.utils.ServerConstants" %>
<%@ page import="java.util.ArrayList" %>


<%
    // clear the questions that were meant to be updated or deleted.
    String clear = request.getParameter("clear");
    if (StringUtils.isNotBlank(clear)) {
        session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_DELETE, new ArrayList<ChallengeQuestion>());
        session.setAttribute(IdentityManagementAdminClient.CHALLENGE_QUESTION_UPDATE, new ArrayList<ChallengeQuestion>());
    }

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
        ChallengeQuestion[] allChallengeQuestions = client.getChallengeQuestionsForTenant(tenantDomain);

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
    <carbon:breadcrumb label="challenge.question.set"
                       resourceBundle="org.wso2.carbon.identity.recovery.ui.i18n.Resources"
                       topPage="true"
                       request="<%=request%>"/>

    <div id="middle">
        <h2><fmt:message key='challenge.set.question.add'/></h2>

        <div id="workArea">
            <table class="styledLeft" id="internal" name="internal" width="100%">
                <tr class="tableOddRow">
                    <td style="width: 30px;">
                        <div style="height:30px;">
                            <a href="javascript:document.location.href='challenge-set-add.jsp'"
                               class="icon-link"
                               style="background-image:url(images/add.gif);">
                                <fmt:message key='challenge.set.add'/>
                            </a>
                        </div>
                    </td>
                </tr>
                <tr class="tableEvenRow">
                    <td style="width: 30px;">
                        <div style="height:30px;">
                            <a href="javascript:document.location.href='challenge-question-add.jsp'"
                               class="icon-link"
                               style="background-image:url(images/add.gif);">
                                <fmt:message key='challenge.question.add'/>
                            </a>
                        </div>
                    </td>
                </tr>

            </table>

            <br/>
        </div>
    </div>
</fmt:bundle>


