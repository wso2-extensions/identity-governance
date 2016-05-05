<!--
~ Copyright (c) 2015, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
~
~ WSO2 Inc. licenses this file to you under the Apache License,
~ Version 2.0 (the "License"); you may not use this file except
~ in compliance with the License.
~ You may obtain a copy of the License at
~
~ http://www.apache.org/licenses/LICENSE-2.0
~
~ Unless required by applicable law or agreed to in writing,
~ software distributed under the License is distributed on an
~ "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
~ KIND, either express or implied. See the License for the
~ specific language governing permissions and limitations
~ under the License.
-->

<%@ page import="java.util.HashMap" %>
<%@ page import="org.wso2.carbon.CarbonConstants" %>
<%@ page import="org.apache.axis2.context.ConfigurationContext" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIUtil" %>
<%@ page import="org.wso2.carbon.utils.ServerConstants" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.Map" %>
<%@ page import="org.wso2.carbon.identity.governance.ui.IdentityGovernanceAdminClient" %>
<%@ page import="org.wso2.carbon.identity.governance.stub.bean.ConnectorConfig" %>
<%@ page import="org.wso2.carbon.identity.governance.stub.bean.Property" %>


<%

    String cookie = (String) session.getAttribute(ServerConstants.ADMIN_SERVICE_COOKIE);
    String backendServerURL = CarbonUIUtil.getServerURL(config.getServletContext(), session);
    ConfigurationContext configContext = (ConfigurationContext) config.getServletContext()
            .getAttribute(CarbonConstants.CONFIGURATION_CONTEXT);

    Map<String, String> configMap = new HashMap<String, String>();

    IdentityGovernanceAdminClient client = new IdentityGovernanceAdminClient(cookie, backendServerURL, configContext);
    ConnectorConfig[] configs = client.getConnectorList();

%>


<script type="text/javascript">
    jQuery(document).ready(function () {

        jQuery('h2.trigger').click(function () {
            if (jQuery(this).next().is(":visible")) {
                this.className = "active trigger";
            } else {
                this.className = "trigger";
            }
            jQuery(this).next().slideToggle("fast");
            return false; //Prevent the browser jump to the link anchor
        });
    });

    $(function () {
        $('#submitBtn').click(function (e) {
            e.preventDefault();
            $('#addTenantConfigurationForm').submit();
        });
    });

    function cancel() {
        location.href = "../admin/login.jsp";
    }

</script>

<fmt:bundle basename="org.wso2.carbon.identity.event.admin.ui.i18n.Resources">
<div id="middle">

<h2>
    Add Configuration Details
</h2>

<div id="workArea">
<form id="addTenantConfigurationForm" name="addTenantConfigurationForm" action="add_config_ajaxprocessor.jsp"
      method="post">
    <% Map<String, String> configurations = new HashMap<String, String>();
   int i=0;
    List<String> values = new ArrayList<String>() {{
        add("true");
        add("false");
    }};
%>

    <%for (int j = 0; j < configs.length; j++) {%>
    <h2 id="role_permission_config_head22" class="active trigger">
        <a href="#"><%=configs[i].getFriendlyName()%>
        </a>
    </h2>

    <div class="toggle_container sectionSub" style="margin-bottom:10px; display: none;" id="roleConfig2">

        <table>

            <%
            Property[] connectorProperties = configs[i].getProperties();
            for(int k = 0; k < connectorProperties.length; k++) {%>
                <tr>
                    <td>
                        <%=connectorProperties[k].getName()%>
                    </td>
                    <td colspan="2"><input type="text" name=<%=connectorProperties[k].getName()%>
                                           id=<%=connectorProperties[k].getName()%> style="width:400px"
                                           value="<%=connectorProperties[k].getValue()%>"/>
                    </td>
                </tr>
            <%}%>
                </table></div>
    <%}
    %>

<%--<h2 id="role_permission_config_head22" class="active trigger">--%>
    <%--<a href="#">Account Lock Configuration</a>--%>
<%--</h2>--%>

<%--<div class="toggle_container sectionSub" style="margin-bottom:10px; display: none;" id="roleConfig2">--%>

<%--<table>--%>

<%--<tr>--%>
    <%--<%--%>
        <%--configurations.put("tenantConfiguration" + i, configMap.get("accountLock.enable"));--%>
    <%--%>--%>

    <%--<td><span>Account Lock Enable</span></td>--%>
    <%--<td colspan="2" name="accountLock.enable" id="accountLock.enable" style="width:410px">--%>
        <%--<%--%>
            <%--for (String value : values) {--%>
                <%--if (value.equals(configurations.get("tenantConfiguration" + i))) {--%>
        <%--%>--%>
        <%--<input type="radio" name="accountLock.enable"--%>
               <%--value="<%=value%>" checked="checked"><%=value%>--%>
        <%--<input type="hidden" name="accountLock.enable.Original"--%>
               <%--value="<%=value%>">--%>
        <%--<%--%>
        <%--} else {--%>
        <%--%>--%>
        <%--<input type="radio" name="accountLock.enable"--%>
               <%--value="<%=value%>"><%=value%>--%>
        <%--<%--%>
                <%--}--%>
            <%--}--%>
        <%--%>--%>

    <%--</td>--%>


<%--</tr>--%>



    <%--<tr>--%>
        <%--<%--%>
            <%--configurations.put("tenantConfiguration" + i, configMap.get("accountLock.On.Failure.Max.Attempts"));--%>
        <%--%>--%>
        <%--<td>--%>
            <%--Maximum Failed Login Attempts--%>
        <%--</td>--%>
        <%--<td colspan="2"><input type="text" name="accountLock.On.Failure.Max.Attempts"--%>
                               <%--id="accountLock.On.Failure.Max.Attempts" style="width:400px"--%>
                               <%--value="<%=configurations.get("tenantConfiguration"+i)%>"/>--%>
        <%--</td>--%>
    <%--</tr>--%>

    <%--<tr>--%>
        <%--<%--%>
            <%--configurations.put("tenantConfiguration" + i, configMap.get("accountLock.Time"));--%>
        <%--%>--%>
        <%--<td>--%>
            <%--Account Lock Time--%>
        <%--</td>--%>
        <%--<td colspan="2"><input type="text" name="accountLock.Time"--%>
                               <%--id="accountLock.Time" style="width:400px"--%>
                               <%--value="<%=configurations.get("tenantConfiguration"+i)%>"/>--%>
    <%--</tr>--%>
<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Account.Unlock.Enable"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td><span>Account Unlock Enable</span></td>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td colspan="2" name="Account.Unlock.Enable" id="Account.Unlock.Enable" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Unlock.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Account.Unlock.Enable.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Unlock.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("accountLock.On.Failure.Max.Attempts"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td><span>Account Max Attempt Enable</span></td>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td colspan="2" name="Account.Max.Attempt.Enable" id="Account.Max.Attempt.Enable" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Max.Attempt.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Account.Max.Attempt.Enable.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Max.Attempt.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>


<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Account.OneTime.Password.Enable"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td><span>Account Onetime Password Enable</span></td>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td colspan="2" name="Account.OneTime.Password.Enable" id="Account.OneTime.Password.Enable" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.OneTime.Password.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Account.OneTime.Password.Enable.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.OneTime.Password.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Account.Password.Reuse.Enable"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td><span>Account Password Reuse Enable</span></td>&ndash;%&gt;--%>


    <%--&lt;%&ndash;<td colspan="2" name="Account.Password.Reuse.Enable" id="Account.Password.Reuse.Enable" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Password.Reuse.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Account.Password.Reuse.Enable.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Password.Reuse.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>


<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Account.Password.Expire.Enable"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td><span>Account Password Expire Enable</span></td>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td colspan="2" name="Account.Password.Expire.Enable" id="Account.Password.Expire.Enable" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Password.Expire.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Account.Password.Expire.Enable.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Account.Password.Expire.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>


<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Notification.Sending.Enable"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<span>Notification Sending Enable</span></td>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td colspan="2" name="Notification.Sending.Enable" id="Notification.Sending.Enable" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Notification.Sending.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Notification.Sending.Enable.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Notification.Sending.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Notification.Expire.Time"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Notification Expire Time&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td colspan="2"><input type="text" name="Notification.Expire.Time"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;id="Notification.Expire.Time" style="width:400px"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;value="<%=configurations.get("tenantConfiguration"+i)%>"/>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Notification.Sending.Internally.Managed"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Notification Sending Internally Managed&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>


    <%--&lt;%&ndash;<td colspan="2" name="Notification.Sending.Internally.Managed" id="Notification.Sending.Internally.Managed"&ndash;%&gt;--%>
        <%--&lt;%&ndash;style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (configurations.get("tenantConfiguration" + i).equals(value)) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Notification.Sending.Internally.Managed"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Notification.Sending.Internally.Managed.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Notification.Sending.Internally.Managed"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>


<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Enable"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Authentication Policy Enable&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>


    <%--&lt;%&ndash;<td colspan="2" name="Authentication.Policy.Enable" id="Authentication.Policy.Enable" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (configurations.get("tenantConfiguration" + i).equals(value)) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Authentication.Policy.Enable.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Enable"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Check.Account.Exist"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Authentication Policy Check Account Exist&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>


    <%--&lt;%&ndash;<td colspan="2" name="Authentication.Policy.Check.Account.Exist" id="Authentication.Policy.Check.Account.Exist"&ndash;%&gt;--%>
        <%--&lt;%&ndash;style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (configurations.get("tenantConfiguration" + i).equals(value)) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Check.Account.Exist"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Authentication.Policy.Check.Account.Exist.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Check.Account.Exist"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Check.Password.Expire"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Authentication Policy Check Password Expire&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td colspan="2" name="Authentication.Policy.Check.Password.Expire" id="Authentication.Policy.Check.Password.Expire"&ndash;%&gt;--%>
        <%--&lt;%&ndash;style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Check.Password.Expire"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Authentication.Policy.Check.Password.Expire.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Check.Password.Expire"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Password.Expire.Time"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Authentication Policy Password Expire Time&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td colspan="2"><input type="text" name="Authentication.Policy.Password.Expire.Time"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;id="Authentication.Policy.Password.Expire.Time" style="width:400px"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;value="<%=configurations.get("tenantConfiguration"+i)%>"/>&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Account.Lock.Time"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Authentication Policy Account Lock Time&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td colspan="2"><input type="text" name="Authentication.Policy.Account.Lock.Time"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;id="Authentication.Policy.Account.Lock.Time" style="width:400px"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;value="<%=configurations.get("tenantConfiguration"+i)%>"/>&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Account.Lock.On.Failure"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Authentication Policy Account Lock On Failure&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td colspan="2" name="Authentication.Policy.Account.Lock.On.Failure"&ndash;%&gt;--%>
        <%--&lt;%&ndash;id="Authentication.Policy.Account.Lock.On.Failure" style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Account.Lock.On.Failure"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Authentication.Policy.Account.Lock.On.Failure.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Account.Lock.On.Failure"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>


<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Check.Password.Reuse"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Authentication Policy Check Password Reuse&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

    <%--&lt;%&ndash;<td colspan="2" name="Authentication.Policy.Check.Password.Reuse" id="Authentication.Policy.Check.Password.Reuse"&ndash;%&gt;--%>
        <%--&lt;%&ndash;style="width:410px">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
            <%--&lt;%&ndash;for (String value : values) {&ndash;%&gt;--%>
                <%--&lt;%&ndash;if (value.equals(configurations.get("tenantConfiguration" + i))) {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Check.Password.Reuse"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>" checked="checked"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="hidden" name="Authentication.Policy.Check.Password.Reuse.Original"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>">&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;} else {&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;<input type="radio" name="Authentication.Policy.Check.Password.Reuse"&ndash;%&gt;--%>
               <%--&lt;%&ndash;value="<%=value%>"><%=value%>&ndash;%&gt;--%>
        <%--&lt;%&ndash;&lt;%&ndash;%>--%>
                <%--&lt;%&ndash;}&ndash;%&gt;--%>
            <%--&lt;%&ndash;}&ndash;%&gt;--%>
        <%--&lt;%&ndash;%>&ndash;%&gt;--%>

    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>

<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>

<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Password.Expire.Frequency"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Password Expire Frequency&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td colspan="2"><input type="text" name="Password.Expire.Frequency"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;id="Password.Expire.Frequency" style="width:400px"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;value="<%=configurations.get("tenantConfiguration"+i)%>"/>&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>


<%--&lt;%&ndash;<tr>&ndash;%&gt;--%>
    <%--&lt;%&ndash;&lt;%&ndash;%>--%>
        <%--&lt;%&ndash;configurations.put("tenantConfiguration" + i, configMap.get("Password.Reuse.Frequency"));&ndash;%&gt;--%>
    <%--&lt;%&ndash;%>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td>&ndash;%&gt;--%>
        <%--&lt;%&ndash;Password Reuse Frequency&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
    <%--&lt;%&ndash;<td colspan="2"><input type="text" name="Password.Reuse.Frequency"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;id="Password.Reuse.Frequency" style="width:400px"&ndash;%&gt;--%>
                           <%--&lt;%&ndash;value="<%=configurations.get("tenantConfiguration"+i)%>"/>&ndash;%&gt;--%>
    <%--&lt;%&ndash;</td>&ndash;%&gt;--%>
<%--&lt;%&ndash;</tr>&ndash;%&gt;--%>


<%--</table>--%>
<%--</div>--%>

<%--<h2 id="role_permission_config_head22" class="active trigger">--%>
    <%--<a href="#">Self-signup Configuration</a>--%>
<%--</h2>--%>

<%--<div class="toggle_container sectionSub" style="margin-bottom:10px; display: none;" id="roleConfig2">--%>

    <%--<table>--%>


        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Captcha.Verification.Internally.Managed"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Captcha Verification Internally Managed--%>
            <%--</td>--%>


            <%--<td colspan="2" name="Captcha.Verification.Internally.Managed" id="Captcha.Verification.Internally.Managed"--%>
                <%--style="width:410px">--%>
                <%--<%--%>
                    <%--for (String value : values) {--%>
                        <%--if (value.equals(configurations.get("tenantConfiguration" + i))) {--%>
                <%--%>--%>
                <%--<input type="radio" name="Captcha.Verification.Internally.Managed"--%>
                       <%--value="<%=value%>" checked="checked"><%=value%>--%>
                <%--<input type="hidden" name="Captcha.Verification.Internally.Managed.Original"--%>
                       <%--value="<%=value%>">--%>
                <%--<%--%>
                <%--} else {--%>
                <%--%>--%>
                <%--<input type="radio" name="Captcha.Verification.Internally.Managed"--%>
                       <%--value="<%=value%>"><%=value%>--%>
                <%--<%--%>
                        <%--}--%>
                    <%--}--%>
                <%--%>--%>

            <%--</td>--%>

        <%--</tr>--%>


    <%--</table>--%>
<%--</div>--%>


<%--<h2 id="role_permission_config_head22" class="active trigger">--%>
    <%--<a href="#">User ID Recovery Configuration</a>--%>
<%--</h2>--%>

<%--<div class="toggle_container sectionSub" style="margin-bottom:10px; display: none;" id="roleConfig2">--%>

    <%--<table>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Check.Account.Lock"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Authentication Policy Check Account Lock--%>
            <%--</td>--%>

            <%--<td colspan="2" name="Authentication.Policy.Check.Account.Lock"--%>
                <%--id="Authentication.Policy.Check.Account.Lock" style="width:410px">--%>
                <%--<%--%>
                    <%--for (String value : values) {--%>
                        <%--if (value.equals(configurations.get("tenantConfiguration" + i))) {--%>
                <%--%>--%>
                <%--<input type="radio" name="Authentication.Policy.Check.Account.Lock"--%>
                       <%--value="<%=value%>" checked="checked"><%=value%>--%>
                <%--<input type="hidden" name="Authentication.Policy.Check.Account.Lock.Original"--%>
                       <%--value="<%=value%>">--%>
                <%--<%--%>
                <%--} else {--%>
                <%--%>--%>
                <%--<input type="radio" name="Authentication.Policy.Check.Account.Lock"--%>
                       <%--value="<%=value%>"><%=value%>--%>
                <%--<%--%>
                        <%--}--%>
                    <%--}--%>
                <%--%>--%>

            <%--</td>--%>

        <%--</tr>--%>

    <%--</table>--%>
<%--</div>--%>


<%--<h2 id="role_permission_config_head22" class="active trigger">--%>
    <%--<a href="#">Password Policy Configuration</a>--%>
<%--</h2>--%>

<%--<div class="toggle_container sectionSub" style="margin-bottom:10px; display: none;" id="roleConfig2">--%>

    <%--<table>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Password.policy.extensions.1.min.length"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Password policy extensions 1 min length--%>
            <%--</td>--%>
            <%--<td colspan="2"><input type="text" name="Password.policy.extensions.1.min.length"--%>
                                   <%--id="Password.policy.extensions.1.min.length" style="width:400px"--%>
                                   <%--value="<%=configurations.get("tenantConfiguration"+i)%>"/>--%>

            <%--</td>--%>
        <%--</tr>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Password.policy.extensions.1.max.length"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Password policy extensions 1 max length--%>
            <%--</td>--%>
            <%--<td colspan="2"><input type="text" name="Password.policy.extensions.1.max.length"--%>
                                   <%--id="Password.policy.extensions.1.max.length" style="width:400px"--%>
                                   <%--value="<%=configurations.get("tenantConfiguration"+i)%>"/>--%>
            <%--</td>--%>
        <%--</tr>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Password.policy.extensions.3.pattern"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Password policy extensions 3 pattern--%>
            <%--</td>--%>
            <%--<td colspan="2"><input type="text" name="Password.policy.extensions.3.pattern"--%>
                                   <%--id="Password.policy.extensions.3.pattern" style="width:400px"--%>
                                   <%--value="<%=configurations.get("tenantConfiguration"+i)%>"/>--%>

            <%--</td>--%>
        <%--</tr>--%>

    <%--</table>--%>
<%--</div>--%>


<%--<h2 id="role_permission_config_head22" class="active trigger">--%>
    <%--<a href="#">One Time Password Configuration</a>--%>
<%--</h2>--%>

<%--<div class="toggle_container sectionSub" style="margin-bottom:10px; display: none;" id="roleConfig2">--%>

    <%--<table>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Check.OneTime.Password"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Authentication Policy Check OneTime Password--%>
            <%--</td>--%>


            <%--<td colspan="2" name="Authentication.Policy.Check.OneTime.Password"--%>
                <%--id="Authentication.Policy.Check.OneTime.Password" style="width:410px">--%>
                <%--<%--%>
                    <%--for (String value : values) {--%>
                        <%--if (value.equals(configurations.get("tenantConfiguration" + i))) {--%>
                <%--%>--%>
                <%--<input type="radio" name="Authentication.Policy.Check.OneTime.Password"--%>
                       <%--value="<%=value%>" checked="checked"><%=value%>--%>
                <%--<input type="hidden" name="Authentication.Policy.Check.OneTime.Password.Original"--%>
                       <%--value="<%=value%>">--%>
                <%--<%--%>
                <%--} else {--%>
                <%--%>--%>
                <%--<input type="radio" name="Authentication.Policy.Check.OneTime.Password"--%>
                       <%--value="<%=value%>"><%=value%>--%>
                <%--<%--%>
                        <%--}--%>
                    <%--}--%>
                <%--%>--%>

            <%--</td>--%>

        <%--</tr>--%>
    <%--</table>--%>
<%--</div>--%>


<%--<h2 id="role_permission_config_head22" class="active trigger">--%>
    <%--<a href="#">User Creation Configuration</a>--%>
<%--</h2>--%>

<%--<div class="toggle_container sectionSub" style="margin-bottom:10px; display: none;" id="roleConfig2">--%>

    <%--<table>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("UserAccount.Verification.Enable"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--UserAccount Verification Enable--%>
            <%--</td>--%>

            <%--<td colspan="2" name="UserAccount.Verification.Enable" id="UserAccount.Verification.Enable"--%>
                <%--style="width:410px">--%>
                <%--<%--%>
                    <%--for (String value : values) {--%>
                        <%--if (value.equals(configurations.get("tenantConfiguration" + i))) {--%>
                <%--%>--%>
                <%--<input type="radio" name="UserAccount.Verification.Enable"--%>
                       <%--value="<%=value%>" checked="checked"><%=value%>--%>
                <%--<input type="hidden" name="UserAccount.Verification.Enable.Original"--%>
                       <%--value="<%=value%>">--%>
                <%--<%--%>
                <%--} else {--%>
                <%--%>--%>
                <%--<input type="radio" name="UserAccount.Verification.Enable"--%>
                       <%--value="<%=value%>"><%=value%>--%>
                <%--<%--%>
                        <%--}--%>
                    <%--}--%>
                <%--%>--%>

            <%--</td>--%>
        <%--</tr>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Temporary.Password.Enable"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Temporary Password Enable--%>
            <%--</td>--%>

            <%--<td colspan="2" name="Temporary.Password.Enable" id="Temporary.Password.Enable" style="width:410px">--%>
                <%--<%--%>
                    <%--for (String value : values) {--%>
                        <%--if (value.equals(configurations.get("tenantConfiguration" + i))) {--%>
                <%--%>--%>
                <%--<input type="radio" name="Temporary.Password.Enable"--%>
                       <%--value="<%=value%>" checked="checked"><%=value%>--%>
                <%--<input type="hidden" name="Temporary.Password.Enable.Original"--%>
                       <%--value="<%=value%>">--%>
                <%--<%--%>
                <%--} else {--%>
                <%--%>--%>
                <%--<input type="radio" name="Temporary.Password.Enable"--%>
                       <%--value="<%=value%>"><%=value%>--%>
                <%--<%--%>
                        <%--}--%>
                    <%--}--%>
                <%--%>--%>

            <%--</td>--%>

        <%--</tr>--%>


        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Temporary.Password.Default.Value"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Temporary Password Default Value--%>
            <%--</td>--%>
            <%--<td colspan="2"><input type="text" name="Temporary.Password.Default.Value"--%>
                                   <%--id="Temporary.Password.Default.Value" style="width:400px"--%>
                                   <%--value="<%=configurations.get("tenantConfiguration"+i)%>"/>--%>
            <%--</td>--%>
        <%--</tr>--%>

        <%--<tr>--%>
            <%--<%--%>
                <%--configurations.put("tenantConfiguration" + i, configMap.get("Authentication.Policy.Account.Lock.On.Creation"));--%>
            <%--%>--%>
            <%--<td>--%>
                <%--Authentication Policy Account Lock On Creation--%>
            <%--</td>--%>

            <%--<td colspan="2" name="Authentication.Policy.Account.Lock.On.Creation"--%>
                <%--id="Authentication.Policy.Account.Lock.On.Creation" style="width:410px">--%>
                <%--<%--%>
                    <%--for (String value : values) {--%>
                        <%--if (value.equals(configurations.get("tenantConfiguration" + i))) {--%>
                <%--%>--%>
                <%--<input type="radio" name="Authentication.Policy.Account.Lock.On.Creation"--%>
                       <%--value="<%=value%>" checked="checked"><%=value%>--%>
                <%--<input type="hidden" name="Authentication.Policy.Account.Lock.On.Creation.Original"--%>
                       <%--value="<%=value%>">--%>
                <%--<%--%>
                <%--} else {--%>
                <%--%>--%>
                <%--<input type="radio" name="Authentication.Policy.Account.Lock.On.Creation"--%>
                       <%--value="<%=value%>"><%=value%>--%>
                <%--<%--%>
                        <%--}--%>
                    <%--}--%>
                <%--%>--%>

            <%--</td>--%>
        <%--</tr>--%>
    <%--</table>--%>
<%--</div>--%>

<tr id="buttonRow">
    <td class="buttonRow">
        <input class="button" type="button" value="Cancel" onclick="cancel()"/>
        <input class="button" type="button" value="Update" id="submitBtn"/>
    </td>
</tr>


</div>
</form>
</div>
</div>
</fmt:bundle>