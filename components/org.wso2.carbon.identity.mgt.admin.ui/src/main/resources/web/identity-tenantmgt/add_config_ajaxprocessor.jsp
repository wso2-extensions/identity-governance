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

<%@ page import="org.wso2.carbon.identity.mgt.admin.ui.TenantIdentityMgtClient" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIUtil" %>
<%@ page import="org.apache.axis2.context.ConfigurationContext" %>
<%@ page import="java.util.HashMap" %>
<%@page import="org.wso2.carbon.utils.ServerConstants" %>
<%@page import="org.wso2.carbon.CarbonConstants" %>
<%@ page import="java.util.ResourceBundle" %>
<%@ page import="java.text.MessageFormat" %>
<%@ page import="org.wso2.carbon.ui.CarbonUIMessage" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="java.util.Enumeration" %>

<%@ page contentType="text/html;charset=UTF-8" language="java" %>

<%
    String BUNDLE = "org.wso2.carbon.identity.event.admin.ui.i18n.Resources";
    ResourceBundle resourceBundle = ResourceBundle.getBundle(BUNDLE, request.getLocale());
    String forwardTo = null;
    try {
//        request.getParameterNames()
        System.out.println();
        String cookie = (String) session.getAttribute(ServerConstants.ADMIN_SERVICE_COOKIE);
        String backendServerURL = CarbonUIUtil.getServerURL(config.getServletContext(), session);
        ConfigurationContext configContext = (ConfigurationContext) config.getServletContext()
                .getAttribute(CarbonConstants.CONFIGURATION_CONTEXT);

        ArrayList<String> configNames = new ArrayList<String>() {{
            add("accountLock.enable");
            add("accountLock.Time");
            add("accountLock.On.Failure.Max.Attempts");
        }};


        HashMap<String, String> configMap = new HashMap<String, String>();

        for (int i = 0; i < configNames.size(); i++) {
            String configValue = request.getParameter(configNames.get(i));
            String configValueOriginal = request.getParameter(configNames.get(i) + ".Original");

            if (configValueOriginal != null) {
                if (!configValue.equals(configValueOriginal)) {
                    configMap.put(configNames.get(i), configValue);
                }
            } else if (configValue != null){
                configMap.put(configNames.get(i), configValue);
            }
        }

        TenantIdentityMgtClient client =
                new TenantIdentityMgtClient(cookie, backendServerURL, configContext);
        client.updateConfiguration(configMap);
        String message = MessageFormat.format(resourceBundle.getString("success.adding.config"), null);
        CarbonUIMessage.sendCarbonUIMessage(message, CarbonUIMessage.INFO, request);
        forwardTo = "../admin/login.jsp";
    } catch (Exception e) {
        String message = MessageFormat.format(resourceBundle.getString("error.adding.config"),
                new Object[]{e.getMessage()});
        CarbonUIMessage.sendCarbonUIMessage(message, CarbonUIMessage.ERROR, request);
        forwardTo = "../admin/login.jsp";
    }
%>

<script type="text/javascript">
    function forward() {
        location.href = "<%=forwardTo%>";
    }

    forward();
</script>