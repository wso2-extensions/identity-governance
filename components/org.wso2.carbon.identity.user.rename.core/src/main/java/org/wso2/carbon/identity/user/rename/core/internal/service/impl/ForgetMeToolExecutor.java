/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.rename.core.internal.service.impl;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateException;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * The class which executes the forget me tool script with needed arguments.
 * <p>
 * The applicable script file (.sh or .bat) is picked based on the OS and run as a separate process.
 */
public class ForgetMeToolExecutor {

    private final static Log log = LogFactory.getLog(ForgetMeToolExecutor.class);

    private static final String SYSTEM_PROPERTY_CARBON_HOME = "carbon.home";
    private static final String FORGETME_TOOL_HOME = "repository/components/tools/forget-me";
    private static final String SCRIPT_NAME_BASH = "forgetme.sh";
    private static final String SCRIPT_NAME_WINDOWS = "forgetme.bat";
    private static final String SYSTEM_PROPERTY_OS_NAME = "os.name";
    private static final String USER_STORE_EXTENSION_CONF_RELATIVE_PATH = "extensions/user-store/conf";

    /**
     * Executes ForgetMeTool to update the existing username of the user.
     *
     * @param existingUsername existing username of the user
     * @param newUsername new username of the user
     * @param userStoreDomain user store domain of the user
     * @param tenantDomain tenant domain of the user
     * @param tenantId tenant id
     * @throws UsernameUpdateException
     */
    public static void run(String existingUsername, String newUsername, String userStoreDomain, String tenantDomain,
                           int tenantId) throws UsernameUpdateException {

        int status = new ForgetMeToolExecutor().execute(existingUsername, newUsername, userStoreDomain, tenantDomain,
                tenantId);
        if (status != 0) {
            throw new UsernameUpdateException("Error while updating the username from : " + existingUsername + " to: " +
                    newUsername + " of user in userstore domain: " + userStoreDomain + " and tenant domain: " +
                    tenantDomain + ". Refer logs at " + Paths.get(System.getProperty(SYSTEM_PROPERTY_CARBON_HOME))
                    .resolve(Paths.get(FORGETME_TOOL_HOME)).resolve(Paths.get
                            (USER_STORE_EXTENSION_CONF_RELATIVE_PATH)).toString() + " for more information");
        }
    }

    private int execute(String existingUsername, String newUsername, String userStoreDomain, String tenantDomain,
                        int tenantId) throws UsernameUpdateException {

        String scriptName = getScriptForOS();

        if (scriptName == null) {
            throw new UsernameUpdateException(String.format("The operating system '%s' doesn't support the forget-me tool script.",
                    System.getProperty(SYSTEM_PROPERTY_OS_NAME)));
        }

        String scriptPath = Paths.get(System.getProperty(SYSTEM_PROPERTY_CARBON_HOME)).resolve(Paths.get
                (FORGETME_TOOL_HOME, "bin", scriptName)).toString();
        if (log.isDebugEnabled()) {
            log.debug("Resolved forget-me tool script path to: " + scriptPath);
        }

        String arguments = buildForgetMeToolArguments(existingUsername, newUsername, userStoreDomain, tenantDomain, tenantId);
        Process process = null;
        try {
            process = Runtime.getRuntime().exec(scriptPath + " " + arguments);
            if (log.isInfoEnabled()) {
                log.info(IOUtils.toString(process.getInputStream()));
            }
            process.waitFor();
            return process.exitValue();
        } catch (IOException | InterruptedException e) {
            throw new UsernameUpdateException("Error while executing username update process for user: " +
                    existingUsername + " in tenant: " + tenantDomain, e);
        }
    }

    private String getScriptForOS() {

        String osName = System.getProperty(SYSTEM_PROPERTY_OS_NAME);
        String scriptName = null;

        if (isUnixOS(osName) || isMacOS(osName)) {
            scriptName = SCRIPT_NAME_BASH;
        } else if (isWindowsOS(osName)) {
            scriptName = SCRIPT_NAME_WINDOWS;
        }

        return scriptName;
    }

    private boolean isWindowsOS(String osName) {

        return osName.toUpperCase().indexOf("WIN") >= 0;
    }

    private boolean isUnixOS(String osName) {

        return (osName.toUpperCase().indexOf("NIX") >= 0 ||
                osName.toUpperCase().indexOf("NUX") >= 0 ||
                osName.toUpperCase().indexOf("AIX") > 0);

    }

    private boolean isMacOS(String osName) {

        return (osName.toUpperCase().indexOf("MAC") >= 0);
    }

    private String buildForgetMeToolArguments(String existingUsername, String newUsername, String userStoreDomain,
                                              String tenantDomain, int tenantId) {

        List<String> args = new ArrayList<>();

        args.add("-d");
        String configDirectoryPath = Paths.get(System.getProperty(SYSTEM_PROPERTY_CARBON_HOME)).resolve(Paths.get
                (FORGETME_TOOL_HOME, USER_STORE_EXTENSION_CONF_RELATIVE_PATH)).toString();
        args.add(configDirectoryPath);

        args.add("-U");
        args.add(existingUsername);

        args.add("-pu");
        args.add(newUsername);

        args.add("-TID");
        args.add(String.valueOf(tenantId));

        args.add("-T");
        args.add(tenantDomain);

        args.add("-D");
        args.add(userStoreDomain);

        return String.join(" ", args);
    }
}
