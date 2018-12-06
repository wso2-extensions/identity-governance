/*
 *   Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *   WSO2 Inc. licenses this file to you under the Apache License,
 *   Version 2.0 (the "License"); you may not use this file except
 *   in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

package org.wso2.carbon.identity.user.session.endpoint.util;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.logging.Log;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.user.session.constant.SessionConstants;
import org.wso2.carbon.identity.user.session.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.session.endpoint.dto.ApplicationDTO;
import org.wso2.carbon.identity.user.session.endpoint.dto.SessionDTO;
import org.wso2.carbon.identity.user.session.endpoint.dto.AllSessionsDTO;
import org.wso2.carbon.identity.user.session.endpoint.exception.*;
import org.wso2.carbon.identity.user.session.manager.SessionManager;
import org.wso2.carbon.identity.user.session.model.Application;
import org.wso2.carbon.identity.user.session.model.UserSession;

import java.util.ArrayList;
import java.util.List;

public class EndpointUtils {

    public static SessionManager getSessionManager() {

        return (SessionManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(SessionManager.class, null);
    }

    public static AllSessionsDTO getSessionResponse(UserSession[] userSessionList) {
        AllSessionsDTO initiateAllSessionsDTO = new AllSessionsDTO();

        List<SessionDTO> userSessionDTOS = new ArrayList<>();

        for (UserSession userSession : userSessionList) {
            SessionDTO sessionDTO = new SessionDTO();
            sessionDTO.setApplications(getApplicationDTO(userSession.getApplications()));
            sessionDTO.setUserAgent(userSession.getUserAgent());
            sessionDTO.setIp(userSession.getIp());
            sessionDTO.setLoginTime(userSession.getLoginTime());
            sessionDTO.setLastAccessTime(userSession.getLastAccessTime());
            sessionDTO.setSessionId(DigestUtils.sha256Hex(userSession.getSessionId()));
            userSessionDTOS.add(sessionDTO);
        }
        initiateAllSessionsDTO.setSessions(userSessionDTOS);
        return initiateAllSessionsDTO;
    }

    public static List<ApplicationDTO> getApplicationDTO(Application[] applicationList) {
        List<ApplicationDTO> applicationDTOList = new ArrayList<>();

        for (Application application : applicationList) {
            ApplicationDTO applicationDTO = new ApplicationDTO();
            applicationDTO.setApp(application.getAppName());
            applicationDTO.setSubject(application.getSubject());
            applicationDTOList.add(applicationDTO);
        }
        return applicationDTOList;
    }

    /**
     * This method is used to create a ConflictRequestException with the known errorCode and message.
     *
     * @param description Error Message Description.
     * @param code        Error Code.
     * @return ConflictRequestException with the given errorCode and description.
     */
    public static ConflictRequestException buildConflictRequestException(String description, String code,
                                                                         Log log, Throwable e) {

        ErrorDTO errorDTO = getErrorDTO(SessionConstants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, description, code);
        logDebug(log, e);
        return new ConflictRequestException(errorDTO);
    }

    /**
     * This method is used to create a NotFoundException with the known errorCode and message.
     *
     * @param description Error Message Description.
     * @param code        Error Code.
     * @return NotFoundException with the given errorCode and description.
     */
    public static NotFoundException buildNotFoundRequestException(String description, String code,
                                                                  Log log, Throwable e) {

        ErrorDTO errorDTO = getErrorDTO(SessionConstants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, description, code);
        logDebug(log, e);
        return new NotFoundException(errorDTO);
    }

    private static ErrorDTO getErrorDTO(String message, String description, String code) {

        ErrorDTO errorDTO = new ErrorDTO();
        errorDTO.setCode(code);
        errorDTO.setMessage(message);
        errorDTO.setDescription(description);
        return errorDTO;
    }

    /**
     * This method is used to create a Forbidden Exception with the known errorCode and message.
     *
     * @param description Error Message Description.
     * @param code        Error Code.
     * @return ForbiddenException with the given errorCode and description.
     */
    public static ForbiddenException buildForbiddenException(String description, String code,
                                                             Log log, Throwable e) {

        ErrorDTO errorDTO = getErrorDTO(SessionConstants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, description, code);
        logDebug(log, e);
        return new ForbiddenException(errorDTO);
    }

    /**
     * This method is used to create a BadRequestException with the known errorCode and message.
     *
     * @param description Error Message Desription.
     * @param code        Error Code.
     * @return BadRequestException with the given errorCode and description.
     */
    public static BadRequestException buildBadRequestException(String description, String code,
                                                               Log log, Throwable e) {

        ErrorDTO errorDTO = getErrorDTO(SessionConstants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, description, code);
        logDebug(log, e);
        return new BadRequestException(errorDTO);
    }

    /**
     * This method is used to create an InternalServerErrorException with the known errorCode.
     *
     * @param code Error Code.
     * @return a new InternalServerErrorException with default details.
     */
    public static InternalServerErrorException buildInternalServerErrorException(String code,
                                                                                 Log log, Throwable e) {
        ErrorDTO errorDTO = getErrorDTO(SessionConstants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT,
                SessionConstants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT, code);
        logError(log, e);
        return new InternalServerErrorException(errorDTO);
    }

    private static void logDebug(Log log, Throwable throwable) {
        if (log.isDebugEnabled()) {
            log.debug(SessionConstants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, throwable);
        }
    }

    private static void logError(Log log, Throwable throwable) {
        log.error(throwable.getMessage(), throwable);
    }
}
