package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.SecurityQuestionApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.SecurityQuestionApiServiceImpl;

public class SecurityQuestionApiServiceFactory {

   private final static SecurityQuestionApiService service = new SecurityQuestionApiServiceImpl();

   public static SecurityQuestionApiService getSecurityQuestionApi()
   {
      return service;
   }
}
