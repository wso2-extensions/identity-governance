package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.ValidateAnswerApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.ValidateAnswerApiServiceImpl;

public class ValidateAnswerApiServiceFactory {

   private final static ValidateAnswerApiService service = new ValidateAnswerApiServiceImpl();

   public static ValidateAnswerApiService getValidateAnswerApi()
   {
      return service;
   }
}
