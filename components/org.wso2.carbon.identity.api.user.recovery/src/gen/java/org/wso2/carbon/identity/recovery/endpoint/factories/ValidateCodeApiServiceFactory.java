package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.ValidateCodeApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.ValidateCodeApiServiceImpl;

public class ValidateCodeApiServiceFactory {

   private final static ValidateCodeApiService service = new ValidateCodeApiServiceImpl();

   public static ValidateCodeApiService getValidateCodeApi()
   {
      return service;
   }
}
