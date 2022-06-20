package org.wso2.carbon.identity.user.endpoint.factories;

import org.wso2.carbon.identity.user.endpoint.ValidateCodeApiService;
import org.wso2.carbon.identity.user.endpoint.impl.ValidateCodeApiServiceImpl;

public class ValidateCodeApiServiceFactory {

   private final static ValidateCodeApiService service = new ValidateCodeApiServiceImpl();

   public static ValidateCodeApiService getValidateCodeApi()
   {
      return service;
   }
}
