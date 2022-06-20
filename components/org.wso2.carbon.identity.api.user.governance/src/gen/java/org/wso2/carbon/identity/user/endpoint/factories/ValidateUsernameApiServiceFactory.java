package org.wso2.carbon.identity.user.endpoint.factories;

import org.wso2.carbon.identity.user.endpoint.ValidateUsernameApiService;
import org.wso2.carbon.identity.user.endpoint.impl.ValidateUsernameApiServiceImpl;

public class ValidateUsernameApiServiceFactory {

   private final static ValidateUsernameApiService service = new ValidateUsernameApiServiceImpl();

   public static ValidateUsernameApiService getValidateUsernameApi()
   {
      return service;
   }
}
