package org.wso2.carbon.identity.claim.verification.endpoint.factories;

import org.wso2.carbon.identity.claim.verification.endpoint.ValidateApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.ValidateApiServiceImpl;

public class ValidateApiServiceFactory {

   private final static ValidateApiService service = new ValidateApiServiceImpl();

   public static ValidateApiService getValidateApi()
   {
      return service;
   }
}
