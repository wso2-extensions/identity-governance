package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.GetRecaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.GetRecaptchaApiServiceImpl;

public class GetRecaptchaApiServiceFactory {

   private final static GetRecaptchaApiService service = new GetRecaptchaApiServiceImpl();

   public static GetRecaptchaApiService getGetRecaptchaApi()
   {
      return service;
   }
}
