package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.RecaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.RecaptchaApiServiceImpl;

public class RecaptchaApiServiceFactory {

   private final static RecaptchaApiService service = new RecaptchaApiServiceImpl();

   public static RecaptchaApiService getRecaptchaApi()
   {
      return service;
   }
}
