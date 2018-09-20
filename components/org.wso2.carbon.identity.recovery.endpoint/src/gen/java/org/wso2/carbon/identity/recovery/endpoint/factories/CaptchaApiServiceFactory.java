package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.CaptchaApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.CaptchaApiServiceImpl;

public class CaptchaApiServiceFactory {

   private final static CaptchaApiService service = new CaptchaApiServiceImpl();

   public static CaptchaApiService getCaptchaApi()
   {
      return service;
   }
}
