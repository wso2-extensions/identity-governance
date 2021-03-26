package org.wso2.carbon.identity.user.endpoint.factories;

import org.wso2.carbon.identity.user.endpoint.ResendCodeApiService;
import org.wso2.carbon.identity.user.endpoint.impl.ResendCodeApiServiceImpl;

public class ResendCodeApiServiceFactory {

   private final static ResendCodeApiService service = new ResendCodeApiServiceImpl();

   public static ResendCodeApiService getResendCodeApi()
   {
      return service;
   }
}
