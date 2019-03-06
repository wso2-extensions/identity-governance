package org.wso2.carbon.identity.claim.verification.endpoint.factories;

import org.wso2.carbon.identity.claim.verification.endpoint.InitApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.InitApiServiceImpl;

public class InitApiServiceFactory {

   private final static InitApiService service = new InitApiServiceImpl();

   public static InitApiService getInitApi()
   {
      return service;
   }
}
