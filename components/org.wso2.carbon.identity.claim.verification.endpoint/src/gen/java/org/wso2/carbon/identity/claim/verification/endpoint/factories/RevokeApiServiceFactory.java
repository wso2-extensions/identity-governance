package org.wso2.carbon.identity.claim.verification.endpoint.factories;

import org.wso2.carbon.identity.claim.verification.endpoint.RevokeApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.RevokeApiServiceImpl;

public class RevokeApiServiceFactory {

   private final static RevokeApiService service = new RevokeApiServiceImpl();

   public static RevokeApiService getRevokeApi()
   {
      return service;
   }
}
