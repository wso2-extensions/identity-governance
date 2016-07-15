package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.ClaimsApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.ClaimsApiServiceImpl;

public class ClaimsApiServiceFactory {

   private final static ClaimsApiService service = new ClaimsApiServiceImpl();

   public static ClaimsApiService getClaimsApi()
   {
      return service;
   }
}
