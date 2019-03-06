package org.wso2.carbon.identity.claim.verification.endpoint.factories;

import org.wso2.carbon.identity.claim.verification.endpoint.ConfirmApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.ConfirmApiServiceImpl;

public class ConfirmApiServiceFactory {

   private final static ConfirmApiService service = new ConfirmApiServiceImpl();

   public static ConfirmApiService getConfirmApi()
   {
      return service;
   }
}
