package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.RecoverPasswordApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.RecoverPasswordApiServiceImpl;

public class RecoverPasswordApiServiceFactory {

   private final static RecoverPasswordApiService service = new RecoverPasswordApiServiceImpl();

   public static RecoverPasswordApiService getRecoverPasswordApi()
   {
      return service;
   }
}
