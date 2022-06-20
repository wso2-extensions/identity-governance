package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.SetPasswordApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.SetPasswordApiServiceImpl;

public class SetPasswordApiServiceFactory {

   private final static SetPasswordApiService service = new SetPasswordApiServiceImpl();

   public static SetPasswordApiService getSetPasswordApi()
   {
      return service;
   }
}
