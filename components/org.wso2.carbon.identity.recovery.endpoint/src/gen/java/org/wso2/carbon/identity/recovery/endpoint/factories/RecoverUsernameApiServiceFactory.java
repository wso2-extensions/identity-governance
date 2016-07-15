package org.wso2.carbon.identity.recovery.endpoint.factories;

import org.wso2.carbon.identity.recovery.endpoint.RecoverUsernameApiService;
import org.wso2.carbon.identity.recovery.endpoint.impl.RecoverUsernameApiServiceImpl;

public class RecoverUsernameApiServiceFactory {

   private final static RecoverUsernameApiService service = new RecoverUsernameApiServiceImpl();

   public static RecoverUsernameApiService getRecoverUsernameApi()
   {
      return service;
   }
}
