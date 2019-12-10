package org.wso2.carbon.identity.user.endpoint.factories;

import org.wso2.carbon.identity.user.endpoint.UpdateUsernameApiService;
import org.wso2.carbon.identity.user.endpoint.impl.UpdateUsernameApiServiceImpl;

public class UpdateUsernameApiServiceFactory {

   private final static UpdateUsernameApiService service = new UpdateUsernameApiServiceImpl();

   public static UpdateUsernameApiService getUpdateUsernameApi()
   {
      return service;
   }
}
