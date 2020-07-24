package org.wso2.carbon.identity.user.endpoint.factories;

import org.wso2.carbon.identity.user.endpoint.LiteApiService;
import org.wso2.carbon.identity.user.endpoint.impl.LiteApiServiceImpl;

public class LiteApiServiceFactory {

   private final static LiteApiService service = new LiteApiServiceImpl();

   public static LiteApiService getLiteApi()
   {
      return service;
   }
}
