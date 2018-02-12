package org.wso2.carbon.identity.user.endpoint.factories;

import org.wso2.carbon.identity.user.endpoint.PiInfoApiService;
import org.wso2.carbon.identity.user.endpoint.impl.PiInfoApiServiceImpl;

public class PiInfoApiServiceFactory {

   private final static PiInfoApiService service = new PiInfoApiServiceImpl();

   public static PiInfoApiService getPiInfoApi()
   {
      return service;
   }
}
