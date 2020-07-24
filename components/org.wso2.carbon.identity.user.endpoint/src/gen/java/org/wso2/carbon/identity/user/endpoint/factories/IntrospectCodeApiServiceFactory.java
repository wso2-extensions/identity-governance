package org.wso2.carbon.identity.user.endpoint.factories;

import org.wso2.carbon.identity.user.endpoint.IntrospectCodeApiService;
import org.wso2.carbon.identity.user.endpoint.impl.IntrospectCodeApiServiceImpl;

public class IntrospectCodeApiServiceFactory {

   private final static IntrospectCodeApiService service = new IntrospectCodeApiServiceImpl();

   public static IntrospectCodeApiService getIntrospectCodeApi()
   {
      return service;
   }
}
