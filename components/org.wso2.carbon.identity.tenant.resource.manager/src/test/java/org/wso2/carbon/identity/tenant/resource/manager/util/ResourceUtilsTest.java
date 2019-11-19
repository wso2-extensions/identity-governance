/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.tenant.resource.manager.util;

import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resources;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;

@PrepareForTest({ TenantResourceManagerDataHolder.class, ConfigurationManager.class })
public class ResourceUtilsTest extends PowerMockTestCase {

    private static final String EMAIL_PUBLISHER = "EmailPublisher";

    @Mock
    TenantResourceManagerDataHolder tenantResourceManagerDataHolder;
    @Mock
    ConfigurationManager configurationManager;

    @DataProvider(name = "resourcesProvider")
    public Object[][] resourcesProvider() {

        Resources resources1 = new Resources();
        Resource emailPublisherResource1 = new Resource();
        emailPublisherResource1.setResourceName(EMAIL_PUBLISHER);
        List<ResourceFile> files1 = new ArrayList<>();
        emailPublisherResource1.setFiles(files1);
        List<Resource> resourcesArray1 = new ArrayList<>();
        resourcesArray1.add(emailPublisherResource1);
        resources1.setResources(resourcesArray1);

        Resources resources2 = new Resources();
        Resource emailPublisherResource2 = new Resource();
        emailPublisherResource2.setResourceName(EMAIL_PUBLISHER);
        List<ResourceFile> files2 = new ArrayList<>();
        ResourceFile resourceFile = new ResourceFile();
        resourceFile.setName(EMAIL_PUBLISHER);
        files2.add(resourceFile);
        emailPublisherResource2.setFiles(files2);
        List<Resource> resourcesArray2 = new ArrayList<>();
        resourcesArray2.add(emailPublisherResource2);
        resources2.setResources(resourcesArray2);

        Resources resources3 = new Resources();
        Resource emailPublisherResource3 = new Resource();
        emailPublisherResource3.setResourceName(EMAIL_PUBLISHER);
        List<ResourceFile> files3 = new ArrayList<>();
        ResourceFile resourceFile1 = new ResourceFile();
        resourceFile1.setName(EMAIL_PUBLISHER);
        files3.add(resourceFile);
        files3.add(resourceFile1);
        emailPublisherResource3.setFiles(files3);
        List<Resource> resourcesArray3 = new ArrayList<>();
        resourcesArray3.add(emailPublisherResource3);
        resources3.setResources(resourcesArray3);

        return new Object[][] {
                { resources1 }, { resources2 }, { resources3 }
        };
    }

    @Test(dataProvider = "resourcesProvider")
    public void testGetResourceManager(Object resources) throws ConfigurationManagementException {

        mockStatic(TenantResourceManagerDataHolder.class);
        mockStatic(ConfigurationManager.class);
        when(TenantResourceManagerDataHolder.getInstance()).thenReturn(tenantResourceManagerDataHolder);
        when(tenantResourceManagerDataHolder.getConfigurationManager()).thenReturn(configurationManager);
        when(configurationManager.getResourcesByType(anyString())).thenReturn((Resources) resources);
        when(configurationManager.getFiles(anyString(), anyString()))
                .thenReturn(((Resources) resources).getResources().get(0).getFiles());

        if (((Resources) resources).getResources().get(0).getFiles().size() == 1) {
            assertNotNull(ResourceUtils.getResourceFile(EMAIL_PUBLISHER),
                    "If number of files are only one then should return the file.");
        } else if (((Resources) resources).getResources().get(0).getFiles().size() > 1) {
            assertNull(ResourceUtils.getResourceFile(EMAIL_PUBLISHER),
                    "If number of files more than one then should return null.");
        } else if (((Resources) resources).getResources().get(0).getFiles().size() == 0) {
            assertNull(ResourceUtils.getResourceFile(EMAIL_PUBLISHER),
                    "If number of files are zero then should return null.");
        }
    }
}
