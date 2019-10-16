package org.wso2.carbon.identity.tenant.resource.manager.core;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.event.publisher.core.EventPublisherService;
import org.wso2.carbon.event.publisher.core.config.EventPublisherConfiguration;
import org.wso2.carbon.event.publisher.core.config.EventPublisherConfigurationFile;
import org.wso2.carbon.event.publisher.core.exception.EventPublisherConfigurationException;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementException;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;

import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_DEPLOYING_EVENT_PUBLISHER_CONFIGURATION;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_FILE;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_HANDLING_INPUT_STREAM;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.getResourceFile;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.handleClientException;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.handleServerException;

/**
 * Tenant Resource manager service implementation.
 */
public class ResourceManagerImpl implements ResourceManager {

    private static Log log = LogFactory.getLog(ResourceManagerImpl.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public void addEventPublisherConfiguration(String eventPublisherName) throws TenantResourceManagementException {

        List<ResourceFile> fileList;

        try {
            fileList = TenantResourceManagerDataHolder.getInstance().getConfigurationManager()
                    .getFiles(TenantResourceConstants.PUBLISHER, eventPublisherName);
        } catch (ConfigurationManagementException e) {
            throw handleServerException(ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_FILE, eventPublisherName, e);
        }
        try {
            if (getResourceFile(eventPublisherName) != null) {
                try {
                    deployEventPublisherConfiguration(
                            convert(TenantResourceManagerDataHolder.getInstance().getConfigurationManager()
                                    .getFileById(fileList.get(0).getId())));
                } catch (IOException e) {
                    throw handleClientException(ERROR_CODE_ERROR_WHEN_HANDLING_INPUT_STREAM, eventPublisherName, e);
                } catch (EventPublisherConfigurationException e) {
                    throw handleServerException(ERROR_CODE_ERROR_WHEN_DEPLOYING_EVENT_PUBLISHER_CONFIGURATION,
                            eventPublisherName, e);
                }
            }
        } catch (ConfigurationManagementException e) {
            throw handleServerException(ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_FILE, eventPublisherName, e);
        }

    }

    /**
     * This is used to deploy an event publisher configuration using.
     *
     * @param publisherConfigXML Event Publisher Configuration in xml.
     * @throws EventPublisherConfigurationException Event Publisher Configuration Exception.
     */
    private void deployEventPublisherConfiguration(String publisherConfigXML)
            throws EventPublisherConfigurationException {

        EventPublisherService carbonEventPublisherService = TenantResourceManagerDataHolder.getInstance()
                .getCarbonEventPublisherService();
        EventPublisherConfiguration eventPublisherConfiguration;

        eventPublisherConfiguration = carbonEventPublisherService.getEventPublisherConfiguration(publisherConfigXML);

        if (TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                .getActiveEventPublisherConfiguration(eventPublisherConfiguration.getEventPublisherName()) != null) {
            destroyEventPublisherConfiguration(eventPublisherConfiguration);
        }
        carbonEventPublisherService.addEventPublisherConfiguration(eventPublisherConfiguration);

    }

    /**
     * This is used to destroy an existing EventPublisher.
     *
     * @param eventPublisherConfiguration Event Publisher Configuration.
     * @throws ConfigurationManagementException Configuration Management Exception.
     */
    private void destroyEventPublisherConfiguration(EventPublisherConfiguration eventPublisherConfiguration)
            throws EventPublisherConfigurationException {

        int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
        EventPublisherConfigurationFile eventPublisherConfigurationFile = new EventPublisherConfigurationFile();
        eventPublisherConfigurationFile.setTenantId(tenantId);
        eventPublisherConfigurationFile.setEventPublisherName(eventPublisherConfiguration.getEventPublisherName());
        eventPublisherConfigurationFile.setFileName(eventPublisherConfiguration.getEventPublisherName());
        eventPublisherConfigurationFile.setStatus(EventPublisherConfigurationFile.Status.DEPLOYED);

        TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                .addEventPublisherConfigurationFile(eventPublisherConfigurationFile, tenantId);
        TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                .removeEventPublisherConfigurationFile(eventPublisherConfiguration.getEventPublisherName(), tenantId);

    }

    /**
     * This is used to convert input stream to a string.
     *
     * @param inputStream Event Publisher Configuration in as a input stream.
     * @throws IOException
     */
    private String convert(InputStream inputStream) throws IOException {

        StringBuilder stringBuilder = new StringBuilder();
        String line;

        try (BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream))) {
            while ((line = bufferedReader.readLine()) != null) {
                stringBuilder.append(line);
            }
        }

        return stringBuilder.toString();
    }

}
