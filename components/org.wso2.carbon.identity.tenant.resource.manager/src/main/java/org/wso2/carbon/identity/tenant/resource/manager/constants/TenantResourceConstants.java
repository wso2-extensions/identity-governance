package org.wso2.carbon.identity.tenant.resource.manager.constants;

public class TenantResourceConstants {

    private TenantResourceConstants() {
    }

    public static final String PUBLISHER = "Publisher";
    public static final String FOR_TENANT_ID = ". for tenant id: ";

    public enum ErrorMessages {

        ERROR_CODE_ERROR_WHEN_HANDLING_INPUT_STREAM("TRM-10001", "Error occurred when handling the file input stream "
                + "of file with name: %s."),
        ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_FILE("TRM-10002", "Error occurred when fetching the "
                + "event publisher configuration file with name: %s."),
        ERROR_CODE_ERROR_WHEN_DEPLOYING_EVENT_PUBLISHER_CONFIGURATION("TRM-10004", "Error occurred when deploying the "
                + "event publisher configuration for with name: %s."),
        ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_PUBLISHER_CONFIGURATION("TRM-10005", "Error occurred while "
                + "loading super tenant event publisher configurations for the tenant with ID: %s."),
        ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_STREAM_CONFIGURATION("TRM-10006", "Error occurred while "
                + "loading super tenant event stream configurations for the tenant with ID: %s."),
        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_STREAM_CONFIGURATION("TRM-10007", "Error occurred while "
                + "creating tenant event stream configuration: %s."),
        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_BY_CONFIG_STORE("TRM-10008", "Error occurred while "
                + "creating tenant event publisher configuration: %s. . Using configuration store"),
        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_USING_SUPER_TENANT_CONFIG(
                "TRM-10009", "Error occurred while creating tenant event publisher configuration: %s. . Using super "
                + "tenant configuration");


        private final String code;
        private final String message;

        ErrorMessages(String code, String message) {
            this.code = code;
            this.message = message;
        }

        public String getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

        @Override
        public String toString() {
            return code + " - " + message;
        }
    }
}
