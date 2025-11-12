package org.wso2.carbon.identity.recovery.endpoint;

public final class Constants {

    public static final String AUTHORIZATION_HEADER = "Authorization";
    public static final String SUCCESS = "SUCCESS";
    public static final String INVALID = "INVALID";
    public static final String FORBIDDEN = "FORBIDDEN";
    public static final String FAILED = "FAILED";
    public static final String SERVER_ERROR = "Error occurred in the server while performing the task.";
    public static final String APPLICATION_JSON = "application/json";
    public static final String DEFAULT_RESPONSE_CONTENT_TYPE = APPLICATION_JSON;
    public static final String HEADER_CONTENT_TYPE = "Content-Type";
    public static final String INCLUDE_DISPLAY_ORDER_CONFIG_ELEMENT =
            "Recovery.V09Api.IncludeDisplayOrderPropertyInClaims";



    //default error messages
    public static final String STATUS_FORBIDDEN_MESSAGE_DEFAULT = "Forbidden";
    public static final String STATUS_NOT_FOUND_MESSAGE_DEFAULT = "Not Found";
    public static final String STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT = "Internal server error";
    public static final String STATUS_METHOD_NOT_ALLOWED_MESSAGE_DEFAULT = "Method Not Allowed";
    public static final String STATUS_BAD_REQUEST_MESSAGE_DEFAULT = "Bad Request";
    public static final String STATUS_CONFLICT_MESSAGE_RESOURCE_ALREADY_EXISTS = "Resource Already Exists";
    public static final String STATUS_CONFLICT_MESSAGE_DEFAULT = "Conflict";
    public static final String TENANT_NAME_FROM_CONTEXT = "TenantNameFromContext";

    public static final String STATUS_INTERNAL_SERVER_ERROR_DESCRIPTION_DEFAULT = "The server encountered "
            + "an internal error. Please contact administrator.";

    public static final String ERROR_CODE_NO_USER_FOUND_FOR_RECOVERY = "20015";
    public static final String ERROR_CODE_MULTIPLE_USERS_MATCHING = "20015";

    // Recovery type
    public static final String USERNAME_RECOVERY = "username-recovery";
    public static final String PASSWORD_RECOVERY = "password-recovery";
    public static final String SELF_REGISTRATION = "self-registration";

    public static final String ERROR_MESSAGE_EMAIL_NOT_FOUND =
            "Email notification sending failed. Sending email address is not configured for the user.";
    public static final String ERROR_CODE_EMAIL_NOT_FOUND = "20016";

}
