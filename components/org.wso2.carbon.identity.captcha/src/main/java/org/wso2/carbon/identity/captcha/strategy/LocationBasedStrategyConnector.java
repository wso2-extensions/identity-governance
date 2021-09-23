package org.wso2.carbon.identity.captcha.strategy;

import org.wso2.carbon.identity.captcha.connector.CaptchaProvider;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

public class LocationBasedStrategyConnector extends AbstractStrategyConnector {

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

    }

    @Override
    public int getPriority() {

        return 10;
    }

    @Override
    public CaptchaProvider getCaptchaProvider(ServletRequest servletRequest, ServletResponse servletResponse) {
        //if client
        return null;
    }

    public static String getClientIpAddress(HttpServletRequest request) {

        String ip = request.getHeader("X-Forwarded-For");
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_CLIENT_IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_X_FORWARDED_FOR");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        return ip;
    }

}
