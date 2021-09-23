package org.wso2.carbon.identity.captcha.strategy;

import org.wso2.carbon.identity.captcha.connector.CaptchaProvider;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

public interface StrategyConnector {

    void init(IdentityGovernanceService identityGovernanceService);

    int getPriority();

    CaptchaProvider getCaptchaProvider(ServletRequest servletRequest, ServletResponse servletResponse);

}
