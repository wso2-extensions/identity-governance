package org.wso2.carbon.identity.captcha.strategy;

import org.wso2.carbon.identity.captcha.connector.CaptchaProvider;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

public class DefaultStrategyConnector extends AbstractStrategyConnector{
    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public CaptchaProvider getCaptchaProvider(ServletRequest servletRequest, ServletResponse servletResponse) {
        return null;
    }
}
