package org.wso2.carbon.identity.idle.account.identification;

import java.nio.file.Paths;
import java.sql.Connection;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import javax.sql.DataSource;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.stubbing.Answer;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.idle.account.identification.dao.IdleAccIdentificationDAO;
import org.wso2.carbon.identity.idle.account.identification.dao.impl.IdleAccIdentificationDAOImpl;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationClientException;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;
import org.wso2.carbon.identity.idle.account.identification.services.IdleAccountIdentificationService;
import org.wso2.carbon.identity.idle.account.identification.services.IdleAccountIdentificationServiceImpl;
import org.wso2.carbon.identity.idle.account.identification.util.TestUtils;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.powermock.api.mockito.PowerMockito.doReturn;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.spy;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.wso2.carbon.base.MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
import static org.wso2.carbon.base.MultitenantConstants.SUPER_TENANT_ID;
import static org.wso2.carbon.identity.idle.account.identification.util.TestUtils.closeH2Base;
import static org.wso2.carbon.identity.idle.account.identification.util.TestUtils.initiateH2Base;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertThrows;

@PrepareForTest({IdentityDatabaseUtil.class, IdentityUtil.class, IdentityTenantUtil.class,
    IdleAccIdentificationDAO.class, IdleAccIdentificationDAOImpl.class})
public class IdleAccountIdentificationServiceImplTest extends PowerMockTestCase {

    private IdleAccountIdentificationService idleAccountIdentificationService;
    private Connection connection;

    private static final String TENANT_DOMAIN = "DEFAULT";
    private static final int TENANT_ID = 3;
    private static final String SAMPLE_EMAIL = "sampleMail";;

    @BeforeMethod
    public void setUp() throws Exception {

        initiateH2Base();
        String carbonHome = Paths.get(System.getProperty("user.dir"), "target", "test-classes").toString();
        System.setProperty(CarbonBaseConstants.CARBON_HOME, carbonHome);
        System.setProperty(CarbonBaseConstants.CARBON_CONFIG_DIR_PATH, Paths.get(carbonHome, "conf").toString());

        mockStatic(IdentityDatabaseUtil.class);
        connection = TestUtils.getConnection();
        when(IdentityDatabaseUtil.getDBConnection(anyBoolean())).thenAnswer((Answer<Connection>) invocation -> connection);

        mockStatic(IdentityTenantUtil.class);
        when(IdentityTenantUtil.getTenantId(anyString())).thenAnswer((Answer<Integer>) invocation -> TENANT_ID);

        idleAccountIdentificationService = new IdleAccountIdentificationServiceImpl(new IdleAccIdentificationDAOImpl());
    }

    @AfterMethod
    public void tearDown() throws Exception {

        connection.close();
        closeH2Base();
    }

    @DataProvider
    public Object[][] getDates1() {

        return new Object[][]{
                {LocalDate.parse("2023-01-31").atStartOfDay(), 5},
                {LocalDate.parse("2023-01-01").atStartOfDay(), 0}
        };
    }

    @Test(dataProvider = "getDates1")
    public void getInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, int expected) throws Exception {

        IdleAccIdentificationDAOImpl idleAccIdentificationDAO = spy(new IdleAccIdentificationDAOImpl());
        doReturn(SAMPLE_EMAIL).when(idleAccIdentificationDAO).fetchUserEmail(anyString());

        IdleAccountIdentificationService idleAccountIdentificationService =
                new IdleAccountIdentificationServiceImpl(idleAccIdentificationDAO);

        List<InactiveUserModel> inactiveUsers = idleAccountIdentificationService.
                getInactiveUsersFromSpecificDate(inactiveAfter, TENANT_DOMAIN);

        assertEquals(inactiveUsers.size(), expected);
    }

    @DataProvider
    public Object[][] getDates2() {

        return new Object[][]{
                {LocalDate.parse("2023-01-31").atStartOfDay(), LocalDate.parse("2023-01-15").atStartOfDay(), 3},
                {LocalDate.parse("2023-01-31").atStartOfDay(), LocalDate.parse("2023-01-30").atStartOfDay(), 0}
        };
    }

    @Test(dataProvider = "getDates2")
    public void getLimitedInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, LocalDateTime excludeBefore,
                                                        int expected) throws Exception {

        IdleAccIdentificationDAOImpl idleAccIdentificationDAO = spy(new IdleAccIdentificationDAOImpl());
        doReturn(SAMPLE_EMAIL).when(idleAccIdentificationDAO).fetchUserEmail(anyString());

        IdleAccountIdentificationService idleAccountIdentificationService =
                new IdleAccountIdentificationServiceImpl(idleAccIdentificationDAO);

        List<InactiveUserModel> inactiveUsers = idleAccountIdentificationService.
                getLimitedInactiveUsersFromSpecificDate(inactiveAfter, excludeBefore, TENANT_DOMAIN);

        assertEquals(inactiveUsers.size(), expected);
    }
}
