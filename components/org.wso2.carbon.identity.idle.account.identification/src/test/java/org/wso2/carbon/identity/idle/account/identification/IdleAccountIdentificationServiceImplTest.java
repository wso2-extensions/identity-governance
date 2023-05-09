package org.wso2.carbon.identity.idle.account.identification;

import java.nio.file.Paths;
import java.sql.Connection;
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

    @Mock
    private IdleAccIdentificationDAOImpl idleAccIdentificationDAO;

    @BeforeMethod
    public void setUp() throws Exception {

        initiateH2Base();
        String carbonHome = Paths.get(System.getProperty("user.dir"), "target", "test-classes").toString();
        System.setProperty(CarbonBaseConstants.CARBON_HOME, carbonHome);
        System.setProperty(CarbonBaseConstants.CARBON_CONFIG_DIR_PATH, Paths.get(carbonHome, "conf").toString());

        DataSource dataSource = mock(DataSource.class);
        mockStatic(IdentityDatabaseUtil.class);
        when(IdentityDatabaseUtil.getDataSource()).thenAnswer((Answer<DataSource>) invocation -> dataSource);

        mockStatic(IdentityTenantUtil.class);
        when(IdentityTenantUtil.getTenantId(anyString())).thenAnswer((Answer<Integer>) invocation -> TENANT_ID);

        connection = TestUtils.getConnection();
        when(IdentityDatabaseUtil.getDBConnection(anyBoolean())).thenAnswer((Answer<Connection>) invocation -> connection);
        // Connection spyConnection = TestUtils.spyConnection(connection);
        // when(dataSource.getConnection()).thenReturn(spyConnection);

        idleAccountIdentificationService = new IdleAccountIdentificationServiceImpl();

        mock(IdleAccIdentificationDAOImpl.class);
        when(idleAccIdentificationDAO.fetchUserEmail(anyString())).thenReturn("sampleMail");

//        IdleAccIdentificationDAOImpl idleAccIdentificationDAO = new IdleAccIdentificationDAOImpl();
//        IdleAccIdentificationDAOImpl idleAccIdentificationDAOSpy = spy(idleAccIdentificationDAO);
//        when(idleAccIdentificationDAOSpy.fetchUserEmail(anyString())).thenReturn("sampleMail");
        //doReturn("sampleMail").when(idleAccIdentificationDAOSpy).fetchUserEmail(anyString());

        String apple = idleAccIdentificationDAO.fetchUserEmail("apple");
        //String apple = "apple";
    }

    @AfterMethod
    public void tearDown() throws Exception {

        connection.close();
        closeH2Base();
    }

    @DataProvider
    public Object[][] getInvalidDates() {

        return new Object[][]{
                {null, null},
                {"2023/01/31", null},
                {"2023-01-31", "2023/01/15"},
                {"2023-13-32", null},
                {"2023-01-31", "2023/100/150"},
        };
    }

    // @Test(dataProvider = "getInvalidDates")
    public void testGetInactiveUsersWithInvalidDates(String inactiveAfter, String excludeBefore) throws Exception {

        assertThrows(IdleAccIdentificationClientException.class, () ->
                idleAccountIdentificationService.getInactiveUsers(inactiveAfter, excludeBefore, TENANT_DOMAIN));
    }

    @DataProvider
    public Object[][] getDates() {

        return new Object[][]{
                {"2023-01-31", null, 5},
                {"2023-01-31", "2023-01-15", 3},
                {"2023-01-01", null, 0}
        };
    }

    @Test(dataProvider = "getDates")
    public void testGetInactiveUsers(String inactiveAfter, String excludeBefore, int expected) throws Exception {

        mock(IdleAccIdentificationDAOImpl.class);
        when(idleAccIdentificationDAO.fetchUserEmail(anyString())).thenReturn("sampleMail");

        List<InactiveUserModel> inactiveUsers = idleAccountIdentificationService.
                getInactiveUsers(inactiveAfter, excludeBefore, TENANT_DOMAIN);

        assertEquals(inactiveUsers.size(), expected);
    }
}
