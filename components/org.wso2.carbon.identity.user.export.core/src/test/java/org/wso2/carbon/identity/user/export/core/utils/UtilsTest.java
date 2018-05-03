package org.wso2.carbon.identity.user.export.core.utils;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.consent.mgt.core.model.Address;
import org.wso2.carbon.consent.mgt.core.model.ConsentPurpose;
import org.wso2.carbon.consent.mgt.core.model.PIICategoryValidity;
import org.wso2.carbon.consent.mgt.core.model.PiiController;
import org.wso2.carbon.consent.mgt.core.model.Receipt;
import org.wso2.carbon.consent.mgt.core.model.ReceiptService;
import org.wso2.carbon.identity.user.export.core.dto.ConsentReceiptDTO;
import org.wso2.carbon.identity.user.export.core.dto.PiiControllerDTO;

import java.util.ArrayList;
import java.util.List;

public class UtilsTest {

    public static final String USERNAME_CLAIM_VALUE = "username1";
    public static final String CONSENT_RECEIPT_ID = "da290c35-2219-4832-8b47-29ca0e323423";
    public static final String RECEIPT_VERSION = "KI-CR-v1.1.0";
    public static final String RECEIPT_JURISDICTION = "global";
    public static final String RECEIPT_COLLECTION_METHOD = "api";
    public static final String RECEIPT_LANGUAGE = "en";
    public static final String RECEIPT_POLICY_URL = "https://sample.policy.url";
    public static final String RECEIPT_STATE = "ACTIVE";
    public static final long RECEIPT_CONSENT_TIMESTAMP = 1517447315404L;
    public static final String PII_CONTROLLER_CONTACT = "sample";
    public static final String PII_CONTROLLER_EMAIL = "sample@abc.com";
    public static final String PII_CONTROLLER_PHONE = "+01433444333";
    public static final String PII_CONTROLLER_URL = "https://sample.piicontroller.url";
    public static final String PII_CONTROLLER_NAME = "sample";
    public static final String ADDRESS_COUNTRY = "USA";
    public static final String ADDRESS_LOCALITY = "Mountain View";
    public static final String ADDRESS_REGION = "CA";
    public static final String ADDRESS_POSTAL_CODE = "94043";
    public static final String ADDRESS_OFFICE_BOX_NUMBER = "233";
    public static final String ADDRESS_STREET_ADDRESS = "1600 Amphitheatre Pkwy";
    public static final String SERVICE_TRAVELOCITY = "travelocity";
    public static final String CONSENT_TYPE = "explicit";
    public static final String CONSENT_PURPOSE = "test-purpose";
    public static final String CONSENT_TERMINATION = "days:50";
    public static final String PII_CATEGORY = "test-category";
    public static final String SPI_CATEGORY = "test-category";
    public static final String PII_CATEGORY_VALIDITY = "days:50";
    public static final String PURPOSE_CATEGORY = "test-purpose-category";
    public static int  PII_CATEGORY_ID = 1;

    @Test
    public void testGetConsentReceiptDTO() throws Exception {

        Receipt receipt = new Receipt();
        receipt.setConsentReceiptId(CONSENT_RECEIPT_ID);
        receipt.setVersion(RECEIPT_VERSION);
        receipt.setJurisdiction(RECEIPT_JURISDICTION);
        receipt.setCollectionMethod(RECEIPT_COLLECTION_METHOD);
        receipt.setLanguage(RECEIPT_LANGUAGE);
        receipt.setPiiPrincipalId(USERNAME_CLAIM_VALUE);
        receipt.setConsentTimestamp(1517447315404L);

        PiiController piiController = new PiiController(PII_CONTROLLER_NAME, false, PII_CONTROLLER_CONTACT,
                PII_CONTROLLER_EMAIL, PII_CONTROLLER_PHONE, PII_CONTROLLER_URL, new Address(ADDRESS_COUNTRY,
                ADDRESS_LOCALITY, ADDRESS_REGION, ADDRESS_OFFICE_BOX_NUMBER, ADDRESS_POSTAL_CODE,
                ADDRESS_STREET_ADDRESS));
        List<PiiController> piiControllers = new ArrayList<>();
        piiControllers.add(piiController);
        receipt.setPiiControllers(piiControllers);

        ReceiptService receiptService = new ReceiptService();
        receiptService.setService(SERVICE_TRAVELOCITY);
        receiptService.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        receiptService.setTenantId(MultitenantConstants.SUPER_TENANT_ID);
        receiptService.setReceiptToServiceId(1);

        ConsentPurpose consentPurpose = new ConsentPurpose();
        consentPurpose.setPurpose(CONSENT_PURPOSE);

        List<String> purposeCategories = new ArrayList<>();
        purposeCategories.add(PURPOSE_CATEGORY);
        consentPurpose.setPurposeCategory(purposeCategories);

        consentPurpose.setConsentType(CONSENT_TYPE);

        PIICategoryValidity piiCategory = new PIICategoryValidity(PII_CATEGORY_ID, PII_CATEGORY_VALIDITY);
        List<PIICategoryValidity> piiCategories = new ArrayList<>();
        piiCategories.add(piiCategory);
        consentPurpose.setPiiCategory(piiCategories);

        consentPurpose.setPrimaryPurpose(true);
        consentPurpose.setTermination(CONSENT_TERMINATION);
        consentPurpose.setThirdPartyDisclosure(false);
        consentPurpose.setServiceToPurposeId(1);

        List<ConsentPurpose> purposes = new ArrayList<>();
        purposes.add(consentPurpose);
        receiptService.setPurposes(purposes);

        List<ReceiptService> receiptServices = new ArrayList<>();
        receiptServices.add(receiptService);
        receipt.setServices(receiptServices);

        receipt.setPolicyUrl(RECEIPT_POLICY_URL);
        receipt.setSensitive(true);
        receipt.setState(RECEIPT_STATE);
        receipt.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        receipt.setTenantId(-1234);

        List<String> spiCategory = new ArrayList<>();
        spiCategory.add(SPI_CATEGORY);
        receipt.setSpiCat(spiCategory);

        ConsentReceiptDTO consentReceiptDTO = Utils.getConsentReceiptDTO(receipt);
        Assert.assertEquals(consentReceiptDTO.getConsentReceiptID(), CONSENT_RECEIPT_ID);
        Assert.assertEquals(consentReceiptDTO.getVersion(), RECEIPT_VERSION);
        Assert.assertEquals(consentReceiptDTO.getJurisdiction(), RECEIPT_JURISDICTION);
        Assert.assertEquals(consentReceiptDTO.getCollectionMethod(), RECEIPT_COLLECTION_METHOD);
        Assert.assertEquals(consentReceiptDTO.getLanguage(), RECEIPT_LANGUAGE);
        Assert.assertEquals(consentReceiptDTO.getPolicyUrl(), RECEIPT_POLICY_URL);
        Assert.assertEquals(consentReceiptDTO.getSensitive(), Boolean.TRUE);
        Assert.assertEquals(consentReceiptDTO.getState(), RECEIPT_STATE);
        Assert.assertEquals(consentReceiptDTO.getTenantDomain(), MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        Assert.assertEquals(consentReceiptDTO.getConsentTimestamp(), Long.valueOf(RECEIPT_CONSENT_TIMESTAMP));

        Assert.assertEquals(consentReceiptDTO.getSpiCat().size(), 1);
        Assert.assertEquals(consentReceiptDTO.getSpiCat().get(0), SPI_CATEGORY);

        List<PiiControllerDTO> piiControllersFromDTO = consentReceiptDTO.getPiiControllers();
        Assert.assertEquals(piiControllersFromDTO.size(), 1);
        Assert.assertEquals(piiControllersFromDTO.get(0).getContact(), PII_CONTROLLER_CONTACT);
        Assert.assertEquals(piiControllersFromDTO.get(0).getEmail(), PII_CONTROLLER_EMAIL);
        Assert.assertEquals(piiControllersFromDTO.get(0).getPhone(), PII_CONTROLLER_PHONE);
        Assert.assertEquals(piiControllersFromDTO.get(0).getPiiControllerUrl(), PII_CONTROLLER_URL);
        Assert.assertEquals(piiControllersFromDTO.get(0).getPiiController(), PII_CONTROLLER_NAME);
        Assert.assertEquals(piiControllersFromDTO.get(0).getOnBehalf(), Boolean.FALSE);

        Assert.assertEquals(piiControllersFromDTO.get(0).getAddress().getAddressCountry(), ADDRESS_COUNTRY);
        Assert.assertEquals(piiControllersFromDTO.get(0).getAddress().getAddressLocality(), ADDRESS_LOCALITY);
        Assert.assertEquals(piiControllersFromDTO.get(0).getAddress().getAddressRegion(), ADDRESS_REGION);
        Assert.assertEquals(piiControllersFromDTO.get(0).getAddress().getPostalCode(), ADDRESS_POSTAL_CODE);
        Assert.assertEquals(piiControllersFromDTO.get(0).getAddress().getPostOfficeBoxNumber(),
                ADDRESS_OFFICE_BOX_NUMBER);
        Assert.assertEquals(piiControllersFromDTO.get(0).getAddress().getStreetAddress(), ADDRESS_STREET_ADDRESS);

        Assert.assertEquals(consentReceiptDTO.getServices().size(), 1);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getService(), SERVICE_TRAVELOCITY);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getTenantDomain(), MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().size(), 1);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getConsentType(), CONSENT_TYPE);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getPurpose(), CONSENT_PURPOSE);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getTermination(), CONSENT_TERMINATION);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getThirdPartyName(), null);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getThirdPartyDisclosure(), Boolean.FALSE);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getPrimaryPurpose(), Boolean.TRUE);

        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getPiiCategory().size(), 1);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getPiiCategory().get(0)
                .getValidity(), PII_CATEGORY_VALIDITY);

        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getPurposeCategory().size(),
                1);
        Assert.assertEquals(consentReceiptDTO.getServices().get(0).getPurposes().get(0).getPurposeCategory().get(0),
                PURPOSE_CATEGORY);
    }
}
