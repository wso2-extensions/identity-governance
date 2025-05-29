-- -----------------------------------------------------
-- Table IDN_IDENTITY_USER_DATA
-- -----------------------------------------------------
CREATE TABLE IDN_IDENTITY_USER_DATA (
            TENANT_ID INTEGER DEFAULT -1234,
            USER_NAME VARCHAR(255) NOT NULL,
            DATA_KEY VARCHAR(255) NOT NULL,
            DATA_VALUE VARCHAR(2048),
            PRIMARY KEY (TENANT_ID, USER_NAME, DATA_KEY)
);

INSERT INTO IDN_IDENTITY_USER_DATA (TENANT_ID, USER_NAME, DATA_KEY, DATA_VALUE) VALUES
(3, 'DEFAULT/sampleUser1@xmail.com', 'http://wso2.org/claims/identity/lastLogonTime', '1672704000000'),
(3, 'DEFAULT/sampleUser2@xmail.com', 'http://wso2.org/claims/identity/lastLogonTime', '1673481600000'),
(3, 'DEFAULT/sampleUser3@xmail.com', 'http://wso2.org/claims/identity/lastLogonTime', '1674000000000'),
(3, 'DEFAULT/sampleUser4@xmail.com', 'http://wso2.org/claims/identity/lastLogonTime', '1674518400000'),
(3, 'DEFAULT/sampleUser5@xmail.com', 'http://wso2.org/claims/identity/lastLogonTime', '1674950400000'),
(3, 'DEFAULT/sampleUser1@xmail.com', 'http://wso2.org/claims/identity/accountState', 'DISABLED'),
(3, 'DEFAULT/sampleUser3@xmail.com', 'http://wso2.org/claims/identity/accountState', 'DISABLED'),
(3, 'DEFAULT/sampleUser5@xmail.com', 'http://wso2.org/claims/identity/accountState', 'DISABLED'),
(3, 'DEFAULT/sampleUser5@xmail.com', 'http://wso2.org/claims/identity/emailVerified', 'true');
