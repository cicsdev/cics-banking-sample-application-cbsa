package com.ibm.cics.cip.cbsa.galasa.tests.install;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Map;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaWebInterface;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaCustomer;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaWebInterface;

import dev.galasa.After;
import dev.galasa.Test;

@Test
public class TestWebui {

    @CbsaWebInterface
	public ICbsaWebInterface webui;

    private String newCustomerNumber = "";
    private String newAccountNumber = "";

    private String customerCreationSuccessMessage = "Customer created successfully";
    private String customerCreationErrorMessage = "Customer creation unsuccessful";
    private String alertMessage = "Date of Birth is not 10 characters. dd-mm-yyyy please";

	private String[] customerData = ICbsaCustomer.generateRandomCustomerData();
    private String dateOfBirth = customerData[7].replace("/", "-");

    @After
    public void returnToHomePage() throws Exception {
        webui.returnHome();
    }

    @Test
    public void createCustomer() throws Exception {
        Map<String, String> result = webui.inputCreateCustomerDobTestData(dateOfBirth,false);

        newCustomerNumber = result.get("customerNumber");
        String messageText = result.get("messageText");

        assertThat(newCustomerNumber).isNotNull();
        assertThat(messageText).contains(customerCreationSuccessMessage);
    }

    @Test
    public void createCustomerInvalidDOB() throws Exception {
        // > 10 characters
        Map<String, String> result1 = webui.inputCreateCustomerDobTestData("25/120/2000",true);
        assertThat(result1.get("alertText")).contains(alertMessage);

        // < 10 characters
        Map<String, String> result2 = webui.inputCreateCustomerDobTestData("1/1/2000",true);
        assertThat(result2.get("alertText")).contains(alertMessage);

        // Incorrect Leap Year
        Map<String, String> result3 = webui.inputCreateCustomerDobTestData("29/02/2019",false);
        assertThat(result3.get("messageText")).contains(customerCreationErrorMessage);

        // Invalid Month
        Map<String, String> result4 = webui.inputCreateCustomerDobTestData("13/13/2013",false);
        assertThat(result4.get("messageText")).contains(customerCreationErrorMessage);

        // Invalid Day
        Map<String, String> result5 = webui.inputCreateCustomerDobTestData("50/05/2015",false);
        assertThat(result5.get("messageText")).contains(customerCreationErrorMessage);

        // Customer Too Old, Age = 151 years old
        String oldDob = webui.getBirthdateFromAge(151);
        Map<String, String> result6 = webui.inputCreateCustomerDobTestData(oldDob,false);
        assertThat(result6.get("messageText")).contains(customerCreationErrorMessage);
    }

    @Test
    public void createCustomerLeapYearDOB() throws Exception {
        // Correct Leap Year
        Map<String, String> result = webui.inputCreateCustomerDobTestData("29/02/2020",false);
        assertThat(result.get("messageText")).contains(customerCreationSuccessMessage);

        String leapYearCustomerNumber = result.get("customerNumber");

        if (leapYearCustomerNumber != null) {
            String successBoxXpath = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[3]/div/div[1]/h3";
            String deleteResult = webui.deleteCustomer(leapYearCustomerNumber, successBoxXpath);
            String successMessage = "Customer deleted successfully";
            assertThat(deleteResult).contains(successMessage);
        }
    }

    @Test
    public void createAccount() throws Exception {
        Map<String, String> result = webui.createAccountForCustomer(newCustomerNumber);
        assertThat(result.get("messageText")).contains("Customer account created successfully");
        assertThat(result.get("accountNumber")).isNotNull();

        newAccountNumber = result.get("accountNumber");
    }

    @Test
    public void createInvalidAccount() throws Exception {
        String invalidCustomerNumber = "49234092";
        String errorMessage = "Customer account failed to create";

        Map<String, String> result = webui.createAccountForCustomer(invalidCustomerNumber);
        assertThat(result.get("messageText")).contains(errorMessage);
        assertThat(result.containsKey("accountNumber")).isFalse();
    }

    @Test
    public void updateCustomer() throws Exception {
        Map<String, String> result = webui.updateCustomer(newCustomerNumber);
        assertThat(result.get("displayedName")).isEqualTo(result.get("updatedName"));
        assertThat(result.get("displayedAddress")).isEqualTo(result.get("updatedAddress"));
    }

    @Test
    public void updateInvalidCustomer() throws Exception {
        String invalidCustomerNumber = "89824923";
        String errorMessage = "No customers found!";

        Map<String, String> result = webui.updateCustomer(invalidCustomerNumber);
        assertThat(result.get("errorMessage")).contains(errorMessage);
    }

    @Test
    public void updateAccount() throws Exception {
        Map<String, String> result = webui.updateAccount(newAccountNumber);

        assertThat(result.get("displayedInterestRate")).isEqualTo(result.get("updatedInterestRate"));
        assertThat(result.get("displayedOverdraftLimit")).isEqualTo(result.get("updatedOverdraftLimit"));
        assertThat(result.get("accountType")).isEqualTo("SAVING");
    }

    @Test
    public void updateBadAccount() throws Exception {
        String invalidAccountNumber = "2423094832";

        Map<String, String> result = webui.updateAccount(invalidAccountNumber);
        assertThat(result.get("errorMessage")).contains("No accounts found!");
    }

    @Test
    public void deleteCustomerWithExistingAccounts() throws Exception {
        String errorBoxXpath = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[2]/div/div[2]/div";
        String errorMessage = "Please delete all associated accounts before deleting the customer";

        String result = webui.deleteCustomer(newCustomerNumber, errorBoxXpath);
        assertThat(result).contains(errorMessage);
    }

    @Test
    public void deleteExistingAccountForCustomer() throws Exception {
        String resultMessage = webui.deleteAccount(newAccountNumber);
        assertThat(resultMessage).contains("Account deleted successfully");
    }

    @Test
    public void deleteCustomerWithNoAccounts() throws Exception {
        String successBoxXpath = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[3]/div/div[1]/h3";
        String successMessage = "Customer deleted successfully";

        String deletionResult = webui.deleteCustomer(newCustomerNumber, successBoxXpath);
        assertThat(deletionResult).contains(successMessage);
    }

    @Test
    public void deleteInvalidCustomer() throws Exception {
        String invalidCustomerNumber = "42490";
        String errorBoxXpath = "/html/body/div/main/div/div[3]/div/div[1]/h3";
        String errorMessage = "No customers found!";

        String result = webui.deleteCustomer(invalidCustomerNumber, errorBoxXpath);
        assertThat(result).contains(errorMessage);
    }
}
