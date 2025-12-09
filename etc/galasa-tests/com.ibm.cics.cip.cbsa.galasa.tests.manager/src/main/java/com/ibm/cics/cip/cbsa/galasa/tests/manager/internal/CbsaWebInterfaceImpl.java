package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal;

import java.nio.file.Path;
import java.time.Duration;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaCustomer;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaWebInterface;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties.CbsaWebInterfaceUrl;

import dev.galasa.selenium.IWebPage;
import dev.galasa.selenium.internal.SeleniumManagerImpl;
import dev.galasa.selenium.internal.WebPageImpl;
import org.openqa.selenium.TimeoutException;

import org.openqa.selenium.Alert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class CbsaWebInterfaceImpl extends WebPageImpl implements ICbsaWebInterface {

    private final static Log logger = LogFactory.getLog(CbsaTerminalImpl.class);

    // Global Variables Here
    private Random random = new Random(System.currentTimeMillis());
    private WebDriverWait wait = new WebDriverWait(this.getWebDriver(), Duration.ofSeconds(5));

    private String[] customerData = ICbsaCustomer.generateRandomCustomerData();
    private String fullName = customerData[1] + " " + customerData[2] + " " + customerData[3];
    private String address = customerData[4];
    private String city = customerData[8];

    private int overdraftLimit = returnRandomNumber();
    private int interestRate = returnRandomNumber();

    public CbsaWebInterfaceImpl(SeleniumManagerImpl selMan, WebDriver webDriver, List<IWebPage> webPages, Path screenshotRasDirectory) throws CbsaException {
        super(
            selMan,
            webDriver,
            webPages,
            screenshotRasDirectory
        );
	}

    public boolean isElementPresent(By locator) {
        try {
            wait.until(ExpectedConditions.visibilityOfElementLocated(locator));
            return true;
        } catch (TimeoutException e) {
            return false;
        }
    }

    public void returnHome() throws CbsaException {
        String url = CbsaWebInterfaceUrl.get();
        this.navigate().to(url);
    }

    public int returnRandomNumber(){
        return random.nextInt(200);
    }

    public String getBirthdateFromAge(int age) {
        // Get today's date
        LocalDate today = LocalDate.now();

        // Subtract the age in years from today's date
        LocalDate birthdate = today.minusYears(age);

        // Format the birthdate as dd/MM/yyyy
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        return birthdate.format(formatter);
    }

    @Override
    public String deleteCustomer(String customerNumber, String messageXpath) throws Exception {
        // Click on delete customer field
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[2]/div/div/div[1]/div/button[2]");
        // Enter customer number
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[1]/div/div/input", customerNumber);
        // Click submit
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[2]/button");

        // Check if the "Customer not found" popup appears immediately due to invalid customer number
        if (isElementPresent(By.xpath(messageXpath))) {
            String errorMessage = this.findElementByXpath(messageXpath).getText();
            return errorMessage;
        }

        // If valid, proceed with the deletion process & click delete button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/button");
        // Click confirmation of delete button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div/div[3]/button[2]");

        // Check for success/error message or return a message if not found
        if (isElementPresent(By.xpath(messageXpath))) {
            String resultMessage = this.findElementByXpath(messageXpath).getText();
            return resultMessage;
        }

        return "Deletion message not found (element not present)";
    }

    @Override
    public Map<String, String> inputCreateCustomerDobTestData(String dob, Boolean expectAlert) throws Exception {
        String customerNumber = null;
        String alertText = null;
        String messageText = null;

        logger.info("Creating Customer: " + fullName + ", " + dob + ", " + address + ", " + city);
        // Click Create Customer
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[2]/div/div/div[1]/div/button[1]");
        // Click on the 'Title' drop down menu
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[1]/div/div/div[1]/fieldset/div[1]/div[1]/div/button");
        // Select the title 'Mr'
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[1]/div/div/div[1]/fieldset/div[1]/div[1]/div/ul/li[1]/div");
        // Enter a full name
        this.sendKeysToElementByXpath("/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[1]/fieldset/div[1]/div[2]/div[2]/div/input", fullName);
        // Enter a date of birth
        this.sendKeysToElementByXpath("/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[1]/fieldset/div[1]/div[3]/div/div/div/span/input", dob);
        // Enter address line 1
        this.sendKeysToElementByXpath("/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[2]/fieldset/div[1]/div[1]/div[2]/div/input", address);
        // Enter a city
        this.sendKeysToElementByXpath("/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[2]/fieldset/div[1]/div[2]/div[2]/div/input", city);
        // Press submit button
        this.clickElementByXpath("/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[2]/fieldset/button");

        // If an alert is present it means that it has failed to create due to the length of input into the date of birth field (Webui functionality)
        // If not then it has failed for another reason, or passed
        if (expectAlert) {
            // Wait for the alert to be present if we expect it
            wait.until(ExpectedConditions.alertIsPresent());
            Alert alert = this.switchTo().alert();
            alertText = alert.getText();
            alert.accept();
        } else {
            // Skip the wait for alert and check success/error messages directly
            String successXpath = "/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[1]/fieldset/div[2]/div/div[2]/h5";
            String errorXpath = "/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[1]/fieldset/div[5]/div/div[1]/h3";

            try {
                // Check for success or error message directly
                if (isElementPresent(By.xpath(successXpath))) {
                    messageText = this.findElementByXpath(successXpath).getText();
                    customerNumber = this.findElementByXpath("/html/body/div[1]/main/div/div[2]/div[1]/div/div/div[1]/fieldset/div[2]/div/div[2]/div[2]").getText();
                } else if (isElementPresent(By.xpath(errorXpath))) {
                    messageText = this.findElementByXpath(errorXpath).getText();
                    customerNumber = null;
                }
            } catch (Exception ex) {
                messageText = "No visible success or error message found.";
                logger.info(messageText);
            }
        }

        // Return to the home page after processing
        returnHome();

        // Return the result map with the extracted data
        Map<String, String> result = new HashMap<>();
        result.put("customerNumber", customerNumber);
        result.put("alertText", alertText);
        result.put("messageText", messageText);
        return result;
    }

    @Override
    public Map<String, String> createAccountForCustomer(String customerNumber) throws Exception {
        // Click on the create account field
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[2]/div/div/div[1]/div/button[4]");

        // Enter the customer number
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div[1]/form/div/div[1]/div/div[2]/div/input", customerNumber);

        // Select the account type "CURRENT"
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[1]/form/div/div[2]/div/div/button");
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[1]/form/div/div[2]/div/div/ul/li[5]/div");

        // Enter overdraft limit and interest rate
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div[1]/form/div/div[3]/div/div[2]/div[1]/input", String.valueOf(overdraftLimit));
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div[1]/form/div/div[4]/div/div[2]/div[1]/input", String.valueOf(interestRate));

        // Click the submit button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[1]/form/div/button");

        String messageXpath = "/html/body/div/main/div/div[2]/div[1]/div[1]/div/div[2]";
        String accountNumberXpath = "/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div/div[2]/p[1]";
        String errorMessageXpath = "/html/body/div[1]/main/div/div[2]/div[1]/div[4]/div/div[2]/h5";
        String successMessage = "Customer account created successfully";

        Map<String, String> result = new HashMap<>();

        // Check for success message
        if (isElementPresent(By.xpath(messageXpath))) {
            String messageText = this.findElementByXpath(messageXpath).getText();
            result.put("messageText", messageText);

            if (messageText.contains(successMessage)) {
                // Retrieve the account number
                String accountNumber = this.findElementByXpath(accountNumberXpath).getText().replaceAll("[^0-9]", "");
                result.put("accountNumber", accountNumber);
            }
        }
        // Check for error message if success message is not found
        else if (isElementPresent(By.xpath(errorMessageXpath))) {
            String errorMessage = this.findElementByXpath(errorMessageXpath).getText();
            result.put("messageText", errorMessage);
        } else {
            // Set a default message if no success or error message found
            result.put("messageText", "No confirmation message found.");
        }

        return result;
    }


    @Override
    public Map<String, String> updateCustomer(String customerNumber) throws Exception {
        // Generate updated customer data
        String[] updatedCustomerData = ICbsaCustomer.generateRandomCustomerData();
        String updatedTitleAndName = updatedCustomerData[0] + " " + updatedCustomerData[1] + " " + updatedCustomerData[2] + " " + updatedCustomerData[3];
        String updatedAddressAndCity = updatedCustomerData[5] + ", " + updatedCustomerData[8];

        Map<String, String> result = new HashMap<>();

        // Click on update customer field
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[2]/div/div/div[1]/div/button[3]");

        // Enter the customer number
        this.clearElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[1]/div/div/input");
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[1]/div/div/input", customerNumber);

        // Click submit button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[1]/div[1]/div[3]/button");

        String errorBoxXpath = "/html/body/div/main/div/div[3]/div/div[1]/h3";
        // If invalid customer, return error message
        if (isElementPresent(By.xpath(errorBoxXpath))) {
            String errorMessage = this.findElementByXpath(errorBoxXpath).getText();
            result.put("errorMessage", errorMessage);
            return result;
        }

        // Continue with update flow and click update button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/button");
        // Clear the existing name and enter the new one
        this.clearElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div/div[2]/div[1]/div/div/input");
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div/div[2]/div[1]/div/div/input", updatedTitleAndName);
        // Clear the existing address and add the new one
        this.clearElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div/div[2]/div[4]/div/div/div/input");
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div/div[2]/div[4]/div/div/div/input", updatedAddressAndCity);
        // Click the submit button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div/div[2]/div[5]/button");

        // Re-search to view updated details
        this.clearElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[1]/div/div/input");
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[1]/div/div/input", customerNumber);
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[3]/button");

        String customerDetailsContainerXpath = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div";

        if (isElementPresent(By.xpath(customerDetailsContainerXpath))) {
            String newName = this.findElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/td[4]").getText();
            String newAddress = this.findElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/td[5]").getText();

            result.put("updatedName", updatedTitleAndName);
            result.put("displayedName", newName);
            result.put("updatedAddress", updatedAddressAndCity);
            result.put("displayedAddress", newAddress);
        } else {
            result.put("errorMessage", "Updated details container not found.");
        }

        return result;
    }

    @Override
    public Map<String, String> updateAccount(String accountNumber) throws Exception {
        Map<String, String> result = new HashMap<>();

        String updatedInterestRate = String.valueOf(interestRate + 5);
        String updatedOverdraftLimit = String.valueOf(overdraftLimit + 1000);

        // Click on update account details option
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[2]/div/div/div[1]/div/button[6]");

        // Enter the account number
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[1]/div/div/input", accountNumber);

        // Click the submit button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[2]/button");

        // Check for error message
        String errorXpath = "/html/body/div/main/div/div[3]/div/div[1]/h3";
        if (isElementPresent(By.xpath(errorXpath))) {
            String errorMessage = this.findElementByXpath(errorXpath).getText();
            result.put("errorMessage", errorMessage);
            return result;
        }

        // Click the update button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/button");

        // Update interest rate
        this.sendKeysToElementByXpath("/html/body/div[1]/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/div/div/div[2]/div[2]/div/div/div/input", updatedInterestRate);

        // Change account type to SAVING
        this.clickElementByXpath("/html/body/div[1]/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/div/div/div[2]/div[3]/div/div/button");
        this.clickElementByXpath("/html/body/div[1]/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/div/div/div[2]/div[3]/div/div/ul/li[4]/div");

        // Update overdraft limit
        this.sendKeysToElementByXpath("/html/body/div[1]/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/div/div/div[2]/div[4]/div/div/div/input", updatedOverdraftLimit);

        // Submit changes
        this.clickElementByXpath("/html/body/div[1]/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/div/div/div[2]/div[5]/button");

        // Re-enter the account number for verification and submit
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[1]/div[1]/div[1]/div/div[1]/input", accountNumber);
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[2]/button");

        // Wait for updated details to appear
        String confirmationContainer = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div";
        if (isElementPresent(By.xpath(confirmationContainer))) {
            String displayedInterestRate = this.findElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/td[5]").getText();
            String displayedOverdraftLimit = this.findElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/td[6]").getText();
            String displayedAccountType = this.findElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr/td[4]").getText();

            result.put("updatedInterestRate", updatedInterestRate);
            result.put("displayedInterestRate", displayedInterestRate);
            result.put("updatedOverdraftLimit", updatedOverdraftLimit);
            result.put("displayedOverdraftLimit", displayedOverdraftLimit);
            result.put("accountType", displayedAccountType);
        } else {
            result.put("errorMessage", "Updated account container not found.");
        }

        return result;
    }

    @Override
    public String deleteAccount(String accountNumber) throws Exception {
        // Click on delete account option
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div[2]/div/div/div[1]/div/button[5]");

        // Enter the account number
        this.sendKeysToElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div/div[1]/div[1]/div/div/input", accountNumber);

        // Click submit
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[1]/div[1]/div[2]/button");

        // Check if the table is present in the DOM
        String tableXpath = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div";
        if (!isElementPresent(By.xpath(tableXpath))) {
            return "Table of accounts not found!";
        }

        // Click delete button
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/button");

        // Check if the confirmation dialog appears
        String confirmationDialogXpath = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div";
        if (!isElementPresent(By.xpath(confirmationDialogXpath))) {
            return "Confirmation dialog not found!";
        }

        // Confirm deletion
        this.clickElementByXpath("/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div/div/table/tbody/tr[1]/div[1]/div/div[3]/button[2]");

        // Check for success message
        String successMessageXpath = "/html/body/div/main/div/div[2]/div/div/div/div/div[2]/div[2]/div/div[1]/h3";
        String successMessage = "Account deleted successfully";

        if (isElementPresent(By.xpath(successMessageXpath))) {
            String deletedConfirmation = this.findElementByXpath(successMessageXpath).getText();
            if (deletedConfirmation.contains(successMessage)) {
                return deletedConfirmation;
            }
        }

        return "Account deletion failed or message not found!";
    }
}
