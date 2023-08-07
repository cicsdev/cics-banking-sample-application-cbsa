import React from 'react';
import { useState } from 'react';
import axios from 'axios';
import {
  Breadcrumb,
  BreadcrumbItem,
  Modal,
  Grid,
  Column,
  Dropdown,
  HeaderName,
  ModalFooter,
  Button,
  Form,
  Stack,
  TextInput,
} from '@carbon/react';

import { Link } from 'react-router-dom';

const AccountCreationPage = () => {

  /**
   * Create variables for default values - current date, sortcode
   */
  const [successText, setSuccessText] = useState("");
  const date = new Date()
  const currentDateToJSON = date.toJSON().split("T")
  const currentDate = currentDateToJSON[0]
  const items = [];
  const sortCode = "987654"

  /**
   * Create states for user entered fields
   */
  const [isModalOpened, setModalOpened] = useState(false);
  const [isFailureModalOpened, setIsFailureModalOpened] = useState(false);
  const [isFailureNetworkModalOpened, setIsFailureNetworkModalOpened] = useState(false);
  const [isLoadingModalOpened, setIsLoadingModalOpened] = useState(false);
  const [enteredInterestRate, setEnteredInterestRate] = useState('');
  const [enteredCustomerID, setEnteredCustomerID] = useState('');
  const [enteredAccountType, setEnteredAccountType] = useState(items[3]);
  const [enteredOverdraftLimit, setEnteredOverdraftLimit] = useState('');

  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  };

  function displayLoadingModal(){
    setIsLoadingModalOpened(wasOpened => !wasOpened)
  }

  function displayFailedModal() {
    setIsFailureModalOpened(wasOpened => !wasOpened);
  }

  function displayFailedNetworkModal() {
    setIsFailureNetworkModalOpened(wasOpened => !wasOpened);
  }

  const enteredCustomerIDChangeHandler = event => {
    setEnteredCustomerID(event.target.value);
  };
  const enteredAccountTypeChangeHandler = event => {
    setEnteredAccountType(event.target.value);
  };
  const enteredOverdraftLimitChangeHandler = event => {
    setEnteredOverdraftLimit(event.target.value);
  };
  const enteredInterestRateChangeHandler = event => {
    setEnteredInterestRate(event.target.value);
  };

  /**
   * Creates an account using the user entered fields then displays either a success or failure modal
   */
  async function createAccount() {
    let responseData;
    try {
      await axios
        .post(process.env.REACT_APP_ACCOUNT_URL, {
          interestRate: enteredInterestRate,
          dateOpened: currentDate,
          overdraft: enteredOverdraftLimit,
          accountType: enteredAccountType,
          customerNumber: enteredCustomerID,
          sortCode: sortCode
        }).then((response) => {
          responseData = response.data
          setSuccessText(responseData.id)
          displayLoadingModal()
          displayModal()
        }).catch(function (error) {
          if (error.response) {
            displayLoadingModal()
            displayFailedModal()
            console.log(error)
          } else if (error.request){
              displayLoadingModal()
              displayFailedNetworkModal()
              console.log(error)
            }
        })
    } catch (e) {
      console.log("Error in creation: " + e)
      displayLoadingModal()
      displayFailedModal()
    }
  }

  async function submitButtonHandler() {
    displayLoadingModal()
    createAccount()
  }

  return (
    <Grid className="landing-page" fullWidth>
      <Column lg={16} md={8} sm={4} className="landing-page__banner">
        <Breadcrumb noTrailingSlash aria-label="Page navigation">
          <BreadcrumbItem>
            <a href="./">Home</a>
          </BreadcrumbItem>
          <BreadcrumbItem>
            <a href="./#/profile/Admin">Control Panel</a>
          </BreadcrumbItem>
          <BreadcrumbItem>Create Account</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">Create new account</h1>
      </Column>
      <div className="content-parent">
        <div className="left-content-account">
          <Form>
            <Stack gap={7}>
              <div style={{ width: 500 }}>
                <TextInput
                  id="text-input-1"
                  type="text"
                  labelText="Customer number"
                  placeholder=""
                  value={enteredCustomerID}
                  onChange={enteredCustomerIDChangeHandler}
                />
              </div>

              <div style={{ width: 500 }}>
                <Dropdown
                  items2={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                  id="default"
                  titleText="Account Type"
                  invalidText="Account Type is not valid"
                  label="Account Type"
                  items={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                  onChange={({ selectedItem }) =>
                    setEnteredAccountType(selectedItem)
                  }

                  selectedItem={enteredAccountType}
                />
              </div>

              <div style={{ width: 500 }}>
                <TextInput
                  id="carbon-number"
                  label="Overdraft Limit:"
                  helperText="Please set the overdraft limit"
                  invalidText="Number is not valid"
                  value={enteredOverdraftLimit}
                  onChange={enteredOverdraftLimitChangeHandler}
                  hideSteppers
                />
              </div>
              <div style={{ width: 500 }}>
              <TextInput
                id="interestRate"
                label="Interest rate:"
                invalidText="Number is not valid"
                helperText="Please enter the interest rate"
                value={enteredInterestRate}
                onChange={enteredInterestRateChangeHandler}
              />
              </div>
              <Button className="displayModal" onClick={submitButtonHandler}>
                Submit
              </Button>
            </Stack>
          </Form>
          <Modal
            passiveModal
            size="sm"
            open={isModalOpened}
            onRequestClose={displayModal}
            preventCloseOnClickOutside>
            <h5>Customer account created successfully</h5>
            <br />
            <br />
            <p>Account Number: {successText}</p>
            <p>Sort Code: {sortCode}</p>
            <p>Account Type: {enteredAccountType}</p>
            <p>Overdraft Limit: {enteredOverdraftLimit}</p>
            <ModalFooter>
              <HeaderName
                className="white-background"
                element={Link}
                to="/Admin/account_details"
                prefix="View account details"
              />
            </ModalFooter>
          </Modal>
          <Modal
            passiveModal
            size="sm"
            open={isLoadingModalOpened}
            preventCloseOnClickOutside
            onRequestClose={displayLoadingModal}>
            <h4> Creating account...</h4>
          </Modal>
          <Modal
            passiveModal
            size="sm"
            open={isFailureNetworkModalOpened}
            preventCloseOnClickOutside
            onRequestClose={displayFailedNetworkModal}>
            <h4> Account failed to create due to a network error</h4>
          </Modal>
          <Modal
            passiveModal
            size="sm"
            open={isFailureModalOpened}
            onRequestClose={displayFailedModal}
            preventCloseOnClickOutside>
            <h5>Customer account failed to create</h5>
            <br />
            <br />
            <p>Please check that all inputs are valid.
              Alternatively, it may be that the customer has reached
              the maximum accounts allowed (10), you can check here:
            </p>
            <ModalFooter>
              <HeaderName
                className="white-background"
                element={Link}
                to="/Admin/customer_details"
                prefix="View customer details"
              />
            </ModalFooter>
          </Modal>
        </div>
        <div className="right-content-account">
          <img className="right-content-account"
            src={`${process.env.PUBLIC_URL}/ibm-db2-support-leadspace.png`}
            alt="account"
          />
        </div>
      </div>
      <Column
        lg={16}
        md={8}
        sm={4}
        className="landing-page__r3 bottom-Column"
      />
    </Grid>
  );
};

export default AccountCreationPage;
