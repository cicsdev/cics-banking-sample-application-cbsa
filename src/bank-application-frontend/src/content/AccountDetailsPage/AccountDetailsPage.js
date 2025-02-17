/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from 'react';
import { useState } from 'react';
import AccountDetailsTable from './AccountDetailsTable';
import axios from 'axios';
import {
  Breadcrumb,
  BreadcrumbItem,
  Button,
  Grid,
  Column,
  NumberInput,
  Modal,
  ModalBody
} from '@carbon/react';

const AccountDetailsPage = () => {
  const [isOpened, setIsOpened] = useState(false);
  const [userInput, setUserInput] = useState("")
  const [accountMainRow, setMainRow] = useState([]);
  const [showNoResultsModal, setShowNoResultsModal] = useState(false)

  const numberInputProps = {
    id: "accountNum",
    label: 'Enter an account number to view the associated account',
    min: 0,
    defaultValue: "",
    invalidText: 'Please provide a valid number',
  };

  function handleChange(value) {
    setUserInput(value)
  }

  function displayNoResults() {
    setShowNoResultsModal(wasOpened => !wasOpened)
  }

  function handleClick() {
    let searchQuery = userInput;
    if (userInput.length !== 0){
        getCustomerAccounts(searchQuery)
        setIsOpened(wasOpened => !wasOpened)
      } else {
        displayNoResults()
      }
  }

  function getYear(date){
    return date.substring(0,4)
  }

  function getMonth(date){
    return date.substring(5,7)
  }

  function getDay(date){
    return date.substring(8,10)
  }

  /**
   * Get the account for a given accountNumber, create an array of the response and set accountMainRow to this array
   * Calls getOtherAccountsForCustomer to find the other accounts tied to this customer's number
   */
  async function getCustomerAccounts(searchQuery) {
    let account;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_ACCOUNT_URL + `/${searchQuery}`)
      .then(response => {
        account = response.data;
      }).catch (function (error) {
        if (error.response){
          displayNoResults()
          console.log(error)
        }
      })
    try {
      let row;
      let formattedDateOpened = getDay(account.dateOpened) + "-" + getMonth(account.dateOpened) + "-" + getYear(account.dateOpened)
      let formattedLastStatementDue = getDay(account.lastStatementDate) + "-" + getMonth(account.lastStatementDate) + "-" + getYear(account.lastStatementDate)
      let formattedNextStatementDue = getDay(account.nextStatementDate) + "-" + getMonth(account.nextStatementDate) + "-" + getYear(account.nextStatementDate)
      row = {
        customerNumber: parseInt(account.customerNumber),
        id: account.id,
        accountNumber: account.id,
        sortCode: account.sortCode,
        accountType: account.accountType,
        interestRate: account.interestRate,
        overdraft: account.overdraft,
        availableBalance: account.availableBalance,
        actualBalance: account.actualBalance,
        formattedDateOpened: formattedDateOpened,
        dateOpened: account.dateOpened,
        formattedLastStatementDue: formattedLastStatementDue,
        formattedNextStatementDue: formattedNextStatementDue,
        lastStatementDue: account.lastStatementDate,
        nextStatementDue: account.nextStatementDate,
      };
      rowBuild.push(row);
      setMainRow(rowBuild)
    } catch (e) {
      console.log("Error: " + e);
    }
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
          <BreadcrumbItem>Account Details</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">
          View Account Details
        </h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <div className="lower-content">
          <div class="cds--grid" style={{ marginLeft: '30px' }}>
            <div class="cds--row">
              <div class="cds--col">
                <div className="upper">
                  <div className="left-part">
                    <NumberInput
                      className="customer-list-view"
                      {...numberInputProps}
                      onChange={e => handleChange(e.target.value)}
                      hideSteppers
                    />
                    <div style={{ marginTop: '20px' }}>
                      <Button type="submit" onClick={handleClick}>
                        Submit
                      </Button>
                    </div>
                  </div>
                  <div className="right-part">
                    <img
                      className="bee"
                      src={`${process.env.PUBLIC_URL}/Financial-Services-Cloud-leadspace.jpg
`}
                      alt="bee"
                    />
                  </div>
                </div>
                {isOpened && (
                  <Column lg={16}>
                    <AccountDetailsTable accountMainRow={accountMainRow}/>
                  </Column>
                )}
              </div>
            </div>
          </div>
        </div>
      </Column>
      <Modal
        modalHeading="No accounts found!"
        open={showNoResultsModal}
        onRequestClose={displayNoResults}
        danger
        passiveModal>
        <ModalBody hasForm>
          Please check that the account number is correct
        </ModalBody>
      </Modal>
    </Grid>
  );
};

export default AccountDetailsPage;
