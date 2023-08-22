/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from 'react';
import { useState } from 'react';
import CustomerDetailsTable from './CustomerDetailsTable';
import axios from 'axios';
import {
  Breadcrumb,
  BreadcrumbItem,
  Button,
  Grid,
  Column,
  NumberInput,
  TextInput,
  Modal,
  ModalBody
} from '@carbon/react';

const CustomerDetailsPage = () => {
  /**
   * States for table visibility and entered search values from the user
   */
  const [isOpened, setTableOpened] = useState(false);
  const [customerDetailsRows, setRows] = useState([]);
  const [accountDetailsRows, setAccountRows] = useState([]);
  const [noResultsOpened, setNoResultsOpened] = useState(false)
  var [numSearch, setNumSearch] = useState("");
  var [nameSearch, setNameSearch] = useState("")


  function handleNumInputChange(e) {
    setNumSearch(e.target.value)
  }

  function handleNameInputChange(e) {
    setNameSearch(e.target.value)
  }

  function displayNoResultsModal() {
    setNoResultsOpened(wasOpened => !wasOpened)
  }

  function submitButtonHandler() {
    let searchQuery;
    if (numSearch !== "") {
      searchQuery = numSearch
      getCustomerByNum(searchQuery)
    }
    else if (nameSearch !== "") {
      searchQuery = nameSearch
      getCustomersByName(searchQuery)
    }
    setTableOpened(wasOpened => !wasOpened)
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
   * Gets the first 10 customers from a given name, builds an array from the response and sets customerDetailsRows' state to this array
   */
  async function getCustomersByName(searchQuery) {
    let responseData;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_CUSTOMER_URL + `/name?name=${searchQuery}&limit=10`)
      .then(response => {
        responseData = response.data;
        try {
          responseData.customers.forEach(customer => {
            let formattedDOB = getDay(customer.dateOfBirth) + "-" + getMonth(customer.dateOfBirth) + "-" + getYear(customer.dateOfBirth)
            let formattedReviewDate = getDay(customer.customerCreditScoreReviewDate) + "-" + getMonth(customer.customerCreditScoreReviewDate) +
            "-" + getYear(customer.customerCreditScoreReviewDate)
            let row;
            row = {
              id: parseInt(customer.id).toString(),
              customerNumber: parseInt(customer.id).toString(),
              sortCode: customer.sortCode,
              customerName: customer.customerName,
              customerAddress: customer.customerAddress,
              formattedDOB : formattedDOB,
              dateOfBirth: customer.dateOfBirth,
              creditScore: customer.customerCreditScore,
              formattedReviewDate : formattedReviewDate,
              nextReviewDate: customer.customerCreditScoreReviewDate,
            };
            rowBuild.push(row);
            getAccountsForCustomers(row.id)
          })
          setRows(rowBuild)
        } catch (e) {
          console.log("Error: " + e);
        }
      }).catch(function (error) {
        if (error.response) {
          console.log(error)
          displayNoResultsModal()
        }
      })

  }

  /**
   * Gets the customer from a given customerNum, builds an array from the response and sets customerDetailsRows' state to this array
   */
  async function getCustomerByNum(searchQuery) {
    let responseData;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_CUSTOMER_URL + `/${searchQuery}`)
      .then(response => {
        responseData = response.data;
        try {
          let row;
          let formattedDOB = getDay(responseData.dateOfBirth) + "-" + getMonth(responseData.dateOfBirth) + "-" + getYear(responseData.dateOfBirth)
          let formattedReviewDate = getDay(responseData.customerCreditScoreReviewDate) + "-" + getMonth(responseData.customerCreditScoreReviewDate) +
            "-" + getYear(responseData.customerCreditScoreReviewDate)
          row = {
            id: parseInt(responseData.id).toString(),
            customerNumber: parseInt(responseData.id).toString(),
            sortCode: responseData.sortCode,
            customerName: responseData.customerName,
            customerAddress: responseData.customerAddress,
            formattedDOB : formattedDOB,
            dateOfBirth: responseData.dateOfBirth,
            creditScore: responseData.customerCreditScore,
            formattedReviewDate : formattedReviewDate,
            nextReviewDate: responseData.customerCreditScoreReviewDate,
          };
          rowBuild.push(row);
          getAccountsForCustomers(row.id)
          setRows(rowBuild)
        } catch (e) {
          console.log("Error: " + e);
        }
      }).catch(function (error) {
        if (error.response) {
          console.log(error)
          displayNoResultsModal()
        }
      })

  }

  /**
   * Gets the accounts for a given customerID, builds an array from the response and sets accountDetailsRows' state to this array
   */
  async function getAccountsForCustomers(customerID) {
    let accountData;
    let accountRowBuild = []
    await axios
      .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerID}`)
      .then(response => {
        accountData = response.data;
        let row;
        accountData.accounts.forEach(account => {
          row = {
            accountNumber: account.id,
            sortCode: account.sortCode,
            accountType: account.accountType,
            interestRate: account.interestRate,
            overdraft: account.overdraft,
            availableBalance: account.availableBalance,
            actualBalance: account.actualBalance,
            accountOpened: account.dateOpened,
            lastStatementDate: account.lastStatementDate,
          };
          accountRowBuild.push(row)
        });
        setAccountRows(accountRowBuild)
      }).catch(function (error) {
        if (error.response) {
          console.log(error)
        }
      })
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
          <BreadcrumbItem>Customer Details</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">
          View Customer Details
        </h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <div className="lower-content">
          <div class="cds--grid" style={{ marginLeft: '30px' }}>
            <div class="cds--row">
              <div class="cds--col">
                <div className="upper">
                  <div className="left-part">
                    <p> Please ensure one field is empty when you press Submit, otherwise the search may not work. After searching, the twistee can be expanded to see accounts belonging to this customer.
</p>
                    <NumberInput
                      className="customer-list-view"
                      id="customerNum"
                      label="Enter a customer's number to view details"
                      placeholder="e.g 1000"
                      invalidText='Please provide a valid number'
                      onChange={e => handleNumInputChange(e)}
                      hideSteppers
                      allowEmpty
                    />
                    <div style={{ marginTop: '20px' }}>
                      <TextInput
                        className="customer-list-name"
                        id="customerNameInput"
                        type="text"
                        labelText="Alternatively, enter the customer's name:"
                        placeholder="Case sensitive"
                        onChange={e => handleNameInputChange(e)}
                      />
                    </div>
                    <div style={{ marginTop: '20px' }}>
                      <Button type="submit" onClick={submitButtonHandler}>
                        Submit
                      </Button>
                    </div>
                  </div>
                  <div className="right-part">
                    <img
                      className="customers"
                      width="30%"
                      src={`${process.env.PUBLIC_URL}/Cloud_report.jpg`}
                      alt="customer"
                    />
                  </div>
                </div>
                {isOpened && (
                  <Column lg={16}>
                    <CustomerDetailsTable customerDetailsRows={customerDetailsRows} accountDetailsRows={accountDetailsRows} />
                  </Column>
                )}
              </div>
            </div>
          </div>
        </div>
      </Column>
      <Modal
        modalHeading="No customers found!"
        open={noResultsOpened}
        onRequestClose={displayNoResultsModal}
        danger
        passiveModal>
        <ModalBody hasForm>
          Please check that the customer number/name is correct
        </ModalBody>
      </Modal>
    </Grid>
  );
};

export default CustomerDetailsPage;
