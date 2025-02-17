/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from 'react';
import { useState } from 'react';
import axios from 'axios';
import CustomerDeleteTables from './CustomerDeleteTables';
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

const CustomerDeletePage = () => {

  /**
   * States to store isOpened value of the table and the value the user has entered to search with
   */
  const [isOpened, setIsOpened] = useState(false);
  var [searchCustomerValue, setSearchCustomerValue] = useState("")
  const [customerDetailsRows, setRows] = useState([]);
  const [accountDetailsRows, setAccountRows] = useState([]);
  const [isNoResultsModalOpen, setIsNoResultsModalOpen] = useState(false)

  function handleCustomerNumberInput(e){
    setSearchCustomerValue(e.target.value)
  }

  function display() {
    setIsOpened(wasOpened => !wasOpened);
  }

  function displayNoResultsModal(){
    setIsNoResultsModalOpen(wasOpened => !wasOpened)
  }

  async function handleSubmitButtonClick(){
    let searchQuery = searchCustomerValue;
    await getCustomerByNum(searchQuery)
    .then(display())
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
    * Finds the customer using the customerNumber entered by the user and creates an array from the server response
    * customerDetailsRows' state is set to this array
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
    * Finds the accounts for the given customer ID and creates an array from the server response.
    * accountDetailsRows' state is set to this array
    */
   async function getAccountsForCustomers(customerID) {
     let accountData;
     let rowBuild = [];
     await axios
       .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerID}`)
       .then(response => {
         accountData = response.data;
         try {
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
             rowBuild.push(row)
           });
           setAccountRows(rowBuild)
         } catch (e) {
           console.log("Error fetching accounts for customer: " + customerID + ": " + e);
         }
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
          <BreadcrumbItem>Delete</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">Customer Deletion</h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <div className="lower-content">
          <div class="cds--grid" style={{ marginLeft: '30px' }}>
            <div class="cds--row">
              <div class="cds--col">
                <div className="upper">
                  <div className="left-part">
                  <h5>Note: A customer cannot be deleted if they still have accounts associated with them</h5>
                    <NumberInput
                      className="customer-list-view"
                      label= 'Enter customer number'
                      min= "0"
                      invalidText= 'Please provide a valid number'
                      hideSteppers
                      allowEmpty
                      onChange={(e) => handleCustomerNumberInput(e)}
                    />
                    <div style={{ marginTop: '20px' }}>
                      <Button type="submit" onClick={handleSubmitButtonClick}>
                        Submit
                      </Button>
                    </div>
                  </div>
                  <div className="right-part">
                    <img
                      className="bee"
                      src={`${process.env.PUBLIC_URL}/Planning_Analytics-4-mobile.jpg`}
                      alt="bee"
                    />
                  </div>
                </div>
                {isOpened && (
                  <Column lg={16}>
                    <CustomerDeleteTables
                    customerRow={customerDetailsRows} accountRow={accountDetailsRows}/>
                  </Column>
                )}
              </div>
            </div>
          </div>
        </div>
      </Column>
      <Modal
        modalHeading="No customers found!"
        open={isNoResultsModalOpen}
        onRequestClose={displayNoResultsModal}
        danger
        passiveModal>
        <ModalBody hasForm>
          Please check that the customer number is correct
        </ModalBody>
      </Modal>
    </Grid>
  );
};

export default CustomerDeletePage;
