/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from 'react';
import { useState } from 'react';
import axios from 'axios';
import {
  DataTable,
  Button,
  Modal,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableExpandHeader,
  TableHeader,
  TableBody,
  TableExpandRow,
  TableCell,
  TableExpandedRow,
  ModalBody,
} from '@carbon/react';

/**
 * Headers for customer row in table
 */
const headers = [
  {
    key: 'customerNumber',
    header: 'Customer Number',
  },
  {
    key: 'sortCode',
    header: 'Sort Code',
  },
  {
    key: 'customerName',
    header: 'Customer Name',
  },
  {
    key: 'customerAddress',
    header: 'Customer Address',
  },
  {
    key: 'formattedDOB',
    header: 'Date of Birth',
  },
  {
    key: 'creditScore',
    header: 'Credit Score',
  },
  {
    key: 'formattedReviewDate',
    header: 'Next Review Date',
  }
];

/**
 * Headers for account rows in table
 */
const account_headers = [
  'Account Number',
  'Sort Code',
  'Account Type',
  'Interest Rate',
  'Overdraft Limit',
  'Available Balance',
  'Actual Balance',
  'Account Opened',
  'Last Statement Due',
];

const CustomerDeleteTables = ({customerRow, accountRow}) => {

  /**
   * States to store the customer and account to delete, as well as the open/close status of the respective modals
   */
  const [customerNameToDelete, setCustomerNameToDelete] = useState("")
  const [accountNumberToDelete, setAccountNumberToDelete] = useState("")
  const [isModalOpened, setModalOpened] = useState(false);
  const [wasUnableDeleteOpened, setUnableDeleteModalOpened] = useState(false);
  const [isSuccessfulCustomerDeleteModalOpened, setSuccessfulCustomerDeleteModalOpened] = useState(false)
  const [isSuccessfulAccountDeleteModalOpened, setSuccessfulAccountDeleteModalOpened] = useState(false)

  /**
   * Get the customer name from the row and show the confirm customer delete modal
   */
  function onDeleteCustomerClick(row) {
    setCustomerNameToDelete(row.cells[2].value)
    displayModal()
  }

  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  }

  function displaySuccessfulCustomerDeleteModal() {
    setSuccessfulCustomerDeleteModalOpened(wasOpened => !wasOpened)
  }

  function displaySuccessfulAccountDeleteModal() {
    setSuccessfulAccountDeleteModalOpened(wasOpened => !wasOpened)
  }

  /**
   * Get the accountNumber from the row and show the confirm account delete modal
   */
  function onDeleteAccountClick(row) {
    setAccountNumberToDelete(row.accountNumber)
    displayAccountModal()
  }

  /**
   * Checks that a customer has no outstanding accounts and then deletes the customer
   * If the customer still has accounts or the delete fails a failure modal is shown, else a success modal is shown
   */
  async function deleteCustomer(row) {
    let customerNumber = row.cells[0].value
    let responseData;
    let howManyAccountsData;
    try {
      await axios
        .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerNumber}`)
        .then(response => {
          howManyAccountsData = response.data
        })
      let numberOfAccounts = howManyAccountsData.numberOfAccounts
      if (parseInt(numberOfAccounts) === 0) {
        await axios
          .delete(process.env.REACT_APP_CUSTOMER_URL + `/${customerNumber}`)
          .then(response => {
            responseData = response.data
            displayModal()
            displaySuccessfulCustomerDeleteModal()
          })
      }
      else {
        displayModal()
        displayUnableDeleteModal()
      }
    } catch (e) {
      console.log(e)
      displayModal()
      displayUnableDeleteModal()
    }
  }

  /**
   * Deletes the account from a given row
   */
  async function deleteAccount(row) {
    let accountNumber = row.accountNumber
    let responseData;
    try {
      await axios
        .delete(process.env.REACT_APP_ACCOUNT_URL + `/${accountNumber}`)
        .then(response => {
          responseData = response.data
          console.log(responseData)
        })
      displayAccountModal()
      displaySuccessfulAccountDeleteModal()
    } catch (e) {
      console.log(e)
      displayAccountModal()
      displayUnableDeleteModal()
    }
  }

  const [isModalAccountOpened, setAccountModalOpened] = useState(false);

  function displayAccountModal() {
    setAccountModalOpened(wasAccountOpened => !wasAccountOpened);
  }

  function displayUnableDeleteModal() {
    setUnableDeleteModalOpened(wasUnableDeleteOpened => !wasUnableDeleteOpened);
  }

  return (
    <DataTable
      rows={customerRow}
      headers={headers}
      render={({
        rows,
        headers,
        getHeaderProps,
        getRowProps,
        getTableProps,
      }) => (
        <TableContainer title="" description="">
          <Table {...getTableProps()}>
            <TableHead>
              <TableRow>
                <TableExpandHeader />
                {headers.map(header => (
                  <TableHeader {...getHeaderProps({ header })}>
                    {header.header}
                  </TableHeader>
                ))}
                <div className="header-filler" />
              </TableRow>
            </TableHead>
            <TableBody>
              {rows.map(row => (
                <React.Fragment key={row.id}>
                  <TableExpandRow {...getRowProps({ row })}>
                    {row.cells.map(cell => (
                      <TableCell key={cell.id}>{cell.value}</TableCell>
                    ))}
                    <Button
                      kind="danger"
                      className="displayModal"
                      onClick={() => onDeleteCustomerClick(row)}>
                      Delete
                    </Button>
                    <Modal
                      modalHeading="Are you sure you want to delete this customer?"
                      open={isModalOpened}
                      onRequestClose={displayModal}
                      onRequestSubmit={() => deleteCustomer(row)}
                      danger
                      primaryButtonText="Delete"
                      secondaryButtonText="Cancel">
                      <ModalBody hasForm>
                        Warning! Are you sure you want to delete {customerNameToDelete}? This action cannot be undone
                      </ModalBody>
                    </Modal>
                    <Modal
                      modalHeading="Unable to delete the customer!"
                      open={wasUnableDeleteOpened}
                      onRequestClose={displayUnableDeleteModal}
                      danger
                      passiveModal>
                      <ModalBody hasForm>
                        Please delete all associated accounts before deleting
                        the customer
                      </ModalBody>
                    </Modal>
                    <Modal
                      modalHeading="Customer deleted successfully"
                      open={isSuccessfulCustomerDeleteModalOpened}
                      onRequestClose={() => {displaySuccessfulCustomerDeleteModal(); window.location.reload(true)}}
                      passiveModal>
                    </Modal>
                  </TableExpandRow>

                  <TableExpandedRow colSpan={headers.length + 2}>
                    <p className="account-details">Accounts belonging to this customer</p>
                    <Table>
                      <TableHead>
                        <TableRow>
                          {account_headers.map(header => (
                            <TableHeader id={header.key} key={header}>
                              {header}
                            </TableHeader>
                          ))}
                        </TableRow>
                      </TableHead>
                      <TableBody>
                        {accountRow.map((row, index) => (
                          <TableRow key={row.id}>
                            {Object.keys(row)
                              .filter(key => key !== 'id')
                              .map(key => {
                                return (
                                  <TableCell key={key}>{row[key]}</TableCell>
                                );
                              })}
                            <Button
                              kind="danger"
                              className="displayModal"
                              onClick={() => onDeleteAccountClick(row)}>
                              Delete
                            </Button>
                            <Modal
                              modalHeading="Are you sure you want to delete account"
                              open={isModalAccountOpened}
                              onRequestClose={displayAccountModal}
                              onRequestSubmit={() => deleteAccount(row)}
                              danger
                              primaryButtonText="Delete"
                              secondaryButtonText="Cancel">
                              <ModalBody>
                                Are you sure you want to delete account {accountNumberToDelete}? This action cannot be undone
                              </ModalBody>
                            </Modal>
                            <Modal
                              modalHeading="Account deleted successfully"
                              open={isSuccessfulAccountDeleteModalOpened}
                              onRequestClose={() => {displaySuccessfulAccountDeleteModal(); window.location.reload()}}
                              passiveModal
                            />
                          </TableRow>
                        ))}
                      </TableBody>
                    </Table>
                  </TableExpandedRow>
                </React.Fragment>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}
    />
  );
};

export default CustomerDeleteTables;
