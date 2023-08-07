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
  Dropdown,
  ModalFooter,
  TextInput,
  TableContainer,
  Table,
  TableHead,
  TableRow,
  TableHeader,
  TableBody,
  TableCell,
} from '@carbon/react';

/**
 * Customer headers in table
 */
const headers = [
  {
    key: 'customerNumber',
    header: "Customer Number",
  },
  {
    key: 'accountNumber',
    header: 'Account Number',
  },
  {
    key: 'sortCode',
    header: 'Sort Code',
  },
  {
    key: 'accountType',
    header: 'Account Type',
  },
  {
    key: 'interestRate',
    header: 'Interest Rate',
  },
  {
    key: 'overdraft',
    header: 'Overdraft',
  },
  {
    key: 'availableBalance',
    header: 'Available Balance',
  },
  {
    key: 'actualBalance',
    header: 'Actual Balance',
  },
  {
    key: 'formattedDateOpened',
    header: 'Date Opened',
  },
  {
    key: 'formattedLastStatementDue',
    header: 'Last Statement Due',
  },
  {
    key: 'formattedNextStatementDue',
    header: 'Next Statement Due',
  },
];

/**
 * Account headers in table
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
  'Next Statement Due',
];

const AccountDetailsTable = ({accountMainRow}) => {
  /**
   * Set states for all of the current account values, before the user edits any. This provides a fallback if any fields are left blank by the user
   */
  const [accountNumber, setAccountNumber] = useState("")
  const [currentAccountType, setCurrentAccountType] = useState("")
  const [currentOverdraft, setCurrentOverdraft] = useState("")
  const [currentInterestRate, setCurrentInterestRate] = useState("")
  const [accountSortCode, setAccountSortCode] = useState("")
  const [currentActualBalance, setCurrentActualBalance] = useState("")
  const [lastStatementDate, setLastStatementDate] = useState("")
  const [nextStatementDate, setNextStatementDate] = useState("")
  const [dateOpened, setDateOpened] = useState("")
  const [currentAvailableBalance, setCurrentAvailableBalance] = useState("")
  const [currentAccountCustomerNumber, setAccountCustomerNumber] = useState("")

  /**
   * States that are edited by the user
   */
  const [enteredOverdraftLimit, setOverdraftLimit] = useState('');
  const [enteredAccountType, setAccountType] = useState("");
  const [enteredInterestRate, setInterestRate] = useState('');


  function onUpdateMainAccountClick(row) {
    clearExistingData()
    setPrefilledMainAccountData(row)
    displayUpdateAccountModal()
  }

  const enteredInterestRateChangeHandler = event => {
    setInterestRate(event.target.value);
  };

  const enteredOverdraftLimitChangeHandler = event => {
    setOverdraftLimit(event.target.value);
  };

  const enteredAccountTypeChangeHandler = event => {
    setAccountType(event.target.value);
  };

  /**
   * Ensure no states have not been reset between account update
   */
  function clearExistingData() {
    setCurrentAccountType("")
    setCurrentOverdraft("")
    setCurrentInterestRate("")
  }

  /**
   * Set current value states with the data from the main row
   */
  function setPrefilledMainAccountData(row) {
    setAccountNumber(row.cells[1].value)
    setAccountSortCode(row.cells[2].value)
    setAccountType(row.cells[3].value)
    setCurrentAccountType(row.cells[3].value)
    setCurrentInterestRate(row.cells[4].value)
    setCurrentOverdraft(row.cells[5].value)
    setCurrentAvailableBalance(row.cells[6].value)
    setCurrentActualBalance(row.cells[7].value)
    setDateOpened(row.cells[8].value)
    setLastStatementDate(row.cells[9].value)
    setNextStatementDate(row.cells[10].value)
  }

  /**
   * Checks that all edited fields have been filled, if one is empty it uses the fallback value that was set before the user began edits
   * 
   */
  async function updateAccount() {
    let responseData;
    let useInterestRate = enteredInterestRate;
    let useOverdraft = enteredOverdraftLimit;
    let useAccountType = enteredAccountType;
    let useAccountNumber = accountNumber;

    if (useInterestRate.length === 0) {
      useInterestRate = currentInterestRate
    }
    if (useOverdraft.length === 0) {
      useOverdraft = currentOverdraft
    }
    if (useAccountType.length === 0) {
      useAccountType = currentAccountType
    }

    //Update the account with the details given
    try {
      await axios
        .put(process.env.REACT_APP_ACCOUNT_URL + `/${useAccountNumber}`, {
          interestRate: useInterestRate,
          lastStatementDate: lastStatementDate,
          nextStatementDate: nextStatementDate,
          dateOpened: dateOpened,
          actualBalance: currentActualBalance,
          overdraft: useOverdraft,
          accountType: useAccountType,
          id: accountNumber,
          customerNumber: currentAccountCustomerNumber,
          sortCode: accountSortCode,
          availableBalance: currentAvailableBalance
        }).then((response) => {
          responseData = response.data
        }).catch(function (error) {
          if (error.response) {
            console.log(error)
          }
        })
    } catch (e) {
      console.log("Error updating account: " + e)
    }
    setUpdateAccountModalOpened(wasUpdateAccountOpened => !wasUpdateAccountOpened)
  }

  const [isUpdateAccountModalOpened, setUpdateAccountModalOpened] = useState(
    false
  );

  function displayUpdateAccountModal() {
    setUpdateAccountModalOpened(
      wasUpdateAccountOpened => !wasUpdateAccountOpened
    );
  }

  return (
    <DataTable
      rows={accountMainRow}
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
                  <TableRow {...getRowProps({ row })}>
                    {row.cells.map(cell => (
                      <TableCell key={cell.id}>{cell.value}</TableCell>
                    ))}
                    <Button
                      className="displayModal"
                      onClick={() => onUpdateMainAccountClick(row)}>
                      Update
                    </Button>
                    <Modal
                      modalHeading="Update Account"
                      passiveModal
                      open={isUpdateAccountModalOpened}
                      onRequestClose={displayUpdateAccountModal}>

                      <div style={{ width: 200 }}>
                        <TextInput
                          id="accountNumber"
                          value={accountNumber}
                          label="Account number (this cannot be changed)"
                          readOnly
                          hideSteppers
                          allowEmpty
                          style={{ marginBottom: "1rem" }}
                        />
                      </div>

                      <div style={{ width: 200 }}>
                        <TextInput
                          id="interestRate"
                          placeHolder={currentInterestRate}
                          labelText="Interest rate:"
                          onChange={enteredInterestRateChangeHandler}
                          style={{ marginBottom: "1rem" }}
                        />
                      </div>

                      <div style={{ width: 200 }}>
                        <Dropdown
                          items2={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                          id="default"
                          titleText="Account Type"
                          label="Account Type"
                          items={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                          onChange={({ selectedItem }) =>
                            setAccountType(selectedItem)
                          }
                          selectedItem={enteredAccountType}
                          style={{ marginBottom: "1rem" }}
                        />
                      </div>

                      <div style={{ width: 200 }}>
                        <TextInput
                          id="carbon-number"
                          labelText="Overdraft Limit:"
                          placeHolder={currentOverdraft}
                          onChange={enteredOverdraftLimitChangeHandler}
                        />
                      </div>

                      <ModalFooter>
                        <Button onClick={updateAccount}>
                          Submit
                        </Button>
                      </ModalFooter>
                    </Modal>
                  </TableRow>
                </React.Fragment>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}
    />
  );
};

export default AccountDetailsTable;
