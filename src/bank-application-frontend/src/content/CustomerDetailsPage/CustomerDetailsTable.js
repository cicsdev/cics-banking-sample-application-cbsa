import React from "react";
import { useState } from "react";
import axios from "axios";
import {
  DataTable,
  NumberInput,
  Button,
  Modal,
  Dropdown,
  ModalFooter,
  TextInput,
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
} from "@carbon/react";

/**
 * Headers for customer rows of the table
 */
const headers = [
  {
    key: "customerNumber",
    header: "Customer Number",
  },
  {
    key: "sortCode",
    header: "Sort Code",
  },
  {
    key: "customerName",
    header: "Customer Name",
  },
  {
    key: "customerAddress",
    header: "Customer Address",
  },
  {
    key: "formattedDOB",
    header: "Date of Birth",
  },
  {
    key: "creditScore",
    header: "Credit Score",
  },
  {
    key: "formattedReviewDate",
    header: "Next Review Date",
  },
];

/**
 * Headers for the account rows of the table
 */
const account_headers = [
  "Account Number",
  "Sort Code",
  "Account Type",
  "Interest Rate",
  "Overdraft Limit",
  "Available Balance",
  "Actual Balance",
  "Account Opened",
  "Last Statement Due",
];
  
const CustomerDetailsTable = ({ customerDetailsRows, accountDetailsRows }) => {
  
  /**
  * Is the updateCustomer popup being used
  */
  const [isUpdateCustomerModalOpened, setUpdateCustomerModalOpened] = useState(
    false
  );

  /**
   * Is the updateAccount popup being used
   */
  const [isUpdateAccountModalOpened, setUpdateAccountModalOpened] = useState(
    false
  );

  /**
   * Is the noCustomer popup being used
   */
  const [isNoCustomersModalOpen, setNoCustomersModalOpened] = useState(
    false
  );

  function displayNoCustomerModal() {
    setNoCustomersModalOpened(
      wasNoCustomerModalOpened => !wasNoCustomerModalOpened
    );
  }

  //Set values for the customer to be updated
  const [currentCustomerName, setCurrentCustomerName] = useState("");
  const [currentCustomerAddress, setCurrentCustomerAddress] = useState("")
  const [currentCustomerNumber, setCurrentCustomerNumber] = useState("")
  const [currentSortCode, setSortCode] = useState("")
  const [currentDateOfBirth, setDateOfBirth] = useState("")
  const [currentCreditScore, setCreditScore] = useState("")
  const [enteredNameChange, setNameChange] = useState("")
  const [enteredAddressChange, setAddressChange] = useState("")

  /**
   * If user presses update customer
   * Calls setPreflledCustomerData to get all the current data out of that row on the table
   * Calls displayUpdatetoCustomerModal to display the updaate customer popup
   */
  function onUpdateCustomerButtonClick(row) {
    setPrefilledCustomerData(row)
    displayUpdateCustomerModal()
  }

  /**
   * Set all the customer data states using the row on the table
   * This allows us to update a specific customer when more than 1 is available
   */
  function setPrefilledCustomerData(row) {
    setCurrentCustomerNumber(row.cells[0].value)
    setSortCode(row.cells[1].value)
    setCurrentCustomerName(row.cells[2].value)
    setCurrentCustomerAddress(row.cells[3].value)
    setDateOfBirth(row.cells[4].value)
    setCreditScore(row.cells[5].value)
  }

  /**
   * Toggle update customer popup visibility
   */
  function displayUpdateCustomerModal() {
    setUpdateCustomerModalOpened(
      wasUpdateCustomerOpened => !wasUpdateCustomerOpened
    );
  }

  const enteredAddressChangeHandler = event => {
    setAddressChange(event.target.value)
  }

  const enteredNameChangeHandler = event => {
    setNameChange(event.target.value)
  }

  /**
   * Updates a customer by making a put request 
   * The function ensures that both of the changeable fields are not left blank by using the value before an update attempt was made as a fallback
   */
  async function updateCustomer() {
    let responseData;
    let useAddress = enteredAddressChange
    let useName = enteredNameChange
    let customerNumber = currentCustomerNumber
    //Checks if the fields were left empty on the customer update popup
    if (useAddress.length === 0) {
      useAddress = currentCustomerAddress
    }
    if (useName.length === 0) {
      useName = currentCustomerName
    }
    try {
      await axios
        .put(process.env.REACT_APP_CUSTOMER_URL + "/" + `${customerNumber}`, {
          customerAddress: useAddress,
          creditScore: currentCreditScore,
          dateOfBirth: currentDateOfBirth,
          sortCode: currentSortCode,
          customerName: useName
        }).then((response) => {
          responseData = response.data
        });
      console.log(responseData)
    } catch (e) {
      console.log("Error updating customer: " + e)
    }
    setUpdateCustomerModalOpened(wasUpdateCustomerOpened => !wasUpdateCustomerOpened)
  }

  //Set default values for the account to be updated
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
   * When the update button on an account is pressed
   * Calls clearExistingData to ensure all states are set back to empty
   * Calls setPrefilledAccountData to set the current values from the row
   * Calls displayUpdateAccountModal to then show the update account popup
   */
  function onUpdateAccountClick(row) {
    clearExistingData()
    setPrefilledAccountData(row)
    displayUpdateAccountModal()
  }

  /**
   * Sets all states to ""
   */
  function clearExistingData() {
    setAccountNumber("")
    setCurrentAccountType("")
    setCurrentOverdraft("")
    setCurrentInterestRate("")
    setAccountSortCode("")
    setCurrentActualBalance("")
    setLastStatementDate("")
    setNextStatementDate("")
    setDateOpened("")
    setAccountType("")
    setCurrentAvailableBalance("")
    setAccountCustomerNumber("")
  }

  /**
   * Sets data to the values within the row
   * This allows us to specify which account we want to update if there is more than 1 available
   */
  function setPrefilledAccountData(row) {
    setAccountNumber(row.accountNumber)
    setAccountType(row.accountType)
    setCurrentAccountType(row.accountType)
    setCurrentOverdraft(row.overdraft)
    setCurrentInterestRate(row.interestRate)
    setAccountSortCode(row.sortCode)
    setCurrentActualBalance(row.actualBalance)
    setLastStatementDate(row.lastStatementDate)
    setNextStatementDate(row.nextStatementDate)
    setDateOpened(row.dateOpened)
    setCurrentAvailableBalance(row.availableBalance)
    setAccountCustomerNumber(row.customerNumber)
  }


  /**
   * States for the user entered fields
   */
  const [enteredOverdraftLimit, setOverdraftLimit] = useState("");
  const [enteredAccountType, setAccountType] = useState("");
  const [enteredInterestRate, setInterestRate] = useState("");

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
   * Update a selected account
   * Checks that entered data from the customer isn't blank - if it is it falls back on whatever the value was before the customer opened the edit modal
   */
  async function updateAccount() {
    let responseData;
    let useInterestRate = enteredInterestRate;
    let useOverdraft = enteredOverdraftLimit;
    let useAccountType = enteredAccountType;
    let useAccountNumber = accountNumber;

    //Checks for empty fields
    if (useInterestRate.length === 0) {
      useInterestRate = currentInterestRate
    }
    if (useOverdraft.length === 0) {
      useOverdraft = currentOverdraft
    }
    if (useAccountType.length === 0) {
      useAccountType = currentAccountType
    }

    try {
      await axios
        .put(process.env.REACT_APP_ACCOUNT_URL + `${useAccountNumber}`, {
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
        }).catch(function(error){
          if (error.response){
            console.log(error)
          }
        })
    } catch (e) {
      console.log("Error updating account: " + e)
    }

    /**
     * Toggle account update modal visibility
     */
    setUpdateAccountModalOpened(wasUpdateAccountOpened => !wasUpdateAccountOpened)
  }

  /**
   * Set account update modal visibility
   */
  function displayUpdateAccountModal() {
    setUpdateAccountModalOpened(
      wasUpdateAccountOpened => !wasUpdateAccountOpened
    );
  };

  /**
   * Called when the table requires the expanded rows
   * - maps the data using the accountNumber as the sorting key
   */
  function getExpandedRows(row) {
    return (
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
            {(accountDetailsRows).map(row => (
              <TableRow key={row.accountNumber}>
                {Object.keys(row)
                  .filter(key => key !== "id")
                  .map(key => {
                    return (
                      <TableCell key={key}>{row[key]}</TableCell>
                    );
                  })}
                <Button
                  className="displayModal"
                  onClick={() => onUpdateAccountClick(row)}
                >
                  Update
                </Button>

              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableExpandedRow>
    );
  }

  return (
    <DataTable
      rows={customerDetailsRows}
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
                <React.Fragment key={row.customerNumber}>
                  <TableExpandRow {...getRowProps({ row })}>
                    {row.cells.map(cell => (
                      <TableCell key={cell.id}>{cell.value}</TableCell>
                    ))}
                    <Button
                      className="displayModal"
                      onClick={() => onUpdateCustomerButtonClick(row)}
                    >
                      Update
                    </Button>
                    <Modal
                      modalHeading="Update Customer"
                      passiveModal
                      open={isUpdateCustomerModalOpened}
                      onRequestClose={displayUpdateCustomerModal}
                    >
                      <TextInput
                        data-modal-primary-focus
                        id="text-input-1"
                        labelText="Customer Name"
                        defaultValue={currentCustomerName}
                        onChange={enteredNameChangeHandler}
                        style={{ marginBottom: "1rem" }}
                      />

                      <NumberInput
                        className="customerNumber"
                        iconDescription="Customer Number (cannot be changed)"
                        label="Customer Number (cannot be changed)"
                        min={0}
                        value={currentCustomerNumber}
                        readOnly
                        style={{ marginBottom: "1rem" }}
                        hideSteppers
                      />

                      <NumberInput
                        className="sortcode-update"
			                  iconDescription="Sort Code (cannot be changed)"
                        label="Sort Code (cannot be changed)"
                        min={0}
                        value={currentSortCode}
                        readOnly
                        hideSteppers
                      />

                      <div style={{ width: 350 }}>
                        <TextInput
                          data-modal-primary-focus
                          id="text-input-1"
                          labelText="Customer Address"
                          defaultValue={currentCustomerAddress}
                          onChange={enteredAddressChangeHandler}
                        />
                      </div>

                      <ModalFooter>
                        <Button
                          onClick={() => updateCustomer()}>
                          Submit
                        </Button>
                      </ModalFooter>
                    </Modal>
                    <Modal
                      passiveModal
                      modalHeading="No customers found"
                      open={isNoCustomersModalOpen}
                      onRequestClose={displayNoCustomerModal}
                    />
                  </TableExpandRow>
                  {getExpandedRows(row)}
                </React.Fragment>
              ))}
              <div>
                <Modal
                  modalHeading="Update Customer Account"
                  passiveModal
                  open={isUpdateAccountModalOpened}
                  onRequestClose={displayUpdateAccountModal}
                >

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
                      helperText="Please set the overdraft limit"
                      placeHolder={currentOverdraft}
                      onChange={enteredOverdraftLimitChangeHandler}
                    />
                  </div>
                  <ModalFooter>
                    <Button
                      onClick={updateAccount}>
                      Submit
                    </Button>
                  </ModalFooter>
                </Modal>
              </div>
            </TableBody>
          </Table>
        </TableContainer>
      )}
    />
  );
}
export default CustomerDetailsTable;

