/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from "react";
import { useState } from "react";
import axios from "axios";
import {
  DataTable,
  NumberInput,
  Button,
  Modal,
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

let newDateOfBirth = currentDateOfBirth.substring(6,10) + "-" + currentDateOfBirth.substring(3,5) + "-" + currentDateOfBirth.substring(0,2)
    try {
      await axios
        .put(process.env.REACT_APP_CUSTOMER_URL + `/${customerNumber}`, {
          customerAddress: useAddress,
          creditScore: currentCreditScore,
          dateOfBirth: newDateOfBirth,
          sortCode: currentSortCode,
          customerName: useName
        }).then((response) => {
        });
    } catch (e) {
      console.log("Error updating customer: " + e)
    }
    setUpdateCustomerModalOpened(wasUpdateCustomerOpened => !wasUpdateCustomerOpened)
    window.location.reload(true)
  }


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
                      onRequestClose={() => {displayUpdateCustomerModal(); window.location.reload(true)}}
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
				onClick={() => {updateCustomer()}}>
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
            </TableBody>
          </Table>
        </TableContainer>
      )}
    />
  );
}
export default CustomerDetailsTable;

