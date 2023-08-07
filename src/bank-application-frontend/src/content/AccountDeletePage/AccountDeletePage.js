import React from 'react';
import { useState } from 'react';
import AccountDeleteTables from './AccountDeleteTables';
import {
  Breadcrumb,
  BreadcrumbItem,
  Button,
  Grid,
  Column,
  NumberInput,
} from '@carbon/react';

const AccountDeletePage = () => {
  const [isOpened, setIsOpened] = useState(false);
  var [searchAccountValue, setSearchAccountValue] = useState("")

  function handleAccountNumberInput(e){
    setSearchAccountValue(e.target.value)
  }

  function display() {
    setIsOpened(wasOpened => !wasOpened);
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
        <h1 className="landing-page__heading">Account Deletion</h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <div className="lower-content">
          <div class="cds--grid" style={{ marginLeft: '30px' }}>
            <div class="cds--row">
              <div class="cds--col">
                <div className="upper">
                  <div className="left-part">
                  <h3 className='inputOptions'> Delete an existing account</h3>
                    <NumberInput
                      className="customer-list-view"
                      label= 'Enter account number'
                      min= "0"
                      invalidText= 'Please provide a valid number'
                      hideSteppers
                      allowEmpty
                      onChange={handleAccountNumberInput}
                    />
                    <div style={{ marginTop: '20px' }}>
                      <Button type="submit" onClick={display}>
                        Submit
                      </Button>
                    </div>
                  </div>
                  <div className="right-part">
                    <img
                      className="bee"
                      src={`${process.env.PUBLIC_URL}/sustainability-v2_0.png`}
                      alt="bee"
                    />
                  </div>
                </div>
                {isOpened && (
                  <Column lg={16}>
                    <AccountDeleteTables 
                    accountQuery={searchAccountValue} />
                  </Column>
                )}
              </div>
            </div>
          </div>
        </div>
      </Column>
    </Grid>
  );
};

export default AccountDeletePage;
