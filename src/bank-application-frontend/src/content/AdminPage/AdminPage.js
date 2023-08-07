/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from 'react';
import { useState } from 'react';
import {
  Breadcrumb,
  BreadcrumbItem,
  Button,
  Tabs,
  Tab,
  TabList,
  TabPanels,
  TabPanel,
  HeaderName,
  Grid,
  Column,
  TextInput,
  NumberInput,
  Modal,
  Form,
} from '@carbon/react';
import { Link } from 'react-router-dom';

const AdminPage = () => {
  const [isModalOpened, setModalOpened] = useState(false);
  const [isSuccessModalOpened, setSuccessModalOpened] = useState(false);

  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  }

  function displaySuccessModal() {
    setSuccessModalOpened(wasOpened => !wasOpened);
    setModalOpened(wasOpened => !wasOpened);
  }

  function deleteModal() {
    setSuccessModalOpened(wasOpened => !wasOpened);
  }

  

  return (
    <Grid className="landing-page" fullWidth>
      <Column lg={16} md={8} sm={4} className="landing-page__banner">
        <Breadcrumb noTrailingSlash aria-label="Page navigation">
          <BreadcrumbItem>
            <a href="./">Home</a>
          </BreadcrumbItem>
          <BreadcrumbItem>Control Panel</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">
          Welcome to CICS Banking Sample Application
        </h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <Tabs defaultSelectedIndex={0}>
          <TabList className="tabs-group" aria-label="Tab navigation">
            <Tab>User Functions</Tab>
          </TabList>
          <TabPanels>
            <TabPanel>
              <Grid className="tabs-group-content">
                <div className="control-panel-parent">
                  <div className="control-panel-left-child">
                    <Column
                      lg={16}
                      md={8}
                      sm={4}
                      className="landing-page__tab-content">
                      <h5>User Services</h5>
                      <br />
                      <Button kind="ghost">
                        <HeaderName
                          className="white-background"
                          element={Link}
                          to="/Admin/customer_creation"
                          prefix="Create a new customer"
                        />
                      </Button>
                      <hr class="half-width" />
                      <Button kind="ghost">
                        <HeaderName
                          className="white-background"
                          element={Link}
                          to="/Admin/customer_deletion"
                          prefix="Delete existing customer"
                        />
                      </Button>
                      <hr class="half-width" />
                      <Button kind="ghost">
                        <HeaderName
                          className="white-background"
                          element={Link}
                          to="/Admin/customer_details"
                          prefix="View/Update customer details"
                        />
                      </Button>
                      <hr class="half-width" />
                      <br />
                      <br />
                      <h5>Account Services</h5>
                      <br />
                      <Button kind="ghost">
                        <HeaderName
                          className="white-background"
                          element={Link}
                          to="/Admin/account_creation"
                          prefix="Create a New Account"
                        />
                      </Button>
                      <hr class="half-width" />
                      <Button kind="ghost">
                        <HeaderName
                          className="white-background"
                          element={Link}
                          to="/Admin/account_deletion"
                          prefix="Delete existing account"
                        />
                      </Button>
                      <hr class="half-width" />
                      <Button kind="ghost">
                        <HeaderName
                          className="white-background"
                          element={Link}
                          to="/Admin/account_details"
                          prefix="View/Update account details"
                        />
                      </Button>
                      <hr class="half-width" />
                    </Column>
                  </div>
                  <div className="control-panel-right-child">
                    <img
                      className="landing-page__illo"
                      src={`${process.env.PUBLIC_URL}/promo-band-illustration-1920x1080-2x.png`}
                      alt="Carbon illustration"
                    />
                  </div>
                </div>
              </Grid>
            </TabPanel>
          </TabPanels>
        </Tabs>
      </Column>
    </Grid>
  );
};

export default AdminPage;
