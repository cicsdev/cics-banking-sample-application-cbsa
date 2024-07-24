/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from 'react';
import {
  Breadcrumb,
  BreadcrumbItem,
  Button,
  Tabs,
  Tab,
  TabList,
  TabPanels,
  TabPanel,
  Grid,
  Column,
} from '@carbon/react';

import { UserAvatar } from '@carbon/react/icons';

const HomePage = () => {
  return (
    <Grid className="landing-page" fullWidth>
      <Column lg={16} md={8} sm={4} className="landing-page__banner">
        <Breadcrumb noTrailingSlash aria-label="Page navigation">
          <BreadcrumbItem>
            <a href="./">Home</a>
          </BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">
          Welcome to CICS Banking Sample Application
        </h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <Tabs defaultSelectedIndex={0}>
          <TabList className="tabs-group" aria-label="Tab navigation">
            <Tab>About</Tab>
            <Tab>User Guide</Tab>
          </TabList>
          <TabPanels>
            <TabPanel>
              <Grid className="tabs-group-content">
                <Column
                  md={4}
                  lg={7}
                  sm={4}
                  className="landing-page__tab-content">
                  <h2 className="landing-page__subheading">
                    What is CICS Banking Sample Application(CBSA)?
                  </h2>
                  <p className="landing-page__p">
                    CICS Banking Sample Application is an Open Source offering developed by the CICS Transaction Server
                    CiP team. Any Open Source consumed can be found in the Notices.
                  </p>

<p className="landing-page__p">
Click on the User icon <UserAvatar size="24" /> on the top
                    right corner. This offers a log in mechanism. This is not used by CBSA but is presented for use if required. Press TAB to move to SUBMIT and then press enter to activate.</p>
<p className="landing-page__p">
This will take you to the main part of the application.
</p>
                  <a href="https://github.com/cicsdev/cics-banking-sample-application-cbsa" target="+blank"><Button>Learn more (GitHub)</Button></a>
                </Column>
                <Column md={4} lg={{ span: 8, offset: 7 }} sm={4}>
                  <img
                    className="landing-page__illo"
                    src={`${process.env.PUBLIC_URL}/Use_cases_reduce_complexity.png`}
                    alt="CICS Bank Sample Application"
                  />
                </Column>
              </Grid>
            </TabPanel>
            <TabPanel>
              <Grid className="tabs-group-content">
                <Column
                  md={4}
                  lg={7}
                  sm={4}
                  className="landing-page__tab-content">
                  <h2 className="landing-page__subheading">
                    To get started, you must be logged in.
                  </h2>
                  <p className="landing-page__p">
                    Click on the User icon <UserAvatar size="24" /> on the top
                    right corner to enter your Username and Password and click
                    'Submit' to log in. This is an example of what could be 
		    implemented. CBSA as shipped does not use these credentials
		    and you can just press "Submit" leaving those fields blank.
                    <br />
                    <br />
                    Once logged in, user can perform the functions listed below:
                    <br />
                    <br />
                    <div class="indent-class">
                      <p>&#x2022; Make payments</p>
                      <br />
                      <p>&#x2022; Create a new customer</p>
                      <br />
                      <p>&#x2022; Create a new account</p>
                      <br />
                      <p>&#x2022; View or update existing customer details</p>
                      <br />
                      <p>&#x2022; View or update existing account details</p>
                      <br />
                      <p>&#x2022; Delete an existing customer </p>
                      <br />
                      <p>&#x2022; Delete an existing account </p>
                      <br />
                    </div>
                  </p>
                </Column>
                <Column md={4} lg={{ span: 8, offset: 7 }} sm={4}>
                  <img
                    className="landing-page__illo"
                    src={`${process.env.PUBLIC_URL}/cics-ts-6_1.png`}
                    alt="CICS Transaction Server for z/OS"
                  />
                </Column>
              </Grid>
            </TabPanel>
          </TabPanels>
        </Tabs>
      </Column>
    </Grid>
  );
};

export default HomePage;
