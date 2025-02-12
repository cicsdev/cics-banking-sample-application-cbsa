/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React, { Component } from 'react';
import './app.scss';
import { Content, Theme } from '@carbon/react';
import HomepageHeader from './components/Homepage-Header';
import AdminHeader from './components/Admin-Header';
import HomePage from './content/HomePage';
import AdminPage from './content/AdminPage';
import CustomerCreationPage from './content/CustomerCreationPage';
import AccountCreationPage from './content/AccountCreationPage';
import CustomerDetailsPage from './content/CustomerDetailsPage';
import AccountDetailsPage from './content/AccountDetailsPage';
import CustomerDeletePage from './content/CustomerDeletePage';
import AccountDeletePage from './content/AccountDeletePage'
import { HashRouter, Route, Switch} from 'react-router-dom';




class App extends Component {


  render() {




    return (

      <HashRouter forceRefresh={true} >
        <Theme theme="g100">
          <HomepageHeader />
          <Switch>
            <Route path="/profile/Admin" component={AdminHeader} />
          </Switch>
          <Switch>
            <Route path="/Admin/customer_creation" component={AdminHeader} />
          </Switch>
          <Switch>
            <Route path="/Admin/account_creation" component={AdminHeader} />
          </Switch>
          <Switch>
            <Route path="/Admin/customer_details" component={AdminHeader} />
          </Switch>
          <Switch>
            <Route path="/Admin/account_details" component={AdminHeader} />
          </Switch>
          <Switch>
            <Route path="/Admin/customer_deletion" component={AdminHeader} />
          </Switch>
          <Switch>
            <Route path="/Admin/account_deletion" component={AdminHeader} />
          </Switch>
        </Theme>
        <Content>
          <Switch>
            <Route exact path="./" component={HomePage} />

            <Route
              path="/Admin/customer_creation"
              component={CustomerCreationPage}
            />
            <Route
              path="/Admin/account_creation"
              component={AccountCreationPage}
            />
            <Route
              path="/Admin/customer_details"
              component={CustomerDetailsPage}
            />
            <Route
              path="/Admin/account_details"
              component={AccountDetailsPage}
            />
            <Route
              path="/Admin/customer_deletion"
              component={CustomerDeletePage}
            />
            <Route
              path="/Admin/account_deletion"
              component={AccountDeletePage}
            />
            <Route path="./profile/Admin" component={AdminPage} />
            <Route path="./#/profile/Admin" component={AdminPage} />
            <Route path="/profile/Admin" component={AdminPage} />

            <Route path="/*" component={HomePage} />
          </Switch>
        </Content>
      </HashRouter>
    );
  }
}

// https://stackoverflow.com/questions/34093913/how-to-debug-react-router
class DebugRouter extends HashRouter {
  constructor(props){
    super(props);
    console.log('initial history is: ', JSON.stringify(this.history, null,2))
    this.history.listen((location, action)=>{
      console.log(
        `The current URL is ${location.pathname}${location.search}${location.hash}`
      )
      console.log(`The last navigation action was ${action}`, JSON.stringify(this.history, null,2));
window.alert(`The current URL is ${location.pathname}${location.search}${location.hash}`);
    });
  }
}

export default App;
