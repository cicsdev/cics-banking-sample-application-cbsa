import React from 'react';
import {
  Header,
  HeaderContainer,
  HeaderName,
  HeaderMenuButton,
  HeaderGlobalBar,
  HeaderGlobalAction,
  SkipToContent,
  SideNav,
  SideNavItems,
  SideNavMenu,
  SideNavMenuItem,
  ExpandableSearch,
  OverflowMenu,
  OverflowMenuItem,
} from '@carbon/react';
import { Switcher, Notification, UserProfile } from '@carbon/react/icons';
import { Link } from 'react-router-dom';

const AdminHeader = () => {
  return (
    <HeaderContainer
      render={({ isSideNavExpanded, onClickSideNavExpand }) => (
        <Header aria-label="Carbon Tutorial">
          <SkipToContent />
          <HeaderMenuButton
            aria-label="Open menu"
            isCollapsible
            onClick={onClickSideNavExpand}
            isActive={isSideNavExpanded}
          />
          <HeaderName element={Link} to="/" >
            CICS Banking Sample Application
          </HeaderName>
          <SideNav
            aria-label="Side navigation"
            expanded={isSideNavExpanded}
            isPersistent={false}
            onOverlayClick={onClickSideNavExpand}>
            <SideNavItems>
              <SideNavMenu title="Create">
                <SideNavMenuItem href="./#/Admin/customer_creation">
                  Create Customer
                </SideNavMenuItem>
                <SideNavMenuItem href="./#/Admin/account_creation">
                  Create Account
                </SideNavMenuItem>
              </SideNavMenu>
              <SideNavMenu title="Delete">
                <SideNavMenuItem href="./#/Admin/deletion">
                  Delete Customer or Account
                </SideNavMenuItem>
              </SideNavMenu>
              <SideNavMenu title="View Details">
                <SideNavMenuItem href="./#/Admin/customer_details">
                  View Customer Details
                </SideNavMenuItem>
                <SideNavMenuItem href="./#/Admin/account_details">
                  View Account Details
                </SideNavMenuItem>
              </SideNavMenu>
              <SideNavMenu title="Update Details">
                <SideNavMenuItem href="./#/Admin/customer_details">
                  Update Customer Details
                </SideNavMenuItem>
                <SideNavMenuItem href="./#/Admin/account_details">
                  Update Account Details
                </SideNavMenuItem>
              </SideNavMenu>
            </SideNavItems>
          </SideNav>
          <HeaderGlobalBar>
            <span
              className="cds--popover-container 
                              cds--popover--caret 
                              cds--popover--high-contrast 
                              cds--popover--bottom 
                              cds--tooltip 
                              cds--icon-tooltip">
              <button
                aria-label="ExpandableSearch"
                tabindex="0"
                class="cds--header__action
                                cds--header__action__search 
                                 cds--btn cds--btn--primary 
                                 cds--btn--icon-only 
                                 cds--btn 
                                 cds--btn--primary"
                type="button"
                aria-labelledby="tooltip-3">
                <ExpandableSearch
                  tooltipAlignment="center"
                  size="sm"
                  labelText="Search"
                  closeButtonLabelText="Clear search input"
                  id="search-expandable-1"
                />
              </button>
            </span>
            <HeaderGlobalAction
              aria-label="Notifications"
              tooltipAlignment="center">
              <Notification size={20} />
            </HeaderGlobalAction>
            <HeaderGlobalAction tooltipAlignment="center">
              <OverflowMenu
                ariaLabel="overflow-menu"
                renderIcon={UserProfile}
                flipped="true">
                <OverflowMenuItem itemText="Profile" />
                <OverflowMenuItem itemText="Privacy" />
                <OverflowMenuItem itemText="Security" />
                <button
                  aria-label="Sign-Out"
                  class="cds--btn 
                                   cds--btn--primary 
                                   cds--btn--primary--submit">
                  <HeaderName element={Link} to="/" prefix="Logout" />
                </button>
              </OverflowMenu>
            </HeaderGlobalAction>
            <HeaderGlobalAction
              aria-label="App Switcher"
              tooltipAlignment="end">
              <Switcher size={20} />
            </HeaderGlobalAction>
          </HeaderGlobalBar>
        </Header>
      )}
    />
  );
};

export default AdminHeader;
