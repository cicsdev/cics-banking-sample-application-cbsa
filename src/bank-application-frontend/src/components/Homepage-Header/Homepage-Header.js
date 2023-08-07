/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

import React from 'react';
import {
  Header,
  HeaderContainer,
  HeaderName,
  HeaderGlobalBar,
  HeaderGlobalAction,
  SkipToContent,
  ExpandableSearch,
  OverflowMenu,
  OverflowMenuItem,
  TextInput,
} from '@carbon/react';
import { Switcher, Notification, UserAvatar } from '@carbon/react/icons';
import { Link } from 'react-router-dom';

const HomepageHeader = () => {
  return (
    <HeaderContainer
      render={({ isSideNavExpanded, onClickSideNavExpand }) => (
        <Header aria-label="Carbon Tutorial">
          <SkipToContent />
          <HeaderName element={Link} to="./" prefix="IBM">
            CICS Banking Sample Application
          </HeaderName>
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
                tabIndex="0"
                className="cds--header__action
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
                renderIcon={UserAvatar}
                flipped="true">
                <OverflowMenuItem itemText="Username" />
                <TextInput id="username" label="Text label" />
                <OverflowMenuItem itemText="Password" />
                <TextInput
                  type="password"
		  label="password"
                  required
                  pattern="(?=.*\d)(?=.*[a-z])(?=.*[A-Z]).{6,}"
                />
                <button
                  aria-label="Submit"
                  class="cds--btn 
                                   cds--btn--primary 
                                   cds--btn--primary--submit">
                  <HeaderName
                    element={Link}
                    to="/profile/Admin"
                    prefix="Submit"
                  />
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

export default HomepageHeader;
