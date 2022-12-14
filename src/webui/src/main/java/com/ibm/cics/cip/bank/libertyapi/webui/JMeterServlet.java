/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.libertyapi.webui;

import com.vaadin.server.ClientConnector;

//Web requests
import com.vaadin.server.DeploymentConfiguration;
import com.vaadin.server.ServiceException;
import com.vaadin.server.VaadinRequest;
import com.vaadin.server.VaadinService;
import com.vaadin.server.VaadinServlet;
import com.vaadin.server.VaadinServletService;
import com.vaadin.server.VaadinSession;
import com.vaadin.ui.Component;


public class JMeterServlet extends VaadinServlet {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    private static final long serialVersionUID = 898354532369443197L;

    public JMeterServlet() {
    	//Disable cross-site request forgery (XSRF) protection 
        System.setProperty(getPackageName() + "." + "disable-xsrf-protection",
                "true");
    }

    @Override
    protected VaadinServletService createServletService(
            DeploymentConfiguration deploymentConfiguration)
            throws ServiceException {
//    	("created jmeter servlet - testing");
        JMeterService service = new JMeterService(this, deploymentConfiguration);
        service.init();

        return service;
    }

    private String getPackageName() {
    	//Get the package name
        String pkgName;
        final Package pkg = this.getClass().getPackage();
        if (pkg != null) {
            pkgName = pkg.getName();
        } else {
            final String className = this.getClass().getName();
            pkgName = new String(className.toCharArray(), 0,
                    className.lastIndexOf('.'));
        }
        return pkgName;
    }

    public static class JMeterService extends VaadinServletService {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";

        private static final long serialVersionUID = -5874716650679865909L;

        public JMeterService(VaadinServlet servlet,
                DeploymentConfiguration deploymentConfiguration)
                throws ServiceException {
            super(servlet, deploymentConfiguration);
        }

        @Override
        protected VaadinSession createVaadinSession(VaadinRequest request)
                throws ServiceException {
        	//Create a new session
            return new JMeterSession(this);
        }
    }

    public static class JMeterSession extends VaadinSession {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";

        private static final long serialVersionUID = 4596901275146146127L;

        public JMeterSession(VaadinService service) {
            super(service);
        }

        @Override
        public String createConnectorId(ClientConnector connector) {
        	//Create a connector ID
            if (connector instanceof Component) {
                Component component = (Component) connector;
                return component.getId() == null ? super
                        .createConnectorId(connector) : component.getId();
            }
            return super.createConnectorId(connector);
        }
    }
}