
package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties;

import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;

import dev.galasa.framework.spi.IConfigurationPropertyStoreService;

@Component(service=CbsaPropertiesSingleton.class, immediate=true)
public class CbsaPropertiesSingleton {

    private static CbsaPropertiesSingleton singletonInstance;
    private static void setInstance(CbsaPropertiesSingleton instance) {
        singletonInstance = instance;
    }

    private IConfigurationPropertyStoreService cps;

    @Activate
    public void activate() {
        setInstance(this);
    }

    @Deactivate
    public void deacivate() {
        setInstance(null);
    }

    public static IConfigurationPropertyStoreService cps() throws CbsaException {
        if (singletonInstance != null) {
            return singletonInstance.cps;
        }
        throw new CbsaException("Attempt to access manager CPS before it has been initialised");
    }

    public static void setCps(IConfigurationPropertyStoreService cps) throws CbsaException {
        if (singletonInstance != null) {
            singletonInstance.cps = cps;
            return;
        }
        throw new CbsaException("Attempt to set manager CPS before instance created");
    }
}
