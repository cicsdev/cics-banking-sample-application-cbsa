package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;

import dev.galasa.framework.spi.ConfigurationPropertyStoreException;
import dev.galasa.framework.spi.cps.CpsProperties;

/**
 * CBSA Region Image Tag
 *
 * @galasa.name cbsa.region.applid
 *
 * @galasa.description The tag for the CICS Region Image used by the CBSA tests
 *
 * @galasa.required Yes
 *
 * @galasa.valid_values A valid Java string value
 *
 * @galasa.examples
 * <code>cbsa.region.applid=IYK2ZQX1</code>
 */

public class CbsaRegionApplid extends CpsProperties {
    public static String get() throws CbsaException {
        try {
            String regionApplid = getStringNulled(CbsaPropertiesSingleton.cps(), "region", "applid");
            if(regionApplid == null) {
                throw new CbsaException("Could not find CICS Region APPLID property. Ensure 'cbsa.region.applid' has been set.");
            }
            return regionApplid;
        } catch (ConfigurationPropertyStoreException e) {
            throw new CbsaException("Problem retrieving CICS Region APPLID from CPS Properties: "  +  e);
        }
    }
}
