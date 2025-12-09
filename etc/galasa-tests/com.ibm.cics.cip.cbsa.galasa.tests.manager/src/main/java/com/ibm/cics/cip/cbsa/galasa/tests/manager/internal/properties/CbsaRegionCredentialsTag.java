package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;

import dev.galasa.framework.spi.ConfigurationPropertyStoreException;
import dev.galasa.framework.spi.cps.CpsProperties;

/**
 * CBSA Region Credentials Tag
 *
 * @galasa.name cbsa.region.credentials.tag
 *
 * @galasa.description Property for retrieving credentials tag to sign on to a CICS Region
 *
 * @galasa.required Yes
 *
 * @galasa.valid_values A valid Java string value
 *
 * @galasa.examples
 * <code>cbsa.region.credentials.tag=PLEXB</code>
 */

public class CbsaRegionCredentialsTag extends CpsProperties {
    public static String get() throws CbsaException {
        try {
            String regionCredentialsTag = getStringNulled(CbsaPropertiesSingleton.cps(), "region", "credentials.tag");
            if(regionCredentialsTag == null) {
                throw new CbsaException("Could not find CICS Region Credentials Tag property. Ensure 'cbsa.region.credentials.tag' has been set.");
            }
            return regionCredentialsTag;
        } catch (ConfigurationPropertyStoreException e) {
            throw new CbsaException("Problem retrieving CICS Region Credential Tag from CPS Properties: "  +  e);
        }
    }
}
