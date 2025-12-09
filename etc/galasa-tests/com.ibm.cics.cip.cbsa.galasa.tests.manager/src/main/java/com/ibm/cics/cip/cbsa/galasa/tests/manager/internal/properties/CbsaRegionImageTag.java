package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;

import dev.galasa.framework.spi.ConfigurationPropertyStoreException;
import dev.galasa.framework.spi.cps.CpsProperties;

/**
 * CBSA Region Image Tag
 *
 * @galasa.name cbsa.region.image.tag
 *
 * @galasa.description The tag for the CICS Region Image used by the CBSA tests
 *
 * @galasa.required Yes
 *
 * @galasa.valid_values A valid Java string value
 *
 * @galasa.examples
 * <code>cbsa.cbsa.region.image.tag=PRIMARY</code>
 */

public class CbsaRegionImageTag extends CpsProperties {
    public static String get() throws CbsaException {
		try {
			String regionImageTag = getStringNulled(CbsaPropertiesSingleton.cps(), "cbsa", "region.image.tag");
			if(regionImageTag == null) {
				throw new CbsaException("Could not find z/OS image tag property. Ensure 'cbsa.region.image.tag' has been set.");
			}
			return regionImageTag;
		} catch (ConfigurationPropertyStoreException e) {
			throw new CbsaException("Problem retrieving z/OS image tag from CPS Properties: "  +  e);
		}
    }
}
