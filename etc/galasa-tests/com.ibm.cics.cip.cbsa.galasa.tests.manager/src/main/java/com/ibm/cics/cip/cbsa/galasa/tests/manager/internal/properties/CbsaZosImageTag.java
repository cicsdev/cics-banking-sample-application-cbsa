package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;

import dev.galasa.framework.spi.ConfigurationPropertyStoreException;
import dev.galasa.framework.spi.cps.CpsProperties;

/**
 * CBSA Zos Image Tag
 *
 * @galasa.name cbsa.zos.image.tag
 *
 * @galasa.description The tag for the ZOS Image used by the CBSA tests
 *
 * @galasa.required Yes
 *
 * @galasa.valid_values A valid Java string value
 *
 * @galasa.examples
 * <code>cbsa.cbsa.zos.image.tag=PRIMARY</code>
 */

public class CbsaZosImageTag extends CpsProperties {
    public static String get() throws CbsaException {
		try {
			String zosImageTag = getStringNulled(CbsaPropertiesSingleton.cps(), "cbsa", "zos.image.tag");
			if(zosImageTag == null) {
				throw new CbsaException("Could not find z/OS image tag property. Ensure 'cbsa.zos.image.tag' has been set.");
			}
			return zosImageTag;
		} catch (ConfigurationPropertyStoreException e) {
			throw new CbsaException("Problem retrieving z/OS image tag from CPS Properties: "  +  e);
		}
    }
}
