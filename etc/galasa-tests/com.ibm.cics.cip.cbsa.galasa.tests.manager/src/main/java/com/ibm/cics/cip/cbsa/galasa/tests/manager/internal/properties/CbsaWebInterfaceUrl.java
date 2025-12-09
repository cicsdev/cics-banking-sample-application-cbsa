package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;

import dev.galasa.framework.spi.ConfigurationPropertyStoreException;
import dev.galasa.framework.spi.cps.CpsProperties;

/**
 * CBSA Webui URL Tag
 *
 * @galasa.name cbsa.webui.url
 *
 * @galasa.description The tag for the CBSA Webui URL used by the Selenium Web Driver to provision a page
 *
 * @galasa.required Yes
 *
 * @galasa.valid_values A valid Java string value
 *
 * @galasa.examples
 * <code>cbsa.webui.url=http://winmvs2c.hursley.ibm.com:26540/webui-1.0/#/profile/Admin</code>
 */

public class CbsaWebInterfaceUrl extends CpsProperties {
    public static String get() throws CbsaException {
        try {
            String url = getStringNulled(CbsaPropertiesSingleton.cps(), "webui", "url");
            if(url == null) {
                throw new CbsaException("Could not find CBSA Webui URL property. Ensure 'cbsa.webui.url' has been set.");
            }
            return url;
        } catch (ConfigurationPropertyStoreException e) {
            throw new CbsaException("Problem retrieving CBSA Webui URL from CPS Properties: "  +  e);
        }
    }
}
