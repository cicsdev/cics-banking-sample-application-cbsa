
/*
 * Copyright contributors to the Galasa project
 *
 * SPDX-License-Identifier: EPL-2.0
 */
package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;
import dev.galasa.framework.spi.cps.CpsProperties;

public class CbsaProperties extends CpsProperties {
    public static String getProperty(String property) throws CbsaException {
        return getStringWithDefault(CbsaPropertiesSingleton.cps(), "FAILED", "image", property, "CBSA");
    }
}
