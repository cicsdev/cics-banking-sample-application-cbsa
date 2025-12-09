package com.ibm.cics.cip.cbsa.galasa.tests.manager;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import dev.galasa.framework.spi.ValidAnnotatedFields;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.FIELD })
@CbsaManagerField
@ValidAnnotatedFields({ ICbsaAccount.class })

public @interface CbsaAccount {
    boolean existing() default false;
    String accountType() default "LOAN";
    String tag() default "";
}
