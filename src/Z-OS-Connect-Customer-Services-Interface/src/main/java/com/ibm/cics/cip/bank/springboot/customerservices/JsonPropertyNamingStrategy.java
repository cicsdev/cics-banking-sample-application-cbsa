/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices;

import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.cfg.MapperConfig;
import com.fasterxml.jackson.databind.introspect.AnnotatedField;
import com.fasterxml.jackson.databind.introspect.AnnotatedMethod;

public class JsonPropertyNamingStrategy extends PropertyNamingStrategy{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    @Override
    public String nameForField(MapperConfig<?> config, AnnotatedField field, String defaultName) {
        return convert(field.getName());
    }

    @Override
    public String nameForGetterMethod(MapperConfig<?> config, AnnotatedMethod method, String defaultName) {
        return convert(method.getName().toString());
    }

    @Override
    public String nameForSetterMethod(MapperConfig<?> config, AnnotatedMethod method, String defaultName) {
        return convert(method.getName().toString());
    }

    private String convert(String input) {
        return input.substring(3);
    }
}
