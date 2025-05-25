package com.cbsa.migration.config;

import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration for OpenAPI documentation
 */
@Configuration
public class OpenApiConfig {

    @Bean
    public OpenAPI bankingAppOpenAPI() {
        return new OpenAPI()
                .info(new Info().title("CBSA Banking Application API")
                        .description("API documentation for the CICS Banking Sample Application Java migration")
                        .version("v0.0.1")
                        .contact(new Contact()
                                .name("CBSA Migration Team")
                                .email("support@cbsa-migration.example.com"))
                        .license(new License().name("Internal Use Only")))
                .externalDocs(new ExternalDocumentation()
                        .description("CBSA Banking Application Migration Documentation")
                        .url("https://example.com/docs"));
    }
}
