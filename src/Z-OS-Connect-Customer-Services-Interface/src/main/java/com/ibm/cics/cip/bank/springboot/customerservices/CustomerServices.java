/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices;

import com.beust.jcommander.JCommander;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(scanBasePackages =
{ "com.ibm.cics.cip.bank.springboot.customerservices.controllers" })
public class CustomerServices
{




	public static void main(String[] args)
	{
		final Logger log = LoggerFactory.getLogger(CustomerServices.class);
		JCommander.newBuilder().build().parse(args);

		log.info("Running with address: {}",
				ConnectionInfo.getAddressAndPort());

		// Run the application. From here out, only the WebController and
		// ParamsController classes really matter.
		SpringApplication.run(CustomerServices.class, args);
	}

}
