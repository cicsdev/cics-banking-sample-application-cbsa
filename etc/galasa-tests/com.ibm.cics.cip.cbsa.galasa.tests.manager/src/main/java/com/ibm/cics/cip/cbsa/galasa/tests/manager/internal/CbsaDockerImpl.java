package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaDocker;

import dev.galasa.docker.DockerManagerException;
import dev.galasa.docker.IDockerContainer;


public class CbsaDockerImpl implements ICbsaDocker{

    private IDockerContainer container;
	private String 			 hostname;

	public CbsaDockerImpl(IDockerContainer container, String hostname) {
		this.container = container;
		this.hostname = hostname;
	}

    public static CbsaDockerImpl provision(CbsaManagerImpl manager) throws CbsaException {
		try {
			IDockerContainer container = manager.getDockerManager().provisionContainer("CBSA-webapp", "CBSA", true, "PRIMARY");
			String hostname = manager.getDockerManager().getEngineHostname("PRIMARY");

			return new CbsaDockerImpl(container, hostname);
		} catch (DockerManagerException e) {
			throw new CbsaException("Failed to provision webapp", e);
		}
	}

    @Override
	public String getHostName() throws CbsaException {
		try {
			int port = container.getExposedPorts().get("8080/tcp").get(0).getPort();
			return this.hostname + ":" + String.valueOf(port);
		} catch (DockerManagerException e) {
			throw new CbsaException("Failed to get webapp port", e);
		}

	}



}
