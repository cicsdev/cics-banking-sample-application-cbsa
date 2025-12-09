package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal;

import javax.validation.constraints.NotNull;

import dev.galasa.ProductVersion;
import dev.galasa.cicsts.CicstsManagerException;
import dev.galasa.cicsts.MasType;
import dev.galasa.cicsts.spi.BaseCicsImpl;
import dev.galasa.cicsts.spi.ICicstsManagerSpi;
import dev.galasa.zos.IZosImage;
import dev.galasa.zosbatch.IZosBatchJob;

public class CicsRegionFactory extends BaseCicsImpl {

        public CicsRegionFactory(ICicstsManagerSpi cicstsManager, String cicsTag, IZosImage zosImage, String applid,
                MasType masType) {
            super(cicstsManager, cicsTag, zosImage, applid, masType);
        }

        @Override
        public void submitRuntimeJcl() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'submitRuntimeJcl'");
        }

        @Override
        public boolean hasRegionStarted() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'hasRegionStarted'");
        }

        @Override
        public ProductVersion getVersion() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'getVersion'");
        }

        @Override
        public void startup() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'startup'");
        }

        @Override
        public void shutdown() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'shutdown'");
        }

        @Override
        public boolean isProvisionStart() {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'isProvisionStart'");
        }

        @Override
        public String getUssHome() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'getUssHome'");
        }

        @Override
        public String getJvmProfileDir() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'getJvmProfileDir'");
        }

        @Override
        public String getJavaHome() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'getJavaHome'");
        }

        @Override
        public IZosBatchJob getRegionJob() throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'getRegionJob'");
        }

        @Override
        public void alterSit(@NotNull String sitParam, String sitValue) throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'alterSit'");
        }

        @Override
        public void removeSit(@NotNull String sitParam) throws CicstsManagerException {
            // TODO Auto-generated method stub
            throw new UnsupportedOperationException("Unimplemented method 'removeSit'");
        }

    }
