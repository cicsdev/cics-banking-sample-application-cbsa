package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal;

import java.lang.annotation.Annotation;

import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.validation.constraints.NotNull;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.osgi.service.component.annotations.Component;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaAccount;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaCustomer;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaManagerField;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaTerminal;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaWebInterface;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaAccount;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaCustomer;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaDocker;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaTerminal;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaWebInterface;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties.CbsaPropertiesSingleton;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties.CbsaRegionApplid;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties.CbsaRegionCredentialsTag;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties.CbsaRegionImageTag;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties.CbsaWebInterfaceUrl;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.properties.CbsaZosImageTag;

import dev.galasa.ManagerException;

import dev.galasa.cicsts.CicstsManagerException;
import dev.galasa.cicsts.MasType;
import dev.galasa.cicsts.spi.ICicsRegionProvisioned;
import dev.galasa.cicsts.spi.ICicstsManagerSpi;
import dev.galasa.docker.spi.IDockerManagerSpi;

import dev.galasa.framework.spi.AbstractManager;
import dev.galasa.framework.spi.AnnotatedField;
import dev.galasa.framework.spi.ConfigurationPropertyStoreException;
import dev.galasa.framework.spi.GenerateAnnotatedField;
import dev.galasa.framework.spi.IConfigurationPropertyStoreService;
import dev.galasa.framework.spi.IDynamicStatusStoreService;
import dev.galasa.framework.spi.IFramework;
import dev.galasa.framework.spi.IManager;
import dev.galasa.framework.spi.ResourceUnavailableException;

import dev.galasa.http.IHttpManager;
import dev.galasa.http.spi.IHttpManagerSpi;

import dev.galasa.framework.spi.creds.CredentialsException;

import dev.galasa.framework.spi.language.GalasaTest;

import dev.galasa.ipnetwork.IpNetworkManagerException;
import dev.galasa.selenium.Browser;
import dev.galasa.selenium.IWebPage;
import dev.galasa.selenium.SeleniumManagerException;

import dev.galasa.selenium.internal.SeleniumManagerImpl;
import dev.galasa.selenium.internal.properties.SeleniumGridEndpoint;
import dev.galasa.textscan.spi.ITextScannerManagerSpi;

import dev.galasa.zos.IZosImage;
import dev.galasa.zos.IZosManager;
import dev.galasa.zos.ZosManagerException;
import dev.galasa.zos.spi.IZosManagerSpi;

import dev.galasa.zos3270.IZos3270Manager;
import dev.galasa.zos3270.TerminalInterruptedException;
import dev.galasa.zos3270.Zos3270Exception;
import dev.galasa.zos3270.Zos3270ManagerException;
import dev.galasa.zos3270.spi.IZos3270ManagerSpi;

@Component(service = { IManager.class })
public class CbsaManagerImpl extends AbstractManager {

    protected static final String NAMESPACE = "cbsa";

    private IZos3270ManagerSpi                 z3270manager;
    private IZosManagerSpi                     zosManager;
    private ITextScannerManagerSpi             textScanner;

    private CbsaTerminalImpl                   cbsaTerminal;
    private CbsaWebInterfaceImpl               cbsaWebInterface;

    private IFramework                         framework;
    private ICicstsManagerSpi                  cicsTsManagerSpi;

    private IDockerManagerSpi                  dockerManager;
    private IHttpManagerSpi                    httpManager;
    private SeleniumManagerImpl                seleniumManager;

    private IConfigurationPropertyStoreService cps;
    private IDynamicStatusStoreService         dss;

    private ICicsRegionProvisioned             cicsRegion;

    private IZosImage                          zosImage;

    private HashMap<String, ICbsaCustomer>     customerMap = new HashMap<String, ICbsaCustomer>();

    private boolean terminalAnnotationFound = false;
    private boolean webInterfaceAnnotationFound = false;

    private final static Log logger = LogFactory.getLog(CbsaManagerImpl.class);

    private Browser                            myBrowser = Browser.FIREFOX;
    private URL                                remoteDriverEndpoint;

    @Override
    public void initialise(@NotNull IFramework framework, @NotNull List<IManager> allManagers,
     @NotNull List<IManager> activeManagers,
      @NotNull GalasaTest galasaTest) throws ManagerException {

        super.initialise(getFramework(), allManagers, activeManagers, galasaTest);

        this.framework = framework;

        if(galasaTest.isJava()) {
          List<AnnotatedField> ourFields = findAnnotatedFields(CbsaManagerField.class);
          if (!ourFields.isEmpty()) {
              youAreRequired(allManagers, activeManagers, galasaTest);
          }
        }
        try {
          IConfigurationPropertyStoreService cps = framework.getConfigurationPropertyService(NAMESPACE);
          CbsaPropertiesSingleton.setCps(cps);
          } catch (ConfigurationPropertyStoreException e) {
          throw new CbsaException("Unable to request framework services for cps", e);
        }
    }

    @Override
    public void youAreRequired(@NotNull List<IManager> allManagers, @NotNull List<IManager> activeManagers, @NotNull GalasaTest galasaTest)
    throws ManagerException {
      if (activeManagers.contains(this)) {
        return;
      }

      activeManagers.add(this);

      z3270manager = addDependentManager(allManagers, activeManagers, galasaTest, IZos3270ManagerSpi.class);
      if (z3270manager == null) {
          throw new Zos3270ManagerException("The zOS 3270 Manager is not available");
      }

      zosManager = addDependentManager(allManagers, activeManagers, galasaTest, IZosManagerSpi.class);
      if (zosManager == null) {
          throw new ZosManagerException("The zOS Manager is not available");
      }

      textScanner = addDependentManager(allManagers, activeManagers, galasaTest, ITextScannerManagerSpi.class);
      if (textScanner == null) {
          throw new ManagerException("The text scanner is not available");
      }

      dockerManager = addDependentManager(allManagers, activeManagers, galasaTest, IDockerManagerSpi.class);
      if (dockerManager == null) {
          throw new ManagerException("The Docker Manager is not available");
      }

      httpManager = addDependentManager(allManagers, activeManagers, galasaTest, IHttpManagerSpi.class);
      if (httpManager == null) {
          throw new ManagerException("The Http Manager is not available");
      }

      cicsTsManagerSpi = addDependentManager(allManagers, activeManagers, galasaTest, ICicstsManagerSpi.class);
      if (cicsTsManagerSpi == null) {
          throw new ManagerException("CICS TS manager was not available.");
      }

      seleniumManager = addDependentManager(allManagers, activeManagers, galasaTest, SeleniumManagerImpl.class);
      if (seleniumManager == null) {
          throw new ManagerException("Selenium Manager is not available");
      }
    }

    @Override
    public void provisionStart() throws ManagerException, ResourceUnavailableException {
      List<AnnotatedField> foundAnnotatedFields = findAnnotatedFields(CbsaManagerField.class);

      for (AnnotatedField annotatedField : foundAnnotatedFields) {
        Field field = annotatedField.getField();
        List<Annotation> annotations = annotatedField.getAnnotations();

        if (field.getType() == ICbsaTerminal.class) {
          CbsaTerminal annotation = field.getAnnotation(CbsaTerminal.class);
          if (annotation != null) {
            terminalAnnotationFound = true;
            try {
              cbsaTerminal = generateCBSATerminalFromAnnotation(field, annotations);
              registerAnnotatedField(field, cbsaTerminal);
            } catch (CredentialsException e) {
              throw new CbsaException("Failed to provision generate - Invalid Credentials: ", e);
            }
          }
        } else if (field.getType() == ICbsaWebInterface.class) {
          CbsaWebInterface annotation = field.getAnnotation(CbsaWebInterface.class);
          if (annotation != null) {
            webInterfaceAnnotationFound = true;
            try {
              cbsaWebInterface = generateCbsaWebInterfaceFromAnnotation(field, annotations);
              registerAnnotatedField(field, cbsaWebInterface);
            } catch (SeleniumManagerException | MalformedURLException | ConfigurationPropertyStoreException e) {
              throw new CbsaException("Failed to provision generate - Selenium error: ", e);
            }
          }
        }
      }

      if (terminalAnnotationFound && cbsaTerminal == null) {
        try {
          this.cbsaTerminal = (CbsaTerminalImpl) generateCbsaTerminal();
        } catch (CredentialsException e) {
          throw new CbsaException("Failed to provision generate - Invalid Credentials: ", e);
        }
      }

      if (webInterfaceAnnotationFound && cbsaWebInterface == null) {
        try {
          this.cbsaWebInterface = (CbsaWebInterfaceImpl) generateCbsaWebInterface();
        } catch (CbsaException | ResourceUnavailableException | SeleniumManagerException | MalformedURLException | ConfigurationPropertyStoreException e) {
          throw new CbsaException("Failed to provision generate - Selenium error: ", e);
        }
      }

      if(cbsaTerminal != null) {
        cbsaTerminal.connectToCics();
        cbsaTerminal.loginToApplication();
      }

      generateAnnotatedFields(CbsaManagerField.class);
    }

    public CbsaTerminalImpl generateCbsaTerminal() throws CbsaException, CredentialsException, CicstsManagerException, ZosManagerException {
      IZosImage zosImage = getZosManager().provisionImageForTag(CbsaZosImageTag.get());
      ICicsRegionProvisioned cicsRegion = new CicsRegionFactory(cicsTsManagerSpi, CbsaRegionImageTag.get(), zosImage, CbsaRegionApplid.get(), MasType.CICS);
      String loginCredentialsTag = CbsaRegionCredentialsTag.get();
      try {
        CbsaTerminalImpl terminal = new CbsaTerminalImpl(framework, cicsRegion, zosImage, getCicsTsManager(), getTextScanManager(), loginCredentialsTag);
        return terminal;
      } catch (ZosManagerException | Zos3270ManagerException | TerminalInterruptedException | IpNetworkManagerException e) {
        throw new CbsaException("We could not find the image CICS ", e);
      }
    }

    @GenerateAnnotatedField(annotation = CbsaTerminal.class)
    public CbsaTerminalImpl generateCBSATerminalFromAnnotation(Field field, List<Annotation> annotations) throws CbsaException, CredentialsException, CicstsManagerException, ZosManagerException {
      return generateCbsaTerminal();
    }

    @GenerateAnnotatedField(annotation = CbsaCustomer.class)
    public ICbsaCustomer generateCBSACustomerFromAnnotation(Field field, List<Annotation> annotations) throws CbsaException, CredentialsException, Zos3270Exception {
      CbsaCustomer annotation = field.getAnnotation(CbsaCustomer.class);
      ICbsaCustomer customer = new CbsaCustomerImpl(this, framework, annotation.existing(), this.getCBSATerminal());
      customerMap.put(annotation.tag(), customer);
      return customer;
    }

    @GenerateAnnotatedField(annotation = CbsaAccount.class)
    public ICbsaAccount generateCBSAAccountFromAnnotation(Field field, List<Annotation> annotations) throws CbsaException, CredentialsException {
      CbsaAccount annotation = field.getAnnotation(CbsaAccount.class);
      return generateCBSAAccount(annotation.accountType(), annotation.tag());
    }

    @GenerateAnnotatedField(annotation = CbsaDockerImpl.class)
    public ICbsaDocker generateCBSAWebApp(Field field, List<Annotation> annotations) throws CbsaException {
      return CbsaDockerImpl.provision(this);
    }

    public CbsaWebInterfaceImpl generateCbsaWebInterface() throws CbsaException, SeleniumManagerException, ResourceUnavailableException, MalformedURLException, ConfigurationPropertyStoreException {
      String url = CbsaWebInterfaceUrl.get();

      remoteDriverEndpoint = new URL(SeleniumGridEndpoint.get());

      FirefoxOptions options = new FirefoxOptions();
      options.addArguments("--headless");

      DesiredCapabilities capabilities = new DesiredCapabilities();
      capabilities.setBrowserName(myBrowser.getDriverName());
      capabilities.merge(options);

      RemoteWebDriver driver = new RemoteWebDriver(remoteDriverEndpoint, capabilities);
      logger.info("Fetching URL");
      driver.get(url);
      logger.info("Page Title = " + driver.getTitle());

      List<IWebPage> pages = new ArrayList<>();

      CbsaWebInterfaceImpl webInterface = new CbsaWebInterfaceImpl(seleniumManager, driver, pages, null);
      return webInterface;
    }

    @GenerateAnnotatedField(annotation = CbsaWebInterface.class)
    public CbsaWebInterfaceImpl generateCbsaWebInterfaceFromAnnotation(Field field, List<Annotation> annotations) throws CbsaException, SeleniumManagerException, ResourceUnavailableException, MalformedURLException, ConfigurationPropertyStoreException {
      return generateCbsaWebInterface();
    }

    @Override
    public void provisionDiscard() {
      for (ICbsaCustomer customer : customerMap.values()) {
        try {
          customer.discardSelf(cbsaTerminal);
        } catch (CbsaException e) {
          logger.info("Failed to provision discard: ", e);
        }
      }
    }

    public ICbsaAccount generateCBSAAccount(String accountType, String tag) throws CbsaException {
      if (accountType.equals("MORTGAGE") || accountType.equals("LOAN") || accountType.equals("ISA")|| accountType.equals("SAVING") || accountType.equals("CURRENT")) {
        ICbsaCustomer customer = customerMap.get(tag);
        ICbsaAccount account = new CbsaAccountImpl(this, framework, customer.getCustomerNumber(), this.cbsaTerminal, accountType);
        customer.addToAccountslist(account);
        return account;
      }
      throw new CbsaException("Invalid account type");
    }

    public ITextScannerManagerSpi getTextScanManager() {
    	return textScanner;
    }

    public ICicstsManagerSpi getCicsTsManager() {
      return this.cicsTsManagerSpi;
    }

    public IZosImage getZosImage() {
      return this.zosImage;
    }

    public ICicsRegionProvisioned getCicsRegion() {
        return cicsRegion;
    }

    public IZosManagerSpi getZosManager(){
      return this.zosManager;
    }

    public IZos3270ManagerSpi getZos3270Manager(){
      return this.z3270manager;
    }

    public CbsaTerminalImpl getCBSATerminal(){
      return this.cbsaTerminal;
    }

    public IDockerManagerSpi getDockerManager() {
      return dockerManager;
    }

    protected IDynamicStatusStoreService getDSS() {
      return this.dss;
    }

    public IConfigurationPropertyStoreService getCPS() {
      return this.cps;
    }

    @Override
    public boolean areYouProvisionalDependentOn(@NotNull IManager otherManager) {
        if (otherManager instanceof IZosManager || otherManager instanceof IZos3270Manager || otherManager instanceof IHttpManager || otherManager instanceof ICicstsManagerSpi) {
            return true;
        }
        return super.areYouProvisionalDependentOn(otherManager);
    }
  }
