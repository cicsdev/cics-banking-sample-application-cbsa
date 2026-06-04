# IBM Record Generator Integration Guide

Quick guide for generating Java classes from COBOL ADATA files using DBB.

## Prerequisites

- COBOL ADATA files (generated during COBOL compilation)
- Functional JZOS JAR (`ibm.jzos.jar`)
- Artifactory credentials for IBM RecordGen JAR

## Step 1: Create symbols.yaml

Define your ADATA files and their COBOL symbols:

```yaml
# symbols.yaml
CREACC:
  - 'HOST-ACCOUNT-ROW'
  - 'HOST-PROCTRAN-ROW'
  - 'RETURN-DATA'

INQCUST:
  - 'ABNDINFO-REC'
  - 'DFHCOMMAREA'

UPDCUST:
  - 'DFHCOMMAREA'
```

**Format**: `ADATA_NAME: [list of COBOL symbol names]`

## Step 2: Configure dbb-app.yaml

### Option A: Generic Mode (Recommended for Quick Start)

Generates Java classes, compiles them, and creates JAR automatically.

```yaml
- task: SetupGradleEnvironment
  variables:
    - name: skipSetup
      value: true

- task: RunBuildTool
  variables:
    - name: buildTool
      value: generic
    - name: buildToolProjectDir
      value: ${APP_DIR_NAME}
    - name: symbolsYamlPath
      value: ${APP_DIR_NAME}/symbols.yaml
    - name: outputDir
      value: src/main/java
    - name: adataDir
      value: output
    - name: jzosJarPath
      value: /path/to/ibm.jzos.jar
    - name: packageName
      value: com.ibm.cics.example
    - name: artifactoryUser
      value: your-email@ibm.com
    - name: artifactoryPassword
      value: your-api-token
```

**Output**:
- `build/generated-sources/` - Java source files
- `build/classes/` - Compiled .class files
- `build/libs/${projectName}.jar` - Final JAR

### Option B: Gradle Mode (Full Build)

Generates Java, compiles, runs tests, creates JAR using Gradle.

```yaml
- task: SetupGradleEnvironment
  variables:
    - name: projectPath
      value: ${APP_DIR_NAME}
    - name: symbolsYamlPath
      value: ${APP_DIR_NAME}/symbols.yaml
    - name: packageName
      value: com.ibm.cics.example
    - name: jzosJarPath
      value: /path/to/ibm.jzos.jar
    - name: adataDir
      value: output

- task: RunBuildTool
  variables:
    - name: buildTool
      value: gradle
    - name: buildToolProjectDir
      value: ${APP_DIR_NAME}
    - name: buildToolTask
      value: build
```

**Output**: Standard Gradle build artifacts in `build/`

### Option C: Maven Mode (Future)

```yaml
- task: RunBuildTool
  variables:
    - name: buildTool
      value: maven
    - name: buildToolTask
      value: package
```

## Step 3: Run DBB Build

```bash
$DBB_HOME/bin/groovyz dbb-build.groovy --lifecycle full-with-java
```

## Configuration Reference

| Parameter | Required | Description | Example |
|-----------|----------|-------------|---------|
| `buildTool` | Yes | Build tool: `generic`, `gradle`, `maven` | `generic` |
| `symbolsYamlPath` | Yes | Path to symbols.yaml | `${APP_DIR_NAME}/symbols.yaml` |
| `jzosJarPath` | Yes | Path to functional JZOS JAR | `/u/user/ibm.jzos.jar` |
| `packageName` | Yes | Java package for generated classes | `com.ibm.cics.example` |
| `adataDir` | Yes | Directory containing ADATA files | `output` |
| `artifactoryUser` | Generic only | Artifactory username | `user@ibm.com` |
| `artifactoryPassword` | Generic only | Artifactory API token | `cmVmdGtu...` |
| `buildToolTask` | Gradle/Maven | Build task/goal | `build` or `package` |

## Quick Start Example

See `dbb-app-generic.yaml` for a complete working example.

## Troubleshooting

**Issue**: `symbols.yaml not found`  
**Fix**: Ensure path is relative to workspace or use absolute path

**Issue**: `JZOS JAR not found`  
**Fix**: Verify `jzosJarPath` points to functional JZOS JAR (not Maven Central stub)

**Issue**: `Artifactory authentication failed`  
**Fix**: Check credentials, use API token instead of password

**Issue**: `Output directory does not exist`  
**Fix**: Use generic mode - it creates directories automatically

## Generated Class Naming

Classes are named using PascalCase: `ADATA_NAME` + `SYMBOL_NAME`

Examples:
- `CREACC.adata` + `HOST-ACCOUNT-ROW` → `CreaccHostAccountRow.java`
- `INQCUST.adata` + `DFHCOMMAREA` → `InqcustDfhcommarea.java`

## Support

For issues or questions, refer to the main project documentation or contact the CICS team.