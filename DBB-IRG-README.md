# DBB + IBM Record Generator (IRG) Integration

Quick guide to configure DBB builds with automatic Java class generation from COBOL ADATA files.

## Prerequisites

- IBM DBB 3.0+
- IBM Record Generator 3.0+
- Functional IBM JZOS JAR (not Maven Central stub)
- Gradle 8.14+ with wrapper
- IBM Artifactory credentials

## Configuration Files

### 1. `build.gradle` - Gradle Build Configuration

```gradle
plugins {
    id 'java'
    id 'com.ibm.recgen.gradle' version '1.0.0'
}

repositories {
    mavenCentral()
    maven {
        url 'https://na.artifactory.swg-devops.com/artifactory/ce-maven-release-local'
        credentials {
            username = project.findProperty('artifactoryUser') ?: System.getenv('ARTIFACTORY_USER')
            password = project.findProperty('artifactoryPassword') ?: System.getenv('ARTIFACTORY_PASSWORD')
        }
    }
}

// Get parameters from DBB
def adataDir = project.findProperty('adataDir') ?: 'output'
def jzosJarPath = project.findProperty('jzosJarPath') ?: '/path/to/functional/ibm.jzos.jar'

// Configure Record Generator
recordGenerator {
    packageName = 'com.ibm.cics.example'
    jzosJar = file(jzosJarPath)
    adataFile = "${adataDir}/PROGRAM.adata"
    symbols = ['DFHCOMMAREA', 'WS-STRUCTURE-NAME']
}

// Optional: Customize JAR manifest
jar {
    manifest {
        attributes(
            'Implementation-Title': project.name,
            'Implementation-Version': project.version
        )
    }
}
```

### 2. `dbb-app.yaml` - DBB Application Configuration

```yaml
tasks:
  - task: Cobol
    variables:
      # COBOL compilation with ADATA generation
      - name: compilerOptions
        append:
          - value: ADATA
            condition: 1 == 1
      
      # Allow compiler warnings (RC=4)
      - name: maxRC
        value: 4
      
      # ADATA output configuration (CRITICAL: use temporary dataset)
      - name: additionalCompileLibraries
        value: 
          - { name: "SYSADATA", dsn: "&&ADATA", options: "new delete" }
      
      # Copy ADATA to PDS after compilation
      - name: additionalOutputDatasets
        value:
          - { ddname: "SYSADATA", deployType: "ADATA", 
              output: "${HLQ}.ADATA(${MEMBER})" }
      
      # Create ADATA PDS
      - name: additionalCreationDatasets
        value:
          - name: ${HLQ}.ADATA
            options: cyl space(50,10) dir(50) dsorg(PO) recfm(V,B) blksize(32760) lrecl(1020)

  # Copy ADATA files from MVS to USS
  - task: StageAdata
    variables:
      - name: hlq
        value: ${HLQ}
      - name: adataDataset
        value: ${HLQ}.ADATA
      - name: adataDir
        value: output

  # Execute Gradle to generate Java classes
  - task: RunBuildTool
    variables:
      - name: buildTool
        value: gradle
      - name: buildToolProjectDir
        value: ${APP_DIR_NAME}
      - name: buildToolTask
        value: build
      - name: adataDir
        value: output
      - name: jzosJarPath
        value: /path/to/functional/ibm.jzos.jar
      - name: packageName
        value: com.ibm.cics.example
      - name: artifactoryUser
        value: your.email@ibm.com
      - name: artifactoryPassword
        value: ${ARTIFACTORY_PASSWORD}
```

## Key Configuration Points

### ADATA Generation (Critical)

**Problem**: Direct PDS member allocation fails with RECFM=VB  
**Solution**: Use temporary dataset (&&ADATA) then copy to PDS

```yaml
# ✅ CORRECT - Temporary dataset
- { name: "SYSADATA", dsn: "&&ADATA", options: "new delete" }

# ❌ WRONG - Direct PDS member fails
- { name: "SYSADATA", dsn: "${HLQ}.ADATA(${MEMBER})", options: "SHR" }
```

### BPXWDYN Syntax

**Problem**: Comma-separated options cause RC=-24  
**Solution**: Use space-separated options

```yaml
# ✅ CORRECT
options: "new delete"

# ❌ WRONG
options: "NEW,DELETE"
```

### Functional JZOS JAR

**Critical**: Maven Central JZOS is a compilation stub only. You MUST provide functional JZOS JAR:

```yaml
jzosJarPath: /u/userid/ibm-jzos/ibm.jzos.jar  # From z/OS SDK or CICS SDK
```

## Build Execution

```bash
# Run DBB build with Java generation
dbb build full-with-java --hlq YOUR.HLQ

# Verify generated JAR
jar -tf build/libs/your-project-1.0.jar | grep '\.class$'
```

## Output Structure

```
build/
├── generated-sources/recgen/
│   └── com/ibm/cics/example/
│       ├── Dfhcommarea.java
│       └── WsStructureName.java
├── classes/java/main/
│   └── com/ibm/cics/example/
│       ├── Dfhcommarea.class
│       └── WsStructureName.class
└── libs/
    └── your-project-1.0.jar  ← Compiled classes
```

## Troubleshooting

| Issue | Solution |
|-------|----------|
| RC=14,905,344 | Use temporary dataset (&&ADATA), not direct PDS member |
| BPXWDYN RC=-24 | Use space-separated options: `"new delete"` |
| "Non-functional JZOS jar" | Provide functional JZOS JAR via `jzosJarPath` |
| No JAR created | Change `buildToolTask` from `generateRecordHelperClasses` to `build` |
| Artifactory auth failed | Set credentials in gradle.properties or environment variables |

## Dependencies

The Record Generator plugin automatically injects these dependencies:

- `com.ibm.jzos:ibm.jzos:4.0.0.0` (Maven Central stub - for compilation only)
- `com.ibm.cics:ibm-recgen:3.0.0` (IBM Artifactory - for generation)

## References

- [IBM Record Generator Documentation](https://www.ibm.com/docs/en/cics-ts/latest)
- [DBB Documentation](https://www.ibm.com/docs/en/dbb)
- [Gradle Record Generator Plugin](https://github.ibm.com/etsi/gradle-recordgenerator)