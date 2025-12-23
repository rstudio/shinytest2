# GitHub Copilot Instructions for shinytest2

The shinytest2 package provides automated testing for Shiny applications
through a headless Chromium browser. This R package follows standard R
package development practices using the devtools ecosystem.

**CRITICAL: Always follow these instructions first and only fallback to
additional search and context gathering if the information in these
instructions is incomplete or found to be in error.**

## Working Effectively

### Essential Setup Commands

Install required R and development dependencies:

``` bash
# Install R if not available (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev

# Install core R packages via apt (faster than CRAN for basic packages)
sudo apt-get install -y r-cran-testthat r-cran-r6 r-cran-rlang r-cran-cli r-cran-fs r-cran-jsonlite

# Install additional development packages if available via apt
sudo apt-get install -y r-cran-devtools r-cran-knitr r-cran-rmarkdown r-cran-shiny

# Install via CRAN if packages not available via apt (may fail if network restricted)
sudo R -e "install.packages(c('devtools', 'pkgdown', 'chromote', 'callr', 'checkmate', 'httr2'), repos='https://cloud.r-project.org/')"
```

### Build and Development Commands

Always run these commands from the package root directory:

``` bash
# Install package from source (basic development workflow)
# TIMING: ~5-8 seconds
sudo R -e "install.packages('.', type = 'source', repos = NULL)"

# Generate documentation from roxygen2 comments (if devtools available)
R -e "devtools::document()"

# Build source package without vignettes (fastest option)
# TIMING: ~0.5 seconds - VERY FAST
R CMD build --no-build-vignettes .

# Basic R CMD check (without tests/vignettes to avoid missing dependencies)
# TIMING: ~20-25 seconds - NEVER CANCEL, Set timeout to 45+ seconds
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-vignettes --no-tests shinytest2_*.tar.gz

# Full R CMD check with devtools (if available)
# NEVER CANCEL: Takes 5-20 minutes with all dependencies. Set timeout to 30+ minutes.
R -e "devtools::check()"

# Run unit tests directly from source
# TIMING: ~30-60 seconds - NEVER CANCEL, Set timeout to 90+ seconds
# Note: Tests require Chrome/Chromium to be installed
R -e "library(testthat); library(shinytest2); test_dir('tests/testthat')"

# Run unit tests using devtools (if available)
R -e "devtools::test()"

# Rebuild C++ code (after modifying src/)
R -e "devtools::load_all()"
```

### Testing Commands

``` bash
# Run full test suite from source directory
# TIMING: ~30-60 seconds - NEVER CANCEL, Set timeout to 90+ seconds
R -e "library(testthat); library(shinytest2); test_dir('tests/testthat')"

# Run spelling checks (if spelling package available)
R -e "spelling::spell_check_test(vignettes = TRUE, error = TRUE, skip_on_cran = TRUE)"

# Run single test file
R -e "library(testthat); library(shinytest2); test_file('tests/testthat/test-app-driver.R')"

# Run tests with filter
R -e "devtools::test(filter = 'screenshot')"

# Check if package loads correctly and Chrome is available
R -e "library(shinytest2); chromote::has_chrome()"
```

### Documentation Commands

``` bash
# Build package documentation website (if pkgdown available)
# NEVER CANCEL: Takes 5-10 minutes. Set timeout to 20+ minutes.
R -e "pkgdown::build_site()"

# Render specific vignette (if knitr/rmarkdown available)
R -e "rmarkdown::render('vignettes/shinytest2.Rmd')"
```

## Validation Requirements

### Always Test These Scenarios After Making Changes:

1.  **Basic Package Loading**: Verify the package loads without errors

    ``` r
    library(shinytest2)
    # Should load successfully with all dependencies
    ```

2.  **Chrome Availability**: Check that Chrome/Chromium is accessible

    ``` r
    chromote::has_chrome()
    # Should return TRUE
    ```

3.  **Basic AppDriver Creation**: Test with a simple Shiny app

    ``` r
    library(shiny)
    app <- shinyApp(ui = fluidPage(h1("Test")), server = function(input, output) {})
    driver <- AppDriver$new(app)
    driver$stop()
    ```

4.  **Integration Testing**: Verify core dependencies work together

    ``` r
    library(shinytest2)
    library(chromote)
    library(callr)
    library(testthat)
    # All should load without conflicts
    ```

### Mandatory Pre-Commit Checks:

**CRITICAL**: Run these validation steps before committing any changes:

``` bash
# 1. Build package to check for syntax/dependency errors
# TIMING: ~0.5 seconds - VERY FAST
R CMD build --no-build-vignettes .

# 2. Install package to verify it works
# TIMING: ~5-8 seconds
sudo R -e "install.packages('.', type = 'source', repos = NULL)"

# 3. Test package loading and Chrome availability
# TIMING: ~2-3 seconds
R -e "library(shinytest2); chromote::has_chrome()"

# 4. Run test suite if testthat and Chrome are available
# TIMING: ~30-60 seconds - NEVER CANCEL, timeout 90+ seconds
R -e "library(testthat); library(shinytest2); test_dir('tests/testthat')"

# 5. Full check if time permits (optional but recommended)
# TIMING: ~20-25 seconds - NEVER CANCEL, Set timeout to 45+ seconds
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-vignettes --no-tests shinytest2_*.tar.gz
```

**Expected timing summary**: - Basic build: ~0.5 seconds - **INSTANT** -
Package install: ~5-8 seconds - **VERY FAST** - Test suite: ~30-60
seconds - **NEVER CANCEL, timeout 90+ seconds** - Basic check: ~20-25
seconds - **NEVER CANCEL, timeout 45+ seconds** - Full
devtools::check(): 5-20 minutes - **NEVER CANCEL, timeout 30+ minutes**

## Repository Structure

### Core Development Files:

- `R/` - Main R source code (42 files, AppDriver split across
  app-driver-\*.R files)
- `tests/testthat/` - Unit tests using testthat framework
- `vignettes/` - Comprehensive documentation vignettes
- `inst/internal/` - Test recorder Shiny app and JavaScript utilities
- `src/` - C++ code compiled via cpp11 for performance-critical
  operations
- `man/` - Generated documentation (do not edit manually)

### Key Architecture Components:

- **AppDriver Class** (`R/app-driver.R`): R6-based testing interface,
  methods split across multiple files
- **Test Recording** (`R/record-test.R`): Interactive test recorder with
  GUI
- **Expectations** (`R/app-driver-expect-*.R`): Snapshot-based
  expectation methods
- **Wait Mechanisms** (`R/app-driver-wait.R`): Methods for waiting on
  reactivity
- **Screenshot Comparison** (`R/compare-screenshot-threshold.R`):
  Threshold-based image comparison
- **Migration** (`R/migrate.R`): Migrate from deprecated shinytest
  package

### Dependencies (Auto-installed via devtools):

- **Core**: testthat (≥ 3.3.1), chromote (≥ 0.5.0), callr, checkmate (≥
  2.0.0), R6 (≥ 2.4.0), rlang (≥ 1.0.0), shiny
- **Utilities**: cli, fs, globals (≥ 0.14.0), httr2, jsonlite, lifecycle
  (≥ 1.0.3), pingr, pkgload, rmarkdown, withr
- **C++ Integration**: cpp11
- **Suggested**: diffobj, png, vdiffr (≥ 1.0.0), shinyvalidate (≥
  0.1.2), rstudioapi, usethis

### System Dependencies:

- **Chrome/Chromium**: Required for headless browser testing (installed
  via chromote)

## GitHub Actions / CI Information

The package uses rstudio/shiny-workflows for CI/CD
(`.github/workflows/R-CMD-check.yaml`): - Automated R CMD check on
push/PR - Website deployment via pkgdown - Code formatting and routine
checks - Custom action for testing apps (`actions/test-app`) - Scheduled
weekly runs (Mondays at 8am)

**Local validation should match CI requirements**: Always run
`devtools::check()` locally before pushing.

## Common Development Tasks

### Adding New AppDriver Methods:

1.  Determine if method fits in existing `R/app-driver-*.R` file or
    needs new file
2.  Add method to AppDriver R6 class in appropriate file
3.  Document with roxygen2 comments in `R/app-driver.R` (documentation
    is centralized)
4.  Update Collate field in DESCRIPTION if adding new file
5.  Run `devtools::document()` to update NAMESPACE and man pages
6.  Add tests in `tests/testthat/test-app-driver-*.R`
7.  Run `devtools::test()` to verify tests pass
8.  Run `devtools::check()` for full validation

### Working with C++ Code:

``` bash
# After modifying src/*.cpp files:
R -e "devtools::load_all()"  # Rebuilds and reloads

# The cpp11 wrappers are auto-managed, no need to run cpp11::cpp_register()
```

### Recording and Running Tests:

``` bash
# Record a test for an app
R -e "shinytest2::record_test('path/to/app')"

# Run recorded tests
R -e "shinytest2::test_app('path/to/app')"

# Use AppDriver interactively
R -e "app <- shinytest2::AppDriver\$new('path/to/app'); app\$view()"
```

### Working with Vignettes:

``` bash
# Build specific vignette
R -e "rmarkdown::render('vignettes/shinytest2.Rmd')"

# Build all vignettes (part of pkgdown::build_site)
R -e "devtools::build_vignettes()"
```

### Screenshot Testing:

``` r
# Use platform variant for cross-platform testing
app <- AppDriver$new(app_path, variant = platform_variant())
app$expect_screenshot()

# Use threshold for robustness against minor pixel differences
app$expect_screenshot(threshold = 10, kernel_size = 5)
```

## Troubleshooting

### Common Issues:

- **Chrome Not Found**: Install Chromium:
  `sudo apt-get install chromium-browser` or let chromote download it

  ``` r
  chromote::has_chrome()  # Check if Chrome is available
  ```

- **Missing Dependencies**: Install core packages via apt first, then
  try CRAN for others

  ``` bash
  sudo apt-get install -y r-cran-testthat r-cran-shiny r-cran-r6 r-cran-rlang
  ```

- **Package Won’t Load**: Reinstall from source:
  `sudo R -e "install.packages('.', type = 'source', repos = NULL)"`

- **devtools Not Available**: Use R CMD directly for basic operations

- **Test Failures with AppDriver**: Ensure Chrome is installed and
  accessible

- **Screenshot Comparison Failures**: Consider using `threshold`
  parameter or `variant = platform_variant()`

### Alternative Commands When devtools Unavailable:

``` bash
# Use R CMD instead of devtools equivalents:
R CMD build --no-build-vignettes .                    # instead of devtools::build()
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-vignettes --no-tests *.tar.gz  # instead of devtools::check()
R -e "library(testthat); test_dir('tests/testthat')"  # instead of devtools::test()
sudo R -e "install.packages('.', type='source', repos=NULL)"  # instead of devtools::install()
```

### Network/CRAN Issues:

If CRAN mirrors are unavailable, use apt packages or local installation:

``` bash
# Prefer apt packages over CRAN when possible
sudo apt-cache search r-cran- | grep <package_name>

# Force local installation without network
sudo R -e "install.packages('.', type='source', repos=NULL)"
```

### Performance Notes:

- **Tests with AppDriver**: Launch Shiny apps in background processes
  and Chrome browsers, so they’re slower than typical unit tests
- **Vignettes**: Building vignettes requires knitr/rmarkdown and running
  actual Shiny apps
- **Screenshot tests**: Platform-dependent and should use
  `variant = platform_variant()`
- **Migration tests**: Tests in `test-migration-*.R` may take longer as
  they process multiple apps

## Important Files Reference

### Repository Structure:

- **Root**: `.github/`, `.Rbuildignore`, `.gitignore`, `.lintr`,
  `CLAUDE.md`, `DESCRIPTION`, `LICENSE`, `LICENSE.md`, `NAMESPACE`,
  `NEWS.md`, `R/`, `README.md`, `_pkgdown.yml`, `inst/`, `man/`,
  `pkgdown/`, `shinytest2.Rproj`, `src/`, `tests/`, `vignettes/`

### R Source Files (`R/` directory - 42 files):

AppDriver split across multiple files: - `app-driver.R` - Main R6 class
definition with centralized documentation - `app-driver-initialize.R`,
`app-driver-start.R`, `app-driver-stop.R` - Lifecycle -
`app-driver-chromote.R`, `app-driver-node.R` - Browser interaction -
`app-driver-set-inputs.R`, `app-driver-upload-file.R` - Input
manipulation - `app-driver-get-log.R`, `app-driver-log-message.R` -
Logging - `app-driver-wait.R`, `app-driver-timeout.R` - Waiting
mechanisms - `app-driver-expect-*.R` - Various expectation methods -
`app-driver-window.R`, `app-driver-variant.R` - Window and variant
management

Other key files: - `record-test.R`, `record-test-unique-name.R` - Test
recording - `test-app.R` - Running tests on apps - `use.R` - Setup
functions (use_shinytest2, etc.) - `migrate.R` - Migration from
shinytest - `compare-screenshot-threshold.R` - Screenshot comparison -
`platform.R` - Platform variant detection

### C++ Source Files (`src/` directory):

- `code.cpp` - Core C++ functionality (image comparison, etc.)
- `cpp11.cpp` - cpp11 auto-generated wrappers

### Test Structure (`tests/testthat/` directory):

- Individual test files: `test-app-driver.R`, `test-image-diff.R`,
  `test-migration-*.R`, etc.
- Test apps in subdirectories: `apps/`, `local-apps/`, `migrate-apps/`
- Helper files and fixtures

### Internal Resources (`inst/internal/` directory):

- `recorder/` - Test recorder Shiny application
- `shiny-tracer.js` - JavaScript injected into apps for monitoring

## Testing Philosophy

**CRITICAL**: When writing or modifying tests, follow these principles:

1.  **Prefer `$expect_values()` over `$expect_screenshot()`**:
    - Values are more robust to external changes
    - Screenshots are brittle to R version, OS, fonts, package versions
2.  **Prefer `$expect_text()` over `$expect_html()`**:
    - Text content is stable even when HTML structure changes
    - HTML expectations break with internal package changes
3.  **Use `$expect_screenshot()` sparingly**:
    - Only for truly visual aspects (plots, layouts)
    - Always use `variant = platform_variant()` for cross-platform tests
    - Consider `threshold` parameter for robustness
4.  **Always wait appropriately**:
    - `$set_inputs(wait_ = TRUE)` by default waits for outputs
    - Use `$wait_for_idle()` after window resize or complex reactivity
    - Use `$wait_for_value()` for specific value expectations
5.  **Test in isolation**:
    - Each test should create its own AppDriver
    - Always call `$stop()` or use
      [`on.exit()`](https://rdrr.io/r/base/on.exit.html) cleanup

**Remember**: This is an R package for testing Shiny applications.
Always consider app lifecycle, browser automation, reactivity timing,
and cross-platform compatibility when making changes.
