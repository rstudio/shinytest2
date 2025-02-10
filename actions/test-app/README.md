
<!-- README.md is generated from README.Rmd. Please edit that file -->

# test-app

[![RStudio
community](https://img.shields.io/badge/community-github--actions-blue?style=social&logo=rstudio&logoColor=75AADB)](https://community.rstudio.com/new-topic?category=Package%20development&tags=github-actions)

This action tests a Shiny application (or Shiny-based document) using
the [`{shinytest2}`](https://github.com/rstudio/shinytest2) package. The
`${{ inputs.app-dir}}/tests/testthat/_snaps` directory will be uploaded
if enabled (default).

# Usage

Inputs available:

- `app-dir` - default `"."`. Directory of Shiny application (or
  Shiny-based document) to test
- `upload-snapshots` - default `true`. Whether to upload all testthat
  snapshots as an artifact.

Typical (single app testing) GHA step usage:

``` yaml
- uses: rstudio/shinytest2/actions/test-app@actions/v1
  with:
    app-dir: |
      dir/to/app
```

Multiple Apps can be tested by supplying multiple directories to
`app-dir` separated by a newline:

``` yaml
- uses: rstudio/shinytest2/actions/test-app@actions/v1
  with:
    app-dir: |
      dir/to/app1
      dir/to/app2
```

# Example workflows

These workflows are a good building block / starting point for testing
your Shiny applications. You may need to alter what is actually executed
to fit your needs.

- [`test-app-description`](#dependencies-in-description-file) - An
  example CI workflow to test your Shiny application given you are using
  a `DESCRIPTION` file to state your dependencies.
- [`test-app-renv`](#dependencies-managed-by-renv) - An example CI
  workflow to test your Shiny application given you are using `{renv}`
  to manage your dependencies.
- [`test-app-package`](#app-within-package-structure) - An example CI
  workflow to test a Shiny application within your local R package. It
  is recommended to use `app-dir` input to set the location of your
  Shiny application.

## Dependencies in DESCRIPTION file

Install workflow using:

``` r
usethis::use_github_action(
  url = "https://github.com/rstudio/shinytest2/raw/main/actions/test-app/example-test-app-descrption.yaml",
  save_as = "test-app-description.yaml"
)
```

Workflow contents:

``` yaml
# Workflow derived from https://github.com/rstudio/shinytest2/tree/main/actions/test-app/example-test-app-description.yaml
# Need help debugging build failures? Start at https://github.com/r-lib/actions#where-to-find-help
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

name: Test app w/ DESCRIPTION

jobs:
  test-app:
    runs-on: ${{ matrix.config.os }}

    name: ${{ matrix.config.os }} (${{ matrix.config.r }})

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: ubuntu-latest, r: release}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v2

      - uses: r-lib/actions/setup-pandoc@v2

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages:
            shinytest2

      - uses: rstudio/shinytest2/actions/test-app@actions/v1
        with:
          app-dir: "."
```

## Dependencies managed by `{renv}`

Install workflow using:

``` r
usethis::use_github_action(
  url = "https://github.com/rstudio/shinytest2/raw/main/actions/test-app/example-test-app-renv.yaml",
  save_as = "test-app-renv.yaml"
)
```

Workflow contents:

``` yaml
# Workflow derived from https://github.com/rstudio/shinytest2/tree/main/actions/test-app/example-test-app-renv.yaml
# Need help debugging build failures? Start at https://github.com/r-lib/actions#where-to-find-help
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

name: Test app w/ {renv}

jobs:
  test-app:
    runs-on: ${{ matrix.config.os }}

    name: ${{ matrix.config.os }} (${{ matrix.config.r }})

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: ubuntu-latest, r: release}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v2

      - uses: r-lib/actions/setup-pandoc@v2

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-renv@v2

      - uses: rstudio/shinytest2/actions/test-app@actions/v1
        with:
          app-dir: "."
```

## App within Package structure

If you are using a local package and want to test an application within
the same repo, this may be a good option. However, it is recommended to
try to execute these tests as a part of your standard `{testthat}`
testting.

Install workflow using:

``` r
usethis::use_github_action(
  url = "https://github.com/rstudio/shinytest2/raw/main/actions/test-app/example-test-app-package.yaml",
  save_as = "test-app-package.yaml"
)
```

Workflow contents:

``` yaml
# Workflow derived from https://github.com/rstudio/shinytest2/tree/main/actions/test-app/example-test-app-package.yaml
# Need help debugging build failures? Start at https://github.com/r-lib/actions#where-to-find-help
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

name: Test package app

jobs:
  test-package-app:
    runs-on: ${{ matrix.config.os }}

    name: ${{ matrix.config.os }} (${{ matrix.config.r }})

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: ubuntu-latest, r: release}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v2

      - uses: r-lib/actions/setup-pandoc@v2

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages:
            local::.
            shinytest2

      - uses: rstudio/shinytest2/actions/test-app@actions/v1
        with:
          app-dir: "."
```

# Development

The `test-app` action uses a sliding git tag that follows the pattern
`actions/vX`, e.g. `actions/v1`. For historical reasons, we also support
the `v1` tag, but future versions will only be available under the
`actions/vX` tag.

The `test-app` action release cycle is not dependent on the shinytest2
package cycle. When changes are made to the `test-app` action, you
should force-update the current sliding tag version:

``` bash
git tag -f v1            # update historical v1 tag
git tag -f actions/v1    # update sliding tag
git push --tags --force  # push tag to github
```

# License

The scripts and documentation in this project are released under the
[MIT License](LICENSE)

# Contributions

Contributions are welcome!
