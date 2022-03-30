# test-app

[![RStudio community](https://img.shields.io/badge/community-github--actions-blue?style=social&logo=rstudio&logoColor=75AADB)](https://community.rstudio.com/new-topic?category=Package%20development&tags=github-actions)

This action tests a Shiny application (or Shiny-based document) using the [`{shinytest2}`](https://github.com/rstudio/shinytest2) package.

# Usage

Inputs available:
- app-dir - default `"."`. Directory of Shiny application (or Shiny-based document) to test

Basic:
```yaml
- uses: actions/checkout@v2
- uses: r-lib/actions/setup-pandoc@v2
- uses: r-lib/actions/setup-r@v2
- uses: r-lib/actions/setup-r-dependencies@v2

- uses: rstudio/shinytest2/actions/test-app@v1
  with:
    app-dir: "."
```

`{renv}`:
```yaml
- uses: actions/checkout@v2
- uses: r-lib/actions/setup-pandoc@v2
- uses: r-lib/actions/setup-r@v2
- uses: r-lib/actions/setup-renv@v2

- uses: rstudio/shinytest2/actions/test-app@v1
  with:
    app-dir: "."
```

# License

The scripts and documentation in this project are released under the [MIT License](LICENSE)

# Contributions

Contributions are welcome!
