#!/bin/bash
Rscript -e 'rmarkdown::render("actions/test-app/README.Rmd")'
git add -A actions/test-app/README.md && \
  git commit -m '`rmarkdown::render("actions/test-app/README.Rmd")` (GitHub Actions)' || \
  echo "No changes found in actions/test-app/README.md"
