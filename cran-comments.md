## CRAN comments

#### 2026-02-16

> Dear maintainer,
>
> Please see the problems shown on
> <https://cran.r-project.org/web/checks/check_results_shinytest2.html>.
>
> Please correct before 2026-03-02 to safely retain your package on CRAN.
>
> The CRAN Team

---------

> Chrome is not a SytemRequiremnts and in any case the menual says
> externel programs should be used condiitonally.
>
> --
> Brian D. Ripley

--------

> ```
> Check Details
> Version: 0.5.0
> Check: tests
> Result: ERROR
>     Running ‘testthat.R’ [23s/97s]
>   Running the tests in ‘tests/testthat.R’ failed.
>   Complete output:
>     > library(testthat)
>     > library(shinytest2)
>     >
>     > test_check("shinytest2")
>     shinytest requires PhantomJS to record and run tests.
>     * To install it, run shinytest::installDependencies()
>     * If it is installed, please check it is available on the PATH
>     Saving _problems/test-not-testing-31.R
>     [ FAIL 1 | WARN 0 | SKIP 68 | PASS 293 ]
>
>     ══ Skipped tests (68) ══════════════════════════════════════════════════════════
>     • App test folder has been ignored (25): 'test-app-files.R:2:3',
>       'test-app-files.R:16:3', 'test-app-files.R:27:3',
>       'test-app-hello-click.R:2:3', 'test-app-hello-execute-js.R:2:3',
>       'test-app-hello-init-args.R:2:3', 'test-app-hello-variant.R:3:3',
>       'test-app-hello.R:13:3', 'test-app-hello.R:37:3', 'test-app-quarto.R:2:3',
>       'test-app-rmd.R:2:3', 'test-app-rmd.R:17:3', 'test-app-rmd.R:32:3',
>       'test-app-rprofile.R:2:3', 'test-app-wait-get-value.R:2:3',
>       'test-app-wait-get-value.R:55:3', 'test-app-wait-get-value.R:66:3',
>       'test-app-wait.R:3:3', 'test-app-wait.R:14:3', 'test-app-widgets.R:4:3',
>       'test-test-app.R:2:3', 'test-test-app.R:16:3', 'test-test-app.R:33:3',
>       'test-test-app.R:47:3', 'test-test-app.R:59:3'
>     • Local app test folder has been ignored (1): 'test-local-apps.R:1:1'
>     • On CRAN (38): 'test-app-bookmark.R:25:3', 'test-app-download.R:73:3',
>       'test-app-download.R:106:3', 'test-app-duplicate-ids.R:15:3',
>       'test-app-duplicate-ids.R:38:3', 'test-app-duplicate-ids.R:61:3',
>       'test-app-duplicate-ids.R:86:3', 'test-app-eval-js.R:24:3',
>       'test-app-eval-js.R:45:3', 'test-app-expect-file-transform.R:26:3',
>       'test-app-export.R:35:3', 'test-app-export.R:66:3',
>       'test-app-export.R:135:3', 'test-app-image.R:39:3', 'test-app-image.R:64:3',
>       'test-app-image.R:76:3', 'test-app-image.R:89:3', 'test-app-image.R:110:3',
>       'test-app-image.R:122:3', 'test-app-logs.R:136:3', 'test-app-logs.R:181:3',
>       'test-app-plotly.R:33:3', 'test-app-screenshot-size.R:5:3',
>       'test-app-screenshot-size.R:117:3', 'test-app-shiny.R:27:3',
>       'test-app-shiny.R:39:3', 'test-app-stop.R:17:3', 'test-app-stop.R:26:3',
>       'test-app-timeout.R:12:1', 'test-app-update.R:43:3',
>       'test-app-update.R:56:3', 'test-app-upload.R:24:3',
>       'test-migration-migrate.R:3:1', 'test-not-testing.R:33:3',
>       'test-save-app.R:62:3', 'test-save-app.R:82:3', 'test-shinytest2.R:1:1',
>       'test-spelling.R:2:3'
>     • Only run on CI (3): 'test-save-app.R:6:3', 'test-save-app.R:24:3',
>       'test-save-app.R:38:3'
>     • Test packages folders have been ignored (1): 'test-pkgs.R:3:3'
>
>     ══ Failed tests ════════════════════════════════════════════════════════════════
>     ── Failure ('test-not-testing.R:31:3'): Running an app not in testing mode has 404 handled when getting values ──
>     Expected `!is.null(app_url)` to be TRUE.
>     Differences:
>     `actual`:   FALSE
>     `expected`: TRUE
>
>
>     [ FAIL 1 | WARN 0 | SKIP 68 | PASS 293 ]
>     Deleting unused snapshots: 'app-bookmark/001.json', 'app-bookmark/001_.png',
>     'app-download/001-download-link.txt', 'app-download/002-download-button.txt',
>     'app-download/003-download-link.csv', 'app-download/004-download-button.csv',
>     'app-download/005-bear.png', 'app-download/006-bear.png',
>     'app-download/007-my_custom_name.txt',
>     'app-expect-file-transform/001-download-button.txt',
>     'app-expect-file-transform/002.json', 'app-expect-file-transform/002_.png',
>     'app-export/cars-points-10-export.svg', 'app-export/cars-points-10.svg',
>     'app-export/cars-points-20-export.svg', 'app-export/cars-points-20.svg',
>     'app-files/kgs-001.json', 'app-files/kgs-001_.png', …,
>     'mac/app-hello/003_.png', and 'mac/app-hello/manual-screenshot.png'
>     Error:
>     ! Test failures.
>     `google-chrome`, `chromium-browser` and `chrome` were not found. Try setting the `CHROMOTE_CHROME` environment variable to the executable of a Chromium-based browser, such as Google Chrome, Chromium or Brave or adding one of these executables to your PATH.
>     Error in initialize(...) : Invalid path to Chrome
>     Execution halted
> ```


#### 2026-02-25

Fixed broken test from CRAN request.

- Barret


## R CMD check results

0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 76 reverse dependencies (69 from CRAN + 7 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 2 packages

Issues with CRAN packages are summarised below.

### Failed to check

* blockr.dag    (NA)
* shiny.destroy (NA)
