# shinytest2 (development version)

* `test_app()` now inherits the existing test reporter when testing multiple apps within a package test file. This allows for a seamless, single reporter output instead of nested reporters being displayed. (#192)

* Fix set of bugs found by @daattali including test files should be opened in the IDE after recording and test and replace missing images in the website (#199)

* Provide example workflows on how to use `rstudio/shinytest2/actions/test-app` GHA action (#217)

* Make `{globals}` an `Imports` package, instead of a `Suggests` package (#223)

* Add support for _not_ recording the screen size when recording a test (#223)

* When setting a date time slider value, it can now handle array inputs properly. When recording a date time slider value, numeric values will not be recorded as milliseconds instead of seconds since epoch. (#223)

* The recording browser window is now closed when either the "Save test and exit" or "Exit" buttons are clicked. (@daattali, #202)

* When creating a test setup file for `{shinytest2}`, use the file path `tests/testthat/setup-shinytest2.R` instead of `tests/testthat/setup.R` to provide some quick context (#224)

* When testing and `{chromote}` can not be started, the test will be skipped. When testing on windows in CI, `{chromote}` will get an attempt to be started (#225)

* Remove trailing comma causing render bug in recorder app (@mehrnoushmalek, #239)

* Update {lintr} lints to v3 (#240)

* Fixed GHA links for `rstudio/shinytest2/actions/test-app@v1` (#249)

* Fix documentation on on `AppDriver`'s `delay` parameter being in **seconds**, not *milli*seconds (#255)


# shinytest2 0.1.1

* Update docs for CRAN (#253)


# shinytest2 0.1.0

* Initial release of package
