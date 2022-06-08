# shinytest2 (development version)

* `test_app()` now inherits the existing test reporter when testing multiple apps within a package test file. This allows for a seamless, single reporter output instead of nested reporters being displayed. (#192)

* Fix set of bugs found by @daattali including test files should be opened in the IDE after recording and test and replace missing images in the website (#199)

* Provide example workflows on how to use `rstudio/shinytest2/actions/test-app` GHA action (#217)

* Make `{globals}` an `Imports` package, instead of a `Suggests` package (#223)

* Add support for _not_ recording the screen size when recording a test (#223)

* When setting a date time slider value, it can now handle array inputs properly. When recording a date time slider value, numeric values will not be recorded as milliseconds instead of seconds since epoch. (#223)

* The recording browser window is now closed when either the "Save test and exit" or "Exit" buttons are clicked. (@daattali, #202)


# shinytest2 0.1.0

* Initial release of package
