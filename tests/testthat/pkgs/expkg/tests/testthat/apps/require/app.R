# print(library)
# print(require)
# print(`::`)
# print(`:::`)

## TODO-future: Be able to check that `::` / `:::` were overridden
# testthat::expect_error(
#   expkg::expkg_exported_value,
#   "Attempted to access local package"
# )
# testthat::expect_error(
#   expkg:::expkg_exported_value,
#   "Attempted to access local package"
# )

testthat::expect_false(exists("expkg_exported_value"))

testthat::expect_true(require(expkg)) # This also attaches the package

testthat::expect_true(is.function(expkg_shiny_app))
testthat::expect_equal(expkg_exported_value, "expkg exported value")
testthat::expect_false(exists("expkg_internal_value"))
testthat::expect_equal(expkg:::expkg_internal_value, "expkg internal value")


expkg_shiny_app()
