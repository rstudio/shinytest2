if (FALSE) {
  # TODO-barret; remove this section
  shinytest2::testing_support() # support for what?
  shinytest2::load_support()    # conflicting intent with `shiny::loadSupport()`
  shinytest2::app_support()     # what is support?
  shinytest2::app_env()         # and do what with it?
  shinytest2::test_env()        # verb "to test"
  shinytest2::load_env()        # what env?
  shinytest2::test_app_env()    # seems like a verb... "to test"

  shinytest2::load_app_env()    # verbose; current choice

  shinytest2::test_setup()      # verb "to test"
  shinytest2::app_setup()       # ??
  shinytest2::load_app_setup()  # cleaner, but more verbose
  shinytest2::load_setup()      # ambiguous
}

shinytest2::load_app_env()
