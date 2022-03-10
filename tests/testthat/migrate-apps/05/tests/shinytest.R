
library(shinytest)
expect_pass(testApp("../", suffix = shinytest2::platform_variant()))
