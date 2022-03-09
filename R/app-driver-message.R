app_warn <- function(self, private, ...) {
  ckm8_assert_app_driver(self, private)
  rlang::warn(..., app = self)
}
app_inform <- function(self, private, ...) {
  ckm8_assert_app_driver(self, private)
  rlang::inform(..., app = self)
}
app_abort <- function(self, private, ...) {
  ckm8_assert_app_driver(self, private)
  rlang::abort(..., app = self, call = rlang::caller_env())
}
