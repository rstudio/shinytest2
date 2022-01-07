

app_is_missing_variant <- function(self, private) {
  ckm8_assert_app_driver(self, private)
  is_missing_value(private$variant)
}


# Return NULL if the variant is missing
app_get_variant <- function(self, private) {
  ckm8_assert_app_driver(self, private)

  variant <- private$variant
  if (is_missing_value(variant)) {
    NULL
  } else {
    variant
  }
}


app_set_variant <- function(self, private, variant) {
  ckm8_assert_app_driver(self, private)

  if (is_missing_value(variant)) {
    variant <- missing_value()
  } else {
    if (identical(variant, FALSE)) {
      variant <- NULL
    }
  }

  private$variant <- variant
  variant
}
