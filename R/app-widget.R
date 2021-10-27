#' A Shiny Widget
#'
#' @description
#' A `Widget2` object represents a Shiny input or output control, and provides
#' methods for finer grained interaction.
#'
#' @importFrom R6 R6Class
Widget2 <- R6Class( # nolint
  "Widget2",
  private = list(
    name = NULL,     # name in shiny
    # element = NULL,  # HTML element with name as id
    nodeId = NULL,   # nodeId of the element
    type = NULL,     # e.g. selectInput
    iotype = NULL,    # "input" or "output"
    chromote_session = NULL
  ),
  public = list(
    get_chromote_session = function() {
      private$chromote_session
    },

    #' @description Create new `Widget2`
    #' @param name Name of a Shiny widget.
    #' @param nodeId Node Id of an Element from the `chromote` session.
    #' @param type Widget2 type
    #' @param iotype Input/output type.
    #' @param chromote Chromote Session
    initialize = function(name, node_id, type, iotype = c("input", "output"), chromote_session) {
      iotype <- match.arg(iotype)

      private$name <- name
      private$nodeId <- node_id # nolint
      private$type <- type
      private$iotype <- iotype
      private$chromote_session <- chromote_session
      invisible(self)
    },

    # #' @description Control id (i.e. `inputId` or `outputId` that control
    # #'   was created with).
    # getName = function() private$name,
    # #' @description Underlying `nodeId` to be used within the Chromium session
    # getNodeId = function() private$nodeId,
    # #' @description Retrieve the underlying HTML for a widget
    # getHtml = function() {
    #   chromote_call_js_on_node(self$get_chromote_session(), private$nodeId, "function() { return this.outerHTML; }")$result$value
    # },
    # #' @description Widget2 type, e.g. `textInput`, `selectInput`.
    # getType = function() private$type,
    # #' @description Is this an input or output control?
    # getIoType = function() private$iotype,
    # #' @description Is this an input control?
    # isInput = function() private$iotype == "input",
    # #' @description Is this an output control?
    # isOutput = function() private$iotype == "output",

    #' @description Get current value of control.
    getValue = function() {
      "!DEBUG widget2_getValue `private$name`"

      if (private$iotype == "input") {
        res <- chromote_call_js_on_node(
          self$get_chromote_session(),
          private$nodeId,
          "function() {
            var el = $(this);
            return el.data('shinyInputBinding').getValue(el[0]);
          }"
        )$result$value
      } else {
        res <- switch(private$type,
          htmlOutput = {
            chromote_call_js_on_node(
              self$get_chromote_session(),
              private$nodeId,
              "function() { return this.innerHTML; }"
            )$result$value
          },
          verbatimTextOutput = , # nolint
          textOutput = {
            chromote_call_js_on_node(
              self$get_chromote_session(),
              private$nodeId,
              "function() { return this.textContent; }"
            )$result$value
          },
          abort(paste0("getValue is not implemented for ", private$type))
        )
      }

      # Post-process, if needed
      res <- switch(private$type,
        checkboxGroupInput = as.character(unlist(res)),
        dateInput = as.Date(res),
        dateRangeInput = as.Date(unlist(res)),
        sliderInput = as.numeric(unlist(res)),
        res
      )

      res
    },

    #' @description Set value of control.
    #' @param value Value to set for the widget.
    setValue = function(value) {
      "!DEBUG widget2_setValue `private$name`"
      if (private$iotype == "output") {
        abort("Cannot set values of output widgets")
      }

      # Preprocess value
      value <- switch(private$type,
        dateRangeInput = list(start = value[1], end = value[2]),
        radioButtons = if (!is.null(value)) as.character(value),
        value
      )

      set_value_script <- "
        function() {
          var el = $(this);
          var val = arguments[0];
          el.data('shinyInputBinding').setValue(el[0], val);
          el.trigger('change');
        }
      "
      chromote_call_js_on_node(self$get_chromote_session(), private$nodeId, set_value_script, arguments = list(value))

      invisible(self)
    },

    #' @description Sends a jQuery click to the element.
    #' @return self, invisibly.
    click = function() {
      click_script <- "
        function() {
          var el = $(this);
          el.click();
        }
      "
      chromote_call_js_on_node(self$get_chromote_session(), private$nodeId, click_script)
      invisible(self)
    },

    # # TODO-future; Not for this release. Comment for now.
    # #' @description Send specified key presses to control.
    # #' @param keys Keys to send to the widget or the app.
    # # ' See [webdriver::key] for how to specific special keys.
    # sendKeys = function(keys) {
    #   "!DEBUG widget2_sendKeys `private$name`"
    #   private$element$sendKeys(keys)
    # },

    # # TODO-future; Not for this release. Comment for now.
    # #' @description Lists the tab names of a [shiny::tabsetPanel()].
    # #'  It fails for other types of widgets.
    # listTabs = function() {
    #   if (private$type != "tabsetPanel") {
    #     abort("'listTabs' only works for 'tabsetPanel' Widgets")
    #   }
    #   tabs <- private$element$find_elements("li a")
    #   vapply(tabs, function(t) t$getData("value"), "")
    # },

    #' @description Upload a file to a [shiny::fileInput()].
    #'  It fails for other types of widgets.
    #' @param filename Path to file to upload
    upload_file = function(filename) {
      self$get_chromote_session()$DOM$setFileInputFiles(files = list(fs::path_abs(filename)), nodeId = private$nodeId)
    }
  )
)









#' @description
#' Finds the a Shiny input or output control.
#' @return A [Widget2].
#' @importFrom rlang %|%
#' @include shiny-driver.R
ShinyDriver2$set("public", "findWidget", function(name, iotype = c("auto", "input", "output")) {
  "!DEBUG finding a Widget2 `name` (`iotype`)"

  iotype <- match.arg(iotype)

  css <- if (iotype == "auto") {
    paste0("#", name)

  } else if (iotype == "input") {
    paste0("#", name, ".shiny-bound-input")

  } else if (iotype == "output") {
    paste0("#", name, ".shiny-bound-output")
  }

  el_node_ids <- chromote_find_elements(self$get_chromote_session(), css)

  if (length(el_node_ids) == 0) {
    abort(paste0(
      "Cannot find ",
      if (iotype != "auto") paste0(iotype, " "),
      "widget ", name
    ))

  } else if (length(el_node_ids) > 1) {
    warning(
      "Multiple ",
      if (iotype != "auto") paste0(iotype, " "),
      "widgets with id ", name
    )
  }

  node_id <- el_node_ids[[1]]
  js_ret <- chromote_call_js_on_node(self$get_chromote_session(), node_id, "
    function() {
      var el = $(this);
      if (el.data('shinyInputBinding') !== undefined) {
        return ['input', el.data('shinyInputBinding').name];
      } else {
        var name = el.data('shinyOutputBinding').binding.name;
        if (name == 'shiny.textOutput' && el[0].tagName == 'PRE') {
          return ['output', 'shiny.verbatimTextOutput'];
        } else {
          return ['output', name];
        }
      }
    }
  ")
  type <- js_ret$result$value

  ## We could use the JS names as well, but it is maybe better to use
  ## the names the users encounter with in the Shiny R docs
  widget_names <- c(
    "shiny.actionButtonInput"  = "actionButton",
    "shiny.checkboxInput"      = "checkboxInput",
    "shiny.checkboxGroupInput" = "checkboxGroupInput",
    "shiny.dateInput"          = "dateInput",
    "shiny.dateRangeInput"     = "dateRangeInput",
    "shiny.fileInputBinding"   = "fileInput",
    "shiny.numberInput"        = "numericInput",
    "shiny.radioInput"         = "radioButtons",
    "shiny.selectInput"        = "selectInput",
    "shiny.sliderInput"        = "sliderInput",
    "shiny.textInput"          = "textInput",
    "shiny.passwordInput"      = "passwordInput",
    "shiny.bootstrapTabInput"  = "tabsetPanel",

    "shiny.textOutput"         = "textOutput",
    "shiny.verbatimTextOutput" = "verbatimTextOutput",
    "shiny.htmlOutput"         = "htmlOutput",
    "shiny.imageOutput"        = "plotOutput",
    "datatables"               = "tableOutput"
  )

  Widget2$new(
    name = name,
    node_id = el_node_ids[[1]],
    type = unname(widget_names[type[[2]]] %|% type[[2]]),
    iotype = type[[1]],
    chromote_session = self$get_chromote_session()
  )
})




#' @description
#' Finds a widget and queries its value. See the `getValue()` method of
#' [Widget2] for more details.
#' @include shiny-driver.R
ShinyDriver2$set("public", "getValue", function(name, iotype = c("auto", "input", "output")) {
  "!DEBUG ShinyDriver2$getValue `name` (`iotype`)"
  self$findWidget(name, iotype)$getValue()
})

#' @description
#' Finds a widget and sets its value. It's a shortcut for `$findWidget()`
#' plus `setValue()`; see the [Widget2] documentation for more details.
#'
#' @param value New value.
#' @return Self, invisibly.
#' @include shiny-driver.R
ShinyDriver2$set("public", "setValue", function(name, value, iotype = c("auto", "input", "output")) {
  "!DEBUG ShinyDriver2$setValue `name`"
  self$findWidget(name, iotype)$setValue(value)
  invisible(self)
})

#' @description
#' Find a widget and click it. It's a shortcut for `$findWidget()`
#' plus `$click()`; see the [Widget2] documentation for more details.
#' @include shiny-driver.R
ShinyDriver2$set("public", "click", function(name, iotype = c("auto", "input", "output")) {
  self$findWidget(name, iotype)$click()
})

# # TODO-future; Not for this release. Comment for now.
# #' @description
# #' Sends the specified keys to specific HTML element. Shortcut for
# #' `findWidget()` plus `sendKeys()`.
# #' @param keys Keys to send to the widget or the app.
# # ' See [webdriver::key] for how to specific special keys.
# #' @return Self, invisibly.
# #' @include shiny-driver.R
# ShinyDriver2$set("public", "sendKeys", function(name, keys) {
#   "!DEBUG ShinyDriver2$sendKeys `name`"
#   self$findWidget(name)$sendKeys(keys)
#   invisible(self)
# })


#' @description
#' Lists the names of all input and output widgets
#' @return A list of two character vectors, named `input` and `output`.
#' @include shiny-driver.R
ShinyDriver2$set("public", "listWidgets", function() {
  "!DEBUG ShinyDriver2$listWidgets"
  res <- chromote_eval(self$get_chromote_session(), "shinytest2.listWidgets()")$result$value

  res$input <- sort_c(unlist(res$input))
  res$output <- sort_c(unlist(res$output))
  res
})

#' @description
#' Check if Shiny widget names are unique.
#' @include shiny-driver.R
ShinyDriver2$set("public", "checkUniqueWidgetNames", function() {
  "!DEBUG ShinyDriver2$checkUniqueWidgetNames"
  widgets <- self$listWidgets()
  inputs <- widgets$input
  outputs <- widgets$output

  check <- function(what, ids) {
    if (any(duplicated(ids))) {
      dup <- paste(unique(ids[duplicated(ids)]), collapse = ", ")
      warning("Possible duplicate ", what, " widget ids: ", dup)
    }
  }

  if (any(inputs %in% outputs)) {
    dups <- unique(inputs[inputs %in% outputs])
    warning(
      "Widget ids both for input and output: ",
      paste(dups, collapse = ", ")
    )

    ## Otherwise the following checks report it, too
    inputs <- setdiff(inputs, dups)
    outputs <- setdiff(outputs, dups)
  }

  if (length(inputs) > 0) check("input", inputs)
  if (length(outputs) > 0) check("output", outputs)

  invisible(self)
})
