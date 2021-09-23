#' A Shiny Widget
#'
#' @description
#' A `Widget2` object represents a Shiny input or output control, and provides
#' methods for finer grained interaction.
#'
#' @importFrom R6 R6Class
Widget2 <- R6Class(
  "Widget2",
  private = list(
    chromote_obj = NULL, # Chromote Session
    name = NULL,     # name in shiny
    # element = NULL,  # HTML element with name as id
    nodeId = NULL,   # nodeId of the element
    type = NULL,     # e.g. selectInput
    iotype = NULL    # "input" or "output"
  ),
  public = list(
    #' @description Create new `Widget2`
    #' @param name Name of a Shiny widget.
    #' @param nodeId Node Id of an Element from the `chromote` session.
    #' @param type Widget2 type
    #' @param iotype Input/output type.
    #' @param chromote Chromote Session
    initialize = function(name, nodeId, type, iotype = c("input", "output"), chromote) {
      iotype <- match.arg(iotype)

      private$name <- name
      private$nodeId <- nodeId
      private$type <- type
      private$iotype <- iotype
      private$chromote_obj <- chromote
      invisible(self)
    },

    #' @description Control id (i.e. `inputId` or `outputId` that control
    #'   was created with).
    getName = function() private$name,
    #' @description Underlying `nodeId` to be used within the Chromium session
    getNodeId = function() private$nodeId,
    #' @description Retrieve the underlying HTML for a widget
    getHtml = function() {
      chromote_call_js_on_node(private$chromote_obj, private$nodeId, "function() { return this.outerHTML; }")$result$value
    },
    #' @description Widget2 type, e.g. `textInput`, `selectInput`.
    getType = function() private$type,
    #' @description Is this an input or output control?
    getIoType = function() private$iotype,
    #' @description Is this an input control?
    isInput = function() private$iotype == "input",
    #' @description Is this an output control?
    isOutput = function() private$iotype == "output",

    #' @description Get current value of control.
    getValue = function(){
      "!DEBUG widget2_getValue `private$name`"

      if (private$iotype == "input") {
        res <- chromote_call_js_on_node(
          private$chromote_obj,
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
              private$chromote_obj,
              private$nodeId,
              "function() {
                return $(this).html();
              }"
            )$result$value
          },
          verbatimTextOutput = ,
          textOutput = {
            chromote_call_js_on_node(
              private$chromote_obj,
              private$nodeId,
              "function() {
                return $(this).text();
              }"
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

      setValueScript <-"
        function() {
          var el = $(this);
          var val = arguments[0];
          el.data('shinyInputBinding').setValue(el[0], val);
          el.trigger('change');
        }
      "
      chromote_call_js_on_node(private$chromote_obj, private$nodeId, setValueScript, arguments = list(value))

      invisible(self)
    },

    #' @description Sends a jQuery click to the element.
    #' @return self, invisibly.
    click = function() {
      clickScript <- "
        function() {
          var el = $(this);
          el.click();
        }
      "
      chromote_call_js_on_node(private$chromote_obj, private$nodeId, clickScript)
      invisible(self)
    },

    #' @description Send specified key presses to control.
    #' @param keys Keys to send to the widget or the app. See [webdriver::key]
    #'   for how to specific special keys.
    sendKeys = function(keys) {
      "!DEBUG widget2_sendKeys `private$name`"
      # TODO-barret
      private$element$sendKeys(keys)
    },

    #' @description Lists the tab names of a [shiny::tabsetPanel()].
    #'  It fails for other types of widgets.
    listTabs = function() {
      if (private$type != "tabsetPanel") {
        abort("'listTabs' only works for 'tabsetPanel' Widgets")
      }
      tabs <- private$element$findElements("li a")
      vapply(tabs, function(t) t$getData("value"), "")
    },

    #' @description Upload a file to a [shiny::fileInput()].
    #'  It fails for other types of widgets.
    #' @param filename Path to file to upload
    uploadFile = function(filename) {
      private$element$uploadFile(filename = filename)
    }
  )
)









#' Try to deduce the shiny input/output element type from its name
#'
#' @param name The name of the Shiny input or output to search for.
#' @param iotype It is possible that an input has the same name as
#'   an output, and in this case there is no way to get element without
#'   knowing whether it is an input or output element.
#'
#' @noRd
#' @importFrom rlang %|%
sd2_findWidget <- function(chromote, name, iotype) {

  "!DEBUG finding a Widget2 `name` (`iotype`)"

  css <- if (iotype == "auto") {
    paste0("#", name)

  } else if (iotype == "input") {
    paste0("#", name, ".shiny-bound-input")

  } else if (iotype == "output") {
    paste0("#", name, ".shiny-bound-output")
  }

  elNodeIds <- chromote_find_elements(chromote, css)

  if (length(elNodeIds) == 0) {
    abort(paste0(
      "Cannot find ",
      if (iotype != "auto") paste0(iotype, " "),
      "widget ", name
    ))

  } else if (length(elNodeIds) > 1) {
    warning(
      "Multiple ",
      if (iotype != "auto") paste0(iotype, " "),
      "widgets with id ", name
    )
  }

  nodeId <- elNodeIds[[1]]
  js_ret <- chromote_call_js_on_node(chromote, nodeId, "
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
    element = elNodeIds[[1]],
    type = unname(widget_names[type[[2]]] %|% type[[2]]),
    iotype = type[[1]],
    chromote = chromote
  )
}
