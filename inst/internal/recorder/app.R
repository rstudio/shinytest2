library(promises)

target_url    <- getOption("shinytest2.recorder.url")
app           <- getOption("shinytest2.app")
debug         <- getOption("shinytest2.debug")
load_timeout  <- getOption("shinytest2.load.timeout")
start_seed    <- getOption("shinytest2.seed")
shiny_options <- getOption("shinytest2.shiny.options")

# If there are any reasons to not run a test, a message should be appended to
# this vector.
dont_run_reasons <- character(0)
add_dont_run_reason <- function(reason) {
  dont_run_reasons <<- c(dont_run_reasons, reason)
}

if (is.null(target_url) || is.null(app$getAppDir())) {
  stop("Test recorder requires the 'shinytest2.recorder.url' and ",
    "'shinytest2.app.dir' options to be set.")
}

# Can't register more than once, so remove existing one just in case.
removeInputHandler("shinytest2.testevents")

# Need to avoid Shiny's default recursive unlisting
registerInputHandler("shinytest2.testevents", function(val, shinysession, name) {
  val
})

escapeString <- function(s) {
  # escape \ as well as "
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  gsub('"', '\\"', s, fixed = TRUE)
}

# A replacement for deparse() that's a little less verbose for named lists.
deparse2 <- function(x) {
  expr <- deparse(x)
  expr <- paste(expr, collapse = "")

  # If the deparsed expression is something like:
  #   "structure(list(a = 1, b = 2), .Names = c(\"a\", \"b\"))"
  # simplify it to "list(a = 1, b = 2)".
  expr <- sub("^structure\\((list.*), \\.Names = c\\([^(]+\\)\\)$", "\\1", expr)
  # Same as above, but for single item in .Names, like:
  #  "structure(list(a = 1), .Names = \"a\")"
  expr <- sub('^structure\\((list.*), \\.Names = \\"[^\\"]*\\"\\)$', "\\1", expr)

  expr
}


# A modified version of shiny::numericInput but with a placholder
numericInput <- function(..., placeholder = NULL) {
  res <- shiny::numericInput(...)
  res$children[[2]]$attribs$placeholder <- placeholder
  res
}

# Create a question mark icon that displays a tooltip when hovered over.
tooltip <- function(text, placement = "top") {
  a(href = "#",
    `data-toggle` = "tooltip",
    title = text,
    icon("question-sign", lib = "glyphicon"),
    `data-placement` = placement,
    `data-html` = "true"
  )
}

enable_tooltip_script <- function() {
  tags$script("$('a[data-toggle=\"tooltip\"]').tooltip({ delay: 250 });")
}

# Given a vector/list, return TRUE if any elements are unnamed, FALSE otherwise.
anyUnnamed <- function(x) {
  # Zero-length vector
  if (length(x) == 0) return(FALSE)

  nms <- names(x)

  # List with no name attribute
  if (is.null(nms)) return(TRUE)

  # List with name attribute; check for any ""
  any(!nzchar(nms))
}

# Given two named vectors, join them together, and keep only the last element
# with a given name in the resulting vector. If b has any elements with the same
# name as elements in a, the element in a is dropped. Also, if there are any
# duplicated names in a or b, only the last one with that name is kept.
mergeVectors <- function(a, b) {
  if (anyUnnamed(a) || anyUnnamed(b)) {
    stop("Vectors must be either NULL or have names for all elements")
  }

  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}

inputProcessors <- list(
  default = function(value) {
    # This function is designed to operate on atomic vectors (not lists), so if
    # this is a list, we need to unlist it.
    if (is.list(value))
      value <- unlist(value, recursive = FALSE)

    if (length(value) > 1) {
      # If it's an array, recurse
      vals <- vapply(value, inputProcessors$default, "")
      return(paste0(
        "c(",
        paste0(vals, collapse = ", "),
        ")"
      ))
    }

    if (length(value) == 0) {
      return("character(0)")
    }

    if (is.character(value)) {
      return(paste0('"', escapeString(value), '"'))
    } else {
      return(as.character(value))
    }
  },

  shiny.action = function(value) {
    '"click"'
  },

  shiny.fileupload = function(value) {
    # Extract filenames, then send to default processor
    value <- vapply(value, function(file) file$name, character(1))
    inputProcessors$default(value)
  }
)

# Add in input processors registered by other packages.
inputProcessors <- mergeVectors(inputProcessors, shinytest2::getInputProcessors())

# Given an input value taken from the client, return the value that would need
# to be passed to app$set_input() to set the input to that value.
processInputValue <- function(value, inputType) {
  if (is.null(inputProcessors[[inputType]])) {
    # For input with type "mypkg.foo", get "mypkg", and then try to load it.
    # This is helpful in cases where the R session running `recordTest()` has
    # not loaded the package with the input type. (There's a separate R session
    # running the Shiny app.) See https://github.com/rstudio/learnr/pull/407 for
    # more info.
    pkg <- strsplit(inputType, ".", fixed = TRUE)[[1]][1]

    if (tryLoadPackage(pkg)) {
      # The set of inputProcessors may have changed by loading the package, so
      # re-merge the registered input processors.
      inputProcessors <<- mergeVectors(inputProcessors, shinytest2::getInputProcessors())
    }
  }

  # Check again if the input type is now registered.
  if (is.null(inputProcessors[[inputType]])) {
    inputType <- "default"
  }

  inputProcessors[[inputType]](value)
}

# Try to load a package, but only once; subsequent calls with the same value of
# `pkg` will do nothing. Returns TRUE if the package is successfully loaded,
# FALSE otherwise.
triedPackages <- character()
tryLoadPackage <- function(pkg) {
  if (pkg %in% triedPackages) {
    return(FALSE)
  }
  triedPackages <<- c(triedPackages, pkg)
  requireNamespace(pkg, quietly = TRUE)
}

# Quote variable/argument names. Normal names like x, x1, or x_y will not be changed, but
# if there are any strange characters, it will be quoted; x-1 will return `x-1`.
quoteName <- function(name) {
  if (!grepl("^[a-zA-Z0-9_]*$", name)) {
    paste0("`", name, "`")
  } else {
    name
  }
}

codeGenerators <- list(
  initialize = function(event, nextEvent = NULL, ...) {
    NA_character_
  },

  input = function(event, nextEvent = NULL, allowInputNoBinding = FALSE, ...) {
    # Extra arguments when using times
    args <- ""

    if (event$inputType == "shiny.fileupload") {
      filename <- processInputValue(event$value, event$inputType)

      code <- paste0(
        "app$uploadFile(",
        quoteName(event$name), " = ", filename,
        args,
        ")"
      )

      # Get unescaped filenames in a char vector, with full path
      filepaths <- vapply(event$value, `[[`, "name", FUN.VALUE = "")
      filepaths <- file.path(app$getTestsDir(), filepaths)

      # Check that all files exist. If not, add a message and don't run test
      # automatically on exit.
      if (!all(file.exists(filepaths))) {
        add_dont_run_reason("An uploadFile() must be updated: use the correct path relative to the app's tests/shinytest directory, or copy the file to the app's tests/shinytest directory.")
        code <- paste0(code,
          " # <-- This should be the path to the file, relative to the app's tests/shinytest directory"
        )
      }

      code

    } else if (event$hasBinding) {
      paste0(
        "app$setInputs(",
        quoteName(event$name), " = ",
        processInputValue(event$value, event$inputType),
        args,
        ")"
      )

    } else {
      if (allowInputNoBinding) {
        args <- paste0(args, ", allowInputNoBinding_ = TRUE")
        if (identical(event$priority, "event")) args <- paste0(args, ', priority_ = "event"')
        paste0(
          "app$setInputs(",
          quoteName(event$name), " = ",
          processInputValue(event$value, inputType = "default"),
          args,
          ")"
        )
      } else {
        paste0(
          "# Input '", quoteName(event$name),
          "' was set, but doesn't have an input binding."
        )
      }
    }
  },

  fileDownload = function(event, nextEvent = NULL, ...) {
    paste0('app$snapshotDownload("', event$name, '")')
  },

  outputEvent = function(event, nextEvent = NULL, ...) {
     NA_character_
  },

  outputSnapshot = function(event, nextEvent = NULL, ...) {
    paste0('expect_snapshot_app(app, items = list(output = "', event$name, '"))')
  },

  snapshot = function(event, nextEvent = NULL, ...) {
    "expect_snapshot_app(app)"
  }
)

appDirBasename <- function() {
  # app$private$path
  fs::path_file(shinytest2:::app_path(app$getAppDir())$dir)
}

generateTestCode <- function(events, name, seed,
  allowInputNoBinding = FALSE)
{

  # Generate code for each input and output event
  eventCode <- mapply(
    function(event, nextEvent) {
      codeGenerators[[event$type]](event, nextEvent,
                                   allowInputNoBinding = allowInputNoBinding)
    },
    events,
    c(events[-1], list(NULL))
  )

  # Find the indices of the initialize event and output events. The code lines
  # and (optional) Sys.sleep() calls for these events will be removed later.
  # We need the output events for now in order to calculate times.
  removeEvents <- vapply(events, function(event) {
    event$type %in% c("initialize", "outputEvent")
  }, logical(1))

  if (length(eventCode) != 0) {
    # Remove unwanted events
    eventCode  <- eventCode[!removeEvents]

    eventCode <- paste0("  ", eventCode, collapse = "\n")
  }

  # TODO-barret add testthat ed 3 wrapper code
  # Need paste instead of file.path because app$getAppFilename() can be NULL which makes file.path grumpy.
  app_path <- paste(
    # Get relative path from app to the testthat tests directory
    fs::path_rel(app$getAppDir(), rprojroot::find_testthat_root_file(path = app$getAppDir())),
    app$getAppFilename(),
    sep = "/"
  )
  inner_code <- paste(
    paste0(
      "  app <- ShinyDriver2$new(\n",
      "    ", paste(c(
        paste0("test_path(\"", app_path, "\")"),
        if (!is.null(seed)) paste0("seed = %s", seed),
        if (!is.null(load_timeout)) paste0("loadTimeout = ", load_timeout),
        if (length(shiny_options) > 0) paste0("shinyOptions = ", deparse2(shiny_options)),
        "variant = os_name_and_r_version()"
        ),
        collapse = ",\n    "
      ), "\n",
      "  )"
    ),
    # paste0('app$snapshotInit("', name, '")'),
    # '',
    eventCode,
    sep = "\n"
  )

  paste0(
    "test_that(\"", fs::path_file(shinytest2:::app_path(app$getAppDir())$dir), " - ", appDirBasename(), "\", {\n",
    inner_code,
    "})\n"
  )
}

hasInputsWithoutBinding <- function(events) {
  any(vapply(events, function(event) {
    return(event$type == "input" && !event$hasBinding)
  }, TRUE))
}

shinyApp(
  ui = fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "recorder.css"),
      tags$script(src = "inject-recorder.js")
    ),

    div(id = "app-iframe-container",
      tags$iframe(id = "app-iframe", src = target_url)
    ),
    div(id = "shiny-recorder",
      div(class = "shiny-recorder-header", "Test event recorder"),
      div(class = "shiny-recorder-controls",
        span(
          actionLink("snapshot",
            span(
              img(src = "snapshot.png", class = "shiny-recorder-icon"),
              "Take snapshot"
            ),
            style = "display: inline;"
          ),
          tooltip(
            HTML("You can also Ctrl-click or &#8984;-click on an output to snapshot just that one output.<br> To trigger a snapshot via the keyboard, press Ctrl-shift-S or &#8984;-shift-S"),
            placement = "bottom"
          ),
          hr()
        ),
        actionLink("exit_save",
          span(
            img(src = "exit-save.png", class = "shiny-recorder-icon"),
            "Save script and exit test event recorder"
          )
        ),
        actionLink("exit_nosave",
          span(
            img(src = "exit-nosave.png", class = "shiny-recorder-icon"),
            "Quit without saving"
          )
        ),
        textInput("testname", label = "On exit, save test script as:", value = "mytest"),
        if (rstudioapi::isAvailable()) checkboxInput("editSaveFile", "Open script in editor on exit", value = TRUE),
        checkboxInput("runScript", "Run test script on exit", value = TRUE),
        checkboxInput(
          "allowInputNoBinding",
          tagList("Save inputs that do not have a binding",
            tooltip(
              paste(
                "This enables recording inputs that do not have a binding, which is common in htmlwidgets",
                "like DT and plotly. Note that playback support is limited: shinytest will set the input",
                "value so that R gets the input value, but the htmlwidget itself will not be aware of the value."
              ),
              placement = "bottom"
            )
          ),
          value = FALSE
        ),
        numericInput("seed",
          label = tagList("Random seed:",
            tooltip("A seed is recommended if your application uses any randomness. This includes all Shiny Rmd documents.")
          ),
          value = start_seed,
          placeholder = "(None)"
        )
      ),
      div(class = "shiny-recorder-header", "Recorded events"),
      div(id = "recorded-events",
        tableOutput("recordedEvents")
      ),
      enable_tooltip_script()
    )
  ),

  server = function(input, output) {
    # Read the recorder.js file for injection into iframe
    output$recorder_js <- renderText({
      file <- "recorder.js"
      readChar(file, file.info(file)$size, useBytes = TRUE)
    })
    outputOptions(output, "recorder_js", suspendWhenHidden = FALSE)

    # echo console output from the driver object (in real-time)
    if (!identical(debug, "none")) {
      nConsoleLines <- 0
      observe({
        invalidateLater(500)
        logs <- app$getDebugLog(debug)
        n <- nrow(logs)
        if (n > nConsoleLines) {
          newLines <- seq.int(nConsoleLines + 1, n)
          print(logs[newLines, ], short = TRUE)
          cat("\n")
        }
        nConsoleLines <<- n
      })
    }

    saveFile <- reactive({
      # file.path(app$getTestsDir(), paste0(input$testname, ".R"))
      tryCatch({
        rprojroot::find_testthat_root_file(
          paste0("test-st2-", appDirBasename(), ".R"),
          # TODO-barret; Given the application directory or the calling directory? Calling directory will need to be passed in
          path = app$getAppDir()
        )
      }, error = function(e) {
        message("Error while finding a location to save the test script:", e)
        req(FALSE)
      })
    })

    # Number of snapshot or fileDownload events in input$testevents
    numSnapshots <- reactive({
      snapshots <- vapply(input$testevents, function(event) {
        return(event$type %in% c("snapshot", "outputSnapshot", "fileDownload"))
      }, logical(1))
      sum(snapshots)
    })

    output$recordedEvents <- renderTable(
      {
        # Genereate list of lists from all events. Inner lists have 'type' and
        # 'name' fields.
        events <- lapply(input$testevents, function(event) {
          type <- event$type

          if (type == "initialize") {
            NULL
          } else if (type == "outputSnapshot") {
            list(type = "snapshot-output", name = event$name)
          } else if (type == "snapshot") {
            list(type = "snapshot", name = "<all>")
          } else if (type == "input") {
            if (event$inputType == "shiny.fileupload") {
              # File uploads are a special case of inputs
              list(type = "file-upload", name = event$name)
            } else if (!event$hasBinding) {
              list(type = "input *", name = event$name)
            } else {
              list(type = "input", name = event$name)
            }
          } else if (type == "fileDownload") {
            list(type = "file-download", name = event$name)
          } else if (type == "outputEvent") {
            list(type = "output-event", name = "--")
          }
        })

        events <- events[!vapply(events, is.null, logical(1))]

        # Transpose list of lists into data frame
        data.frame(
          `Event type` = vapply(events, `[[`, character(1), "type"),
          Name = vapply(events, `[[`, character(1), "name"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      },
      width = "100%",
      rownames = TRUE
    )

    saveAndExit <- function(delete_test_file = FALSE) {
      stopApp({

        seed <- as.integer(input$seed)
        if (is.null(seed) || is.na(seed))
          seed <- NULL

        code <- generateTestCode(
          input$testevents,
          input$testname,
          seed = seed,
          allowInputNoBinding = input$allowInputNoBinding
        )

        if (isTRUE(delete_test_file)) {
          unlink(saveFile())
        }
        if (file.exists(saveFile())) {
          cat("\n\n", file = saveFile(), append = TRUE)
        }
        cat(code, file = saveFile(), append = TRUE)
        message("Saved test code to ", saveFile())
        if (isTRUE(input$editSaveFile)) {
          file.edit(saveFile())
        }

        invisible(list(
          test_file = saveFile(),
          run = input$runScript && (length(dont_run_reasons) == 0),
          dont_run_reasons = dont_run_reasons
        ))
      })
    }


    presentModal <- function(modalDialog, cancels, oks) {
      promise(function(resolve, reject) {

        obs <- list()
        lapply(cancels, function(cancel) {
          obs[[length(obs) + 1]] <<- observeEvent(input[[cancel]],
            {
              # cancel all observers
              lapply(obs, function(ob) { ob$destroy() })
              reject(cancel)
            },
            ignoreInit = TRUE
          )

        })

        lapply(oks, function(ok) {
          obs[[length(obs) + 1]] <<- observeEvent(input[[ok]],
            {
              # cancel all observers
              lapply(obs, function(ob) { ob$destroy() })
              resolve(ok)
            },
            ignoreInit = TRUE
          )
        })

        showModal(modalDialog)
      })
    }

    observeEvent(input$exit_save, {

      if (numSnapshots() == 0) {
        showModal(
          modalDialog("Must have at least one snapshot to save and exit.")
        )
        return()
      }

      p <- promise_resolve(TRUE)

      if (hasInputsWithoutBinding(input$testevents) && !input$allowInputNoBinding) {
        p <- p %...>% {
          presentModal(
            modalDialog(
              tagList(
                "There are some input events (marked with a *) that do not have a corresponding input binding.",
                "If you want them to be saved in the test script, press Cancel, then check ",
                tags$b("Save inputs that do not have a binding."),
                "If you don't want to save them, press Continue."
              ),
              footer = tagList(
                actionButton("inputs_no_binding_cancel",   "Cancel",   `data-dismiss` = "modal"),
                actionButton("inputs_no_binding_continue", "Continue", `data-dismiss` = "modal")
              )
            ),
            "inputs_no_binding_cancel",
            "inputs_no_binding_continue"
          )
        }
      }

      p <- p %...>% {
        if (file.exists(saveFile())) {
          presentModal(
            modalDialog(
              paste0("Overwrite ", basename(saveFile()), "?"),
              footer = tagList(
                actionButton("overwrite_cancel",   "Cancel",   `data-dismiss` = "modal"),
                actionButton("overwrite_append", "Append", `data-dismiss` = "modal"),
                actionButton("overwrite_overwrite", "Overwrite", `data-dismiss` = "modal")
              )
            ),
            "overwrite_cancel",
            c("overwrite_overwrite", "overwrite_append")
          )
        } else {
          promise_resolve(TRUE)
        }
      }

      p <- p %...>% {
        delete_test_file <- identical(., "overwrite_overwrite")
        saveAndExit(delete_test_file)
      }

      # When Cancel is pressed, catch the rejection.
      p <- p %...!% {
        NULL
      }

      # Need to return something other than the promise. Otherwise Shiny will
      # wait for the promise to resolve before processing any further
      # reactivity, including the inputs from the actionButtons, so the app
      # will simply stop responding.
      NULL
    })

    observeEvent(input$exit_nosave, {
      stopApp({
        message("Quitting without saving or running tests.")
        invisible(list(
          file = NULL,
          run = FALSE
        ))
      })
    })
  }
)
