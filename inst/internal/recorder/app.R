library(shiny)
library(promises)

target_url    <- getOption("shinytest2.recorder.url")
app           <- getOption("shinytest2.app")
debug         <- getOption("shinytest2.debug")
load_timeout  <- getOption("shinytest2.load.timeout")
start_seed    <- getOption("shinytest2.seed")
shiny_args    <- getOption("shinytest2.shiny.args")

# If there are any reasons to not run a test, a message should be appended to
# this vector.
dont_run_reasons <- character(0)
add_dont_run_reason <- function(reason) {
  dont_run_reasons <<- c(dont_run_reasons, reason)
}

if (is.null(target_url) || is.null(app$get_path())) {
  stop("Test recorder requires the 'shinytest2.recorder.url' and ",
    "'shinytest2.app.dir' options to be set.")
}

# Can't register more than once, so remove existing one just in case.
removeInputHandler("shinytest2.testevents")

# Need to avoid Shiny's default recursive unlisting
registerInputHandler("shinytest2.testevents", function(val, shinysession, name) {
  val
})

escape_string <- function(s) {
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
numeric_input <- function(..., placeholder = NULL) {
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
  shiny::tags$script("$('a[data-toggle=\"tooltip\"]').tooltip({ delay: 250 });")
}

# Given a vector/list, return TRUE if any elements are unnamed, FALSE otherwise.
any_unnamed <- function(x) {
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
merge_vectors <- function(a, b) {
  if (any_unnamed(a) || any_unnamed(b)) {
    stop("Vectors must be either NULL or have names for all elements")
  }

  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}

input_processors <- list(
  default = function(value) {
    # This function is designed to operate on atomic vectors (not lists), so if
    # this is a list, we need to unlist it.
    if (is.list(value))
      value <- unlist(value, recursive = FALSE)

    if (length(value) > 1) {
      # If it's an array, recurse
      vals <- vapply(value, input_processors$default, "")
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
      return(paste0('"', escape_string(value), '"'))
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
    input_processors$default(value)
  }
)

# Add in input processors registered by other packages.
input_processors <- merge_vectors(input_processors, shinytest2::get_input_processors())

# Given an input value taken from the client, return the value that would need
# to be passed to app$set_input() to set the input to that value.
process_input_value <- function(value, input_type) {
  if (is.null(input_processors[[input_type]])) {
    # For input with type "mypkg.foo", get "mypkg", and then try to load it.
    # This is helpful in cases where the R session running `record_test()` has
    # not loaded the package with the input type. (There's a separate R session
    # running the Shiny app.) See https://github.com/rstudio/learnr/pull/407 for
    # more info.
    pkg <- strsplit(input_type, ".", fixed = TRUE)[[1]][1]

    if (try_load_package(pkg)) {
      # The set of `input_processors` may have changed by loading the package, so
      # re-merge the registered input processors.
      input_processors <<- merge_vectors(input_processors, shinytest2::get_input_processors())
    }
  }

  # Check again if the input type is now registered.
  if (is.null(input_processors[[input_type]])) {
    input_type <- "default"
  }

  input_processors[[input_type]](value)
}

# Try to load a package, but only once; subsequent calls with the same value of
# `pkg` will do nothing. Returns TRUE if the package is successfully loaded,
# FALSE otherwise.
tried_packages <- character()
try_load_package <- function(pkg) {
  if (pkg %in% tried_packages) {
    return(FALSE)
  }
  tried_packages <<- c(tried_packages, pkg)
  requireNamespace(pkg, quietly = TRUE)
}

# Quote variable/argument names. Normal names like x, x1, or x_y will not be changed, but
# if there are any strange characters, it will be quoted; x-1 will return `x-1`.
quote_name <- function(name) {
  if (!grepl("^[a-zA-Z0-9_]*$", name)) {
    paste0("`", name, "`")
  } else {
    name
  }
}

code_generators <- list(
  initialize = function(event, next_event = NULL, ...) {
    NA_character_
  },

  input = function(event, next_event = NULL, allow_no_input_binding = FALSE, ...) {
    # Extra arguments when using times
    args <- ""

    if (event$inputType == "shiny.fileupload") {
      filename <- process_input_value(event$value, event$inputType)

      code <- paste0(
        "app$uploadFile(",
        quote_name(event$name), " = ", filename,
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
        "app$set_inputs(",
        quote_name(event$name), " = ",
        process_input_value(event$value, event$inputType),
        args,
        ")"
      )

    } else {
      if (allow_no_input_binding) {
        args <- paste0(args, ", allow_no_input_binding_ = TRUE")
        if (identical(event$priority, "event")) args <- paste0(args, ', priority_ = "event"')
        paste0(
          "app$set_inputs(",
          quote_name(event$name), " = ",
          process_input_value(event$value, input_type = "default"),
          args,
          ")"
        )
      } else {
        paste0(
          "# Input '", quote_name(event$name),
          "' was set, but doesn't have an input binding."
        )
      }
    }
  },

  fileDownload = function(event, next_event = NULL, ...) {
    paste0('app$expect_download("', event$name, '")')
  },

  outputEvent = function(event, next_event = NULL, ...) {
     NA_character_
  },

  outputSnapshot = function(event, next_event = NULL, ...) {
    paste0('app$expect_appshot(items = list(output = "', event$name, '"))')
  },

  snapshot = function(event, next_event = NULL, ...) {
    "app$expect_appshot()"
  }
)

app_dir <- function() {
  app$get_path()
}
app_dir_basename <- function() {
  fs::path_file(app_dir())
}
app_file_basename <- function() {
  path <- app$get_path()
  if (dir.exists(path)) return(".")
  basename(path)
}

generate_test_code <- function(events, name, seed,
  allow_no_input_binding = FALSE)
{

  # Generate code for each input and output event
  event_code <- mapply(
    function(event, next_event) {
      code_generators[[event$type]](event, next_event,
                                   allow_no_input_binding = allow_no_input_binding)
    },
    events,
    c(events[-1], list(NULL))
  )

  # Find the indices of the initialize event and output events. The code lines
  # and (optional) Sys.sleep() calls for these events will be removed later.
  # We need the output events for now in order to calculate times.
  remove_events <- vapply(events, function(event) {
    event$type %in% c("initialize", "outputEvent")
  }, logical(1))

  if (length(event_code) != 0) {
    # Remove unwanted events
    event_code  <- event_code[!remove_events]

    event_code <- paste0("  ", event_code, collapse = "\n")
  }

  # From the tests dir, it is up two folders and then the app file
  app_path <- paste("../../", app_file_basename())
  inner_code <- paste(
    paste0(
      "  app <- AppDriver$new(\n",
      "    ", paste(c(
        paste0("test_path(\"", app_path, "\")"),
        if (!is.null(seed)) paste0("seed = %s", seed),
        if (!is.null(load_timeout)) paste0("load_timeout = ", load_timeout),
        if (length(shiny_args) > 0) paste0("shiny_args = ", deparse2(shiny_args)),
        "variant = os_name_and_r_version()"
        ),
        collapse = ",\n    "
      ), "\n",
      "  )"
    ),
    # paste0('app$snapshotInit("', name, '")'),
    # '',
    event_code,
    sep = "\n"
  )

  paste0(
    "test_that(\"", app_dir_basename(), "\", {\n",
    inner_code, "\n",
    "})\n"
  )
}

has_inputs_without_binding <- function(events) {
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
          "allow_no_input_binding",
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
        numeric_input("seed",
          label = tagList("Random seed:",
            tooltip("A seed is recommended if your application uses any randomness. This includes all Shiny Rmd documents.")
          ),
          value = start_seed,
          placeholder = "(None)"
        )
      ),
      div(class = "shiny-recorder-header", "Recorded events"),
      div(id = "recorded-events",
        tableOutput("recorded_events")
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
      n_console_lines <- 0
      observe({
        invalidateLater(500)
        logs <- app$get_debug_log(debug)
        n <- nrow(logs)
        if (n > n_console_lines) {
          new_lines <- seq.int(n_console_lines + 1, n)
          print(logs[new_lines, ], short = TRUE)
          cat("\n")
        }
        n_console_lines <<- n
      })
    }

    save_file <- reactive({
      file.path(app_dir(), "tests", "testthat", paste0(input$testname, ".R"))
    })

    # Number of snapshot or fileDownload events in input$testevents
    num_snapshots <- reactive({
      snapshots <- vapply(input$testevents, function(event) {
        return(event$type %in% c("snapshot", "outputSnapshot", "fileDownload"))
      }, logical(1))
      sum(snapshots)
    })

    output$recorded_events <- renderTable(
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

    save_and_exit <- function(delete_test_file = FALSE) {
      stopApp({

        seed <- as.integer(input$seed)
        if (is.null(seed) || is.na(seed))
          seed <- NULL

        code <- generate_test_code(
          input$testevents,
          input$testname,
          seed = seed,
          allow_no_input_binding = input$allow_no_input_binding
        )

        # (maybe) Remove prior file
        if (isTRUE(delete_test_file)) {
          unlink(save_file())
        }

        # Make sure folder exists
        dir.create(fs::path_dir(save_file()), recursive = TRUE, showWarnings = FALSE)

        add_library_call <- TRUE
        if (file.exists(save_file())) {
          code <- paste0(code, "\n\n")
          # don't double library()
          add_library_call <- !any(grepl(readLines(save_file()), "^library\\(shinytest2\\)$"))
        }
        if (add_library_call) {
          code <- paste0("library(shinytest2)\n", code)
        }
        cat(code, file = save_file(), append = TRUE)
        message("Saved test code to ", save_file())
        if (isTRUE(input$editSaveFile)) {
          file.edit(save_file())
        }

        invisible(list(
          test_file = save_file(),
          run = input$runScript && (length(dont_run_reasons) == 0),
          dont_run_reasons = dont_run_reasons
        ))
      })
    }


    present_modal <- function(modal_dialog, cancels, oks) {
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

        showModal(modal_dialog)
      })
    }

    observeEvent(input$exit_save, {

      if (num_snapshots() == 0) {
        showModal(
          modal_dialog("Must have at least one snapshot to save and exit.")
        )
        return()
      }

      p <- promise_resolve(TRUE)

      if (has_inputs_without_binding(input$testevents) && !input$allow_no_input_binding) {
        p <- p %...>% {
          present_modal(
            modal_dialog(
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
        if (file.exists(save_file())) {
          present_modal(
            modal_dialog(
              paste0("Overwrite ", basename(save_file()), "?"),
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
        save_and_exit(delete_test_file)
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
