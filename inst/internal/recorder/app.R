# TODO-barret; fix saving code
# TODO-barret; auto-opt in to saving all values with no input binding
# TODO-barret; delete unused code
# TODO-barret; record many many tests


library(shiny)
library(promises)

target_url    <- getOption("shinytest2.recorder.url")
app           <- getOption("shinytest2.app")
load_timeout  <- getOption("shinytest2.load.timeout")
start_seed    <- getOption("shinytest2.seed")
shiny_args    <- getOption("shinytest2.shiny.args")
save_file     <- getOption("shinytest2.test_file")
allow_input_no_binding <- isTRUE(getOption("shinytest2.allow_input_no_binding"))

# If there are any reasons to not run a test, a message should be appended to
# this vector.
dont_run_reasons <- character(0)
add_dont_run_reason <- function(reason) {
  dont_run_reasons <<- c(dont_run_reasons, reason)
}

if (is.null(target_url) || is.null(app$get_path())) {
  abort(paste0("Test recorder requires the 'shinytest2.recorder.url' and ",
    "'shinytest2.app.dir' options to be set."))
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
  span(
    # href = "#",
    `data-toggle` = "tooltip",
    # style="color: #0000EE;",
    # style="color: #1a0dab;",
    title = text,
    icon("question-sign", lib = "glyphicon"),
    `data-placement` = placement,
    `data-html` = "true"
  )
}

enable_tooltip_script <- function() {
  shiny::tags$script("$('span[data-toggle=\"tooltip\"]').tooltip({ delay: 250 });")
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
    abort("Vectors must be either NULL or have names for all elements")
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
    structure("click", class = c("st2_click", "character"))
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

# code_generators <- list(
#   # initialize = function(event, next_event = NULL, ...) {
#   #   NA_character_
#   # },

#   input = function(event, next_event = NULL, allow_input_no_binding = FALSE, ...) {
#     # Extra arguments when using times
#     args <- ""

#     if (event$inputType == "shiny.fileupload") {
#       filename <- process_input_value(event$value, event$inputType)

#       code <- paste0(
#         "app$uploadFile(",
#         quote_name(event$name), " = ", filename,
#         args,
#         ")"
#       )

#       # Get unescaped filenames in a char vector, with full path
#       filepaths <- vapply(event$value, `[[`, "name", FUN.VALUE = "")
#       filepaths <- file.path(app$getTestsDir(), filepaths)

#       # Check that all files exist. If not, add a message and don't run test
#       # automatically on exit.
#       if (!all(file.exists(filepaths))) {
#         add_dont_run_reason("An uploadFile() must be updated: use the correct path relative to the app's tests/shinytest directory, or copy the file to the app's tests/shinytest directory.")
#         code <- paste0(code,
#           " # <-- This should be the path to the file, relative to the app's tests/shinytest directory"
#         )
#       }

#       code

#     } else if (event$hasBinding) {
#       paste0(
#         "app$set_inputs(",
#         quote_name(event$name), " = ",
#         process_input_value(event$value, event$inputType),
#         args,
#         ")"
#       )

#     } else {
#       if (isTRUE(allow_input_no_binding)) {
#         args <- paste0(args, ", allow_input_no_binding_ = TRUE")
#         if (identical(event$priority, "event")) args <- paste0(args, ', priority_ = "event"')
#         paste0(
#           "app$set_inputs(",
#           quote_name(event$name), " = ",
#           process_input_value(event$value, input_type = "default"),
#           args,
#           ")"
#         )
#       } else {
#         paste0(
#           "# Input '", quote_name(event$name),
#           "' was set, but doesn't have an input binding."
#         )
#       }
#     }
#   }

#   # fileDownload = function(event, next_event = NULL, ...) {
#   #   paste0('app$expect_download("', event$name, '")')
#   # },

#   # outputEvent = function(event, next_event = NULL, ...) {
#   #    NA_character_
#   # },

#   # inputSnapshot = function(event, next_event = NULL, ...) {
#   #   paste0('app$expect_values(input = "', event$name, '"))')
#   # },

#   # outputSnapshot = function(event, next_event = NULL, ...) {
#   #   paste0('app$expect_values(output = "', event$name, '"))')
#   # },

#   # appshot = function(event, next_event = NULL, ...) {
#   #   "app$expect_appshot()"
#   # }
# )

app_dir <- function() {
  app$get_path()
}
app_dir_basename <- function() {
  fs::path_file(app_dir())
}
app_test_path <- function() {
  path <- app$get_path()
  if (dir.exists(path)) return(NULL)
  basename(path)
}

save_file <- file.path(app_dir(), "tests", "testthat", save_file)

generate_test_code <- function(events, name, seed
  # allow_input_no_binding = FALSE
  )
{

  # Generate code for each input and output event
  # event_code <- mapply(
  #   function(event, next_event) {
  #     code_generators[[event$type]](event, next_event,
  #                                  allow_input_no_binding = allow_input_no_binding)
  #   },
  #   events,
  #   c(events[-1], list(NULL))
  # )
  event_code <- unlist(lapply(events, `[[`, app_code)) # remove NULLs
  event_code <- paste0("    ", event_code, collapse = "\n")


  # # Find the indices of the initialize event and output events. The code lines
  # # and (optional) Sys.sleep() calls for these events will be removed later.
  # # We need the output events for now in order to calculate times.
  # remove_events <- vapply(events, function(event) {
  #   event$type %in% c("initialize", "outputEvent")
  # }, logical(1))

  # if (length(event_code) != 0) {
  #   # Remove unwanted events
  #   event_code  <- event_code[!remove_events]

  #   event_code <- paste0("  ", event_code, collapse = "\n")
  # }

  has_expect_screenshot <- any(unlist(lapply(events, `[[`, type)) == "expectScreenshot")

  # From the tests dir, it is up two folders and then the app file
  inner_code <- paste(
    paste0(
      "  app <- AppDriver$new(\n",
      "    ", paste(c(
        app_test_path(),
        if (has_expect_screenshot) "variant = platform_variant()",
        if (!is.null(name)) paste0("name = ", deparse2(name)),
        if (!is.null(seed)) paste0("seed = ", seed),
        if (!is.null(load_timeout)) paste0("load_timeout = ", load_timeout),
        if (length(shiny_args) > 0) paste0("shiny_args = ", deparse2(shiny_args)),
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

  ret <- paste0(
    "test_that(\"shinytest2: ", name, "\", {\n",
    inner_code, "\n",
    "})\n"
  )

  cat(ret)

  ret
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
      div(class = "shiny-recorder-header", tags$code("{shinytest2}"), "expectations"),
      div(class = "shiny-recorder-controls",
        actionButton("values",
          span(
            img(src="shiny.png", class = "shiny-recorder-icon", style="height: 23px;vertical-align: middle;"),
            "Expect Shiny values"
          )
        ),
        tooltip(
          HTML("To capture all Shiny values via the keyboard, press Ctrl-shift-V or or &#8984;-shift-V.<br/>You can also Ctrl-click or &#8984;-click on an input/output to capture just that one input/output."),
          placement = "bottom"
        ),
        actionButton("screenshot",
          span(
            img(src="snapshot.png", class = "shiny-recorder-icon"),
            "Expect screenshot",
            style = "display: inline;"
          )
        ),
        tooltip(
          HTML("To trigger a screenshot via the keyboard, press Ctrl-shift-S or &#8984;-shift-S"),
          placement = "bottom"
        ),
        # actionLink("values",
        #   span(
        #     img(src="shiny.png", class = "shiny-recorder-icon", style="height: 23px;vertical-align: middle;"),
        #     "Capture Shiny values",
        #     tooltip(
        #       HTML("To capture all Shiny values via the keyboard, press Ctrl-shift-V or or &#8984;-shift-V.<br/>You can also Ctrl-click or &#8984;-click on an input/output to capture just that one input/output."),
        #       placement = "bottom"
        #     )
        #   )
        # ),
        # # br(style="display:block; margin-bottom: 10em;"),
        # actionLink("screenshot",
        #   span(
        #     img(src="snapshot.png", class = "shiny-recorder-icon"),
        #     "Take screenshot",
        #     tooltip(
        #       HTML("To trigger a screenshot via the keyboard, press Ctrl-shift-S or &#8984;-shift-S"),
        #       placement = "bottom"
        #     )
        #     , style = "display: inline;"
        #   )
        # ),
        # hr(),

        # checkboxInput(
        #   "includeVariant",
        #   tagList(
        #     "Save variant information",
        #     tooltip(tagList(
        #       "Setting the", tags$code("variant"), "is required when calling",
        #       tags$code("app$expect_screenshot()"), ". Screenshots between different operating systems and R versions are not stable."
        #     ))
        #   ),
        #   value = FALSE
        # ),
        # checkboxInput(
        #   "allow_input_no_binding",
        #   # tagList("Save inputs that do not have a binding",
        #   tagList("Save inputs without a binding",
        #     tooltip(
        #       paste(
        #         "This enables recording inputs that do not have a binding, which is common in htmlwidgets",
        #         "like DT and plotly. Note that playback support is limited: shinytest will set the input",
        #         "value so that R gets the input value, but the htmlwidget itself will not be aware of the value."
        #       ),
        #       placement = "bottom"
        #     )
        #   ),
        #   value = FALSE
        # )
      ),
      div(class = "shiny-recorder-header", "Code"),
      uiOutput("recorded_events"),
      # div(id = "save-and-quit",
      #   actionLink("exit_save",
      #     span(
      #       img(src = "exit-save.png", class = "shiny-recorder-icon"),
      #       "Save test and exit recorder"
      #     )
      #   ),
      #   actionLink("exit_nosave",
      #     span(
      #       img(src = "exit-nosave.png", class = "shiny-recorder-icon"),
      #       "Quit without saving"
      #     )
      #   )
      # ),
      div(class = "shiny-recorder-header", "Save"),
      div(id = "save-and-quit",
        tagAppendChild(
          tagAppendAttributes(
            textInput("testname", label = "Test name:", value = app_dir_basename()),
            class = "inline-input-container",
            # style = "padding-top: 0.5em;"
          ),
          tooltip("The name of the test should describe what the set of expectations are trying to confirm."),
        ),
        tagAppendChild(
          tagAppendAttributes(
            numeric_input("seed",
              label = "Random seed:",
              value = start_seed,
              placeholder = "(None)"
            ),
            class = "inline-input-container"
          ),
          tooltip("A seed is recommended if your application uses any randomness. This includes all Shiny Rmd documents.")
        ),
        actionButton("exit_nosave",
          span(
            img(src = "exit-nosave.png", class = "shiny-recorder-icon"),
            "Exit"
          )
        ),
        actionButton("exit_save",
          span(
            img(src = "exit-save.png", class = "shiny-recorder-icon"),
            "Save test and exit"
          )
        )
      ),
      enable_tooltip_script()
    )
  ),

  server = function(input, output, session) {
    # Read the recorder.js file for injection into iframe
    observeEvent(once = TRUE, session$clientData$url_hostname, {
      file <- "recorder.js"
      session$sendCustomMessage(
        "recorder_js",
        readChar(file, file.info(file)$size, useBytes = TRUE)
      )
    })


    # echo console output from the driver object (in real-time)
    n_console_lines <- 0
    observe({
      # invalidateLater(500)
      logs <- subset(app$get_log(), location == "shiny")
      # print(logs)
      n <- nrow(logs)
      if (n > n_console_lines) {
        new_lines <- seq.int(n_console_lines + 1, n)
        cat("\n\n")
        print(logs[new_lines, ])
        cat("\n")
      }
      n_console_lines <<- n
    })

    # save_file <- reactive({
    #   testname <- input$testname
    #   if (!grepl("^test-", testname)) {
    #     testname <- paste0("test-", testname)
    #   }
    #   if (!grepl("\\.[rR]$", testname)) {
    #     testname <- paste0(testname, ".R")
    #   } else {
    #     sub("\\.r$", ".R", testname)
    #   }
    #   file.path(app_dir(), "tests", "testthat", testname)
    # })


    trim_testevents <- reactive({
      events <- input$testevents

      trim_occurred <- TRUE
      while (trim_occurred) {
        trim_occurred <- FALSE

        to_remove <- c()
        for (i in seq_along(events)) {
          if (i == 1) next
          prev_event <- events[[i - 1]]
          curr_event <- events[[i]]
          should_remove <- FALSE
          if (prev_event$type == curr_event$type) {
            switch(curr_event$type,
              # "expectValues" = {
              #   should_remove <-
              #     identical(prev_event$key, curr_event$key) &&
              #     identical(prev_event$value, curr_event$value)
              # },
              "outputEvent" = ,
              # "expectScreenshot" = ,
              # "waitForIdle" = ,
              "setWindowSize" = {should_remove <- TRUE}
              # inputEvent? No!
              # This could have unexpected behavior compared to recording.
              # The user can remove the extra lines
            )
          } else if (
            i >= 3 &&
            curr_event$type == "setWindowSize" &&
            prev_event$type == "outputEvent" &&
            events[[i - 2]]$type == "setWindowSize"
          ) {
            should_remove <- TRUE
          }
          if (should_remove) {
            to_remove[length(to_remove) + 1] <- i - 1
          }
        }

        if (length(to_remove)) {
          trim_occurred <- TRUE
          events <- events[-to_remove]
        }
      }

      events <- lapply(events, function(event) {
        event$app_code <-
          switch(event$type,
            "initialize" = NULL,
            # "output" = list(type = "snapshot-output", name = event$name),
            # "appshot" = list(type = "appshot", name = "<all>"),
            "waitForIdle" = "app$wait_for_idle()",
            "setWindowSize" = paste0("app$set_window_size(width = ", event$width, ", height = ", event$height, ")"),
            "expectDownload" = paste0("app$expect_download(\"", event$name, "\")"),
            "expectScreenshot" = "app$expect_screenshot()",
            "expectValues" = {
              key <- event$key
              value <- event$value
              if (!is.null(key)) {
                paste0("app$expect_values(", quote_name(key), " = ", process_input_value(value, "default"), ")")
              } else {
                paste0("app$expect_values()")
              }
            },
            "inputEvent" = {
              key <- quote_name(event$name)
              value <- process_input_value(event$value, event$inputType)
              if (inherits(value, "st2_click")) {
                # `"click"` event
                paste0("app$click(\"", event$name, "\")")
              } else if (event$inputType == "shiny.fileupload") {
                # File uploads are a special case of inputs
                code <- paste0(
                  "app$upload_file(", key, " = ", value, ")"
                )

                # Get unescaped filenames in a char vector, with full path
                filepaths <- vapply(event$value, `[[`, "name", FUN.VALUE = "")
                filepaths <- file.path(app_dir(), "tests", "testthat", filepaths)

                # Check that all files exist. If not, add a message and don't run test
                # automatically on exit.
                if (!all(file.exists(filepaths))) {
                  add_dont_run_reason("An uploadFile() must be updated: use the correct path relative to the app's ./tests/testthat directory, or copy the file to the app's ./tests/testthat directory.")
                  code <- paste0(code,
                    " # <-- This should be the path to the file, relative to the app's tests/testthat directory"
                  )
                }

                code
              } else {
                if (!event$hasBinding && !allow_input_no_binding) {
                  # TODO-barret; test
                  structure(
                    paste0(
                      "# Input '", quote_name(event$name),
                      "' was set, but doesn't have an input binding."
                    ),
                    class = c("st2_comment", "character")
                  )
                } else {
                  args <- ""
                  if (!event$hasBinding && allow_input_no_binding) {
                    # TODO-barret; test
                    args <- paste0(args, ", allow_input_no_binding_ = TRUE")
                    if (identical(event$priority, "event")) {
                      args <- paste0(args, ', priority_ = "event"')
                    }
                  }

                  paste0(
                    "app$set_inputs(",
                    quote_name(event$name), " = ",
                    process_input_value(event$value, input_type = "default"),
                    args,
                    ")"
                  )
                }
              }
            },
            "outputEvent" = {
              structure("# output event", class = c("st2_comment", "character"))
            },
            stop(paste0("Unknown type: ", event$type))
          )
        event
      })

      events
    })

    # observeEvent({trim_testevents()}, {
    #   for (event in trim_testevents()) {
    #     if (event$type == "expectScreenshot") {
    #       if (!input$includeVariant) {
    #         updateCheckboxInput(inputId = "includeVariant", value = TRUE)
    #         return()
    #       }
    #     }
    #   }
    # })

    # Number of snapshot or fileDownload events in trim_testevents()
    num_snapshots <- reactive({
      snapshots <- vapply(trim_testevents(), function(event) {
        return(event$type %in% c("expectValues", "expectScreenshot", "expectDownload", "expectDownload"))
      }, logical(1))
      sum(snapshots)
    })


    output$recorded_events <- renderUI({
      events <- trim_testevents()
      event_codes <- lapply(events, `[[`, "app_code")
      event_codes <- event_codes[!vapply(event_codes, is.null, logical(1))]
      # Genereate list of lists from all event_codes. Inner lists have 'type' and
      # 'name' fields.
      tagList(
        tags$pre(div(
          .noWS = "outside",
          lapply(event_codes, function(event_code) {
            can_select <- !inherits(event_code, "st2_comment")
            # https://stackoverflow.com/a/64917958/591574
            tagList(
              tags$span(
                class = paste0(if (can_select) "line-number ", "no-select-code")
              ),
              tags$code(
                event_code,
                .noWS = "outside",
                class = if (!can_select) "no-select-code slant-code"
              )
            )
          })
        )),
        tags$script(HTML(
          '$("#recorded_events pre")[0].scrollTop = $("#recorded_events pre div")[0].scrollHeight;'
        ))
      )
    })

    save_and_exit <- function(delete_test_file = FALSE) {
      stopApp({

        seed <- as.integer(input$seed)
        if (is.null(seed) || is.na(seed))
          seed <- NULL

        code <- generate_test_code(
          trim_testevents(),
          input$testname,
          seed = seed
          # allow_input_no_binding = input$allow_input_no_binding
        )

        # (maybe) Remove prior file
        if (isTRUE(delete_test_file)) {
          unlink(save_file)
        }

        # Make sure folder exists
        dir.create(fs::path_dir(save_file), recursive = TRUE, showWarnings = FALSE)

        add_library_call <- TRUE
        if (file.exists(save_file)) {
          code <- paste0(code, "\n\n")
          # don't double library()
          add_library_call <- !any(grepl(readLines(save_file), "^library\\(shinytest2\\)$"))
        }
        if (add_library_call) {
          code <- paste0("library(shinytest2)\n\n", code)
        }
        cat(code, file = save_file, append = TRUE)
        message("Saved test code to ", save_file)

        invisible(list(
          test_file = test_file,
          run = TRUE && (length(dont_run_reasons) == 0),
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
          modalDialog("Must have at least one snapshot to save and exit.")
        )
        return()
      }

      p <- promise_resolve(TRUE)

      if (has_inputs_without_binding(trim_testevents()) && !input$allow_input_no_binding) {
        p <- p %...>% {
          present_modal(
            modalDialog(
              # TODO-barret; Change to a question
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
        if (file.exists(save_file)) {
          present_modal(
            modalDialog(
              paste0("Overwrite ", basename(save_file), "?"),
              footer = tagList(
                actionButton("overwrite_cancel",    "Cancel",    `data-dismiss` = "modal"),
                actionButton("overwrite_append",    "Append",    `data-dismiss` = "modal"),
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
