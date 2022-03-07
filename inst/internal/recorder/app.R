# TODO-barret; record many many tests
# TODO-barret; find all names of existing appdriver new names; If match is found, use `name-X` where `X` is a number that gets inc larger.


library(shiny)
library(promises)

target_url    <- getOption("shinytest2.recorder.url")
app           <- getOption("shinytest2.app")
app_name      <- getOption("shinytest2.name")
load_timeout  <- getOption("shinytest2.load.timeout")
start_seed    <- getOption("shinytest2.seed")
shiny_args    <- getOption("shinytest2.shiny.args")
save_file     <- getOption("shinytest2.test_file")
allow_no_input_binding <- getOption("shinytest2.allow_no_input_binding")

# If there are any reasons to not run a test, a message should be appended to
# this vector.
dont_run_reasons <- character(0)
add_dont_run_reason <- function(reason) {
  dont_run_reasons <<- c(dont_run_reasons, reason)
}

if (is.null(target_url) || is.null(app$get_path())) {
  abort(paste0("Test recorder requires the 'shinytest2.recorder.url' and ",
    "'shinytest2.app' options to be set."))
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
  tagAppendAttributes(
    shiny::numericInput(...),
    placeholder = placeholder,
    .cssSelector = "input"
  )
}

# Create a question mark icon that displays a tooltip when hovered over.
tooltip <- function(text, placement = "top") {
  span(
    `data-toggle` = "tooltip",
    title = text,
    icon("question-sign", lib = "glyphicon"),
    `data-placement` = placement,
    `data-html` = "true",
    `data-container` = "body"
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


app_dir <- function() {
  path <- app$get_path()
  if (shinytest2:::is_rmd(path)) {
    path <- fs::path_dir(path)
  }
  path
}
test_save_file <- file.path(app_dir(), "tests", "testthat", save_file)
app_test_path <- function() {
  path <- app$get_path()
  # NULL maps to `../../`
  if (dir.exists(path)) return(NULL)
  # TODO-barret; test for RMD
  # Return .Rmd file name
  rel_path <- fs::path_rel(path, fs::path_dir(test_save_file))
  paste0("test_path(", deparse2(rel_path), ")")
}


generate_test_code <- function(events, name, seed) {

  # Remove st2_comment code events
  height <- NULL
  width <- NULL
  event_code <- unlist(lapply(events, function(event) {
    if (inherits(event$app_code, "st2_comment")) {
      return(NULL)
    }
    switch(event$type,
      "setWindowSize" = {
        if (isTRUE(event$first_set_window_size)) {
          height <<- event$height
          width <<- event$width
          NULL
        } else {
          event$app_code
        }
      },
      event$app_code
    )
  })) # Unlist to remove `NULL`s
  event_code <- paste0(event_code, collapse = "\n")

  has_expect_screenshot <- any(unlist(lapply(events, `[[`, "type")) == "expectScreenshot")

  # From the tests dir, it is up two folders and then the app file
  inner_code <- paste(
    paste0(
      "app <- AppDriver$new(\n",
      "  ", paste(c(
        app_test_path(),
        # TODO-future; Should this value be a parameter?
        # Going with "no" for now as it is difficult to capture the expression
        # when nothing else is an expression
        if (has_expect_screenshot) "variant = platform_variant()",
        if (isTRUE(nzchar(name))) paste0("name = ", deparse2(name)),
        if (!is.null(seed)) paste0("seed = ", seed),
        if (!is.null(height)) paste0("height = ", height),
        if (!is.null(width)) paste0("width = ", width),
        if (!is.null(load_timeout)) paste0("load_timeout = ", load_timeout),
        if (length(shiny_args) > 0) paste0("shiny_args = ", deparse2(shiny_args)),
        NULL # used for trailing comma
        ),
        collapse = ",\n  "
      ), "\n",
      ")"
    ),
    event_code,
    sep = "\n"
  )
  # Use R's default formatter to wrap the code
  inner_code <-
    paste0(
      lapply(parse(text = inner_code), rlang::expr_text, width = 78L),
      collapse = "\n"
    )
  inner_code <- gsub("\n", "\n  ", paste0("  ", inner_code))

  ret <- paste0(
    "test_that(\"{shinytest2} recording: ", name, "\", {\n",
    inner_code, "\n",
    "})\n"
  )

  ret
}

has_inputs_without_binding <- function(events) {
  any(vapply(events, function(event) {
    return(event$type == "inputEvent" && !event$hasBinding)
  }, TRUE))
}


# Keep a pointer to the last err/std lines that were printed.
# Only display the new ones if the recorder is refreshed
n_console_err_lines <- 0
n_console_std_lines <- 0

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
      div(class = "shiny-recorder-header", tags$code("{shinytest2}"), "expections"),
      div(class = "shiny-recorder-controls form-group",
        actionButton("values",
          span(
            img(src = "shiny.png", class = "shiny-recorder-icon", style = "height: 23px;vertical-align: middle;"),
            "Expect Shiny values"
          )
        ),
        tooltip(
          HTML("To capture all Shiny values via the keyboard, press Ctrl-shift-V or or &#8984;-shift-V.<br/>You can also Ctrl-click or &#8984;-click on an input/output to capture just that one input/output."),
          placement = "bottom"
        ),
        actionButton("screenshot",
          span(
            img(src = "snapshot.png", class = "shiny-recorder-icon"),
            "Expect screenshot",
            style = "display: inline;"
          )
        ),
        tooltip(
          HTML("To trigger a screenshot via the keyboard, press Ctrl-shift-S or &#8984;-shift-S"),
          placement = "bottom"
        ),
      ),
      div(class = "shiny-recorder-header", "Code"),
      uiOutput("recorded_events"),
      div(class = "shiny-recorder-header", "Save"),
      div(id = "save-and-quit",
        tagAppendChild(
          tagAppendAttributes(
            textInput("testname", label = "Test name:", value = app_name),
            class = "inline-input-container",
          ),
          tooltip("The name of the test should be short, unique, and path-friendly way to describe what the set of expectations are trying to confirm."),
        ),
        tagAppendChild(
          tagAppendAttributes(
            numeric_input("seed",
              label = "Random seed:",
              value = start_seed,
              min = 0,
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
          ),
          class = "disabled",
          title = "Perform an \"Expectation\" to enable \"Save test and exit\" button"
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
    observe({
      invalidateLater(500)
      logs <- subset(app$get_log(), location == "shiny")

      print_logs <- function(..., n) {
        logs_sub <- subset(logs, ...)
        n_sub <- nrow(logs_sub)
        if (n_sub > n) {
          print(logs_sub[seq.int(n + 1, n_sub), ])
          cat("\n")
        }
        n_sub
      }

      n_console_err_lines <<- print_logs(level == "error", n = n_console_err_lines)
      n_console_std_lines <<- print_logs(level != "error", n = n_console_std_lines)
    })

    allow_no_input_binding_react <- reactiveVal(allow_no_input_binding)

    trim_testevents <- reactive({
      events <- input$testevents

      has_removed <- TRUE
      # Might repeat ~ 3 times
      while (has_removed) {
        has_removed <- FALSE
        to_remove <- c()

        for (i in seq_along(events)) {
          if (i == 1) next
          prev_event <- events[[i - 1]]
          curr_event <- events[[i]]
          if (prev_event$type == curr_event$type) {
            switch(curr_event$type,
              "outputEvent" = , # nolint
              "waitForIdle" = , # nolint
              "setWindowSize" = {
                # Remove previous event
                to_remove[length(to_remove) + 1] <- i - 1
              },
              "inputEvent" = {
                if (!isTRUE(allow_no_input_binding_react())) {
                  if (!curr_event$hasBinding && !prev_event$hasBinding) {
                    to_remove[length(to_remove) + 1] <- i
                  }
                }
              }
            )
          } else if (
            i >= 3 &&
            curr_event$type == "setWindowSize" &&
            prev_event$type == "outputEvent" &&
            events[[i - 2]]$type == "setWindowSize"
          ) {
            # If two setWindowSize events sandwich an outputEvent,
            # remove the first setWindowSize
            to_remove[length(to_remove) + 1] <- i - 2
          }
        }

        if (length(to_remove)) {
          has_removed <- TRUE
          events <- events[-to_remove]
        }
      }

      found_first_set_window_size <- FALSE

      events <- lapply(events, function(event) {
        event$app_code <-
          switch(event$type,
            "initialize" = NULL,
            "waitForIdle" = "app$wait_for_idle()",
            "setWindowSize" = {
              code <- paste0("app$set_window_size(width = ", event$width, ", height = ", event$height, ")")
              if (shinytest2:::is_false(found_first_set_window_size)) {
                found_first_set_window_size <<- TRUE
                event$first_set_window_size <- TRUE
              }
              code
            },
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
                if (!all(fs::file_exists(filepaths))) {
                  # TODO-barret; test
                  add_dont_run_reason("An uploadFile() must be updated: use the correct path relative to the app's ./tests/testthat directory, or copy the file to the app's ./tests/testthat directory.")
                  code <- paste0(code,
                    " # <-- This should be the path to the file, relative to the app's tests/testthat directory"
                  )
                }

                code
              } else {
                if (!event$hasBinding && !isTRUE(allow_no_input_binding_react())) {
                  structure(
                    paste0(
                      "# Update unbound `input` value"
                    ),
                    class = c("st2_comment", "character")
                  )
                } else {
                  args <- ""
                  if (!event$hasBinding && isTRUE(allow_no_input_binding_react())) {
                    # TODO-barret; test
                    args <- paste0(args, ", allow_no_input_binding_ = TRUE")
                    if (identical(event$priority, "event")) {
                      args <- paste0(args, ', priority_ = "event"')
                    }
                  }

                  paste0(
                    "app$set_inputs(",
                    quote_name(event$name), " = ",
                    value,
                    args,
                    ")"
                  )
                }
              }
            },
            "outputEvent" = {
              structure("# Update output value", class = c("st2_comment", "character"))
            },
            stop(paste0("Unknown type: ", event$type))
          )
        event
      })

      events
    })

    has_expectation_event <- reactive({
      for (event in trim_testevents()) {
        switch(event$type,
          "expectValues" = , # nolint
          "expectScreenshot" = , # nolint
          "expectDownload" = {
            return(TRUE)
          }
        )
      }
      FALSE
    })

    testname_validator <- function(name) {
      if (is.null(name)) return()
      if (!fs::file_exists(test_save_file)) return()

      cur_test_names <- known_app_driver_name_values(test_save_file)
      # Convert names to chars
      cur_test_names <- unique(as.character(lapply(cur_test_names, function(x) {
        x %||% "`NULL`"
      })))
      if (name %in% cur_test_names) {
        shiny::tags$div(
          "Please use a unique name. Known names:",
          shiny::tags$ul(
            lapply(cur_test_names, shiny::tags$li)
          )
        )
      }
    }
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("testname", testname_validator)
    iv$add_rule("seed", shinyvalidate::sv_integer(allow_na = TRUE))
    iv_screenshot <- shinyvalidate::InputValidator$new()
    iv_screenshot$condition(~ !has_expectation_event())
    iv_screenshot$add_rule("screenshot", ~ "At least one expectation must be made")

    iv$add_validator(iv_screenshot)
    iv$enable()

    # Use reactiveVal dedupe feature
    save_enabled <- reactiveVal(FALSE)
    save_enable_obs <- observe({
      enable_save <- iv$is_valid()
      save_enabled(enable_save)
      session$sendCustomMessage("enable_save_button", enable_save)
    })

    # If an unbound input value is updated, ask the user if the event should be recorded
    no_binding_obs <- list()
    no_binding_obs[[1]] <- observeEvent(trim_testevents(), {
      if (!is.null(allow_no_input_binding_react())) {
        # Cancel the observers and return
        lapply(no_binding_obs, function(ob) { ob$destroy() })
        no_binding_obs <<- list()
        return();
      }

      # Don't do anything if there is no unbound input event
      if (!has_inputs_without_binding(trim_testevents())) {
        return()
      }

      no_binding_obs[[2]] <<-
        observeEvent(
          input$inputs_no_binding_ignore,
          {
            allow_no_input_binding_react(FALSE)
          },
          ignoreInit = TRUE
        )
      no_binding_obs[[3]] <<-
        observeEvent(
          input$inputs_no_binding_save,
          {
            allow_no_input_binding_react(TRUE)
          },
          ignoreInit = TRUE
        )

      showModal(
        modalDialog(
          tagList(
            "An update input event does not have a corresponding input binding.", tags$br(),
            tags$ul(
              tags$li("Click", tags$code("Record"), " to record updates to", tags$code("input"), "without a binding."),
              tags$li("Click", tags$code("Ignore"), " to discard these events."),
            ),
            # tags$br(),
          ),
          footer = tagList(
            actionButton("inputs_no_binding_ignore", "Ignore", `data-dismiss` = "modal"),
            actionButton("inputs_no_binding_save",   "Record", `data-dismiss` = "modal"),
            tooltip(tagList(
              "To prevent this modal from being displayed, set the parameter", tags$br(),
              tags$ul(
                tags$li(tags$code("record_test(allow_no_input_binding = TRUE)"), "to", tags$strong("record"), "these events."),
                tags$li(tags$code("record_test(allow_no_input_binding = FALSE)"), "to", tags$strong("ignore"), "these events.")
              )
            ), placement = "left"),
            enable_tooltip_script(),
          )
        )
      )
    })

    output$recorded_events <- renderUI({
      events <- trim_testevents()
      event_codes <- unlist(lapply(events, `[[`, "app_code")) # Unlist to remove `NULL`s
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

    observeEvent(input$exit_save, {
      req(save_enabled())

      stopApp({
        seed <- as.integer(input$seed)
        if (is.null(seed) || is.na(seed)) {
          seed <- NULL
        }

        code <- generate_test_code(
          trim_testevents(),
          input$testname,
          seed = seed
        )
        # Add separator lines between code and prior tests
        code <- paste0("\n\n", code)

        # Make sure tests folder exists.
        fs::dir_create(fs::path_dir(test_save_file), recurse = TRUE)

        add_library_call <- TRUE
        if (fs::file_exists(test_save_file)) {
          # Don't double library()
          add_library_call <- !any(grepl("^\\s*library\\s*\\(\\s*shinytest2\\s*\\)\\s*$", readLines(test_save_file)))
        }
        if (add_library_call) {
          code <- paste0("library(shinytest2)", code)
        }

        # TODO-barret; Save runner file
        test_runner_file <- fs::path(fs::path_dir(fs::path_dir(test_save_file)), "testthat.R")
        overwrite_test_runner <-
          if (fs::file_exists(test_runner_file)) {
            if (!any(grepl("test_app(", readLines(test_runner_file), fixed = TRUE))) {
              rlang::warn(paste0("Overwriting test runner ", fs::path_rel(test_runner_file, app$get_path()), " with `shinytest2::test_app()` call to ensure proper a testing environment."))
              # Runner exists. Overwrite existing contents
              TRUE
            } else {
              # Runner exists. Don't overwrite existing contents
              FALSE
            }
          } else {
            # File missing. Create it.
            TRUE
          }
        if (overwrite_test_runner) {
          shinytest2:::use_shinytest2_runner(app$get_path(), quiet = FALSE)
        }

        rlang::inform(paste0("Saving test file: ", fs::path_rel(test_save_file, app$get_path())))
        cat(code, file = test_save_file, append = TRUE)

        invisible(list(
          test_file = test_save_file,
          dont_run_reasons = dont_run_reasons
        ))
      })
    })
    observeEvent(input$exit_nosave, {
      stopApp({
        invisible(list(
          test_file = NULL,
          dont_run_reasons = NULL
        ))
      })
    })
  }
)
