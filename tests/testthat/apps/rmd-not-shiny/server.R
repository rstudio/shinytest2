function(input, output) {

  reg_formula <- reactive({
    as.formula(paste("mpg ~", input$x))
  })

  output$reg_plot <- renderPlot({
    par(mar = c(4, 4, .1, .1))
    plot(reg_formula(), data = mtcars, pch = 19)
  })

  output$format_type <- renderText({
    input$format
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste("my-report", sep = ".", switch(
        input$format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },

    content = function(file) {
      src <- normalizePath("report.Rmd")

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)

      library(rmarkdown)
      out <- render("report.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )

}
