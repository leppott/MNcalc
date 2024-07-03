# Resources Panel

function() {
  tabPanel("tabpan_references"
           , h2("References")
           # , fluidPage(h2("About"
           #                #, style  = "text-align:center"
           #                )
           #             , p("Background info")
           #
           #             , p("Version 0.5.0.9080")
           #
           #
           #              )## fluidPage ~ END
            , includeHTML(file.path("www", "rmd_html", "ShinyHTML_References.html"))
         #  , htmlOutput("html_about") # use with iframe
           )##tabPanel ~ END
}##FUNCTION ~ END

# output$UI_about = renderUI({
#   p("About stuff here.")
# })
