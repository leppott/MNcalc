# Prepare Data Panel, outside app

function() {
  fluidPage(
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction"
                 ,includeHTML(file.path("www"
                                        , "rmd_html"
                                        , "ShinyHTML_FB_OutApp1Intro.html"))
                 )
        , tabPanel("Bug BCG"
                   ,includeHTML(file.path("www"
                                          , "rmd_html"
                                          , "ShinyHTML_FB_OutApp2BugBCG.html"))
                   )

        , tabPanel("Fish BCG"
                   ,includeHTML(file.path("www"
                                          , "rmd_html"
                                          , "ShinyHTML_FB_OutApp3FishBCG.html"))
                 )
        , tabPanel("Bug IBI"
                   ,includeHTML(file.path("www"
                                          , "rmd_html"
                                          , "ShinyHTML_FB_OutApp4BugIBI.html"))
                   )
        , tabPanel("Fish IBI"
                   ,includeHTML(file.path("www"
                                          , "rmd_html"
                                          , "ShinyHTML_FB_OutApp5FishIBI.html"))
                   )
      )##tabsetPanel ~ END
    )##mainPanel ~ END
  )##fluidPage ~ END
}##FUNCTION ~ END





# function() {
#   mainPanel(
#   tabsetPanel("tab_filebuilder_outsideapp"
#            , tabPanel(title = "Introduction"
#                       , includeHTML(file.path("www"
#                                               , "rmd_html"
#                                               , "ShinyHTML_FB_outsideapp.html"))
#                       )
#            , tabPanel(title = "Bug BCG"
#                       , includeHTML(file.path("www"
#                                               , "rmd_html"
#                                               , "ShinyHTML_FB_outsideapp.html"))
#                       )
#            , tabPanel(title = "Fish BCG"
#                       , includeHTML(file.path("www"
#                                               , "rmd_html"
#                                               , "ShinyHTML_FB_outsideapp.html"))
#            )
#            , tabPanel(title = "Bug IBI"
#                       , includeHTML(file.path("www"
#                                               , "rmd_html"
#                                               , "ShinyHTML_FB_outsideapp.html"))
#            )
#            , tabPanel(title = "Fish IBI"
#                       , includeHTML(file.path("www"
#                                               , "rmd_html"
#                                               , "ShinyHTML_FB_outsideapp.html"))
#            )
#   )##tabSetPanel ~ END
#   )##MainPanel ~ END
# }##FUNCTION ~ END
