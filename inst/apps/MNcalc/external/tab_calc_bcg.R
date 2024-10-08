# Calculate BCG Panel

function() {
  sidebarLayout(
    sidebarPanel(
       h2("Calculate BCG Models")
       , p("This function will render all steps and make available files for download.")
       , br()

       , h4("A. Upload a file")
       , p("If no file name showing below repeat 'Import File' in the left sidebar.")
       , p(textOutput("fn_input_display_bcg"))

       , h4("B. Define Community (for metrics)")
       , selectInput("si_community"
                     , label = "Community"
                     , choices = c("", sel_community))

       , h4("C. Mark Redundant (Non-Distinct) Taxa")
       , includeHTML(file.path("www", "rmd_html", "ShinyHTML_RedundantTaxa.html"))
       , checkboxInput("ExclTaxa"
                       , "Generate Redundant Taxa Column"
                       , TRUE)

       , h4("D. Define BCG Model")
       , p("Determined by INDEX_NAME and INDEX_CLASS in data input file.")
       # , selectInput("si_model"
       #               , label = "BCG Model"
       #               , choices = sel_bcg_models
       #               , selected = "BCG_MariNW_Bugs500ct")
       #, uiOutput("UI_col_calcmet_Cols2Keep")

       , h4("E. Run Calculations")
       , p("This button will calculate metrics values, metric memberships
           , level membership, and level assignment.")
       , useShinyjs()
       , shinyjs::disabled(shinyBS::bsButton("b_calc_bcg"
                                             , label = "Run Calculations"))

       , h4("F. Download Results")
       , p("All input and output files will be available in a single zip file.")
       , shinyjs::disabled(downloadButton("b_download_bcg"
                                          , "Download Results"))
        )## sidebarPanel ~ END
    , mainPanel(
        tabsetPanel(type = "tabs"
                    , tabPanel(title = "Introduction"
                               , includeHTML(file.path("www"
                                                      , "rmd_html"
                                            , "ShinyHTML_Calc_BCG_1Intro.html"))
                               )
                    , tabPanel(title = "Bug BCG"
                               , includeHTML(file.path("www"
                                                      , "rmd_html"
                                            , "ShinyHTML_Calc_BCG_2Bug.html"))
                               , value = "tab_BCG_Bugs")
                    , tabPanel(title = "Fish BCG"
                               , includeHTML(file.path("www"
                                                      , "rmd_html"
                                          , "ShinyHTML_Calc_BCG_3Fish.html"))
                               , value = "tab_BCG_Fish"
                               )
                    )## tabsetPanel ~ END

    )## mainPanel ~ END
  )##sidebarLayout ~ END
}##FUNCTION ~ END
