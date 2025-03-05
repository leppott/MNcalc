#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# nolint start
library(shiny)
# nolint end

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # INPUT Display Names ####

  output$fn_input_display_bcg <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_bcg

  output$fn_input_display_ibi <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_ibi

  output$fn_input_display_taxatrans <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_taxatrans

  output$fn_input_display_indexclass <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_indexclass

  output$fn_input_display_indexclassparam <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_indexclassparam

  output$fn_input_display_met_therm <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_met_therm

  output$fn_input_display_modtherm <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_modtherm

  output$fn_input_display_mtti <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_mtti

  output$fn_input_display_bdi <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_bdi

  output$fn_input_display_map <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_map

  output$fn_input_display_rep_bio <- renderText({
    inFile <- input$fn_input_rep_bio

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_rep_bio

  output$fn_input_display_rep_multi <- renderText({
    inFile <- input$fn_input_rep_multi

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_rep_multi

  # ~~~~IMPORT~~~~----
  # IMPORT ----
  file_watch <- reactive({
    # trigger for df_import()
    input$fn_input
  })## file_watch

  ## IMPORT, df_import ####
  df_import <- eventReactive(file_watch(), {
    # use a multi-item reactive so keep on a single line (if needed later)

    # input$df_import will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$fn_input

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    sep_user <- input$sep

    # Define file
    fn_inFile <- inFile$datapath

    #message(getwd())
    message(paste0("Import, separator: '", input$sep,"'"))
    message(paste0("Import, file name: ", input$fn_input$name))

    # # Add "Results" folder if missing
    # boo_Results <- dir.exists(file.path(".", "results"))
    # if(boo_Results==FALSE){
    #   dir.create(file.path(".", "Results"))
    # }

    # Remove existing files in "results"
    clean_results()

    ### Mod, BCG_ATTR----
    # Read user imported file
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # many permutations of BCG_Attr so check for it first then import

    df_header <- read.delim(fn_inFile
                            , header = TRUE
                            , sep = sep_user
                            , stringsAsFactors = FALSE
                            , na.strings = c("", "NA")
                            , nrows = 0)
    col_num_bcgattr <- grep("BCG_ATTR", toupper(names(df_header)))
    classes_df <- sapply(df_header, class)
    col_name_bcgattr <- names(df_header)[col_num_bcgattr]

    # 2023-12-04, modify for MN with multiple BCG_ATTR fields

    if (length(col_num_bcgattr) == 0) {
      # BCG_Attr present = FALSE
      # define classes = FALSE
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
    } else if (!"complex" %in%  as.vector(classes_df[col_num_bcgattr])) {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = FALSE
      # define classes on import = FALSE (change to text after import)
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      #df_input[, col_num_bcgattr] <- as.character(df_input[, col_num_bcgattr])
      # doesn't work for multiple columns
      # df_input[, col_num_bcgattr] <- lapply(df_input[, col_num_bcgattr], as.character)
      # error for some files
      # use a loop :(
      for (b in col_num_bcgattr) {
        df_input[, b] <- as.character(df_input[, b])
      }## FOR ~ b
      #
    } else {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = TRUE
      # define classes on import = TRUE
      #classes_df <- sapply(df_header, class)
      classes_df[col_num_bcgattr] <- "character"
      df_input <- read.table(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA")
                             #, colClasses = c(col_name_bcgattr = "character"))
                             # , colClasses = classes_df)
                             , colClasses = classes_df[col_name_bcgattr])
    }## IF ~ col_num_bcgattr



    # OLD
    # Will get a 'warning' for unknown columns but harmless
    # df_input <- read.delim(fn_inFile
    #                        , header = TRUE
    #                        , sep = ","
    #                        , stringsAsFactors = FALSE
    #                        , colClasses = c("BCG_Attr" = "character"
    #                                         , "BCG_ATTR" = "character"
    #                                         , "bcg_attr" = "character"
    #                                         , "BCG_attr" = "character"))

    # Copy user files to results sub-folder
    copy_import_file(import_file = input$fn_input)

    ## button, enable, calc ----
    shinyjs::enable("b_calc_taxatrans")
    shinyjs::enable("b_calc_indexclass")
    shinyjs::enable("b_calc_indexclassparam")
    shinyjs::enable("b_calc_bcg")
    shinyjs::enable("b_calc_ibi")
    shinyjs::enable("b_calc_met_therm")
    shinyjs::enable("b_calc_modtherm")
    shinyjs::enable("b_calc_mtti")
    shinyjs::enable("b_calc_bdi")

    # shinyjs::enable("b_calc_rep_bio")
    # shinyjs::enable("b_calc_rep_multi")

    # update cb_taxatrans_sum
    # doesn't work here as timing is after the file is created

    return(df_input)

  })##output$df_import ~ END

  ## IMPORT, df_import_DT ----
  output$df_import_DT <- DT::renderDT({
    df_data <- df_import()
  }##expression~END
  , filter = "top"
  , caption = "Table. Imported data."
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                   , autoWidth = TRUE)
  )##df_import_DT~END

  ## IMPORT, col names ----
  col_import <- eventReactive(file_watch(), {

    inFile <- input$fn_input

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    # temp df
    df_temp <- df_import()
    # Column Names
    input_colnames <- names(df_temp)
    #
    return(input_colnames)

  })## col_import

  # ~~~~FILE BUILDER~~~~ ----
  # FB, TAXATRANS ----
  ## TaxaTrans, UI ----

  # output$UI_taxatrans_pick_official <- renderUI({
  #   str_col <- "Calculation"
  #   selectInput("taxatrans_pick_official"
  #               , label = str_col
  #               , choices = c("", df_pick_taxoff[, "project"])
  #               , multiple = FALSE)
  # })## UI_colnames

  # output$UI_taxatrans_pick_official_project <- renderUI({
  #   str_col <- "Official Taxa Data, Column Taxa_ID"
  #   selectInput("taxatrans_pick_official_project"
  #               , label = str_col
  #               , choices = names(df_pick_taxoff)
  #               , multiple = FALSE)
  # })## UI_colnames

  output$UI_taxatrans_user_col_taxaid <- renderUI({
    str_col <- "Column, TaxaID"
    selectInput("taxatrans_user_col_taxaid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "TaxaID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_drop <- renderUI({
    str_col <- "Columns to Drop"
    selectInput("taxatrans_user_col_drop"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_n_taxa <- renderUI({
    str_col <- "Column, Taxa Count (number of individuals or N_Taxa)"
    selectInput("taxatrans_user_col_n_taxa"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "N_Taxa"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_groupby <- renderUI({
    str_col <- "Columns to Keep in Output"
    selectInput("taxatrans_user_col_groupby"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_sampid <- renderUI({
    str_col <- "Column, Unique Sample Identifier (e.g., SampleID)"
    selectInput("taxatrans_user_col_sampid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "SampleID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_indexname <- renderUI({
    str_col <- "Column, Index Name (e.g., Index_Name)"
    selectInput("taxatrans_user_col_indexname"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Index_Name"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_indexclass <- renderUI({
    # str_col <- "BCG (Bugs and Fish): Column, Index Class (e.g., Index_Class)"
    str_col <- "Column, Index Class (e.g., Index_Class)"
    selectInput("taxatrans_user_col_indexclass"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Index_Class"
                , multiple = FALSE)
  })## UI_colnames

  # output$UI_taxatrans_user_col_gprr <- renderUI({
  #   str_col <- "MN_BCG_Bugs only: Column, GP/RR (e.g., GP_RR)"
  #   selectInput("taxatrans_user_col_gprr"
  #               , label = str_col
  #               , choices = c("", names(df_import()))
  #               , selected = "GP_RR"
  #               , multiple = FALSE)
  # })## UI_colnames


  # ## TaxaTrans, combine ----
  # observeEvent(input$cb_TaxaTrans_Summ, {
  #   # turn on/off extra selection boxes based on checkbox
  #   if(input$cb_TaxaTrans_Summ == TRUE) {
  #     shinyjs::enable("UI_taxatrans_user_col_n_taxa")
  #     shinyjs::enable("UI_taxatrans_user_col_groupby")
  #   } else {
  #     shinyjs::disable("UI_taxatrans_user_col_n_taxa")
  #     shinyjs::disable("UI_taxatrans_user_col_groupby")
  #   }## IF ~ checkbox
  #
  # }, ignoreInit = FALSE
  # , ignoreNULL = FALSE)## observerEvent ~ cb_TaxaTrans_Summ
  # #})


  ## b_Calc_TaxaTrans ----
  observeEvent(input$b_calc_taxatrans, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 00, Initialize ----
      prog_detail <- "Calculation, Taxa Translator..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 6
      prog_sleep <- 0.25

      ## Calc, 01, Import User Data ----
      prog_detail <- "Import Data, User"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")
      # path_results_sub <- file.path(path_results
      #                               , paste(abr_results, fn_abr, sep = "_"))
      # # Add "Results" folder if missing
      # boo_Results <- dir.exists(file.path(path_results_sub))
      # if (boo_Results == FALSE) {
      #   dir.create(file.path(path_results_sub))
      # }
      # Add "reference" folder if missing
      path_results_ref <- file.path(path_results, dn_files_ref)
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }
      # Add "Results" folder based on user selection later in this step

      # button, disable, download
      shinyjs::disable("b_download_taxatrans")

      # Import data
      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Fun Param, Define
      sel_proj <- input$taxatrans_pick_official
      sel_user_taxaid <- input$taxatrans_user_col_taxaid
      #sel_col_drop <- unlist(input$taxatrans_user_col_drop)
      sel_user_ntaxa <- input$taxatrans_user_col_n_taxa
      sel_user_groupby <- unlist(input$taxatrans_user_col_groupby)
      sel_summ <- input$cb_TaxaTrans_Summ
      sel_user_indexname <- input$taxatrans_user_col_indexname
      sel_user_indexclass <- input$taxatrans_user_col_indexclass
      sel_user_gprr <- "GP.RR" # input$taxatrans_user_col_gprr
      # require name

      fn_taxoff <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                  , "filename"]
      fn_taxoff_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "metadata_filename"]
      col_taxaid_official_match <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                  , "taxaid"]
      col_taxaid_official_project <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                    , "calc_taxaid"]
      col_drop_project <- unlist(strsplit(df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                         , "col_drop"], ","))
      fn_taxoff_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "attributes_filename"]
      fn_taxoff_attr_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "attributes_metadata_filename"]
      col_taxaid_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                        , "attributes_taxaid"]
      sel_user_sampid <- input$taxatrans_user_col_sampid

      sel_taxaid_drop <-  df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                     , "taxaid_drop"]
      dir_proj_results <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                         , "dir_results"]

      ### MN, col, groupby ----
      # Auto add bug or fish columns for selecting correct BCG_ATTR
      # Ensure doesn't double count (unique)
      if (sel_proj == "MN BCG (bugs)") {
        sel_user_groupby <- unique(c(sel_user_groupby
                                     , sel_user_indexclass
                                     , sel_user_gprr))
      } else if (sel_proj == "MN IBI (bugs)") {
        # future use, add region
      } else if (sel_proj == "MN BCG (fish)") {
        sel_user_groupby <- unique(c(sel_user_groupby
                                     , sel_user_indexclass))
      } else if (sel_proj == "MN IBI (fish)") {
        # future use, add region
      }## IF ~ sel_proj

      # include = yes; unique(sel_user_groupby)
      # include sampid, taxaid, and n_taxa so not dropped
      user_col_keep <- names(df_input)[names(df_input) %in% c(sel_user_groupby
                                                              , sel_user_sampid
                                                              , sel_user_taxaid
                                                              , sel_user_ntaxa
                                                              , sel_user_indexname
                                                              , sel_user_indexclass
                                                              , sel_user_gprr)]
      # flip to col_drop
      user_col_drop <- names(df_input)[!names(df_input) %in% user_col_keep]

      # Fun Param, Test

      if (sel_proj == "") {
        # end process with pop up
        msg <- "'Calculation' is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        # validate(msg)
      }## IF ~ sel_proj

      if (is.na(fn_taxoff_meta) | fn_taxoff_meta == "") {
        # set value to NULL
        df_official_metadata <- NULL
      }## IF ~ fn_taxaoff_meta

      if (is.na(sel_user_ntaxa) | sel_user_ntaxa == "") {
        sel_user_ntaxa <- NULL
      }## IF ~ fn_taxaoff_meta

      if (is.null(sel_summ)) {
        sel_summ <- FALSE
      }## IF ~ sel_summ

      if (sel_taxaid_drop == "NULL") {
        sel_taxaid_drop <- NULL
      }## IF ~ sel_taxaid_drop

      if (is.null(sel_user_indexname)) {
        sel_user_indexname <- NULL
      } else if (is.na(sel_user_indexname)) {
        sel_user_indexname <- NULL
      } else if (sel_user_indexname == "") {
        sel_user_indexname <- NULL
      }## IF ~ sel_user_indexname


      if (is.null(sel_user_indexclass)) {
        sel_user_indexclass <- NULL
      } else if(is.na(sel_user_indexclass)) {
        sel_user_indexclass <- NULL
      } else if (sel_user_indexclass == "") {
        sel_user_indexclass <- NULL
      }## IF ~ sel_user_indexclass

      if (is.null(sel_user_gprr)) {
        sel_user_gprr <- NULL
      } else if(is.na(sel_user_gprr)) {
        sel_user_gprr <- NULL
      } else if (sel_user_gprr == "") {
        sel_user_gprr <- NULL
      }## IF ~ sel_user_gprr

      message(paste0("User response to summarize duplicate sample taxa = "
               , sel_summ))

      # Different result subfolder based on project (bugs/fish and BCG/IBI)
      # 2023-12-14
      if (sel_proj == "MN_BCG_Bugs") {
        dir_proj_results <- paste("Bugs", dir_proj_results, sep = "_")
        fn_save_calc <- "BCG"
      } else if (sel_proj == "MN_IBI_Bugs") {
        dir_proj_results <- paste("Bugs", dir_proj_results, sep = "_")
        fn_save_calc <- "IBI"
      } else if (sel_proj == "MN_BCG_Fish") {
        dir_proj_results <- paste("Fish", dir_proj_results, sep = "_")
        fn_save_calc <- "BCG"
      } else if (sel_proj == "MN_IBI_Fish") {
        dir_proj_results <- paste("Fish", dir_proj_results, sep = "_")
        fn_save_calc <- "IBI"
      }## IF ~ sel_proj

      dn_files <- paste(abr_results, dir_proj_results, sep = "_")

      # Add "Results" folder if missing
      path_results_sub <- file.path(path_results, dn_files)
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      ### MN  ----
      if (is.null(sel_user_indexname)) {
        # end process with pop up
        msg <- "'Index_Name' column name is required and is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sel_user_indexname

      if (is.null(sel_user_indexclass) &
          (sel_proj == "MN_BCG_Bugs" | sel_proj == "MN_BCG_Fish")) {
        # end process with pop up
        msg <- "'Index_Class' column name is required for BCG and is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sel_user_indexclass

      #if (is.null(sel_user_gprr) & sel_proj == "MN_BCG_Bugs") {
      if (!(sel_user_gprr %in% sel_user_groupby) & sel_proj == "MN_BCG_Bugs") {
        # end process with pop up
        msg <- "'GP.RR' column name is required for BCG_Bugs and is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sel_user_gprr

      ### Check TaxaID for bad characters----
      tnames_user <- sort(unique(df_input[, sel_user_taxaid]))
      tnames_iconv <- iconv(tnames_user)
      tnames_bad <- tnames_user[is.na(tnames_iconv) |
                                  tnames_user != tnames_iconv]
      tnames_recnum <- which(df_input[, sel_user_taxaid] %in% tnames_bad)
      if (length(tnames_bad) != 0) {
        # end process with pop up
        msg <- paste0("Bad (non-ASCII) characters in taxa names!"
                      , "\n\n"
                      , "Imported file record numbers:"
                      , "\n"
                      , "R doesn't count the title row so add one to get the row number in Excel."
                      , "\n\n"
                      , paste(tnames_recnum, collapse = "\n")
        )
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sel_user_taxaid ~ non-ASCII

      ## Calc, 03, Import Official Data (and Metadata)  ----
      prog_detail <- "Import Data, Official and Metadata"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      ## Data,  Official Taxa----
      url_taxoff <- file.path(url_bmtsf_base
                              , "taxa_official"
                              , "MN"
                              , fn_taxoff)
      temp_taxoff <- tempfile(fileext = ".csv")
      httr::GET(url_taxoff, write_disk(temp_taxoff))

      df_taxoff <- read.csv(temp_taxoff)

      ## Data, Official Taxa, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_meta <- file.path(url_bmtsf_base
                                     , "taxa_official"
                                     , "MN"
                                     , fn_taxoff_meta)
        temp_taxoff_meta <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_meta, write_disk(temp_taxoff_meta))

        df_taxoff_meta <- read.csv(temp_taxoff_meta)
      }## IF ~ fn_taxaoff_meta

      ## Data, Official Attributes----
      if (!is.null(fn_taxoff_attr)) {
        url_taxoff_attr <- file.path(url_bmtsf_base
                                     , "taxa_official"
                                     , "MN"
                                     , fn_taxoff_attr)
        temp_taxoff_attr <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_attr, write_disk(temp_taxoff_attr))

        df_taxoff_attr <- read.csv(temp_taxoff_attr)
      }## IF ~ fn_taxoff_attr

      ## Data, Official Attributes, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_attr_meta <- file.path(url_bmtsf_base
                                     , "taxa_official"
                                     , "MN"
                                     , fn_taxoff_attr_meta)
        temp_taxoff_attr_meta <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_attr_meta, write_disk(temp_taxoff_attr_meta))

        df_taxoff_attr_meta <- read.csv(temp_taxoff_attr_meta)
      }## IF ~ fn_taxaoff_meta


      ## Calc, 03, Run Function ----
      prog_detail <- "Calculate, Taxa Trans"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # QC for all necessary fields (outside those from the dropdowns)
      myIndexName <- sel_user_indexname
      req_BCG_bugs <- c("GP.RR")
      req_IBI_bugs <- c("LargeRareCount", "DRAINSQMI")
      req_BCG_fish <- c("DRAINSQMI")
      req_IBI_fish <- c("N_Anomalies", "DRAINSQMI", "GRADIENT", "Distance_m")

      if (myIndexName == "MN_BCG_Bugs" && any(!(req_BCG_bugs %in% sel_user_groupby))) {
          msg <- "One or more of the required fields for the BCG_Bugs are not included. See table to the right."
          shinyalert::shinyalert(title = "Taxa Translate"
                                 , text = msg
                                 , type = "error"
                                 , closeOnEsc = TRUE
                                 , closeOnClickOutside = TRUE)
          validate(msg)
      } else if (myIndexName == "MN_IBI_Bugs" && any(!(req_IBI_bugs %in% sel_user_groupby))) {
        msg <- "One or more of the required fields for the IBI_Bugs are not included. See table to the right."
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      } else if (myIndexName == "MN_BCG_Fish" && any(!(req_BCG_fish %in% sel_user_groupby))) {
        msg <- "One or more of the required fields for the BCG_Fish are not included. See table to the right."
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      } else if (myIndexName == "MN_IBI_Fish" && any(!(req_IBI_fish %in% sel_user_groupby))) {
        msg <- "One or more of the required fields for the IBI_Fish are not included. See table to the right."
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      } else {
        # No action needed if none of the conditions are met
      }

      # function parameters
      df_user                 <- df_input
      df_official             <- df_taxoff
      df_official_metadata    <- df_taxoff_meta
      taxaid_user             <- sel_user_taxaid
      taxaid_official_match   <- col_taxaid_official_match
      taxaid_official_project <- col_taxaid_official_project
      taxaid_drop             <- sel_taxaid_drop
      col_drop                <- user_col_drop #NULL #sel_col_drop
      sum_n_taxa_boo          <- TRUE
      sum_n_taxa_col          <- sel_user_ntaxa
      sum_n_taxa_group_by     <- c(sel_user_sampid
                                   , sel_user_taxaid
                                   , sel_user_groupby)

      ## RUN taxa_translate ----
      taxatrans_results <- BioMonTools::taxa_translate(df_user
                                                       , df_official
                                                       , df_official_metadata
                                                       , taxaid_user
                                                       , taxaid_official_match
                                                       , taxaid_official_project
                                                       , taxaid_drop
                                                       , col_drop
                                                       , sum_n_taxa_boo
                                                       , sum_n_taxa_col
                                                       , sum_n_taxa_group_by
                                                       , trim_ws = TRUE
                                                       , match_caps = TRUE)

      ## Munge ----

      nrow_results_merge <- nrow(taxatrans_results$merge)

      # Remove non-project taxaID cols
      # Specific to shiny project, not a part of the taxa_translate function
      col_keep <- !names(taxatrans_results$merge) %in% col_drop_project
      if (nrow_results_merge > 0) {
        taxatrans_results$merge <- taxatrans_results$merge[, col_keep]
      }## taxatrans_results$merge > 0

      # Attributes if have 2nd file
      if (!is.na(fn_taxoff_attr) & nrow_results_merge > 0) {

        df_ttrm <- taxatrans_results$merge
        # drop translation file columns
        col_keep_ttrm <- names(df_ttrm)[names(df_ttrm) %in% c(sel_user_sampid
                                                            , sel_user_taxaid
                                                            , sel_user_ntaxa
                                                            , sel_user_indexname
                                                            , sel_user_indexclass
                                                            , sel_user_gprr
                                                            , "Match_Official"
                                                            , sel_user_groupby)]
        df_ttrm <- df_ttrm[, col_keep_ttrm]

        ### COMBINE same TaxaID and sum N_Taxa----
        boo_combine_taxa <- TRUE
        if (boo_combine_taxa) {
          # use 'known' values with tidyverse then change back
          #
          # name field to known value
          col_ttrm_ntaxa <- "ttrm_ntaxa"
          df_ttrm[, col_ttrm_ntaxa] <- df_ttrm[, sel_user_ntaxa]
          col_non_ntaxa <- names(df_ttrm)[!names(df_ttrm) %in% sel_user_ntaxa]
          # drop ntaxa
          df_ttrm <- df_ttrm[, col_non_ntaxa]
          # columns by (for summarize)
          col_by <- col_non_ntaxa[!col_non_ntaxa %in% col_ttrm_ntaxa]
          # sum
          df_ttrm <- dplyr::summarise(df_ttrm
                                     , .by = dplyr::all_of(col_by)
                                     , sum_ntaxa = sum(ttrm_ntaxa, na.rm = TRUE)
                                     )
          # rename 'known' ntaxa back to 'user' value
          names(df_ttrm)[names(df_ttrm) == "sum_ntaxa"] <- sel_user_ntaxa
        }## boo_combine_taxa

        # merge with attributes
        df_merge_attr <- merge(df_ttrm
                               , df_taxoff_attr
                               , by.x = taxaid_user
                               , by.y = col_taxaid_attr
                               , all.x = TRUE
                               , sort = FALSE
                               , suffixes = c("_xDROP", "_yKEEP"))
        # Drop duplicate names from Trans file (x)
        col_keep <- names(df_merge_attr)[!grepl("_xDROP$"
                                                , names(df_merge_attr))]
        df_merge_attr <- df_merge_attr[, col_keep]
        # KEEP and rename duplicate names from Attribute file (y)
        names(df_merge_attr) <- gsub("_yKEEP$", "", names(df_merge_attr))
        # Save back to results list
        taxatrans_results$merge <- df_merge_attr

        # QC check
        # testthat::expect_equal(nrow(df_merge_attr), nrow(df_ttrm))
        # testthat::expect_equal(sum(df_merge_attr[, sel_user_ntaxa], na.rm = TRUE)
        #                        , sum(df_ttrm[, sel_user_ntaxa], na.rm = TRUE))
      }## IF ~ !is.na(fn_taxoff_attr)

      # Reorder by SampID and TaxaID
      taxatrans_results$merge <- taxatrans_results$merge[
           order(taxatrans_results$merge[, sel_user_sampid]
                   , taxatrans_results$merge[, sel_user_taxaid]), ]

      # Mod Results Merge if have data
      if (nrow_results_merge > 0) {
        # Add input filenames
        taxatrans_results$merge[, "file_taxatrans"] <- fn_taxoff
        taxatrans_results$merge[, "file_attributes"] <- fn_taxoff_attr
        # Resort columns
        col_start <- c(sel_user_sampid
                       , sel_user_taxaid
                       , sel_user_ntaxa
                       , "file_taxatrans"
                       , "file_attributes")
        col_other <- names(taxatrans_results$merge)[!names(taxatrans_results$merge)
                                                    %in% col_start]
        taxatrans_results$merge <- taxatrans_results$merge[, c(col_start
                                                             , col_other)]


        # Convert required file names to standard
        ## do at end so don't have to modify any other variables
        boo_req_names <- TRUE
        if (boo_req_names == TRUE) {
          names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                         %in% sel_user_sampid] <- "SampleID"
          names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                         %in% sel_user_taxaid] <- "TaxaID"
          names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                         %in% sel_user_ntaxa] <- "N_Taxa"
        }## IF ~ boo_req_names

        # Hack/Fix
        # Noteworthy NA causing issue later in Shiny app
        # 20231201, only if have Noteworthy
        if ("NOTEWORTHY" %in% toupper(taxatrans_results$merge)) {
          taxatrans_results$merge$Noteworthy <- ifelse(is.na(taxatrans_results$merge$Noteworthy)
                                                       , FALSE
                                                       , TRUE)
        }## IF ~ Noteworthy

        ### MN BCG_ATTR ----
        # MN BCG_ATTR depends on Index_Class (and also GP/RR for bugs)
        # taxa_translate added all the BCG Attr fields
        # create single column
        if (sel_proj == "MN_BCG_Bugs") {

          # BCG_ATTR, Reset
          if ("BCG_ATTR" %in% toupper(names(taxatrans_results$merge))) {
            taxatrans_results$merge <- taxatrans_results$merge[,
                                                               !toupper(names(taxatrans_results$merge))
                                                               %in% "BCG_ATTR"]
            taxatrans_results$merge$BCG_ATTR <- NA_character_
          }## IF ~ BCG_ATTR
          #
          # BCG_ATTR, Define
          ## ensure all MN BCG are character or fails
         taxatrans_results$merge <- taxatrans_results$merge %>%
            dplyr::mutate(BCG_ATTR = dplyr::case_when(.data[[sel_user_indexclass]] == "bug1" & .data[[sel_user_gprr]] == "GP" ~ as.character(BCG_Attr_GP)
                                                      , .data[[sel_user_indexclass]] == "bug2" & .data[[sel_user_gprr]] == "GP" ~ as.character(BCG_Attr_GP)
                                                      , .data[[sel_user_indexclass]] == "bug3" & .data[[sel_user_gprr]] == "GP" ~ as.character(BCG_Attr_GP)
                                                      , .data[[sel_user_indexclass]] == "bug4" & .data[[sel_user_gprr]] == "GP" ~ as.character(BCG_Attr_GP)
                                                      , .data[[sel_user_indexclass]] == "bug5" & .data[[sel_user_gprr]] == "GP" ~ as.character(BCG_Attr_GP)
                                                      , .data[[sel_user_indexclass]] == "bug6" & .data[[sel_user_gprr]] == "GP" ~ as.character(BCG_Attr_GP)
                                                      , .data[[sel_user_indexclass]] == "bug7" & .data[[sel_user_gprr]] == "GP" ~ as.character(BCG_Attr_GP)
                                                      , .data[[sel_user_indexclass]] == "bug1" & .data[[sel_user_gprr]] == "RR" ~ as.character(BCG_Attr_RR)
                                                      , .data[[sel_user_indexclass]] == "bug2" & .data[[sel_user_gprr]] == "RR" ~ as.character(BCG_Attr_RR)
                                                      , .data[[sel_user_indexclass]] == "bug3" & .data[[sel_user_gprr]] == "RR" ~ as.character(BCG_Attr_RR)
                                                      , .data[[sel_user_indexclass]] == "bug4" & .data[[sel_user_gprr]] == "RR" ~ as.character(BCG_Attr_RR)
                                                      , .data[[sel_user_indexclass]] == "bug5" & .data[[sel_user_gprr]] == "RR" ~ as.character(BCG_Attr_RR)
                                                      , .data[[sel_user_indexclass]] == "bug6" & .data[[sel_user_gprr]] == "RR" ~ as.character(BCG_Attr_RR)
                                                      , .data[[sel_user_indexclass]] == "bug7" & .data[[sel_user_gprr]] == "RR" ~ as.character(BCG_Attr_RR)
                                                      , .data[[sel_user_indexclass]] == "bug8" ~ as.character(BCG_Cool_8)
                                                      , .data[[sel_user_indexclass]] == "bug9" ~ as.character(BCG_Cold_9)
                                                      , .default = NA_character_
            )## case_when
            )## mutate

        } else if (sel_proj == "MN_BCG_Fish") {
          # BCG_ATTR, Reset
          if ("BCG_ATTR" %in% toupper(names(taxatrans_results$merge))) {
            taxatrans_results$merge <- taxatrans_results$merge[,
                                                               !toupper(names(taxatrans_results$merge))
                                                               %in% "BCG_ATTR"]
            taxatrans_results$merge$BCG_ATTR <- NA_character_
          }## IF ~ BCG_ATTR
          #
          # BCG_ATTR, Define
          ## ensure all MN BCG are character or fails
          taxatrans_results$merge <- taxatrans_results$merge %>%
            dplyr::mutate(BCG_ATTR = dplyr::case_when(.data[[sel_user_indexclass]] == "fish1" ~ as.character(BCG_Attr_MN2_01)
                                                      , .data[[sel_user_indexclass]] == "fish2" ~ as.character(BCG_Attr_MN2_02)
                                                      , .data[[sel_user_indexclass]] == "fish3" ~ as.character(BCG_Attr_MN2_03)
                                                      , .data[[sel_user_indexclass]] == "fish4" ~ as.character(BCG_Attr_MN2_04)
                                                      , .data[[sel_user_indexclass]] == "fish5" ~ as.character(BCG_Attr_MN2_05)
                                                      , .data[[sel_user_indexclass]] == "fish6" ~ as.character(BCG_Attr_MN2_06)
                                                      , .data[[sel_user_indexclass]] == "fish7" ~ as.character(BCG_Attr_MN2_07)
                                                      , .data[[sel_user_indexclass]] == "fish10" ~ as.character(BCG_Attr_MN2_10)
                                                      , .data[[sel_user_indexclass]] == "fish11" ~ as.character(BCG_Attr_MN2_11)
                                                      , .data[[sel_user_indexclass]] == "fish10a" ~ as.character(BCG_Attr_MN2_10)
                                                      , .data[[sel_user_indexclass]] == "fish11a" ~ as.character(BCG_Attr_MN2_11)
                                                      , .data[[sel_user_indexclass]] == "fish10b" ~ as.character(BCG_Attr_MN2_10)
                                                      , .data[[sel_user_indexclass]] == "fish11b" ~ as.character(BCG_Attr_MN2_11)
                                                      , .default = NA_character_
            )## case_when  (10a, 10b, 11a, 11b)
            )## mutate
        }## IF ~ sel_proj

      }## taxatrans_results$merge > 0


      # need index class brought through

      ## Rename IC and IN ----
      # user names could be non-standard
      names(taxatrans_results$merge)[names(taxatrans_results$merge) %in%
                                       sel_user_indexname] <- "Index_Name"
      names(taxatrans_results$merge)[names(taxatrans_results$merge) %in%
                                       sel_user_indexclass] <- "Index_Class"


      ## Calc, 04, Save Results ----
      prog_detail <- "Save Results"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Save files

      ## File version names
      df_save <- data.frame(Calculation = sel_proj
                            , OperationalTaxonomicUnit = col_taxaid_official_project
                            , TranslationTable = fn_taxoff
                            , AttributeTable = fn_taxoff_attr)
      # fn_part <- paste0("_", abr_filebuilder, "_0taxasource", ".csv")
      fn_part <- paste(fn_save_calc, "TaxaTranslator_source.csv", sep = "_")
      write.csv(df_save
                # , file.path(path_results_sub, paste0(fn_input_base, fn_part))
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa User
      # saved when imported

      # 2023-11-03, save original filenames
      # add taxatrans metadata

      ## Taxa Official
      # df_save <- df_official
      # fn_part <- paste0(fn_abr_save, "1official", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff
                , file.path(path_results_ref, fn_taxoff))

      ## Taxa Official, meta data
      # df_save <- taxatrans_results$official_metadata # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "1metadata", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_meta
                , file.path(path_results_ref, fn_taxoff_meta))

      ## Taxa Official, Attributes
      # df_save <- df_taxoff_attr
      # fn_part <- paste0(path_results_ref, "1attributes", ".csv")
      # write.csv(df_save
      #           , file.path(path_results, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_attr
                , file.path(path_results_ref, fn_taxoff_attr))

      ## Taxa Official, Attributes, meta data
      # df_save <- taxatrans_results$official_metadata # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "1metadata", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_attr_meta
                , file.path(path_results_ref, fn_taxoff_attr_meta))

      ## translate - crosswalk
      df_save <- taxatrans_results$taxatrans_unique # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "2taxamatch", ".csv")
      fn_part <- paste(fn_save_calc, "TaxaTranslator_modify.csv", sep = "_")
      write.csv(df_save
                # , file.path(path_results_sub, paste0(fn_input_base, fn_part))
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Non Match
      df_save <- data.frame(taxatrans_results$nonmatch)
      # fn_part <- paste0(fn_abr_save, "3nonmatch", ".csv")
      fn_part <- paste(fn_save_calc, "TaxaTranslator_nonmatch.csv", sep = "_")
      write.csv(df_save
                # , file.path(path_results_sub, paste0(fn_input_base, fn_part))
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa Trans
      df_save <- taxatrans_results$merge
      # fn_part <- paste0(fn_abr_save, "4taxaattr", ".csv")
      fn_part <- paste(fn_save_calc, "TaxaTranslator_TAXAATTR.csv", sep = "_")
      write.csv(df_save
                # , file.path(path_results_sub, paste0(fn_input_base, fn_part))
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Calc, 05, Create Zip ----
      prog_detail <- "Create Zip File For Download"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Create zip file for download
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)


      ## Calc, 06, Clean Up ----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # button, enable, download
      shinyjs::enable("b_download_taxatrans")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Total Records (Input) = "
                    , nrow(df_input)
                    , "\n\n"
                    , "Number of mismatch taxa = "
                    , nrow(taxatrans_results$nonmatch)
                    )
      if (sel_proj == "MN_BCG_Bugs") {
        # extra message contents for BCG_Bugs
        bad_gprr <- sum(!grepl("GP|RR", taxatrans_results$merge$GP.RR))
        msg <- paste0(msg
                      , "\n\n"
                      , "Number of records without GP/RR;\n"
                      , "i.e., no BCG Attribute will be assigned = "
                      , bad_gprr
                      )
      }## IF ~ analysis type
      msg <- paste0(msg
                    , "\n\n"
                    , "Elapse Time ("
                    , units(duration)
                    , ") = "
                    , round(duration, 2)
                    )
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Calculating BCG"
    )## withProgress

  }##expr ~ ObserveEvent

  )##observeEvent ~ b_taxatrans_calc

  ## b_download_TaxaTrans ----
  output$b_download_taxatrans <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ TaxaTrans



  # FB, MERGE FILES, CSV ----

  ## Merge, Import, FileWatch ----
  file_watch_mf1 <- reactive({
    # trigger for df_import()
    input$fn_input_mf1
  })## file_watch

  file_watch_mf2 <- reactive({
    # trigger for df_import()
    input$fn_input_mf2
  })## file_watch

  ### Merge, Import, df_import_mf1 ----
  df_import_mf1 <- eventReactive(file_watch_mf1(), {
    # use a multi-item reactive so keep on a single line (if needed later)

    # input$df_import_mf1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$fn_input_mf1

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    sep_user <- input$sep

    # Define file
    fn_inFile <- inFile$datapath

    #message(getwd())
    # message(paste0("Import, separator: '", input$sep,"'"))
    message(paste0("Import, file name: ", inFile$name))

    # Remove existing files in "results"
    clean_results()

    #### Mod, BCG_ATTR----
    # Read user imported file
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # many permutations of BCG_Attr so check for it first then import
    df_header <- read.delim(fn_inFile
                            , header = TRUE
                            , sep = sep_user
                            , stringsAsFactors = FALSE
                            , na.strings = c("", "NA")
                            , nrows = 0)
    col_num_bcgattr <- grep("BCG_ATTR", toupper(names(df_header)))
    classes_df <- sapply(df_header, class)
    col_name_bcgattr <- names(df_header)[col_num_bcgattr]

    if (identical(col_num_bcgattr, integer(0))) {
      # BCG_Attr present = FALSE
      # define classes = FALSE
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
    } else if (as.vector(classes_df[col_num_bcgattr]) != "complex") {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = FALSE
      # define classes on import = FALSE (change to text after import)
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      df_input[, col_num_bcgattr] <- as.character(df_input[, col_num_bcgattr])
    } else {
      # BCG_Attr present = TRUE
      # define classes = TRUE
      classes_df <- sapply(df_header, class)
      classes_df[col_num_bcgattr] <- "character"
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA")
                             #, colClasses = classes_df)
                             #, colClasses = c(col_name_bcgattr = "character"))
                             , colClasses = classes_df[col_name_bcgattr])

    }## IF ~ col_num_bcgattr == integer(0)


    # OLD
    # Will get a 'warning' for unknown columns but harmless
    # df_input <- read.delim(fn_inFile
    #                        , header = TRUE
    #                        , sep = ","
    #                        , stringsAsFactors = FALSE
    #                        , colClasses = c("BCG_Attr" = "character"
    #                                         , "BCG_ATTR" = "character"
    #                                         , "bcg_attr" = "character"
    #                                         , "BCG_attr" = "character"))

    # result folder and files
    path_results_sub <- file.path(path_results, dn_files_input)
    # Add "Results" folder if missing
    boo_Results <- dir.exists(file.path(path_results_sub))
    if (boo_Results == FALSE) {
      dir.create(file.path(path_results_sub))
    }

    # Copy to "Results" sub-folder - Import "as is"
    file.copy(inFile$datapath
              , file.path(path_results_sub, inFile$name))

    # button, enable, calc
    shinyjs::enable("b_calc_mergefiles")

    # activate tab Panel with table of imported data
    updateTabsetPanel(session = getDefaultReactiveDomain()
                      , "MF_mp_tsp"
                      , selected = "tab_MF_1")

    # Return Value
    return(df_input)

  })##output$df_import_mf1 ~ END


  ### Merge, Import, df_import_mf2----
  df_import_mf2 <- eventReactive(file_watch_mf2(), {
    # use a multi-item reactive so keep on a single line (if needed later)

    # input$df_import_mf1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$fn_input_mf2

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    # Define file
    fn_inFile <- inFile$datapath

    sep_user <- input$sep

    #message(getwd())
    #message(paste0("Import, separator: '", input$sep,"'"))
    message(paste0("Import, file name: ", inFile$name))

    # Move Results folder clean up to calc button
    # Assume import 2nd file after 1st

    #### Mod, BCG_ATTR----
    # Read user imported file
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # many permutations of BCG_Attr so check for it first then import
    df_header <- read.delim(fn_inFile
                            , header = TRUE
                            , sep = sep_user
                            , stringsAsFactors = FALSE
                            , na.strings = c("", "NA")
                            , nrows = 0)
    col_num_bcgattr <- grep("BCG_ATTR", toupper(names(df_header)))
    classes_df <- sapply(df_header, class)
    col_name_bcgattr <- names(df_header)[col_num_bcgattr]

    if (identical(col_num_bcgattr, integer(0))) {
      # BCG_Attr present = FALSE
      # define classes = FALSE
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
    } else if (as.vector(classes_df[col_num_bcgattr]) != "complex") {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = FALSE
      # define classes on import = FALSE (change to text after import)
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      df_input[, col_num_bcgattr] <- as.character(df_input[, col_num_bcgattr])
    } else {
      # BCG_Attr present = TRUE
      # define classes = TRUE
      classes_df <- sapply(df_header, class)
      classes_df[col_num_bcgattr] <- "character"
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA")
                             # , colClasses = classes_df)
                             #, colClasses = c(col_name_bcgattr = "character"))
                             , colClasses = classes_df[col_name_bcgattr])

    }## IF ~ col_num_bcgattr == integer(0)


    # OLD
    # Will get a 'warning' for unknown columns but harmless
    # df_input <- read.delim(fn_inFile
    #                        , header = TRUE
    #                        , sep = ","
    #                        , stringsAsFactors = FALSE
    #                        , colClasses = c("BCG_Attr" = "character"
    #                                         , "BCG_ATTR" = "character"
    #                                         , "bcg_attr" = "character"
    #                                         , "BCG_attr" = "character"))

    # result folder and files
    path_results_sub <- file.path(path_results, dn_files_input)
    # Add "Results" folder if missing
    boo_Results <- dir.exists(file.path(path_results_sub))
    if (boo_Results == FALSE) {
      dir.create(file.path(path_results_sub))
    }

    # Copy to "Results" sub-folder - Import "as is"
    file.copy(inFile$datapath
              , file.path(path_results_sub, inFile$name))

    # button, enable, calc
    shinyjs::enable("b_calc_mergefiles")

    # activate tab Panel with table of imported data
    updateTabsetPanel(session = getDefaultReactiveDomain()
                      , "MF_mp_tsp"
                      , selected = "tab_MF_2")

    # Return Value
    return(df_input)

  })##output$df_import_mf2 ~ END

  ### Merge, Import, df_import_mf1_DT ----
  output$df_import_mf1_DT <- DT::renderDT({
    df_data <- df_import_mf1()
  }##expression~END
  , filter = "top"
  , caption = "Table. MergeFile 1 (Samples)."
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                   , autoWidth = TRUE)
  )##df_import_mf1_DT ~ END

  ### Merge, Import, df_import_mf2_DT ----
  output$df_import_mf2_DT <- DT::renderDT({
    df_data <- df_import_mf2()
  }##expression~END
  , filter = "top"
  , caption = "Table. MergeFile 2 (Sites)."
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                   , autoWidth = TRUE)
  )##df_import_mf1_DT ~ END

  # ### Merge, df_mf_merge_DT ----
  # # repeat merge statement in calc section for merge files
  # output$df_mf_merge_DT <- DT::renderDT({
  #   ## column names
  #   col_siteid_mf1 <- input$mergefiles_f1_col_merge
  #   col_siteid_mf2 <- input$mergefiles_f2_col_merge
  #   # QC
  #   validate(need(col_siteid_mf1, "Missing merge column, file 1.")
  #            , need(col_siteid_mf2, "Missing merge column, file 2."))
  #   df_merge <- merge(df_import_mf1()
  #                     , df_import_mf2()
  #                     , by.x = col_siteid_mf1
  #                     , by.y = col_siteid_mf2
  #                     , suffixes = c(".x", ".y")
  #                     , all.x = TRUE
  #                     , sort = FALSE
  #   )
  #
  #  # move MF2 columns to the start (at end after merge)
  #  ## use index numbers
  #  ncol_1x <- ncol(df_import_mf1())
  #  ncol_merge <- ncol(df_merge)
  #  df_merge <- df_merge[, c(1, seq(ncol_1x + 1, ncol_merge), 2:ncol_1x)]
  #
  #   return(df_merge)
  # }##expression~END
  # , filter = "top"
  # , caption = "Table. MergeFile 2 (Sites)."
  # , options = list(scrollX = TRUE
  #                  , pageLength = 5
  #                  , lengthMenu = c(5, 10, 25, 50, 100, 1000)
  #                  , autoWidth = TRUE)
  # )##df_import_mf1_DT ~ END

  ## Merge, UI----

  output$UI_mergefiles_f1_col_merge <- renderUI({
    str_col <- "Merge Identifier, Primary File, Column Name"
    selectInput("mergefiles_f1_col_merge"
                , label = str_col
                # , choices = c("SiteID", "feature", "in progress")
                , choices = c("", names(df_import_mf1()))
                , selected = "SiteID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_mergefiles_f2_col_merge <- renderUI({
    str_col <- "Merge Identifier, Secondary File, Column Name"
    selectInput("mergefiles_f2_col_merge"
                , label = str_col
                #, choices = c("SiteID", "feature", "in progress")
                , choices = c("", names(df_import_mf2()))
                , selected = "SiteID"
                , multiple = FALSE)
  })## UI_colnames

  ## b_Calc_MergeFiles ----
  observeEvent(input$b_calc_mergefiles_csv, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 00, Set Up Shiny Code ----

      prog_detail <- "Calculation, Merge Files, CSV..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 6
      prog_sleep <- 0.25

      ## Calc, 01, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input_mf1)
      copy_import_file(import_file = input$fn_input_mf2)

      # result folder and files
      fn_abr <- abr_mergefiles
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # button, disable, download
      shinyjs::disable("b_download_mergefiles_csv")

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # inputs
      ## file names
      fn_mf1 <- input$fn_input_mf1$name
      fn_mf2 <- input$fn_input_mf2$name
      ## column names
      col_siteid_mf1 <- input$mergefiles_f1_col_merge
      col_siteid_mf2 <- input$mergefiles_f2_col_merge
      ## file name base (file 1)
      fn_input_base <- tools::file_path_sans_ext(fn_mf1)

      # Stop if don't have both MF1 and MF2
      if (is.null(fn_mf1)) {
        msg <- "Merge File 1 filename is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      if (is.null(fn_mf2)) {
        msg <- "Merge File 2 filename is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      # Stop if colname for merge is NA
      if (col_siteid_mf1 == "") {
        msg <- "Merge File 1 merge column is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      if (col_siteid_mf2 == "") {
        msg <- "Merge File 2 merge column is missing!"
        shinyalert::shinyalert(title = "Merge File Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)



      # Remove non-MergeFiles files
      # Remove all files in "Results" folder
      # 2 file imports so moved Results folder clean up here from import section
      # fn_results <- list.files(path_results
      #                          , full.names = TRUE
      #                          , include.dirs = FALSE
      #                          , recursive = TRUE)
      # message(paste0("Files in 'results' folder (before removal) = "
      #                , length(fn_results)))
      # comment out 2023-11-03
      #
      # # Exclude MF1 and MF2
      # fn_mf_keep <- file.path(path_results
      #                         , c(fn_mf1, fn_mf2))
      # fn_results <- fn_results[!fn_results %in% fn_mf_keep]
      # # Remove non MF files
      # file.remove(fn_results) # ok if no files
      # # QC, repeat
      # fn_results2 <- list.files(path_results
      #                           , full.names = TRUE
      #                           , include.dirs = FALSE
      #                           , recursive = TRUE)
      # message(paste0("Files in 'results' folder (after removal [should be 2]) = "
      #                , length(fn_results2)))


      ## Calc, 03, Run Function----
      suff_1x <- ".x"
      suff_2y <- ".y"
      df_merge <- merge(df_import_mf1()
                        , df_import_mf2()
                        , by.x = col_siteid_mf1
                        , by.y = col_siteid_mf2
                        , suffixes = c(suff_1x, suff_2y)
                        , all.x = TRUE
                        , sort = FALSE
      )
      # ***REPEAT*** same merge statement in DT statement for display on tab

      # move MF2 columns to the start (at end after merge)
      ## use index numbers
      ncol_1x <- ncol(df_import_mf1())
      ncol_merge <- ncol(df_merge)
      df_merge <- df_merge[, c(1, seq(ncol_1x + 1, ncol_merge), 2:ncol_1x)]

      ## Calc, 04, Save Results ----

      fn_merge <- paste0(fn_input_base, fn_abr_save, "RESULTS.csv")
      pn_merge <- file.path(path_results_sub, fn_merge)
      write.csv(df_merge, pn_merge, row.names = FALSE)


      ## Calc, 05, Clean Up----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # # activate tab Panel with table of imported data
      # updateTabsetPanel(session = getDefaultReactiveDomain()
      #                   , "MF_mp_tsp"
      #                   , selected = "tab_MF_merge")


      ## Calc, 06, Zip Results ----
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      # button, enable, download
      shinyjs::enable("b_download_mergefiles")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Elapse Time (", units(duration), ") = ", round(duration, 2))
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Merging Files"
    )## withProgress ~ END
  }##expr ~ ObserveEvent ~ END
  )##observeEvent ~ b_calc_met_therm ~ END


  ## b_download_mergefiles_csv ----
  output$b_download_mergefiles_csv <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input_mf2
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_mergefiles
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ MergeFiles

  # FB, MERGE FILES, ZIP ----
  # merge two sets of zip files

  ## Merge, ZIP, Import, FileWatch ----
  file_watch_mf1_zip <- reactive({
    # trigger for df_import()
    input$fn_input_mf1_zip
  })## file_watch

  file_watch_mf2_zip <- reactive({
    # trigger for df_import()
    input$fn_input_mf2_zip
  })## file_watch

  ## b_Calc_MergeFiles_ZIP ----
  observeEvent(input$b_calc_mergefiles_zip, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 00, Set Up Shiny Code ----

      prog_detail <- "Calculation, Merge Files, ZIP..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 5
      prog_sleep <- 0.25

      ## Calc, 01, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input_mf1_zip)
      copy_import_file(import_file = input$fn_input_mf2_zip)

      # result folder and files
      fn_abr <- abr_mf_zip
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

     # button, disable, download
      shinyjs::disable("b_download_mergefiles_zip")

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # inputs

      fn_mf1 <- input$fn_input_mf1_zip$name
      fn_mf2 <- input$fn_input_mf2_zip$name

      dn_zip1 <- "zip1"
      dn_zip2 <- "zip2"

      path_mf_zip <- file.path(path_results, dn_files_input)

      fn_mf1_zip <- file.path(path_mf_zip, fn_mf1)
      fn_mf2_zip <- file.path(path_mf_zip, fn_mf2)

      path_mf1_zip <- file.path(path_mf_zip, dn_zip1)
      path_mf2_zip <- file.path(path_mf_zip, dn_zip2)

      # Stop if don't have both MF1 and MF2
      if (is.null(fn_mf1)) {
        msg <- "Merge Files ZIP 1 filename is missing!"
        shinyalert::shinyalert(title = "Merge Files ZIP Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      if (is.null(fn_mf2)) {
        msg <- "Merge Files ZIP 2 filename is missing!"
        shinyalert::shinyalert(title = "Merge Files ZIP Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      # Prep Files

      print("unzip")
      ## Unzip files
      utils::unzip(fn_mf1_zip, exdir = path_mf1_zip)
      utils::unzip(fn_mf2_zip, exdir = path_mf2_zip)

      # delete zips
      file.remove(fn_mf1_zip)
      file.remove(fn_mf2_zip)

      # listfiles
      files_zip1 <- list.files(path_mf1_zip
                               , pattern = "\\.csv$"
                               , full.names = FALSE
                               , recursive = TRUE
                               , include.dirs = TRUE)
      files_zip2 <- list.files(path_mf2_zip
                               , pattern = "\\.csv$"
                               , full.names = FALSE
                               , recursive = TRUE
                               , include.dirs = TRUE)

      # base names
      base_name1 <- basename(files_zip1)
      base_name2 <- basename(files_zip2)

      # match files
      files_common <- NA
      files_1not2 <- base_name1[!base_name1 %in% base_name2]
      files_2not1 <- base_name2[!base_name2 %in% base_name1]

      files_zip1_sanszipdir <- gsub(paste0("^",
                                           file_path_sans_ext(fn_mf1),
                                           "/"),
                                    "",
                                    files_zip1)
      files_zip2_sanszipdir <- gsub(paste0("^",
                                           file_path_sans_ext(fn_mf2),
                                           "/"),
                                    "",
                                    files_zip2)


      # Stop if no common files
      if (length(files_common) == 0) {
        msg <- "Merge Files, No Common Files!"
        shinyalert::shinyalert(title = "Merge Files ZIP Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ is.null (mf1)

      # Create Dataframe of files
      df_zip <- data.frame(path_1 = files_zip1,
                           base_1 = base_name1,
                           base_1_sanszipdir = files_zip1_sanszipdir)
      df_zip$match <- files_zip1_sanszipdir %in% files_zip2_sanszipdir
      match_files12_sanszipdir <- match(files_zip1_sanszipdir
                                      , files_zip2_sanszipdir)
      df_zip$path_2 <- files_zip2[match_files12_sanszipdir]
      df_zip$base_2 <- base_name2[match_files12_sanszipdir]
      df_zip$path_3 <- gsub(paste0("^", path_results),
                            path_results_sub,
                            df_zip$base_1_sanszipdir)
      df_zip$dir_3 <- dirname(df_zip$path_3)

      ### Calc, 03, Run Function----
      prog_detail <- "Merge Files"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # Merge Files
      suff_1x <- ".A"
      suff_2y <- ".B"

      for (i in seq_len(nrow(df_zip))) {
        # ensure dir exists
        path_dir3_i <- file.path(df_zip[i, "dir_3"])
        boo_dir3_i <- dir.exists(path_dir3_i)
        if (boo_dir3_i == FALSE) {
          dir.create(path_dir3_i)
        }## IF ~ boo_dir3

        if(df_zip[i, "match"] == FALSE) {
          # Non-Matches, copy without any merge
          fc_from <- file.path(path_mf1_zip, df_zip[i, "path_1"])
          fc_to <- df_zip[i, "path_3"]
          file.copy(fc_from, fc_to)
        } else {
          # open A
          df1_i <- read.csv(file.path(path_mf1_zip, df_zip[i, "path_1"]))
          # open B
          df2_i <- read.csv(file.path(path_mf2_zip, df_zip[i, "path_2"]))
          # Merge as C
          df3_i <- merge(df1_i,
                         df2_i,
                         suffixes = c(suff_1x, suff_2y),
                         all = TRUE,
                         sort = FALSE)
          # Save C
          write.csv(df3_i, df_zip[i, "path_3"])
        }## IF ~ match
      }## FOR ~ i

      # Copy non-matching files
      ## Files 1 not 2
      ### in loop above

      ## Files 2 not 1
      ## similar to match == false in loop above
      fc_filter <- match(files_2not1, base_name2)
      fc_from <- file.path(path_mf2_zip, files_zip2) # path_1 equivalent
      fc_to <- gsub(paste0("^", path_results),
                    path_results_sub,
                    files_zip2_sanszipdir)
      file.copy(fc_from[fc_filter], fc_to[fc_filter])

      ## Calc, 04, Clean Up----
      prog_detail <- "Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # Remove user input before zip
      unlink(file.path(path_results, dn_files_input), recursive = TRUE) # includes directories


      ## Calc, 05, Zip Results ----
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      # button, enable, download
      shinyjs::enable("b_download_mergefiles_zip")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Elapse Time (",
                    units(duration),
                    ") = ",
                    round(duration, 2),
                    "\n\n",
                    "Files not matching (A to B):\n",
                    paste(files_1not2, collapse = "\n"),
                    "\n\n",
                    "Files not matching (B to A):\n",
                    paste(files_2not1, collapse = "\n")
                    )
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Merging Files, ZIP"
    )## withProgress ~ END
  }##expr ~ ObserveEvent ~ END
  )##observeEvent ~ b_calc_met_therm ~ END


  ## b_download_mergefiles_zip ----
  output$b_download_mergefiles_zip <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input_mf2_zip
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_mergefiles
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ MergeFiles

  #~~~~CALC~~~~----

  # Calc, BCG ----
  ## b_Calc_BCG ----
  observeEvent(input$b_calc_bcg, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 0, Set Up Shiny Code ----

      prog_detail <- "Calculation, BCG..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 13
      prog_sleep <- 0.25

      ## Calc, 1, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      # 2023-12-14, add community
      fn_comm <- .simpleCap(input$si_community)
      fn_abr <- abr_bcg
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_comm, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # reference folder
      path_results_ref <- file.path(path_results, dn_files_ref)
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }

      # button, disable, download
      shinyjs::disable("b_download_bcg")

      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      ## Calc, 1.5, QC and Test Inputs ----
      prog_detail <- "Calculate, QC and Test Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # QC, names to upper case
      names(df_input) <- toupper(names(df_input))

      # QC, Index_Name
      # Only MN_BCG so can add if missing
      if (!"INDEX_NAME" %in% toupper(names(df_input))) {
        df_input[, "INDEX_NAME"] <- "MN_BCG"
      }## IF ~ INDEX_NAME

      indexname_dat <- unique(df_input$INDEX_NAME)

      if (length(indexname_dat) > 1) {
        msg <- paste0("More than one INDEX_NAME in data!"
                      , "\n\n"
                      , "Only a single index can be used at one time.")
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## Mult indexname-data

      if (indexname_dat != "MN_BCG") {
        msg <- paste0("Incorrect INDEX_NAME in data!"
                      , "\n\n"
                      , "Only a value of 'MN_BCG' is acceptable.")
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## wrong indexname-data

      # QC, check for required fields
      col_req <- c("INDEX_NAME"
                   , "INDEX_CLASS"
                   , "SAMPLEID"
                   , "TAXAID"
                   , "N_TAXA")
      col_req_boo <- col_req %in% names(df_input)
      col_req_missing <- col_req[!col_req_boo]

      if (sum(col_req_boo) != length(col_req)) {
        msg <- paste0("Required fields (columns) missing!"
                      , "\n\n"
                      , paste(col_req_missing, collapse = ", "))
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sum(col_req_boo) != length(col_req)

      # QC, Index Class to Character
      df_input[, "INDEX_CLASS"] <- as.character(df_input[, "INDEX_CLASS"])

      ### QC, Index_CLASS, Community
      # confirm data matches selection
      comm_sel <- tolower(fn_comm)
      if(comm_sel == "bugs") {comm_sel <- "bug"} # match BCG Rules
      comm_dat <- gsub("[0-9]+"
                       , ""
                       , tolower(unique(substr(df_input[, "INDEX_CLASS"]
                                               , 1
                                               , 4))))
      # fish extra letters in number combo for 10 and 11


      if (!comm_sel %in% comm_dat) {
        msg <- paste0("Community selection does not match data!"
                      , "\n\n"
                      , "Selection = ", comm_sel
                      , "\n\n"
                      , "Data = ", comm_dat)
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ comm_sel !%in% comm_dat

      # DrainSqMi for fish
      if (comm_sel == "fish" & !"DRAINSQMI" %in% names(df_input)) {
        msg <- paste0("Required fields (columns) missing!"
                      , "\n\n"
                      , "DRAINSQMI is needed for Fish BCG.")
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ DRAINSQMI

      ## Calc, 2, Exclude Taxa ----
      prog_detail <- "Calculate, Exclude Taxa"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      message(paste0("User response to generate ExclTaxa = ", input$ExclTaxa))

      if (input$ExclTaxa) {
        ## Get TaxaLevel names present in user file
        phylo_all <- c("Kingdom"
                       , "Phylum"
                       , "SubPhylum"
                       , "Class"
                       , "SubClass"
                       , "Order"
                       , "SubOrder"
                       , "InfraOrder"
                       , "SuperFamily"
                       , "Family"
                       , "SubFamily"
                       , "Tribe"
                       , "Genus"
                       , "SubGenus"
                       , "Species"
                       , "Variety")
        phylo_all <- toupper(phylo_all) # so matches rest of file

        # case and matching of taxa levels handled inside of markExluded

        # Overwrite existing column if present
        # ok since user checked the box to calculate
        if ("EXCLUDE" %in% toupper(names(df_input))) {
          # save original user input
          df_input[, "EXCLUDE_USER"] <- df_input[, "EXCLUDE"]
          # drop column
          df_input <- df_input[, !names(df_input) %in% "EXCLUDE"]
        }## IF ~ Exclude

        # overwrite current data frame
        df_input <- BioMonTools::markExcluded(df_samptax = df_input
                                              , SampID = "SAMPLEID"
                                              , TaxaID = "TAXAID"
                                              , TaxaCount = "N_TAXA"
                                              , Exclude = "EXCLUDE"
                                              , TaxaLevels = phylo_all
                                              , Exceptions = NA)

        # Save Results
        # fn_excl <- paste0(fn_input_base, fn_abr_save, "1markexcl.csv")
        fn_excl <- "BCG_1markexcl.csv"
        dn_excl <- path_results_sub
        pn_excl <- file.path(dn_excl, fn_excl)
        write.csv(df_input, pn_excl, row.names = FALSE)

      }## IF ~ input$ExclTaxa


      ## Calc, 3, BCG Flag Cols ----
      # get columns from Flags (non-metrics) to carry through
      prog_detail <- "Calculate, Keep BCG Model Columns"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Rules - should all be metrics but leaving here just in case
      # Flags - not always metrics,
      # Index Name for import data
      import_IndexName <- unique(df_input$INDEX_NAME)
      # QC Flags for chosen BCG model (non-metrics)
      cols_flags <- unique(df_checks[df_checks$Index_Name == import_IndexName
                                     , "Metric_Name"])
      # can also add other columns to keep if feel so inclined
      cols_flags_keep <- cols_flags[cols_flags %in% names(df_input)]

      ## Mod, Add, Index_Class 10a/b----
      # after flags even though flags not selecting based on Index_Class
      # Assume names are upper case and present in BCG
      #
      # missing area to zero
      if (comm_sel == "fish") {
        df_input <- df_input %>%
          dplyr::mutate(DRAINSQMI = dplyr::case_when(is.na(DRAINSQMI) ~ 0
                                                     , .default = DRAINSQMI)) %>%
          dplyr::mutate(INDEX_CLASS = dplyr::case_when(
            INDEX_CLASS == "fish10a" & DRAINSQMI < 10 ~ "fish10a_small"
            , INDEX_CLASS == "fish10a" & DRAINSQMI >= 10 ~ "fish10a_large"
            , INDEX_CLASS == "fish10b" & DRAINSQMI < 10 ~ "fish10b_small"
            , INDEX_CLASS == "fish10b" & DRAINSQMI >= 10 ~ "fish10b_large"
            , .default = INDEX_CLASS)
          )
      }## IF ~ fish

      ## Calc, 3b, Rules ----
      prog_detail <- "Calculate, BCG Rules"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # filter for data Index_Name in data (drop 2 extra columns)
      df_rules <- df_bcg_models[df_bcg_models$Index_Name == import_IndexName
                                , !names(df_bcg_models) %in% c("SITE_TYPE", "INDEX_REGION")]
      # Save
      # fn_rules <- paste0(fn_input_base, fn_abr_save, "3metrules.csv")
      fn_rules <- "BCG_3metrules.csv"
      dn_rules <- path_results_sub
      pn_rules <- file.path(dn_rules, fn_rules)
      write.csv(df_rules, pn_rules, row.names = FALSE)

      ## Calc, 4, MetVal----
      prog_detail <- "Calculate, Metric, Values"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      # QC
      # df_input <- read.csv(file.path("inst", "extdata", "Data_BCG_PacNW.csv"))
      # df_metval <- BioMonTools::metric.values(df_input, "bugs", boo.Shiny = TRUE)

      if (length(cols_flags_keep) > 0) {
        # keep extra cols from Flags (non-metric)
        df_metval <- BioMonTools::metric.values(df_input
                                                , input$si_community
                                                , fun.cols2keep = cols_flags_keep
                                                , boo.Shiny = TRUE
                                                , verbose = TRUE
                                                , taxaid_dni = "DNI")
      } else {
        df_metval <- BioMonTools::metric.values(df_input
                                                , input$si_community
                                                , boo.Shiny = TRUE
                                                , verbose = TRUE
                                                , taxaid_dni = "DNI")
      }## IF ~ length(col_rules_keep)


      #df_metval$INDEX_CLASS <- df_metval$INDEX_CLASS

      ### Save Results ----

      # fn_metval <- paste0(fn_input_base, fn_abr_save, "2metval_all.csv")
      fn_metval <- "BCG_2metval_all.csv"
      dn_metval <- path_results_sub
      pn_metval <- file.path(dn_metval, fn_metval)
      write.csv(df_metval, pn_metval, row.names = FALSE)

      ### Save Results (BCG) ----
      # Munge
      ## Model and QC Flag metrics only
      # cols_flags defined above
      cols_model_metrics <- unique(df_bcg_models[
        df_bcg_models$Index_Name == import_IndexName, "Metric_Name", TRUE])
      cols_req <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                    , "ni_total", "nt_total")
      cols_metrics_flags_keep <- unique(c(cols_req
                                          , cols_flags
                                          , cols_model_metrics))
      df_metval_slim <- df_metval[, names(df_metval) %in% cols_metrics_flags_keep]
      # Save
      # fn_metval_slim <- paste0(fn_input_base, fn_abr_save, "2metval_BCG.csv")
      fn_metval_slim <- "BCG_2metval_BCG.csv"
      dn_metval_slim <- path_results_sub
      pn_metval_slim <- file.path(dn_metval_slim, fn_metval_slim)
      write.csv(df_metval_slim, pn_metval_slim, row.names = FALSE)

      ## Calc, 5, MetMemb----
      prog_detail <- "Calculate, Metric, Membership"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_metmemb <- BCGcalc::BCG.Metric.Membership(df_metval, df_bcg_models)
      # Save Results
      # fn_metmemb <- paste0(fn_input_base, fn_abr_save, "3metmemb.csv")
      fn_metmemb <- "BCG_3metmemb.csv"
      dn_metmemb <- path_results_sub
      pn_metmemb <- file.path(dn_metmemb, fn_metmemb)
      write.csv(df_metmemb, pn_metmemb, row.names = FALSE)


      ## Calc, 6, LevMemb----
      prog_detail <- "Calculate, Level, Membership"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_levmemb <- BCGcalc::BCG.Level.Membership(df_metmemb, df_bcg_models)
      # Save Results
      # fn_levmemb <- paste0(fn_input_base, fn_abr_save, "4levmemb.csv")
      fn_levmemb <- "BCG_4levmemb.csv"
      dn_levmemb <- path_results_sub
      pn_levmemb <- file.path(dn_levmemb, fn_levmemb)
      write.csv(df_levmemb, pn_levmemb, row.names = FALSE)


      ## Calc, 7, LevAssign----
      prog_detail <- "Calculate, Level, Assignment"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_levassign <- BCGcalc::BCG.Level.Assignment(df_levmemb)
      # Save Results
      # fn_levassign <- paste0(fn_input_base, fn_abr_save, "5levassign.csv")
      fn_levassign <- "BCG_5levassign.csv"
      dn_levassign <- path_results_sub
      pn_levassign <- file.path(dn_levassign, fn_levassign)
      write.csv(df_levassign, pn_levassign, row.names = FALSE)


      ## Calc, 8, QC Flags----
      prog_detail <- "Calculate, QC Flags"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # 2023-12-06
      # Split if no flags so doesn't crash

      # Check if Flags exist for data
      col_index_metval <- c("INDEX_NAME", "INDEX_CLASS")
      col_index_checks <- c("Index_Name", "INDEX_CLASS")
      index_metval <- unique(df_metval[, col_index_metval])
      index_checks <- unique(df_checks[, col_index_checks])
      index_merge <- merge(index_metval, index_checks
                           , by.x = col_index_metval
                           , by.y = col_index_checks)

      if (nrow(index_merge) == 0) {

        # create dummy files
        str_nodata <- "No flags for the Index Name/Class combinations present in data"
        # Flags
        df_flags <- data.frame(x = str_nodata
                                   , CHECKNAME = "No Flags"
                                   , FLAG = NA)
        df_lev_flags <- df_levassign
        # Flags Summary
        df_lev_flags_summ <- data.frame(x = str_nodata)
        # Results
        df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
        # Flag Metrics
        df_metflags <- data.frame(x = str_nodata)

      } else {

        # Calc
        # df_checks loaded in global.R
        df_flags <- BioMonTools::qc.checks(df_metval, df_checks)
        # Change terminology; PASS/FAIL to NA/flag
        df_flags[, "FLAG"][df_flags[, "FLAG"] == "FAIL"] <- "flag"
        df_flags[, "FLAG"][df_flags[, "FLAG"] == "PASS"] <- NA
        # long to wide format
        df_flags_wide <- reshape2::dcast(df_flags
                                         , SAMPLEID ~ CHECKNAME
                                         , value.var = "FLAG")
        # Calc number of "flag"s by row.
        df_flags_wide$NumFlags <- rowSums(df_flags_wide == "flag", na.rm = TRUE)
        # Rearrange columns
        NumCols <- ncol(df_flags_wide)
        df_flags_wide <- df_flags_wide[, c(1, NumCols, 2:(NumCols - 1))]
        # Merge Levels and Flags
        df_lev_flags <- merge(df_levassign
                              , df_flags_wide
                              , by.x = "SampleID"
                              , by.y = "SAMPLEID"
                              , all.x = TRUE)
        # Flags Summary
        df_lev_flags_summ <- as.data.frame.matrix(table(df_flags[, "CHECKNAME"]
                                                        , df_flags[, "FLAG"]
                                                        , useNA = "ifany"))
        # Results
        df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
        ## remove L1:6

        # Flag Metrics
        col2keep_metflags <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                               , "METRIC_NAME", "CHECKNAME", "METRIC_VALUE"
                               , "SYMBOL", "VALUE", "FLAG")
        df_metflags <- df_flags[, col2keep_metflags]

      }## IF ~ check for matching index name and class

      ## Mod, Remove, Index_Class 10a/b----
      df_results[, "INDEX_NAME"] <- gsub("_large$"
                                         , ""
                                         , df_results[, "INDEX_NAME"])
      df_results[, "INDEX_NAME"] <- gsub("_small$"
                                         , ""
                                         , df_results[, "INDEX_NAME"])

      # QC for if BCG Levels not consecutive
      # that is, more than 1 level apart
      bcg_diff_2 <- abs(df_results$Primary_BCG_Level -
                          df_results$Secondary_BCG_Level)
      bcg_diff_2[is.na(bcg_diff_2)] <- 0
      bcg_diff_2_pf <- ifelse(bcg_diff_2 >= 2, "FAIL", "PASS")
      df_results[, "QC_BCG_NonConsecutive"] <- bcg_diff_2_pf

      # Save, Flags Summary
      # fn_levflags <- paste0(fn_input_base, fn_abr_save, "6levflags.csv")
      fn_levflags <- "BCG_6levflags.csv"
      dn_levflags <- path_results_sub
      pn_levflags <- file.path(dn_levflags, fn_levflags)
      write.csv(df_lev_flags_summ, pn_levflags, row.names = TRUE)

      # Save, Results
      # fn_results <- paste0(fn_input_base, fn_abr_save, "RESULTS.csv")
      fn_results <- "_BCG_RESULTS.csv"
      dn_results <- path_results_sub
      pn_results <- file.path(dn_results, fn_results)
      write.csv(df_results, pn_results, row.names = FALSE)

      # Save, Flag Metrics
      # fn_metflags <- paste0(fn_input_base, fn_abr_save, "6metflags.csv")
      fn_metflags <- "BCG_6metflags.csv"
      dn_metflags <- path_results_sub
      pn_metflags <- file.path(dn_metflags, fn_metflags)
      write.csv(df_metflags, pn_metflags, row.names = FALSE)


      ## Calc, 9, RMD----
      prog_detail <- "Calculate, Create Report"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      strFile.RMD <- file.path("external"
                               , "RMD_Results"
                               , "Results_BCG_Summary.Rmd")
      strFile.RMD.format <- "html_document"
      # strFile.out <- paste0(fn_input_base, fn_abr_save, "RESULTS.html")
      strFile.out <- "_BCG_RESULTS.html"
      dir.export <- path_results_sub
      rmarkdown::render(strFile.RMD
                        , output_format = strFile.RMD.format
                        , output_file = strFile.out
                        , output_dir = dir.export
                        , quiet = TRUE)


      ## Calc, 10, Save, Reference----
      prog_detail <- "Calculate, Save, Reference"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      path_results_ref <- file.path(path_results, dn_files_ref)

      ## Index Class
      fn_save <- "IndexClass.xlsx"
      file_from <- temp_indexclass_crit
      file_to <- file.path(path_results_ref, fn_save)
      file.copy(file_from, file_to)

      # ## Metric Flags
      # fn_save <- "MetricFlags.xlsx"
      # file_from <- temp_bcg_checks
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)
      #
      # ## Metric Names
      # fn_save <- "MetricNames.xlsx"
      # file_from <- temp_metricnames
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)
      #
      # ## Metric Scoring
      # fn_save <- "MetricScoring.xlsx"
      # file_from <- temp_metricscoring
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)
      #
      # ## BCG Rules
      # fn_save <- "Rules.xlsx"
      # file_from <- temp_bcg_models
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)


      ## Calc, 11, Clean Up----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # Create zip file of results
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      # button, enable, download
      shinyjs::enable("b_download_bcg")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Total Records (Input) = ", nrow(df_input)
                    , "\n\n"
                    , "Elapse Time (", units(duration), ") = ", round(duration, 2))
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Calculating BCG"
    )## withProgress ~ END
  }##expr ~ ObserveEvent ~ END
  )##observeEvent ~ b_calc_bcg ~ END

  ## b_download_BCG ----
  output$b_download_bcg <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_bcg
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ BCG

  # Calc, IBI ----
  ## b_Calc_IBI ----
  observeEvent(input$b_calc_ibi, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 0, Set Up Shiny Code ----

      prog_detail <- "Calculation, IBI..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 13
      prog_sleep <- 0.25

      ## Calc, 1, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      # 2023-12-14, add community
      fn_comm <- .simpleCap(input$si_community_ibi)
      fn_abr <- abr_ibi
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_comm, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # reference folder
      path_results_ref <- file.path(path_results, dn_files_ref)
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }

      # button, disable, download
      shinyjs::disable("b_download_ibi")

      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      ## Calc, 3, QC and Test Inputs ----
      prog_detail <- "Calculate, QC and Test Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      ### QC, names to upper case
      names(df_input) <- toupper(names(df_input))

      ### QC, Index_Name
      # Only MN_IBI so can add if missing
      if (!"INDEX_NAME" %in% toupper(names(df_input))) {
        df_input[, "INDEX_NAME"] <- paste0("MN_IBI_", fn_comm)
      }## IF ~ INDEX_NAME

      ### QC, Index_Name, Community
      # confirm data matches selection
      comm_sel <- paste0("MN_IBI_", fn_comm)
      comm_dat <- unique(df_input[, "INDEX_NAME"])

      if (length(comm_dat) > 1) {
        msg <- paste0("More than one INDEX_NAME in data!"
                      , "\n\n"
                      , "Only a single index community can be used at one time.")
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## Mult comm-data

      if (comm_sel != comm_dat) {
        msg <- paste0("Community selection does not match data!"
                      , "\n\n"
                      , "Selection = ", comm_sel
                      , "\n\n"
                      , "Data = ", comm_dat)
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ comm_sel != comm_dat


      ### QC, check for required fields
      col_req_both <- c("INDEX_NAME"
                        , "INDEX_CLASS"
                        , "SAMPLEID"
                        , "TAXAID"
                        , "N_TAXA"
                        , "DRAINSQMI")
      col_req_bugs <- c("LARGERARECOUNT"
                        , "FFG"
                        , "TOLVAL"
                        , "TOLVAL2"
                        , "ORDER"
                        , "FAMILY"
                        , "HABIT"
                        , "LONGLIVED"
                        , "CLASS")
      col_req_fish <- c("N_ANOMALIES"
                        , "DISTANCE_M"
                        #, "WIDTH_M"
                        , "GRADIENT"
                        #, "SCHOOLING"
                        , "TYPE"
                        , "HABITAT"
                        , "REPRODUCTION"
                        , "TOLER"
                        , "TROPHIC")
      # set col_req for check by community
      if (input$si_community_ibi == "bugs") {
        col_req <- c(col_req_both, col_req_bugs)
      } else if (input$si_community_ibi == "fish") {
        col_req <- c(col_req_both, col_req_fish)
      }## IF ~ community

      col_req_boo <- col_req %in% names(df_input)
      col_req_missing <- col_req[!col_req_boo]

      if (sum(col_req_boo) != length(col_req)) {
        msg <- paste0("Required fields (columns) missing!"
                      , "\n\n"
                      , paste(col_req_missing, collapse = ", "))
        shinyalert::shinyalert(title = "Index Calculation Error"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sum(col_req_boo) != length(col_req)

      # rename DISTANCE_M to LENGTH_M to match BioMonTools code
      names(df_input)[toupper(names(df_input)) == "DISTANCE_M"] <- "SAMP_LENGTH_M"

      ## Calc, 4, Exclude Taxa ----
      prog_detail <- "Calculate, Exclude Taxa"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      message(paste0("User response to generate ExclTaxa = ", input$ExclTaxa_ibi))

      if (input$ExclTaxa_ibi) {
        ## Get TaxaLevel names present in user file
        # to upper so matches rest of file
        phylo_all <- toupper(c("Kingdom"
                       , "Phylum"
                       , "SubPhylum"
                       , "Class"
                       , "SubClass"
                       , "Order"
                       , "SubOrder"
                       , "InfraOrder"
                       , "SuperFamily"
                       , "Family"
                       , "SubFamily"
                       , "Tribe"
                       , "Genus"
                       , "SubGenus"
                       , "Species"
                       , "Variety"))

        # case and matching of taxa levels handled inside of markExluded

        # Overwrite existing column if present
        # ok since user checked the box to calculate
        if ("EXCLUDE" %in% toupper(names(df_input))) {
          # save original user input
          df_input[, "EXCLUDE_USER"] <- df_input[, "EXCLUDE"]
          # drop column
          df_input <- df_input[, !names(df_input) %in% "EXCLUDE"]
        }## IF ~ Exclude

        # Skip if no Phylo data
        TaxaLevels <- toupper(c("Kingdom"
                        , "Phylum"
                        , "SubPhylum"
                        , "Class"
                        , "SubClass"
                        , "Order"
                        , "SubOrder"
                        , "SuperFamily"
                        , "Family"
                        , "SubFamily"
                        , "Tribe"
                        , "Genus"
                        , "SubGenus"
                        , "Species"
                        , "Variety"))
        TaxaLevels_boo <- TaxaLevels %in% toupper(names(df_input))
        if (sum(TaxaLevels_boo) == 0) {
          boo_Exclude <- "EXCLUDE" %in% toupper(names(df_input))
          if (boo_Exclude == FALSE) {
            df_input[, "EXCLUDE"] <- FALSE
          }## IF ~ boo_Exclude == FALSE
        } else {
          # overwrite current data frame
          df_input <- BioMonTools::markExcluded(df_samptax = df_input
                                                , SampID = "SAMPLEID"
                                                , TaxaID = "TAXAID"
                                                , TaxaCount = "N_TAXA"
                                                , Exclude = "EXCLUDE"
                                                , TaxaLevels = phylo_all
                                                , Exceptions = NA)

        }## IF ~ sum(TaxaLevels_boo) == 0

        # Save Results
        # fn_excl <- paste0(fn_input_base, fn_abr_save, "1markexcl.csv")
        fn_excl <- "IBI_1markexcl.csv"
        dn_excl <- path_results_sub
        pn_excl <- file.path(dn_excl, fn_excl)
        write.csv(df_input, pn_excl, row.names = FALSE)

      }## IF ~ input$ExclTaxa






      ## Calc, x3, IBI Flag Cols ----
      # get columns from Flags (non-metrics) to carry through
      prog_detail <- "Calculate, Keep IBI Model Columns"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Rules - should all be metrics but leaving here just in case
      # Flags - not always metrics,
      # Index Name for import data
      import_IndexName <- unique(df_input$INDEX_NAME)
      # QC Flags for chosen IBI model (non-metrics)
      cols_flags <- unique(df_checks[df_checks$Index_Name == import_IndexName
                                     , "Metric_Name"])
      # can also add other columns to keep if feel so inclined
      cols_flags_keep <- cols_flags[cols_flags %in% names(df_input)]
      if (input$si_community_ibi == "bugs") {
        cols_flags_keep <- unique(c(cols_flags_keep
                                    , "DRAINSQMI"))
      } else if (input$si_community_ibi == "fish") {
        cols_flags_keep <- unique(c(cols_flags_keep
                                , "GRADIENT"
                                , "DRAINSQMI"))
      }## IF ~ community


      ## Calc, x3b, Rules ----
      prog_detail <- "Calculate, IBI Rules"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community_ibi))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # filter for data Index_Name in data (drop 2 extra columns)
      col_drop <- c("SITE_TYPE", "INDEX_REGION") # none
      df_rules <- df_metricscoring[df_metricscoring$INDEX_NAME == import_IndexName
                                , !names(df_metricscoring) %in% col_drop]

      # Metric names
      metnames <- sort(unique(df_rules$METRIC_NAME))
      metnames_grepl_lr <- grepl("^nt_|^pt_", metnames)
      metnames_lr_t <- metnames[metnames_grepl_lr]
      metnames_lr_f <- metnames[!metnames_grepl_lr]

      # Save
      # fn_rules <- paste0(fn_input_base, fn_abr_save, "3metrules.csv")
      fn_rules <- "IBI_3metrules.csv"
      dn_rules <- path_results_sub
      pn_rules <- file.path(dn_rules, fn_rules)
      write.csv(df_rules, pn_rules, row.names = FALSE)

      ## Calc, 4, MetVal----
      prog_detail <- "Calculate, Metric, Values"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community_ibi))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)


      if(input$si_community_ibi == "bugs") {
        ### Calc, MetVal, Bugs ----
        #### LargeRare MOD ----
        df_input_lr_t <- df_input %>%
          dplyr::mutate(LARGERARECOUNT = tidyr::replace_na(LARGERARECOUNT, 0)) %>%
          dplyr::mutate(N_TAXA = N_TAXA + LARGERARECOUNT)
        df_input_lr_f <- df_input # no changes needed

        if (length(cols_flags_keep) > 0) {
          # keep extra cols from Flags (non-metric)
          fun_cols2keep <- cols_flags_keep
        } else {
          fun_cols2keep <- NULL
        }## IF ~ length(col_rules_keep)
        df_metval_lr_t <- BioMonTools::metric.values(df_input_lr_t
                                        , fun.Community = input$si_community_ibi
                                        , fun.MetricNames = metnames_lr_t
                                        , fun.cols2keep = fun_cols2keep
                                        , boo.Shiny = TRUE
                                        , verbose = TRUE
                                        , taxaid_dni = "DNI")
        df_metval_lr_f <- BioMonTools::metric.values(df_input_lr_f
                                                     , fun.Community = input$si_community_ibi
                                                     , fun.MetricNames = metnames_lr_f
                                                     , fun.cols2keep = fun_cols2keep
                                                     , boo.Shiny = TRUE
                                                     , verbose = TRUE
                                                     , taxaid_dni = "DNI")
        # merge two outputs
        # drop ni_total (default) from lr_t
        df_metval <- merge(df_metval_lr_t[, !(names(df_metval_lr_t) %in% "ni_total")]
                           , df_metval_lr_f
                           , by = c("SAMPLEID"
                                    , "INDEX_NAME"
                                    , "INDEX_CLASS"
                                    , "DRAINSQMI"))


      } else if (input$si_community_ibi == "fish") {
        ### Calc, MetVal, Fish ----
        if (length(cols_flags_keep) > 0) {
          # keep extra cols from Flags (non-metric)
          fun_cols2keep <- cols_flags_keep
        } else {
          fun_cols2keep <- NULL
        }## IF ~ length(col_rules_keep)
        df_metval <- BioMonTools::metric.values(df_input
                                                , input$si_community_ibi
                                                , fun.cols2keep = fun_cols2keep
                                                , boo.Shiny = TRUE
                                                , verbose = TRUE
                                                , taxaid_dni = "DNI")
      }## IF ~ community


      ### MetVal MODS----
      # Value Modifications for MN
      #### BUGS
      if (input$si_community_ibi == "bugs") {
        ##### Keep original values
        suffix_orig <- "_ORIG"
        cols_mod_ibi <- c("x_HBI2"
                          , "pi_Chi2Dipt"
                          , "x_HBI"
                          , "pi_tv_toler8"
                          , "pi_TrichNoHydro"
                          , "nt_Odon"
                          , "nt_Pleco"
                          , "nt_tv_intol2"
                          , "pt_Insect")
        cols_mod_ibi_orig <- paste0(cols_mod_ibi, suffix_orig)
        df_metval[, cols_mod_ibi_orig] <- df_metval[, cols_mod_ibi]
        ##### Modications
        # Log10(x+1)
        df_metval <- dplyr::mutate(df_metval
            # y = x - ((m * log10(z)) + b)
            # z is gradient or drainage area
            , x_HBI2 = dplyr::case_when(INDEX_CLASS == "9"
                                ~ x_HBI2 - ((0.579 * log10(DRAINSQMI)) + 17.923)
                                , .default = x_HBI2)
            , pi_Chi2Dipt = dplyr::case_when(INDEX_CLASS == "9"
                                     ~ pi_Chi2Dipt - ((9.428 * log10(DRAINSQMI)) + 45.12)
                                     , .default = pi_Chi2Dipt)
            , x_HBI = dplyr::case_when(INDEX_CLASS == "9"
                               ~ x_HBI - ((0.375 * log10(DRAINSQMI)) + 6.048)
                               , .default = x_HBI)
            , pi_tv_toler8 = dplyr::case_when(INDEX_CLASS == "9"
                                      ~ pi_tv_toler8 - ((4.239 * log10(DRAINSQMI)) + 7.249)
                                      , .default = pi_tv_toler8)
            # Log10(x+1)
            , pi_TrichNoHydro = dplyr::case_when(INDEX_CLASS == "1" |
                                                   INDEX_CLASS == "2" |
                                                   INDEX_CLASS == "4" |
                                                   INDEX_CLASS == "6" |
                                                   INDEX_CLASS == "7"
                                       ~ log10(pi_TrichNoHydro + 1)
                                       , .default = pi_TrichNoHydro)
            , nt_Odon = dplyr::case_when(INDEX_CLASS == "3" | INDEX_CLASS == "5"
                                 ~ log10(nt_Odon + 1)
                                 , .default = nt_Odon)
            , nt_Pleco = dplyr::case_when(INDEX_CLASS == "3" | INDEX_CLASS == "5"
                                  ~ log10(nt_Pleco + 1)
                                  , .default = nt_Pleco)
            , nt_tv_intol2 = dplyr::case_when(INDEX_CLASS == "4" | INDEX_CLASS == "6" | INDEX_CLASS == "7"
                                      ~ log10(nt_tv_intol2 + 1)
                                      , .default = nt_tv_intol2)
            # Log10(x+1) where x is 0-1 not 0-100
            # , pi_TrichNoHydro = dplyr::case_when(INDEX_CLASS == "4" | INDEX_CLASS == "6" | INDEX_CLASS == "7"
            #                              ~ log10((pi_TrichNoHydro / 100) + 1)
            #                              , .default = pi_TrichNoHydro)
            # ArcSin(Sqrt)
            , pt_Insect = dplyr::case_when(INDEX_CLASS == "3" | INDEX_CLASS == "5"
                                   ~ asin(sqrt(pt_Insect / 100))
                                   , .default = pt_Insect)

        )## mutate
      }## IF ~ bugs

      #### FISH
      if (input$si_community_ibi == "fish") {
        ##### Keep original values
        suffix_orig <- "_ORIG"
        cols_mod_ibi <- c("nt_simplelithophil"
                          , "pt_tv_sens"
                          , "pi_tv_sens_ExclSchool"
                          , "pi_tv_senscoldwater_ExclSchool"
                          , "pt_detritivore"
                          , "nt_tv_tolercoldwater"
                          , "pt_natcoldwater"
                          , "pt_tv_senscoldwater"
                          , "pi_natcoldwater_ExclSchool"
                          , "pi_tv_tolercoldwater_ExclSchool"
                          , "pi_nonlithophil_ExclSchool"
                          , "pi_Perciformes_ExclSchool")
        cols_mod_ibi_orig <- paste0(cols_mod_ibi, suffix_orig)
        df_metval[, cols_mod_ibi_orig] <- df_metval[, cols_mod_ibi]
        ##### Modications
        df_metval <- dplyr::mutate(df_metval
            # change 8 and 9 to 10 and 11
            # y = x - ((m * log10(z)) + b)
            # z is gradient or drainage area
            , nt_simplelithophil = dplyr::case_when(INDEX_CLASS == "1"
                                             ~ nt_simplelithophil - ((3.945 * log10(GRADIENT)) + 11.187)
                                             , .default = nt_simplelithophil)
            , pt_tv_sens = dplyr::case_when(INDEX_CLASS == "1"
                                     ~ pt_tv_sens - ((16.042 * log10(GRADIENT)) + 33.5)
                                     , .default = pt_tv_sens)
            , pt_tv_sens = dplyr::case_when(INDEX_CLASS == "4"
                                     ~ pt_tv_sens - ((11.902 * log10(GRADIENT)) + 43.121)
                                     , .default = pt_tv_sens)
            , pi_tv_sens_ExclSchool = dplyr::case_when(INDEX_CLASS == "4"
                                                ~ pi_tv_sens_ExclSchool - ((22.503 * log10(GRADIENT)) + 51.121)
                                                , .default = pi_tv_sens_ExclSchool)
            , pi_tv_senscoldwater_ExclSchool = dplyr::case_when(INDEX_CLASS == "10"
                                                         ~ pi_tv_senscoldwater_ExclSchool - ((-27.382 * log10(DRAINSQMI)) + 114.322)
                                                         , .default = pi_tv_senscoldwater_ExclSchool)
            , pt_detritivore = dplyr::case_when(INDEX_CLASS == "10"
                                         ~ pt_detritivore - ((16.211 * log10(DRAINSQMI)) + -5.276)
                                         , .default = pt_detritivore)
            , nt_tv_tolercoldwater = dplyr::case_when(INDEX_CLASS == "10"
                                               ~ nt_tv_tolercoldwater - ((1.089 * log10(DRAINSQMI)) + -0.827)
                                               , .default = nt_tv_tolercoldwater)
            , pt_natcoldwater = dplyr::case_when(INDEX_CLASS == "10"
                                          ~ pt_natcoldwater - ((-24.242 * log10(DRAINSQMI)) + 54.017)
                                          , .default = pt_natcoldwater)
            , pt_tv_senscoldwater = dplyr::case_when(INDEX_CLASS == "11"
                                              ~ pt_tv_senscoldwater - ((23.788 * log10(GRADIENT)) + 24.437)
                                              , .default = pt_tv_senscoldwater)
            # Log10(x+1)
            , pi_natcoldwater_ExclSchool = dplyr::case_when(INDEX_CLASS == "10"
                                                     ~ log10(pi_natcoldwater_ExclSchool + 1)
                                                     , .default = pi_natcoldwater_ExclSchool)
            , pi_tv_tolercoldwater_ExclSchool = dplyr::case_when(INDEX_CLASS == "11"
                                                          ~ log10(pi_tv_tolercoldwater_ExclSchool + 1)
                                                          , .default = pi_tv_tolercoldwater_ExclSchool)
            , pi_nonlithophil_ExclSchool = dplyr::case_when(INDEX_CLASS == "11"
                                                     ~ log10(pi_nonlithophil_ExclSchool + 1)
                                                     , .default = pi_nonlithophil_ExclSchool)
            , pi_Perciformes_ExclSchool = dplyr::case_when(INDEX_CLASS == "11"
                                                    ~ log10(pi_Perciformes_ExclSchool + 1)
                                                    , .default = pi_Perciformes_ExclSchool)
          )## MUTATE
      }## IF ~ fish

      ### Save Results ----

      # fn_metval <- paste0(fn_input_base, fn_abr_save, "2metval_all.csv")
      fn_metval <- "IBI_2metvall_all.csv"
      dn_metval <- path_results_sub
      pn_metval <- file.path(dn_metval, fn_metval)
      write.csv(df_metval, pn_metval, row.names = FALSE)

      ### Save, MetVal ----
      # Munge
      ## Model and QC Flag metrics only
      # cols_flags defined above
      cols_model_metrics <- unique(df_rules[, "METRIC_NAME", TRUE])
      cols_req <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                    , "ni_total", "nt_total"
                    , "ni_total_ExclSchool", "nt_total_ExclSchool")
      cols_metrics_flags_keep <- unique(c(cols_req
                                          , cols_flags
                                          , cols_model_metrics
                                          , cols_mod_ibi_orig))
      df_metval_slim <- df_metval[, names(df_metval) %in%
                                    cols_metrics_flags_keep]
      # Save
      # fn_metval_slim <- paste0(fn_input_base, fn_abr_save, "2metval_IBI.csv")
      fn_metval_slim <- "IBI_2metval_IBI.csv"
      dn_metval_slim <- path_results_sub
      pn_metval_slim <- file.path(dn_metval_slim, fn_metval_slim)
      write.csv(df_metval_slim, pn_metval_slim, row.names = FALSE)


      ## Calc, 55, IBI Scores ----
      prog_detail <- "Calculate, IBI Scores"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # EXAMPLE
      # Thresholds
      fn_thresh <- file.path(system.file(package = "BioMonTools")
                             , "extdata"
                             , "MetricScoring.xlsx")
      df_thresh_metric <- read_excel(fn_thresh, sheet = "metric.scoring")
      df_thresh_index <- read_excel(fn_thresh, sheet = "index.scoring")

      # # index to BCG.PacNW.L1
      # df_metric_values_bugs$INDEX_NAME <- myIndex
      # df_metric_values_bugs$INDEX_CLASS <- "ALL"

      # SCORE Metrics
      df_metric_scores <- metric.scores(df_metval
                                             , cols_metrics_flags_keep
                                             , "INDEX_NAME"
                                             , "INDEX_CLASS"
                                             , df_thresh_metric
                                             , df_thresh_index)


      ## Calc, 99, Post Index Adjustments ----
      prog_detail <- "Calculate, Post IBI Score Adjustments"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      ### Bugs
      # No Adjustments

      ### Fish
      if (input$si_community_ibi == "fish") {
        # Low End Scoring
        # Some metrics to score of zero
        # INDEX_CLASS 1, 2, 4, 5
        # pi_* to zero when ni_total < 25
        # nt_* and pt_* to zero when nt_total < 6
        # INDEX_CLASS 3, 6, 7
        # pi_* to zero when ni_total < 25
        # nt_* and pt_* to zero when nt_total < 6
        # INDEX_CLASS 10 and 11 (old 8 and 9)
        # no adjustment
        ic_les_1 <- c("1", "2", "4", "5")
        ic_les_2 <- c("3", "6", "7")
        ic_les_all <- c(ic_les_1, ic_les_2)

        # Rework Final Scoring, part 1/2
        df_metric_scores <- df_metric_scores %>%
          # Add Index_n_metrics
          mutate(Index_n_metrics = case_when(INDEX_CLASS == "1" ~ 11
                                             , INDEX_CLASS == "2" ~ 8
                                             , INDEX_CLASS == "3" ~ 6
                                             , INDEX_CLASS == "4" ~ 10
                                             , INDEX_CLASS == "5" ~ 11
                                             , INDEX_CLASS == "6" ~ 10
                                             , INDEX_CLASS == "7" ~ 9
                                             , INDEX_CLASS == "8" ~ 7 # class 10
                                             , INDEX_CLASS == "9" ~ 8 # class 11
                                             , INDEX_CLASS == "10" ~ 7
                                             , INDEX_CLASS == "11" ~ 8
                                             , .default = NA)) %>%
          # modify index columns
          mutate(sum_Index_ORIG = sum_Index
                 , Index_ORIG = Index
                 , sum_Index = 0
                 , Index = 0
                 ) %>%
          # LES, Multiplier
          mutate(les_mult_ni = case_when(ni_total_ExclSchool < 25
                                    & INDEX_CLASS %in% ic_les_1 ~ 0
                                    , .default = 1)) %>%
          mutate(les_mult_ni = case_when(ni_total_ExclSchool < 25
                                         & INDEX_CLASS %in% ic_les_2 ~ 0
                                         , .default = les_mult_ni)) %>%
          mutate(les_mult_nt = case_when(nt_total_ExclSchool < 6
                                         & INDEX_CLASS %in% ic_les_1 ~ 0
                                         , .default = 1)) %>%
          mutate(les_mult_nt = case_when(nt_total_ExclSchool < 4
                                         & INDEX_CLASS %in% ic_les_2 ~ 0
                                         , .default = les_mult_nt)) %>%
          # LES, Adjust Scores, pi
          mutate(across(starts_with("SC_pi_"), ~ . * les_mult_ni)) %>%
          # LES, Adjust Scores, nt
          mutate(across(starts_with("SC_nt_"), ~ . * les_mult_nt)) %>%
          # LES, Adjust Scores, pt
          mutate(across(starts_with("SC_pt_"), ~ . * les_mult_nt)) %>%
          # Re-sum SCORES
          rowwise() %>%
            mutate(sum_Index = sum(c_across(cols = starts_with("SC_"))
                                    , na.rm = TRUE))


        # QC
        show_msg <- FALSE
        if(show_msg) {
          select(df_metric_scores, c(INDEX_CLASS
                                     , ni_total
                                     , les_mult_ni
                                     , nt_total
                                     , les_mult_nt))
        }## show_msg

        # Rework Final Scoring, part 2/2
        df_metric_scores <- df_metric_scores %>%
          # Recalc Index
          mutate(Index = round(sum_Index * 10 / Index_n_metrics, 2)) %>%
          # Score % DELT (Exclude School)
          mutate(Index_mod_delt_ExclSchool = case_when(pi_delt_ExclSchool >= 4 ~ -10
                                            , pi_delt_ExclSchool >= 2 ~ -5
                                            , .default = 0)) %>%
          # Update sum_index
          mutate(Index = max(0, Index + Index_mod_delt_ExclSchool, na.rm = TRUE))

        ## Recalc Index_Nar ----
        # 20240919, Recalc Narrative after adjustments
        # reuse code from BioMonTools::metric.scores.R

        DF_Thresh_Index <- df_thresh_index
        DF_Metrics      <- df_metric_scores
        col_IndexName   <- "INDEX_NAME"
        col_IndexClass  <- "INDEX_CLASS"
        col_ni_total    <- "ni_total"

        # Need to cycle based on Index (aa) and Region (bb)
        for (aa in unique(as.matrix(DF_Metrics[,col_IndexName]))) {
          for (bb in unique(as.matrix(DF_Metrics[,col_IndexClass]))) {

            # Thresholds (filter with dplyr)
            fun.Thresh.myIndex <- as.data.frame(dplyr::filter(DF_Thresh_Index
                                                              , INDEX_NAME == aa
                                                              & INDEX_CLASS == bb))
            # QC
            if (nrow(fun.Thresh.myIndex) != 1) {
              #return(0)
              next
            }
            # thresholds
            #fun.NumMetrics       <- as.numeric(fun.Thresh.myIndex[, "NumMetrics"])
            fun.ScoreRegime      <- fun.Thresh.myIndex[, "ScoreRegime"]
            fun.Scale            <- fun.Thresh.myIndex[, "ScoreScaling"]
            fun.Index.Nar.Thresh <- fun.Thresh.myIndex[, c(paste0("Thresh0"
                                                                  , seq_len(7)))]
            fun.Index.Nar.Nar    <- fun.Thresh.myIndex[, c(paste0("Nar0", seq_len(6)))]

            fun.Index.Nar.Numb <- sum(!is.na(fun.Index.Nar.Nar), na.rm = TRUE)

            fun.ZeroInd_Use <- fun.Thresh.myIndex[, "Use_ZeroInd"]
            fun.ZeroInd_Sc  <- fun.Thresh.myIndex[, "ZeroInd_Score"]
            fun.ZeroInd_Nar <- fun.Thresh.myIndex[, "ZeroInd_Narrative"]

            # default value
            # 20240919, cut out default numeric value

            ### INDEX, Score Regime
            # 20240919, cut out summation of index value
            fun.Result <- DF_Metrics[, "Index", TRUE] # tibble to vector

            # Narrative
            myBreaks <- as.numeric(paste(fun.Index.Nar.Thresh[1, 1:(fun.Index.Nar.Numb + 1)]))
            myLabels <- paste(fun.Index.Nar.Nar[1, 1:fun.Index.Nar.Numb])
            fun.Result.Nar <- as.vector(cut(fun.Result
                                            , breaks = myBreaks
                                            , labels = myLabels
                                            , include.lowest = TRUE
                                            , right = FALSE
                                            , ordered_result = TRUE))

            # Update for zero individuals
            if (fun.ZeroInd_Use == TRUE) {
              boo_zero_ni_total <- DF_Metrics[, col_ni_total] == 0
              fun.Result[boo_zero_ni_total]     <- fun.ZeroInd_Sc
              fun.Result.Nar[boo_zero_ni_total] <- fun.ZeroInd_Nar
            }##IF~fun.zeroind_use~END


            # Update input DF with matching values
            myTF <- DF_Metrics[, col_IndexName] == aa & DF_Metrics[, col_IndexClass] == bb
            DF_Metrics[myTF, "Index"]     <- fun.Result[myTF]
            DF_Metrics[myTF, "Index_Nar"] <- fun.Result.Nar[myTF]

            # Add factor levels for Index_Nar
            ## only works if doing a single index or multiple indices with the same narrative categories
            # DF_Metrics[myTF, "Index_Nar"] <- factor(DF_Metrics[myTF, "Index_Nar"]
            #                                         , levels = fun.Index.Nar.Nar
            #                                         , labels = fun.Index.Nar.Nar
            #                                         , ordered = TRUE)

          }##FOR.bb.END
        }##FOR.aa.END
        #
        # 20240919, update data frame to include changes
        df_metric_scores <- DF_Metrics


        # QC
        if(show_msg) {
          select(df_metric_scores, c(sum_Index_ORIG
                                     , Index_ORIG
                                     , sum_Index
                                     , Index
                                     , Index_mod_delt_ExclSchool
                                     , pi_delt_ExclSchool
                                     , Index_n_metrics
                                     ))
        }## show_msg

      }## IF ~ FIBI


      ## Calc, 8, QC Flags----
      prog_detail <- "Calculate, QC Flags"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # 2023-12-06
      # Split if no flags so doesn't crash

      # Check if Flags exist for data
      col_index_metval <- c("INDEX_NAME", "INDEX_CLASS")
      col_index_checks <- c("Index_Name", "INDEX_CLASS")
      index_metval <- unique(df_metval[, col_index_metval])
      index_checks <- unique(df_checks[, col_index_checks])
      index_merge <- merge(index_metval, index_checks
                           , by.x = col_index_metval
                           , by.y = col_index_checks)

      # if (nrow(index_merge) == 0) {
      #
      #   # create dummy files
      #   str_nodata <- "No flags for the Index Name/Class combinations present in data"
      #   # Flags
      #   df_flags <- data.frame(x = str_nodata
      #                          , CHECKNAME = "No Flags"
      #                          , FLAG = NA)
      #   df_lev_flags <- df_levassign
      #   # Flags Summary
      #   df_lev_flags_summ <- data.frame(x = str_nodata)
      #   # Results
      #   df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
      #   # Flag Metrics
      #   df_metflags <- data.frame(x = str_nodata)
      #
      # } else {
      #
      #   # Calc
      #   # df_checks loaded in global.R
      #   df_flags <- BioMonTools::qc.checks(df_metval, df_checks)
      #   # Change terminology; PASS/FAIL to NA/flag
      #   df_flags[, "FLAG"][df_flags[, "FLAG"] == "FAIL"] <- "flag"
      #   df_flags[, "FLAG"][df_flags[, "FLAG"] == "PASS"] <- NA
      #   # long to wide format
      #   df_flags_wide <- reshape2::dcast(df_flags
      #                                    , SAMPLEID ~ CHECKNAME
      #                                    , value.var = "FLAG")
      #   # Calc number of "flag"s by row.
      #   df_flags_wide$NumFlags <- rowSums(df_flags_wide == "flag", na.rm = TRUE)
      #   # Rearrange columns
      #   NumCols <- ncol(df_flags_wide)
      #   df_flags_wide <- df_flags_wide[, c(1, NumCols, 2:(NumCols - 1))]
      #   # Merge Levels and Flags
      #   df_lev_flags <- merge(df_levassign
      #                         , df_flags_wide
      #                         , by.x = "SampleID"
      #                         , by.y = "SAMPLEID"
      #                         , all.x = TRUE)
      #   # Flags Summary
      #   df_lev_flags_summ <- as.data.frame.matrix(table(df_flags[, "CHECKNAME"]
      #                                                   , df_flags[, "FLAG"]
      #                                                   , useNA = "ifany"))
      #   # Results
      #   df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
      #   ## remove L1:6
      #
      #   # Flag Metrics
      #   col2keep_metflags <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
      #                          , "METRIC_NAME", "CHECKNAME", "METRIC_VALUE"
      #                          , "SYMBOL", "VALUE", "FLAG")
      #   df_metflags <- df_flags[, col2keep_metflags]
      #
      # }## IF ~ check for matching index name and class


      # # Save, Flags Summary
      # fn_levflags <- paste0(fn_input_base, fn_abr_save, "6levflags.csv")
      # dn_levflags <- path_results_sub
      # pn_levflags <- file.path(dn_levflags, fn_levflags)
      # write.csv(df_lev_flags_summ, pn_levflags, row.names = TRUE)

      # Save, Results
      df_results <- df_metric_scores
      # Munge
      ## Rename SAMP_LENGTH_M to DISTANCE_M
      names(df_results)[names(df_results) %in% "SAMP_LENGTH_M"] <- "DISTANCE_M"
      ## IBI only columns
      cols_ibi_req <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                        , "GRADIENT", "DRAINSQMI", "DISTANCE_M"
                        , "ni_total", "nt_total"
                        , "ni_total_ExclSchool", "nt_total_ExclSchool")
      cols_ibi_met_val <- unique(df_rules[, "METRIC_NAME", TRUE])
      cols_ibi_met_sco <- paste0("SC_", cols_ibi_met_val)
      cols_ibi_index <- c("sum_Index", "Index", "Index_Nar"
                          , "Index_n_metrics")
      cols_ibi_index_fish <- c("sum_Index_ORIG", "Index_ORIG"
                               , "les_mult_ni", "les_mult_nt"
                               , "Index_mod_delt_ExclSchool")
      cols_ibi_keep <- unique(c(cols_ibi_req
                                , cols_ibi_met_val
                                , cols_ibi_met_sco
                                , cols_ibi_index
                                , cols_ibi_index_fish))
      df_results_slim <- df_results[, names(df_results) %in% cols_ibi_keep]
      # ok to have fish specific when bugs since using "%in%"

      # fn_results <- paste0(fn_input_base, fn_abr_save, "RESULTS.csv")
      fn_results <- "IBI_4results_all.csv" # 20240919, rename
      dn_results <- path_results_sub
      pn_results <- file.path(dn_results, fn_results)
      write.csv(df_results_slim, pn_results, row.names = FALSE)

      # Slimmer file, 20240919
      cols_slim2 <- c("SAMPLEID",
                      "INDEX_NAME",
                      "INDEX_CLASS",
                      "Index",
                      "Index_Nar")
      df_results_slim2 <- df_results_slim[, cols_slim2]
      fn_results <- "_IBI_RESULTS.csv"
      dn_results <- path_results_sub
      pn_results <- file.path(dn_results, fn_results)
      write.csv(df_results_slim2, pn_results, row.names = FALSE)

      # Save, Flag Metrics
      # fn_metflags <- paste0(fn_input_base, fn_abr_save, "6metflags.csv")
      # dn_metflags <- path_results_sub
      # pn_metflags <- file.path(dn_metflags, fn_metflags)
      # write.csv(df_metflags, pn_metflags, row.names = FALSE)


      ## Calc, 9, RMD----
      prog_detail <- "Calculate, Create Report"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      strFile.RMD <- file.path("external"
                               , "RMD_Results"
                               , "Results_IBI_Summary.Rmd")
      strFile.RMD.format <- "html_document"
      # strFile.out <- paste0(fn_input_base, fn_abr_save, "RESULTS.html")
      strFile.out <- "_BCG_RESULTS.html"
      dir.export <- path_results_sub
      # rmarkdown::render(strFile.RMD
      #                   , output_format = strFile.RMD.format
      #                   , output_file = strFile.out
      #                   , output_dir = dir.export
      #                   , quiet = TRUE)

      ## Calc, 10, Save, Reference----
      prog_detail <- "Calculate, Save, Reference"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      path_results_ref <- file.path(path_results, dn_files_ref)

      ## Index Class
      fn_save <- "IndexClass.xlsx"
      file_from <- temp_indexclass_crit
      file_to <- file.path(path_results_ref, fn_save)
      file.copy(file_from, file_to)

      ## Metric Flags
      # fn_save <- "MetricFlags.xlsx"
      # file_from <- temp_ibi_checks
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)

      # ## Metric Names
      # fn_save <- "MetricNames.xlsx"
      # file_from <- temp_metricnames
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)
      #
      # ## Metric Scoring
      # fn_save <- "MetricScoring.xlsx"
      # file_from <- temp_metricscoring
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)

      # ## IBI Rules
      # fn_save <- "Rules.xlsx"
      # file_from <- temp_ibi_models
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)


      ## Calc, 11, Clean Up----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # Create zip file of results
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      # button, enable, download
      shinyjs::enable("b_download_ibi")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Total Records (Input) = ", nrow(df_input)
                    , "\n\n"
                    , "Elapse Time (", units(duration), ") = ", round(duration, 2))
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Calculating IBI"
    )## withProgress ~ END
  }##expr ~ ObserveEvent ~ END
  )##observeEvent ~ b_calc_ibi ~ END

  ## b_download_IBI ----
  output$b_download_ibi <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_ibi
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ IBI


  #~~~~MAP~~~~----
  ### MAP ----

  ### Map, UI ----

  output$UI_map_datatype <- renderUI({
    str_col <- "Select data type (calculation) to map."
    selectInput("map_datatype"
                , label = str_col
                , choices = c("", map_datatypes)
                , multiple = FALSE)
  })## UI_datatype

  output$UI_map_col_xlong <- renderUI({
    str_col <- "Column, Longitude (decimal degrees))"
    selectInput("map_col_xlong"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Longitude"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_ylat <- renderUI({
    str_col <- "Column, Latitude (decimal degrees)"
    selectInput("map_col_ylat"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Latitude"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_sampid <- renderUI({
    str_col <- "Column, SampleID (unique station or sample identifier)"
    selectInput("map_col_sampid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "SampleID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_mapval <- renderUI({
    str_col <- "Column, Value to Map (e.g., BCG or IBI value)"
    selectInput("map_col_mapval"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "Index_Value"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_map_col_keep <- renderUI({
    str_col <- "Additional Columns to Keep in Map Popup"
    selectInput("map_col_keep"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  ### Map, Leaflet ----
  output$map_leaflet <- renderLeaflet({

    # data for plot
    df_map <- df_import()

    #     # Rename columns based on user selection
    #     df_map[, ]


    #
    # col_Stations <- "blue"
    # col_Segs     <- "black" # "grey59"
    # fill_Segs    <- "lightskyblue"

    # data_GIS_eco3_orwa_bcg <- data_GIS_eco3_orwa_bcg %>%
    #   mutate(Fill = case_when(BCG_Valid == TRUE ~ "#FFFFFF"
    #                           , TRUE ~ "#808080"
    #   )) %>%
    #   mutate(Border = case_when(BCG_Valid == TRUE ~ "#000000"
    #                             , TRUE ~ "#03F"
    #   ))

    # Map
    #leaflet() %>%
    leaflet(data = df_map) %>%
      # Groups, Base
      # addTiles(group="OSM (default)") %>%  #default tile too cluttered
      addProviderTiles("CartoDB.Positron"
                       , group = "Positron") %>%
      addProviderTiles(providers$OpenStreetMap
                       , group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery
                       , group = "ESRI World Imagery") %>%
      # addProviderTiles(providers$USGS.USImagery
      #                  , group = "USGS Imagery") %>%
      # addPolygons(data = data_GIS_eco3_orwa
      #             , group = "Ecoregions, Level III"
      #             , popup = ~paste0(LEVEL3, ", ", LEVEL3_NAM)
      #             , fillColor = ~LEVEL3
      #             ) %>%
      # addPolygons(data = data_GIS_eco3_orwa_bcg
      #             , group = "Ecoregions, Level III"
      #             , popup = ~paste0(US_L3CODE
      #                               , ", "
      #                               , US_L3NAME
      #                               , ", valid for BCG = "
      #                               , BCG_Valid)
      #             , fillColor = ~Fill
      #             , color = ~Border
      #             , weight = 3
      # ) %>%
      # addPolygons(data = data_GIS_BCGclass
      #             , group = "BCG Class"
      #             , popup = ~BCGclass_v
      #             , fillColor = rgb(255, 0, 195, maxColorValue = 255)) %>%
      # addPolygons(data = data_GIS_NorWeST_ORWA
      #             , group = "NorWeST"
      #             , popup = ~Unit_OBSPR) %>%
      # addPolygons(data = data_GIS_NHDplus_catch_ORWA
      #             , group = "NHD+ Catchments") %>%
      # addPolylines(data = data_GIS_NHDplus_flowline_ORWA
      #              , group = "NHD+ Flowline") %>%
      # # # Groups, Overlay
      # addCircles(lng = ~longitude
      #            , lat = ~latitude
      #            , color = col_Stations
      #            , popup = ~paste0("Station: ", station, as.character("<br>")
      #                            , "Latitude: ", latitude, as.character("<br>")
      #                            , "Longitude: ", longitude, as.character("<br>")
      #                            )
      #            , radius = 30
      #            , group = "Stations") %>%
      # # Legend
      # addLegend("bottomleft"
      #           , colors = c(col_Stations, col_Segs)
      #           , labels = c("Stations", "CB Outline")
      #           , values = NA) %>%
      # Layers, Control
      addLayersControl(baseGroups = c("Positron"
                                      , "Open Street Map"
                                      , "ESRI World Imagery"
                                      # , "USGS Imagery"
      )
      , overlayGroups = c("Ecoregions, Level III"
                          # , "BCG Class"
                          # , "NorWeST"
                          # , "NHD+ Catchments"
                          # , "NHD+ Flowlines"
      )
      ) %>%
      # Layers, Hide
      # hideGroup(c("Ecoregions, Level III"
      #            # , "BCG Class"
      #            # , "NorWeST"
      #            # , "NHD+ Catchments"
      #            # , "NHD+ Flowlines"
      # )) %>%
      # # Mini map
      addMiniMap(toggleDisplay = TRUE) %>%
    # Legend
    # addLegend("bottomleft"
    #           , title = "L3 Ecoregions, BCG Valid"
    #           , colors = c("#000000", "#03F")
    #           , labels = c("TRUE", "FALSE")
    #           # , layerID = "Ecoregions, Level III"
    #           )
      # # Set zoom and view to MN
      # setView(lng = -94.6859,
      #         lat = 46.7296,
      #         zoom = 6)
    # Set zoom and view to Bemidji, MN
    setView(lng = -94.8882,
            lat = 47.4810,
            zoom = 8)



  })## map_leaflet ~ END

  ### Map, Leaflet, Proxy ----
  # update map based on user selections
  # tied to Update button
  # https://rstudio.github.io/leaflet/shiny.html
  # need a reactive to trigger, use map update button
  observeEvent(input$but_map_update, {

    #### Data ----
    df_map <- df_import()
    names_data <- names(df_map)

    no_narrative <- "No Narrative Designation"
    size_default <- 50

    ### Map_L_P, Gather and Test Inputs----
    sel_map_datatype   <- input$map_datatype
    sel_map_col_xlong  <- input$map_col_xlong
    sel_map_col_ylat   <- input$map_col_ylat
    sel_map_col_sampid <- input$map_col_sampid
    sel_map_col_keep   <- input$map_col_keep

    sel_map_col_mapval <- NA_character_
    sel_map_col_mapnar <- NA_character_
    sel_map_col_color  <- NA_character_

    if (is.null(sel_map_datatype) | sel_map_datatype == "") {
      # end process with pop up
      msg <- "'Data Type' name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_datatype

    if (is.null(sel_map_col_xlong) | sel_map_col_xlong == "") {
      # end process with pop up
      msg <- "'Longitude' column name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_xlong

    if (is.null(sel_map_col_ylat) | sel_map_col_ylat == "") {
      # end process with pop up
      msg <- "'Latitude' column name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_ylat

    if (is.null(sel_map_col_sampid) | sel_map_col_sampid == "") {
      # end process with pop up
      msg <- "'SampleID' column name is missing!"
      shinyalert::shinyalert(title = "Update Map"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_sampid

    ### Munge Data ----
    #### Munge, Val, Nar, Size
    if (sel_map_datatype == "BCG_bugs") {
      sel_map_col_mapval <- "BCG_Status"
      sel_map_col_mapnar <- "BCG_Status2"
    } else if (sel_map_datatype == "BCG_fish") {
      sel_map_col_mapval <- "BCG_Status"
      sel_map_col_mapnar <- "BCG_Status2"
    } else if (sel_map_datatype == "IBI_bugs") {
      sel_map_col_mapval <- "Index"
      sel_map_col_mapnar <- "Index_Nar"
    } else if (sel_map_datatype == "IBI_fish") {
      sel_map_col_mapval <- "Index"
      sel_map_col_mapnar <- "Index_Nar"
    }## IF ~ sel_datatype ~ END


    # QC, Value in data frame
    boo_map_col_mapval <- sel_map_col_mapval %in% names_data
    if (boo_map_col_mapval == FALSE) {
      # end process with pop up
      msg <- paste0("Map Value column name ("
                    , sel_map_col_mapval
                    , ") is missing!")
      shinyalert::shinyalert(title = "Update Data"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)
    }## IF ~ sel_map_col_sampid



    # Rename Columns to known values
    ## Add Jitter to Lat-Long to avoid overlap
    # 1 second ~ 1/3600 ~ 0.000278 ~ 37.5 meters
    # 7 seconds ~ 262.3 meters
    jit_fac <- 0/3600
    nrow_data <- nrow(df_map)
    noise_y <- runif(nrow_data, -jit_fac, jit_fac)
    noise_x <- runif(nrow_data, -jit_fac, jit_fac)
    # no jitter so points are exact
    # leave in so can use at a later date if needed

    df_map <- df_map %>%
      mutate(map_ID = df_map[, sel_map_col_sampid]
             # , map_ylat = jitter(df_map[, sel_map_col_ylat], jit_fac)
             # , map_xlong = jitter(df_map[, sel_map_col_xlong], jit_fac)
             , map_ylat = df_map[, sel_map_col_ylat] + noise_y
             , map_xlong = df_map[, sel_map_col_xlong] + noise_x
             , map_mapval = df_map[, sel_map_col_mapval]
             , map_mapnar = df_map[, sel_map_col_mapnar]
             , map_color = NA_character_
             , map_size = NA_real_
             , map_popup = paste0(as.character("<b>"), "SampleID: ", as.character("</b>"), df_map[, sel_map_col_sampid], as.character("<br>")
                                  , as.character("<b>"), "Latitude: ", as.character("</b>"), df_map[, sel_map_col_ylat], as.character("<br>")
                                  , as.character("<b>"), "Longitude: ", as.character("</b>"), df_map[, sel_map_col_xlong], as.character("<br>")
                                  , as.character("<b>"), "Data Type: ", as.character("</b>"), sel_map_datatype, as.character("<br>")
                                  , as.character("<b>"), "Value: ", as.character("</b>"), df_map[, sel_map_col_mapval], as.character("<br>")
                                  , as.character("<b>"), "Narrative: ", as.character("</b>"), df_map[, sel_map_col_mapnar], as.character("<br>")
             )
      )

    ### Munge, Color, Size, Legend
    # by index value or narrative
    if (sel_map_datatype == "BCG_bugs") {
      #### BCG_bugs ----
      leg_title <- "BCG, Bugs"
      # cut_brk <- seq(0.5, 6.5, 1)
      # cut_lab <- c("blue", "green", "lightgreen", "gray", "orange", "red")
      # leg_col <- cut_lab
      # leg_nar <- paste0("L", 1:6)
      # df_map[, "map_color"] <- cut(df_map[, "map_mapval"]
      #                              , breaks = cut_brk
      #                              , labels = cut_lab
      #                              , include.lowest = TRUE
      #                              , right = FALSE
      #                              , ordered_result = TRUE)
      leg_col <- c(col_bcg_1,
                   col_bcg_2,
                   col_bcg_2.5,
                   col_bcg_3,
                   col_bcg_3.5,
                   col_bcg_4,
                   col_bcg_4.5,
                   col_bcg_5,
                   col_bcg_5.5,
                   col_bcg_6,
                   col_bcg_NA)
      leg_nar <- c("1"
                   , "2"
                   , "2.5"
                   , "3"
                   , "3.5"
                   , "4"
                   , "4.5"
                   , "5"
                   , "5.5"
                   , "6"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapval == leg_nar[1] ~ leg_col[1]
                                     , map_mapval == leg_nar[2] ~ leg_col[2]
                                     , map_mapval == leg_nar[3] ~ leg_col[3]
                                     , map_mapval == leg_nar[4] ~ leg_col[4]
                                     , map_mapval == leg_nar[5] ~ leg_col[5]
                                     , map_mapval == leg_nar[6] ~ leg_col[6]
                                     , map_mapval == leg_nar[7] ~ leg_col[7]
                                     , map_mapval == leg_nar[8] ~ leg_col[8]
                                     , map_mapval == leg_nar[9] ~ leg_col[9]
                                     , map_mapval == leg_nar[10] ~ leg_col[10]
                                     , TRUE ~ leg_col[11]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- size_default
    } else if (sel_map_datatype == "BCG_fish") {
      #### BCG_fish ----
      leg_title <- "BCG, Fish"
      # cut_brk <- seq(0.5, 6.5, 1)
      # cut_lab <- c("blue", "green", "lightgreen", "gray", "orange", "red")
      # leg_col <- cut_lab
      # leg_nar <- paste0("L", 1:6)
      # df_map[, "map_color"] <- cut(df_map[, "map_mapval"]
      #                              , breaks = cut_brk
      #                              , labels = cut_lab
      #                              , include.lowest = TRUE
      #                              , right = FALSE
      #                              , ordered_result = TRUE)
      leg_col <- c(col_bcg_1,
                   col_bcg_2,
                   col_bcg_2.5,
                   col_bcg_3,
                   col_bcg_3.5,
                   col_bcg_4,
                   col_bcg_4.5,
                   col_bcg_5,
                   col_bcg_5.5,
                   col_bcg_6,
                   col_bcg_NA)
      leg_nar <- c("1"
                   , "2"
                   , "2.5"
                   , "3"
                   , "3.5"
                   , "4"
                   , "4.5"
                   , "5"
                   , "5.5"
                   , "6"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapval == leg_nar[1] ~ leg_col[1]
                                     , map_mapval == leg_nar[2] ~ leg_col[2]
                                     , map_mapval == leg_nar[3] ~ leg_col[3]
                                     , map_mapval == leg_nar[4] ~ leg_col[4]
                                     , map_mapval == leg_nar[5] ~ leg_col[5]
                                     , map_mapval == leg_nar[6] ~ leg_col[6]
                                     , map_mapval == leg_nar[7] ~ leg_col[7]
                                     , map_mapval == leg_nar[8] ~ leg_col[8]
                                     , map_mapval == leg_nar[9] ~ leg_col[9]
                                     , map_mapval == leg_nar[10] ~ leg_col[10]
                                     , TRUE ~ leg_col[11]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- size_default
    } else if (sel_map_datatype == "IBI_bugs") {
      #### IBI_bugs ----
      leg_title <- "IBI, Bugs"
      # cut_brk <- seq(0.5, 6.5, 1)
      # cut_lab <- c("blue", "green", "lightgreen", "gray", "orange", "red")
      # leg_col <- cut_lab
      # leg_nar <- paste0("L", 1:6)
      # df_map[, "map_color"] <- cut(df_map[, "map_mapval"]
      #                              , breaks = cut_brk
      #                              , labels = cut_lab
      #                              , include.lowest = TRUE
      #                              , right = FALSE
      #                              , ordered_result = TRUE)
      leg_col <- c(col_ibi_excep,
                   col_ibi_ucl,
                   col_ibi_gen,
                   col_ibi_lcl,
                   col_ibi_bgen,
                   col_ibi_NA)
      leg_nar <- c("Exceptional"
                   , "Upper CL"
                   , "General"
                   , "Lower CL"
                   , "Below General"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , TRUE ~ leg_col[6]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- size_default
    } else if (sel_map_datatype == "IBI_fish") {
      #### IBI_bugs ----
      leg_title <- "IBI, Fish"
      # cut_brk <- seq(0.5, 6.5, 1)
      # cut_lab <- c("blue", "green", "lightgreen", "gray", "orange", "red")
      # leg_col <- cut_lab
      # leg_nar <- paste0("L", 1:6)
      # df_map[, "map_color"] <- cut(df_map[, "map_mapval"]
      #                              , breaks = cut_brk
      #                              , labels = cut_lab
      #                              , include.lowest = TRUE
      #                              , right = FALSE
      #                              , ordered_result = TRUE)
      leg_col <- c(col_ibi_excep,
                   col_ibi_ucl,
                   col_ibi_gen,
                   col_ibi_lcl,
                   col_ibi_bgen,
                   col_ibi_NA)
      leg_nar <- c("Exceptional"
                   , "Upper CL"
                   , "General"
                   , "Lower CL"
                   , "Below General"
                   , "NA"
      )
      df_map <- df_map %>%
        mutate(map_color = case_when(map_mapnar == leg_nar[1] ~ leg_col[1]
                                     , map_mapnar == leg_nar[2] ~ leg_col[2]
                                     , map_mapnar == leg_nar[3] ~ leg_col[3]
                                     , map_mapnar == leg_nar[4] ~ leg_col[4]
                                     , map_mapnar == leg_nar[5] ~ leg_col[5]
                                     , TRUE ~ leg_col[6]
        ))
      # TRUE is ELSE and #808080 is gray
      df_map[, "map_size"] <- size_default
    } else {
      leg_title <- NA
      df_map[, "map_color"] <- "gray"
      df_map[, "map_size"] <- size_default
      leg_col <- "gray"
      leg_nar <- no_narrative
    }## IF ~ sel_datatype ~ COLOR



    ### Map ----
    # Bounding box
    # zoom to data
    map_bbox <- c(min(df_map[, sel_map_col_xlong], na.rm = TRUE)
                  , min(df_map[, sel_map_col_ylat], na.rm = TRUE)
                  , max(df_map[, sel_map_col_xlong], na.rm = TRUE)
                  , max(df_map[, sel_map_col_ylat], na.rm = TRUE)
    )

    #~~~~~~~~~~~~~~~~~~~~~~
    # repeat code from base
    #~~~~~~~~~~~~~~~~~~~~~~
    # zoom levels, https://leafletjs.com/examples/zoom-levels/

    #leaflet() %>%
    leafletProxy("map_leaflet", data = df_map) %>%
      # Groups, Base
      # addProviderTiles("CartoDB.Positron"
      #                  , group = "Positron") %>%
      # addProviderTiles(providers$Stamen.TonerLite
      #                  , group = "Toner Lite") %>%
      # addProviderTiles(providers$OpenStreetMap
      #                  , group = "Open Street Map") %>%
      clearControls() %>%
      clearShapes() %>%
      clearMarkers() %>%
      # Groups, Overlay
      # addCircles(lng = ~map_xlong
      #            , lat = ~map_ylat
      #            , color = ~map_color
      #            , popup = ~map_popup
      #            , radius = ~map_size
      #            , group = "Samples") %>%
      addCircleMarkers(lng = ~map_xlong
                       , lat = ~map_ylat
                       , color = ~map_color
                       , popup = ~map_popup
                       #, radius = ~map_size
                       , fill = ~map_color
                       , stroke = TRUE
                       , fillOpacity = 0.75
                       , group = "Samples"
                       , clusterOptions = markerClusterOptions(
                                              spiderfyDistanceMultiplier = 1.5
                                              , showCoverageOnHover = TRUE
                                              , freezeAtZoom = 13)
      ) %>%
      # Test different points
      # addAwesomeMarkers(lng = ~map_xlong
      #                   , lat = ~map_ylat
      #                   , popup = ~map_popup
      #                   , clusterOptions = markerClusterOptions()) %>%
      # Legend
      addLegend("bottomleft"
                , colors = leg_col
                , labels = leg_nar
                , values = NA
                , title = leg_title) %>%
      # Layers, Control
      addLayersControl(baseGroups = c("Positron"
                                      , "Open Street Map"
                                      , "ESRI World Imagery")
                       , overlayGroups = c("Samples"
                                           , "Ecoregions, Level III"
                                           #, "BCG Class"
                                           # , "NorWeST"
                                           # , "NHD+ Catchments"
                                           # , "NHD+ Flowlines"
                       )
      ) %>%
      # Layers, Hide
      hideGroup(c("Ecoregions, Level III"
                  # , "BCG Class"
                  # , "NorWeST"
                  # , "NHD+ Catchments"
                  # , "NHD+ Flowlines"
      )) %>%
      # Bounds
      fitBounds(map_bbox[1], map_bbox[2], map_bbox[3], map_bbox[4])


  })## MAP, Leaflet, PROXY

  #~~~~REPORTS~~~~----

  # Rep, IMPORT ----
  file_watch_rep_multi <- reactive({
    # trigger for import_rep_multi()
    input$fn_input_rep_multi
    # NOT WORKING
  })## file_watch

  ## Rep, IMPORT, rep_multi----
  import_rep_multi <- eventReactive(file_watch_rep_multi(), {
    # use a multi-item reactive so keep on a single line (if needed later)

    # NOT WORKING
    #
    # # Define file
    # fn_inFile <- inFile$datapath
    #
    # #message(getwd())
    # message(paste0("Import, file name: ", inFile$name))
    #
    # # Remove existing files in "results"
    # clean_results()
    #
    # # Copy user files to results sub-folder
    # copy_import_file(import_file = inFile)
    #
    # # result folder and files
    # fn_abr <- abr_report
    # fn_abr_save <- paste0("_", fn_abr, "_")
    # path_results_sub <- file.path(path_results
    #                               , paste(abr_results, fn_abr, sep = "_"))
    # # Add "Results" folder if missing
    # boo_Results <- dir.exists(file.path(path_results_sub))
    # if (boo_Results == FALSE) {
    #   dir.create(file.path(path_results_sub))
    # }
    #
    # # button, disable, download
    # shinyjs::disable("b_download_rep_multi")
    #
    # # unzip
    # zip::unzip(file.path(path_results_sub, inFile$name)
    #            , overwrite = TRUE
    #            , exdir = path_results)
    #


    ### button, enable, calc ----
    shinyjs::enable("b_calc_rep_multi")

  })##output$df_import_rep_multi ~ END


  # Report, Bio  ----
  observeEvent(input$b_calc_rep_bio, {
    shiny::withProgress({

      ### Calc, 00, Set Up Shiny Code ----

      prog_detail <- "Calculation, Report, Biological..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 5
      prog_sleep <- 0.25

      ## Calc, 01, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Define file
      inFile <- input$fn_input_rep_bio
      fn_inFile <- inFile$datapath
      #message(getwd())
      message(paste0("Import, file name: ", inFile$name))

      if (is.null(inFile)) {
        # end process with pop up
        msg <- paste("No file uploaded.  Upload a file and try again."
                     , "OR file did not finish loading.\nWait for 'Upload Complete' message before clicking."
                     , sep = "\n\n")
        shinyalert::shinyalert(title = "Report"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        shiny::validate(msg)
      }## IF ~ is.null(inFile)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = inFile)

      # result folder and files
      fn_abr <- abr_report
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # button, disable, download
      shinyjs::disable("b_download_rep_bio")

      # User Input folder
      path_results_user <- file.path(path_results, dn_files_input)

      # unzip
      zip::unzip(file.path(path_results_user, inFile$name)
                 , overwrite = TRUE
                 , exdir = path_results_user)

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))

      # Template file
      fn_template <- list.files(path_results_user
                                , pattern = "^_Template_Report_Bio.*\\.xlsx$"
                                , recursive = TRUE)

      if (length(fn_template) == 0) {
        # end process with pop up
        msg <- "'_Template_Report_Bio*.xlsx' file is missing!"
        shinyalert::shinyalert(title = "Report"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        # shiny::validate(msg)
      }## IF ~ length(fn_template) == 0

      if (length(fn_template) > 1) {
        # end process with pop up
        msg <- "'_Template_Report_Bio*.xlsx' found more than once!"
        shinyalert::shinyalert(title = "Report"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        # shiny::validate(msg)
      }## IF ~ length(fn_template) > 1

      message(paste0("Template, file name: ", fn_template))

      # Files, ALL
      fn_all <- list.files(path = path_results_user
                           , full.names = TRUE
                           , recursive = TRUE)
      len_fn_all <- length(fn_all)
      # Files, DataFrame
      df_fn_all <- data.frame("path" = fn_all
                              , "file" = basename(fn_all)
                              , "dir_full" = dirname(fn_all))
      df_fn_all[, "dir_zip"] <- sub(paste0("^", path_results_user, "/")
                                    , ""
                                    , df_fn_all[, "dir_full"])

      # Read Template
      # read template file
      path_template <- file.path(path_results_user, fn_template)
      n_skip <- 3

      # **Template Worksheets**
      # other
      # Bug_IBI_BCG
      # Fish_IBI_BCG
      # BugData
      # FishData
      # Bug_IBI_Metrics_NFRR3
      # Bug_IBI_Metrics_NFGP4
      # Fish_Metrics_NFStream5
      # Fish_Metrics_NFHead6

      ### Template, Other----
      sh_template <- "Other"
      df_template_other <- readxl::read_excel(path = path_template
                                                  , sheet = sh_template
                                                  , skip = n_skip)
      df_template_other[, "sheet"] <- sh_template

      ### Template, Bug_IBI_BCG----
      sh_template <- "Bug_IBI_BCG"
      df_template_b_ibi_bcg <- readxl::read_excel(path = path_template
                                              , sheet = sh_template
                                              , skip = n_skip)
      df_template_b_ibi_bcg[, "sheet"] <- sh_template

      ### Template, Fish_IBI_BCG ----
      sh_template <- "Fish_IBI_BCG"
      df_template_f_ibi_bcg <- readxl::read_excel(path = path_template
                                                       , sheet = sh_template
                                                       , skip = n_skip)
      df_template_f_ibi_bcg[, "sheet"] <- sh_template

      ### Template, BugData ----
      sh_template <- "BugData"
      df_template_b_samps <- readxl::read_excel(path = path_template
                                                     , sheet = sh_template
                                                     , skip = n_skip)
      df_template_b_samps[, "sheet"] <- sh_template

      ### Template, FishData ----
      sh_template <- "FishData"
      df_template_f_samps <- readxl::read_excel(path = path_template
                                                     , sheet = sh_template
                                                     , skip = n_skip)
      df_template_f_samps[, "sheet"] <- sh_template

      ### Template, Bug_IBI_Metrics_NFRR3 ----
      sh_template <- "Bug_IBI_Metrics_NFRR3"
      df_template_b_ibi_met_nfrr3 <- readxl::read_excel(path = path_template
                                                , sheet = sh_template
                                                , skip = n_skip)
      df_template_b_ibi_met_nfrr3[, "sheet"] <- sh_template

      ### Template, Bug_IBI_Metrics_NFGP4 ----
      sh_template <- "Bug_IBI_Metrics_NFGP4"
      df_template_b_ibi_met_nfgp4 <- readxl::read_excel(path = path_template
                                              , sheet = sh_template
                                              , skip = n_skip)
      df_template_b_ibi_met_nfgp4[, "sheet"] <- sh_template

      ### Template, Fish_Metrics_NFStream5 ----
      sh_template <- "Fish_Metrics_NFStream5"
      df_template_f_met_nfstream5 <- readxl::read_excel(path = path_template
                                             , sheet = sh_template
                                             , skip = n_skip)
      df_template_f_met_nfstream5[, "sheet"] <- sh_template

      ### Template, Fish_Metrics_NFHead6 ----
      sh_template <- "Fish_Metrics_NFHead6"
      df_template_f_met_nfhead6 <- readxl::read_excel(path = path_template
                                                  , sheet = sh_template
                                                  , skip = n_skip)
      df_template_f_met_nfhead6[, "sheet"] <- sh_template

      ### Template, file names ----
      df_template_all <- dplyr::bind_rows(df_template_other
                                          , df_template_b_ibi_bcg
                                          , df_template_f_ibi_bcg
                                          , df_template_b_samps
                                          , df_template_f_samps
                                          , df_template_b_ibi_met_nfrr3
                                          , df_template_b_ibi_met_nfgp4
                                          , df_template_f_met_nfstream5
                                          , df_template_f_met_nfhead6
                                          , .id = "id")
      df_template_sourcefiles <- unique(df_template_all[, c("inclusion"
                                                            , "source folder"
                                                            , "source file (or suffix)")
                                                        , TRUE])
      df_template_sourcefiles[, c("exact", "csv", "present")] <- NA_integer_

      ### QC, File Names----
      # check for each as CSV and Exact

      for (i in seq_len(nrow(df_template_sourcefiles))) {

        df_template_sourcefiles[i, "exact"] <- sum(grepl(pattern = df_template_sourcefiles[i, "source file (or suffix)"], fn_all))

        df_template_sourcefiles[i, "csv"] <- sum(grepl(pattern = paste0(df_template_sourcefiles[i, "source file (or suffix)"], "\\.csv$"), fn_all))

      }## FOR ~ i

      df_template_sourcefiles[, "present"] <- df_template_sourcefiles[, "exact"] +
        df_template_sourcefiles[, "csv"]

      sourcefiles_missing <- dplyr::filter(df_template_sourcefiles
                                           , inclusion == "required"
                                           & (present == 0 | is.na(present)))

      if (nrow(sourcefiles_missing) > 0) {
        # end process with pop up
        msg <- paste0("REQUIRED Template Source Files missing!\n"
                      , paste(unique(sourcefiles_missing$`source file (or suffix)`)
                              , collapse = "\n" )
        )
        shinyalert::shinyalert(title = "Report"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        # shiny::validate(msg)
      }## IF ~ nrow(sourcefiles_missing) > 0

      ### File Names, Add Path
      for (i in seq_len(nrow(df_template_sourcefiles))) {

        df_template_sourcefiles[i, "path"] <- ifelse(df_template_sourcefiles[i, "source folder", TRUE] == "NA"
                                                     , df_template_sourcefiles[, "source file (or suffix)", TRUE]
                                                     , file.path(df_template_sourcefiles[i, "source folder", TRUE]
                                                                 , df_template_sourcefiles[i, "source file (or suffix)", TRUE])
        )
      }## FOR ~ i

      ### File Names, Add to col names
      # join or merge

      # Check file.exists for each entry.
      df_template_sourcefiles[, "exist_file"] <- NA

      # Fail if files don't exist.
      # if (length(fn_template) == 0) {
      #   # end process with pop up
      #   msg <- "'Template_TemperatureReport' file is missing!"
      #   shinyalert::shinyalert(title = "Report"
      #                          , text = msg
      #                          , type = "error"
      #                          , closeOnEsc = TRUE
      #                          , closeOnClickOutside = TRUE)
      #   shiny::validate(msg)
      # }## IF ~ length(fn_template) == 0

      # import each file
      # Check for column in file
      df_template_sourcefiles[, "exist_col"] <- NA

      ### Primary Keys ----
      pk_stations <- df_template_other[df_template_other[, "file"] == "Stations"
                                       , "primarykey"
                                       , TRUE]
      # pk_samples <- df_template_other[df_template_other[, "file"] == "Sample"
      #                                 , "primarykey"
      #                                 , TRUE]
      pk_bugs <- df_template_other[df_template_other[, "file"] == "Bug Data"
                                       , "primarykey"
                                       , TRUE]
      pk_fish <- df_template_other[df_template_other[, "file"] == "Fish Data"
                                   , "primarykey"
                                   , TRUE]


      ## Calc, 03, Data ----
      prog_detail <- "Calculation, Create Data Tables"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)
      ### Assemble data for each tab of the report

      # read template file
      # read all files
      # merge or join for each worksheet

      ### Data, NOTES ----
      notes_head <- as.data.frame(cbind(c("Project Name"
                                          , "Specific Task"
                                          , NA
                                          , "author@email.com"
                                          , as.character(Sys.Date())
                                          , NA
                                          , "Path & FileName"
                                          , "FileName"
                                          , "SheetName"
                                          , NA
                                          , "Description of Work"
                                          , ""
      )
      , c(rep(NA, 6)
          , '=LEFT(@CELL("filename",B7),FIND("]",@CELL("filename",B7)))'
          , '=MID(@CELL("filename",B8),FIND("[",@CELL("filename",B8)),(FIND("]",@CELL("filename",B8))-FIND("[",@CELL("filename",B8)))+1)'
          , '=MID(@CELL("filename",B9),FIND("]",@CELL("filename",B9))+1,LEN(@CELL("filename",B9))-FIND("]",@CELL("filename",B9)))'
          , rep(NA, 3))))
      #, c(rep(NA, 6), rep("formula", 3), rep(NA, 3))))
      class(notes_head[, 2]) <- "formula"
      notes_toc <- as.data.frame(rbind(
        c("NOTES"
          , "Description of work and other worksheets"
          , '=HYPERLINK(FileName&"NOTES"&"!A1","NOTES")')
        , c("Bug_IBI_BCG"
            , "Bug_IBI_BCG"
            , '=HYPERLINK(FileName&"Bug_IBI_BCG"&"!A1","Bug_IBI_BCG")')
        , c("Fish_IBI_BCG"
            , "Fish_IBI_BCG"
            , '=HYPERLINK(FileName&"Fish_IBI_BCG"&"!A1","Fish_IBI_BCG")')
        , c("BugData"
            , "BugData"
            , '=HYPERLINK(FileName&"BugData"&"!A1","BugData")')
        , c("FishData"
            , "FishData"
            , '=HYPERLINK(FileName&"FishData"&"!A1","FishData")')
        , c("Bug_IBI_BCG_Metrics_NFRR3"
            , "Bug_IBI_BCG_Metrics_NFRR3"
            , '=HYPERLINK(FileName&"Bug_IBI_BCG_Metrics_NFRR3"&"!A1","Bug_IBI_BCG_Metrics_NFRR3")')
        , c("Bug_IBI_BCG_Metrics_NFGP4"
            , "Bug_IBI_BCG_Metrics_NFGP4"
            , '=HYPERLINK(FileName&"Bug_IBI_BCG_Metrics_NFGP4"&"!A1","Bug_IBI_BCG_Metrics_NFGP4")')
        , c("Fish_Metrics_NFStream5"
            , "Fish_Metrics_NFStream5"
            , '=HYPERLINK(FileName&"Fish_Metrics_NFStream5"&"!A1","Fish_Metrics_NFStream5")')
        , c("Fish_Metrics_NFHead6"
            , "Fish_Metrics_NFHead6"
            , '=HYPERLINK(FileName&"Fish_Metrics_NFHead6"&"!A1","Fish_Metrics_NFHead6")')
      ))
      names(notes_toc) <- c("Worksheet", "Description", "Link")
      class(notes_toc$Link) <- "formula"

      # ### Data, Summary, Color Thresholds ----
      # df_col_thresh <- read.csv(file.path("data"
      #                                     , "report_color_thresholds.csv"))
      #
      # # compile each in a helper script
      #
      # # # Get stations
      # # pk_stations <- df_template_other[df_template_other[, "file"] == "Stations"
      # #                                  , "primarykey"
      # #                                  , TRUE]
      # # pk_samples <- df_template_other[df_template_other[, "file"] == "Sample"
      # #                                  , "primarykey"
      # #                                  , TRUE]
      #
      # # zip file extracted to "results/_user_input"
      #
      # ### Data, Summary, Header ----
      # ls_report_summary_header <- build_report_table(df_template_summary_header
      #                                                , fld_name_orig = "original name"
      #                                                , fld_name_disp = "display name"
      #                                                , fld_desc = "descriptor"
      #                                                , fld_incl = "inclusion"
      #                                                , fld_folder = "source folder"
      #                                                , fld_file = "source file (or suffix)"
      #                                                , fld_colr = "color code"
      #                                                , fld_sort = "sort"
      #                                                , path_files = file.path(path_results, "_user_input")
      #                                                , tbl_name = "summary_header")
      #
      # df_report_summary_header <- ls_report_summary_header$data
      # # df_report_summary_header <- mtcars
      #
      # ### Data, Summary, Wide ----
      # ls_report_summary_wide <- build_report_table(df_template_summary_wide
      #                                              , fld_name_orig = "original name"
      #                                              , fld_name_disp = "display name"
      #                                              , fld_desc = "descriptor"
      #                                              , fld_incl = "inclusion"
      #                                              , fld_folder = "source folder"
      #                                              , fld_file = "source file (or suffix)"
      #                                              , fld_colr = "color code"
      #                                              , fld_sort = "sort"
      #                                              , path_files = file.path(path_results, "_user_input")
      #                                              , tbl_name = "summary_wide")
      #
      # df_report_summary_wide <- ls_report_summary_wide$data
      # # df_report_summary_wide <- mtcars

      ### Data, Bug IBI BCG ----

      # ls_report_b_ibi_bcg <- build_report_table(df_template_b_ibi_bcg
      #                                              , fld_name_orig = "original name"
      #                                              , fld_name_disp = "display name"
      #                                              , fld_desc = "descriptor"
      #                                              , fld_incl = "inclusion"
      #                                              , fld_folder = "source folder"
      #                                              , fld_file = "source file (or suffix)"
      #                                              , fld_colr = "color code"
      #                                              , fld_sort = "sort"
      #                                              , path_files = file.path(path_results, "_user_input")
      #                                              , tbl_name = "Bug_IBI_BCG")
      #
      # df_report_b_ibi_bcg <- ls_report_b_ibi_bcg$data
      df_report_b_ibi_bcg <- iris

      ### Data, Fish IBI BCG ----
      # ls_report_f_ibi_bcg <- build_report_table(df_template_f_ibi_bcg
      #                                         , fld_name_orig = "original name"
      #                                         , fld_name_disp = "display name"
      #                                         , fld_desc = "descriptor"
      #                                         , fld_incl = "inclusion"
      #                                         , fld_folder = "source folder"
      #                                         , fld_file = "source file (or suffix)"
      #                                         , fld_colr = "color code"
      #                                         , path_files = file.path(path_results, "_user_input")
      #                                         , tbl_name = "Fish_IBI_BCG")
      #
      # df_report_f_ibi_bcg <- ls_report_f_ibi_bcg$data
      df_report_f_ibi_bcg <- ToothGrowth

      ### Data, Bug Data ----
      # ls_report_b_samps <- build_report_table(df_template_b_samps
      #                                       , fld_name_orig = "original name"
      #                                       , fld_name_disp = "display name"
      #                                       , fld_desc = "descriptor"
      #                                       , fld_incl = "inclusion"
      #                                       , fld_folder = "source folder"
      #                                       , fld_file = "source file (or suffix)"
      #                                       , fld_colr = "color code"
      #                                       , path_files = file.path(path_results, "_user_input")
      #                                       , tbl_name = "BugData")
      #
      # df_report_b_samps <- ls_report_b_samps$data
       df_report_b_samps <- PlantGrowth

      ### Data, Fish Data ----
      # ls_report_f_samps <- build_report_table(df_template_f_samps
      #                                      , fld_name_orig = "original name"
      #                                      , fld_name_disp = "display name"
      #                                      , fld_desc = "descriptor"
      #                                      , fld_incl = "inclusion"
      #                                      , fld_folder = "source folder"
      #                                      , fld_file = "source file (or suffix)"
      #                                      , fld_colr = "color code"
      #                                      , fld_sort = "sort"
      #                                      , path_files = file.path(path_results, "_user_input")
      #                                      , tbl_name = "FishData")
      #
      # df_report_f_samps <- ls_report_f_samps$data
       df_report_f_samps <- USArrests

      ### Data, Bug IBI Metrics NFRR3----
      # ls_report_b_ibi_met_nfrr3 <- build_report_table(df_template_b_ibi_met_nfrr3
      #                                           , fld_name_orig = "original name"
      #                                           , fld_name_disp = "display name"
      #                                           , fld_desc = "descriptor"
      #                                           , fld_incl = "inclusion"
      #                                           , fld_folder = "source folder"
      #                                           , fld_file = "source file (or suffix)"
      #                                           , fld_colr = "color code"
      #                                           , fld_sort = "sort"
      #                                           , path_files = file.path(path_results, "_user_input")
      #                                           , tbl_name = "Bug_IBI_Metrics_NFRR3")
      # df_report_b_ibi_met_nfrr3 <- ls_report_b_ibi_met_nfrr3$data
      df_report_b_ibi_met_nfrr3 <- cars

      ### Data, Bug IBI Metrics NFGP4----
      # ls_report_b_ibi_met_nfgp4 <- build_report_table(df_template_b_ibi_met_nfgp4
      #                                           , fld_name_orig = "original name"
      #                                           , fld_name_disp = "display name"
      #                                           , fld_desc = "descriptor"
      #                                           , fld_incl = "inclusion"
      #                                           , fld_folder = "source folder"
      #                                           , fld_file = "source file (or suffix)"
      #                                           , fld_colr = "color code"
      #                                           , fld_sort = "sort"
      #                                           , path_files = file.path(path_results, "_user_input")
      #                                           , tbl_name = "Bug_IBI_Metrics_NFGP4")
      # df_report_b_ibi_met_nfgp4 <- ls_report_b_ibi_met_nfgp4$data
       df_report_b_ibi_met_nfgp4 <- cars

      ### Data, Fish Metrics NFStream5----
      # ls_report_fish_met_nfstream5 <- build_report_table(df_template_fish_met_nfstream5
      #                                           , fld_name_orig = "original name"
      #                                           , fld_name_disp = "display name"
      #                                           , fld_desc = "descriptor"
      #                                           , fld_incl = "inclusion"
      #                                           , fld_folder = "source folder"
      #                                           , fld_file = "source file (or suffix)"
      #                                           , fld_colr = "color code"
      #                                           , fld_sort = "sort"
      #                                           , path_files = file.path(path_results, "_user_input")
      #                                           , tbl_name = "Fish_Metrics_Stream5")
      # df_report_fish_met_nfstream5 <- ls_report_fish_met_nfstream5$data
       df_report_fish_met_nfstream5 <- cars

      ### Data, Fish Metrics NFHead6----
      # ls_report_fish_met_nfhead6 <- build_report_table(df_template_fish_met_nfhead6
      #                                           , fld_name_orig = "original name"
      #                                           , fld_name_disp = "display name"
      #                                           , fld_desc = "descriptor"
      #                                           , fld_incl = "inclusion"
      #                                           , fld_folder = "source folder"
      #                                           , fld_file = "source file (or suffix)"
      #                                           , fld_colr = "color code"
      #                                           , fld_sort = "sort"
      #                                           , path_files = file.path(path_results, "_user_input")
      #                                           , tbl_name = "Fish_Metrics_NFHead6")
      # df_report_fish_met_nfhead6 <- ls_report_fish_met_nfhead6$data
      df_report_fish_met_nfhead6 <- cars

      ### Data, SampID ----
      # Check and Fail if not present

      # # SampID_summary_wide
      # tbl_name <- "summary_wide"
      # df_check_sampid <- df_report_summary_wide
      # boo_sampid <- toupper(pk_samples) %in% toupper(names(df_check_sampid))
      # if (boo_sampid == FALSE) {
      #   # end process with pop up
      #   msg <- paste("REQUIRED column (SampleID) missing!"
      #                , paste0("Table: ", tbl_name)
      #                , paste0("Value: ", pk_samples)
      #                , sep = "\n\n" )
      #   shinyalert::shinyalert(title = "Report"
      #                          , text = msg
      #                          , type = "error"
      #                          , closeOnEsc = TRUE
      #                          , closeOnClickOutside = TRUE)
      #   # shiny::validate(msg)
      # }## IF ~ boo_sampID ~ summary
      # col_sampid_summary_wide <- names(df_check_sampid)[match(toupper(pk_samples), toupper(names(df_check_sampid)))]
#**QC**----
      # # SampID_topindicator
      # tbl_name <- "Bug_IBI_BCG"
      # df_check_sampid <- df_report_topindicator
      # boo_sampid <- toupper(pk_samples) %in% toupper(names(df_check_sampid))
      # if (boo_sampid == FALSE) {
      #   # end process with pop up
      #   msg <- paste("REQUIRED column (SampleID) missing!"
      #                , paste0("Table: ", tbl_name)
      #                , paste0("Value: ", pk_samples)
      #                , sep = "\n\n" )
      #   shinyalert::shinyalert(title = "Report"
      #                          , text = msg
      #                          , type = "error"
      #                          , closeOnEsc = TRUE
      #                          , closeOnClickOutside = TRUE)
      #   # shiny::validate(msg)
      # }## IF ~ boo_sampID ~ topindicator
      # col_sampid_topindicator <- names(df_check_sampid)[match(toupper(pk_samples), toupper(names(df_check_sampid)))]
      #
      # # SampID_samples
      # tbl_name <- "Fish_IBI_BCG"
      # df_check_sampid <- df_report_samples
      # boo_sampid <- toupper(pk_samples) %in% toupper(names(df_check_sampid))
      # if (boo_sampid == FALSE) {
      #   # end process with pop up
      #   msg <- paste("REQUIRED column (SampleID) missing!"
      #                , paste0("Table: ", tbl_name)
      #                , paste0("Value: ", pk_samples)
      #                , sep = "\n\n" )
      #   shinyalert::shinyalert(title = "Report"
      #                          , text = msg
      #                          , type = "error"
      #                          , closeOnEsc = TRUE
      #                          , closeOnClickOutside = TRUE)
      #   # shiny::validate(msg)
      # }## IF ~ boo_sampID ~ samples
      # col_sampid_samples <- names(df_check_sampid)[match(toupper(pk_samples), toupper(names(df_check_sampid)))]
      #
      # # SampID_flags
      # tbl_name <- "flags"
      # df_check_sampid <- df_report_flags
      # boo_sampid <- toupper(pk_samples) %in% toupper(names(df_check_sampid))
      # if (boo_sampid == FALSE) {
      #   # end process with pop up
      #   msg <- paste("REQUIRED column (SampleID) missing!"
      #                , paste0("Table: ", tbl_name)
      #                , paste0("Value: ", pk_samples)
      #                , sep = "\n\n" )
      #   shinyalert::shinyalert(title = "Report"
      #                          , text = msg
      #                          , type = "error"
      #                          , closeOnEsc = TRUE
      #                          , closeOnClickOutside = TRUE)
      #   # shiny::validate(msg)
      # }## IF ~ boo_sampID ~ flags
      # col_sampid_flags <- names(df_check_sampid)[match(toupper(pk_samples), toupper(names(df_check_sampid)))]

 #**QC*end*----
      # SampID_site
      # not needed for this table

      # SampID_taxatrans
      # not needed for this table

      # # StatID_summary_wide
      # tbl_name <- "summary_wide"
      # df_check_sampid <- df_report_summary_wide
      # boo_statid <- toupper(pk_stations) %in% toupper(names(df_check_sampid))
      # if (boo_sampid == FALSE) {
      #   # end process with pop up
      #   msg <- paste("REQUIRED column (StationID) missing!"
      #                , paste0("Table: ", tbl_name)
      #                , paste0("Value: ", pk_samples)
      #                , sep = "\n\n" )
      #   shinyalert::shinyalert(title = "Report"
      #                          , text = msg
      #                          , type = "error"
      #                          , closeOnEsc = TRUE
      #                          , closeOnClickOutside = TRUE)
      #   # shiny::validate(msg)
      # }## IF ~ boo_statid ~ summary
      # col_statid_summary_wide <- names(df_check_sampid)[match(toupper(pk_stations), toupper(names(df_check_sampid)))]

      ## Calc, 04, Excel ----
      prog_detail <- "Calculation, Create Excel"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      ### Excel, SUBSET ----
      # Filter for each Station

      # Remove all files except Results Excel
      # Then download button only has one file to target
      # reuse code from df_import()
      # fn_results <- list.files(path_results_user
      #                          , full.names = TRUE
      #                          , include.dirs = TRUE
      #                          , recursive = TRUE)
      unlink(path_results_user, recursive = TRUE) # includes directories


      # stations_all <- unique(df_report_site[, pk_stations])

      ## create file for each station
      #s <- stations_all[1] #QC

#       for (s in stations_all) {
#
#         s_num <- match(s, stations_all)
#         s_total <- length(stations_all)
#         msg <- paste0("Working on Report; Biological; "
#                       , s_num
#                       , " of "
#                       , s_total
#                       , "; "
#                       , s)
#         message(msg)
#
#         # Update progress
#         prog_detail <- paste0("Calculation, Create Excel; "
#                               , s_num
#                               , "/"
#                               , s_total)
#         message(paste0("\n", prog_detail))
#         # Increment the progress bar, and update the detail text.
#         incProgress(1/s_total/prog_n, detail = prog_detail)
#         Sys.sleep(prog_sleep)
#
#         #### Munge Tables---
#         # filter for current station
#         # use table specific SampleID
#
#         # df_report_summary_header_s <- df_report_summary_header %>%
#         #   dplyr::filter(.data[[pk_stations]] == s) %>%
#         #   tidyr::pivot_longer(tidyr::everything()
#         #                       , values_transform = as.character)
#         #
#         # df_report_summary_wide_s <- df_report_summary_wide %>%
#         #   dplyr::filter(.data[[col_statid_summary_wide]] == s)
#
#         # # Samples for current Stations
#         # s_samps <- df_report_summary_wide_s %>%
#         #   dplyr::pull(.data[[col_sampid_summary_wide]])
#
#         # df_report_topindicator_s <- df_report_topindicator  %>%
#         #   dplyr::filter(.data[[col_sampid_topindicator]] %in% s_samps)
#         #
#         # df_report_samples_s <- df_report_samples  %>%
#         #   dplyr::filter(.data[[col_sampid_samples]] %in% s_samps)
#         #
#         # df_report_flags_s <- df_report_flags  %>%
#         #   dplyr::filter(.data[[col_sampid_flags]] %in% s_samps)
#         #
#         # df_report_site_s <- df_report_site %>%
#         #   dplyr::filter(.data[[pk_stations]] == s) %>%
#         #   tidyr::pivot_longer(tidyr::everything()
#         #                       , values_transform = as.character)
#         #
#         # df_report_taxatrans_s <- df_report_taxatrans
#
#         # # transposed df remove names
#         # names(df_report_summary_header_s) <- c("", "")
#         # names(df_report_site_s) <- c("", "")
# #**WB create**----
        ### Excel, WB, Create----
        # Create WB
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "NOTES", tabColour = "darkgray")
        openxlsx::addWorksheet(wb, "Bug_IBI_BCG")
        openxlsx::addWorksheet(wb, "Fish_IBI_BCG")
        openxlsx::addWorksheet(wb, "BugData")
        openxlsx::addWorksheet(wb, "FishData")
        openxlsx::addWorksheet(wb, "Bug_IBI_Metrics_NFRR3")
        openxlsx::addWorksheet(wb, "Bug_IBI_Metrics_NFGP5")
        openxlsx::addWorksheet(wb, "Fish_Metrics_NFStream5")
        openxlsx::addWorksheet(wb, "Fish_Metrics_NFHead6")

        mySR <- 8 # number of rows to skip for new worksheets
#         mySR_trans <- 2 # for transposed df, skip worksheet title
#
        ### Excel, Formatting ----
        #### Excel, Formatting, Styles ----
        style_title <- openxlsx::createStyle(fontName = "Cambria"
                                             , fontSize = 18
                                             , fontColour = "#1F497D"
                                             , textDecoration = "bold")
        style_h1 <- openxlsx::createStyle(fontName = "Calibri"
                                          , fontSize = 15
                                          , fontColour = "#1F497D"
                                          , textDecoration = "bold"
                                          , border = "Bottom"
                                          , borderColour = "#4F81BD"
                                          , borderStyle = "thick")
        style_h2 <- openxlsx::createStyle(fontName = "Calibri"
                                          , fontSize = 13
                                          , fontColour = "#1F497D"
                                          , textDecoration = "bold"
                                          , border = "Bottom"
                                          , borderColour = "#A7BFDE"
                                          , borderStyle = "thick")
        style_hyperlink <- openxlsx::createStyle(fontName = "Calibri"
                                                 , fontSize = 11
                                                 , fontColour = "#0000FF"
                                                 , textDecoration = "underline")
        style_bold <- openxlsx::createStyle(textDecoration = "bold")
        style_date <- openxlsx::createStyle(numFmt = "DATE")
        style_halign_center <- openxlsx::createStyle(halign = "center")
#
#         # options not exportable
#         # openxlsx::options("openxlsx.dateFormat" = "yyyy-mm-dd")
#         # openxlsx::options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
#
#         #### Excel, Formatting, CF, Styles ----
#         # fgFill *only* works for Conditinal Formatting
#         # no harm in adding bgFill for use with on CF
#         style_cf_ft_vcold          <- openxlsx::createStyle(bgFill = "#140AE6"
#                                                             , fgFill = "#140AE6")
#         style_cf_ft_vcold_cold     <- openxlsx::createStyle(bgFill = "#0066FF"
#                                                             , fgFill = "#0066FF")
#         style_cf_ft_tie_vcold_cold <- openxlsx::createStyle(bgFill = "#7B9BF5"
#                                                             , fgFill = "#7B9BF5")
#         style_cf_ft_cold_vcold     <- openxlsx::createStyle(bgFill = "#0AE1EC"
#                                                             , fgFill = "#0AE1EC")
#         style_cf_ft_cold           <- openxlsx::createStyle(bgFill = "#9AF3FC"
#                                                             , fgFill = "#9AF3FC")
#         style_cf_ft_cold_cool      <- openxlsx::createStyle(bgFill = "#BEFEFB"
#                                                             , fgFill = "#BEFEFB")
#         style_cf_ft_tie_cold_cool  <- openxlsx::createStyle(bgFill = "#DDFBFF"
#                                                             , fgFill = "#DDFBFF")
#         style_cf_ft_cool_cold      <- openxlsx::createStyle(bgFill = "#C6FFB9"
#                                                             , fgFill = "#C6FFB9")
#         style_cf_ft_cool           <- openxlsx::createStyle(bgFill = "#34FB25"
#                                                             , fgFill = "#34FB25")
#         style_cf_ft_cool_warm      <- openxlsx::createStyle(bgFill = "#FFFF66"
#                                                             , fgFill = "#FFFF66")
#         style_cf_ft_tie_warm_cool  <- openxlsx::createStyle(bgFill = "#FFFFE5"
#                                                             , fgFill = "#FFFFE5")
#         style_cf_ft_warm_cool      <- openxlsx::createStyle(bgFill = "#E4DFEC"
#                                                             , fgFill = "#E4DFEC")
#         style_cf_ft_warm           <- openxlsx::createStyle(bgFill = "#FFC000"
#                                                             , fgFill = "#FFC000")
#         style_cf_ft_na             <- openxlsx::createStyle(bgFill = "#808080"
#                                                             , fgFill = "#808080")
#         style_cf_bcg_1             <- openxlsx::createStyle(bgFill = "blue"
#                                                             , fgFill = "blue")
#         style_cf_bcg_2             <- openxlsx::createStyle(bgFill = "green"
#                                                             , fgFill = "green")
#         style_cf_bcg_3             <- openxlsx::createStyle(bgFill = "lightgreen"
#                                                             , fgFill = "lightgreen")
#         style_cf_bcg_4             <- openxlsx::createStyle(bgFill = "gray"
#                                                             , fgFill = "gray")
#         style_cf_bcg_5             <- openxlsx::createStyle(bgFill = "orange"
#                                                             , fgFill = "orange")
#         style_cf_bcg_6             <- openxlsx::createStyle(bgFill = "red"
#                                                             , fgFill = "red")
#         style_cf_bcg_na            <- openxlsx::createStyle(bgFill = "#808080"
#                                                             , fgFill = "#808080")
#         style_cf_bdi_high          <- openxlsx::createStyle(bgFill = "blue"
#                                                             , fgFill = "blue")
#         style_cf_bdi_medium        <- openxlsx::createStyle(bgFill = "lightgreen"
#                                                             , fgFill = "lightgreen")
#         style_cf_bdi_low           <- openxlsx::createStyle(bgFill = "gray"
#                                                             , fgFill = "gray")
#         style_cf_bdi_na            <- openxlsx::createStyle(bgFill = "#808080"
#                                                             , fgFill = "#808080")
#         style_cf_mtti_vcold        <- openxlsx::createStyle(bgFill = "#00B0F0"
#                                                             , fgFill = "#00B0F0")
#         style_cf_mtti_cold         <- openxlsx::createStyle(bgFill = "#9AF3FC"
#                                                             , fgFill = "#9AF3FC")
#         style_cf_mtti_cool         <- openxlsx::createStyle(bgFill = "#92D050"
#                                                             , fgFill = "#92D050")
#         style_cf_mtti_cool_warm    <- openxlsx::createStyle(bgFill = "#FFFF00"
#                                                             , fgFill = "#FFFF00")
#         style_cf_mtti_warm         <- openxlsx::createStyle(bgFill = "#FFC000"
#                                                             , fgFill = "#FFC000")
#         style_cf_mtti_na           <- openxlsx::createStyle(bgFill = "#808080"
#                                                             , fgFill = "#808080")
#         style_cf_bcg2_1            <- openxlsx::createStyle(bgFill = "blue"
#                                                             , fgFill = "blue")
#         style_cf_bcg2_2            <- openxlsx::createStyle(bgFill = "green"
#                                                             , fgFill = "green")
#         style_cf_bcg2_2minus       <- openxlsx::createStyle(bgFill = "green"
#                                                             , fgFill = "green")
#         style_cf_bcg2_tie_2_3      <- openxlsx::createStyle(bgFill = "darkgreen"
#                                                             , fgFill = "darkgreen")
#         style_cf_bcg2_3plus        <- openxlsx::createStyle(bgFill = "lightgreen"
#                                                             , fgFill = "lightgreen")
#         style_cf_bcg2_3            <- openxlsx::createStyle(bgFill = "lightgreen"
#                                                             , fgFill = "lightgreen")
#         style_cf_bcg2_3minus       <- openxlsx::createStyle(bgFill = "lightgreen"
#                                                             , fgFill = "lightgreen")
#         style_cf_bcg2_tie_3_4      <- openxlsx::createStyle(bgFill = "yellow"
#                                                             , fgFill = "yellow")
#         style_cf_bcg2_4plus        <- openxlsx::createStyle(bgFill = "gray"
#                                                             , fgFill = "gray")
#         style_cf_bcg2_4            <- openxlsx::createStyle(bgFill = "gray"
#                                                             , fgFill = "gray")
#         style_cf_bcg2_4minus       <- openxlsx::createStyle(bgFill = "gray"
#                                                             , fgFill = "gray")
#         style_cf_bcg2_tie_4_5      <- openxlsx::createStyle(bgFill = "brown"
#                                                             , fgFill = "brown")
#         style_cf_bcg2_5plus        <- openxlsx::createStyle(bgFill = "orange"
#                                                             , fgFill = "orange")
#         style_cf_bcg2_5            <- openxlsx::createStyle(bgFill = "orange"
#                                                             , fgFill = "orange")
#         style_cf_bcg2_5minus       <- openxlsx::createStyle(bgFill = "orange"
#                                                             , fgFill = "orange")
#         style_cf_bcg2_tie_5_6      <- openxlsx::createStyle(bgFill = "purple"
#                                                             , fgFill = "purple")
#         style_cf_bcg2_6plus        <- openxlsx::createStyle(bgFill = "red"
#                                                             , fgFill = "red")
#         style_cf_bcg2_6            <- openxlsx::createStyle(bgFill = "red"
#                                                             , fgFill = "red")
#         style_cf_bcg2_na           <- openxlsx::createStyle(bgFill = "#808080"
#                                                             , fgFill = "#808080")
#
#         #### Excel, Formatting, CF, Rules ----
#         cf_rule_ft_vcold          <- "VeryCold"
#         cf_rule_ft_vcold_cold     <- "VCold_Cold"
#         cf_rule_ft_tie_vcold_cold <- "TIE_VCold_Cold"
#         cf_rule_ft_cold_vcold     <- '="Cold_VCold"'
#         cf_rule_ft_cold           <- '="Cold"'
#         cf_rule_ft_cold_cool      <- '="Cold_Cool"'
#         cf_rule_ft_tie_cold_cool  <- '="TIE_Cold_Cool"'
#         cf_rule_ft_cool_cold      <- '="Cool_Cold"'
#         cf_rule_ft_cool           <- '="Cool"'
#         cf_rule_ft_cool_warm      <- '="Cool_Warm"'
#         cf_rule_ft_tie_warm_cool  <- '="TIE_Warm_Cool"'
#         cf_rule_ft_warm_cool      <- '="Warm_Cool"'
#         cf_rule_ft_warm           <- '="Warm"'
#         cf_rule_ft_na             <- '="NA"'
#         cf_rule_bcg_1             <- '="1"'
#         cf_rule_bcg_2             <- '="2"'
#         cf_rule_bcg_3             <- '="3"'
#         cf_rule_bcg_4             <- '="4"'
#         cf_rule_bcg_5             <- '="5"'
#         cf_rule_bcg_6             <- '="6"'
#         cf_rule_bcg_na            <- '="NA"'
#         cf_rule_bdi_high          <- '="High"'
#         cf_rule_bdi_medium        <- '="Medium"'
#         cf_rule_bdi_low           <- '="Low"'
#         cf_rule_bdi_na            <- '="NA"'
#         cf_rule_mtti_vcold        <- '="Very cold"'
#         cf_rule_mtti_cold         <- '="Cold"'
#         cf_rule_mtti_cool         <- '="Cool"'
#         cf_rule_mtti_cool_warm    <- '="Cool/warm"'
#         cf_rule_mtti_warm         <- '="Warm"'
#         cf_rule_mtti_na           <- '="NA"'
#         cf_rule_bcg2_1            <- '="1"'
#         cf_rule_bcg2_2            <- '="2"'
#         cf_rule_bcg_2minus        <- '="2-"'
#         cf_rule_bcg2_tie_2_3      <- '="2/3 tie"'
#         cf_rule_bcg2_3plus        <- '="3+"'
#         cf_rule_bcg2_3            <- '="3"'
#         cf_rule_bcg2_3minus       <- '="3-"'
#         cf_rule_bcg2_tie_3_4      <- '="3/4 tie"'
#         cf_rule_bcg2_4plus        <- '="4+"'
#         cf_rule_bcg2_4            <- '="4"'
#         cf_rule_bcg2_4minus       <- '="4-"'
#         cf_rule_bcg2_tie_4_5      <- '="4/5 tie"'
#         cf_rule_bcg2_5plus        <- '="5+"'
#         cf_rule_bcg2_5            <- '="5"'
#         cf_rule_bcg2_5minus       <- '="5-"'
#         cf_rule_bcg2_tie_5_6      <- '="5/6 tie"'
#         cf_rule_bcg2_6plus        <- '="6+"'
#         cf_rule_bcg2_6            <- '="6"'
#         cf_rule_bcg2_na           <- '="NA"'
#
        ### Excel, WS, data ----
        #### Excel, WS, data, NOTES----
        openxlsx::writeData(wb
                            , sheet = "NOTES"
                            , x = notes_head
                            , startCol = 1
                            , startRow = 1
                            , colNames = FALSE)
        openxlsx::writeDataTable(wb
                                 , sheet = "NOTES"
                                 , x = notes_toc
                                 , startCol = 1
                                 , startRow = 15
                                 , colNames = TRUE
                                 , tableStyle = "TableStyleMedium9")

        openxlsx::addStyle(wb
                           , sheet = "NOTES"
                           , rows = 1
                           , cols = 1
                           , style = style_title)
        openxlsx::addStyle(wb
                           , sheet = "NOTES"
                           , rows = 2
                           , cols = 1
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "NOTES"
                           , rows = 4
                           , cols = 1
                           , style = style_hyperlink)
        openxlsx::addStyle(wb
                           , sheet = "NOTES"
                           , rows = 5
                           , cols = 1
                           , style = style_date)
        openxlsx::addStyle(wb
                           , sheet = "NOTES"
                           , rows = 7:9
                           , cols = 1
                           , style = style_bold)
        openxlsx::addStyle(wb
                           , sheet = "NOTES"
                           , rows = 11
                           , cols = 1
                           , style = style_h2)
#
#
#
#         # #### Excel, WS, data, Summary, Header ----
#         # # transposed
#         # openxlsx::writeData(wb
#         #                     , sheet = "summary"
#         #                     , x = df_report_summary_header_s
#         #                     , startCol = 1
#         #                     , startRow = mySR_trans)
#         #
#         # #### Excel, WS, data, Summary, Color Thresholds ----
#         # mySC_colthresh <- 8
#         # # Title
#         # openxlsx::writeData(wb
#         #                     , sheet = "summary"
#         #                     , x = "Color Code Thresholds"
#         #                     , startCol = mySC_colthresh
#         #                     , startRow = 1)
#         # # Title Style
#         # openxlsx::addStyle(wb
#         #                    , sheet = "summary"
#         #                    , rows = 1
#         #                    , cols = mySC_colthresh:(mySC_colthresh + ncol(df_col_thresh))
#         #                    , style = style_h2)
#         # # Body
#         # openxlsx::writeData(wb
#         #                     , sheet = "summary"
#         #                     , x = df_col_thresh
#         #                     , startCol = mySC_colthresh
#         #                     , startRow = 2
#         #                     , headerStyle = style_bold)
#
#
#         # #### Excel, WS, data, Summary, Wide ----
#         # # below transposed header
#         # mySF_summmary_wide <- mySR_trans +
#         #   nrow(df_report_summary_header_s) +
#         #   2
#         # openxlsx::writeData(wb
#         #                     , sheet = "summary"
#         #                     , x = df_report_summary_wide_s
#         #                     , startCol = 1
#         #                     , startRow = mySF_summmary_wide
#         #                     , headerStyle = style_bold
#         #                     , withFilter = TRUE)
#
        #### Excel, WS, data, Bug IBI BCG ----
        openxlsx::writeData(wb
                            , sheet = "Bug_IBI_BCG"
                            , x = df_report_b_ibi_bcg
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold
                            , withFilter = TRUE)

        #### Excel, WS, data, Fish IBI BCG ----
        # transposed
        openxlsx::writeData(wb
                            , sheet = "Fish_IBI_BCG"
                            , x = df_report_f_ibi_bcg
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold
                            , withFilter = TRUE)


        #### Excel, WS, data, Bug Data ----
        openxlsx::writeData(wb
                            , sheet = "BugData"
                            , x = df_report_b_samps
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold
                            , withFilter = TRUE)

        #### Excel, WS, data, Fish Data ----
        openxlsx::writeData(wb
                            , sheet = "FishData"
                            , x = df_report_f_samps
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold)

        #### Excel, WS, data, Bug IBI Metrics NFRR3 ----
        openxlsx::writeData(wb
                            , sheet = "Bug_IBI_Metrics_NFRR3"
                            , x = df_report_b_ibi_met_nfrr3
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold
                            , withFilter = TRUE)

        #### Excel, WS, data, Bug IBI Metrics NFGP4 ----
        openxlsx::writeData(wb
                            , sheet = "Bug_IBI_Metrics_NFRR3"
                            , x = df_report_b_ibi_met_nfgp4
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold
                            , withFilter = TRUE)

        #### Excel, WS, data, Fish Metrics NFStream5 ----
        openxlsx::writeData(wb
                            , sheet = "Fish_Metrics_NFStream5"
                            , x = df_report_fish_met_nfstream5
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold
                            , withFilter = TRUE)

        #### Excel, WS, data, Fish Metrics NFHead6 ----
        openxlsx::writeData(wb
                            , sheet = "Fish_Metrics_NFHead6"
                            , x = df_report_fish_met_nfhead6
                            , startCol = 1
                            , startRow = mySR
                            , headerStyle = style_bold
                            , withFilter = TRUE)
#
#         ### Excel, Freeze Panes----
#         # openxlsx::freezePane(wb
#         #                      , sheet = "summary"
#         #                      , firstActiveRow = mySF_summmary_wide + 1
#         #                      , firstActiveCol = "E")
#         openxlsx::freezePane(wb
#                              , sheet = "topindicator"
#                              , firstActiveRow = mySR + 1)
#         openxlsx::freezePane(wb
#                              , sheet = "samples"
#                              , firstActiveRow = mySR + 1)
#         openxlsx::freezePane(wb
#                              , sheet = "flags"
#                              , firstActiveRow = mySR + 1)
#         openxlsx::freezePane(wb, sheet = "site"
#                              , firstActiveRow = mySR + 1)
#         openxlsx::freezePane(wb
#                              , sheet = "taxatrans"
#                              , firstActiveRow = mySR + 1)
#         openxlsx::freezePane(wb
#                              , sheet = "taxatrans"
#                              , firstActiveRow = mySR + 1)
#         openxlsx::freezePane(wb
#                              , sheet = "taxatrans"
#                              , firstActiveRow = mySR + 1)
#         openxlsx::freezePane(wb
#                              , sheet = "taxatrans"
#                              , firstActiveRow = mySR + 1)
#
#         ### Excel, Auto-Filter----
#         # Add with writeData
#         #
#         # openxlsx::addFilter(wb
#         #                     , sheet = "summary"
#         #                     , rows = mySR
#         #                     , cols = 1:ncol(df_report_summary_wide))
#         # openxlsx::addFilter(wb
#         #                     , sheet = "topindicator"
#         #                     , rows = mySR
#         #                     , cols = 1:ncol(df_report_topindicator))
#         # openxlsx::addFilter(wb
#         #                     , sheet = "samples"
#         #                     , rows = mySR
#         #                     , cols = 1:ncol(df_report_samples))
#         # openxlsx::addFilter(wb
#         #                     , sheet = "flags"
#         #                     , rows = mySR
#         #                     , cols = 1:ncol(df_report_flags))
#         # openxlsx::addFilter(wb
#         #                     , sheet = "site"
#         #                     , rows = mySR
#         #                     , cols = 1:ncol(df_report_site))
#         # openxlsx::addFilter(wb
#         #                     , sheet = "taxatrans"
#         #                     , rows = mySR
#         #                     , cols = 1:ncol(df_report_taxatrans))
#
#
#
        ### Excel, WS Name to A1 ----
        # name
        # openxlsx::writeData(wb
        #                     , sheet = "summary"
        #                     , x = "summary"
        #                     , startCol = 1
        #                     , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "Bug_IBI_BCG"
                            , x = "Bug_IBI_BCG"
                            , startCol = 1
                            , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "Fish_IBI_BCG"
                            , x = "Fish_IBI_BCG"
                            , startCol = 1
                            , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "BugData"
                            , x = "BugData"
                            , startCol = 1
                            , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "FishData"
                            , x = "FishData"
                            , startCol = 1
                            , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "Bug_IBI_Metrics_NFRR3"
                            , x = "Bug_IBI_Metrics_NFRR3"
                            , startCol = 1
                            , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "Bug_IBI_Metrics_NFGP5"
                            , x = "Bug_IBI_Metrics_NFGP5"
                            , startCol = 1
                            , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "Fish_Metrics_NFStream5"
                            , x = "Fish_Metrics_NFStream5"
                            , startCol = 1
                            , startRow = 1)
        openxlsx::writeData(wb
                            , sheet = "Fish_Metrics_NFHead6"
                            , x = "Fish_Metrics_NFHead6"
                            , startCol = 1
                            , startRow = 1)
        # style
        # openxlsx::addStyle(wb
        #                    , sheet = "summary"
        #                    , rows = 1
        #                    , cols = 1:4
        #                    , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "Bug_IBI_BCG"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "Fish_IBI_BCG"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "BugData"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "FishData"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "Bug_IBI_Metrics_NFRR3"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "Bug_IBI_Metrics_NFGP5"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "Fish_Metrics_NFStream5"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)
        openxlsx::addStyle(wb
                           , sheet = "Fish_Metrics_NFHead6"
                           , rows = 1
                           , cols = 1:4
                           , style = style_h1)

        ### Excel, Apply Style ----

        #### NOTES, Named Range
        openxlsx::createNamedRegion(wb
                                    , sheet = "NOTES"
                                    , name = "FileName"
                                    , rows = 8
                                    , cols = 2)

#
#         ##### summary, Color Thresholds----
#         ## Center justify to all but last col
#         # openxlsx::addStyle(wb
#         #                    , sheet = "summary"
#         #                    , rows = 3:7
#         #                    , cols = 8:16
#         #                    , style = style_halign_center
#         #                    , gridExpand = TRUE)
#         ## NA to all
#         # openxlsx::addStyle(wb
#         #                    , sheet = "summary"
#         #                    , rows = 3:7
#         #                    , cols = 8:17
#         #                    , style = style_cf_ft_na
#         #                    , gridExpand = TRUE)
#         ##### MTTI----
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 3
#                            , cols = 8
#                            , style = style_cf_mtti_vcold
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 4
#                            , cols = 8
#                            , style = style_cf_mtti_cold
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 5
#                            , cols = 8
#                            , style = style_cf_mtti_cool
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 6
#                            , cols = 8
#                            , style = style_cf_mtti_cool_warm
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 7
#                            , cols = 8
#                            , style = style_cf_ft_warm
#                            , gridExpand = TRUE)
#         ##### thermal metrics----
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 3
#                            , cols = c(10:11, 12, 13:15, 17)
#                            , style = style_cf_ft_vcold_cold
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 4
#                            , cols = c(10, 11, 12, 13:15, 17)
#                            , style = style_cf_ft_cold
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 5
#                            , cols = c(11, 12, 13, 14, 15, 17)
#                            , style = style_cf_ft_cool
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 6
#                            , cols = c(11:15, 17)
#                            , style = style_cf_ft_tie_warm_cool
#                            , gridExpand = TRUE)
#         openxlsx::addStyle(wb
#                            , sheet = "summary"
#                            , rows = 7
#                            , cols = c(15:17)
#                            , style = style_cf_ft_warm
#                            , gridExpand = TRUE)
#
#
#
#         ### Excel, col width ----
#         # "auto" doesn't seem to work as it only looks at the first row
#         # add 2 for filter
#         widths_val_filt <- 3
#         widths_val_min <- 6
#         # not quite right but close enough
#         # not sure if getting proper names() for widths
#
#         #NOTES
#         widths_notes <- c(23, 39, 23)
#         openxlsx::setColWidths(wb
#                                , sheet = "NOTES"
#                                , cols = seq_len(length(widths_notes))
#                                , widths = widths_notes)
#
#
#         df_widths <- df_report_summary_wide_s
#         widths_min <- rep(widths_val_min, ncol(df_widths))
#         widths_df <- unlist(lapply(df_widths, function(x) max(nchar(x), na.rm = TRUE)))
#         widths_names <- unlist(lapply(names(df_widths), function(x) max(nchar(x), na.rm = TRUE)))
#         widths_excel <- pmax(widths_min
#                              , widths_df
#                              , widths_names
#                              , na.rm = TRUE) + widths_val_filt
#         openxlsx::setColWidths(wb
#                                , sheet = "summary"
#                                , cols = seq_len(ncol(df_widths))
#                                , widths = widths_excel)
#         rm(df_widths)
#
#         df_widths <- df_report_topindicator_s
#         widths_min <- rep(widths_val_min, ncol(df_widths))
#         widths_df <- unlist(lapply(df_widths, function(x) max(nchar(x), na.rm = TRUE)))
#         widths_names <- unlist(lapply(names(df_widths), function(x) max(nchar(x), na.rm = TRUE)))
#         widths_excel <- pmax(widths_min
#                              , widths_df
#                              , widths_names
#                              , na.rm = TRUE) + widths_val_filt
#         openxlsx::setColWidths(wb
#                                , sheet = "topindicator"
#                                , cols = seq_len(ncol(df_widths))
#                                , widths = widths_excel)
#         rm(df_widths)
#
#         df_widths <- df_report_samples_s
#         widths_min <- rep(widths_val_min, ncol(df_widths))
#         widths_df <- unlist(lapply(df_widths, function(x) max(nchar(x), na.rm = TRUE)))
#         widths_names <- unlist(lapply(names(df_widths), function(x) max(nchar(x), na.rm = TRUE)))
#         widths_excel <- pmax(widths_min
#                              , widths_df
#                              , widths_names
#                              , na.rm = TRUE) + widths_val_filt
#         openxlsx::setColWidths(wb
#                                , sheet = "samples"
#                                , cols = seq_len(ncol(df_widths))
#                                , widths = widths_excel)
#         rm(df_widths)
#
#         df_widths <- df_report_flags_s
#         widths_min <- rep(widths_val_min, ncol(df_widths))
#         widths_df <- unlist(lapply(df_widths, function(x) max(nchar(x), na.rm = TRUE)))
#         widths_names <- unlist(lapply(names(df_widths), function(x) max(nchar(x), na.rm = TRUE)))
#         widths_excel <- pmax(widths_min
#                              , widths_df
#                              , widths_names
#                              , na.rm = TRUE) + widths_val_filt
#         openxlsx::setColWidths(wb
#                                , sheet = "flags"
#                                , cols = seq_len(ncol(df_widths))
#                                , widths = widths_excel)
#
#         df_widths <- df_report_site_s
#         widths_min <- rep(widths_val_min, ncol(df_widths))
#         widths_df <- unlist(lapply(df_widths, function(x) max(nchar(x), na.rm = TRUE)))
#         widths_names <- unlist(lapply(names(df_widths), function(x) max(nchar(x), na.rm = TRUE)))
#         widths_excel <- pmax(widths_min
#                              , widths_df
#                              , widths_names
#                              , na.rm = TRUE) + widths_val_filt
#         openxlsx::setColWidths(wb
#                                , sheet = "site"
#                                , cols = seq_len(ncol(df_widths))
#                                , widths = widths_excel)
#         rm(df_widths)
#
#         df_widths <- df_report_taxatrans_s
#         widths_min <- rep(widths_val_min, ncol(df_widths))
#         widths_df <- unlist(lapply(df_widths, function(x) max(nchar(x), na.rm = TRUE)))
#         widths_names <- unlist(lapply(names(df_widths), function(x) max(nchar(x), na.rm = TRUE)))
#         widths_excel <- pmax(widths_min
#                              , widths_df
#                              , widths_names
#                              , na.rm = TRUE) + widths_val_filt
#         openxlsx::setColWidths(wb
#                                , sheet = "taxatrans"
#                                , cols = seq_len(ncol(df_widths))
#                                , widths = widths_excel)
#         rm(df_widths)
#
#         ### Excel, Conditional Formatting----
#
#         #### Excel, CF, Fuzzy Thermal----
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="VeryCold"'
#         #                       , style = style_cf_ft_vcold)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="VCold_Cold"'
#         #                       , style = style_cf_ft_vcold_cold)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="TIE_VCold_Cold"'
#         #                       , style = style_cf_ft_tie_vcold_cold)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Cold_VCold"'
#         #                       , style = style_cf_ft_cold_vcold)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Cold"'
#         #                       , style = style_cf_ft_cold)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Cold_Cool"'
#         #                       , style = style_cf_ft_cold_cool)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="TIE_Cold_Cool"'
#         #                       , style = style_cf_ft_tie_cold_cool)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Cool_Cold"'
#         #                       , style = style_cf_ft_cool_cold)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Cool"'
#         #                       , style = style_cf_ft_cool)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Cool_Warm"'
#         #                       , style = style_cf_ft_cool_warm)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="TIE_Warm_Cool"'
#         #                       , style = style_cf_ft_tie_warm_cool)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Warm_Cool"'
#         #                       , style = style_cf_ft_warm_cool)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="Warm"'
#         #                       , style = style_cf_ft_warm)
#         # conditionalFormatting(wb, "Fuzzy_Thermal"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_ft))
#         #                       , rule = '="NA"'
#         #                       , style = style_cf_ft_na)
#         #
#         #### Excel, CF, BCG----
#         # conditionalFormatting(wb, "BCG"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg))
#         #                       , rule = '="1"'
#         #                       , style = style_cf_bcg_1)
#         # conditionalFormatting(wb, "BCG"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg))
#         #                       , rule = '="2"'
#         #                       , style = style_cf_bcg_2)
#         # conditionalFormatting(wb, "BCG"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg))
#         #                       , rule = '="3"'
#         #                       , style = style_cf_bcg_3)
#         # conditionalFormatting(wb, "BCG"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg))
#         #                       , rule = '="4"'
#         #                       , style = style_cf_bcg_4)
#         # conditionalFormatting(wb, "BCG"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg))
#         #                       , rule = '="5"'
#         #                       , style = style_cf_bcg_5)
#         # conditionalFormatting(wb, "BCG"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg))
#         #                       , rule = '="6"'
#         #                       , style = style_cf_bcg_6)
#         #
#         # conditionalFormatting(wb, "BCG"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg))
#         #                       , rule = '="NA"'
#         #                       , style = style_cf_bcg_na)
#         #
#         #### Excel, CF, BDI----
#         #
#         # conditionalFormatting(wb, "BDI"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bdi))
#         #                       , rule = '="High"'
#         #                       , style = style_cf_bdi_high)
#         # conditionalFormatting(wb, "BDI"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bdi))
#         #                       , rule = '="Medium"'
#         #                       , style = style_cf_bdi_medium)
#         # conditionalFormatting(wb, "BDI"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bdi))
#         #                       , rule = '="Low"'
#         #                       , style = style_cf_bdi_low)
#         # conditionalFormatting(wb, "BDI"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bdi))
#         #                       , rule = '="NA"'
#         #                       , style = style_cf_bdi_na)
#         #
#         #
#         #### Excel, CF, BCG2----
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="1"'
#         #                       , style = style_cf_bcg2_1)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="2"'
#         #                       , style = style_cf_bcg2_2)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="2-"'
#         #                       , style = style_cf_bcg2_2minus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="2/3 tie"'
#         #                       , style = style_cf_bcg2_tie_2_3)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="3+"'
#         #                       , style = style_cf_bcg2_3plus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="3"'
#         #                       , style = style_cf_bcg2_3)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="3-"'
#         #                       , style = style_cf_bcg2_3minus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="3/4 tie"'
#         #                       , style = style_cf_bcg2_tie_3_4)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="4+"'
#         #                       , style = style_cf_bcg2_4plus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="4"'
#         #                       , style = style_cf_bcg2_4)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="4-"'
#         #                       , style = style_cf_bcg2_4minus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="4/5 tie"'
#         #                       , style = style_cf_bcg2_tie_4_5)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="5+"'
#         #                       , style = style_cf_bcg2_5plus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="5"'
#         #                       , style = style_cf_bcg2_5)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="5-"'
#         #                       , style = style_cf_bcg2_5minus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="5/6 tie"'
#         #                       , style = style_cf_bcg2_tie_5_6)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="6+"'
#         #                       , style = style_cf_bcg2_6plus)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="6"'
#         #                       , style = style_cf_bcg2_6)
#         # conditionalFormatting(wb, "BCG2"
#         #                       , cols = 2
#         #                       , rows = (mySR + 1):(mySR + nrow(df_bcg2))
#         #                       , rule = '="NA"'
#         #                       , style = style_cf_bcg2_na)
#
#
#         #### Excel, CF, summary, MTTI----
#         df_cf <- df_report_summary_wide_s
#         cols_cf <- match("MTTI", names(df_cf))
#         rows_cf <- (mySF_summmary_wide + 1):(mySF_summmary_wide + nrow(df_cf))
#
#         # Applied in reverse order in Excel
#         openxlsx::conditionalFormatting(wb, "summary"
#                                         , cols = cols_cf
#                                         , rows = rows_cf
#                                         , rule = '="NA"'
#                                         , style = style_cf_mtti_na)
#         openxlsx::conditionalFormatting(wb, "summary"
#                                         , cols = cols_cf
#                                         , rows = rows_cf
#                                         , rule = '>=23'
#                                         , style = style_cf_mtti_warm)
#         openxlsx::conditionalFormatting(wb, "summary"
#                                         , cols = cols_cf
#                                         , rows = rows_cf
#                                         , rule = '<23'
#                                         , style = style_cf_mtti_cool_warm)
#         openxlsx::conditionalFormatting(wb, "summary"
#                                         , cols = cols_cf
#                                         , rows = rows_cf
#                                         , rule = '<21'
#                                         , style = style_cf_mtti_cool)
#         openxlsx::conditionalFormatting(wb, "summary"
#                                         , cols = cols_cf
#                                         , rows = rows_cf
#                                         , rule = '<19'
#                                         , style = style_cf_mtti_cold)
#         openxlsx::conditionalFormatting(wb, "summary"
#                                         , cols = cols_cf
#                                         , rows = rows_cf
#                                         , rule = '<16'
#                                         , style = style_cf_mtti_vcold)
#
#         #### Excel, CF, summary, Thermal Metrics----
#         df_cf <- df_report_summary_wide_s
#         rows_cf <- (mySF_summmary_wide + 1):(mySF_summmary_wide + nrow(df_cf))
#
#         ##### nt_ti_stenocold----
#         myMetNam <- "nt_ti_stenocold"
#         cols_cf <- match(myMetNam, names(df_col_thresh)) + mySC_colthresh - 1
#         #
#         if (!is.na(cols_cf)) {
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '="NA"'
#                                           , style = style_cf_ft_na)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=1'
#                                           , style = style_cf_ft_cold)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=3'
#                                           , style = style_cf_ft_vcold_cold)
#         }## IF ~ !is.na(cols_cf)
#
#         ##### nt_ti_stenocold_cold----
#         myMetNam <- "nt_ti_stenocold_cold"
#         cols_cf <- match(myMetNam, names(df_col_thresh)) + mySC_colthresh - 1
#         #
#         if (!is.na(cols_cf)) {
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '="NA"'
#                                           , style = style_cf_ft_na)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=1'
#                                           , style = style_cf_ft_tie_warm_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=3'
#                                           , style = style_cf_ft_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=5'
#                                           , style = style_cf_ft_cold)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=10'
#                                           , style = style_cf_ft_vcold_cold)
#         }## IF ~ !is.na(cols_cf)
#
#         ##### nt_ti_stenocold_cold_cool----
#         myMetNam <- "nt_ti_stenocold_cold_cool"
#         cols_cf <- match(myMetNam, names(df_col_thresh)) + mySC_colthresh - 1
#         #
#         if (!is.na(cols_cf)) {
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '="NA"'
#                                           , style = style_cf_ft_na)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=9'
#                                           , style = style_cf_ft_tie_warm_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=20'
#                                           , style = style_cf_ft_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=25'
#                                           , style = style_cf_ft_cold)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=30'
#                                           , style = style_cf_ft_vcold_cold)
#         }## IF ~ !is.na(cols_cf)
#
#         ##### pt_ti_stenocold_cold_cool----
#         myMetNam <- "pt_ti_stenocold_cold_cool"
#         cols_cf <- match(myMetNam, names(df_col_thresh)) + mySC_colthresh - 1
#         #
#         if (!is.na(cols_cf)) {
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '="NA"'
#                                           , style = style_cf_ft_na)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=20'
#                                           , style = style_cf_ft_tie_warm_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=35'
#                                           , style = style_cf_ft_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=50'
#                                           , style = style_cf_ft_cold)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=65'
#                                           , style = style_cf_ft_vcold_cold)
#         }## IF ~ !is.na(cols_cf)
#
#         ##### pi_ti_stenocold_cold_cool----
#         myMetNam <- "pi_ti_stenocold_cold_cool"
#         cols_cf <- match(myMetNam, names(df_col_thresh)) + mySC_colthresh - 1
#         #
#         if (!is.na(cols_cf)) {
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '="NA"'
#                                           , style = style_cf_ft_na)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=10'
#                                           , style = style_cf_ft_tie_warm_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=30'
#                                           , style = style_cf_ft_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=40'
#                                           , style = style_cf_ft_cold)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=55'
#                                           , style = style_cf_ft_vcold_cold)
#         }## IF ~ !is.na(cols_cf)
#
#         ##### pt_ti_warm_stenowarm----
#         myMetNam <- "pt_ti_warm_stenowarm"
#         cols_cf <- match(myMetNam, names(df_col_thresh)) + mySC_colthresh - 1
#         #
#         if (!is.na(cols_cf)) {
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '="NA"'
#                                           , style = style_cf_ft_na)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=40'
#                                           , style = style_cf_ft_warm)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '<40'
#                                           , style = style_cf_ft_tie_warm_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '<15'
#                                           , style = style_cf_ft_cool)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '<10'
#                                           , style = style_cf_ft_cold)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '<5'
#                                           , style = style_cf_ft_vcold_cold)
#         }## IF ~ !is.na(cols_cf)
#
#         ##### nt_ti_stenowarm----
#         myMetNam <- "nt_ti_stenowarm"
#         cols_cf <- match(myMetNam, names(df_col_thresh)) + mySC_colthresh - 1
#         #
#         if (!is.na(cols_cf)) {
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '="NA"'
#                                           , style = style_cf_ft_na)
#           openxlsx::conditionalFormatting(wb, "summary"
#                                           , cols = cols_cf
#                                           , rows = rows_cf
#                                           , rule = '>=2'
#                                           , style = style_cf_ft_warm)
#         }## IF ~ !is.na(cols_cf)
#
#
#         # # CF, data bar
#         # addWorksheet(wb, "databar")
#         # writeData(wb, "databar", -5:5)
#         # conditionalFormatting(wb, "databar", cols = 1, rows = 1:12, type = "databar")
#
#         ### Excel, WB, Save  ----
#         # prog_detail <- "Save Results"
#         # message(paste0("\n", prog_detail))
#         # # Increment the progress bar, and update the detail text.
#         # incProgress(1/prog_n, detail = prog_detail)
#         # Sys.sleep(prog_sleep)
#         #
#
#         # Save new Excel file.
#         fn_wb <- file.path(path_results_sub, paste0("results_", s, ".xlsx"))
#         openxlsx::saveWorkbook(wb, fn_wb, overwrite = TRUE)
#
#       }## FOR ~ s


      #
              # Save new Excel file.
              fn_wb <- file.path(path_results_sub, paste0("report_bio.xlsx"))
              openxlsx::saveWorkbook(wb, fn_wb, overwrite = TRUE)




      ## Calc, 04, Zip Results ----
      prog_detail <- "Create Zip File"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)


      ## Calc, 05, Clean Up ----
      prog_detail <- "Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # button, enable, download
      shinyjs::enable("b_download_rep_bio")

    }## expr ~ withProgress ~
    , message = "Calculating Report, Biological"
    )## withProgress ~
  }##expr ~ ObserveEvent ~
  )##observeEvent ~ b_calc_rep_bio



  ## b_download_rep_multi ----
  output$b_download_rep_multi <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input_rep_multi
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_report
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".xlsx")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, dn_files_report, "results.xlsx"), fname)

    }##content~END
  )##download ~ Report Multi


  ## b_download_rep_bio ----
  output$b_download_rep_bio <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input_rep_bio
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_report
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
  )##download ~ Report bio



})##shinyServer ~ END
