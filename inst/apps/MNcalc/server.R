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

  output$fn_input_display_rep_single <- renderText({
    inFile <- input$fn_input_rep_single

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_rep_single

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

    # shinyjs::enable("b_calc_rep_single")
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



  # FB, MERGE FILES ----

  ## Merge, Import ----
  ### Merge, Import, FileWatch ----
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
  observeEvent(input$b_calc_mergefiles, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 00, Set Up Shiny Code ----

      prog_detail <- "Calculation, Merge Files..."
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
      shinyjs::disable("b_download_mergefiles")

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


  ## b_download_mergefiles ----
  output$b_download_mergefiles <- downloadHandler(

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
            , pi_tv_senscoldwater_ExclSchool = dplyr::case_when(INDEX_CLASS == "8"
                                                         ~ pi_tv_senscoldwater_ExclSchool - ((-27.382 * log10(DRAINSQMI)) + 114.322)
                                                         , .default = pi_tv_senscoldwater_ExclSchool)
            , pt_detritivore = dplyr::case_when(INDEX_CLASS == "8"
                                         ~ pt_detritivore - ((16.211 * log10(DRAINSQMI)) + -5.276)
                                         , .default = pt_detritivore)
            , nt_tv_tolercoldwater = dplyr::case_when(INDEX_CLASS == "8"
                                               ~ nt_tv_tolercoldwater - ((1.089 * log10(DRAINSQMI)) + -0.827)
                                               , .default = nt_tv_tolercoldwater)
            , pt_natcoldwater = dplyr::case_when(INDEX_CLASS == "8"
                                          ~ pt_natcoldwater - ((-24.242 * log10(DRAINSQMI)) + 54.017)
                                          , .default = pt_natcoldwater)
            , pt_tv_senscoldwater = dplyr::case_when(INDEX_CLASS == "9"
                                              ~ pt_tv_senscoldwater - ((23.788 * log10(GRADIENT)) + 24.437)
                                              , .default = pt_tv_senscoldwater)
            # Log10(x+1)
            , pi_natcoldwater_ExclSchool = dplyr::case_when(INDEX_CLASS == "8"
                                                     ~ log10(pi_natcoldwater_ExclSchool + 1)
                                                     , .default = pi_natcoldwater_ExclSchool)
            , pi_tv_tolercoldwater_ExclSchool = dplyr::case_when(INDEX_CLASS == "9"
                                                          ~ log10(pi_tv_tolercoldwater_ExclSchool + 1)
                                                          , .default = pi_tv_tolercoldwater_ExclSchool)
            , pi_nonlithophil_ExclSchool = dplyr::case_when(INDEX_CLASS == "9"
                                                     ~ log10(pi_nonlithophil_ExclSchool + 1)
                                                     , .default = pi_nonlithophil_ExclSchool)
            , pi_Perciformes_ExclSchool = dplyr::case_when(INDEX_CLASS == "9"
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
                    , "ni_total", "nt_total")
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
        # INDEX_CLASS 8 and 9
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
          mutate(les_mult_ni = case_when(ni_total < 25
                                    & INDEX_CLASS %in% ic_les_1 ~ 0
                                    , .default = 1)) %>%
          mutate(les_mult_ni = case_when(ni_total < 25
                                         & INDEX_CLASS %in% ic_les_2 ~ 0
                                         , .default = les_mult_ni)) %>%
          mutate(les_mult_nt = case_when(nt_total < 6
                                         & INDEX_CLASS %in% ic_les_1 ~ 0
                                         , .default = 1)) %>%
          mutate(les_mult_nt = case_when(nt_total < 4
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
          mutate(Index = round(sum_Index * 10 / Index_n_metrics)) %>%
          # Score % DELT
          mutate(Index_mod_delt = case_when(pi_delt_ExclSchool >= 2 ~ -5
                                            , pi_delt_ExclSchool >= 4 ~ -10
                                            , .default = 0)) %>%
          # Update sum_index
          mutate(Index = Index + Index_mod_delt)

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
                                     , Index_mod_delt
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
                        , "ni_total", "nt_total")
      cols_ibi_met_val <- unique(df_rules[, "METRIC_NAME", TRUE])
      cols_ibi_met_sco <- paste0("SC_", cols_ibi_met_val)
      cols_ibi_index <- c("sum_Index", "Index", "Index_Nar"
                          , "Index_n_metrics")
      cols_ibi_index_fish <- c("sum_Index_ORIG", "Index_ORIG"
                               , "les_mult_ni", "les_mult_nt"
                               , "Index_mod_delt")
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

})##shinyServer ~ END
