# Template 2, Data 
# Erik.Leppo@tetratech.com
# 2023-10-25
# Work on template code in script before move to server.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# START ----

# Directory
setwd(file.path("inst", "shiny-examples", "BCGcalc"))
# Other
path_results <- file.path("results")


# Minimal ----
{
# Template file
fn_template <- list.files(path_results
                          , pattern = "^Template_TemperatureReport.*\\.xlsx$")


# Files, ALL
fn_all <- list.files(path = path_results
                     , full.names = TRUE
                     , recursive = TRUE)
len_fn_all <- length(fn_all)
# Files, DataFrame
df_fn_all <- data.frame("path" = fn_all
                        , "file" = basename(fn_all)
                        , "dir_full" = dirname(fn_all))
df_fn_all[, "dir_zip"] <- sub(paste0("^", path_results, "/")
                              , ""
                              , df_fn_all[, "dir_full"])

# Read Template
# read template file
path_template <- file.path(path_results, fn_template)
n_skip <- 3

### Template, Summary, Header ----
sh_template <- "summary_header"
df_template_summary_header <- readxl::read_excel(path = path_template
                                                 , sheet = sh_template
                                                 , skip = n_skip)
df_template_summary_header[, "sheet"] <- sh_template

### Template, Summary, Wide ----
sh_template <- "summary_wide"
df_template_summary_wide <- readxl::read_excel(path = path_template
                                               , sheet = sh_template
                                               , skip = n_skip)
df_template_summary_wide[, "sheet"] <- sh_template

### Template, Top Indicator ----
sh_template <- "topindicator"
df_template_topindicator <- readxl::read_excel(path = path_template
                                               , sheet = sh_template
                                               , skip = n_skip)
df_template_topindicator[, "sheet"] <- sh_template

### Template, Samples ----
sh_template <- "samples"
df_template_samples <- readxl::read_excel(path = path_template
                                          , sheet = sh_template
                                          , skip = n_skip)
df_template_samples[, "sheet"] <- sh_template

### Template, Flags ----
sh_template <- "flags"
df_template_flags <- readxl::read_excel(path = path_template
                                        , sheet = sh_template
                                        , skip = n_skip)
df_template_flags[, "sheet"] <- sh_template

### Template, Site ----
sh_template <- "site"
df_template_site <- readxl::read_excel(path = path_template
                                       , sheet = sh_template
                                       , skip = n_skip)
df_template_site[, "sheet"] <- sh_template

### Template, Taxa Trans ----
sh_template <- "taxatrans"
df_template_taxatrans <- readxl::read_excel(path = path_template
                                            , sheet = sh_template
                                            , skip = n_skip)
df_template_taxatrans[, "sheet"] <- sh_template

### Template, file names ----
df_template_all <- dplyr::bind_rows(df_template_summary_header
                                    , df_template_summary_wide
                                    , df_template_topindicator
                                    , df_template_samples
                                    , df_template_flags
                                    , df_template_site
                                    , df_template_taxatrans
                                    , .id = "id")
df_template_sourcefiles <- unique(df_template_all[, c("inclusion", "source folder", "source file (or suffix)"), TRUE])
df_template_sourcefiles[, c("exact", "pattern", "present")] <- NA_integer_

### QC, File Names----
# check for each as CSV and Exact

for (i in seq_len(nrow(df_template_sourcefiles))) {
  
  # not working
  df_template_sourcefiles[i, "exact"] <- sum(grepl(pattern = paste("^", df_template_sourcefiles[i, "source file (or suffix)"], "$")
                                                   , basename(fn_all)))
  
  df_template_sourcefiles[i, "pattern"] <- sum(grepl(pattern = paste0(".?", df_template_sourcefiles[i, "source file (or suffix)"], "$")
                                                     , basename(fn_all)))
  
}## FOR ~ i

df_template_sourcefiles[, "present"] <- df_template_sourcefiles[, "exact"] + 
  df_template_sourcefiles[, "pattern"]

sourcefiles_missing <- dplyr::filter(df_template_sourcefiles
                                     , inclusion == "required" 
                                     & (present == 0 | is.na(present)))

# Shiny Alert for missing required files



# NEW**----



### File Names, Add Path
# smaller file
for (i in seq_len(nrow(df_template_sourcefiles))) {
  
  if (df_template_sourcefiles[i, "present", TRUE] == 1) {
    
    df_template_sourcefiles[i, "path", TRUE] <- fn_all[grep(pattern = paste0(".?", df_template_sourcefiles[i, "source file (or suffix)"], "$")
          , basename(fn_all))]
    
  } else {
    next # != 1
  }## IF ~ present == 1
  
}## FOR ~ i  
  
  # 
  # df_template_sourcefiles[i, "path"] <- ifelse(df_template_sourcefiles[i, "source folder", TRUE] == "NA"
  #                                              , df_template_sourcefiles[i, "source file (or suffix)", TRUE]
  #                                              , file.path(df_template_sourcefiles[i, "source folder", TRUE]
  #                                                          , df_template_sourcefiles[i, "source file (or suffix)", TRUE])
  # )
  # 
  # 

# File Names, File Exists
df_template_sourcefiles[, "exist_file"] <- file.exists(df_template_sourcefiles[, "path", TRUE])

  


### File Names, Add to col names
dim(df_template_all)
df_template_all_info <- dplyr::left_join(x = df_template_all
                                    , y = unique(df_template_sourcefiles[, c("source folder"
                                                                      , "source file (or suffix)"
                                                                      , "path"
                                                                      , "exist_file")])
                                    , multiple = "all"
                                    )
dim(df_template_all_info) # should be same number of rows + 2 cols

# should have kicked out earlier if files not present

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

# identify files
path_unique <- sort(unique(df_template_all_info[, "path", TRUE]))

df_template_all_info <- dplyr::mutate(df_template_all_info
                               , list_num = dplyr::case_when(!is.na(path) ~ paste0("data"
                                                                            , sprintf("%02d"
                                                                                      , match(df_template_all_info[, "path", TRUE]
                                                                                              , path_unique)))
                                                      , TRUE ~ NA))


# df_template_all_info[, "list_num"] <- paste0("data"
#                                              , sprintf("%02d"
#                                                        , match(df_template_all_info[, "path", TRUE]
#                                                                , path_unique)))

files2import <- unique(df_template_all_info[, c("path", "list_num"), TRUE])
files2import <- na.omit(files2import)

ls_data <- vector(mode = "list", length = nrow(files2import))


# DATA, IMPORT -----

# import all the files to a list
for (i in seq_len(nrow(files2import))) {

  #ls_data[[i]] <- read.csv(files2import[i, "path", TRUE])
  ls_data[[i]] <- readr::read_csv(files2import[i, "path", TRUE])
                                 # , show_col_types = FALSE)
  # leave on verbose so can trouble shoot later if necessary
  # user tidyverse to preserve column names to match user names in Excel
  
}## FOR ~ i ~ files2import

# add names
names(ls_data) <- files2import[, "list_num", TRUE]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# use master list so have all info

}## MINIMAL


#cycle through "sheet" in df_template_all_info
sheet_names <- unique(df_template_all_info[, "sheet", TRUE])

# initalize list for results
ls_results <- vector(mode = "list", length = length(sheet_names))
names(ls_results) <- sheet_names

p <- 3 # QC
for (p in c(3)) {
# for (p in seq_len(length(sheet_names))) {
  sheet_names[p]

  ## data, site ----
  data_sheet <- sheet_names[p] # site
  data_template <- df_template_all_info[df_template_all_info[, "sheet"] == data_sheet, ]
  
  # cycle each data_set
  data_sets <- unique(data_template$list_num)
  n_data_sets <- length(data_sets)
  
  #q <- 2 # QC
  for (q in seq_len(length(data_sets))) {
    data_sets[q]
    
    # filter data_template
    data_template_q <- data_template[data_template$list_num == data_sets[q], ]
    
    # get correct data sheet
    ls_sheet <- ls_data[[data_sets[q]]] # e.g., "data08"
    
    names_sheet <- names(ls_sheet)
    
    boo_req <- data_template_q[, "inclusion", TRUE] == "required"
    
    data_template_q_names <- data_template_q[, "original name", TRUE]
    data_template_q_names_req <- data_template_q_names[boo_req]
    
    # Always add StationID and SampleID
    names_base <- c("StationID", "SampleID")
    names_extra <- c(names_base, toupper(names_base))
    
    
    # Check names
    data_template_q_names_extra <- unique(c(data_template_q_names, names_extra))
    boo_names_extra_present <- data_template_q_names_extra %in% names_sheet
    
    boo_names_present_req <- data_template_q_names_req %in% names_sheet
    
    names_req_missing <- data_template_q_names_req[!boo_names_present_req]
    
    msg <- paste0("Worksheet '", data_sheet, "':\n\n", paste(names_req_missing, collapse = "\n"))
    cat(msg)
    
    # only stop if required is missing
    if (any(boo_names_present_req == FALSE)) {
      # end process with pop up
      msg <- paste0("Worksheet '", data_sheet, "':\n\n", paste(names_req_missing, collapse = "\n"))
      shinyalert::shinyalert(title = "Template 'original name' not matching data names!"
                             , text = msg
                             , type = "error"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      shiny::validate(msg)
    }## IF ~ length(fn_template) == 0
    
    # use present columns only
    ## maintain order from template
    data_template_q_names_present <- data_template_q_names_extra[boo_names_extra_present]
    
    df_pq <- ls_sheet[, data_template_q_names_present]
    
    if (q == 1) {
      df_p <- df_pq
    } else {
      # Combine with dplyr join and let it figure out the "by"
      names_p <- toupper(names(df_p))
      names_pq <- toupper(names(df_pq))
      names_int <- intersect(names_p, names_pq)
      by_p <- names(df_p)[match(names_int, names_p)]
      by_pq <- names(df_pq)[match(names_int, names_pq)]
      
      
      # stop if no way to join
      if (length(names_int) == 0) {
        # end process with pop up
        msg <- paste0("Worksheet '", data_sheet, "':\n\n", "no common column to join.")
        shinyalert::shinyalert(title = "Template missing column to join to Stations or Samples!"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        shiny::validate(msg)
      }## IF ~ length(fn_template) == 0
      
      # probably only works for single pair or names
      df_p <- dplyr::left_join(df_p, df_pq
                        , by = dplyr::join_by(!!by_p == !!by_pq))
    }## IF ~ q == 1
    
  }## FOR ~ q
  
  # save results
  ls_results[[p]] <- df_p
  
  # cleanup
  rm(df_p)
  rm(df_pq)
  
}## FOR ~ p

str(ls_results)


# ALL







