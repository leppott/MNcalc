# Pseudo Code for Sever for Report Template
# Erik.Leppo@tetratech.com
# 2023-10-23
#~~~~~~~~~~~~~~~
# Write code as script then insert in to server code


setwd(file.path("inst", "shiny-examples", "BCGcalc"))

## Template ----
# read template file (^Template_TemperatureReport)
# validate for only one file

# unzip
zip::unzip(file.path("results", "example_report_files.zip")
           , overwrite = TRUE
           , exdir = "results")

# on import change what is displayed.
# zip - go to list of files after unzip

fn_template <- list.files("results", pattern = "^Template_TemperatureReport.*\\.xlsx$")

if (length(fn_template) == 0) {
  # end process with pop up
  msg <- "'Template_TemperatureReport' file is missing!"
  shinyalert::shinyalert(title = "Report"
                         , text = msg
                         , type = "error"
                         , closeOnEsc = TRUE
                         , closeOnClickOutside = TRUE)
  shiny::validate(msg)
}## IF ~ length(fn_template) == 0

if (length(fn_template) > 1) {
  # end process with pop up
  msg <- "'Template_TemperatureReport' found more than once!"
  shinyalert::shinyalert(title = "Report"
                         , text = msg
                         , type = "error"
                         , closeOnEsc = TRUE
                         , closeOnClickOutside = TRUE)
  shiny::validate(msg)
}## IF ~ length(fn_template) > 1

# List all files
fn_all <- list.files(path = "results", full.names = TRUE, recursive = TRUE)
len_fn_all <- length(fn_all)

# Read Template
# read template file
path_template <- file.path("results", fn_template)
n_skip <- 3

### Data, Summary, Header ----
sh_template <- "summary_header"
df_template_summary_header <- readxl::read_excel(path = path_template
                                                 , sheet = sh_template
                                                 , skip = n_skip)

### Data, Summary, Wide ----
sh_template <- "summary_wide"
df_template_summary_wide <- readxl::read_excel(path = path_template
                                               , sheet = sh_template
                                               , skip = n_skip)

### Data, Top Indicator ----
sh_template <- "topindicator"
df_template_topindicator <- readxl::read_excel(path = path_template
                                               , sheet = sh_template
                                               , skip = n_skip)

### Data, Samples ----
sh_template <- "samples"
df_template_samples <- readxl::read_excel(path = path_template
                                          , sheet = sh_template
                                          , skip = n_skip)

### Data, Flags ----
sh_template <- "flags"
df_template_flags <- readxl::read_excel(path = path_template
                                        , sheet = sh_template
                                        , skip = n_skip)

### Data, Site ----
sh_template <- "site"
df_template_site <- readxl::read_excel(path = path_template
                                       , sheet = sh_template
                                       , skip = n_skip)

### Data, Taxa Trans ----
sh_template <- "taxatrans"
df_template_taxatrans <- readxl::read_excel(path = path_template
                                            , sheet = sh_template
                                            , skip = n_skip)

# template file names
df_template_all <- dplyr::bind_rows(df_template_summary_header
                                         , df_template_summary_wide
                                         , df_template_topindicator
                                         , df_template_samples
                                         , df_template_flags
                                         , df_template_site
                                         , df_template_taxatrans
                                         , .id = "id")
df_template_sourcefiles <- unique(df_template_all[, c("inclusion", "source folder", "source file (or suffix)"), TRUE])
df_template_sourcefiles[, c("Exact", "CSV")] <- NA_integer_

# check for each as CSV and Exact

for (i in seq_len(nrow(df_template_sourcefiles))) {
  
  df_template_sourcefiles[i, "Exact"] <- sum(grepl(pattern = df_template_sourcefiles[i, "source file (or suffix)"], fn_all))
  
  df_template_sourcefiles[i, "CSV"] <- sum(grepl(pattern = paste0(df_template_sourcefiles[i, "source file (or suffix)"], "\\.csv$"), fn_all))
  
}## FOR ~ i

df_template_sourcefiles[, "Present"] <- df_template_sourcefiles[, "Exact"] + df_template_sourcefiles[, "CSV"]

sourcefiles_missing <- dplyr::filter(df_template_sourcefiles, inclusion == "required" & (Present == 0 | is.na(Present)))
  
if (nrow(sourcefiles_missing) > 0) {
  # end process with pop up
  msg <- paste0("Template Source Files missing!\n"
               , paste(unique(sourcefiles_missing$`source file (or suffix)`)
                       , collapse = "\n" )
  )
  shinyalert::shinyalert(title = "Report"
                         , text = msg
                         , type = "error"
                         , closeOnEsc = TRUE
                         , closeOnClickOutside = TRUE)
  shiny::validate(msg)
}## IF ~ nrow(sourcefiles_missing) > 0


## Gather and Test Inputs----
# template file
# then files named in template
# not "other" tab but all others
# cols D (inclusion), E (folder), F (name or suffix)



## Data ----
### Assemble data for each tab of the report

# read template file

### Data, Summary, Header ----
df_report_summary_header <- NA

### Data, Summary, Wide ----
df_report_summary_wide <- NA

### Data, Top Indicator ----
df_report_topindicator <- NA

### Data, Samples ----
df_report_samples <- NA

### Data, Flags ----
df_report_flags <- NA

### Data, Site ----
df_report_site <- NA

### Data, Taxa Trans ----
df_report_taxatrans <- NA


## Excel ----

### Excel, Summary, Header ----

### Excel, Summary, Wide ----

### Excel, Top Indicator ----

### Excel, Samples ----

### Excel, Flags ----

### Excel, Site ----

### Excel, Taxa Trans ----