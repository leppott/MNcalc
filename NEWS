MNcalc-NEWS
================
<Erik.Leppo@tetratech.com>
2024-10-28 14:48:34.108848

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2024-10-28 14:48:34.123558

# MNcalc 0.2.0.9055 (2024-10-28)

- refactor: Update “delt” to “delt_ExclSchool” to avoid confusion
  - Calculations are “delt_ExclSchool” but this wasn’t in the column
    names
- fix: Update DELT modification order of operations, high to low
  - Was \>=2 then \>=4 and \>=4 not getting trigged

# MNcalc 0.2.0.9054 (2024-10-25)

- fix: Update FIBI transformation code
  - Update classes 8 and 9 to 10 and 11
    - Fixed the overall index earlier but not the individual metric
      transformations

# MNcalc 0.2.0.9053 (2024-10-25)

- refactor: Update BioMonTools package
  - FIBI class 1 metric change

# MNcalc 0.2.0.9052 (2024-10-25)

- refactor: Update BioMonTools package
  - new metrics for BIBI

# MNcalc 0.2.0.9051 (2024-10-24)

- refactor: Modify fish IBI final score
  - Round to 2 decimal places (was 0)
  - Adjust final score to zero if DELT modifier makes the score negative

# MNcalc 0.2.0.9050 (2024-10-17)

- fix: Update BCG output BCG_2metval_BCG.csv
  - Single column Tibble causing issues, added TRUE to convert to vector

# MNcalc 0.2.0.9049 (2024-10-17)

- fix: Update index scoring number of metrics, Fish IBI, classes 10 and
  11

# MNcalc 0.2.0.9048 (2024-10-09)

- refactor: Add taxa without GP.RR to file builder task complete
  - Conditional for analysis type, only for BCG_Bugs

# MNcalc 0.2.0.9047 (2024-10-08)

- refactor: Update to BCGcalc package
  - Rules.xlsx
  - BCG.Level.Membership

# MNcalc 0.2.0.9046 (2024-10-07)

- refactor: Update BioMonTools package version
  - Includes change to scoring regime text for pi_TrichNoHydro

# MNcalc 0.2.0.9045 (2024-10-03)

- refactor: Update Shiny app display text in multiple places

# MNcalc 0.2.0.9044 (2024-09-30)

- fix: Update with new BioMonTools version

# MNcalc 0.2.0.9043 (2024-09-30)

- refactor: New version of BioMonTools
  - Edits to MN_IBI_Bugs and MN_IBI_Fish index numeric and narrative
    thresholds

# MNcalc 0.2.0.9042 (2024-09-27)

- refactor: Update text in Shiny Alert for TaxaID

# MNcalc 0.2.0.9041 (2024-09-27)

- refactor: Add ShinyAlert for bad characters in TaxaID

# MNcalc 0.2.0.9040 (2024-09-23)

- fix: Use same transformation for all regions for pi_TrichNoHydro
  metric

# MNcalc 0.2.0.9039 (2024-09-20)

- refactor: Rename Index Name and Index Class in File Builder process

# MNcalc 0.2.0.9038 (2024-09-19)

- refactor: Modify IBI results (bugs and fish)
  - Rename \_IBI_RESULTS.csv to IBI_4results_all.csv
  - Create \_IBI_RESULTS.csv file with less columns (easier to read)
- fix: Update FIBI Index Narrative to include post calculation changes

# MNcalc 0.2.0.9037 (2024-09-19)

- refactor: Added to BCG calc in Shiny QC check for non-consecutive
  Levels
- fix: Update Fish IBI code for DISTANCE_M
  - Did not convert properly to SAMP_LENGTH_M in background
  - Column was being dropped and zero values used instead
  - Affected numpermeter-tol metric
    - Region 6, Northern Headwater
    - Region 7, Low Gradient

# MNcalc 0.2.0.9036 (2024-08-29)

- docs: Add flextable to DESCRIPTION

# MNcalc 0.2.0.9035 (2024-08-29)

- refactor: Update 2 tables in FB_TaxaTrans_1About from knitr to
  flextable

# MNcalc 0.2.0.9034 (2024-07-29)

- fix: Removed MetricNames.xlsx and MetricScoring.xlsx copies in output.

# MNcalc 0.2.0.9033 (2024-07-29)

- refactor: Updated language in multiple UI elements per stylistic
  edits.

# MNcalc 0.2.0.9032 (2024-07-25)

- refactor: IBI slim down columns to only those for IBI for \_RESULTS
  file

# MNcalc 0.2.0.9031 (2024-07-25)

- fix: BCG fish check for DRAINSQMI for small/large not in if statement
  - Was applying to fish and bugs causing bugs to crash

# MNcalc 0.2.0.9030 (2024-07-25)

- refactor: Remove gp_rr selection box
  - Comment out from tab_filebuilder_taxatrans
  - Comment out UI from server.R
  - Uncomment ShinyAlert for gprr
- refactor: Replace logic for identifying GPRR from cols2keep

# MNcalc 0.2.0.9029 (2024-07-25)

- fix: Enabled taxatrans_user_col_gprr in tab_filebuilder_taxatrans.R

# MNcalc 0.2.0.9028 (2024-07-24)

- refactor: Rework BCG Fish calculation for 10a and 10b
  - Reclass 10 based on DRAINSQMI
  - Undo reclass when save results

# MNcalc 0.2.0.9027 (2024-07-24)

- refactor: Removed the following files from Calculation output
  - MetricFlags.xlsx
  - MetricNames.xlsx
  - MetricScoring.xlsx
  - Rules.xlsx

# MNcalc 0.2.0.9026 (2024-07-24)

- refactor: Added QC check for File Builder required fields

# MNcalc 0.2.0.9025 (2024-07-24)

- docs: Update NEWS for recent commits
- docs: Update version number in DESCRIPTION and global.R

# MNcalc 0.2.0.9024 (2024-07-24)

- refactor: Added Instructions tab

# MNcalc 0.2.0.9023 (2024-07-24)

- refactor: Updates to various UI components per Jen’s requests

# MNcalc 0.2.0.9022 (2024-07-24)

- refactor: Modify Shiny file builder, taxa translate to combine
  duplicate taxa

# MNcalc 0.2.0.9021 (2024-07-24)

- refactor: Comment out subtabs in db_main_sb.R
  - Prepare Data Introduction
  - Outside the App

# MNcalc 0.2.0.9020 (2024-07-23)

- docs: Update NEWS
- refactor: Update version number

# MNcalc 0.2.0.9019 (2024-07-23)

- refactor: Add “\_ORIG” metrics for IBI calculations in server.R
  - bugs and fish for all classes
  - Add to files
    - IBI_2metval_all.csv
    - IBI_2metval_IBI.csv
    - \_IBI_RESULTS.csv

# MNcalc 0.2.0.9018 (2024-07-23)

- refactor: Updates to About and References UI tabs

# MNcalc 0.2.0.9017 (2024-07-18)

# MNcalc 0.2.0.9016 (2024-07-18)

- refactor: Clean up
  - Remove unused code from Server.R
  - Remove unused tabs
  - Remove unused RMD files
- fix: Update ouput for BCG calculation in Server.R
  - BCG_2metvall_all.csv to BCG_2metval_all.csv
- refactor: Update calculation and file builder code for any issues
- docs: Add pull request updates to NEWS

# MNcalc 0.2.0.9015 (2024-07-16)

- refactor: Minor update to styling of a table in the UI

# MNcalc 0.2.0.9014 (2024-07-15)

- refactor: Reformatted UI for the file builder section
  - New language, tables, and figures provided by Jen

# MNcalc 0.2.0.9013 (2024-07-15)

- refactor: Updated UI for calculation tabs with new language, tables,
  and figures

# MNcalc 0.2.0.9012 (2024-07-12)

- refactor: Updated about tab, reference tab, and added troubleshooting
  tab

# MNcalc 0.2.0.9011 (2024-07-05)

- refactor: Update naming scheme for result files
  - BCG
  - IBI
  - taxa translate
- refactor: taxa translate file names prefix with BCG/IBI but not
  Bugs/Fish
  - Already in subfolder with assemblage name

# MNcalc 0.2.0.9010 (2024-07-05)

- refactor: Update TaxaTranslator result folders to match calculations
  folders
  - TaxaTrans was lower case and calc was first letter cap for
    assemblage

# MNcalc 0.2.0.9009 (2024-07-05)

- refactor: Update URL references in Server.R for support files
- refactor: Update TaxaTranslator result folder (typo for bugs ibi)

# MNcalc 0.2.0.9008 (2024-07-05)

- refactor: Update Global.R for new location of support files

# MNcalc 0.2.0.9007 (2024-07-03)

- refactor: Update links in Reference to GitHub raw
  - Will download instead of open

# MNcalc 0.2.0.9006 (2024-07-03)

- refactor: Update links in References to pull from GitHub so open in
  new tab
- refactor: Change names of Reference PDFs
- refactor: Update zip file with all reference PDFs

# MNcalc 0.2.0.9005 (2024-07-02)

- refactor: Update About page

# MNcalc 0.2.0.9004 (2024-06-13)

- refactor: Update file builder taxa translator
  - Add Index_Name as a required field
  - Update pop ups for Index_Name, Index_Class, and GP/RR
  - Ensure Index_Name, Index_Class, and GP/RR are carried through to
    results, Issue \#8

# MNcalc 0.2.0.9003 (2024-06-13)

- refactor: Remove text about additional fields on file builder, Issue
  \#9

# MNcalc 0.2.0.9002 (2024-06-13)

- refactor: Update file builder pop up to include number of mismatches,
  Issue \#13

# MNcalc 0.2.0.9001 (2024-06-13)

- fix: Update complete pop up for misspelling, Issue \#12

# MNcalc 0.2.0 (2024-06-12)

- refactor: Update version number for deliverable

# MNcalc 0.1.0.9019 (2024-06-12)

- refactor: Add conditions for no matches for taxa translator

# MNcalc 0.1.0.9018 (2024-06-12)

- refactor: Add pop up to ensure BCG community selection matches data,
  Issue#6
- refactor: Uncomment the BIBI metric input transformations

# MNcalc 0.1.0.9017 (2024-06-12)

- feature: Add .simpleCaps function from `toupper` help
  - Shiny app, scripts; helper_functions.R
- refactor: Add pop up to ensure IBI community selection matches data
- refactor: Rename RedLake to MN, Issue \#4
  - About
  - Apps directory
  - UI.R app title
  - NEWS
  - DESCRIPTION
  - README
- refactor: Ensure use RedLake not RedLakes

# MNcalc 0.1.0.9016 (2024-06-10)

- refactor: Update Fish IBI post calculation adjustments

# MNcalc 0.1.0.9015 (2024-06-06)

- refactor: Update Fish IBI post Index calculation adjustments
  - Low End Scoring
  - percent DELT adjustment

# MNcalc 0.1.0.9014 (2024-06-04)

- fix: Update file to avoid readxl import message
  - data/BCGcalc_Shiny_map_inputs_20230828.xlsx

# MNcalc 0.1.0.9013 (2024-06-04)

- fix: Change IBI Calc output tab from “Calc_BCG_Ouput” to
  “Calc_IBI_Output”
- refactor: New PC so more files showing as changed

# MNcalc 0.1.0.9012 (2024-01-10)

- feature: Add IBI calculation to Shiny app

# MNcalc 0.1.0.9011 (2023-12-21)

- refactor: Calculation community buttons to default to NA
  - calc_BCG
  - calc_thermalfuzzy
  - calc_thermalmetrics

# MNcalc 0.1.0.9110 (2023-12-14)

- refactor: Modify results subfolder to include community
  - Taxa Translate
  - Calculate BCG (and later IBI)

# MNcalc 0.1.0.9009 (2023-12-14)

- refactor: Auto add INDEX_NAME if missing for BCG calculation

# MNcalc 0.1.0.9008 (2023-12-14)

- refactor: Update on-screen text from OR-WA to MN
- refactor: Add MN BCG and IBI documentation
- refactor: Remove some OR-WA specific files

# MNcalc 0.1.0.9007 (2023-12-13)

- refactor: Update language on screen for calculations
  - BCG to BCG and IBI
- fix: Update output for calculations results when no flags
- refactor: Add files to results reference file
  - MetricNames.xlsx
  - MetricScoring.xlsx
  - Rules.xlsx
  - MetricFlags.xlsx
  - IndexClass.xlsx
- refactor: Update code for downloading reference files from GitHub
  - global.R
  - server.R
- fix: Update taxa translate mutate case_when
  - fails if columns not the same class

# MNcalc 0.1.0.9006 (2023-12-07)

- refactor: modify global packages order

# MNcalc 0.1.0.9005 (2023-12-07)

- refactor: Add complete pop up to merge files

# MNcalc 0.1.0.9004 (2023-12-07)

- fix: Update app BCG calculation for cases where have no flags
  - Same as BCGcalc v2.0.0.9126 commit

# MNcalc 0.1.0.9003 (2023-12-05)

- refactor: Update Shiny to work with Red Lakes data
  - Rename App
  - Update import routine for multiple BCG_ATTR columns
  - Update taxa translate for Red Lakes (more user fields)
  - Comment out sections of app not using for Red Lakes
  - Update ReadMe in app
  - Add routine for generating ecoregion
  - Add information to BCG calculation when not the correct region
    - Include output file
  - Remove MTTI model (OR-WA specific)

# MNcalc 0.1.0.9002 (2023-12-05)

- feature: Add Shiny app files from BCGcalc, v2.0.0.9112 (2023-12-04)

# MNcalc 0.1.0.9001 (2023-12-05)

- docs: Add base package files
  - DESCRIPTION
  - NEWS
  - README
  - app function

# MNcalc 0.1.0 (2023-12-05)

- Created GitHub repository
