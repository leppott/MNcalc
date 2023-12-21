RedLakesCalc-NEWS
================
<Erik.Leppo@tetratech.com>
2023-12-21 13:27:19.980681

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2023-12-21 13:27:20.010984

# RedLakesCalc 0.1.0.9111 (2023-12-21)

- refactor: Calculation community buttons to default to NA
  - calc_BCG
  - calc_thermalfuzzy
  - calc_thermalmetrics

# RedLakesCalc 0.1.0.9110 (2023-12-14)

- refactor: Modify results subfolder to include community
  - Taxa Translate
  - Calculate BCG (and later IBI)

# RedLakesCalc 0.1.0.9009 (2023-12-14)

- refactor: Auto add INDEX_NAME if missing for BCG calculation

# RedLakesCalc 0.1.0.9008 (2023-12-14)

- refactor: Update on-screen text from OR-WA to MN
- refactor: Add MN BCG and IBI documentation
- refactor: Remove some OR-WA specific files

# RedLakesCalc 0.1.0.9007 (2023-12-13)

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

# RedLakesCalc 0.1.0.9006 (2023-12-07)

- refactor: modify global packages order

# RedLakesCalc 0.1.0.9005 (2023-12-07)

- refactor: Add complete pop up to merge files

# RedLakesCalc 0.1.0.9004 (2023-12-07)

- fix: Update app BCG calculation for cases where have no flags
  - Same as BCGcalc v2.0.0.9126 commit

# RedLakesCalc 0.1.0.9003 (2023-12-05)

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

# RedLakesCalc 0.1.0.9002 (2023-12-05)

- feature: Add Shiny app files from BCGcalc, v2.0.0.9112 (2023-12-04)

# RedLakesCalc 0.1.0.9001 (2023-12-05)

- docs: Add base package files
  - DESCRIPTION
  - NEWS
  - README
  - app function

# RedLakesCalc 0.1.0 (2023-12-05)

- Created GitHub repository
