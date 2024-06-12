MNcalc-README
================
<Erik.Leppo@tetratech.com>
2024-06-12 14:50:20.32147

<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Last Update: 2024-06-12 14:50:20.343995

# MNcalc

Biological Condition Gradient (BCG) and Index of Biotic Integrity (IBI)
calculator Shiny app. For the Red Lake Tribe in Minnesota. Calculates
the MN state BCG and IBI in a Shiny app using the BioMonTools and
BCGcalc R packages.

# Badges

to come later

# Installation

To install the current version use the code below to install from
GitHub. The use of “force = TRUE” ensures the package is installed even
if already present. If the package `remotes` is missing the code below
will install it.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("leppott/MNcalc", force=TRUE)
```

# Purpose

To aid users in the calculation of the MN BCG and IBI (bugs and fish).

# Usage

After installing the package can launch the app using the code below.

``` r
app("MNcalc")
```

# Shiny

The Shiny app code is included in the package but is also on the web.

<https://tetratech-wtr-wne.shinyapps.io/MNcalc/>
