
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Artefact orientations and site formation processes at Madjebebe, northern Australia

This repository contains the data and code for our paper:

> Theodore Thompson, Ben Marwick, Nathan Mitchell, Richard Fullagar,
> Lynley Wallis, Mike Smith, & Chris Clarkson (nd). *Artefact
> orientations and site formation processes at Madjebebe, northern
> Australia*. <https://doi.org/xxx/xxx>

### How to cite

Please cite this compendium as:

> Theodore Thompson, Ben Marwick, Nathan Mitchell, Richard Fullagar,
> Lynley Wallis, Mike Smith, & Chris Clarkson (2021). *Compendium of R
> code and data for Artefact orientations and site formation processes
> at Madjebebe, northern Australia*. Accessed 28 Jun 2021. Online at
> <https://doi.org/10.17605/OSF.IO/7B5QD>

## Contents

The **analysis** directory contains:

-   [:file\_folder: paper](/analysis/paper): R Markdown source document
    for manuscript. Includes code to reproduce the figures and tables
    generated by the analysis. It also has a rendered version,
    `paper.docx`, suitable for reading (the code is replaced by figures
    and tables in this file)
-   [:file\_folder: data](/analysis/data): Data used in the analysis.
-   [:file\_folder: figures](/analysis/figures): Plots and other
    illustrations

## How to run in your broswer or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping: - open the `.Rproj`
file in RStudio - run `renv::restore()` to ensure you have the packages
this analysis depends on (this will install any you don’t have) -
finally, open `analysis/paper/paper.Rmd` and knit to produce the
`paper.docx`, or run `rmarkdown::render("analysis/paper/paper.Rmd")` in
the R console, or run the code in the script files.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
