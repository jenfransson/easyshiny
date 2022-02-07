# easyshiny package

`easyshiny` is a R package that allows users to create interactive Shiny-based 
web applications to quickly visualise single-cell data.

# Installation

First, users can run the following code to check if the packages required by 
`easyshiny` exist and install them if required:

``` r
reqPkg = c("data.table", "Matrix", "hdf5r", "reticulate", "ggplot2", 
           "gridExtra", "glue", "readr", "RColorBrewer", "R.utils", "remotes", "Seurat")
newPkg = reqPkg[!(reqPkg %in% installed.packages()[,"Package"])]
if(length(newPkg)){install.packages(newPkg)}

# If you are using h5ad file as input, run the code below as well
# reticulate::py_install("anndata")
```

Furthermore, on the system where the Shiny app will be deployed, users can run 
the following code to check if the packages required by the Shiny app exist 
and install them if required:

``` r
reqPkg = c("shiny", "shinyhelper", "data.table", "Matrix", "DT", "hdf5r", 
           "reticulate", "ggplot2", "gridExtra", "magrittr", "ggdendro")
newPkg = reqPkg[!(reqPkg %in% installed.packages()[,"Package"])]
if(length(newPkg)){install.packages(newPkg)}
```

`easyshiny` can then be installed from GitHub as follows:

``` r
remotes::install_github("royfrancis/easyshiny")
```

To launch the app using information from a Seurat file, run as shown below assuming `obj` is a Seurat object.

```r
library(ShinyCell)
scConf = createConfig(obj)
makeShinyApp(obj, scConf, gex.assay="RNA", gex.slot="data", gene.mapping = FALSE, shiny.title = "My Awesome App", shiny.dir="app")
shiny::runApp("app")
```

# Usage & more information

This project is forked from [ShinyCell](https://github.com/SGDDNB/ShinyCell). Go there to get more information on how to run, citation etc.
