% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_app.R
\name{make_app}
\alias{make_app}
\title{Make a shiny app}
\usage{
make_app(
  obj,
  scConf,
  gex.assay = NA,
  gex.slot = c("data", "scale.data", "counts"),
  gene.mapping = FALSE,
  shiny.title = "scRNA-seq shiny app",
  shiny.dir = "shinyApp/",
  shiny.prefix = "sc1",
  enableSubset = TRUE,
  defPtSiz = 1.25,
  default.gene1 = NA,
  default.gene2 = NA,
  default.multigene = NA,
  default.dimred = NA,
  tabs = c("civge", "civci", "gevge", "gem", "gec", "vio", "pro", "hea", "about"),
  theme = "flatly",
  font = "Lato",
  ganalytics = NA
)
}
\arguments{
\item{obj}{input single-cell object for Seurat (v3+) / SingleCellExperiment
data or input file path for h5ad / loom files}

\item{scConf}{shinycell config data.table}

\item{gex.assay}{assay in single-cell data object to use for plotting
gene expression, which must match one of the following:
\itemize{
  \item{Seurat objects}: "RNA" or "integrated" assay,
    default is "RNA"
  \item{SCE objects}: "logcounts" or "normcounts" or "counts",
    default is "logcounts"
  \item{h5ad files}: "X" or any assay in "layers",
    default is "X"
  \item{loom files}: "matrix" or any assay in "layers",
    default is "matrix"
}}

\item{gex.slot}{slot in single-cell assay to plot. This is only used
for Seurat objects (v3+). Default is to use the "data" slot}

\item{gene.mapping}{specifies whether to convert human / mouse Ensembl gene
IDs (e.g. ENSG000xxx / ENSMUSG000xxx) into "user-friendly" gene symbols.
Set this to \code{TRUE} if you are using Ensembl gene IDs. Default is
\code{FALSE} which is not to perform any conversion. Alternatively, users
can supply a named vector where \code{names(gene.mapping)} correspond
to the actual gene identifiers in the gene expression matrix and
\code{gene.mapping} correspond to new identifiers to map to}

\item{shiny.title}{title for shiny app}

\item{shiny.dir}{specify directory to create the shiny app in. Default is
to create a new directory named "shinyApp"}

\item{shiny.prefix}{specify file prefix}

\item{enableSubset}{specify whether to enable "Toggle to subset cells"
functionality in the shiny app. Default is to enable this functionality}

\item{defPtSiz}{specify default point size for single cells. For example, a
smaller size can be used if you have many cells in your dataset}

\item{default.gene1}{specify primary default gene to show}

\item{default.gene2}{specify secondary default gene to show}

\item{default.multigene}{character vector specifying the default genes to
show in bubbleplot / heatmap}

\item{default.dimred}{character vector specifying the two default dimension
reductions. Default is to use UMAP if not TSNE embeddings}

\item{tabs}{Vector of tab names to include}

\item{theme}{Bootstrap theme}

\item{font}{Google font for plots}

\item{ganalytics}{Google analytics tracking ID (e.g. "UA-123456789-0")}
}
\value{
directory containing shiny app
}
\description{
Make a shiny app based on the shinycell config data.table and single-cell
data object.
}
\author{
John F. Ouyang

Roy Francis
}
