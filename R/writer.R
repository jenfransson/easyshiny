#' Write code for loading libraries
#' @param lib vector of libraries
#' @rdname wrLib
#' @export wrLib
#'
wrLib <- function(lib) {
  oup <- ""
  for (iLib in lib) {
    oup <- paste0(oup, "library(", iLib, ")\n")
  }
  glue::glue(paste0(oup,"\n"))
}

#' Write code for font
#' @param font Google font name
#' @rdname wrFont
#' @export wrFont
#'
wrFont <- function(font = "Lato") {
  paste0(
'
sysfonts::font_add_google(name = "',font,'", family = "',font,'")
showtext::showtext_auto()
'
  )
}

#' Write code for loading objects for server.R
#' @param prefix file prefix
#' @rdname wrSVload
#' @export wrSVload
#'
wrSVload <- function(prefix) {
glue::glue('

{prefix}conf = readRDS("{prefix}conf.rds")
{prefix}def  = readRDS("{prefix}def.rds")
{prefix}gene = readRDS("{prefix}gene.rds")
{prefix}meta = readRDS("{prefix}meta.rds")

')
}

#' Write code for fixed portion of server.R
#' @rdname wrSVfix
#' @export wrSVfix
#'
wrSVfix <- function() {
  glue::glue('

### Useful stuff ----

# Colour palette
cList <- list(
  c(
    "grey85", "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84",
    "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000"
  ),
  c(
    "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
    "#FEE090", "#FDAE61", "#F46D43", "#D73027"
  )[c(1, 1:9, 9)],
  c(
    "#FDE725", "#AADC32", "#5DC863", "#27AD81", "#21908C",
    "#2C728E", "#3B528B", "#472D7B", "#440154"
  )
)
names(cList) <- c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple")

# Panel sizes
pList <- c("400px", "600px", "800px")
names(pList) <- c("Small", "Medium", "Large")
pList2 <- c("500px", "700px", "900px")
names(pList2) <- c("Small", "Medium", "Large")
pList3 <- c("600px", "800px", "1000px")
names(pList3) <- c("Small", "Medium", "Large")
# baseplot font size
sList <- c(18, 24, 30)
names(sList) <- c("Small", "Medium", "Large")
# ggrepel font size
lList <- c(5, 6, 7)
names(lList) <- c("Small", "Medium", "Large")

# Function to extract legend
g_legend <- function(a.gplot) {{
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}}

# Plot theme
sctheme <- function(base_size = 24, XYval = TRUE, Xang = 0, XjusH = 0.5) {{
  oupTheme <- theme(
    text = element_text(size = base_size, family = "Lato"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = "white"),
    axis.line = element_line(colour = "grey20"),
    axis.ticks = element_line(colour = "grey20"),
    axis.title = element_text(colour = "grey20"),
    axis.text = element_text(size = base_size, colour = "grey20"),
    axis.text.x = element_text(angle = Xang, hjust = XjusH),
    legend.position = "bottom",
    legend.key = element_rect(colour = NA, fill = NA)
  )
  if (!XYval) {{
    oupTheme <- oupTheme +
      theme(
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
      )
  }}
  
  return(oupTheme)
}}

### Plotting functions ----

# @description Plot cell information on dimred
# @param inpConf (data.frame) Configuration table
# @param inpMeta (data.frame) Metadata table
# @param inpdrX (Character) X axis variable for DR
# @param inpdrY (Character) Y axis variable for DR
# @param inp1 (Character) Gene name to use
# @param inpsub1 (Character) Name of metadata column for subsetting
# @param inpsub2 (Character/Vector) Levels under metadata column for subsetting
# @param inpsiz
# @param inpcol
# @param inpord
# @param inpfsz (Character) Font size
# @param inpasp
# @param inptxt
# @param inplab
#
scDRcell <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt, inplab) {{
  
  if (is.null(inpsub1)) {{ inpsub1 <- inpConf$UI[1] }}

  # Prepare ggData
  ggData <- inpMeta[, c(
    inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
    inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID
  ),
  with = FALSE
  ]
  colnames(ggData) <- c("X", "Y", "val", "sub")
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  bgCells <- FALSE
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  }}

  if (inpord == "Max-1st") {{
    ggData <- ggData[order(val)]
  }} else if (inpord == "Min-1st") {{
    ggData <- ggData[order(-val)]
  }} else if (inpord == "Random") {{
    ggData <- ggData[sample(nrow(ggData))]
  }}

  # Do factoring if required
  if (!is.na(inpConf[UI == inp1]$fCL)) {{ ggCol <- strsplit(inpConf[UI == inp1]$fCL, "\\\\|")[[1]]
    names(ggCol) <- levels(ggData$val)
    ggLvl <- levels(ggData$val)[levels(ggData$val) %in% unique(ggData$val)]
    ggData$val <- factor(ggData$val, levels = ggLvl)
    ggCol <- ggCol[ggLvl] }}

  # Actual ggplot
  ggOut <- ggplot(ggData, aes(X, Y, color = val))
  if (bgCells) {{ ggOut <- ggOut +
    geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 20) }}
  ggOut <- ggOut +
    geom_point(size = inpsiz, shape = 20) +
    xlab(inpdrX) +
    ylab(inpdrY) +
    sctheme(base_size = sList[inpfsz], XYval = inptxt)

  if (is.na(inpConf[UI == inp1]$fCL)) {{
    ggOut <- ggOut +
    scale_color_gradientn("", colours = cList[[inpcol]]) +
    guides(color = guide_colorbar(barwidth = 20)) 
  }} else {{
    sListX <- min(nchar(paste0(levels(ggData$val), collapse = "")), 200)
    sListX <- 0.75 * (sList - (1.5 * floor(sListX / 50)))
    ggOut <- ggOut + scale_color_manual("", values = ggCol) +
      guides(color = guide_legend(
        override.aes = list(size = 5),
        nrow = inpConf[UI == inp1]$fRow
      )) +
      theme(legend.text = element_text(size = sListX[inpfsz]))

    if (inplab) {{ ggData3 <- ggData[, .(X = mean(X), Y = mean(Y)), by = "val"]
      lListX <- min(nchar(paste0(ggData3$val, collapse = "")), 200)
      lListX <- lList - (0.25 * floor(lListX / 50))
      ggOut <- ggOut +
        geom_text_repel(
          data = ggData3, aes(X, Y, label = val),
          color = "grey10", bg.color = "grey95", bg.r = 0.15,
          size = lListX[inpfsz], seed = 42
        ) }} 
    }}

  if (inpasp == "Square") {{ ggOut <- ggOut + coord_fixed(ratio = rat) }} else if (inpasp == "Fixed") {{ ggOut <- ggOut + coord_fixed() }}

  if(is.numeric(inpMeta[[inp1]])) ggOut <- ggOut + guides(color = guide_colorbar(barwidth = 25))
  return(ggOut)
}}

# @description Cell number, stats data
# @param inpConf (data.frame) Configuration table
# @param inpMeta (data.frame) Metadata table
# @param inp1 (Character) Name of metadata column for coloring points
# @param inp2 (Character) Gene name to use
# @param inpsub1 (Character) Name of metadata column for subsetting
# @param inpsub2 (Character/Vector) Levels under metadata column for subsetting
# @param inpH5 (Character) Name/path to h5 object
# @param inpGene (Character) Gene to use
# @param inpsplt (Character) Whether to split table by Quartile or Decile
#
scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, inpH5, inpGene, inpsplt) {{
  
  if (is.null(inpsub1)) {{ inpsub1 <- inpConf$UI[1] }}

  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),
    with = FALSE
  ]

  colnames(ggData) <- c("group", "sub")
  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val2 <- h5data$read(args = list(inpGene[inp2], quote(expr = T)))
  ggData[val2 < 0]$val2 <- 0
  h5file$close_all()
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{ ggData <- ggData[sub %in% inpsub2] }}

  # Split inp1 if necessary
  if (is.na(inpConf[UI == inp1]$fCL)) {{ if (inpsplt == "Quartile") {{ nBk <- 4 }}
    if (inpsplt == "Decile") {{ nBk <- 10 }}
    ggData$group <- cut(ggData$group, breaks = nBk) }}

  # Actual data.table
  ggData$express <- FALSE
  ggData[val2 > 0]$express <- TRUE
  ggData1 <- ggData[express == TRUE, .(nExpress = .N), by = "group"]
  ggData <- ggData[, .(nCells = .N), by = "group"]
  ggData <- ggData1[ggData, on = "group"]
  ggData <- ggData[, c("group", "nCells", "nExpress"), with = FALSE]
  ggData[is.na(nExpress)]$nExpress <- 0
  ggData$pctExpress <- 100 * ggData$nExpress / ggData$nCells
  ggData <- ggData[order(group)]
  colnames(ggData)[3] <- paste0(colnames(ggData)[3], "_", inp2)

  return(ggData)
}}

# @description Plot gene expression on dimred
# @param inpConf (data.frame) Configuration table
# @param inpMeta (data.frame) Metadata table
# @param inpdrX (Character) X axis variable for DR
# @param inpdrY (Character) Y axis variable for DR
# @param inp1 (Character) Gene name to use
# @param inpsub1 (Character) Name of metadata column for subsetting
# @param inpsub2 (Character/Vector) Levels under metadata column for subsetting
# @param inpH5
# @param inpGene
# @param inpsiz
# @param inpcol
# @param inpfsz
# @param inppasp
# @param inptxt
#
scDRgene <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, inpH5, inpGene, inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt) {{
  
  if (is.null(inpsub1)) {{ inpsub1 <- inpConf$UI[1] }}

  # Prepare ggData
  ggData <- inpMeta[, c(
    inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
    inpConf[UI == inpsub1]$ID
  ),
  with = FALSE
  ]
  colnames(ggData) <- c("X", "Y", "sub")
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))

  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val <- h5data$read(args = list(inpGene[inp1], quote(expr = )))
  ggData[val < 0]$val <- 0
  h5file$close_all()
  bgCells <- FALSE

  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{ bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2] }}

  if (inpord == "Max-1st") {{ ggData <- ggData[order(val)] }} else if (inpord == "Min-1st") {{ ggData <- ggData[order(-val)] }} else if (inpord == "Random") {{ ggData <- ggData[sample(nrow(ggData))] }}

  # Actual ggplot
  ggOut <- ggplot(ggData, aes(X, Y, color = val))
  if (bgCells) {{ ggOut <- ggOut +
    geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 20) }}

  ggOut <- ggOut +
    geom_point(size = inpsiz, shape = 20) + xlab(inpdrX) + ylab(inpdrY) +
    sctheme(base_size = sList[inpfsz], XYval = inptxt) +
    scale_color_gradientn(inp1, colours = cList[[inpcol]]) +
    guides(color = guide_colorbar(barwidth = 20))

  if (inpasp == "Square") {{ ggOut <- ggOut + coord_fixed(ratio = rat) }} else if (inpasp == "Fixed") {{ ggOut <- ggOut + coord_fixed() }}

  return(ggOut)
}}

# Plot gene coexpression on dimred
bilinear <- function(x, y, xy, Q11, Q21, Q12, Q22) {{
  oup <- (xy - x) * (xy - y) * Q11 + x * (xy - y) * Q21 + (xy - x) * y * Q12 + x * y * Q22
  oup <- oup / (xy * xy)
  return(oup)
}}

scDRcoex <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inp2, inpsub1,
                     inpsub2, inpH5, inpGene, inpsiz, inpcol, inpord, inpfsz,
                     inpasp, inptxt) {{
  
  if (is.null(inpsub1)) {{ inpsub1 <- inpConf$UI[1] }}

  # Prepare ggData
  ggData <- inpMeta[, c(
    inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
    inpConf[UI == inpsub1]$ID
  ),
  with = FALSE
  ]
  colnames(ggData) <- c("X", "Y", "sub")
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))

  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val1 <- h5data$read(args = list(inpGene[inp1], quote(expr = )))
  ggData[val1 < 0]$val1 <- 0
  ggData$val2 <- h5data$read(args = list(inpGene[inp2], quote(expr = )))
  ggData[val2 < 0]$val2 <- 0
  h5file$close_all()
  bgCells <- FALSE

  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  }}

  # Generate coex color palette
  cInp <- strsplit(inpcol, "; ")[[1]]
  if (cInp[1] == "Red (Gene1)") {{ c10 <- c(255, 0, 0) }} else if (cInp[1] == "Orange (Gene1)") {{ c10 <- c(255, 140, 0) }} else {{ c10 <- c(0, 255, 0) }}

  if (cInp[2] == "Green (Gene2)") {{ c01 <- c(0, 255, 0) }} else {{ c01 <- c(0, 0, 255) }}

  c00 <- c(217, 217, 217)
  c11 <- c10 + c01
  nGrid <- 16
  nPad <- 2
  nTot <- nGrid + nPad * 2
  gg <- data.table(v1 = rep(0:nTot, nTot + 1), v2 = sort(rep(0:nTot, nTot + 1)))
  gg$vv1 <- gg$v1 - nPad
  gg[vv1 < 0]$vv1 <- 0
  gg[vv1 > nGrid]$vv1 <- nGrid
  gg$vv2 <- gg$v2 - nPad
  gg[vv2 < 0]$vv2 <- 0
  gg[vv2 > nGrid]$vv2 <- nGrid
  gg$cR <- bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
  gg$cG <- bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
  gg$cB <- bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
  gg$cMix <- rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
  gg <- gg[, c("v1", "v2", "cMix")]

  # Map colours
  ggData$v1 <- round(nTot * ggData$val1 / max(ggData$val1))
  ggData$v2 <- round(nTot * ggData$val2 / max(ggData$val2))
  ggData$v0 <- ggData$v1 + ggData$v2
  ggData <- gg[ggData, on = c("v1", "v2")]

  if (inpord == "Max-1st") {{ ggData <- ggData[order(v0)] }} else if (inpord == "Min-1st") {{ ggData <- ggData[order(-v0)] }} else if (inpord == "Random") {{ ggData <- ggData[sample(nrow(ggData))] }}

  # Actual ggplot
  ggOut <- ggplot(ggData, aes(X, Y))
  if (bgCells) {{ ggOut <- ggOut +
    geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 20) }}
  ggOut <- ggOut +
    geom_point(size = inpsiz, shape = 20, color = ggData$cMix) +
    xlab(inpdrX) + ylab(inpdrY) +
    sctheme(base_size = sList[inpfsz], XYval = inptxt) +
    scale_color_gradientn(inp1, colours = cList[[1]]) +
    guides(color = guide_colorbar(barwidth = 20))

  if (inpasp == "Square") {{ ggOut <- ggOut + coord_fixed(ratio = rat) }} else if (inpasp == "Fixed") {{ ggOut <- ggOut + coord_fixed() }}
  return(ggOut) }}

scDRcoexLeg <- function(inp1, inp2, inpcol, inpfsz) {{
  
  # Generate coex color palette
  cInp <- strsplit(inpcol, "; ")[[1]]
  if (cInp[1] == "Red (Gene1)") {{ c10 <- c(255, 0, 0) }} else if (cInp[1] == "Orange (Gene1)") {{ c10 <- c(255, 140, 0) }} else {{ c10 <- c(0, 255, 0) }}

  if (cInp[2] == "Green (Gene2)") {{ c01 <- c(0, 255, 0) }} else {{ c01 <- c(0, 0, 255) }}

  c00 <- c(217, 217, 217)
  c11 <- c10 + c01
  nGrid <- 16
  nPad <- 2
  nTot <- nGrid + nPad * 2
  gg <- data.table(v1 = rep(0:nTot, nTot + 1), v2 = sort(rep(0:nTot, nTot + 1)))
  gg$vv1 <- gg$v1 - nPad
  gg[vv1 < 0]$vv1 <- 0
  gg[vv1 > nGrid]$vv1 <- nGrid
  gg$vv2 <- gg$v2 - nPad
  gg[vv2 < 0]$vv2 <- 0
  gg[vv2 > nGrid]$vv2 <- nGrid
  gg$cR <- bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
  gg$cG <- bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
  gg$cB <- bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
  gg$cMix <- rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
  gg <- gg[, c("v1", "v2", "cMix")]

  # Actual ggplot
  ggOut <- ggplot(gg, aes(v1, v2)) +
    geom_tile(fill = gg$cMix) +
    xlab(inp1) +
    ylab(inp2) +
    coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = c(0, nTot), label = c("low", "high")) +
    scale_y_continuous(breaks = c(0, nTot), label = c("low", "high")) +
    sctheme(base_size = sList[inpfsz], XYval = TRUE)
  return(ggOut)
}}

scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, inpH5, inpGene) {{
  
  if (is.null(inpsub1)) {{
  inpsub1 <- inpConf$UI[1]
  }}

  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("sub")
  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val1 <- h5data$read(args = list(inpGene[inp1], quote(expr = )))
  ggData[val1 < 0]$val1 <- 0
  ggData$val2 <- h5data$read(args = list(inpGene[inp2], quote(expr = )))
  ggData[val2 < 0]$val2 <- 0
  h5file$close_all()
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{ ggData <- ggData[sub %in% inpsub2] }}

  # Actual data.table
  ggData$express <- "none"
  ggData[val1 > 0]$express <- inp1
  ggData[val2 > 0]$express <- inp2
  ggData[val1 > 0 & val2 > 0]$express <- "both"
  ggData$express <- factor(ggData$express, levels = unique(c("both", inp1, inp2, "none")))
  ggData <- ggData[, .(nCells = .N), by = "express"]
  ggData$percent <- 100 * ggData$nCells / sum(ggData$nCells)
  ggData <- ggData[order(express)]
  colnames(ggData)[1] <- "expression > 0"
  return(ggData)
}}

# Plot violin / boxplot
scVioBox <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, inpH5, inpGene, inptyp, inppts, inpsiz, inpfsz) {{
  
  if (is.null(inpsub1)) {{ inpsub1 <- inpConf$UI[1] }}

  # Prepare ggData
  ggData <- inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),
    with = FALSE
  ]
  colnames(ggData) <- c("X", "sub")

  # Load in either cell meta or gene expr
  if (inp2 %in% inpConf$UI) {{
    ggData$val <- inpMeta[[inpConf[UI == inp2]$ID]]
  }} else {{
    h5file <- H5File$new(inpH5, mode = "r")
    h5data <- h5file[["grp"]][["data"]]
    ggData$val <- h5data$read(args = list(inpGene[inp2], quote(expr = )))
    ggData[val < 0]$val <- 0
    set.seed(42)
    tmpNoise <- rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000
    ggData$val <- ggData$val + tmpNoise
    h5file$close_all()
  }}

  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{
    ggData <- ggData[sub %in% inpsub2]
  }}

  # Do factoring
  ggCol <- strsplit(inpConf[UI == inp1]$fCL, "\\\\|")[[1]]
  names(ggCol) <- levels(ggData$X)
  ggLvl <- levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)]
  ggData$X <- factor(ggData$X, levels = ggLvl)
  ggCol <- ggCol[ggLvl]

  # Actual ggplot
  if (inptyp == "violin") {{
    ggOut <- ggplot(ggData, aes(X, val, fill = X)) +
    geom_violin(scale = "width")
  }} else {{
    ggOut <- ggplot(ggData, aes(X, val, fill = X)) +
    geom_boxplot()
  }}
  if (inppts) {{ ggOut <- ggOut + geom_jitter(size = inpsiz, shape = 20) }}

  ggOut <- ggOut + xlab(inp1) + ylab(inp2) +
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "none")
  return(ggOut)
  }}

# Plot proportion plot
scProp <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, inptyp, inpflp, inpfsz) {{
  
  if (is.null(inpsub1)) {{ inpsub1 <- inpConf$UI[1] }}

  # Prepare ggData
  ggData <- inpMeta[, c(
    inpConf[UI == inp1]$ID, inpConf[UI == inp2]$ID,
    inpConf[UI == inpsub1]$ID
  ),
  with = FALSE
  ]
  colnames(ggData) <- c("X", "grp", "sub")

  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{ ggData <- ggData[sub %in% inpsub2] }}
  ggData <- ggData[, .(nCells = .N), by = c("X", "grp")]
  ggData <- ggData[, {{ tot <- sum(nCells)
    .SD[, .(
      pctCells = 100 * sum(nCells) / tot,
      nCells = nCells
    ), by = "grp"] }}, by = "X"]

  # Do factoring
  ggCol <- strsplit(inpConf[UI == inp2]$fCL, "\\\\|")[[1]]
  names(ggCol) <- levels(ggData$grp)
  ggLvl <- levels(ggData$grp)[levels(ggData$grp) %in% unique(ggData$grp)]
  ggData$grp <- factor(ggData$grp, levels = ggLvl)
  ggCol <- ggCol[ggLvl]

  # Actual ggplot
  if (inptyp == "Proportion") {{ ggOut <- ggplot(ggData, aes(X, pctCells, fill = grp)) +
    geom_col() +
    ylab("Cell Proportion (%)") }} else {{ ggOut <- ggplot(ggData, aes(X, nCells, fill = grp)) +
    geom_col() +
    ylab("Number of Cells") }}

  if (inpflp) {{ ggOut <- ggOut + coord_flip() }}

  ggOut <- ggOut + xlab(inp1) +
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "right")

  return(ggOut)
  }}

# Get gene list
scGeneList <- function(inp, inpGene) {{
  geneList <- data.table(
  gene = unique(trimws(strsplit(inp, ",|;|\n")[[1]])),
  present = TRUE
  )
  geneList[!gene %in% names(inpGene)]$present <- FALSE
  return(geneList)
}}

# Plot gene expression bubbleplot / heatmap
scBubbHeat <- function(inpConf, inpMeta, inp, inpGrp, inpPlt, inpsub1, inpsub2, inpH5, inpGene, inpScl, inpRow, inpCol, inpcols, inpfsz, save = FALSE) {{
  
  if (is.null(inpsub1)) {{ inpsub1 <- inpConf$UI[1] }}

  # Identify genes that are in our dataset
  geneList <- scGeneList(inp, inpGene)
  geneList <- geneList[present == TRUE]
  shiny::validate(need(nrow(geneList) <= 50, "More than 50 genes to plot! Please reduce the gene list!"))
  shiny::validate(need(nrow(geneList) > 1, "Please input at least 2 genes to plot!"))

  # Prepare ggData
  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData <- data.table()
  for (iGene in geneList$gene) {{ tmp <- inpMeta[, c("sampleID", inpConf[UI == inpsub1]$ID), with = FALSE]
    colnames(tmp) <- c("sampleID", "sub")
    tmp$grpBy <- inpMeta[[inpConf[UI == inpGrp]$ID]]
    tmp$geneName <- iGene
    tmp$val <- h5data$read(args = list(inpGene[iGene], quote(expr = )))
    ggData <- rbindlist(list(ggData, tmp)) }}
  h5file$close_all()
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {{ ggData <- ggData[sub %in% inpsub2] }}
  shiny::validate(need(uniqueN(ggData$grpBy) > 1, "Only 1 group present, unable to plot!"))

  # Aggregate
  ggData$val <- expm1(ggData$val)
  ggData <- ggData[, .(val = mean(val), prop = sum(val > 0) / length(sampleID)),
    by = c("geneName", "grpBy")
  ]
  ggData$val <- log1p(ggData$val)

  # Scale if required
  colRange <- range(ggData$val)
  if (inpScl) {{ ggData[, val := scale(val), keyby = "geneName"]
    colRange <- c(-max(abs(range(ggData$val))), max(abs(range(ggData$val)))) }}

  # hclust row/col if necessary
  ggMat <- dcast.data.table(ggData, geneName ~ grpBy, value.var = "val")
  tmp <- ggMat$geneName
  ggMat <- as.matrix(ggMat[, -1])
  rownames(ggMat) <- tmp
  if (inpRow) {{
    hcRow <- dendro_data(as.dendrogram(hclust(dist(ggMat))))
    ggRow <- ggplot() +
      coord_flip() +
      geom_segment(data = hcRow$segments, aes(x = x, y = y, xend = xend, yend = yend)) +
      scale_y_continuous(
        breaks = rep(0, uniqueN(ggData$grpBy)),
        labels = unique(ggData$grpBy), expand = c(0, 0)
      ) +
      scale_x_continuous(
        breaks = seq_along(hcRow$labels$label),
        labels = hcRow$labels$label, expand = c(0, 0.5)
      ) +
      sctheme(base_size = sList[inpfsz]) +
      theme(
        axis.title = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(color = "white", angle = 45, hjust = 1)
      )
    ggData$geneName <- factor(ggData$geneName, levels = hcRow$labels$label) 
    }} else {{ ggData$geneName <- factor(ggData$geneName, levels = rev(geneList$gene)) }}

  if (inpCol) {{
    hcCol <- dendro_data(as.dendrogram(hclust(dist(t(ggMat)))))
    ggCol <- ggplot() +
      geom_segment(data = hcCol$segments, aes(x = x, y = y, xend = xend, yend = yend)) +
      scale_x_continuous(
        breaks = seq_along(hcCol$labels$label),
        labels = hcCol$labels$label, expand = c(0.05, 0)
      ) +
      scale_y_continuous(
        breaks = rep(0, uniqueN(ggData$geneName)),
        labels = unique(ggData$geneName), expand = c(0, 0)
      ) +
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +
      theme(
        axis.title = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(color = "white")
      )
    ggData$grpBy <- factor(ggData$grpBy, levels = hcCol$labels$label)
    }}

  # Actual plot according to plottype
  if (inpPlt == "Bubbleplot") {{ # Bubbleplot
    ggOut <- ggplot(ggData, aes(grpBy, geneName, color = val, size = prop)) +
      geom_point() +
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +
      scale_x_discrete(expand = c(0.05, 0)) +
      scale_y_discrete(expand = c(0, 0.5)) +
      scale_size_continuous("proportion",
        range = c(0, 8),
        limits = c(0, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)
      ) +
      scale_color_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) +
      guides(color = guide_colorbar(barwidth = 20)) +
      theme(axis.title = element_blank(), legend.box = "vertical") }} else {{ # Heatmap
    ggOut <- ggplot(ggData, aes(grpBy, geneName, fill = val)) +
      geom_tile() +
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +
      scale_x_discrete(expand = c(0.05, 0)) +
      scale_y_discrete(expand = c(0, 0.5)) +
      scale_fill_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) +
      guides(fill = guide_colorbar(barwidth = 20)) +
      theme(axis.title = element_blank()) }}

  # Final tidy
  ggLeg <- g_legend(ggOut)
  ggOut <- ggOut + theme(legend.position = "none")
  if (!save) {{ if (inpRow & inpCol) {{ ggOut <-
    grid.arrange(ggOut, ggLeg, ggCol, ggRow,
      widths = c(7, 1), heights = c(1, 7, 2),
      layout_matrix = rbind(c(3, NA), c(1, 4), c(2, NA))
    ) }} else if (inpRow) {{ ggOut <-
    grid.arrange(ggOut, ggLeg, ggRow,
      widths = c(7, 1), heights = c(7, 2),
      layout_matrix = rbind(c(1, 3), c(2, NA))
    ) }} else if (inpCol) {{ ggOut <-
    grid.arrange(ggOut, ggLeg, ggCol,
      heights = c(1, 7, 2),
      layout_matrix = rbind(c(3), c(1), c(2))
    ) }} else {{ ggOut <-
    grid.arrange(ggOut, ggLeg,
      heights = c(7, 2),
      layout_matrix = rbind(c(1), c(2))
    ) }} }} else {{ if (inpRow & inpCol) {{ ggOut <-
    arrangeGrob(ggOut, ggLeg, ggCol, ggRow,
      widths = c(7, 1), heights = c(1, 7, 2),
      layout_matrix = rbind(c(3, NA), c(1, 4), c(2, NA))
    ) }} else if (inpRow) {{ ggOut <-
    arrangeGrob(ggOut, ggLeg, ggRow,
      widths = c(7, 1), heights = c(7, 2),
      layout_matrix = rbind(c(1, 3), c(2, NA))
    ) }} else if (inpCol) {{ ggOut <-
    arrangeGrob(ggOut, ggLeg, ggCol,
      heights = c(1, 7, 2),
      layout_matrix = rbind(c(3), c(1), c(2))
    ) }} else {{ ggOut <-
    arrangeGrob(ggOut, ggLeg,
      heights = c(7, 2),
      layout_matrix = rbind(c(1), c(2))
    ) }} }}
  
  return(ggOut)
}}


### Server code ----
shinyServer(function(input, output, session) {{

### For all tags and Server-side selectize
observe_helpers()

  '
  )
}

#' Write code for tab1
#'
wrSVtab1 <- function() {
paste0('  
### Tab 1 ----

{subst}  output${prefix}a1sub1.ui <- renderUI({{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a1sub1]$fID, "\\\\|")[[1]]
{subst}    checkboxGroupInput("{prefix}a1sub2", "Select which cells to show", inline = TRUE, choices = sub, selected = sub)
{subst}  }})
{subst}  observeEvent(input${prefix}a1sub1non, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a1sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}a1sub2", label = "Select which cells to show", choices = sub, selected = NULL, inline = TRUE)
{subst}  }})
{subst}  observeEvent(input${prefix}a1sub1all, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a1sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}a1sub2", label = "Select which cells to show", choices = sub, selected = sub, inline = TRUE)
{subst}  }})

output${prefix}a1oup1 <- renderPlot({{
  req(input${prefix}a1inp1)
  scDRcell({prefix}conf, {prefix}meta, input${prefix}a1drX, input${prefix}a1drY, input${prefix}a1inp1, input${prefix}a1sub1, input${prefix}a1sub2, input${prefix}a1siz, input${prefix}a1col1, input${prefix}a1ord1, input${prefix}a1fsz, input${prefix}a1asp, input${prefix}a1txt, input${prefix}a1lab1)
}})

output${prefix}a1oup1.ui <- renderUI({{
  imageOutput("{prefix}a1oup1", height = pList[input${prefix}a1psz])
}})

output${prefix}a1oup1.pdf <- downloadHandler(
 filename = function() {{ paste0("{prefix}", input${prefix}a1drX,"_", input${prefix}a1drY,"_", input${prefix}a1inp1,".pdf") }},
 content = function(file) {{
   ggsave(
   file, device = "pdf", useDingbats = FALSE,
   plot = scDRcell({prefix}conf, {prefix}meta, input${prefix}a1drX, input${prefix}a1drY, input${prefix}a1inp1,   input${prefix}a1sub1, input${prefix}a1sub2, input${prefix}a1siz, input${prefix}a1col1, input${prefix}a1ord1,  input${prefix}a1fsz, input${prefix}a1asp, input${prefix}a1txt, input${prefix}a1lab1)
   )
}})

output${prefix}a1oup1.png <- downloadHandler(
 filename = function() {{ paste0("{prefix}",input${prefix}a1drX,"_",input${prefix}a1drY,"_", input${prefix}a1inp1,".png") }},
 content = function(file) {{
   ggsave(
   file, device = "png", dpi = input${prefix}a1oup1.res,
   plot = scDRcell({prefix}conf, {prefix}meta, input${prefix}a1drX, input${prefix}a1drY, input${prefix}a1inp1,   input${prefix}a1sub1, input${prefix}a1sub2, input${prefix}a1siz, input${prefix}a1col1, input${prefix}a1ord1,  input${prefix}a1fsz, input${prefix}a1asp, input${prefix}a1txt, input${prefix}a1lab1)
   )
}})

output${prefix}a1.dt <- renderDataTable({{
 req(input${prefix}a1inp2)
 ggData = scDRnum({prefix}conf, {prefix}meta, input${prefix}a1inp1, input${prefix}a1inp2, input${prefix}a1sub1, input${prefix}a1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a1splt)
 datatable(ggData, rownames = FALSE, extensions = "Buttons", options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
   formatRound(columns = c("pctExpress"), digits = 2)
}})

output${prefix}a1oup2 <- renderPlot({{
 req(input${prefix}a1inp2)
 scDRgene({prefix}conf, {prefix}meta, input${prefix}a1drX, input${prefix}a1drY, input${prefix}a1inp2, input${prefix}a1sub1, input${prefix}a1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a1siz, input${prefix}a1col2, input${prefix}a1ord2, input${prefix}a1fsz, input${prefix}a1asp, input${prefix}a1txt)
}})

output${prefix}a1oup2.ui <- renderUI({{
 imageOutput("{prefix}a1oup2", height = pList[input${prefix}a1psz])
}})

output${prefix}a1oup2.pdf <- downloadHandler(
 filename = function() {{ paste0("{prefix}",input${prefix}a1drX,"_",input${prefix}a1drY,"_", input${prefix}a1inp2,".pdf") }},
 content = function(file) {{
   ggsave(
   file, device = "pdf", useDingbats = FALSE,
   plot = scDRgene({prefix}conf, {prefix}meta, input${prefix}a1drX, input${prefix}a1drY, input${prefix}a1inp2,  input${prefix}a1sub1, input${prefix}a1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a1siz, input${prefix}a1col2, input${prefix}a1ord2, input${prefix}a1fsz, input${prefix}a1asp, input${prefix}a1txt)
   )
}})

output${prefix}a1oup2.png <- downloadHandler(
 filename = function() {{ paste0("{prefix}",input${prefix}a1drX,"_",input${prefix}a1drY,"_", input${prefix}a1inp2,".png") }},
 content = function(file) {{
   ggsave(
   file, device = "png", dpi = input${prefix}a1oup2.res,
   plot = scDRgene({prefix}conf, {prefix}meta, input${prefix}a1drX, input${prefix}a1drY, input${prefix}a1inp2, input${prefix}a1sub1, input${prefix}a1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a1siz, input${prefix}a1col2, input${prefix}a1ord2, input${prefix}a1fsz, input${prefix}a1asp, input${prefix}a1txt)
   )
}}) # End of tab 1

')
}

#' Write code for tab2
#'
wrSVtab2 <- function() {
paste0('
### Tab 2 ----
  
{subst}  output${prefix}a2sub1.ui <- renderUI({{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a2sub1]$fID, "\\\\|")[[1]]
{subst}    checkboxGroupInput("{prefix}a2sub2", "Select which cells to show", inline = TRUE, choices = sub, selected = sub)
{subst}  }})
{subst}  observeEvent(input${prefix}a2sub1non, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a2sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}a2sub2", label = "Select which cells to show", choices = sub, selected = NULL, inline = TRUE)
{subst}  }})
{subst}  observeEvent(input${prefix}a2sub1all, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a2sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}a2sub2", label = "Select which cells to show", choices = sub, selected = sub, inline = TRUE)
{subst}  }})

output${prefix}a2oup1 <- renderPlot({{
  req(input${prefix}a2inp1)
  scDRcell({prefix}conf, {prefix}meta, input${prefix}a2drX, input${prefix}a2drY, input${prefix}a2inp1, input${prefix}a2sub1, input${prefix}a2sub2, input${prefix}a2siz, input${prefix}a2col1, input${prefix}a2ord1, input${prefix}a2fsz, input${prefix}a2asp, input${prefix}a2txt, input${prefix}a2lab1)
}})

output${prefix}a2oup1.ui <- renderUI({{
  imageOutput("{prefix}a2oup1", height = pList[input${prefix}a2psz])
}})

output${prefix}a2oup1.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}a2drX, "_", input${prefix}a2drY, "_", input${prefix}a2inp1, ".pdf") }},
  content = function(file) {{ ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scDRcell({prefix}conf, {prefix}meta, input${prefix}a2drX, input${prefix}a2drY, input${prefix}a2inp1, input${prefix}a2sub1, input${prefix}a2sub2, input${prefix}a2siz, input${prefix}a2col1, input${prefix}a2ord1, input${prefix}a2fsz, input${prefix}a2asp, input${prefix}a2txt, input${prefix}a2lab1) )
}})

output${prefix}a2oup1.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}a2drX, "_", input${prefix}a2drY, "_", input${prefix}a2inp1, ".png") }},
  content = function(file) {{
    ggsave(
    file, device = "png", dpi = input${prefix}a2oup1.res,
    plot = scDRcell({prefix}conf, {prefix}meta, input${prefix}a2drX, input${prefix}a2drY, input${prefix}a2inp1, input${prefix}a2sub1, input${prefix}a2sub2, input${prefix}a2siz, input${prefix}a2col1, input${prefix}a2ord1, input${prefix}a2fsz, input${prefix}a2asp, input${prefix}a2txt, input${prefix}a2lab1)
    )
}})

output${prefix}a2oup2 <- renderPlot({{
  req(input${prefix}a2inp2)
  scDRcell({prefix}conf, {prefix}meta, input${prefix}a2drX, input${prefix}a2drY, input${prefix}a2inp2, input${prefix}a2sub1, input${prefix}a2sub2, input${prefix}a2siz, input${prefix}a2col2, input${prefix}a2ord2, input${prefix}a2fsz, input${prefix}a2asp, input${prefix}a2txt, input${prefix}a2lab2)
}})

output${prefix}a2oup2.ui <- renderUI({{
  imageOutput("{prefix}a2oup2", height = pList[input${prefix}a2psz])
}})

output${prefix}a2oup2.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}",input${prefix}a2drX,"_",input${prefix}a2drY,"_", input${prefix}a2inp2,".pdf") }},
  content = function(file) {{
    ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scDRcell({prefix}conf, {prefix}meta, input${prefix}a2drX, input${prefix}a2drY, input${prefix}a2inp2, input${prefix}a2sub1, input${prefix}a2sub2, input${prefix}a2siz, input${prefix}a2col2, input${prefix}a2ord2, input${prefix}a2fsz, input${prefix}a2asp, input${prefix}a2txt, input${prefix}a2lab2) 
    )
}})

output${prefix}a2oup2.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}",input${prefix}a2drX,"_",input${prefix}a2drY,"_", input${prefix}a2inp2,".png") }},
  content = function(file) {{
    ggsave(
    file, device = "png", dpi = input${prefix}a2oup2.res,
    plot = scDRcell({prefix}conf, {prefix}meta, input${prefix}a2drX, input${prefix}a2drY, input${prefix}a2inp2, input${prefix}a2sub1, input${prefix}a2sub2, input${prefix}a2siz, input${prefix}a2col2, input${prefix}a2ord2, input${prefix}a2fsz, input${prefix}a2asp, input${prefix}a2txt, input${prefix}a2lab2)
    )
}}) # End of tab 2

')
}

#' Write code for tab3
#'
wrSVtab3 <- function() {
paste0('
### Tab 3 ----

{subst}  output${prefix}a3sub1.ui <- renderUI({{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a3sub1]$fID, "\\\\|")[[1]]
{subst}    checkboxGroupInput("{prefix}a3sub2", "Select which cells to show", inline = TRUE, choices = sub, selected = sub)
{subst}  }})
{subst}  observeEvent(input${prefix}a3sub1non, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a3sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}a3sub2", label = "Select which cells to show", choices = sub, selected = NULL, inline = TRUE)
{subst}  }})
{subst}  observeEvent(input${prefix}a3sub1all, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}a3sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}a3sub2", label = "Select which cells to show", choices = sub, selected = sub, inline = TRUE)
{subst}  }})

output${prefix}a3oup1 <- renderPlot({{
  req(input${prefix}a3inp1)
  scDRgene({prefix}conf, {prefix}meta, input${prefix}a3drX, input${prefix}a3drY, input${prefix}a3inp1, input${prefix}a3sub1, input${prefix}a3sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a3siz, input${prefix}a3col1, input${prefix}a3ord1, input${prefix}a3fsz, input${prefix}a3asp, input${prefix}a3txt)
}})

output${prefix}a3oup1.ui <- renderUI({{
  imageOutput("{prefix}a3oup1", height = pList[input${prefix}a3psz])
}})

output${prefix}a3oup1.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}a3drX, "_", input${prefix}a3drY, "_", input${prefix}a3inp1, ".pdf") }},
  content = function(file) {{
    ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scDRgene({prefix}conf, {prefix}meta, input${prefix}a3drX, input${prefix}a3drY, input${prefix}a3inp1, input${prefix}a3sub1, input${prefix}a3sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a3siz, input${prefix}a3col1, input${prefix}a3ord1, input${prefix}a3fsz, input${prefix}a3asp, input${prefix}a3txt)
    )
}})

output${prefix}a3oup1.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}a3drX, "_", input${prefix}a3drY, "_", input${prefix}a3inp1,".png") }},
  content = function(file) {{
    ggsave(
    file, device = "png", dpi = input${prefix}a3oup1.res,
    plot = scDRgene({prefix}conf, {prefix}meta, input${prefix}a3drX, input${prefix}a3drY, input${prefix}a3inp1, 
                    input${prefix}a3sub1, input${prefix}a3sub2,
                    "{prefix}gexpr.h5", {prefix}gene,
                    input${prefix}a3siz, input${prefix}a3col1, input${prefix}a3ord1,
                    input${prefix}a3fsz, input${prefix}a3asp, input${prefix}a3txt)
    )
}})

output${prefix}a3oup2 <- renderPlot({{
  req(input${prefix}a3inp2)
  scDRgene({prefix}conf, {prefix}meta, input${prefix}a3drX, input${prefix}a3drY, input${prefix}a3inp2, input${prefix}a3sub1, input${prefix}a3sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a3siz, input${prefix}a3col2, input${prefix}a3ord2, input${prefix}a3fsz, input${prefix}a3asp, input${prefix}a3txt)
}})

output${prefix}a3oup2.ui <- renderUI({{
  imageOutput("{prefix}a3oup2", height = pList[input${prefix}a3psz])
}})

output${prefix}a3oup2.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}a3drX, "_", input${prefix}a3drY, "_", input${prefix}a3inp2,".pdf") }},
  content = function(file) {{
    ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scDRgene({prefix}conf, {prefix}meta, input${prefix}a3drX, input${prefix}a3drY, input${prefix}a3inp2, 
                    input${prefix}a3sub1, input${prefix}a3sub2,
                    "{prefix}gexpr.h5", {prefix}gene,
                    input${prefix}a3siz, input${prefix}a3col2, input${prefix}a3ord2,
                    input${prefix}a3fsz, input${prefix}a3asp, input${prefix}a3txt)
    )
}})

output${prefix}a3oup2.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}a3drX, "_", input${prefix}a3drY, "_", input${prefix}a3inp2,".png") }},
  content = function(file) {{
    ggsave(
    file, device = "png", dpi = input${prefix}a3oup2.res,
    plot = scDRgene({prefix}conf, {prefix}meta, input${prefix}a3drX, input${prefix}a3drY, input${prefix}a3inp2, input${prefix}a3sub1, input${prefix}a3sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}a3siz, input${prefix}a3col2, input${prefix}a3ord2, input${prefix}a3fsz, input${prefix}a3asp, input${prefix}a3txt)
    )
}}) # End of tab 3


')
}

#' Write code for tab4
#'
wrSVtab4 <- function() {
paste0('
### Tab 4 ----

{subst}  output${prefix}b2sub1.ui <- renderUI({{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}b2sub1]$fID, "\\\\|")[[1]]
{subst}    checkboxGroupInput("{prefix}b2sub2", "Select which cells to show", inline = TRUE, choices = sub, selected = sub)
{subst}  }})
{subst}  observeEvent(input${prefix}b2sub1non, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}b2sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}b2sub2", label = "Select which cells to show", choices = sub, selected = NULL, inline = TRUE)
{subst}  }})
{subst}  observeEvent(input${prefix}b2sub1all, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}b2sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}b2sub2", label = "Select which cells to show", choices = sub, selected = sub, inline = TRUE)
{subst}  }})

output${prefix}b2oup1 <- renderPlot({{
  scDRcoex({prefix}conf, {prefix}meta, input${prefix}b2drX, input${prefix}b2drY, input${prefix}b2inp1, input${prefix}b2inp2, input${prefix}b2sub1, input${prefix}b2sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}b2siz, input${prefix}b2col1, input${prefix}b2ord1, input${prefix}b2fsz, input${prefix}b2asp, input${prefix}b2txt)
}})

output${prefix}b2oup1.ui <- renderUI({{
  imageOutput("{prefix}b2oup1", height = pList2[input${prefix}b2psz])
}})

output${prefix}b2oup1.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}b2drX, "_", input${prefix}b2drY, "_", input${prefix}b2inp1, "_", input${prefix}b2inp2, ".pdf") }},
  content = function(file) {{
    ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scDRcoex({prefix}conf, {prefix}meta, input${prefix}b2drX, input${prefix}b2drY, input${prefix}b2inp1, input${prefix}b2inp2, input${prefix}b2sub1, input${prefix}b2sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}b2siz, input${prefix}b2col1, input${prefix}b2ord1, input${prefix}b2fsz, input${prefix}b2asp, input${prefix}b2txt)
    )
}})

output${prefix}b2oup1.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}b2drX, "_", input${prefix}b2drY, "_", input${prefix}b2inp1, "_", input${prefix}b2inp2, ".png") }},
  content = function(file) {{ ggsave(
    file, device = "png", dpi = input${prefix}b2oup1.res,
    plot = scDRcoex({prefix}conf, {prefix}meta, input${prefix}b2drX, input${prefix}b2drY, input${prefix}b2inp1, input${prefix}b2inp2, input${prefix}b2sub1, input${prefix}b2sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}b2siz, input${prefix}b2col1, input${prefix}b2ord1, input${prefix}b2fsz, input${prefix}b2asp, input${prefix}b2txt) )
}})

output${prefix}b2oup2 <- renderPlot({{
  scDRcoexLeg(input${prefix}b2inp1, input${prefix}b2inp2, input${prefix}b2col1, input${prefix}b2fsz)
}})

output${prefix}b2oup2.ui <- renderUI({{
  imageOutput("{prefix}b2oup2", height = "300px")
}})

output${prefix}b2oup2.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}b2drX, "_", input${prefix}b2drY, "_", input${prefix}b2inp1, "_", input${prefix}b2inp2, "_leg.pdf") }},
  content = function(file) {{ ggsave(
    file, device = "pdf", height = 3, width = 4, useDingbats = FALSE,
    plot = scDRcoexLeg(input${prefix}b2inp1, input${prefix}b2inp2, input${prefix}b2col1, input${prefix}b2fsz) )
}})

output${prefix}b2oup2.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}",input${prefix}b2drX,"_",input${prefix}b2drY,"_", 
                                  input${prefix}b2inp1,"_",input${prefix}b2inp2,"_leg.png") }},
  content = function(file) {{ ggsave(
    file, device = "png", height = 3, width = 4,
    plot = scDRcoexLeg(input${prefix}b2inp1, input${prefix}b2inp2, input${prefix}b2col1, input${prefix}b2fsz) )
}})

output${prefix}b2.dt <- renderDataTable({{
  ggData = scDRcoexNum({prefix}conf, {prefix}meta, input${prefix}b2inp1, input${prefix}b2inp2, input${prefix}b2sub1, input${prefix}b2sub2, "{prefix}gexpr.h5", {prefix}gene)
  datatable(ggData, rownames = FALSE, extensions = "Buttons", options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
            formatRound(columns = c("percent"), digits = 2)
}}) # End of tab 4

')
}

#' Write code for tab5
#'
wrSVtab5 <- function() {
paste0('
### Tab 5 ----

{subst}  output${prefix}c1sub1.ui <- renderUI({{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}c1sub1]$fID, "\\\\|")[[1]]
{subst}    checkboxGroupInput("{prefix}c1sub2", "Select which cells to show", inline = TRUE, choices = sub, selected = sub)
{subst}  }})
{subst}  observeEvent(input${prefix}c1sub1non, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}c1sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}c1sub2", label = "Select which cells to show", choices = sub, selected = NULL, inline = TRUE)
{subst}  }})
{subst}  observeEvent(input${prefix}c1sub1all, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}c1sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}c1sub2", label = "Select which cells to show", choices = sub, selected = sub, inline = TRUE)
{subst}  }})

output${prefix}c1oup <- renderPlot({{
  scVioBox({prefix}conf, {prefix}meta, input${prefix}c1inp1, input${prefix}c1inp2, input${prefix}c1sub1, input${prefix}c1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}c1typ, input${prefix}c1pts, input${prefix}c1siz, input${prefix}c1fsz)
}})

output${prefix}c1oup.ui <- renderUI({{
  imageOutput("{prefix}c1oup", height = pList2[input${prefix}c1psz])
}})

output${prefix}c1oup.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}c1typ, "_", input${prefix}c1inp1, "_", input${prefix}c1inp2, ".pdf") }},
  content = function(file) {{
    ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scVioBox({prefix}conf, {prefix}meta, input${prefix}c1inp1, input${prefix}c1inp2, input${prefix}c1sub1, input${prefix}c1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}c1typ, input${prefix}c1pts, input${prefix}c1siz, input${prefix}c1fsz)
    )
}})

output${prefix}c1oup.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}c1typ, "_", input${prefix}c1inp1, "_", input${prefix}c1inp2,".png") }},
  content = function(file) {{
    ggsave(
    file, device = "png", dpi = input${prefix}c1oup.res,
    plot = scVioBox({prefix}conf, {prefix}meta, input${prefix}c1inp1, input${prefix}c1inp2, input${prefix}c1sub1, input${prefix}c1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}c1typ, input${prefix}c1pts, input${prefix}c1siz, input${prefix}c1fsz)
    )
}}) # End of tab 5


')
}

#' Write code for tab6
#'
wrSVtab6 <- function() {
paste0('
### Tab 6 ----

{subst}  output${prefix}c2sub1.ui <- renderUI({{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}c2sub1]$fID, "\\\\|")[[1]]
{subst}    checkboxGroupInput("{prefix}c2sub2", "Select which cells to show", inline = TRUE, choices = sub, selected = sub)
{subst}  }})
{subst}  observeEvent(input${prefix}c2sub1non, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}c2sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}c2sub2", label = "Select which cells to show", choices = sub, selected = NULL, inline = TRUE)
{subst}  }})
{subst}  observeEvent(input${prefix}c2sub1all, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}c2sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}c2sub2", label = "Select which cells to show", choices = sub, selected = sub, inline = TRUE)
{subst}  }})

output${prefix}c2oup <- renderPlot({{
  scProp({prefix}conf, {prefix}meta, input${prefix}c2inp1, input${prefix}c2inp2, input${prefix}c2sub1, input${prefix}c2sub2, input${prefix}c2typ, input${prefix}c2flp, input${prefix}c2fsz)
}})

output${prefix}c2oup.ui <- renderUI({{
  imageOutput("{prefix}c2oup", height = pList2[input${prefix}c2psz])
}})

output${prefix}c2oup.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}c2typ, "_", input${prefix}c2inp1, "_", input${prefix}c2inp2, ".pdf") }},
  content = function(file) {{
    ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scProp({prefix}conf, {prefix}meta, input${prefix}c2inp1, input${prefix}c2inp2, input${prefix}c2sub1, input${prefix}c2sub2, input${prefix}c2typ, input${prefix}c2flp, input${prefix}c2fsz)
    )
  }})

output${prefix}c2oup.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}", input${prefix}c2typ, "_", input${prefix}c2inp1, "_", input${prefix}c2inp2, ".png") }},
  content = function(file) {{
    ggsave(
    file, device = "png", dpi = input${prefix}c2oup.res,
    plot = scProp({prefix}conf, {prefix}meta, input${prefix}c2inp1, input${prefix}c2inp2, input${prefix}c2sub1, input${prefix}c2sub2, input${prefix}c2typ, input${prefix}c2flp, input${prefix}c2fsz)
    )
  }}) # End of tab 6\n

')
}

#' Write code for tab7
#'
wrSVtab7 <- function() {
paste0('
### Tab 7 ----

{subst}  output${prefix}d1sub1.ui <- renderUI({{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}d1sub1]$fID, "\\\\|")[[1]]
{subst}    checkboxGroupInput("{prefix}d1sub2", "Select which cells to show", inline = TRUE, choices = sub, selected = sub)
{subst}  }})
{subst}  observeEvent(input${prefix}d1sub1non, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}d1sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}d1sub2", label = "Select which cells to show", choices = sub, selected = NULL, inline = TRUE)
{subst}  }})
{subst}  observeEvent(input${prefix}d1sub1all, {{
{subst}    sub = strsplit({prefix}conf[UI == input${prefix}d1sub1]$fID, "\\\\|")[[1]]
{subst}    updateCheckboxGroupInput(session, inputId = "{prefix}d1sub2", label = "Select which cells to show", choices = sub, selected = sub, inline = TRUE)
{subst}  }})

output${prefix}d1oupTxt <- renderUI({{
  geneList = scGeneList(input${prefix}d1inp, {prefix}gene)
  if(nrow(geneList) > 50){{
    HTML("More than 50 input genes! Please reduce the gene list!")
  }} else {{
    oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted")
    if(nrow(geneList[present == FALSE]) > 0){{
      oup = paste0(oup, "<br/>", nrow(geneList[present == FALSE]), " genes not found (", paste0(geneList[present == FALSE]$gene, collapse = ", "), ")")
    }}
    HTML(oup)
  }}
}})

output${prefix}d1oup <- renderPlot({{
  scBubbHeat({prefix}conf, {prefix}meta, input${prefix}d1inp, input${prefix}d1grp, input${prefix}d1plt, input${prefix}d1sub1, input${prefix}d1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}d1scl, input${prefix}d1row, input${prefix}d1col, input${prefix}d1cols, input${prefix}d1fsz)
}})

output${prefix}d1oup.ui <- renderUI({{
  imageOutput("{prefix}d1oup", height = pList3[input${prefix}d1psz])
}})

output${prefix}d1oup.pdf <- downloadHandler(
  filename = function() {{ paste0("{prefix}",input${prefix}d1plt,"_",input${prefix}d1grp,".pdf") }},
  content = function(file) {{
    ggsave(
    file, device = "pdf", useDingbats = FALSE,
    plot = scBubbHeat({prefix}conf, {prefix}meta, input${prefix}d1inp, input${prefix}d1grp, input${prefix}d1plt, input${prefix}d1sub1, input${prefix}d1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}d1scl, input${prefix}d1row, input${prefix}d1col, input${prefix}d1cols, input${prefix}d1fsz, save = TRUE)
    )
}})

output${prefix}d1oup.png <- downloadHandler(
  filename = function() {{ paste0("{prefix}",input${prefix}d1plt,"_",input${prefix}d1grp,".png") }},
  content = function(file) {{
    ggsave(
    file, device = "png", dpi = input${prefix}d1oup.res,
    plot = scBubbHeat({prefix}conf, {prefix}meta, input${prefix}d1inp, input${prefix}d1grp, input${prefix}d1plt, input${prefix}d1sub1, input${prefix}d1sub2, "{prefix}gexpr.h5", {prefix}gene, input${prefix}d1scl, input${prefix}d1row, input${prefix}d1col, input${prefix}d1cols, input${prefix}d1fsz, save = TRUE)
    )
}}) # End of tab 7         
       
')
}

#' Write code for main block of server.R
#' @param prefix file prefix
#' @param subst Conditional
#' @param tabs Vector of tab numbers to include
#' @rdname wrSVmain
#' @export wrSVmain
#'
wrSVmain <- function(prefix, subst = "", tabs = c(1,2,3,4,5,6,7)) {
glue::glue(
'optCrt="{{ option_create: function(data,escape) {{return(\'<div class=\\"create\\"><strong>\' + \'</strong></div>\');}} }}"
updateSelectizeInput(session, "{prefix}a1inp2", choices = names({prefix}gene), server = TRUE,
                     selected = {prefix}def$gene1, options = list(
                       maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "{prefix}a3inp1", choices = names({prefix}gene), server = TRUE,
                     selected = {prefix}def$gene1, options = list(
                       maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "{prefix}a3inp2", choices = names({prefix}gene), server = TRUE,
                     selected = {prefix}def$gene2, options = list(
                       maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "{prefix}b2inp1", choices = names({prefix}gene), server = TRUE,
                     selected = {prefix}def$gene1, options = list(
                       maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "{prefix}b2inp2", choices = names({prefix}gene), server = TRUE,
                     selected = {prefix}def$gene2, options = list(
                       maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
updateSelectizeInput(session, "{prefix}c1inp2", server = TRUE,
                     choices = c({prefix}conf[is.na(fID)]$UI,names({prefix}gene)),
                     selected = {prefix}conf[is.na(fID)]$UI[1], options = list(
                       maxOptions = length({prefix}conf[is.na(fID)]$UI) + 3,
                       create = TRUE, persist = TRUE, render = I(optCrt)))',
  ifelse(1 %in% tabs, wrSVtab1(), ""),
  ifelse(2 %in% tabs, wrSVtab2(), ""),
  ifelse(3 %in% tabs, wrSVtab3(), ""),
  ifelse(4 %in% tabs, wrSVtab4(), ""),
  ifelse(5 %in% tabs, wrSVtab5(), ""),
  ifelse(6 %in% tabs, wrSVtab6(), ""),
  ifelse(7 %in% tabs, wrSVtab7(), ""),
  .sep = "\n"
)
}

#' Write code for final portion of server.R
#'
#' @rdname wrSVend
#' @export wrSVend
#'
wrSVend <- function() {
  paste0(
    '
    })
    
    '
  )
}

#' Write code for loading objects for ui.R
#'
#' @param prefix file prefix
#'
#' @rdname wrUIload
#' @export wrUIload
#'
wrUIload <- function(prefix) {
  glue::glue('

{prefix}conf = readRDS("{prefix}conf.rds")
{prefix}def  = readRDS("{prefix}def.rds")

')
}

#' Write code for front portion of ui.R
#' @param title shiny app title
#' @param theme bootstrap theme
#' @param ganalytics Google analytics tracking ID (e.g. "UA-123456789-0")
#' @importFrom shinythemes shinytheme
#' @rdname wrUIsingle
#' @export wrUIsingle
#'
wrUIsingle <- function(title, theme = "flatly", ganalytics = NA) {
  if (!is.na(ganalytics)) {
    ga <- 'tags$head(includeHTML(("google-analytics.html"))),'
  } else {
    ga <- ""
  }
  
  glue::glue(
'

### UI code
shinyUI(
fluidPage(style="margin:0;padding:0;",
tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
{ga}
theme = shinythemes::shinytheme("{theme}"),
navbarPage(
"{title}"
'
  )
}

#' Write code for tab1 of ui.R
#'
wrUItab1 <- function() {
paste0(',
# tab 1 ----
tabPanel(
  "CellInfo vs GeneExpr",
  fluidRow(
    class = "container page",
    column(
      12,
      # row 1 ----
      fluidRow(
        class = "tab-section",
        column(
          12,
          h3("Cell information vs Gene expression"),
          p("Cell information and gene expression side-by-side on low-dimensional represention.")
        )
      ),
      # row 2 ----
      fluidRow(
        class = "tab-section",
        # row 2 col 1
        column(
          4,
          div(
            class = "input-panel",
            h4("Dimension Reduction"),
            selectInput("{prefix}a1drX", "X-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[1]
            ),
            selectInput("{prefix}a1drY", "Y-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[2]
            )
          )
        ), # row 1 col 1
        # row 2 col 2
        {subst}column(
          {subst}4,
          {subst}div(
            {subst}class = "input-panel",
            {subst}checkboxInput("{prefix}a1togL", "Subset cells"),
            {subst}conditionalPanel(
              {subst}condition = "input.{prefix}a1togL == true",
              {subst}selectInput("{prefix}a1sub1", "Cell information to subset:",
                {subst}choices = {prefix}conf[grp == TRUE]$UI,
                {subst}selected = {prefix}def$grp1
              {subst}),
              {subst}uiOutput("{prefix}a1sub1.ui"),
              {subst}actionButton("{prefix}a1sub1all", "Select all groups", class = "btn btn-primary btn-sm"),
              {subst}actionButton("{prefix}a1sub1non", "Deselect all groups", class = "btn btn-primary btn-sm")
            {subst})
          {subst})
        {subst}), # End of column
        # row 2 col 3
        column(
          4,
          div(
            class = "input-panel",
            checkboxInput("{prefix}a1tog0", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}a1tog0 == true",
              sliderInput("{prefix}a1siz", "Point size:",
                min = 0, max = 4, value = 1.25, step = 0.25
              ),
              radioButtons("{prefix}a1psz", "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Medium", inline = TRUE
              ),
              radioButtons("{prefix}a1fsz", "Font size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              ),
              radioButtons("{prefix}a1asp", "Aspect ratio:",
                choices = c("Square", "Fixed", "Free"),
                selected = "Square", inline = TRUE
              ),
              checkboxInput("{prefix}a1txt", "Show axis text", value = FALSE)
            )
          )
        ) # row 2 col 3
      ), # row 2
      # row 3 ----
      fluidRow(
        class = "tab-section",
        # row 3 col 1
        column(6,
          style = "border-right: 2px solid #f3f6f4",
          h4("Cell information"),
          # row 3 col 1 row 1
          fluidRow(
            class = "tab-section",
            column(
              6,
              div(
                class = "input-panel",
                selectInput("{prefix}a1inp1", "Cell info:",
                  choices = {prefix}conf$UI,
                  selected = {prefix}def$meta1
                ) %>%
                  helper(
                    type = "inline", size = "m", fade = TRUE,
                    title = "Cell information to colour cells by",
                    content = c(
                      "Select cell information to colour cells",
                      "- Categorical covariates have a fixed colour palette",
                      paste0(
                        "- Continuous covariates are coloured in a ",
                        "Blue-Yellow-Red colour scheme, which can be ",
                        "changed in the plot controls"
                      )
                    )
                  )
              )
            ),
            column(
              6,
              div(
                class = "input-panel",
                checkboxInput("{prefix}a1tog1", "Adjust graphics"),
                conditionalPanel(
                  condition = "input.{prefix}a1tog1 == true",
                  radioButtons("{prefix}a1col1", "Colour (Continuous data):",
                    choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"),
                    selected = "Blue-Yellow-Red"
                  ),
                  radioButtons("{prefix}a1ord1", "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Original", inline = TRUE
                  ),
                  checkboxInput("{prefix}a1lab1", "Show cell info labels", value = TRUE)
                )
              )
            )
          ),
          # row 3 col 1 row 2
          fluidRow(
            class = "tab-section",
            column(
              12,
              uiOutput("{prefix}a1oup1.ui")
            )
          ),
          # row 3 col 1 row 3
          fluidRow(
            class = "tab-section",
            column(
              12,
              div(
                class = "input-panel",
                numericInput("{prefix}a1oup1.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
                downloadButton("{prefix}a1oup1.pdf", "Download PDF", class = "btn-sm"),
                downloadButton("{prefix}a1oup1.png", "Download PNG", class = "btn-sm"),
                checkboxInput("{prefix}a1tog9", "Show cell numbers / statistics")
              )
            )
          ),
          conditionalPanel(
            condition = "input.{prefix}a1tog9 == true",
            h4("Cell numbers / statistics"),
            radioButtons("{prefix}a1splt", "Split continuous cell info into:",
              choices = c("Quartile", "Decile"),
              selected = "Decile", inline = TRUE
            ),
            dataTableOutput("{prefix}a1.dt")
          )
        ), # row 3 col 1
        # row 3 col 2
        column(
          6,
          h4("Gene expression"),
          fluidRow(
            class = "tab-section",
            column(
              6,
              div(
                class = "input-panel",
                selectInput("{prefix}a1inp2", "Gene name:", choices = NULL) %>%
                  helper(
                    type = "inline", size = "m", fade = TRUE,
                    title = "Gene expression to colour cells by",
                    content = c(
                      "Select gene to colour cells by gene expression",
                      paste0(
                        "- Gene expression are coloured in a ",
                        "White-Red colour scheme which can be ",
                        "changed in the plot controls"
                      )
                    )
                  )
              )
            ),
            column(
              6,
              div(
                class = "input-panel",
                checkboxInput("{prefix}a1tog2", "Adjust graphics"),
                conditionalPanel(
                  condition = "input.{prefix}a1tog2 == true",
                  radioButtons("{prefix}a1col2", "Colour:",
                    choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"),
                    selected = "White-Red"
                  ),
                  radioButtons("{prefix}a1ord2", "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Max-1st", inline = TRUE
                  )
                )
              )
            )
          ),
          fluidRow(
            class = "tab-section",
            column(
              12,
              uiOutput("{prefix}a1oup2.ui")
            )
          ),
          fluidRow(
            class = "tab-section",
            column(
              12,
              div(
                class = "input-panel",
                numericInput("{prefix}a1oup2.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
                downloadButton("{prefix}a1oup2.pdf", "Download PDF", class = "btn-sm"),
                downloadButton("{prefix}a1oup2.png", "Download PNG", class = "btn-sm")
              )
            )
          )
        ) # row 3 col 2
      ), # row 3
      hr()
    )
  )
) # End of tab 1
')
}

#' Write code for tab2 of ui.R
#'
#'
wrUItab2 <- function() {
paste0(',
# tab 2 ----
tabPanel(
  "CellInfo vs CellInfo",
  fluidRow(
    class = "container page",
    column(
      12,
      # row 1 ----
      fluidRow(
        class = "tab-section",
        column(
          12,
          h3("Cell info vs cell info"),
          p("Two cell informations side-by-side on low-dimensional represention.")
        ) # row 1 col 1
      ), # row 1
      # row 2 ----
      fluidRow(
        class = "tab-section",
        column(
          4,
          div(
            class = "input-panel",
            h4("Dimension Reduction"),
            selectInput("{prefix}a2drX", "X-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[1]
            ),
            selectInput("{prefix}a2drY", "Y-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[2]
            )
          )
        ), # row 2 col 2
        # row 2 col 2
        {subst}column(4,
          {subst}div(
            {subst}class = "input-panel",
            {subst}checkboxInput("{prefix}a2togL", "Subset cells"),
            {subst}conditionalPanel(
              {subst}condition = "input.{prefix}a2togL == true",
              {subst}selectInput("{prefix}a2sub1", "Cell information to subset:",
               {subst} choices = {prefix}conf[grp == TRUE]$UI,
               {subst} selected = {prefix}def$grp1
              {subst}),
              {subst}uiOutput("{prefix}a2sub1.ui"),
              {subst}actionButton("{prefix}a2sub1all", "Select all groups", class = "btn btn-primary btn-sm"),
              {subst}actionButton("{prefix}a2sub1non", "Deselect all groups", class = "btn btn-primary btn-sm")
            {subst})
          {subst})
        {subst}), # row 2 col 2
        # row 2 col 3
        column(
          4,
          div(
            class = "input-panel",
            checkboxInput("{prefix}a2tog0", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}a2tog0 == true",
              sliderInput("{prefix}a2siz", "Point size:",
                min = 0, max = 4, value = 1.25, step = 0.25
              ),
              radioButtons("{prefix}a2psz", "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Medium", inline = TRUE
              ),
              radioButtons("{prefix}a2fsz", "Font size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              ),
              radioButtons("{prefix}a2asp", "Aspect ratio:",
                choices = c("Square", "Fixed", "Free"),
                selected = "Square", inline = TRUE
              ),
              checkboxInput("{prefix}a2txt", "Show axis text", value = FALSE)
            )
          )
        ) # row 2 col 3
      ), # row 2
      # row 3 ----
      fluidRow(
        class = "tab-section",
        # row 3 col 1
        column(
          6,
          style = "border-right: 2px solid #f3f6f4",
          h4("Cell info 1"),
          # row 3 col 1 row 1
          fluidRow(
            class = "tab-section",
            column(
              6,
              div(
                class = "input-panel",
                selectInput("{prefix}a2inp1", "Cell info:",
                  choices = {prefix}conf$UI,
                  selected = {prefix}def$meta1
                ) %>%
                  helper(
                    type = "inline", size = "m", fade = TRUE,
                    title = "Cell information to colour cells by",
                    content = c(
                      "Select cell information to colour cells",
                      "- Categorical covariates have a fixed colour palette",
                      paste0(
                        "- Continuous covariates are coloured in a ",
                        "Blue-Yellow-Red colour scheme, which can be ",
                        "changed in the plot controls"
                      )
                    )
                  )
              )
            ),
            column(
              6,
              div(
                class = "input-panel",
                checkboxInput("{prefix}a2tog1", "Adjust graphics"),
                conditionalPanel(
                  condition = "input.{prefix}a2tog1 == true",
                  radioButtons("{prefix}a2col1", "Colour (Continuous data):",
                    choices = c(
                      "White-Red", "Blue-Yellow-Red",
                      "Yellow-Green-Purple"
                    ),
                    selected = "Blue-Yellow-Red"
                  ),
                  radioButtons("{prefix}a2ord1", "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Original", inline = TRUE
                  ),
                  checkboxInput("{prefix}a2lab1", "Show cell info labels", value = TRUE)
                )
              )
            )
          ),
          # row 3 col 1 row 2
          fluidRow(column(12, uiOutput("{prefix}a2oup1.ui"))),
          # row 3 col 1 row 3
          fluidRow(
            class = "tab-section",
            column(
              12,
              div(
                class = "input-panel",
                numericInput("{prefix}a2oup1.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
                downloadButton("{prefix}a2oup1.pdf", "Download PDF", class = "btn-sm"),
                downloadButton("{prefix}a2oup1.png", "Download PNG", class = "btn-sm")
              )
            )
          )
        ), # row 3 col 1
        # row 3 col 2
        column(
          6, h4("Cell info 2"),
          fluidRow(
            class = "tab-section",
            column(
              6,
              div(
                class = "input-panel",
                selectInput("{prefix}a2inp2", "Cell info:",
                  choices = {prefix}conf$UI,
                  selected = {prefix}def$meta2
                ) %>%
                  helper(
                    type = "inline", size = "m", fade = TRUE,
                    title = "Cell information to colour cells by",
                    content = c(
                      "Select cell information to colour cells",
                      "- Categorical covariates have a fixed colour palette",
                      paste0(
                        "- Continuous covariates are coloured in a ",
                        "Blue-Yellow-Red colour scheme, which can be ",
                        "changed in the plot controls"
                      )
                    )
                  )
              )
            ),
            column(
              6,
              div(
                class = "input-panel",
                checkboxInput("{prefix}a2tog2", "Adjust graphics"),
                conditionalPanel(
                  condition = "input.{prefix}a2tog2 == true",
                  radioButtons("{prefix}a2col2", "Colour (Continuous data):",
                    choices = c(
                      "White-Red", "Blue-Yellow-Red",
                      "Yellow-Green-Purple"
                    ),
                    selected = "Blue-Yellow-Red"
                  ),
                  radioButtons("{prefix}a2ord2", "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Original", inline = TRUE
                  ),
                  checkboxInput("{prefix}a2lab2", "Show cell info labels", value = TRUE)
                )
              )
            )
          ),
          fluidRow(column(12, uiOutput("{prefix}a2oup2.ui"))),
          fluidRow(
            class = "tab-section",
            column(
              12,
              div(
                class = "input-panel",
                numericInput("{prefix}a2oup2.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
                downloadButton("{prefix}a2oup2.pdf", "Download PDF", class = "btn-sm"),
                downloadButton("{prefix}a2oup2.png", "Download PNG", class = "btn-sm")
              )
            )
          )
        ) # row 3 col 2
      ), # row 3
      hr()
    )
  )
) # End of tab 2
')
}

#' Write code for tab3 of ui.R
#'
wrUItab3 <- function() {
paste0(',
# tab 3 ----
tabPanel(
  "GeneExpr vs GeneExpr",
  fluidRow(
    class = "container page",
    column(
      12,
      # row 1 ----
      fluidRow(
        class = "tab-section",
        column(
          12,
          h3("Gene expression vs Gene expression"),
          p("Visualise two gene expressions side-by-side on low-dimensional representions.")
        )
      ),
      # row 2 ----
      fluidRow(
        class = "tab-section",
        # row 2 col 1
        column(
          4,
          div(
            class = "input-panel",
            h4("Dimension Reduction"),
            selectInput("{prefix}a3drX", "X-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[1]
            ),
            selectInput("{prefix}a3drY", "Y-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[2]
            )
          )
        ), # row 1 col 1
        # row 2 col 2
        {subst}column(4,
          {subst}div(
            {subst}class = "input-panel",
            {subst}checkboxInput("{prefix}a3togL", "Subset cells"),
            {subst}conditionalPanel(
             {subst} condition = "input.{prefix}a3togL == true",
              {subst}selectInput("{prefix}a3sub1", "Cell information to subset:",
               {subst} choices = {prefix}conf[grp == TRUE]$UI,
                {subst}selected = {prefix}def$grp1
              {subst}),
              {subst}uiOutput("{prefix}a3sub1.ui"),
              {subst}actionButton("{prefix}a3sub1all", "Select all groups", class = "btn btn-primary btn-sm"),
              {subst}actionButton("{prefix}a3sub1non", "Deselect all groups", class = "btn btn-primary btn-sm")
            {subst})
          {subst})
        {subst}), # End of column
        # row 2 col 3
        column(
          4,
          div(
            class = "input-panel",
            checkboxInput("{prefix}a3tog0", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}a3tog0 == true",
              sliderInput("{prefix}a3siz", "Point size:",
                min = 0, max = 4, value = 1.25, step = 0.25
              ),
              radioButtons("{prefix}a3psz", "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Medium", inline = TRUE
              ),
              radioButtons("{prefix}a3fsz", "Font size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              ),
              radioButtons("{prefix}a3asp", "Aspect ratio:",
                choices = c("Square", "Fixed", "Free"),
                selected = "Square", inline = TRUE
              ),
              checkboxInput("{prefix}a3txt", "Show axis text", value = FALSE)
            )
          )
        ) # row 2 col 3
      ), # row 2
      # row 3 ----
      fluidRow(
        class = "tab-section",
        column(
          6,
          style = "border-right: 2px solid #f3f6f4",
          h4("Gene expression 1"),
          fluidRow(
            class = "tab-section",
            column(
              6,
              div(
                class = "input-panel",
                selectInput("{prefix}a3inp1", "Gene name:", choices = NULL) %>%
                  helper(
                    type = "inline", size = "m", fade = TRUE,
                    title = "Gene expression to colour cells by",
                    content = c(
                      "Select gene to colour cells by gene expression",
                      paste0(
                        "- Gene expression are coloured in a ",
                        "White-Red colour scheme which can be ",
                        "changed in the plot controls"
                      )
                    )
                  )
              )
            ),
            column(
              6,
              div(
                class = "input-panel",
                checkboxInput("{prefix}a3tog1", "Adjust graphics"),
                conditionalPanel(
                  condition = "input.{prefix}a3tog1 == true",
                  radioButtons("{prefix}a3col1", "Colour:",
                    choices = c(
                      "White-Red", "Blue-Yellow-Red",
                      "Yellow-Green-Purple"
                    ),
                    selected = "White-Red"
                  ),
                  radioButtons("{prefix}a3ord1", "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Max-1st", inline = TRUE
                  )
                )
              )
            )
          ),
          # row 3 col 1 row 2
          fluidRow(
            class = "tab-section",
            column(12, uiOutput("{prefix}a3oup1.ui"))
          ),
          # row 3 col 1 row 3
          fluidRow(
            class = "tab-section",
            column(
              12,
              div(
                class = "input-panel",
                numericInput("{prefix}a3oup1.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
                downloadButton("{prefix}a3oup1.pdf", "Download PDF", class = "btn-sm"),
                downloadButton("{prefix}a3oup1.png", "Download PNG", class = "btn-sm")
              )
            )
          )
        ), # row 3 col 1
        # row 3 col 2
        column(
          6, h4("Gene expression 2"),
          fluidRow(
            class = "tab-section",
            column(
              6,
              div(
                class = "input-panel",
                selectInput("{prefix}a3inp2", "Gene name:", choices = NULL) %>%
                  helper(
                    type = "inline", size = "m", fade = TRUE,
                    title = "Gene expression to colour cells by",
                    content = c(
                      "Select gene to colour cells by gene expression",
                      paste0(
                        "- Gene expression are coloured in a ",
                        "White-Red colour scheme which can be ",
                        "changed in the plot controls"
                      )
                    )
                  )
              )
            ),
            column(
              6,
              div(
                class = "input-panel",
                checkboxInput("{prefix}a3tog2", "Adjust graphics"),
                conditionalPanel(
                  condition = "input.{prefix}a3tog2 == true",
                  radioButtons("{prefix}a3col2", "Colour:",
                    choices = c(
                      "White-Red", "Blue-Yellow-Red",
                      "Yellow-Green-Purple"
                    ),
                    selected = "White-Red"
                  ),
                  radioButtons("{prefix}a3ord2", "Plot order:",
                    choices = c("Max-1st", "Min-1st", "Original", "Random"),
                    selected = "Max-1st", inline = TRUE
                  )
                )
              )
            )
          ),
          fluidRow(
            class = "tab-section",
            column(12, uiOutput("{prefix}a3oup2.ui"))
          ),
          fluidRow(
            class = "tab-section",
            column(
              12,
              div(
                class = "input-panel",
                  numericInput("{prefix}a3oup2.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
                  downloadButton("{prefix}a3oup2.pdf", "Download PDF", class = "btn-sm"),
                  downloadButton("{prefix}a3oup2.png", "Download PNG", class = "btn-sm")
              )
            )
          )
        ) # row 3 col 2
      ), # row 3
      hr()
    )
  )
) # End of tab 3
')
}

#' Write code for tab4 of ui.R
#'
wrUItab4 <- function() {
paste0(',
# tab 4 ----
tabPanel(
  "Gene coexpression",
  fluidRow(
    class = "container page",
    column(
      12,
      # row 1 ----
      fluidRow(
        class = "tab-section",
        column(
          12,
          h3("Coexpression of two genes on reduced dimensions"),
          p("Visualise the coexpression of two genes on low-dimensional representions.")
        )
      ),
      # row 2 ----
      fluidRow(
        class = "tab-section",
        # row 2 col 1
        column(
          4,
          div(
            class = "input-panel",
            h4("Dimension Reduction"),
            selectInput("{prefix}b2drX", "X-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[1]
            ),
            selectInput("{prefix}b2drY", "Y-axis:",
              choices = {prefix}conf[dimred == TRUE]$UI,
              selected = {prefix}def$dimred[2]
            )
          )
        ), # row 1 col 1
        # row 2 col 2
        {subst}column(4,
          {subst}div(
            {subst}class = "input-panel",
            {subst}checkboxInput("{prefix}b2togL", "Subset cells"),
            {subst}conditionalPanel(
             {subst} condition = "input.{prefix}b2togL == true",
             {subst} selectInput("{prefix}b2sub1", "Cell information to subset:",
              {subst}  choices = {prefix}conf[grp == TRUE]$UI,
              {subst}  selected = {prefix}def$grp1
              {subst}),
              {subst}uiOutput("{prefix}b2sub1.ui"),
              {subst}actionButton("{prefix}b2sub1all", "Select all groups", class = "btn btn-primary btn-sm"),
              {subst}actionButton("{prefix}b2sub1non", "Deselect all groups", class = "btn btn-primary btn-sm")
            {subst})
          {subst})
        {subst}), # End of column
        # row 2 col 3
        column(
          4,
          div(
            class = "input-panel",
            checkboxInput("{prefix}b2tog0", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}b2tog0 == true",
              sliderInput("{prefix}b2siz", "Point size:",
                min = 0, max = 4, value = 1.25, step = 0.25
              ),
              radioButtons("{prefix}b2psz", "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Medium", inline = TRUE
              ),
              radioButtons("{prefix}b2fsz", "Font size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              ),
              radioButtons("{prefix}b2asp", "Aspect ratio:",
                choices = c("Square", "Fixed", "Free"),
                selected = "Square", inline = TRUE
              ),
              checkboxInput("{prefix}b2txt", "Show axis text", value = FALSE)
            )
          )
        ) # row 2 col 3
      ), # row 2
      # row 3 ----
      fluidRow(
        class = "tab-section",
        # row 3 col 1
        column(
          3,
          style = "border-right: 2px solid #f3f6f4",
          div(
            class = "input-panel",
            h4("Gene Expression"),
            selectInput("{prefix}b2inp1", "Gene 1:", choices = NULL) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "Gene expression to colour cells by",
                content = c(
                  "Select gene to colour cells by gene expression",
                  paste0(
                    "- Gene expression are coloured in a ",
                    "White-Red colour scheme which can be ",
                    "changed in the plot controls"
                  )
                )
              ),
            selectInput("{prefix}b2inp2", "Gene 2:", choices = NULL) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "Gene expression to colour cells by",
                content = c(
                  "Select gene to colour cells by gene expression",
                  paste0(
                    "- Gene expression are coloured in a ",
                    "White-Blue colour scheme which can be ",
                    "changed in the plot controls"
                  )
                )
              ),
            checkboxInput("{prefix}b2tog1", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}b2tog1 == true",
              radioButtons("{prefix}b2col1", "Colour:",
                choices = c(
                  "Red (Gene1); Blue (Gene2)",
                  "Orange (Gene1); Blue (Gene2)",
                  "Red (Gene1); Green (Gene2)",
                  "Green (Gene1); Blue (Gene2)"
                ),
                selected = "Red (Gene1); Blue (Gene2)"
              ),
              radioButtons("{prefix}b2ord1", "Plot order:",
                choices = c("Max-1st", "Min-1st", "Original", "Random"),
                selected = "Max-1st", inline = TRUE
              )
            )
          )
        ), # row 3 col 1
        # row 3 col 2
        column(
          6,
          style = "border-right: 2px solid #f3f6f4",
          uiOutput("{prefix}b2oup1.ui"),
          div(
            class = "input-panel",
            numericInput("{prefix}b2oup1.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
            downloadButton("{prefix}b2oup1.pdf", "Download PDF", class = "btn-sm"),
            downloadButton("{prefix}b2oup1.png", "Download PNG", class = "btn-sm")
          )
        ), # row 3 col 2
        # row 3 col 3
        column(
          3,
          uiOutput("{prefix}b2oup2.ui"),
          downloadButton("{prefix}b2oup2.pdf", "Download PDF", class = "btn-sm"),
          downloadButton("{prefix}b2oup2.png", "Download PNG", class = "btn-sm"),
          h4("Cell numbers"),
          dataTableOutput("{prefix}b2.dt")
        ) # row 3 col 3
      ), # row 3
      hr()
    )
  )
) # End of tab 4
')
}

#' Write code for tab5 of ui.R
#'
wrUItab5 <- function() {
paste0(',
# tab 5 ----
tabPanel(
  "Violinplot / Boxplot",
  fluidRow(
    class = "container page",
    column(
      12,
      # row 1 ----
      fluidRow(
        class = "tab-section",
        column(
          12,
          h3("Cell information / gene expression violin plot / box plot"),
          p("Visualise the gene expression or continuous cell information (e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).")
        )
      ),
      # row 2 ----
      fluidRow(
        class = "tab-section",
        # row 2 col 1
        column(
          3,
          div(
            class = "input-panel",
            style = "border-right: 2px solid #f3f6f4",
            selectInput("{prefix}c1inp1", "Cell information (X-axis):",
              choices = {prefix}conf[grp == TRUE]$UI,
              selected = {prefix}def$grp1
            ) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "Cell information to group cells by",
                content = c(
                  "Select categorical cell information to group cells by",
                  "- Single cells are grouped by this categorical covariate",
                  "- Plotted as the X-axis of the violin plot / box plot"
                )
              ),
            selectInput("{prefix}c1inp2", "Cell Info / Gene name (Y-axis):", choices = NULL) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "Cell Info / Gene to plot",
                content = c(
                  "Select cell info / gene to plot on Y-axis",
                  "- Can be continuous cell information (e.g. nUMIs / scores)",
                  "- Can also be gene expression"
                )
              ),
            radioButtons("{prefix}c1typ", "Plot type:",
              choices = c("violin", "boxplot"),
              selected = "violin", inline = TRUE
            ),
            checkboxInput("{prefix}c1pts", "Show data points", value = FALSE),
            {subst}checkboxInput("{prefix}c1togL", "Subset cells"),
            {subst}conditionalPanel(
              {subst}condition = "input.{prefix}c1togL == true",
              {subst}selectInput("{prefix}c1sub1", "Cell information to subset:",
               {subst} choices = {prefix}conf[grp == TRUE]$UI,
               {subst} selected = {prefix}def$grp1
              {subst}),
              {subst}uiOutput("{prefix}c1sub1.ui"),
              {subst}actionButton("{prefix}c1sub1all", "Select all groups", class = "btn btn-primary btn-sm"),
              {subst}actionButton("{prefix}c1sub1non", "Deselect all groups", class = "btn btn-primary btn-sm")
            {subst}),
            checkboxInput("{prefix}c1tog", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}c1tog == true",
              sliderInput("{prefix}c1siz", "Data point size:",
                min = 0, max = 4, value = 1.25, step = 0.25
              ),
              radioButtons("{prefix}c1psz", "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              ),
              radioButtons("{prefix}c1fsz", "Font size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              )
            )
          ),
          div(
            class = "input-panel",
            numericInput("{prefix}c1oup.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
            downloadButton("{prefix}c1oup.pdf", "Download PDF", class = "btn-sm"),
            downloadButton("{prefix}c1oup.png", "Download PNG", class = "btn-sm")
          )
        ), # row 2 col 1
        # row 2 col 2
        column(
          9, uiOutput("{prefix}c1oup.ui")
        ) # row 2 col 2
      ), # row 2
      hr()
    )
  )
) # End of tab 5
')
}

#' Write code for tab6 of ui.R
#'
wrUItab6 <- function() {
paste0(',
# tab 6 ----
tabPanel(
  "Proportion plot",
  fluidRow(
    class = "container page",
    column(
      12,
      # row 1 ----
      fluidRow(
        class = "tab-section",
        column(
          12,
          h3("Proportion / cell numbers across different cell information"),
          p("Visualise the composition of single cells based on one discrete cell information across another discrete cell information. Usage examples include the library or cellcycle composition across clusters.")
        )
      ),
      # row 2 ----
      fluidRow(
        class = "tab-section",
        # row 2 col 1
        column(
          3,
          style = "border-right: 2px solid #f3f6f4",
          div(
            class = "input-panel",
            selectInput("{prefix}c2inp1", "Cell information to plot (X-axis):",
              choices = {prefix}conf[grp == TRUE]$UI,
              selected = {prefix}def$grp2
            ) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "Cell information to plot cells by",
                content = c(
                  "Select categorical cell information to plot cells by",
                  "- Plotted as the X-axis of the proportion plot"
                )
              ),
            selectInput("{prefix}c2inp2", "Cell information to group / colour by:",
              choices = {prefix}conf[grp == TRUE]$UI,
              selected = {prefix}def$grp1
            ) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "Cell information to group / colour cells by",
                content = c(
                  "Select categorical cell information to group / colour cells by",
                  "- Proportion / cell numbers are shown in different colours"
                )
              ),
            radioButtons("{prefix}c2typ", "Plot value:",
              choices = c("Proportion", "CellNumbers"),
              selected = "Proportion", inline = TRUE
            ),
            checkboxInput("{prefix}c2flp", "Flip X/Y", value = FALSE),
            {subst}checkboxInput("{prefix}c2togL", "Subset cells"),
            {subst}conditionalPanel(
             {subst} condition = "input.{prefix}c2togL == true",
             {subst} selectInput("{prefix}c2sub1", "Cell information to subset:",
              {subst}  choices = {prefix}conf[grp == TRUE]$UI,
               {subst} selected = {prefix}def$grp1
              {subst}),
              {subst}uiOutput("{prefix}c2sub1.ui"),
              {subst}actionButton("{prefix}c2sub1all", "Select all groups", class = "btn btn-primary btn-sm"),
              {subst}actionButton("{prefix}c2sub1non", "Deselect all groups", class = "btn btn-primary btn-sm")
            ),
            checkboxInput("{prefix}c2tog", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}c2tog == true",
              radioButtons("{prefix}c2psz", "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Medium", inline = TRUE
              ),
              radioButtons("{prefix}c2fsz", "Font size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              )
            )
          ),
          div(
            class = "input-panel",
            numericInput("{prefix}c2oup.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
            downloadButton("{prefix}c2oup.pdf", "Download PDF", class = "btn-sm"),
            downloadButton("{prefix}c2oup.png", "Download PNG", class = "btn-sm")
          )
        ), # row 2 col 1
        # row 2 col 2
        column(
          9, uiOutput("{prefix}c2oup.ui")
        ) # row 2 col 2
      ), # row 2
      hr()
    )
  )
) # End of tab 6
')
}

#' Write code for tab7 of ui.R
#'
wrUItab7 <- function() {
paste0(',
# tab 7 ----
tabPanel(
  "Bubbleplot / Heatmap",
  fluidRow(
    class = "container page",
    column(
      12,
      # row 1 ----
      fluidRow(
        class = "tab-section",
        column(
          12,
          h3("Gene expression bubbleplot / heatmap"),
          p("Visualise the gene expression patterns of multiple genes grouped by categorical cell information (e.g. library / cluster). The normalised expression are averaged, log-transformed and then plotted.")
        )
      ),
      # row 2 ----
      fluidRow(
        class = "tab-section",
        column(
          3,
          style = "border-right: 2px solid #f3f6f4",
          div(
            class = "input-panel",
            textAreaInput("{prefix}d1inp", HTML("List of gene names <br />
                                        (Max 50 genes, separated <br />
                                         by , or ; or newline):"),
              height = "200px",
              value = paste0({prefix}def$genes, collapse = ", ")
            ) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "List of genes to plot on bubbleplot / heatmap",
                content = c(
                  "Input genes to plot",
                  "- Maximum 50 genes (due to ploting space limitations)",
                  "- Genes should be separated by comma, semicolon or newline"
                )
              ),
            selectInput("{prefix}d1grp", "Group by:",
              choices = {prefix}conf[grp == TRUE]$UI,
              selected = {prefix}conf[grp == TRUE]$UI[1]
            ) %>%
              helper(
                type = "inline", size = "m", fade = TRUE,
                title = "Cell information to group cells by",
                content = c(
                  "Select categorical cell information to group cells by",
                  "- Single cells are grouped by this categorical covariate",
                  "- Plotted as the X-axis of the bubbleplot / heatmap"
                )
              ),
            radioButtons("{prefix}d1plt", "Plot type:",
              choices = c("Bubbleplot", "Heatmap"),
              selected = "Bubbleplot", inline = TRUE
            ),
            checkboxInput("{prefix}d1scl", "Scale gene expression", value = TRUE),
            checkboxInput("{prefix}d1row", "Cluster rows (genes)", value = TRUE),
            checkboxInput("{prefix}d1col", "Cluster columns (samples)", value = FALSE),
            {subst}checkboxInput("{prefix}d1togL", "Subset cells"),
            {subst}conditionalPanel(
              {subst}condition = "input.{prefix}d1togL == true",
              {subst}selectInput("{prefix}d1sub1", "Cell information to subset:",
                {subst}choices = {prefix}conf[grp == TRUE]$UI,
                {subst}selected = {prefix}def$grp1
              {subst}),
              {subst}uiOutput("{prefix}d1sub1.ui"),
              {subst}actionButton("{prefix}d1sub1all", "Select all groups", class = "btn btn-primary btn-sm"),
              {subst}actionButton("{prefix}d1sub1non", "Deselect all groups", class = "btn btn-primary btn-sm")
            {subst}),
            checkboxInput("{prefix}d1tog", "Adjust graphics"),
            conditionalPanel(
              condition = "input.{prefix}d1tog == true",
              radioButtons("{prefix}d1cols", "Colour scheme:",
                choices = c(
                  "White-Red", "Blue-Yellow-Red",
                  "Yellow-Green-Purple"
                ),
                selected = "Blue-Yellow-Red"
              ),
              radioButtons("{prefix}d1psz", "Plot size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Medium", inline = TRUE
              ),
              radioButtons("{prefix}d1fsz", "Font size:",
                choices = c("Small", "Medium", "Large"),
                selected = "Small", inline = TRUE
              )
            )
          ),
          div(
            class = "input-panel",
            numericInput("{prefix}d1oup.res", "Resolution:", min = 72, max = 600, value = 150, step = 5),
            downloadButton("{prefix}d1oup.pdf", "Download PDF", class = "btn-sm"),
            downloadButton("{prefix}d1oup.png", "Download PNG", class = "btn-sm")
          )
        ), # row 2 col 1
        # row 2 col 2
        column(
          9, h4(htmlOutput("{prefix}d1oupTxt")),
          uiOutput("{prefix}d1oup.ui")
        ) # row 2 col 2
      ), # row 2
      hr()
    )
  )
) # End of tab 7
')
}

#' Write code for about page
#'
wrUIabout <- function() {
paste0('\n,
# about ----
tabPanel(
  "About",
  fluidRow(
    class = "container page",
    column(
      12,
      includeMarkdown("about.md")
    )
  )
)'
)
}

#' Write code for main block of ui.R
#' @param prefix file prefix
#' @param subst Conditional
#' @param ptsiz Point size
#' @param tabs Vector of tab numbers to include
#' @param about Should about page be added as a tab?
#' @rdname wrUImain
#' @export wrUImain
#'
wrUImain <- function(prefix, subst = "", ptsiz = "1.25", tabs = c(1,2,3,4,5,6,7), about = TRUE) {
  glue::glue(
    ifelse(1 %in% tabs, wrUItab1(), ""),
    ifelse(2 %in% tabs, wrUItab2(), ""),
    ifelse(3 %in% tabs, wrUItab3(), ""),
    ifelse(4 %in% tabs, wrUItab4(), ""),
    ifelse(5 %in% tabs, wrUItab5(), ""),
    ifelse(6 %in% tabs, wrUItab6(), ""),
    ifelse(7 %in% tabs, wrUItab7(), ""),
    ifelse(about, wrUIabout(), "")
  )
}

#' Write code for final portion of ui.R
#'
#' @rdname wrUIend
#' @export wrUIend
#'
wrUIend <- function() {
  glue::glue(
'
)
)
)\n
'
  )
}

#' Write code for google-analytics.html
#' @param gaID Google analytics tracking ID (e.g. "UA-123456789-0")
#'
#' @rdname wrUIga
#' @export wrUIga
#'
wrUIga <- function(gaID) {
  glue::glue(
    '<!-- Global site tag (gtag.js) - Google Analytics -->
      <script async src="https://www.googletagmanager.com/gtag/js?id={gaID}"></script>
      <script>\n",
      window.dataLayer = window.dataLayer || [];
    function gtag(){{dataLayer.push(arguments);}}
    gtag("js", new Date());
    
    gtag("config", "{gaID}");
    </script>\n'
  )
}

#' Write code for about. about.md
#' @rdname wrAbout
#' @export wrAbout
#'
wrAbout <- function(){
  paste('
### About

App description and author info.

')  
}

#' Write code for custom css. www/styles.css
#' @rdname wrCSS
#' @export wrCSS
#'
wrCSS <- function(){
  paste('
/* easyshiny */
/* custom css styles */

.page {
    margin: auto;
}

.tab-section {
  margin-bottom: 1.5em;
}

.input-panel {
  background-color: #f3f6f4;
  padding: 15px;
  border-radius: 6px;
}

a:hover,
a:focus,
a:active,
.navbar-default .navbar-nav>li>a:hover,
.navbar-default .navbar-nav>li>a:focus,
.navbar-default .navbar-nav>li>a:active,
.navbar-default .navbar-brand:hover,
.navbar-default .navbar-brand:focus,
.navbar-default .navbar-brand:active {
  color: #bcbcbc;
}

.btn {
  margin: 5px 0px;
}

.dt-buttons {
  margin: 1em 0em;
}

')  
}
