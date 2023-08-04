packages = c(
    "passport", 
    "dplyr",
    "ranger", 
    "PEIP",
    "geojsonio",
    "broom",
    "missForest",
    "tidyr",
    "data.table",
    "countrycode",
    "caret",
    "quantmod",
    "lubridate",
    "DALEX",
    "ggplot2",
    "gridExtra",
    "caTools",
    "stringr",
    "viridis",
    "grid",
    "reshape2",
    "colorspace",
    "dichromat",
    "gtable",
    "gganimate",
    "transformr",
    "wesanderson",
    "sjPlot",
    "rgdal")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
