
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# seascapeR <a href='https://marinebon.org/seascapeR'><img src='man/figures/logo.svg' align="right" height="150" /></a>

[![pkgdown](https://github.com/marinebon/seascapeR/workflows/pkgdown/badge.svg)](https://github.com/marinebon/seascapeR/actions?query=workflow%3Apkgdown)

<!--
[![R build status](https://github.com/mitchelloharawild/icon/workflows/R-CMD-check/badge.svg)](https://github.com/mitchelloharawild/icon/actions?workflow=R-CMD-check)
[![Coverage status](https://codecov.io/gh/mitchelloharawild/icon/branch/master/graph/badge.svg)](https://codecov.io/gh/mitchelloharawild/icon?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/icon)](https://cran.r-project.org/package=icon)
-->

<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/icon?color=brightgreen)](https://cran.r-project.org/package=icon) -->

The `seascapeR` package provides functions for fetching, analyzing and
visualizing
[Seascapes](https://coastwatch.noaa.gov/cw/satellite-data-products/multi-parameter-models/seascape-pelagic-habitat-classification.html),
a global and regional sea water classification product derived from
satellite imagery by [Maria Kavanaugh
(OSU)](https://ceoas.oregonstate.edu/people/maria-kavanaugh) and
[MarineBON.org](https://marinebon.org).

## Install

``` r
remotes::install_github("marinebon/seascapeR")
```

## Use

Load the library and map Seascape classes using a web map server (wms)
that loads image tiles (not data) interactively (zoom, pan) from R.

``` r
library(seascapeR)

map_seascape_wms("2020-11-15", ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10)
```

![](man/figures/README-map_seascape_wms-1.png)<!-- -->

Get Seascape data for a year and plot Seascape classes over time.

``` r
r_mo_2019 <- get_seascape_data(
  ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10, 
  dataset = "global_monthly", var = "CLASS", 
  date_beg = "2019-01-01", date_end = "2020-01-01")
#> info() output passed to x; setting base url to: https://cwcgom.aoml.noaa.gov/erddap/

# here's the raster stack
r_mo_2019
#> class      : RasterStack 
#> dimensions : 401, 401, 160801, 13  (nrow, ncol, ncell, nlayers)
#> resolution : 0.05, 0.05  (x, y)
#> extent     : -91.35, -71.3, 14.5, 34.55  (xmin, xmax, ymin, ymax)
#> crs        : +proj=longlat +datum=WGS84 +no_defs 
#> names      : CLASS_2019.01.15, CLASS_2019.02.15, CLASS_2019.03.15, CLASS_2019.04.15, CLASS_2019.05.15, CLASS_2019.06.15, CLASS_2019.07.15, CLASS_2019.08.15, CLASS_2019.09.15, CLASS_2019.10.15, CLASS_2019.11.15, CLASS_2019.12.15, CLASS_2020.01.15 
#> min values :                3,                3,                3,                3,                3,                3,                3,                3,                3,                3,                3,                3,                3 
#> max values :               27,               28,               28,               28,               28,               28,               27,               28,               28,               28,               27,               27,               27

# map the first layer
map_seascape_raster(
  raster::raster(r_mo_2019, 1))
```

![](man/figures/README-plot_seascape_ts-1.png)<!-- -->

``` r
# plot the whole time series
plot_seascape_ts(r_mo_2019, show_legend = "always")
```

![](man/figures/README-plot_seascape_ts-2.png)<!-- -->

Note that when you run the code above in any of these R environments,
you get an interactive visualization:

1.  [RStudio](https://rstudio.com/products/rstudio/download/) Console;
2.  knitted html from an [Rmarkdown](https://rmarkdown.rstudio.com/)
    document; or
3.  a [Shiny](https://shiny.rstudio.com/) app.

The interactive map allows you to zoom and pan. The time series has
sliders to change the time window and hover over to get values.

For more on how to use the `seascapeR` functions and view these
interactive visualizations, see the [Getting
Started](articles/seascapeR.html) article.

For more on how to contribute to `seascapeR` package development, see
[CONTRIBUTE.md](https://github.com/marinebon/seascapeR/blob/main/CONTRIBUTE.md).
