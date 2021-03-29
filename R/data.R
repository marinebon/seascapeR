#' National Marine Sanctuaries list
#'
#' National Marine Sanctuaries list of short code and full name.
#'
#' @format A list with keys and values indicating:
#' \describe{
#'   \item{`code`}{Short lower-case Sanctuary code used by `get_url_ply()`.}
#'   \item{`name`}{Full name, capital case.}
#' }
#' @source \url{https://sanctuaries.noaa.gov/library/imast_gis.html}
#' @concept data
"nms"

#' SeaScape Global Classes information with average values per Class
#'
#' Average properties of Seascape Global Classes.
#'
#' @format A table of average values per class:
#' \describe{
#'   \item{`CLASS`}{unique integer identifier of the Class}
#'   \item{`NAME`}{descriptive name}
#'   \item{`SST (°C)`}{sea surface temperature in degrees Celsius}
#'   \item{`SSS (psu)`}{sea surface salinity in Practical Salt Units}
#'   \item{`ADT (m)`}{absolute dynamic topography in meters}
#'   \item{`ICE (%)`}{ice as percent cover, if present}
#'   \item{`CDOM (m^-1)`}{colored dissolved organic matter per square meter}
#'   \item{`CHLA (mg m^-3)`}{Chlorophyll _a_ per cubic meter}
#'   \item{`NFLH (W m^-2 um^-1 sr^-1)`}{normalized fluorescence line height}
#'   \item{`NFLH:CHL`}{normalized fluorescence line height chlorophyll component}
#'   \item{`LATITUDE`}{latitude}
#'   \item{`DOMINANT HEMISPHERE`}{dominant hemisphere}
#'   \item{`DOMINANT SEASON`}{dominant season} }
#' @source CLASS comment from
#'   \href{https://cwcgom.aoml.noaa.gov/erddap/griddap/noaa_aoml_4729_9ee6_ab54.html}{ERDDAP - Monthly Global Seascapes - Data Access Form}
#' @concept data
"ss_gl_classes"

#' SeaScape Variables
#'
#' Basic information about variables used to inform SeaScape Classes
#'
#' @format A table of average values per class:
#' \describe{
#'   \item{`var`}{original variable name in \code{\link{ss_gl_classes}}}
#'   \item{`v`}{short variable name, useful for file naming}
#'   \item{`description`}{text description}}
#' @concept data
"ss_vars"
