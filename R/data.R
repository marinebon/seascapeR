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

#' SeaScape Global Classes
#'
#' Average properties of Seascape Global Classes.
#'
#' @format A table of average values per class:
#' \describe{
#'   \item{`CLASS`}{unique integer identifier of the Class}
#'   \item{`NAME`}{descriptive name}
#'   \item{`SST  (°C)`}{sea surface temperature}
#'   \item{`SSS (psu)`}{sea surface salinity}
#'   \item{`ADT (m)`}{TBD}
#'   \item{`ICE (%)`}{percent ice, if present}
#'   \item{`CDOM (m^-1)`}{colored dissolved organic matter}
#'   \item{`CHLA (mg m^-3)`}{Chlorophyll a}
#'   \item{`NFLH (W m^-2 um^-1 sr^-1)`}{TBD}
#'   \item{`NFLH:CHL`}{TBD}
#'   \item{`LATITUDE`}{latitude}
#'   \item{`DOMINANT HEMISPHERE`}{TBD}
#'   \item{`DOMINANT SEASON`}{TBD}
#' }
#' @source \url{https://sanctuaries.noaa.gov/library/imast_gis.html}
#' @concept data
"ss_gl_classes"
