#' get erddap data for salmon forecase using rerddap
#'
#' @param dataset_id the dataset_id form erddap
#' @param gridded logical, gridded or now
#' @param min_lat minimum latitude
#' @param max_lat maximum latitude
#' @param min_lon minimum longitude
#' @param max_lon maximum longitude
#' @param min_year minimum year
#' @param max_year maximum year
#'
#' @return a list with the info, description, and data
#'
query_erddap <- function(dataset_id,
                         gridded,
                         min_lat,
                         max_lat,
                         min_lon,
                         max_lon,
                         min_year,
                         max_year,
                         stride = 2) {
  # get information on layer
  erd_info <- rerddap::info(dataset_id)
  
  # figure out if longitude is in 0-360 or east-west
  lon_form <-
    erd_info$alldata$longitude$value[erd_info$alldata$longitude$attribute_name == "actual_range"] %>%
    stringr::str_replace_all(" ", '') %>%
    str_split(",", simplify = TRUE) %>%
    map_dbl(as.numeric)
  
  if (min(lon_form) > 0) {
    min_lon <- 360 + min_lon
    
    max_lon <- 360 + max_lon
    
  }
  
  # figure out date range in the data
  date_range <-
    erd_info$alldata$time$value[erd_info$alldata$time$attribute_name == "actual_range"] %>%
    stringr::str_replace_all(" ", '') %>%
    str_split(",", simplify = TRUE) %>%
    map( ~ lubridate::as_datetime(as.numeric(.x)) %>%
           lubridate::as_date())
  
  min_date <-
    pmax(date_range[[1]], lubridate::ymd(paste(min_year, "01", "01", sep = "-")))
  
  
  max_date <-
    min(date_range[[2]], lubridate::ymd(paste(max_year, "12", "31", sep = "-")))
  
  if (gridded) {
    erd_data <- rerddap::griddap(
      erd_info,
      time = c(min_date,
               max_date),
      latitude = c(min_lat, max_lat),
      longitude = c(min_lon, max_lon),
      stride = stride
    )
    
    temp <- erd_data$data
    
    temp <-  temp %>%
      dplyr::mutate(geometry = purrr::map2(longitude, latitude, ~ sf::st_point(x = c(.x, .y), dim = 'XY'))) %>%
      ungroup() %>%
      mutate(geometry = sf::st_sfc(geometry, crs =
                                     paste0(
                                       "+proj=longlat +datum=WGS84",
                                       ifelse(any(temp$lon > 180), " +pm=360dw", " +no_defs")
                                     ))) %>%
      sf::st_sf()
  } else {
    erd_data <- rerddap::tabledap(erd_info,
                                  time = c(lubridate::ymd(paste(
                                    min_year, "01", "01", sep = "-"
                                  )),
                                  lubridate::ymd(paste(
                                    max_year, "12", "31", sep = "-"
                                  ))))
    
    
  }
  
  
  out <- list(summary = erd_data$summary,
              data = temp,
              info = erd_info)
}