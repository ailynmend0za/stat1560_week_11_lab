#function 1
#'Estimate NHPP arrival rates
#'
#'@param data data frame containing info on start_station, end_station, 
#'start_time, and end_time
#'@param window_start the start of the window, inclusive
#'@param window_end the end of the window, exclusive
#'
#'@return data frame with columns origin, destination, hour, observed_rate,
#'availability, and rate

estimate_arrival_rates <- function(data, window_start, window_end) {
  window_start <- as.numeric(ymd_hms(window_start))
  window_end <- as.numeric(ymd_hms(window_end))
  #input data checks
  stopifnot(
    is.data.frame(data),
    all(c("start_station", "end_station", "start_time", "end_time") %in% names(data)),
    !is.na(window_start), !is.na(window_end), window_end > window_start
  )
  #determine window period in days
  days <- (window_end - window_start) / 86400

  #convert stations to characters
  data$start_station <- as.character(data$start_station)
  data$end_station <- as.character(data$end_station)
  
  #modified from week 10
  events <- data %>%
    mutate(event_type = ifelse(start_station == "R" | end_station == "R", "R", "C")) %>%
    pivot_longer(cols = c(start_station, end_station), 
                 names_to = "event",
                 values_to = "station") %>%
    filter(station != "R") %>%
    mutate(
      time = as.numeric(ymd_hms(ifelse(event == "start_station", start_time, end_time))),
      inventory_change = ifelse(event == "start_station", -1, 1),
      order = case_when(
        event_type == "R" & inventory_change == 1 ~ 1,
        event_type == "C" & inventory_change == -1 ~ 2,
        event_type == "C" & inventory_change == 1 ~ 3,
        event_type == "R" & inventory_change == -1 ~ 4
      )
    ) %>%
    arrange(time, order)
  
    
  
}