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
  
  stations <- unique(events$station)
  availability <- matrix(0,length(stations), 24, dimnames = list(stations, as.character(0:23)))
  
  #determine how many hours each station has at least one bike available during each hour
  for (s in stations) {
    e <- events %>% filter(station == s)
    inventory <- sum(e$inventory_change[e$time < window_start])
    previous_time <- window_start
    e <- e %>% filter(time >= window_start, time < window_end)
    
    for(i in 1:nrow(e)) {
      event_time <- e$time[i]
      if (inventory > 0) {
        current <- previous_time
        while (current < event_time) {
          h <- floor ((current %% 86400)/3600)
          finish <- min(event_time, (floor(current/3600)+1)*3600)
          availability[s,as.character(h)] <- availability[s,as.character(h)] + (finish-current)/3600
          current <- finish
        }
      }
      inventory <- inventory + e$inventory_change[i]
      previous_time <- event_time
    }
    if (inventory > 0) {
      current <- previous_time
      while(current < window_end) {
        h <- floor((current %% 86400)/3600)
        finish <- min(window_end, (floor(current/3600)+1)*3600)
        availability[s,as.character(h)] <- availability[s,as.character(h)] + (finish-current)/3600
        current <- finish
      }
    }
  }
  
  #determine how many customer trips
  trips <- data %>%
    filter(start_station != "R", end_station != "R") %>%
    mutate(time = as.numeric(ymd_hms(start_time)),
           hour = hour(ymd_hms(start_time)))%>%
    filter(time >= window_start, time < window_end) %>%
    count(origin = start_station, 
          destination = end_station, 
          hour, 
          name = "departures")
    
  
  hours <- rep(0,24)
  current <- window_start
  
  while(current < window_end) {
    h <- floor((current %% 86400)/3600)
    finish <- min(window_end, (floor(current/3600) + 1)*3600)
    hours[h+1] <- hours[h+1] + (finish-current)/3600
    current <- finish
  }
  
  trips$available_hours <- 0
  for (i in 1:nrow(trips)) {
    o <- trips$origin[i]
    h <- trips$hour[i]
    trips$available_hours[i] <- availability[o,as.character(h)]
  }
  
  trips <- trips %>%
    mutate(observed_rate = departures/hours[hour+1],
           availability = available_hours / hours[hour+1],
           rate = ifelse(available_hours > 0, departures/available_hours, NA)) %>%
    select(origin, destination, hour, observed_rate, availability, rate)
  return(trips)
}