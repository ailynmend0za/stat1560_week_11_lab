#function 1
#'Estimate NHPP arrival rates
#'
#'@description estimate the rate of user requests to travel from an origin station 
#'to a destination station, in requests per hour
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
  #input data checks: check that data is a data frame, window start/end times 
  #not missing, and that end comes after start time
  stopifnot(
    is.data.frame(data),
    all(c("start_station", "end_station", 
          "start_time", "end_time") %in% names(data)),
    !is.na(window_start), !is.na(window_end), window_end > window_start
  )
  
  #convert stations to characters
  data$start_station <- as.character(data$start_station)
  data$end_station <- as.character(data$end_station)
  
  #modified from week 10
  events <- data %>%
    mutate(event_type = ifelse(start_station == "R" | end_station == "R", "R", 
                               "C")) %>%
    pivot_longer(cols = c(start_station, end_station), 
                 names_to = "event",
                 values_to = "station") %>%
    filter(station != "R") %>%
    mutate(
      time = as.numeric(ymd_hms(ifelse(event == "start_station", start_time, 
                                       end_time))),
      inventory_change = ifelse(event == "start_station", -1, 1),
      order = case_when(
        event_type == "R" & inventory_change == 1 ~ 1,
        event_type == "C" & inventory_change == -1 ~ 2,
        event_type == "C" & inventory_change == 1 ~ 3,
        event_type == "R" & inventory_change == -1 ~ 4
      )
    ) %>%
    arrange(time, order)
  
  #obtain all station names
  stations <- unique(events$station)
  #create availability matrix
  availability <- matrix(0,length(stations), 24, 
                         dimnames = list(stations, as.character(0:23)))
  
  #determine how many hours each station has at least one bike available 
  # during each hour
  for (s in stations) {
    #create smaller data frame that has events for the current station
    station_events <- events %>% filter(station == s)
    inventory <- sum(station_events$inventory_change[station_events$time < window_start])
    previous_time <- window_start
    station_events <- station_events %>% filter(time >= window_start, time < window_end)
    
    if (nrow(station_events) > 0) {
    for(i in 1:nrow(station_events)) {
      event_time <- station_events$time[i]
      if (inventory > 0) {
        current <- previous_time
        while (current < event_time) {
    #determine what hour of day the time is from; determine number of seconds 
    #that have passed within the current day, then divide by 3600 to convert 
    #from seconds to hours; round the value down to the nearest whole number
          hour_of_day <- floor ((current %% 86400)/3600)
     #determine when interval should end, 
     #either at the start of the next event, or the end of the current hour
          end <- min(event_time, (floor(current/3600)+1)*3600)
     #update availability matrix for the current station and hour
          availability[s,as.character(hour_of_day)] <- 
            availability[s,as.character(hour_of_day)] + (end-current)/3600
          current <- end
        }
      }
      #update inventory and time
      inventory <- inventory + station_events$inventory_change[i]
      previous_time <- event_time
      }
    }
    if (inventory > 0) {
      current <- previous_time
      while(current < window_end) {
        hour_of_day <- floor((current %% 86400)/3600)
        end <- min(window_end, (floor(current/3600)+1)*3600)
        availability[s,as.character(hour_of_day)] <- 
          availability[s,as.character(hour_of_day)] + (end-current)/3600
        current <- end
      }
    }
  }
  
  #determine how many customer trips
  trips <- data %>%
    filter(start_station != "R", end_station != "R") %>%
    mutate(time = as.numeric(ymd_hms(start_time)),
           hour = hour(ymd_hms(start_time)))%>%
    filter(time >= window_start, time < window_end) %>%
    #determine how many trips occurred for each combination of these:
    count(origin = start_station, 
          destination = end_station, 
          hour, 
          name = "departures")
    
  #determine observation hours
  hours <- rep(0,24)
  current <- window_start
  
  while(current < window_end) {
    #determine what hour of day you are in
    hour_of_day <- floor((current %% 86400)/3600)
    end <- min(window_end, (floor(current/3600) + 1)*3600)
    #need to use hours_of_day + 1 because hours begin at 0
    hours[hour_of_day +1] <- hours[hour_of_day+1] + (end-current)/3600
    current <- end
  }
  
  #create column of available hours
  trips$available_hours <- rep(0, nrow(trips))
  if (nrow(trips) > 0) {
  for (i in 1:nrow(trips)) {
    origin <- trips$origin[i]
    hour_of_day <- trips$hour[i]
#using the availability matrix, retrieve the value corresponding to the station 
#and the hour of day
    trips$available_hours[i] <- availability[origin,as.character(hour_of_day)]
    }
  }
  
#calculate observed rate, availability, rate
  trips <- trips %>%
#determine observed rate by dividing departures by total number of hours
    mutate(observed_rate = departures/hours[hour+1],
#determine proportion of time that bikes were available
           availability = available_hours / hours[hour+1],
#determine the rate of departures over available number of hours
           rate = ifelse(available_hours > 0, 
                         departures/available_hours, NA)) %>%
    select(origin, destination, hour, observed_rate, availability, rate)
  return(trips)
}

#function 2
#' Simulate NHPP arrivals
#' 
#' @description simulate customer requests using thinning
#' @param rates data frame with hourly rates from the previous function
#' @param start_time the beginning of the window time, inclusive
#' @param end_time the end of the window time, exclusive
#' 
#' @return data frame with columns origin, destination, and arrival_time

simulate_arrivals <- function(rates, start_time, end_time) {
  start_time <- as.numeric(ymd_hms(start_time))
  end_time <- as.numeric(ymd_hms(end_time))
  
  #input data checks: check that data is a data frame, start/end times 
  #not missing, and that end comes after start time
  stopifnot(
    is.data.frame(rates),
    all(c("origin", "destination", "hour", "rate") %in% names(rates)), 
    !is.na(start_time), !is.na(end_time), end_time > start_time
  )
  
  #find each combination of origin-destination pairs
  pairs <- unique(rates[,c("origin","destination")])
  
  #create data frame to store the arrival info
  arrivals <- data.frame(
    origin = character(),
    destination = character(),
    arrival_time = numeric()
  )
  
  #loop for each pair
  for (i in 1:nrow(pairs)) {
    origin <- pairs$origin[i]
    destination <- pairs$destination[i]
    pair_rates <- rates %>%
      filter(origin == pairs$origin[i],
             destination == pairs$destination[i])
    #store max rate
    M <- max(pair_rates$rate)
    if (M > 0) {
      #create empty list of candidate times
      candidate_times <- numeric()
      #set candidate time to start time
      candidate_time <- start_time
      while (candidate_time < end_time) {
        wait <- rexp(1,rate = M)*3600
        candidate_time <- candidate_time + wait
        if (candidate_time < end_time) {
          candidate_times <- c(candidate_times,candidate_time)
        }
      }
      for (u in candidate_times) {
          #find hour of arrival and rate
          hour_of_day <- floor((u %%86400)/3600)
          hourly_rate <- pair_rates$rate[pair_rates$hour == hour_of_day]
          if (length(hourly_rate) > 0 && runif(1) < hourly_rate/M) {
            arrivals <- rbind(arrivals, data.frame(origin = origin,
                                                   destination = destination, 
                                                   arrival_time = u))
          }
      }
    }
  }
  arrivals <- arrivals %>%
    arrange(arrival_time)
  return(arrivals)
}

