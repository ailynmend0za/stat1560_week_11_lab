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
