# stat1560_week_11_lab

#this repository contains two functions. 

#the first function estimates arrival rates from bike-share data. this function will be
#estimate_arrival_rates(data, window_start, window_end) and will be used to reconstruct 
#station inventories and measure bike availability. it will take in trip data and an 
#observation window and return the estimated hourly rates. it will count customer departures 
#from origin stations to their destination stations, calculate the 
#fraction of each hour that each origin station has at least one bike, and 
#estimate the request rate by adjusting observed departures for the time bikes were available. 
#it will return a table with columns origin, destination, hour (0-23), observed_rate, availability, 
#and rate.

#the second function simulates arrivals using those rates. this function will be 
#simulate_arrivals(rates, start_time, end_time). this function will use the first 
#function's rate table to simulate customer requests during the half-open interval 
#[start_time, end_time). it will simulate each origin-destination pair independently
#using thinning. it will return columns origin, destination, and arrival_time.

#how to run an example?
#to use these functions and run an example, the user should load in the R script, 
#import the bike-share data, then call the function estimate_arrival_rates() with the desired
#input values/observation windows. next, the user should call the function simulate_arrivals() 
#using the rates estimated from the previous function. 