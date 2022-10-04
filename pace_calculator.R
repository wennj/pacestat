paceCalculator <- function(raceDistance, raceTimeVector, raceUnits = "km"){
  
  pace_KM <- as_hms(raceTimeVector*60/raceDistance/60)
  
  pace_KM <- round_hms(pace_KM, 1)
  
  return(pace_KM)
}