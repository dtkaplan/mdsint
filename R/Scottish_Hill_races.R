#' Data from Scottish Hill races
#' 
#' Winners in Scottish Hill races in the mid-2010s. Most races have divisions for males and for females. The winner in 
#' each division is listed. 
#' 
#' @docType data
#' @name Hill_runners
#' @keywords datasets
#' @usage data(Hill_runners)
#' 
#' @source The data were scraped from <http://www.scottishhillracing.co.uk>. See details in the package file `inst/rawdata/ScottishHillRacing-get-data.Rmd`.
#' 
#' @format 2224 winners of Scottish Hill races
#' * `year` -- integer: the year the race was held
#' * `sex` -- the division of the winner, male or female
#' * `name` -- character string listing the name of the winner
#' * `time` -- the winning time, in seconds
#' * `race`` -- character string with the name of the race
#' 
NA

#' Characteristics of the Scottish Hill races themselves.
#' 
#' `Hill_races` gives a description of the physical characteristics of each race. Most races were run for several years. There's a male and 
#' a female winner for each race year.
#' 
#' @docType data
#' @name Hill_races
#' @keywords datasets
#' @usage data(Hill_races)
#' @format  154 races for which the winners are reported in `Hill_runners`. Variables:
#' * `distance`. The total length of the race, in km.
#' * `climb`. The vertical climb in the race, in m. A negative number means the race was run downhill rather than the more common uphill race.
#' * `race`. character string with the name of the race
#' 
#' @source The data were scraped from <http://www.scottishhillracing.co.uk>. See details in the package file `inst/rawdata/ScottishHillRacing-get-data.Rmd`.


NA
