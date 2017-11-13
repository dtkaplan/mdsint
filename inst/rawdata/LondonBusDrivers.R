Orig_data <- tribble(
  ~ age_min, ~ age_max, ~ job, ~ nworkers, ~ nevents, ~ day3, ~ month3, ~ year3,
  35,  39, "conductor",  2955,  0,  0,  0,  0,
  40,  44, "conductor",  1856,  0,  0,  0,  0,
  45,  49, "conductor",  1438,  0,  0,  0,  0,
  50,  54, "conductor",  1322,  4,  1,  1,  1,
  55,  59, "conductor",  1199, 12,  1,  2,  4,
  60,  64, "conductor",   813,  9,  2,  3,  4,
  35,  39, "driver",     3179,  2,  0,  1,  1,
  40,  44, "driver",     3002,  6,  2,  2,  2,
  45,  49, "driver",     3213, 14,  5,  6,  8,
  50,  54, "driver",     2626, 15,  5,  7, 10,
  55,  59, "driver",     1892, 17,  6,  9, 12,
  60,  64, "driver",     1442, 22,  6, 11, 14
)
make_morris_data <- function(Raw) {
  Res <- NULL
  for (k in 1:nrow(Raw)) {
    This_group <- with(Raw,
                       data_frame(age = round(runif(nworkers[k], min=age_min[k], max = age_max[k])),
                                  job = job[k], attack = "no", 
                                  day3 = "survived", month3 = "survived", year3 = "survived"))
    nsurvived <- with(Raw, nevents[k] - year3[k])
    All <- 1:Raw$nworkers[k]
    survived <- sample(All, size = nsurvived)
    remaining <- setdiff(All, survived)
    day3 <- sample(remaining, size = Raw$day3[k])
    remaining <- setdiff(remaining, day3)
    month3 <- sample(remaining, size = (Raw$month3[k] - Raw$day3[k]))
    remaining <- setdiff(remaining, month3)
    year3 <- sample(remaining, size = (Raw$year3[k] - Raw$month3[k]))
    This_group$attack[c(survived, day3, month3, year3)] <- "attack"
    This_group$day3[day3] <- "died"
    This_group$month3[c(day3, month3)] <- "died"
    This_group$year3[c(day3, month3, year3)] <- "died"
    Res <- rbind(Res, This_group)
  }
  
  Res
}
Bus_workers <- make_morris_data(Orig_data)
save(Bus_workers, file = "data/Bus_workers.rda")
