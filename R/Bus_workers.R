#' Coronary heart disease in London transport workers
#' 
#' In the 1950s, epidemiologist Jerry Morris explored the hypothesis that
#' lack of exercise is associated with higher levels of coronary heart disease. One of his
#' first systematic observations of this was the different levels of disease in bus drivers
#' (who sit while they work) and bus conductors (who, in the double-decker London busses, go 
#' up and downstairs frequently while collecting tickets). These data are a reverse-engineered version
#' of the summary provided in the main table of Morris's paper.
#' 
#' @docType data
#' @name Bus_workers
#' @keywords datasets
#' @usage data(Bus_workers)
#' 
#' @source The summary data are in J.N. Morris and P.A.B. Raffle (1954) "Coronary heart disease in transport
#' workers: A progress report" *Brit. J. industr. Med.*, **11**:260-264
#' 
#' @format 24937 transport workers in London busses.
#' * `age` -- age of the worker in 1949-1950
#' * `job` -- whether the worker was a bus driver or conductor
#' * `event` -- whether the worker presented with medical symptoms of coronary heart disease
#' * `day3` -- whether the worker survived the first 3 days after the onset of symptoms
#' * `month3` -- whether the worker survived the first 3 months after the onset of symptoms
#' * `year3` -- whether the worker survived the first 3 years after the onset of symptoms.
#' Note that a worker who died in the first three days by necessity failed to survive the first 
#' three months or years. Workers who did not have an attack are listed as having
#' survived the three years of follow-up.
#' 
NA


