source("hospitalSelector.R")

best <- function(state, outcome)
{
	hospitalSelector(state, outcome, output="Hospital.Name", rank=1)
}
