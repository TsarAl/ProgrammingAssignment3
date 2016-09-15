source("hospitalSelector.R");

rankhospital <- function(state, outcome, rank)
{
	hospitalSelector(state, outcome, output="Hospital.Name", rank=rank)
}
