best <- function(state, outcome) 
{
	OutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses="character");
	varPreFix<-"Hospital.30.Day.Death..Mortality..Rates.from.";
	varInput<-c("heart attack","heart failure","pneumonia");
	varSuffix<-c("Heart.Attack","Heart.Failure","Pneumonia");
}
