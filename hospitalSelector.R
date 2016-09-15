hospitalSelector<- function(state, outcome, output="Hospital.Name", rank=1) 
{
	# Read CSV File. Define column labels and 'outcome' input
	OutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses="character");
	varPrefix<-"Hospital.30.Day.Death..Mortality..Rates.from.";
	varSuffix<-c("Heart.Attack","Heart.Failure","Pneumonia");
	varInput<-c("heart attack","heart failure","pneumonia");

	# Create full column label vars and add 'outcome' input as label
	varMortality<-setNames(unlist(lapply(varPrefix,paste,varSuffix,sep="")),varInput);

	# Check for invalid inputs ('output' should be checked for but is not part of the assignment)
	if(is.na(varMortality[outcome])) stop("invalid outcome");
	if(!any(unique(OutcomeData$State)==state)) stop("invalid state");
	if(!any(colnames(OutcomeData)==output)) stop("invalid column to display");

	# Use only data from 'state' defined in input
	stateData<-OutcomeData[grep(state, OutcomeData$State),]

	# Order 'outcome' as numeric, include 'output' as second var and isolate first observation
	rtnVal<-stateData[order(suppressWarnings(as.numeric(stateData[, varMortality[outcome]])),stateData[,output], na.last=NA), ]

	# Define 'rank' based on input 
	if(rank=="worst") rank<-nrow(rtnVal)
	if(rank=="best") rank<-1
	if(rank>nrow(stateData)) return(NA)

	return(rtnVal[rank,output])
}


rankhospital <- function(state, outcome, rank)
{
	workhorse(state, outcome, output="Hospital.Name", rank=rank)
}
