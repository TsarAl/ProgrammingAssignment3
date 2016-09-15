rankall<- function(outcome, rank="best", output="Hospital.Name") 
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
	if(!any(colnames(OutcomeData)==output)) stop("invalid column to display");

	# Get all used States from OutcomeData
	States<-sort(unique(OutcomeData$State));
	
	i<-1; OutputName=character(); OutputState=character();
	for (state in States) 
	{
		stateData<-OutcomeData[grep(state, OutcomeData$State),]

		# Order 'outcome' as numeric, include 'output' as second var and isolate first observation
		rtnVal<-stateData[order(suppressWarnings(as.numeric(stateData[, varMortality[outcome]])),stateData[,output], na.last=NA), ]

		# Define 'rank' based on input 
		rankx<-rank;
		if(rank=="worst") rankx<-nrow(rtnVal)
		if(rank=="best") rankx<-1
		if(rank>nrow(stateData)) rankx<-nrow(rtnVal)

		OutputName[i] <-as.character(rtnVal[rankx,output]);
		OutputState[i]<-as.character(state);
		i<-i+1;
	}

	rtnVal<-data.frame(OutputName, OutputState);
	colnames(rtnVal) <- c(output,"State");
	return(rtnVal);
}

