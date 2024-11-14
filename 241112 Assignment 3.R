###################### Part 1 ######################

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

###################### Part 2 ###################

best <- function(state, outcome){
        df <- read.csv("outcome-of-care-measures.csv", header = TRUE)
        colname <- paste('Hospital.30.Day.Death..Mortality..Rates.from.', outcome, sep="")
        df <- df[df$State == state, ]#df has been filtered for given state
       best_outcome <- min(df[[colname]], na.rm = TRUE) 
       # Above reports the minimum value of mortality of given outcome
       
       # To print invalid state name for invalid input
       if (!(state %in% df$State)) {
               stop("invalid state")
       }
       
       if (!(colname %in% colnames(df))) {
               stop("Invalid outcome name")
       }
       
       # now apply best outcome on the filtered df
       best_hospitals <- df$Hospital.Name[df[[colname]] == best_outcome]
        best_hospital <- sort(best_hospitals)[1]
       return(best_hospital)
        # To print invalid state name for invalid input
 
        
        #df only displays state relevant data
        # report name if state = state, mortality = min
}

# column 2 - hospital name
# column 11 - 30 day mortality rate from heart attack --> 15
# column 17 - 30 day mortality rate from heart failure --> 21
# column 23 - 30 day mortality rate from pneumonia --> 27


################### Part 3 ###########################

rankedhospital <- function(state, outcome, num){

        df <- read.csv("outcome-of-care-measures.csv", header = TRUE)
colname <- paste('Hospital.30.Day.Death..Mortality..Rates.from.', outcome, sep="")
df <- df[df$State == state, ]#df has been filtered for given state

df_state <- df[df$State == state & !is.na(df[[colname]]), ]

# Convert the outcome column to numeric
df_state[[colname]] <- as.numeric(df_state[[colname]])

# Rank hospitals by outcome and handle ties by hospital name alphabetically
ranked_df <- df_state[order(df_state[[colname]], df_state$Hospital.Name), ]

# Determine which hospital to return based on `num`
if (num == "best") {
        return(ranked_df$Hospital.Name[1])
} else if (num == "worst") {
        return(ranked_df$Hospital.Name[nrow(ranked_df)])
} else if (is.numeric(num) && num > 0 && num <= nrow(ranked_df)) {
        return(ranked_df$Hospital.Name[num])
} else {
        return(NA)  # Return NA if `num` is out of range
}}


################ Part 4 ##############################


rankall <- function(outcome, num = 1 ){
        df <- read.csv("outcome-of-care-measures.csv", header = TRUE)
        colname <- paste('Hospital.30.Day.Death..Mortality..Rates.from.', outcome, sep="")
        df[[colname]] <- as.numeric(df[[colname]], na.rm = TRUE)
        df_ranked <- df[order(df[[colname]]), ]
        if(num == "best"){
                return(df_ranked[1,c("Hospital.Name", "State")])}
                else if(num == "worst"){
                        return(df_ranked[nrow(df_ranked),c("Hospital.Name", "State")])
                }
                else{
                        return(df_ranked[1:num,c("Hospital.Name", "State")])
                }
        
}




















