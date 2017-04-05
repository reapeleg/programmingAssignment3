########################################################################################################################################
# Function rankall returns from outcome-of-care-measures.csv a 2 column dataframe containing hospital name per state with rank specified for an outcome 
# of lowest 30-day mortality rate of "heart attack", "heart failure", or "pneumonia".
# Hospital name is retrieved from Hospital.Name variable (column 2 in .csv). 
# ---------------------------------------------------------------------------------------------------------------------------------------
# in params: 
#---------------------------------------------------------------------------------------------------------------------------------------
# 1) outcome - can be "heart attack", "heart failure", or "pneumonia" (columns 11,17,23 in .csv)
# 2) num - a rank of hospital in sate for outcome. can be "best","worst" or an integer for specific rank (lower is better).
# ---------------------------------------------------------------------------------------------------------------------------------------
# remarks: 
#---------------------------------------------------------------------------------------------------------------------------------------
# 1) Hospitals with no data for given outcome are excluded from results.
# 2) In case num is bigger then number of hospitals in state for an outcome NA is returned for that state.
# 3) If there is a tie between hospitals for given outcome, first name in alphabetical order is returned.
# 4) If an invalid outcome value is passed, the stop function throws error "invalid outcome". 
#    If an invalid state value is passed, the stop function throws error "invalid state".
########################################################################################################################################
rankall <- function(outcome,num="best")
{
    #-------------------------------------------------------------------------------------------------------------------------------- 
    # initialize variables
    #--------------------------------------------------------------------------------------------------------------------------------
    #empty char vector (length=0) for best hospital name
    rankedHospitalName <- character(0)#empty data frame for hospitals data 
    hospData <- data.frame(a ="NULL",b = "NULL",c = "NULL") # all columns may include chars in this csv
    #initialize columnClasses and columnNames with nulls (specific types and names will be set once we know which columns we need)
    columnClasses <- rep("NULL", 46);
    columnNames <- rep("NULL", 46);
    outcomeColNum <- integer(0) # empty vector for column number of outcome
    #--------------------------------------------------------------------------------------------------------------------------------
    # if outcome is valid, read data:
    #--------------------------------------------------------------------------------------------------------------------------------
    outcome <- trimws(outcome, which = c("both", "left", "right")) #remove leading and tailing whitespaces from the outcome param
    tryCatch(#start try catch function
        {#start try section: assert the outcome param:
            if (tolower(outcome) %in% c("heart attack" , "heart failure" , "pneumonia")) #tolower(charV) <-> toupper(charV)
            {   # define csv columns with variables according to outcome parameter:
                outcomeColNum <- switch(outcome, "heart attack"=11, "heart failure"=17,23) #else 23 (pneumonia death rate).
                # replace "NULL" with "NA" in column numbers we need from csv:
                columnClasses[c(2,7,outcomeColNum)] <- "character"  # all columns may include chars (includingthe rates...)
                columnNames[c(2,7,outcomeColNum)] <- c("hospital","state","rate")  # set column names
                fileName <- c("outcome-of-care-measures.csv")  
                hospData <- read.csv(fileName, header = TRUE,sep=",",dec = ".",stringsAsFactors=FALSE,colClasses=columnClasses,col.names = columnNames)
                #remove NA from data:
                hospData <- hospData[is.na(hospData[,1])==F,]
                #remove rows with un numeric data: create logical vector where T stands for numeric value in the rate column  
                numericRows <- !is.na(as.numeric(as.character(hospData[,3]))) #hospData[is.numeric(hospData[,1])==F,] checks the whole vector for being numeric or not...
                hospData <- hospData[numericRows,]
                #convert the rate column from character to numeric
                hospData$rate <- as.numeric(hospData$rate)
                #sort hospDate by state, rate and name:
                hospData <- hospData[
                                    order(hospData[,2],hospData[,3], hospData[,1] ),
                                    ]
                # use transform to add a new rank column for outcome per state:
                hospData <- transform(hospData,        #ave splits the rate variable by state and apply the rank function to each such group of rates (x):
                                           stateRank = ave(rate, state,FUN = function(x) rank(x, ties.method = "first"))) # ! to rank more is better use -x
                # use transform to add a new column with the last (worst) rank for outcome per state:
                hospData <- transform(hospData,        #ave splits the rate variable by state and apply the rank function to each such group of rates (x):
                                      stateRankWorst = ave(stateRank, state,FUN = function(x) max(x))) # ! to rank more is better use -x
                # return the ranked hospital per state
                if (is.character(num)) {
                     if (num == "best") hospData <- hospData[hospData$stateRank==1,c("hospital","state")]
                     else if (num == "worst") hospData <- hospData[hospData$stateRank==hospData$stateRankWorst ,c("hospital","state")]
                     else hospData <- NA
                }
                else
                {
                    # get all states that dont have a rank as high (low) as num (all values extracted from the "state" column are comparable)
                    statesNA <- unique(hospData[hospData$stateRankWorst<num, "state"],incomparables = FALSE) 
                    hospNamesNA <- rep(NA,length(statesNA))
                    hospNA <- data.frame(hospital=hospNamesNA,state=statesNA)
                    
                    hospData <- hospData[hospData$stateRank==num, c("hospital","state")]
                    hospData <- rbind(hospData,hospNA) # rbind demands same column names in merged DF!
                    #sort hospDate by state
                    hospData <- hospData[order(hospData[,2]),]
                    #rename rows to states:
                    rownames(hospData) <- hospData[,2]
                }
                return (hospData)
            }# if end
            #  else raise error action for invalid action:
            else stop("invalid outcome")
        }# try section end
        #define the catch (error) function
        ,error=function(errMsg){cat("ERROR :",conditionMessage(errMsg), "\n")}
    )# trycatch function end
}#end function
