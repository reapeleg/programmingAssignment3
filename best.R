########################################################################################################################################
# Function best returns from outcome-of-care-measures.csv a character vector with best hospital name, 
# of lowest 30-day mortality rate for one outcome in one state. 
# A Valid outcome is :"heart attack", "heart failure", or "pneumonia".
# Hospital name is retrieved from Hospital.Name variable (column 2 in .csv). 
# ---------------------------------------------------------------------------------------------------------------------------------------
# in params: 
#---------------------------------------------------------------------------------------------------------------------------------------
# 1) state - 2 letters abraviation of state name (column 7 in .csv).
# 2) outcome - can be "heart attack", "heart failure", or "pneumonia" (columns 11,17,23 in .csv)
# ---------------------------------------------------------------------------------------------------------------------------------------
# remarks: 
#---------------------------------------------------------------------------------------------------------------------------------------
# 1) Hospitals with no data for given outcome are excluded from results.
# 2) If there is a tie between hospitals for given outcome, first name in alphabetical order is returned.
# 3) If an invalid outcome value is passed, the stop function throws error "invalid outcome". 
#    If an invalid state value is passed, the stop function throws error "invalid state".
########################################################################################################################################

# !before calling function:  setwd("C:/Users/reape/OneDrive/docs/study/DS/r/progAssignment3/submit") 
best <- function(state,outcome)
{
    #-------------------------------------------------------------------------------------------------------------------------------- 
    # initialize variables
    #--------------------------------------------------------------------------------------------------------------------------------
    #empty char vector (length=0) for best hospital name
    bestHospitalName <- character(0)
    #empty data frame for hospitals data 
    hospData <- data.frame(a ="NULL",b = "NULL",c = "NULL") # all columns may include chars in this csv
    #initialize columnClasses and columnNames with nulls (specific types and names will be set once we know which columns we need)
    columnClasses <- rep("NULL", 46);
    columnNames <- rep("NULL", 46);
    outcomeColNum <- integer(0) # empty vector for column number 
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
            columnNames[c(2,7,outcomeColNum)] <- c("name","state","rate")  # set column names
            fileName <- c("outcome-of-care-measures.csv")  
            hospData <- read.csv(fileName, header = TRUE,sep=",",dec = ".",stringsAsFactors=FALSE,colClasses=columnClasses,col.names = columnNames)
            #remove NA from data:
            hospData <- hospData[is.na(hospData[,1])==F,]
            #remove rows with un numeric data: create logical vector where T stands for numeric value in the rate column  
            numericRows <- !is.na(as.numeric(as.character(hospData[,3]))) #hospData[is.numeric(hospData[,1])==F,] checks the whole vector for being numeric or not...
            hospData <- hospData[numericRows,]
            tryCatch(#start try catch 2 function
            {   #assert state param is valid:
                if (tolower(state) %in% tolower(hospData$state))
                {   #sub set the state param data:
                    hospData <- hospData[hospData$state==state,]
                    #convert the rate column from character to numeric
                    hospData$rate <- as.numeric(hospData$rate)
                    #get the minimal death rate
                    minRate <- min(hospData[,3])
                    #get hospital data with best minRate
                    bestHospitals <- hospData[hospData$rate==minRate,]
                    #add a new column ranking selected hospital names
                    bestHospitals[ , "nrank"] <- rank(bestHospitals[,1],ties.method="first")
                    #get the minimal ranked hospital name:
                    bestHospitalName <- bestHospitals[bestHospitals$nrank==min(bestHospitals[,4]),"name"]
                    return(bestHospitalName)
                    #return (hospData)
                }# if end
                # else raise error action for invalid state:
                else stop("invalid state")
            }# try section 2 end
            )# trycatch 2 function end
        }# if end
        #  else raise error action for invalid action:
        else stop("invalid outcome")
    }# try section end
    #define the catch (error) function
    ,error=function(errMsg){cat("ERROR :",conditionMessage(errMsg), "\n")}
    )# trycatch function end
}#end function


