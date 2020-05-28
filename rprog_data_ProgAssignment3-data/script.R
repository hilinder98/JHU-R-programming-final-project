## the 2 commands below were given as to us as part of the assignment
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## be sure that the unzipped files are in your
## working directory. I had to change mine with the setwd() function. this function reads our csv file. the colClasses argument
## loads our columns as all containing character vectors, i believe. I am not sure what they would be loaded as if we werent
## to call this argument. its possible that if we didnt call this then we would load our columns in some other fucked up wa
## anyways. we'll see later in the assignment that we will have to coerce some columns to numeric in order to actually do 
## calculations
head(outcome) ## prints the first few rows or the outcome dataset

ncol(outcome) ## there are hella columns, as we can see from when call the head function. ncol returns the number of columns ther are


outcome[, 11] <- as.numeric(outcome[, 11]) ## Since we read in our data with the colClass = "character"
## all our columns are currently character vectors and we have to coerce to numeric with the as.numeric function if we want 
## to do any sort of calculation
## You may get a warning about NAs being introduced; that is okay > hist(outcome[, 11])
columnnames <- colnames(outcome) ## this will create a list of the column names. this is useful because our column names
## are very long strings and it doesnt make sense to spend so much time using the dollar sign operator
## now we can just print columnnames before we do an operation with a specific column and save some time
hist(outcome[, 11]) ## this creates a histogram of the 30 day death rate from heart attack


##Finding the best hospital in a state
##
##So, what we first need to do is be able to subset our data by state
california <- outcome[which(outcome[,7] == "CA"),] ## this created a new data fram that only includes observations from cali
california[, 11] <- as.numeric(california[, 11]) ## coerces our values for 30 day heart attack deaths to numeric so that we can
## be sure that we can do caluculations
##next, we need to be able to, within that subset of hospitals within a specified state, find the lowest 30 (minimum) day death rate for
## whatever disease we specify and the return the name of the hospital(s) that achieved that rate.
## 
best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## simple enough, just like in the example from part 1
    dataofinterest  <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]) # pneumonia
                       , stringsAsFactors = FALSE ) ## to make our lives a little easier, we can create a new data frame
                        ## that only includes the columns we care about. we will se that it will be really easy to rename our
                        ## columns now that we have a more manageable data frame. we have have to use the stringsAsFactors = False arg
                        ## in order to be able to rename the columns as we do in the next line of code. The issue of strings as factors
                        ## is complicated, but do keep in mind that when running into issues when augmenting data frames
                        ## your issue may lie in the fact that strings are factors. actually, you dont need to use stringAsFactor.
                        ## tbh i im just copying this shit and explaining as I go. In some other cases, though, when messing with
                        ## data frames it'll probably be a good idea to use the stringAsFactor arg.
    colnames(dataofinterest) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") ## renames the columns of our
    ## new data fram
    ## Check that state and outcome are valid
    if(!state %in% dataofinterest[, "state"]){ ## in plain words, this line could be read as: if the character argument typed into the best 
      ##  function is not the same as any of the character strings values stored in the in the "state" column
        stop('invalid state') ## we get the stop message
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
        } else {
            rowsforastate <- which(dataofinterest[, "state"] == state) ## speceficstatedata is a integer vector denoting
            ## the integer values corresponding to rows that have the character string of the state whose character string
            ## we entered as an arguent originally
            statespecificdata <- dataofinterest[rowsforastate, ]    ## creates a data frame that only includes rows for the state
            ## we care about
            outcomevals <- as.numeric(statespecificdata[,outcome]) ## creates a numeric vector containing a list of values
            ## corresponding to the death numbers for the disease specified
            min_val <- min(outcomevals, na.rm = TRUE) ## calculates the minimum value for outcomevals
            result <- statespecificdata[, "hospital"][which(outcomevals == min_val)]
            output  <- result[order(result)]
        }
    return(output)
            
    ## Return hospital name in that state with lowest 30-day death ## rate
}



rankhospital <- function(state, outcome, rank = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    dataofinterest   <- as.data.frame(cbind(data[, 2],  # hospital
                                data[, 7],  # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(dataofinterest) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (!state %in% dataofinterest[, "state"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(rank)) { ## if the value we enter for the arg rank is numeric, then
        statespecific <- which(dataofinterest[, "state"] == state) ## this call with the following filters to our state of interest
        statespecific1 <- dataofinterest[statespecific, ]                     # extracting dataframe for the called state
        statespecific1[, eval(outcome)] <- as.numeric(statespecific1[, eval(outcome)]) ## converts character vector
        ## to a numeric vector
        statespecific1 <- statespecific1[order(statespecific1[, eval(outcome)], statespecific1[, "hospital"]), ] ## orders our data frame
        ## first based on the level of outcome and then based on the alphabetical name of our hospital of interest
        output <- statespecific1[, "hospital"][rank] ## returns the the hospital in the data frame for the rank (which is a number) row
    } else if (!is.numeric(rank)){ ## if rank is not numeric
        if (rank == "best") {
            output <- best(state, outcome)
        } else if (rank == "worst") {
            statespecific <- which(dataofinterest[, "state"] == state)
            statespecific1 <- dataofinterest[statespecific, ]    
            statespecific1[, eval(outcome)] <- as.numeric(statespecific1[, eval(outcome)])
            statespecific1 <- statespecific1[order(statespecific1[, eval(outcome)], statespecific1[, "hospital"], decreasing = TRUE), ]
            ## by setting the decreasing arg, we flip the order of the list
            output <- statespecific1[, "hospital"][1] ## returns the first hospital in the data frame, which is the one with the worst outcome
        } else {
            stop('invalid rank')
        }
    }
    return(output)
}

rankall <- function(outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    dataofinterest   <- as.data.frame(cbind(data[, 2],  # hospital
                                data[, 7],  # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(dataofinterest) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    dataofinterest[, eval(outcome)] <- as.numeric(dataofinterest[, eval(outcome)])
    
    ## Check that state and outcome are valid
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(num)) { ## if we ender a number for arg num
        by_state <- with(dataofinterest, split(dataofinterest, state)) ## we split our data by each state (i think this relies on
        ##lexical scoping)
        ordered  <- list() ## initialize an empty list
        for (i in seq_along(by_state)){ ## for each state
            by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                 by_state[[i]][, "hospital"]), ]
            ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
    } else if (!is.numeric(num)) {
        if (num == "best") {
            by_state <- with(dataofinterest, split(dataofinterest, state))
            ordered  <- list()
            for (i in seq_along(by_state)){
                by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                     by_state[[i]][, "hospital"]), ]
                ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        } else if (num == "worst") {
            by_state <- with(dataofinterest, split(dataofinterest, state))
            ordered  <- list()
            for (i in seq_along(by_state)){
                by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                     by_state[[i]][, "hospital"], 
                                                     decreasing = TRUE), ]
                ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        } else {
            stop('invalid num')
        }
    }
    return(output)
}


