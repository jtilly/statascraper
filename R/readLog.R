#' Read tab
#' 
#' Reads STATA tables created by \code{tab x y} and \code{tab x, sum(y)}. For
#' each \code{tab} command found, this function will create a CSV or RData file. 
#' For this function to work there must be a statement 
#' \code{di "tab2csv filename.csv"} preceding every \code{tab} command.
#' 
#' @param filename The filename of the STATA log file
#' @param outdir Directory for output
#' @param RData store output in RData
read.tab = function(filename, outdir = ".", RData = FALSE) {
    
    if (!file.exists(filename)) {
        stop("File ", filename, " does not exist.")
    }
    
    source.str = readLines(filename)
    
    # loop through all the lines
    rX = 1
    while (rX <= length(source.str)) {
        
        #print(sprintf("Line %.0f out of %.0f", rX, length(source.str)))
        
        # does the table contain a total?
        tab.matrix = NULL
        tab.matrix.content = NULL
        tab.matrix.colnames = NULL
        tab.matrix.rownames = NULL
        tab.matrix.csv = NULL
        tab.matrix.command = NULL
        
        # tab2csv
        if (str_sub(source.str[rX], 1, 7) == "tab2csv") {
            
            # skip if no obs
            if(any(source.str[rX+(1:5)] == "no observations"))  {
                rX = rX + 1
                next
            }
            if(RData) {
                tab.matrix.csv = paste0(gsub("tab2csv[ ](using |)+([a-zA-Z0-9_-]+).csv.*", "\\2", source.str[rX], perl = TRUE), ".RData")
            } else {
                tab.matrix.csv = paste0(gsub("tab2csv[ ](using |)+([a-zA-Z0-9_-]+).csv.*", "\\2", source.str[rX], perl = TRUE), ".csv")
            }
                
            # this line contains the label of y
            while(length(grep(".+((Total)|(Freq\\.))", source.str[rX])) == 0) {
                rX = rX + 1
            }
            
            # find the delimiter
            delimiter = gsub(".+((Total)|(Freq\\.))", "\\1", source.str[rX])
            
            # take a peek two rows below for the number of columns
            tab.matrix.ncol = length(strsplit(gsub(",", "", trim(strsplit(source.str[rX+2], "\\Q|\\E")[[1]][2]), fixed = TRUE), split = "[ ]+")[[1]])
            
            # extract labelString
            labelString = trim(strsplit(source.str[rX], "\\Q|\\E")[[1]][2])
            if(labelString == "Mean   Std. Dev.       Freq.") {
                tab.matrix.colnames = c("Means", "Std. Dev.", "Freq.")
            } else if (labelString == "N      mean        sd       p25       p50       p75") {
                tab.matrix.colnames = c("N", "mean", "sd", "p25", "p50", "p75")
            } else {
                tab.matrix.colnames = NULL
                # create column labels (that are of length 10)
                for (cX in 1:(tab.matrix.ncol)) {
                    tab.matrix.colnames = c(tab.matrix.colnames,
                                            trim(str_sub(labelString, start = (cX - 1)*11 + 1, end = cX*11)))
                }
                
                if(length(strsplit(source.str[rX], "\\Q|\\E")[[1]]) == 3) {
                    tab.matrix.colnames = c(tab.matrix.colnames, str_replace(delimiter, "\\.", ""))
                }
                
            }
            rm(labelString)
            
            # skip the next line
            rX = rX + 2
            
            # read data
            while (TRUE) {
                if (str_sub(source.str[rX], start = 1, end = 5) == "-----") {
                    break
                }
                # get rid of everthing that is not alpha:numeric and put into a row vector
                row = trim(strsplit(source.str[rX], "\\Q|\\E")[[1]])
                # assign first element to rownames
                tab.matrix.rownames = c(tab.matrix.rownames, row[1])
                # assign middle element to content
                tab.matrix.content = c(tab.matrix.content, unlist(strsplit(gsub("[^ a-z0-9-_.]+", "\\1", row[-1], perl = TRUE), split = "[ ]+")))
                # increment row counter
                rX = rX + 1
            }
            # create matrix
            tab.matrix = matrix(
                as.numeric(tab.matrix.content), byrow = TRUE, dimnames = list(rownames = tab.matrix.rownames,
                                                                              colnames = tab.matrix.colnames),
                nrow = length(tab.matrix.rownames), ncol = length(tab.matrix.colnames)
            )
            if (!is.null(tab.matrix.csv)) {
                if(RData) {
                    save(tab.matrix, file = paste0(outdir, .Platform$file.sep, tab.matrix.csv))
                } else {
                    write.csv(tab.matrix, file = paste0(outdir, .Platform$file.sep, tab.matrix.csv))
                }
                print(paste("Created", tab.matrix.csv))
            }
            
        }
        
        rX = rX + 1
    }
    
    
}


#' Read tabstat
#' 
#' Reads STATA tables created by \code{tabstat y, by(x)}. For each \code{tab} 
#' command found, this function will create a CSV or RData file. For this function to
#' work there must be a statement \code{di "tabstat2csv filename.csv"}
#' preceding every \code{tabstat} command.
#' 
#' @param filename The filename of the STATA log file
#' @param outdir Directory for output
#' @param RData store output in RData
read.tabstat = function(filename, outdir = ".", RData = FALSE) {
    
    if (!file.exists(filename)) {
        stop("File ", filename, " does not exist.")
    }
    
    source.str = readLines(filename)
    
    # loop through all the lines
    rX = 1
    while (rX <= length(source.str)) {
        
        #print(sprintf("Line %.0f out of %.0f", rX, length(source.str)))
        
        # does the table contain a total?
        tab.matrix = NULL
        tab.matrix.content = NULL
        tab.matrix.colnames = NULL
        tab.matrix.rownames = NULL
        tab.matrix.csv = NULL
        tab.matrix.command = NULL
        
        # tab2csv
        if (str_sub(source.str[rX], 1, 11) == "tabstat2csv") {
            
            # skip if no obs
            if(any(source.str[rX+(1:5)] == "no observations"))  {
                rX = rX + 1
                next
            }
            
            if(RData) {
                tab.matrix.csv = paste0(gsub("tabstat2csv[ ](using |)+([a-zA-Z0-9_-]+).csv.*", "\\2", source.str[rX], perl = TRUE), ".RData")
            } else {
                tab.matrix.csv = paste0(gsub("tabstat2csv[ ](using |)+([a-zA-Z0-9_-]+).csv.*", "\\2", source.str[rX], perl = TRUE), ".csv")
            }
            
            # find the separator line -----
            nX = 1
            while(str_sub(source.str[rX], start = 1, end = 5) != "-----") {
                rX = rX + 1
                nX = nX + 1
                if(nX > 10) {
                    breakOutofLoop = TRUE
                    break
                } 
                breakOutofLoop = FALSE
            }
            if(breakOutofLoop) {
                rX = rX - 9
                next
            }
            
            # go back one line to get to the labels
            rX = rX-1
            
            # take a peek two rows below for the number of columns
            tab.matrix.ncol = length(strsplit(gsub("[^ a-z0-9-_]+", "\\1", trim(strsplit(source.str[rX+2], "\\Q|\\E")[[1]][2]), perl = TRUE), split = "[ ]+")[[1]])
            
            # extract labelString
            labelString = trim(strsplit(source.str[rX], "\\Q|\\E")[[1]][2])
            tab.matrix.colnames = NULL
            # create column labels
            tab.matrix.colnames = strsplit(labelString , "[ ]+")[[1]]
            rm(labelString)
            
            # skip the next line
            rX = rX + 2
            
            # read data
            while (TRUE) {
                if (str_sub(source.str[rX], start = 1, end = 5) == "-----") {
                    break
                }
                # get rid of everthing that is not alpha:numeric and put into a row vector
                row = trim(strsplit(source.str[rX], "\\Q|\\E")[[1]])
                # assign first element to rownames
                tab.matrix.rownames = c(tab.matrix.rownames, row[1])
                # assign middle element to content
                tab.matrix.content = c(tab.matrix.content, unlist(strsplit(gsub(",", "", row[2], fixed = TRUE), split = "[ ]+")))
                # increment row counter
                rX = rX + 1
            }
            
            # replace . by zero
            tab.matrix.content[tab.matrix.content=="."] = 0
            
            # create matrix
            tab.matrix = matrix(
                # make matrix
                as.numeric(tab.matrix.content), byrow = TRUE, dimnames = list(rownames = tab.matrix.rownames,
                                                                              colnames = tab.matrix.colnames),
                nrow = length(tab.matrix.rownames), ncol = length(tab.matrix.colnames)
            )
            if (!is.null(tab.matrix.csv)) {
                if(RData) {
                    save(tab.matrix, file = paste0(outdir, .Platform$file.sep, tab.matrix.csv))   
                } else {
                    write.csv(tab.matrix, file = paste0(outdir, .Platform$file.sep, tab.matrix.csv))
                }
                print(paste("Created", tab.matrix.csv))
            }
            
        }
        
        rX = rX + 1
    }
    
    
}


#' Read regression output
#' 
#' Reads STATA tables created by linear regressions. For
#' each \code{(a)reg} command found, this function will create an \code{.RData} 
#' file that contains the list \code{tab.reg} with the regression output.
#' 
#' @param filename The filename of the STATA log file
#' @param outdir Directory for output
#' @param RData store output in RData
read.reg = function(filename, outdir = ".", RData = TRUE) {
    
    if (!file.exists(filename)) {
        stop("File ", filename, " does not exist.")
    }
    
    if(!RData) {
        warning("RData = FALSE not (yet) supported.")
    }
    
    source.str = readLines(filename)
    
    # loop through all the lines
    rX = 1
    while (rX <= length(source.str)) {
        
        #print(sprintf("Line %.0f out of %.0f", rX, length(source.str)))
        
        tab.reg = list()
        tab.reg$command = NULL
        tab.reg$x = NULL
        tab.reg$coeff = NULL
        tab.reg$se = NULL
        tab.reg$absorbed = NULL

        # "Linear Regression"
        if (str_sub(source.str[rX], 1, 17) == "Linear regression") {
            
            nX = 2
            while(nX < 50) {
                if(str_sub(source.str[rX-nX], 1, 6) == ". areg" ||
                   str_sub(source.str[rX-nX], 1, 5) == ". reg") {
                    tab.reg$command = source.str[rX-nX]
                    break
                }
                nX = nX + 1
            }
            
            if (str_sub(source.str[rX], 1, 39) == "Linear regression, absorbing indicators") {
                absorb = TRUE   
            } else {
                absorb = FALSE
            }
            
            tab.reg$obs = as.numeric(gsub("[^\\=]* \\=[ ]*([0-9,.]+)", "\\1", source.str[rX]))
            rX = rX + 1
            if(gsub("[^\\=]* \\=[ ]*([0-9,.]+)", "\\1", source.str[rX]) == ".") {
                tab.reg$Fstat = Inf    
            } else {
                tab.reg$Fstat = as.numeric(gsub("[^\\=]* \\=[ ]*([0-9,.]+)", "\\1", source.str[rX]))    
            }
            rX = rX + 2
            tab.reg$R2 = as.numeric(gsub("[^\\=]* \\=[ ]*([0-9,.]+)", "\\1", source.str[rX]))
            if(absorb) {
                rX = rX + 1    
            }
            rX = rX + 6
            tab.reg$y = trim(gsub("(.+) \\|.+", "\\1", source.str[rX]))
            rX = rX + 2
            while(str_sub(source.str[rX], start = 1, end = 5) != "-----") {
                if(trim(gsub(".*\\|(.*)", "\\1", source.str[rX]))=="") {
                    rX = rX + 1
                    next
                }
                
                # coeff is exactly zero, continue
                if(as.numeric(gsub(".+ \\| [ ]*([^ ]+) .+", "\\1", source.str[rX])) == 0) {
                    rX = rX + 1
                    next
                }
                
                tab.reg$coefficients = c(tab.reg$coefficients, as.numeric(gsub(".+ \\| [ ]*([^ ]+) .+", "\\1", source.str[rX])))
                tab.reg$x = c(tab.reg$x, trim(gsub("(.+) \\| .+", "\\1", source.str[rX])))
                tab.reg$se = c(tab.reg$se, as.numeric(gsub(".+ \\| [ ]*[^ ]+[ ]+([^ ]+) .+", "\\1", source.str[rX])))
                rX = rX + 1
            }
            
            if(absorb) {
                rX = rX + 1
                tab.reg$absorbed = trim(gsub("(.+) \\| .+", "\\1", source.str[rX]))
            }
            
            save(tab.reg, file = paste0(outdir, .Platform$file.sep, gsub("([ =.,\\(\\)&><]*)", "", tab.reg$command), ".RData"))
            
        } else {
            rX = rX + 1
        }
        
    }
    
}



#' Trim whitespaces from a string
#' 
#' This function removes whitespaces from the beginning and end of a string
#' 
#' @param x The string to be stripped.
#' @return A string without whitespaces at the beginning and end.
trim = function(x) {
    gsub("^\\s+|\\s+$", "", x)
}

#' helper function that loads a specific file and returns the variables therein
load.RData <- function(fileName){
    load(fileName)
    data.frame(get(ls()[ls() != "fileName"]))
}
