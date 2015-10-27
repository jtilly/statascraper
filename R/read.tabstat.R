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
#' @param tag specifies what tag to look for in the STATA log file
read.tabstat = function(filename, outdir = ".", RData = FALSE, tag = "tabstat2csv") {

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

        # tab2csv
        if (str_sub(source.str[rX], 1, str_length(tag)) == tag) {

            # skip if no obs
            if (any(source.str[rX + (1:5)] == "no observations"))  {
                rX = rX + 1
                next
            }

            if (RData) {
                tab.matrix.csv = paste0(gsub(paste0(tag, "[ ](using |)+([a-zA-Z0-9_-]+).(csv|RData).*"), "\\2", source.str[rX], perl = TRUE), ".RData")
            } else {
                tab.matrix.csv = paste0(gsub(paste0(tag, "[ ](using |)+([a-zA-Z0-9_-]+).(csv|RData).*"), "\\2", source.str[rX], perl = TRUE), ".csv")
            }

            # find the separator line -----
            nX = 1
            while (str_sub(source.str[rX], start = 1, end = 5) != "-----") {
                rX = rX + 1
                nX = nX + 1
                if (nX > 10) {
                    breakOutofLoop = TRUE
                    break
                }
                breakOutofLoop = FALSE
            }
            if (breakOutofLoop) {
                rX = rX - 9
                next
            }

            # go back one line to get to the labels
            rX = rX - 1

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
            tab.matrix.content[tab.matrix.content == "."] = 0

            # create matrix
            tab.matrix = matrix(
                # make matrix
                as.numeric(tab.matrix.content), byrow = TRUE, dimnames = list(rownames = tab.matrix.rownames,
                                                                              colnames = tab.matrix.colnames),
                nrow = length(tab.matrix.rownames), ncol = length(tab.matrix.colnames)
            )
            if (!is.null(tab.matrix.csv)) {
                if (RData) {
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
