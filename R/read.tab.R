#' Read tab
#'
#' Reads STATA tables created by \code{tab x y} and \code{tab x, sum(y)}. For
#' each \code{tab} command found, this function will create a CSV or RData file.
#' For this function to work there must be a statement
#' \code{di "{tag} filename.csv"} preceding every \code{tab} command.
#'
#' @param filename The filename of the STATA log file
#' @param outdir Directory for output
#' @param RData store output in RData
#' @param tag specifies what tag to look for in the Stata log file
read.tab = function(filename, outdir = ".", RData = FALSE, tag = "tab2csv") {

    if (!file.exists(filename)) {
        stop("File ", filename, " does not exist.")
    }

    source.str = readLines(filename)

    # loop through all the lines
    rX = 1
    while (rX <= length(source.str)) {

        tab.matrix = NULL
        tab.matrix.content = NULL
        tab.matrix.colnames = NULL
        tab.matrix.rownames = NULL
        tab.matrix.csv = NULL

        # find the tag
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

            # this line contains the label of y
            while (length(grep(".+((Total)|(Freq\\.))", source.str[rX])) == 0) {
                rX = rX + 1
            }

            # find the delimiter
            delimiter = gsub(".+((Total)|(Freq\\.))", "\\1", source.str[rX])

            # take a peek two rows below for the number of columns
            tab.matrix.ncol = length(strsplit(gsub(",", "", trim(strsplit(source.str[rX + 2], "\\Q|\\E")[[1]][2]), fixed = TRUE), split = "[ ]+")[[1]])

            # extract labelString
            labelString = trim(strsplit(source.str[rX], "\\Q|\\E")[[1]][2])
            if (labelString == "Mean   Std. Dev.       Freq.") {
                tab.matrix.colnames = c("Means", "Std. Dev.", "Freq.")
            } else if (labelString == "N      mean        sd       p25       p50       p75") {
                tab.matrix.colnames = c("N", "mean", "sd", "p25", "p50", "p75")
            } else {
                tab.matrix.colnames = NULL
                # create column labels (that are of length 10)
                for (cX in 1:(tab.matrix.ncol)) {
                    tab.matrix.colnames = c(tab.matrix.colnames,
                                            trim(str_sub(labelString, start = (cX - 1) * 11 + 1, end = cX * 11)))
                }

                if (length(strsplit(source.str[rX], "\\Q|\\E")[[1]]) == 3) {
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
