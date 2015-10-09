# readLog
read.tab = function(filename) {

    if (!file.exists(filename)) {
        stop("File ", filename, " does not exist.")
    }

    source.str = readLines(filename)

    # loop through all the lines
    rX = 1
    while (rX <= length(source.str)) {

        # print(sprintf("Line %.0f out of %.0f", rX, length(source.str)))

        # does the table contain a total?
        tab.matrix = NULL
        tab.matrix.content = NULL
        tab.matrix.colnames = NULL
        tab.matrix.rownames = NULL
        tab.matrix.csv = NULL
        tab.matrix.command = NULL

        # find lines that start with .tab
        if (str_sub(source.str[rX], start = 1, end = 6) == ". tab ") {

            if(length(grep("sum", source.str[rX])) > 0) {
                rX = rX + 1
                next
            }

            # store the original command
            tab.matrix.command = trim(str_sub(source.str[rX], start = 2))

            # find reference to CSV
            if(str_sub(source.str[rX - 2], start = 1, end = 7) == "tab2csv") {
                tab.matrix.csv = paste0(gsub("tab2csv[ ]+([a-zA-Z0-9_-]+).csv.*", "\\1", source.str[rX-2], perl = TRUE), ".csv")
            } else {
                tab.matrix.csv = paste0("other_", rX, ".csv")
            }

            # first variable is the group by argument
            x.varname = gsub(". tab[ ]+([a-z0-9_]+).*", "\\1", source.str[rX], perl = TRUE)

            # second variable is the variable of interest
            y.varname = gsub(". tab[ ]+([a-z0-9_]+)[ ]+([a-z0-9_]+).*", "\\2", source.str[rX], perl = TRUE)

            # skip the next line
            rX = rX + 2

            # this line contains the label of y

            while(str_sub(source.str[rX], start = -5) != "Total") {
                rX = rX + 1
            }

            # take a peak two rows below for the number of columns
            tab.matrix.ncol = length(strsplit(gsub("[^ a-z0-9-_]+", "\\1", trim(strsplit(source.str[rX+2], "\\Q|\\E")[[1]][2]), perl = TRUE), split = "[ ]+")[[1]])

            # extract labelString
            labelString = trim(strsplit(source.str[rX], "\\Q|\\E")[[1]][2])
            tab.matrix.colnames = NULL
            # create column labels (that are of length 10)
            for (cX in 1:(tab.matrix.ncol)) {
                tab.matrix.colnames = c(tab.matrix.colnames,
                                        trim(str_sub(labelString, start = (cX - 1)*11 + 1, end = cX*11)))
            }

            tab.matrix.colnames = c(tab.matrix.colnames, "Total")

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
                tab.matrix.content = c(tab.matrix.content, unlist(strsplit(gsub("[^ a-z0-9-_]+", "\\1", row[2:3], perl = TRUE), split = "[ ]+")))
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
                write.csv(tab.matrix, file = tab.matrix.csv)
                print(paste("Created", tab.matrix.csv))
            }

        }

        rX = rX + 1
    }


}


trim = function(x) {
    gsub("^\\s+|\\s+$", "", x)
}
