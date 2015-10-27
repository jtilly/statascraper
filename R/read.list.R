#' Read list
#'
#' Reads STATA tables created by \code{list x y}. For each \code{list} command found, this function will create a CSV or RData file.
#' For this function to work there must be a statement
#' \code{di "{tag} filename.csv"} preceding every \code{list} command, where
#' \code{{tag}} is defined in the argument to this function.
#'
#' @param filename The filename of the STATA log file
#' @param outdir Directory for output
#' @param RData store output in RData
#' @param tag specifies what tag to look for in the STATA log file
read.list = function(filename, outdir = ".", RData = FALSE, tag = "list2csv") {

    if (!file.exists(filename)) {
        stop("File ", filename, " does not exist.")
    }

    source.str = readLines(filename)

    # loop through all the lines
    rX = 1
    while (rX <= length(source.str)) {

        list.matrix = NULL
        list.matrix.content = NULL
        list.matrix.colnames = NULL
        list.matrix.csv = NULL

        # find the tag
        if (str_sub(source.str[rX], 1, str_length(tag)) == tag) {

            # skip if no obs
            if (any(source.str[rX + (1:5)] == "no observations"))  {
                rX = rX + 1
                next
            }
            if (RData) {
                list.matrix.csv = paste0(gsub(paste0(tag, "[ ](using |)+([a-zA-Z0-9_-]+).(csv|RData).*"), "\\2", source.str[rX], perl = TRUE), ".RData")
            } else {
                list.matrix.csv = paste0(gsub(paste0(tag, "[ ](using |)+([a-zA-Z0-9_-]+).(csv|RData).*"), "\\2", source.str[rX], perl = TRUE), ".csv")
            }

            # skip lines until we get to the table header
            while (TRUE) {
                rX = rX + 1
                if (str_sub(trim(source.str[rX]), 1, 6) == "+-----") {
                    break
                }
            }

            rX = rX + 1

            # this line contains the variable names
            list.matrix.colnames = unlist(str_split(trim(str_replace_all(source.str[rX], "\\|", "")), "[ ]+"))

            # skip the next line
            rX = rX + 2

            # read data
            while (TRUE) {

                # lists end with +---------
                if (str_sub(trim(source.str[rX]), 1, 6) == "+-----") {
                    break
                }

                # every sixth line is empty
                if (str_sub(trim(source.str[rX]), 1, 6) == "|-----") {
                    rX = rX + 1
                    next
                }

                # get rid of everthing that is not alpha:numeric and put into a row vector
                row = trim(strsplit(source.str[rX], "\\Q|\\E")[[1]])

                # assign middle element to content
                list.matrix.content = c(list.matrix.content, unlist(strsplit(gsub("[^ a-z0-9-_.]+", "\\1", row[-1], perl = TRUE), split = "[ ]+")))

                # increment row counter
                rX = rX + 1
            }
            # create matrix
            list.matrix = matrix(
                as.numeric(list.matrix.content), byrow = TRUE, dimnames = list(rownames = NULL,
                                                                              colnames = list.matrix.colnames),
                ncol = length(list.matrix.colnames)
            )
            if (!is.null(list.matrix.csv)) {
                if (RData) {
                    save(list.matrix, file = paste0(outdir, .Platform$file.sep, list.matrix.csv))
                } else {
                    write.csv(list.matrix, file = paste0(outdir, .Platform$file.sep, list.matrix.csv))
                }
                print(paste("Created", list.matrix.csv))
            }

        }

        rX = rX + 1
    }

}
