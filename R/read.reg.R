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

    if (!RData) {
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
