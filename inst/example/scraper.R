# scraper.R
library("statascraper")

# point to stata log file
logfile = paste(system.file("example", package = "statascraper"), "test.log", sep = .Platform$file.sep)

# In this example, we scrape the Stata log file "test.log" that was created by
# "stata.do"

# read tab commands
read.tab(filename = logfile, outdir = ".", RData = TRUE, tag = "tab2r")
head(load.RData("tab.RData"))
head(load.RData("tabsum.RData"))

# read tabstat commands
read.tabstat(filename = logfile, outdir = ".", RData = TRUE, tag = "tabstat2r")
head(load.RData("tabstat.RData"))

# read list commands
read.list(filename = logfile, outdir = ".", RData = TRUE, tag = "list2r")
head(load.RData("list.RData"))
