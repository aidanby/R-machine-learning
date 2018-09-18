##repo to commit
repoToPercent = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\repo_to_bddPercent.csv")
repoToPercent$percent <- (repoToPercent$bddcommits / repoToPercent$totalcommits) * 100

percent <- repoToPercent$percent
names(percent) <- repoToPercent$username


dotchart(percent, xlab = "percent (%)", main = "Percentage of BDD Commits")

##percent vs code changed
percentageVScode = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\ALL_percentagesVScode.csv")
percentageVScode$percent <- (percentageVScode$bddcommits / percentageVScode$totalcommits) * 100
with(percentageVScode, plot(percent, linesofcodechanged, 
                               main='Percentages of BDD Commits vs Lines of .Feature Code Changed \n in All Repositories', 
                               xlab = 'Percentages of BDD Commits (%)', ylab = '.feature Code Lines Changed', pch = 16, cex = 1.1, col = 'blue'))
fit <- lm(percent~linesofcodechanged, data = percentageVScode)
abline(fit)

##commit to commitSize
CommitToCommitSize = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\bugsnag_commitSizes.csv")
commitSize = CommitToCommitSize$commitsize
names(commitSize) <- CommitToCommitSize$commitid
dotchart(commitSize, xlab = "Commit Size", main = "Commit Sizes for all Commits \n in Repository: bugsnag-bugsnag-android")

##commit size vs code changed
commitSizeVsCodeAll = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\All_sizeVScode.csv")
with(commitSizeVsCodeAll, plot(commitsize, linesofcodechanged, 
                                main='Commit Size vs Lines of .Feature Code Changed \n in All Repositories', 
                                xlab = 'Commit Size', ylab = '.feature Code Lines Changed', pch = 16, cex = 1.1, col = 'blue'))
fit <- lm(commitsize~linesofcodechanged, data = commitSizeVsCodeAll)
abline(fit)


commitSizeVsCodeBugs = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\bugsnag_sizeVScode.csv")
with(commitSizeVsCodeBugs, plot(commitsize, linesofcodechanged, 
                            main='Commit Size vs Lines of .Feature Code Changed \n in Repository bugsnag-bugsnag-android', 
                            xlab = 'Commit Size', ylab = '.feature Code Lines Changed', pch = 16, cex = 1.1, col = 'blue'))
lm(commitsize~linesofcodechanged, data = commitSizeVsCodeBugs)
abline(0.7046, 3)



commitSizeVsCodeSvein = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\sveinung_sizeVScode.csv")
with(commitSizeVsCodeSvein, plot(commitsize, linesofcodechanged, 
                            main='Commit Size vs Lines of .Feature Code Changed \n in Repository sveinung-pritest-server', 
                            xlab = 'Commit Size', ylab = '.feature Code Lines Changed',  pch = 16, cex = 1.1, col = 'blue'
                            ))
abline(0.7046, 5)

##file to code


fileToCode = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\bugsnag_822.csv")

codeNumber = fileToCode$linesofcodechanged

names(codeNumber) <- fileToCode$filesuploaded


dotchart(codeNumber, xlab = "lines of code impacted", main = ".feature Files Code Impact for Commit: 822968fac179c2cf444d9792e90182ebe4f666e0 \n in Repository: bugsnag-bugsnag-android")


##file rules


fileRules = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\sveinung_fileRules.csv")
count = fileRules$count
names(count) <- fileRules$rules
dotchart(count, xlab = "Number of Occurences", main = "Association Rules for Files \n in Repository: sveinung-pritest-server")

##code rules

codeRules = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\bugs_codeRules.csv")
count = codeRules$count
names(count) <- codeRules$rules
dotchart(count, xlab = "Number of Occurences", main = "Association Rules for Code \n in Repository: bugsnag-bugsnag-android")

##LOC
library(beanplot)
library(dplyr)
BDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\LOC_analysis\\normalizedLOC_BDD.csv")
nonBDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\LOC_analysis\\normalizedLOC_nonBDD.csv")
BDD <- filter(BDD, BDD$normalizedLOC < 10)
plot(BDD)





plot(codeLOCBDD, type = "o", pch= 1 ,col = "black", ylab = "LOC", main = "LOC Evolution for 133 BDD Repositories")
lines(testLOCBDD, type = "o", pch=1, col = "red")
legend(1,63000, legend = c("Source Files", "Test Files"), col = c("Black", "Red"), lty=1:1, cex=0.8, title = "Line Types", text.font = 4, bg = 'lightblue')


codeLOCBDD <- nonBDD$codeLOC
testLOCBDD <- nonBDD$testLOC
timeBDD <- nonBDD$date


plot(codeLOCBDD, type = "o", pch= 1 ,col = "black", ylab = "LOC", main = "LOC Evolution for 133 Non-BDD Repositories")
lines(testLOCBDD, type = "o", pch=1, col = "red")
legend(1,151000, legend = c("Source Files", "Test Files"), col = c("Black", "Red"), lty=1:1, cex=0.8, title = "Line Types", text.font = 4, bg = 'lightblue')



##pullRequests and issue reports
library(beanplot)
library(effsize)

pullBDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\pullRequests\\pullRequests_BDD_toR.csv")
pullnonBDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\pullRequests\\pullRequests_nonBDD_toR.csv")
pullBDD_data <- pullBDD$timedifference
pullnonBDD_data <- pullnonBDD$timedifference
pullBDD_data <- subset(pullBDD_data, pullBDD_data < 10000)
pullnonBDD_data <- subset(pullnonBDD_data, pullnonBDD_data < 10000)
pulltoPlot1 <- quantile(BDD_data, probs = seq(0, 1, 0.1), na.rm = FALSE,
                   names = TRUE, type = 7)
pulltoPlot2 <- quantile(nonBDD_data, probs = seq(0, 1, 0.1), na.rm = FALSE,
                   names = TRUE, type = 7)

issueBDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\issueReports\\issues_BDD_toR.csv")
issuenonBDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\issueReports\\issues_nonBDD_toR.csv")
issueBDD_data <- issueBDD$timedifference
issuenonBDD_data <- issuenonBDD$timedifference
issueBDD_data <- subset(issueBDD_data, issueBDD_data < 10000)
issuenonBDD_data <- subset(issuenonBDD_data, issuenonBDD_data < 10000)
issuetoPlot1 <- quantile(issueBDD_data, probs = seq(0, 1, 0.1), na.rm = FALSE,
                    names = TRUE, type = 7)
issuetoPlot2 <- quantile(issuenonBDD_data, probs = seq(0, 1, 0.1), na.rm = FALSE,
                    names = TRUE, type = 7)



beanplot(pulltoPlot1, pulltoPlot2, issuetoPlot1, issuetoPlot2, ylab = 'Time Period (minutes)', 
         main = 'Merge and Close Time for Pull Requests and Issue Reports', side = 'both', axes=F, beanlines = 'median', col = list("black", c("grey", "white")), what = c(TRUE, TRUE, TRUE, FALSE))
axis(1, at = 1:2, labels = c('Pull Requests', 'Issue Reports'))
axis(2, at = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000))
box(which = "plot")
legend(2.15, 11200,fill = c("black", "grey"), legend = c("BDD", "non-BDD"), cex=1)


wilcox.test(pullBDD_data, pullnonBDD_data)
cliff.delta(pullBDD_data, pullnonBDD_data)

wilcox.test(issueBDD_data, issuenonBDD_data)
cliff.delta(issueBDD_data, issuenonBDD_data)

issuePercentageBDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\issueReports\\issues_BDD_commitPercentages.csv")
issuePercentageNonBDD = read.csv("C:\\Users\\happyuser\\Desktop\\GitHub_Scraper\\1000000\\issueReports\\issues_nonBDD_commitPercentages.csv")
percentageBDD <- as.numeric(issuePercentageBDD$percentages)
percentageNonBDD <- as.numeric(issuePercentageNonBDD$percentages)
wilcox.test(percentageBDD, percentageNonBDD)
cliff.delta(percentageBDD, percentageNonBDD)



##time difference between commits
timeDiff = read.csv("C:\\Users\\happyuser\\Desktop\\CSV_fromSQL\\nlplinks.csv")
data <- subset(timeDiff$timedifference, timeDiff$timedifference < 1440)
barplot(data, ylab = "Time Difference (minutes)", xlab = "Commit", main = "Commit Time Differences")
quantile(data, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7)
toPlot <- quantile(data, probs = seq(0, 1, 0.1), na.rm = FALSE,
                 names = TRUE, type = 7)
boxplot(toPlot, main = "Time between Commits", ylab = "Minutes")
data <- sort(data)
barplot(data, ylab = "Time Difference (minutes)", xlab = "Commit", main = "Sorted Commit Time Differences")


##similarity
simData <- unique(timeDiff$similarity)
quantile(simData, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7)
toPlotSim <- quantile(simData, probs = seq(0, 1, 0.1), na.rm = FALSE,
                   names = TRUE, type = 7)
boxplot(toPlotSim, ylab = "Cosine Similarity")


