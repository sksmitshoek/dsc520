festivalData <- read.delim("Discovering Statistics\\data\\DownloadFestival(No Outlier).dat", header=TRUE)

festivalHistogram <- ggplot(festivalData, aes(day1)) + theme(legend.position="none")
festivalHistogram + geom_histogram(binwidth = 0.4) + 
  labs(x="Hygiene (Day 1 of Festival)", y = "Frequency")

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x="Gender", y="Hygiene")

festivalData <- festivalData[order(festivalData$day1),]

density <- ggplot(festivalData, aes(day1))
density + geom_density() + labs(x="Hygiene", y="Density Estimate")
