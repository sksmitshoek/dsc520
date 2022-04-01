hiccupsData <- read.delim("Discovering Statistics\\data\\Hiccups.dat", header=TRUE)
head(hiccupsData)

hiccups <- stack(hiccupsData)
names(hiccups) <- c("Hiccups", "Intervention")

line <- ggplot(hiccups, aes(Intervention, Hiccups))

line + 
  stat_summary(fun=mean, geom="point") + 
  stat_summary(fun=mean, geom="line", aes(group=1), colour="Blue", linetype="dashed") + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=.2, colour="red") + 
  labs(x="Intervention", y="Mean Number of Hiccups")