survey <- read.csv("data\\acs-14-1yr-s0201.csv")

str(survey)

nrow(survey)

ncol(survey)

sur_histo <- ggplot(survey, aes(HSDegree))


sur_histo + geom_histogram(aes(y=..density..), bins = 45) + 
  labs(x="% of Population with High School Degrees", y="Frequency") +
  ggtitle("Population with High School Degrees") + 
  stat_function(fun=dnorm, args=list(mean=mean(survey$HSDegree, na.rm=TRUE), 
                                     sd=sd(survey$HSDegree, na.rm=TRUE)))

sur_histo + geom_density() + 
  labs(x="% of Population with High School Degrees", y="Number of Communities") +
  ggtitle("Population with High School Degrees")

round(stat.desc(survey$HSDegree, basic=FALSE, norm=TRUE), 2)
