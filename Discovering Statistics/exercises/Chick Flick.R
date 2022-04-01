chickFlick <- read.delim("Discovering Statistics\\data\\ChickFlick.dat", header=TRUE)
head(chickFlick)

bar <- ggplot(chickFlick, aes(film, arousal))
bar + 
  stat_summary(fun=mean, geom="bar", fill="white", colour="Black") +
  stat_summary(fun.data=mean_cl_normal, geom="pointrange")

bar <- ggplot(chickFlick, aes(film, arousal, fill=gender))
bar + stat_summary(fun=mean, geom="bar", position="dodge") + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=.9), width=.2)

bar <- ggplot(chickFlick, aes(film, arousal, fill=film))
bar + stat_summary(fun=mean, geom="bar") + 
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=.2) +
  facet_wrap(~gender) +
  labs(x="Film", y="Mean Arousal") +
  theme(legend.position="none")
  