scores <- read.csv("data\\scores.csv", header=TRUE, stringsAsFactors=TRUE)

sport_scores <- subset(scores, scores$Section == "Sports")
reg_scores <- subset(scores, scores$Section == "Regular")

sport_hist <- ggplot(sport_scores, aes(x=Score))
sport_hist + geom_histogram(bins = 7) + 
  labs(x="Total Points", y="Number of Students") +
  ggtitle("Sports Section Scores")

reg_hist <- ggplot(reg_scores, aes(x=Score))
reg_hist + geom_histogram(bins = 7) + 
  labs(x="Total Points", y="Number of Students") +
  ggtitle("Regular Section Scores")

ggplot(scores, aes(x=Score)) + 
  geom_histogram(bins=7) + 
  facet_wrap(~Section, ncol=1)

stat.desc(sport_scores$Score, basic=FALSE, norm=TRUE)
stat.desc(reg_scores$Score, basic=FALSE, norm=TRUE)