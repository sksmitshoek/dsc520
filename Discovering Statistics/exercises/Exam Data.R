examData <- read.delim("Discovering Statistics\\data\\Exam Anxiety.dat")
head(examData)

scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point()
scatter + geom_point() + labs(x="Exam Anxiety", y="Exam Performance %")
scatter + geom_point() + geom_smooth() + labs(x="Exam Anxiety", y="Exam Performance %")
scatter + geom_point() + geom_smooth(method="lm") + labs(x="Exam Anxiety", y="Exam Performance %")

scatter <- ggplot(examData, aes (Anxiety, Exam, colour=Gender))
scatter + geom_point() + geom_smooth(method="lm")
scatter + geom_point() + geom_smooth(method="lm", aes(fill=Gender), alpha=0.1)
scatter + geom_point() + geom_smooth(method="lm", aes(fill=Gender), alpha=0.1) + 
  labs(x="Exam Anxiety", y="Exam Performance %", colour = "Gender")