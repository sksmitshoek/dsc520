setwd("C:\\Users\\sksmi\\PeytoAccess\\Personal\\Bellevue\\DSC520\\dsc520")
facebookData <- read.delim("Discovering Statistics\\data\\FacebookNarcissism.dat")
head(facebookData)

graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))

graph + geom_point()
graph + geom_point(shape=17)
graph + geom_point(size=6)
graph + geom_point(aes(colour=Rating_Type))

graph + geom_point(aes(colour=Rating_Type), position="jitter")
graph + geom_point(aes(shape=Rating_Type), position="jitter")
