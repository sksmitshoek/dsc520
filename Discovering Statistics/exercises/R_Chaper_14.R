sport <- c("Hockey", "Baseball", "Football")
league <- c("NHL", "MLB", "NFL")
trophy <- c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy")
trophies1 <- cbind(sport, league, trophy)

trophies2 <- data.frame(sport=c("Basketball", "Golf"),
                        league=c("NBA", "PGA"),
                        trophy=c("Larry O'Brien Championship Trophy",
                                 "Wanamaker Trophy"),
                        stringsAsFactors = FALSE)

trophies <- rbind(trophies1, trophies2)

cbind(Sport=sport, Association=league, Prize=trophy)

#########################################################

download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
              destfile = "Discovering Statistics\\data\\ForeignAid.zip")
unzip("Discovering Statistics\\data\\ForeignAid.zip", 
      exdir="Discovering Statistics\\data\\ForeignAid")

library(stringr)
theFiles <- dir("Discovering Statistics\\data\\ForeignAid", pattern="\\.csv")
for(a in theFiles)
{
  nameToUse <- str_sub(string=a, start=12, end=18)
  temp <- read.table(file=file.path("Discovering Statistics/data/ForeignAid", a),
                     header=TRUE, sep=",", stringsAsFactors=FALSE)
  assign(x=nameToUse, value=temp)
}


Aid90s00s <- merge(x=Aid_90s, y=Aid_00s,
                   by.x = c("Country.Name", "Program.Name"),
                   by.y = c("Country.Name", "Program.Name"))
head(Aid90s00s)


library(plyr)
Aid90s00sJoin <- join(x=Aid_90s, y=Aid_00s,
                      by=c("Country.Name", "Program.Name"))
head(Aid90s00sJoin)

frameNames <- str_sub(string=theFiles, start=12, end=18)
frameList <- vector("list", length(frameNames))
names(frameList) <- frameNames

for(a in frameNames)
{
  frameList[[a]] <- eval(parse(text=a))
}

allAid <- Reduce(function(...){
  join(..., by=c("Country.Name", "Program.Name"))},
  frameList)
dim(allAid)


library(data.table)
dt90<- data.table(Aid_90s, key=c("Country.Name", "Program.Name"))
dt00<- data.table(Aid_00s, key=c("Country.Name", "Program.Name"))

dt0090 <- dt90[dt00]

library(reshape2)
melt00 <- melt(Aid_00s, id.vars=c("Country.Name", "Program.Name"),
               variable.name="Year", value.name = "Dollars")

library(scales)
melt00$Year <- as.numeric(str_sub(melt00$Year, start=3, 6))
meltAgg <- aggregate(Dollars ~ Program.Name + Year, data=melt00, 
                     sum, na.rm=TRUE)

library(ggplot2)
ggplot(meltAgg, aes(x=Year, y=Dollars)) +
  geom_line(aes(group=Program.Name)) + 
  facet_wrap(~ Program.Name)

cast00 <- dcast(melt00, Country.Name + Program.Name ~ Year, value.var="Dollars")