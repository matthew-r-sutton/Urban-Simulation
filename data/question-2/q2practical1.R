setwd("C:/Users/matth/Documents/casa0002/question2")

#install.packages(c("sp","MASS","reshape2","geojsonio","rgdal","downloader","maptools","dplyr","broom","stplanr","ggplot2","leaflet"))
library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom)
library(stplanr)
library(ggplot2)
library(leaflet)

#fetch ONS boundary data for England
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
plot(EW)

#check head of EW
head(EW@data)

#pull London from EW
London <- EW[grep("^E09",EW@data$lad15cd),]
plot(London)

#check data
summary(London)

#reproject to BNG
BNG = "+init=epsg:27700"
LondonBNG <- spTransform(London, BNG)

#order by borough code
LondonBNG <- LondonBNG[order(LondonBNG$lad15cd),]

#generate a distance matrix of distances between boroughs
dist <- spDists(LondonBNG)

#melt the matrix into a list of origin/destination pairs
distPair <- melt(dist)

#read in London Commuting Data
cdata <- read.csv("http://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1")

#read in a lookup table for translating between old and new borough codes
CodeLookup <- read.csv("https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1")

#read in population and income data
popincome <- read.csv("https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1")

#now mearge these data into the flow dataframe
cdata$OrigCodeNew <- CodeLookup$NewCode[match(cdata$OrigCode, CodeLookup$OldCode)]
cdata$DestCodeNew <- CodeLookup$NewCode[match(cdata$DestCode, CodeLookup$OldCode)]
cdata$vi1_origpop <- popincome$pop[match(cdata$OrigCodeNew, popincome$code)]
cdata$vi2_origsal <- popincome$med_income[match(cdata$OrigCodeNew, popincome$code)]
cdata$wj1_destpop <- popincome$pop[match(cdata$DestCodeNew, popincome$code)]
cdata$wj2_destsal <- popincome$med_income[match(cdata$DestCodeNew, popincome$code)]

#arrange data by OrigCodeNew, and then DestCodeNew
cdata <- arrange(cdata,OrigCodeNew,DestCodeNew)

#create new total flows column that omits intra-borough flows
cdata$TotalNoIntra <- ifelse(cdata$OrigCode == cdata$DestCode,0,cdata$Total)
cdata$offset <- ifelse(cdata$OrigCode== cdata$DestCode,0.0000000001,1)
#now add distance column to dataframe
cdata$dist <- distPair$value

head(cdata)

#subset by the first 7 borough codes
toMatch <- c("00AA","00AB","00AC","00AD","00AE","00AF","00AG")
#first the origins
cdatasub <- cdata[grep(paste(toMatch,collapse = "|"), cdata$OrigCode),]
#then the destinations
cdatasub <- cdatasub[grep(paste(toMatch,collapse = "|"), cdata$DestCode),]
#chop out intra-borough flows
cdatasub <- cdatasub[cdatasub$OrigCode!=cdatasub$DestCode,]
#chop out everything after the first 42 pairs
cdatasub <- cdatasub[1:42,]
#order rows: OrigCodeNew, DestCodeNew, TotalNoIntra
cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, TotalNoIntra, everything())


#plot the flows
#use od2line from Robin Lovelace's stplanr package
travel_network <- od2line(flow=cdatasub,zones=LondonBNG)
#set line widths to be relative to the flow
w <- cdatasub$Total / max(cdatasub$Total) * 10
#plot
plot(travel_network, lwd=w)
plot(LondonBNG, add=T)

#OR PLOT ON LEAFLET
#transform to wgs84
travel_networkwgs <- spTransform(travel_network, "+init=epsg:4326")
#plot in leaflet
leaflet() %>% addTiles() %>% addPolylines(data = travel_networkwgs)

#OR AS A MATRIX
#create pivor table to turn paired list into matrix (and compute margins)
cdatasubmat <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "Total", margins = c("Orig","Dest"))
cdatasubmat

#plot commuter flows against distance and fit a line
plot1 <- qplot(cdata$dist, cdata$Total)
plot1 +stat_function(fun=function(x)x^-1, geom="line", aes(colour="^-2"))

#now origin & destination data
plot2 <- qplot(cdata$vi1_origpop, cdata$Total)
plot2 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))

plot3 <- qplot(cdata$wj2_destsal, cdata$Total)
plot3 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))


#set up parameter variables
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(cdatasub$Total)

#create flow estimates
vi1_mu <- cdatasub$vi1_origpop^mu
wj2_alpha <- cdatasub$wj2_destsal^alpha
dist_beta <- cdatasub$dist^beta
T1 <- vi1_mu*wj2_alpha*dist_beta
k <- T2/sum(T1)

#run model and store all of the new flow estimates in a new column
cdatasub$unconstrainedEst1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
sum(cdatasub$unconstrainedEst1)

#convert to matrix & examine
cdatasubmat1 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "unconstrainedEst1", margins = c("Orig","Dest"))
cdatasubmat1
cdatasubmat

#calc r^2 to determine "goodness-of-fit"
CalcRSquared <- function(observed, estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(cdatasub$Total,cdatasub$unconstrainedEst1)

#OR
#calc root mean squared error (RMSE) to determin "goodness-of-fit"
CalcRMSE <- function(observed, estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}
CalcRMSE(cdatasub$Total, cdatasub$unconstrainedEst1)


#plot flows to get a sense of how they look
qplot(cdata$Total) + geom_histogram()
#POISSON

#run the unconstrained model
uncosim <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
summary(uncosim)


#reassign new parameter values
k <- uncosim$coefficients[1]
mu <- uncosim$coefficients[2]
alpha <- uncosim$coefficients[3]
beta <- -uncosim$coefficients[4]

#now plug everything back into the Equation 6 model... (be careful with the positive and negative signing of the parameters as the beta parameter may not have been saved as negative so will need to force negative)
cdatasub$unconstrainedEst2 <- exp(k+(mu*log(cdatasub$vi1_origpop))+(alpha*log(cdatasub$wj2_destsal))-(beta*log(cdatasub$dist)))

#which is exactly the same as this...
cdatasub$unconstrainedEst2 <- (exp(k)*exp(mu*log(cdatasub$vi1_origpop))*exp(alpha*log(cdatasub$wj2_destsal))*exp(-beta*log(cdatasub$dist)))

#and of course, being R, there is an even easier way of doing this...
cdatasub$fitted <- fitted(uncosim)

#run the model
cdatasub$unconstrainedEst2 <- round(cdatasub$unconstrainedEst2,0)
sum(cdatasub$unconstrainedEst2)

#check as a matrix
cdatasubmat2 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "unconstrainedEst2",margins = c("Orig","Dest"))
cdatasubmat2

#check to see if the "goodness-of-fit" has improved
CalcRSquared(cdatasub$Total,cdatasub$unconstrainedEst2)
CalcRMSE(cdatasub$Total,cdatasub$unconstrainedEst2)
