library(igraph)
require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(broom)
require(brainGraph)

setwd("C:/Users/matth/Documents/casa0002/question1")

#create var that captures the .csv file
file_network <- "london_network.csv"

#read-in .csv file
data <- read.csv(file_network,header=TRUE)

#turn read-in .csv file into a data.frame obj
network <- data.frame(data)

#add weight column that is equal to 1 / distance
network$weight <- (1/network$distance)

#construct the network
node1 <- network$station_1_name
node2 <- network$station_2_name
weight <- network$weight

df <- data.frame(node1,node2,weight)

#create g_tube object as a igraph graph object, that is undirected
g_tube <- graph.data.frame(df,directed=FALSE)

#create function to standardize the layout of all graph plots
graphPlot <- function(graph) {
  #set radius_vertex & width_lines variables
  radius_vertex <- 3
  width_lines <- 6
  
  #set seed so that the layout is the same every time it is plotted
  set.seed(0010)
  
  #set layout1 to automatically choose the best layout for the graph
  layout1 <- layout_components(graph)
  
  #plot the graph with the settings specified above
  plot(graph,layout=layout1,vertex.size=radius_vertex,vertex.label.cex=1)
}

#create function to create the network
createNetwork <- function(node1s,node2s,weights) {
  node1 <<- node1s
  node2 <<- node2s
  weight <<- weights
}

#plot the original network
graphPlot(g_tube)

#get global efficiency of full network
eff = efficiency(g_tube, type = "global")
effpct <- ((eff/eff) * 100)
cat("Initial efficiency: ",effpct,"%")

#####################################
######### DEGREE CENTRALITY ######### 
#####################################

#calc degree of each station
deg1 <- degree(g_tube)

#determine which station has the greatest degree
names(deg1)[deg1==max(deg1)]
#KING'S CROSS ST. PANCRAS

#create NETWORKDEG1 from NETWORK, where all links with KING'S CROSS ST. PANCRAS are omitted
networkDeg1 <- network[which(network$station_1_name != "King's Cross St. Pancras"),]
networkDeg1 <- networkDeg1[which(networkDeg1$station_2_name != "King's Cross St. Pancras"),]
#12 links dropped, which matches the degree of king's cross

#check how many links have KING'S CROSS as the origin or destination
nrow(network[which(network$station_1_name == "King's Cross St. Pancras"),])
#1
nrow(network[which(network$station_2_name == "King's Cross St. Pancras"),])
#11
# 1 + 11 = 12, which matches

#create new network
node1 <- networkDeg1$station_1_name
node2 <- networkDeg1$station_2_name
weight <- networkDeg1$weight
dfDeg1 <- data.frame(node1,node2,weight)

#create the graph object and plot it
g_deg1 <- graph.data.frame(dfDeg1,directed = F)
graphPlot(g_deg1)

#calc changes in efficiency
effDeg1 <- efficiency(g_deg1,type="global")
effDeg1Pct <- (effDeg1/eff) *100
effDeg1Chng <- effDeg1Pct - effpct

#calc degree centrality of the new network
deg2 <- degree(g_deg1)

#determine which station has the greatest degree
names(deg2)[deg2==max(deg2)]
#BAKER STREET

#create NETWORKDEG2 from NETWORKDEG1, where all links with BAKER STREET are omitted
networkDeg2 <- networkDeg1[which(networkDeg1$station_1_name != "Baker Street"),]
networkDeg2 <- networkDeg2[which(networkDeg2$station_2_name != "Baker Street"),]
#10 links dropped, which matches the degree of BAKER STREET

#check how many links have BAKER STREET as the origin or destination
nrow(networkDeg1[which(networkDeg1$station_1_name == "Baker Street"),])
#10
nrow(networkDeg1[which(networkDeg1$station_2_name == "Baker Street"),])
#0
# 0 + 10 = 10, which matches

#create new network
node1 <- networkDeg2$station_1_name
node2 <- networkDeg2$station_2_name
weight <- networkDeg2$weight
dfDeg2 <- data.frame(node1,node2,weight)

#create the graph object and plot it
g_deg2 <- graph.data.frame(dfDeg2,directed = F)
graphPlot(g_deg2)

#calc changes in efficiency
effDeg2 <- efficiency(g_deg2,type="global")
effDeg2Pct <- (effDeg2/eff) *100
effDeg2Chng <- effDeg2Pct - effDeg1Pct

cat("Initial Efficiency: ",100,"%")
cat("Iteration 1 Efficiency: ",effDeg1Pct,"%")
cat(" Change in Efficiency: ",effDeg1Chng,"%")
cat("Iteration 2 Efficiency: ",effDeg2Pct,"%")
cat(" Change in Efficiency: ",effDeg2Chng,"%")


##########################################
######### BETWEENNESS CENTRALITY ######### 
##########################################

#reset network nodes and weights
createNetwork(network$station_1_name,network$station_2_name,network$weight)

#calc the betweeness centrality for each station
bet1 <- betweenness(g_tube, v = V(g_tube),directed = F, weights = weight)

#determine which station has the greatest b/t centrality
names(bet1)[bet1==max(bet1)]
#GREEN PARK

#create NETWORKBET1 from NETWORK, where all links with GREEN PARK are omitted
networkBet1 <- network[which(network$station_1_name != "Green Park"),]
networkBet1 <- networkBet1[which(networkBet1$station_2_name != "Green Park"),]
#6 links dropped

#check how many links have GREEN PARK as the origin or destination
nrow(network[which(network$station_1_name == "Green Park"),])
#5
nrow(network[which(network$station_2_name == "Green Park"),])
#1
# 5 + 1 = 6, which matches

#create new network
createNetwork(networkBet1$station_1_name,networkBet1$station_2_name,networkBet1$weight)
dfBet1 <- data.frame(node1,node2,weight)

#create the graph object and plot it
g_bet1 <- graph.data.frame(dfBet1,directed = F)
graphPlot(g_bet1)

#calc changes in efficiency
effBet1 <- efficiency(g_bet1,type="global")
effBet1Pct <- (effBet1/eff) *100
effBet1Chng <- effBet1Pct - effpct

#calc the betweeness centrality for each station
bet2 <- betweenness(g_bet1, v = V(g_bet1), directed = F, weights = NULL)

#determine which station has the greatest b/t centrality
names(bet2)[bet2==max(bet2)]
#KING'S CROSS ST. PANCRAS

#create NETWORKBET2 from NETWORKBET1, where all links with KING'S CROSS ST. PANCRAS are omitted
networkBet2 <- networkBet1[which(networkBet1$station_1_name != "King's Cross St. Pancras"),]
networkBet2 <- networkBet2[which(networkBet2$station_2_name != "King's Cross St. Pancras"),]
#12 links dropped

#check how many links have KING'S CROSS as the origin or destination
nrow(networkBet1[which(networkBet1$station_1_name == "King's Cross St. Pancras"),])
#1
nrow(networkBet1[which(networkBet1$station_2_name == "King's Cross St. Pancras"),])
#11
# 1 + 11 = 12, which matches

#create new network
createNetwork(networkBet2$station_1_name,networkBet2$station_2_name,networkBet2$weight)
dfBet2 <- data.frame(node1,node2,weight)

#create the graph object and plot it
g_bet2 <- graph.data.frame(dfBet2,directed = F)
graphPlot(g_bet2)

#calc changes in efficiency
effBet2 <- efficiency(g_bet2,type="global")
effBet2Pct <- (effBet2/eff) *100
effBet2Chng <- effBet2Pct - effBet1Pct


cat("Initial Efficiency: ",100,"%")
cat("Iteration 1 Efficiency: ",effBet1Pct,"%")
cat(" Change in Efficiency: ",effBet1Chng,"%")
cat("Iteration 2 Efficiency: ",effBet2Pct,"%")
cat(" Change in Efficiency: ",effBet2Chng,"%")


##########################################
########### CENTRALITY INDEX ############# 
##########################################

#reset network nodes and weights
createNetwork(network$station_1_name,network$station_2_name,network$weight)

#calc the degree and betweenness centralities for each station
degdex1 <- degree(g_tube)
betdex1 <- betweenness(g_tube, directed = F, weights = weight)

#write the 2 centrality measures to csv's
write.csv(degdex1,"degdex1.csv")
write.csv(betdex1,"betdex1.csv")

#check csv's to see which station has the highest index value
#BAKER STREET

#create NETWORKBET1 from NETWORK, where all links with BAKER STREET are omitted
networkDex1 <- network[which(network$station_1_name != "Baker Street"),]
networkDex1 <- networkDex1[which(networkDex1$station_2_name != "Baker Street"),]
#10 links dropped

#check how many links have KING'S CROSS as the origin or destination
nrow(network[which(network$station_1_name == "Baker Street"),])
#10
nrow(network[which(network$station_2_name == "Baker Street"),])
#0
# 10 + 0 = 10, which matches

#create new network
createNetwork(networkDex1$station_1_name,networkDex1$station_2_name,networkDex1$weight)
dfDex1 <- data.frame(node1,node2,weight)

#create the graph object and plot it
g_dex1 <- graph.data.frame(dfDex1,directed = F)
graphPlot(g_dex1)

#calc changes in efficiency
effDex1 <- efficiency(g_dex1,type="global")
effDex1Pct <- (effDex1/eff) *100
effDex1Chng <- effDex1Pct - effpct

#calc the degree and betweenness centralities for each station
degdex2 <- degree(g_dex1)
betdex2 <- betweenness(g_dex1, directed = F, weights = weight)

#write the 2 centrality measures to csv's
write.csv(degdex2,"degdex2.csv")
write.csv(betdex2,"betdex2.csv")

#check csv's to see which station has the highest index value
#GREEN PARK

#create NETWORKDEX2 from NETWORKDEX1, where all links with GREEN PARK are omitted
networkDex2 <- networkDex1[which(networkDex1$station_1_name != "Green Park"),]
networkDex2 <- networkDex2[which(networkDex2$station_2_name != "Green Park"),]
#6 links dropped

#check how many links have GREEN PARK as the origin or destination
nrow(network[which(network$station_1_name == "Green Park"),])
#5
nrow(network[which(network$station_2_name == "Green Park"),])
#1
# 5 + 1 = 6, which matches

#create new network
createNetwork(networkDex2$station_1_name,networkDex2$station_2_name,networkDex2$weight)
dfDex2 <- data.frame(node1,node2,weight)

#create the graph object and plot it
g_dex2 <- graph.data.frame(dfDex2,directed = F)
graphPlot(g_dex2)

#calc changes in efficiency
effDex2 <- efficiency(g_dex2,type="global")
effDex2Pct <- (effDex2/eff) *100
effDex2Chng <- effDex2Pct - effDex1Pct

#calc the degree and betweenness centralities for each station
degdex3 <- degree(g_dex2)
betdex3 <- betweenness(g_dex2, directed = F, weights = weight)

cat("Initial Efficiency: ",100,"%")
cat("Iteration 1 Efficiency: ",effDex1Pct,"%")
cat(" Change in Efficiency: ",effDex1Chng,"%")
cat("Iteration 2 Efficiency: ",effDex2Pct,"%")
cat(" Change in Efficiency: ",effDex2Chng,"%")
cat("Iteration 3 Efficiency: ",effDex3Pct,"%")
cat(" Change in Efficiency: ",effDex3Chng,"%")

write.csv(deg1,"deg1.csv")
write.csv(bet1,"bet1.csv")
