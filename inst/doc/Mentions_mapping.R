## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- load packages, include=FALSE---------------------------------------
library(twittnet)
library(tibble)
library(rtweet)
library(network)
library(sna)
library(GGally)
library(scales)


## ---- recip_mentioners-----------------------------------------------------------------------------
MH.mentioners<- recip_mentioners(user="817544938623737858", min.tie= 2, startday=Sys.Date()-30, verbose=F)

## ----check mentions--------------------------------------------------------------------------------
MH.mentioners$mentions[1:3,]

## ----check mentions contacts-----------------------------------------------------------------------
contact.info<- lookup_users(MH.mentioners$mentions$contact)
contact.info[,c(1,8,9)]

## ----recip_mentioners_network----------------------------------------------------------------------
MH.network<- recip_mentioners_network("817544938623737858", min.tie=2, startday= Sys.Date()-30)

## ----check sociogram-------------------------------------------------------------------------------
MH.network$sociogram[1:3,1:3]

## ----prep for vis----------------------------------------------------------------------------------
net<- network(MH.network$sociogram, directed = F)
edges<- numeric(0)
for(i in 1:(nrow(MH.network$sociogram)-1)){
  edges<- append(edges, MH.network$sociogram[(i+1):nrow(MH.network$sociogram),i])
}
edges<- as.numeric(edges[edges>0])


## ---- summary_plot, fig.height=8-------------------------------------------------------------------
ggnet2(net, label=T, edge.label = edges, size=20,color="darkblue", label.trim= 3, layout.par = list(cell.jitter = 0.75),   label.color="white", label.size = 10,edge.color = "cadetblue",  
       edge.label.color = "darkblue", edge.label.size = 10, edge.size = log(edges)+.5)



