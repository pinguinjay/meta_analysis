knitr::opts_chunk$set(echo = TRUE)

## Network meta-analysis
## read data
# desantis_long<-read.delim("clipboard")

## install.packages("readxl")
library("readxl")

desantis_long<-read_excel("DeSantis_2014.xlsx",sheet = "DeSantis")
knitr::kable(desantis_long,cation = "DeSantis 2014")
desantis_long

## Use R package netmeta
library(netmeta)

# Transform data from long arm-based format to contrast-based format
# Argument 'sm' has to be used for odds ratio as summary measure; by
# default the risk ratio is used in the metabin function called
# internally.
desantis_long$txt<-as.factor(desantis_long$t)
# Rename all levels of txt
levels(desantis_long$txt) <- c("Placebo","NAL","ACA","NAL+ACA")

## Relevel the order of treatments according to alphabet
desantis_long$txt <- factor(desantis_long$txt, levels = c("ACA", "NAL", "NAL+ACA", "Placebo"))


desantis_wide <- pairwise(treat=txt, event=r, n=n,
studlab=id, data=desantis_long, sm="OR")
desantis_wide

desantis_wide$pair <- paste(desantis_wide$txt1, "-", desantis_wide$txt2)

meta_subgp<-metagen(TE, seTE, sm = "OR", subgroup=pair, data=desantis_wide)
meta_subgp

forest(meta_subgp)


# Define order of treatments in printouts and forest plots
#
txts <- c("Placebo","NAL","ACA","NAL+ACA")

# Conduct network meta-analysis
net1 <- netmeta(desantis_wide,reference.group = "Placebo",
                sm = "OR", fixed = FALSE, random = TRUE,
                backtransf=TRUE, seq = txts, nchar.trts = 38)
summary(net1)

netgraph(net1)

## network graphs looking like those in Stata
n.subj<-desantis_long$n
n.txts<-desantis_long$txt
netsize<-data.frame(n.txts, n.subj)
pointsize<-aggregate(n.subj ~ factor(n.txts), data=netsize, FUN=sum)

netgraph(net1, points=TRUE, multiarm=FALSE, plastic=FALSE, 
         scale=1.1, cex.points=pointsize$n.subj/300, cex=1.5,
         number.of.studies=TRUE, thickness="number.of.studies", 
		 pos.number.of.studies=0.3, col.number.of.studies="black", 
		 bg.number.of.studies=FALSE, offset=0.05, 
		 cex.number.of.studies=0.8, col="lightblue", 
		 col.points="steelblue")


forest(net1)

## Create forest plot according to effect size
forest(net1,reference.group = "Placebo",sortvar = TE,xlim=c(0.2,4),at=c(0.2,0.5,0.8,1,1.5,2,4),
       col.square = "blue",
       smlab = "Treatments vs. Placebo \n (Alcohol dependence)")

ranking<-netrank(net1, small.values = "good")
ranking

netleague <- netleague(net1,fixed = FALSE, bracket = "(", separator = " to ", digits=2, 
seq = netrank(net1, small.values = "good"))

write.csv(netleague$random, "desantis_netleague.csv")

## Comparison-adjusted Funnel Plots
ranking.r<-as.data.frame(ranking$Pscore.random)
names(ranking.r)<-"pscore"
ranking.r$trtnames<-rownames(ranking.r)
ranking.order<-ranking.r[order(ranking.r$pscore, decreasing = TRUE),]

funnel <- funnel(net1,order = ranking.order$trtnames,pch = 19,
legend=FALSE,method.bias = TRUE,studlab = TRUE,cex.studlab = 0.7)

## Decompose variations into between and within designs
decomp.design(net1)

## node-splitting model
netsplit(net1)

# pdf(file="desantis_nodeplit.pdf",width=8,height=20,paper='special') 

forest(netsplit(net1))

# dev.off()

