###################################################
### chunk number 76: moths
###################################################
moths <- read.table("/home/mae/Documents/Summervillle_CristMothsALL_mapTest.txt", header=TRUE)
rowsnct <- c(4,7,9,11,6,3,5,1,10,12,8,2) 
rowswap<- c(16, 20,21,14,17,19,15,18,13)
#identify
mothsnct <- moths[rowsnct,]
mothswap <- moths[rowswap,]
library(maps)

#wap <- map.text("state",xlim=c(-83, -82.3), ylim=c(39.4,39.7))
#map.text("state",xlim=c(-85.1, -84.5), ylim=c(39.4,39.6))
#text(moths[['long']], moths[['lat']], moths[['site']], cex=.5)
#identify(wap)
#locator()
map.text("state",xlim=c(-85.5, -82), ylim=c(38.9,39.95))
text(-85.5, 39.9,  labels="Indiana", adj=c(0,1))
text(-83, 39.9,  labels="Ohio", adj=c(1,1))
text(-84.5, 38.5,  labels="Kentucky", adj=c(0,0))
map.axes()

points(moths[['long']], moths[['lat']], pch=1)
n <- dim(mothsnct)[1]
x <- cos( seq(pi, 1.25*2*pi, length=n) )
y <- sin(seq(pi, 1.25*2*pi, length=n) )

x2 <- .3*x + mean(mothsnct[['long']])
y2 <- .25*y + mean(mothsnct[['lat']])

segments(mothsnct[['long']], mothsnct[['lat']], x2, y2)

x3 <- .35*x + mean(mothsnct[['long']])
y3 <- .3*y + mean(mothsnct[['lat']])

text(x3,y3, mothsnct[['spp']])

n <- dim(mothswap)[1]
x <- cos( seq(pi, 1.25*2*pi, length=n) )
y <- sin(seq(pi, 1.25*2*pi, length=n) )

x2 <- .3*x + mean(mothswap[['long']])
y2 <- .25*y + mean(mothswap[['lat']])

x2 <- .3*x + -82.531
y2 <- .25*y + 39.584

segments(mothswap[['long']], mothswap[['lat']], x2, y2)

x3 <- .35*x + -82.531
y3 <- .3*y + 39.584

text(x3,y3, mothswap[['spp']], adj=c(.75,0.5))


###################################################
### chunk number 77:  eval=FALSE
###################################################
## data(moths)
moths <- read.table("/home/mae/Documents/Summervillle_CristMothsALL.txt", header=TRUE)


###################################################
### chunk number 78: 
###################################################
a1 <- mean(moths[['spp']])



###################################################
### chunk number 79: 
###################################################
a2 <- sum(c(NCT=179, WAP=173) * c(12,9)/21)
g <- 230


###################################################
### chunk number 80: 
###################################################
b1 <- a2-a1
b2 <- g-a2
abg <- c(a1=a1, b1=b1, a2=a2, b2=b2, g=g)
abg
a1 + b1 + b2 == g


###################################################
### chunk number 81: partplot
###################################################
par(las=2)
bp <- barplot(abg[c(1,2,4,5)], hor=T, xlim=c(0, 250), space=1,
              names=expression(alpha[1],beta[1],beta[2],gamma))
text(abg[1]/2, bp[1], c("Mean site\nrichness"), cex=.8)
text(abg[2]/2, bp[2], c("Species not in sites,\nbut within ecoregion"), cex=.8)
text(abg[4]/2, bp[3], c("Species not\nin ecoregions"), cex=.8)
text(abg[5]/2, bp[4], c("Regional richness (all sampled species)"), cex=.8)
