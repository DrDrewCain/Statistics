# install.package(ggplot2)
# install.package(reshape2)
# install.package(dplyr)
# install.package(tidyverse)
# install.package(treemap)
# install.package(fmsb)
# install.package(reshape)
# install.package(MASS)
# install.package(vcd)
# install.package(wordcloud)
# install.package(RColorBrewer)
# install.package(ggcorrplot)
# install.package(BSDA)

#Please download dataset from the following:
#https://www.kaggle.com/datasets/sashankpillai/spotify-top-200-charts-20202021


library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyverse)
library(treemap)
library(fmsb)
library(reshape)
library(MASS)
library(vcd)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(ggcorrplot)
library(BSDA)

#### Plotting the attributes of the top 100 songs - Graph and analysis of the attributes of the top 100 songs in 2020 and 2021 ####

top2020<-read.csv("spotify_dataset")

# Multiplying the attributes by 10 to make them better
 top2020$Danceability<- top2020$Danceability*(100)
 top2020$Energy<- top2020$Energy*(100)
 top2020$Speechiness<- top2020$Speechiness*(100)
 top2020$Acousticness<- top2020$Acousticness*(100)
 top2020$Liveness<- top2020$Liveness*(100)
 top2020$Valence<- top2020$Valence*(100)
 top2020$Loudness<- top2020$Loudness*(10)


# Taking the top 5 ONLY because anymore would be too much artists
top10 <- top2020[c(1:10),]
# Only using the attributes in columns 13,14,15,16,17,18,19,22
top10test<- top10[,c(5,13,14,15,16,17,18,19,22)]
top10<- top10[, c(5,13,14,15,16,17,18,19,22)]
top10<- as.data.frame(top10)
top10.long <- melt(top10, id.vars=c("Song.Name"))
plot10<- ggplot(data=top10.long, aes(x=variable, y=value))+geom_bar(aes(y=value,
fill=Song.Name),stat="identity", alpha=0.8 , position="dodge")+ ylab("Value")+ xlab("Attributes to a
song")+ ggtitle("Attributes Distribution of the Top 10 songs in Spotify 2020 and 2021 ")
plot10


spotify_streams <- top2020 %>%
  arrange(desc(Streams)) %>%
  head(6)

# ggplot(data=spotify_streams) + geom_bar(aes(y= Streams, x=Artist)) + labs(title = "Artists with Most Streams")
ggplot(data=spotify_streams, aes(x=Artist, y=Streams))+geom_bar(aes(y=Streams,
fill=Artist),stat="identity", alpha=0.8 , position="dodge")+ ylab("Streams")+ xlab("Artists") + ggtitle("Spotify Artists with Most Stream of 2020 and 2021" )

# ggplot(data=spotify_streams) + geom_bar(aes(y= Streams, x=Artist)) + labs(title = "Artists with Most Streams")
ggplot(data=spotify_streams, aes(x=`Song.Name`, y=Streams))+geom_bar(aes(y=Streams,
fill=`Song.Name`),stat="identity", alpha=0.8 , position="dodge")+ ylab("Streams")+ xlab("Song Name")+ ggtitle("Spotify Songs with Most Stream of 2020 and 2021" ) + theme(axis.text.x = element_text(face="bold", size=4, angle=90))


#### Top artists - Graph and analysis of the top 10 artists that had the most songs in 2020 ###
artistG <- group_by(top2020, Artist)
artistG2 <- dplyr::summarise(artistG, count=n())
artistG2 <- arrange(artistG2, desc(count))
artistG3 <- filter(artistG2, count>19)

# Plotting of the number of songs the top 5 artists have in the Top 200 of Spotify
artistPlot <- ggplot(artistG3, aes(x=reorder(Artist,count),y=count))+
geom_bar(aes(y=count,fill=Artist), stat="identity")+
labs(x="Artists", y="Number of Songs",
title="Artists Has twenty or more Songs")+ theme(legend.position="none", axis.text.x =
element_text(angle = 60, hjust = 1))
artistPlot

# TAYLOR SWITFT Radar Chart
# Filtering out the songs
radarSong1 <- filter(top2020, Artist %in% c("Taylor Swift"))
# Only using attributes in columns 2,4,5,9,10,12,13
radarSong2<- radarSong1[, c(5,13,14,15,16,17,18,19,22)]
rownames(radarSong2)=radarSong2$Song.Name
radarSong3 <- radarSong2[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , radarSong3)
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),
rgb(0.5,0.4,0.8,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4))
radarchart( data , axistype=1 ,
#custom polygon
pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
#custom labels
vlcex=1 , title="Taylor Swift Top Songs"
)
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in ,
text.col = "black", cex=0.7, pt.cex=1.2)

# Justin Bieber Radar Chart
radarSong1 <- filter(top2020, Artist %in% c("Justin Bieber"))
radarSong2<- radarSong1[, c(5,13,14,15,16,17,18,19,22)]
rownames(radarSong2)=radarSong2$Song.Name
radarSong3 <- radarSong2[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , radarSong3)
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),
rgb(0.5,0.4,0.8,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4))
radarchart( data , axistype=1 ,
#custom polygon
pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
#custom labels
vlcex=1 , title="Justin Bieber Top Songs"
)
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in ,
text.col = "black", cex=0.7, pt.cex=1.5)

# Lil Uzi Vert Radar Chart
radarSong1 <- filter(top2020, Artist %in% c("Lil Uzi Vert"))
radarSong2<- radarSong1[, c(5,13,14,15,16,17,18,19,22)]
rownames(radarSong2)=radarSong2$Song.Name
radarSong3 <- radarSong2[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , radarSong3)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),
rgb(0.5,0.4,0.8,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4))
radarchart( data , axistype=1 ,
#custom polygon
pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
#custom labels
vlcex=1 , title="Lil Uzi Vert Top Songs"
)
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in ,
text.col = "black", cex=0.7, pt.cex=1.5)

# Juice WRLD Radar Chart
radarSong1 <- filter(top2020, Artist %in% c("Juice WRLD"))
radarSong2<- radarSong1[, c(5,13,14,15,16,17,18,19,22)]
rownames(radarSong2)=radarSong2$Song.Name
radarSong3 <- radarSong2[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , radarSong3)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),
rgb(0.5,0.4,0.8,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4))
radarchart( data , axistype=1 ,
#custom polygon
pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
#custom labels
vlcex=1 , title="Juice WRLD Top Songs"
)
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in ,
text.col = "black", cex=0.7, pt.cex=1.5)

# BTS Radar Chart
radarSong1 <- filter(top2020, artists %in% c("BTS"))
radarSong2<- radarSong1[, c(5,13,14,15,16,17,18,19,22)]
rownames(radarSong2)=radarSong2$Song.Name
radarSong3 <- radarSong2[, c(2,3,4,5,6,7)]
data=rbind(rep(100,6) , rep(0,6) , radarSong3)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),
rgb(0.5,0.4,0.8,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) , rgb(0.5,0.4,0.8,0.4))
radarchart( data , axistype=1 ,
#custom polygon
pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
#custom labels
vlcex=1 , title="BTS Top Songs"
)
legend(x=1.3, y=1.0, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in ,
text.col = "black", cex=0.7, pt.cex=1.5)

### Graph and Analysis of popular Tempos in the Top 200 songs ###
# Tempo classification
top2020$Tempoc[top2020$Tempo >= 66 & top2020$Tempo <76] <- "Adagio"
top2020$Tempoc[top2020$Tempo >= 76 & top2020$Tempo <108] <- "Andante"
top2020$Tempoc[top2020$Tempo >= 108 & top2020$Tempo <120] <- "Moderato"
top2020$Tempoc[top2020$Tempo >= 120 & top2020$Tempo <156 ] <- "Allegro"
top2020$Tempoc[top2020$Tempo >= 156 & top2020$Tempo <176] <- "Vivace"
top2020$Tempoc[top2020$Tempo >= 176 ] <- "Presto"
top2020$tlabel[top2020$Tempo >= 66 & top2020$Tempo <76] <-" 66- 76"
top2020$tlabel[top2020$Tempo >= 76 & top2020$Tempo <108] <- "76-108"
top2020$tlabel[top2020$Tempo >= 108 & top2020$Tempo <120] <- "108- 120"
top2020$tlabel[top2020$Tempo >= 120 & top2020$Tempo <156 ] <- "120 -156"
top2020$tlabel[top2020$Tempo >= 156 & top2020$Tempo <176] <- "156-176"
top2020$tlabel[top2020$Tempo >= 176 ] <- "> 176"

tempoC <- group_by(top2020, Tempoc ,tlabel )
tempoC2 <- dplyr::summarise(tempoC, count=n())
tempoC2 <- arrange(tempoC2, desc(count))

tempoBar<- ggplot(data=tempoC2, aes(x=Tempoc, y=count)) +geom_bar(aes(y=count),stat="identity", alpha=0.8,fill="cyan" )+ ylab("Count")+ xlab("Tempo Type")+ ggtitle("What is the most popular Tempo type? ")+ geom_text(aes(label=tlabel), vjust=1, color="maroon", size=3.5)+ theme_minimal()
tempoBar

### Correlation Heat Map #######
# Looking at all the attributes
mydata <- top2020[, c(13,14,15,16,17,18,19,20,21,22)]
head(mydata)
cormat <- round(cor(mydata),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

#Plotting the key
ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) +
geom_tile()
# plotting the heat map
library(ggplot2)
ggplot(data = melted_cormat, aes(X1, X2, fill = value, label = value))+
geom_tile(color = "white")+
scale_fill_gradient2(low = "blue", high = "red", mid = "white",
midpoint = 0, limit = c(-1,1), space = "Lab",
name="Pearson\nCorrelation") +
theme_minimal()+
theme(axis.text.x = element_text(angle = 45, vjust = 1,
size = 12, hjust = 1))+
coord_fixed()


### Bootstrap Correlation Technique ####

# Create function which is exactly like how we created the bootstrap in assignment3 with parameters (Data Sheet Input, X column, Y Column, Number of times Bootstrap is repeated, Alpha Level - Confidence Interval)
Assignment3<- function(mydata,x.index,y.index,nboot,confidence) 
{
    par(mfrow=c(1,1))
    ########## mysmooth.forKS - Square Root implementation #########################
    # This is creating a smoothing spline on a reduced variance data sample (Square root of y)
    smsp.strcv<-smooth.spline(mydata[[x.index]],mydata[[y.index]])
    # Storing the residuals on a reduced variance data sample (Square root of data)
    smspcv.resid<-mydata[[y.index]]-approx(smsp.strcv$x,smsp.strcv$y,mydata[[x.index]])$y
    # Calculating studentized residuals - a residual divided by the estimate of its standard deviation
    # You compute a studentized residual to be able to accurately compare residuals as they are now standardized
    sd.resid<-sqrt(sum(smspcv.resid^2)/(length(mydata[[1]]) -smsp.strcv$df))
    stud.resid<-smspcv.resid/sd.resid
    # Calculate D for distribution of studentized residuals for standard normal distribution
    D<-ks.test(stud.resid,pnorm)$statistic
    # Determining the smooth based on D distribution calculations
    my.smooth<-approx(smsp.strcv$x,smsp.strcv$y,mydata[[x.index]])$y
    str0 <- list(D=D,raw.resid=smspcv.resid,sd.resid=sd.resid,smooth=my.smooth)
    # Performing the bootstrap with the D distribution calculations (With studentized residuals)
    # Bootstrap is to perform computations on data itself to estimate variance of the generated statistics computed from the data.
    smooth.dist<-NULL
    base.smooth<-str0$smooth
    base.sd<-str0$sd.resid
    base.resid<-str0$raw.resid
    my.bootdata<-mydata
    n1<-length(base.smooth)
    # Run the loop - nboot times (Which we say is 1000)
    for(i in 1:nboot) 
    {
        bres<-sample(base.resid,length(base.resid),replace=T)
        boot.dat<-((base.smooth+bres))
        my.bootdata[[y.index]]<-boot.dat
        ########## mysmooth.forKS - No Square Root implementation #####################
        smsp.strcv<-smooth.spline(my.bootdata[[x.index]],(my.bootdata[[y.index]]))
        # Storing the residuals on a not reduced variance data sample ( Without Square root of data)
        smspcv.resid<-(my.bootdata[[y.index]])-approx(smsp.strcv$x,smsp.strcv$y,my.bootdata[[x.index]])$y
        # Calculating studentized residuals - a residual divided by the estimate of its standard deviation
        # You compute a studentized residual to be able to accurately compare residuals as they are now standardized
        sd.resid<-sqrt(sum(smspcv.resid^2)/(length(my.bootdata[[1]]) -smsp.strcv$df))
        stud.resid<-smspcv.resid/sd.resid
        # Calculate D for distribution of studentized residuals for standard normal distribution
        D<-ks.test(stud.resid,pnorm)$statistic
        my.smooth<-approx(smsp.strcv$x,smsp.strcv$y,my.bootdata[[x.index]])$y
        bstr0 <- list(D=D,raw.resid=smspcv.resid,sd.resid=sd.resid,smooth=my.smooth)
        boot.smooth<-bstr0$smooth
        smooth.dist<-rbind(smooth.dist,boot.smooth-base.smooth)
    }
    # n1 times (Which is the length of the data set - 37, 37 observations in the data)
    n1<-length(smooth.dist[1,])
    alpha<-1-confidence
    LB<-NULL
    UB<-NULL
    # Run the loop - n1 times (Which is the length of the data set - 37, 37 observations in the data)
    for(i in 1:n1)
    {
        s1<-sort(smooth.dist[,i])
        n2<-length(s1)
        v1<-c(1:n2)/n2
        # Creates 2 different LB and UB from the previous bootstrap run
        bvec<-approx(v1,s1,c(alpha/2,1-alpha/2))$y
        LB<-c(LB,base.smooth[i]-bvec[2])
        UB<-c(UB,base.smooth[i]-bvec[1])
    }
    # Sets up empty plot
    plot(rep(mydata[[x.index]],4),c(LB,base.smooth,UB,mydata[[y.index]]),xlab="X",ylab="Y",type="n")
    # Blue Line - with the forced linear fit on the data - smoothing spline with 2 degrees of freedom linear fit
    lines(smooth.spline(mydata[[x.index]],mydata[[y.index]],df=2),col=4)
    # Graph the dots
    points(mydata[[x.index]],mydata[[y.index]])
    o1<-order(mydata[[x.index]])
    # Two bootstrap plots using the bootstrap data calculated
    lines(mydata[[x.index]][o1],LB[o1],col=2)
    lines(mydata[[x.index]][o1],UB[o1],col=2)
    # Green Line - default smoothing spline as done in previous assignment
    lines(mydata[[x.index]][o1],base.smooth[o1],col=3)
}
# Commands used to generate the correlations between the following factors:
#Danceability and Energy
Assignment3(top2020,14,15,1000,0.95)
#Danceability and Loudness (14 and 16)
Assignment3(top2020,14,16, 1000,0.95)
#Danceability and acousticness
Assignment3(top2020,14,18,1000,0.95)
#Danceability and Liveliness
Assignment3(top2020,14,19,1000,0.95)
#Danceability and Valence 
Assignment3(top2020,14,21, 1000, 0.95)
#Energy and Loudness
Assignment3(top2020,15,16,1000,0.95)
#Energy and Speechiness
Assignment3(top2020,15,17, 1000, 0.95)
#Energy and Acoustiness
Assignment3(top2020,15,18,1000,0.95)
#Energy and Liveliness
Assignment3(top2020,15,19,1000,0.95)
#Energy and Valence
Assignment3(top2020,15,21,1000,0.95)
#Loudness and Valence
Assignment3(top2020,16,21,1000,0.95)
#Loudness and Liveliness 
Assignment3(top2020,16,19,1000,0.95)
#Energy and Tempo
Assignment3(top2020,15,22,1000,0.95)


### Confidence Interval Technique ####
for(i in 13:22) {
    data <- as.matrix(top2020[i])
    cp <- data.frame(data)
    hi <- z.test(data,sigma.x = 0.5)
    cat(sprintf("We are 95 percent confident that the true mean value of %s of future hit songs on
    Spotify will fall between \"%f\" and \"%f\"\n", names(cp), hi$conf.int[1], hi$conf.int[2]))

}# The following is an graphical analysis of Spotify's top 200 songs 
