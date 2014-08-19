library(mgcv)
library(ggplot2)


na.values <- is.na(redsox$px)
sum(na.values)
dfdf <- redsox[!na.values, ]

data <- subset(pitch_data, pitch_data$des == "Called Strike" | pitch_data$des == "Ball")

write.csv(data, "pitch_data2.csv")

lester <- read.csv("lester.csv", header=TRUE, stringsAsFactors=FALSE)
lester$stand <- gsub("L", "Left-handed Batter", lester$stand)
lester$stand <- gsub("R", "Right-handed Batter", lester$stand)
unique(lester$stand)
lester$stand <- as.factor(lester$stand)

lester$pitcher_name <- "Jon Lester"

les <- lester[lester$des == "Called Strike" | lester$des == "Ball", ]
les$des <- as.factor(les$des)

m <- bam(factor(des) ~ s(px, pz, by=factor(stand)) + factor(stand), 
          data=les, family = binomial(link='logit'))

qplot(m)

names(m)
head(m$fitted.values)
head(m$Vp)

les$fitted <- m$fitted.values

plot(m,pages=1,rug=FALSE)
plot(m,pages=1,rug=FALSE,seWithMean=TRUE)

p <- ggplot(data = les, aes(x=px, y=pz, z=fitted)) + 
        facet_grid(. ~ stand) +
        stat_density2d(geom="tile", aes(fill=..density..), n=30, contour=FALSE)
#         geom_contour(breaks=seq(min(les$fitted), max(les$fitted), length.out=10)) +
#         geom_tile(aes(fill=les$fitted), alpha=.5)
p


## http://www.r-bloggers.com/rethinking-loess-for-binomial-response-pitch-fx-strike-zone-maps/
library(gam)

les.l <- les[les$stand == "Left-handed Batter", ]
les.r <- les[les$stand == "Right-handed Batter", ]


fit.gam <- gam(des ~ lo(px, span=.5*aspect.ratio, degree=1) + lo(pz, span=.5, degree=1), 
               family=binomial(link="logit"))
myx.gam <- matrix(data=seq(from=-2, to=2, length=30), nrow=30, ncol=30)
myz.gam <- t(matrix(data=seq(from=0,to=5, length=30), nrow=30, ncol=30))
fitdata.gam <- data.frame(px=as.vector(myx.gam), pz=as.vector(myz.gam))

mypredict.gam <- predict(fit.gam, fitdata.gam, type="response")
mypredict.gam <- matrix(mypredict.gam,nrow=c(30,30))

df <- data.frame(fitdata.gam, z=as.vector(mypredict.gam))

attach(les)

aspect.ratio <- (max(px)-min(px))/(max(pz)-min(pz))

pred <- function(x) {

        fit.gam <- gam(data=x, des ~ lo(px, span=.5*aspect.ratio, degree=1) + lo(pz, span=.5, degree=1), 
               family=binomial(link="logit"))
        
        myx.gam <- matrix(data=seq(from=-2, to=2, length=30), nrow=30, ncol=30)
        myz.gam <- t(matrix(data=seq(from=0,to=5, length=30), nrow=30, ncol=30))
        
        fitdata.gam <- data.frame(px=as.vector(myx.gam), pz=as.vector(myz.gam))

        mypredict.gam <- predict(fit.gam, fitdata.gam, type="response")
        mypredict.gam <- matrix(mypredict.gam,nrow=c(30,30))

        df <- data.frame(fitdata.gam, z=as.vector(mypredict.gam))
        df
}

df.l <- pred(les.l)
df.r <- pred(les.r)

head(df.l)

# plot

filled.contour(x=seq(from=-2, to=2, length=30), y=seq(from=0, to=5, length=30), z=mypredict.gam, axes=T, zlim=c(0,1), nlevels=50,
               color=colorRampPalette(c("darkblue", "blue4", "darkgreen", "green4", "greenyellow", "yellow", "gold", "orange", "darkorange", "red", "darkred")),
               main="Bruce Froemming Strike Zone Map (GAM Package)", xlab="Horizontal Location (ft.)", ylab="Vertical Location (ft.)",
               
               plot.axes={
                       axis(1, at=c(-2,-1,0,1,2), pos=0, labels=c(-2,-1,0,1,2), las=0, col="black")
                       axis(2, at=c(0,1,2,3,4,5), pos=-2, labels=c(0,1,2,3,4,5), las=0, col="black")
#                        rect(-0.708335, mean(les$sz_bot), 0.708335, mean(les$sz_top), border="black", lty="dashed", lwd=2)
               },
               
               key.axes={
                       ylim=c(0,1.0)
                       axis(4, at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), labels=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), pos=1, las=0, col="black")
               })

text(1.4, 2.5, "Probability of Strike Call", cex=1.1, srt=90)

sz.bot <- 1.6
sz.top <- 3.6
sz.left <- -0.95
sz.right <- 0.95
sz.dim <- data.frame(cbind(px = c(sz.left, sz.right, sz.right, sz.left, sz.left), pz = c(sz.bot, sz.bot, sz.top, sz.top, sz.bot)))

p1 <- ggplot(df.l, aes(x=px, y=pz)) +
        geom_path(data=sz.dim, aes(px, pz), colour="black", linetype=2) +
        geom_contour(aes(z=df.l$z), breaks=seq(0,1,length.out=10)) +
#         scale_color_gradientn(colours = topo.colors(10)) +
        xlim(-2.5,2.5) + ylim(0,5) +
        theme_minimal() +
        theme(aspect.ratio=1, strip.text.x=element_text(size=12), legend.position="right", plot.title=element_text(size=14),
              axis.title=element_text(size=12), axis.title.x=element_text(vjust=0), axis.title.y=element_text(vjust=1)) + 
        ggtitle("Left-handed Batters") + xlab("Horizontal Location") + ylab("Height")
#         stat_contour(geom="tile", aes(fill=as.vector(mypredict.gam)))

p2 <- ggplot(df.r, aes(x=px, y=pz)) +
        geom_contour(aes(z=z), breaks=seq(0,1,length.out=10), size=0.5) +
#         geom_tile(fill=df.r$z) +
#         scale_color_gradientn(colours = topo.colors(10)) +
        geom_path(data=sz.dim, aes(px, pz), colour="black", alpha=1, linetype=5) +
        xlim(-2.5,2.5) + ylim(0,5) +
        theme_minimal() +
        theme(aspect.ratio=1, strip.text.x=element_text(size=12), legend.position="right", plot.title=element_text(size=14),
              axis.title=element_text(size=12), axis.title.x=element_text(vjust=0), axis.title.y=element_text(vjust=1)) + 
        ggtitle("Right-handed Batters") + xlab("Horizontal Location") + ylab("Height")
#         stat_contour(geom="tile", aes(fill=as.vector(mypredict.gam)))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

multiplot(p1, p2, cols=2)

library(gridExtra)
grid.arrange(p1, p2, ncol=2)

q1 <- ggplot(df.l, aes(x=px, y=pz)) +
        stat_contour(geom="polygon", bins=8, aes(z=z, fill=..level..)) +
        scale_fill_gradientn(colours = topo.colors(10), guide=guide_colorbar(title="P(Strike)")) +
#         scale_fill_gradientn(colours = rainbow(10), guide=guide_legend(title="P(Called Strike)")) +
#         scale_fill_continuous( name = "P(Called Strike)", low = "dodgerblue", high = "orange", guide="legend") +
#         geom_contour(breaks=seq(0,1,length.out=10), colour="#999999") +
        geom_path(data=sz.dim, aes(px, pz), colour="black", alpha=1, linetype=5) +
        xlim(-2,2) + ylim(0.5,4.5) +
        theme_minimal() +
        theme(aspect.ratio=1, strip.text.x=element_text(size=12), legend.position="bottom", plot.title=element_text(size=14),
              axis.title=element_text(size=12), axis.title.x=element_text(vjust=0), axis.title.y=element_text(vjust=1)) + 
        ggtitle("Left-handed Batters") + xlab("Horizontal Location") + ylab("Height")

q2 <- ggplot(df.r, aes(x=px, y=pz)) +
        stat_contour(geom="polygon", bins=8, aes(z=z, fill=..level..)) +
        scale_fill_gradientn(colours = topo.colors(10), guide=guide_colorbar(title="P(Strike)")) +
        #         scale_fill_gradientn(colours = rainbow(10), guide=guide_legend(title="P(Called Strike)")) +
        #         scale_fill_continuous( name = "P(Called Strike)", low = "dodgerblue", high = "orange", guide="legend") +
        #         geom_contour(breaks=seq(0,1,length.out=10), colour="#999999") +
        geom_path(data=sz.dim, aes(px, pz), colour="black", alpha=1, linetype=5) +
        xlim(-2,2) + ylim(0.5,4.5) +
        theme_minimal() +
        theme(aspect.ratio=1, strip.text.x=element_text(size=12), legend.position="bottom", plot.title=element_text(size=14),
              axis.title=element_text(size=12), axis.title.x=element_text(vjust=0), axis.title.y=element_text(vjust=1)) + 
        ggtitle("Right-handed Batters") + xlab("Horizontal Location") + ylab("Height")

grid.arrange(q1, q2, ncol=2)