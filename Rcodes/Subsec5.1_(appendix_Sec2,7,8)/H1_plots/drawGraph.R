

#=====================Setting 1====================

load(file = 'BAGofT1rej.rda')
BAGofTr <- pRes$rejM
load(file = 'HL1rej.rda')
HLr <- pRes$rejM
load(file = 'CH1rej.rda')
CHr <- pRes$rejM
load(file = 'GRP1rej.rda')
GRPr <- pRes$rejM

plotDat<- data.frame(it = rep(c(1:3), 4 ),
                     rej = c(HLr, CHr, GRPr, BAGofTr),
                     legend = c(rep("HL", 3), rep("CH", 3), rep("GRP", 3), rep("BAGofT", 3)) )

# point size
psize <- 2.2

library(ggplot2)

pd <- position_dodge(0.1) # move them .05 to the left and right

p1 <- ggplot(data = plotDat, aes(x = it, y = rej)) + 
  labs(title=bquote("Setting 1, "~gamma~"="~1.),
       y="Rejection rate", x="Number of observations") + 
  ylim(0, 1) + 
  theme(legend.position = "top") +
  geom_line(size=0.2,aes(color = legend), show.legend = F) + 
  geom_point(size=psize ,aes(color = legend, shape = legend),alpha = 0.65) + 
  scale_x_discrete(limits=as.character(c(100,200,800))) 
#plot(p1)

#=====================Setting 2====================

load(file = 'BAGofT2rej.rda')
BAGofTr <- pRes$rejM
load(file = 'HL2rej.rda')
HLr <- pRes$rejM
load(file = 'CH2rej.rda')
CHr <- pRes$rejM
load(file = 'GRP2rej.rda')
GRPr <- pRes$rejM

plotDat<- data.frame(it = rep(c(1:3), 4 ),
                     rej = c(HLr, CHr, GRPr, BAGofTr),
                     legend = c(rep("HL", 3), rep("CH", 3), rep("GRP", 3), rep("BAGofT", 3)) )

library(ggplot2)

pd <- position_dodge(0.1) # move them .05 to the left and right

p2 <- ggplot(data = plotDat, aes(x = it, y = rej)) + 
  labs(title=bquote("Setting 2, "~gamma~"="~1.),
       y="Rejection rate", x="Number of observations") + 
  ylim(0, 1) + 
  theme(legend.position = "top") +
  geom_line(size=0.2,aes(color = legend), show.legend = F) + 
  geom_point(size=psize ,aes(color = legend, shape = legend),alpha = 0.65) + 
  scale_x_discrete(limits=as.character(c(100,200,800)))
# plot(p2)

#=====================Setting 3====================

load(file = 'BAGofT3rej.rda')
BAGofTr <- pRes$rejM
load(file = 'HL3rej.rda')
HLr <- pRes$rejM
load(file = 'CH3rej.rda')
CHr <- pRes$rejM
load(file = 'GRP3rej.rda')
GRPr <- pRes$rejM

plotDat<- data.frame(it = rep(c(1:3), 4 ),
                     rej = c(HLr, CHr, GRPr, BAGofTr),
                     legend = c(rep("HL", 3), rep("CH", 3), rep("GRP", 3), rep("BAGofT", 3)) )

library(ggplot2)

pd <- position_dodge(0.1) # move them .05 to the left and right

p3 <- ggplot(data = plotDat, aes(x = it, y = rej)) + 
  labs(title=bquote("Setting 3, "~gamma~"="~1.),
       y="Rejection rate", x="Number of observations") + 
  ylim(0, 1) + 
  theme(legend.position = "top") +
  geom_line(size=0.2,aes(color = legend), show.legend = F) + 
  geom_point(size=psize ,aes(color = legend, shape = legend),alpha = 0.65) + 
  scale_x_discrete(limits=as.character(c(100,200,800)))
# plot(p3)

#=====================Setting 4====================

load(file = 'BAGofT4rej.rda')
BAGofTr <- pRes$rejM
load(file = 'HL4rej.rda')
HLr <- pRes$rejM
load(file = 'CH4rej.rda')
CHr <- pRes$rejM
load(file = 'GRP4rej.rda')
GRPr <- pRes$rejM

plotDat<- data.frame(it = rep(c(1:3), 4 ),
                     rej = c(HLr, CHr, GRPr, BAGofTr),
                     legend = c(rep("HL", 3), rep("CH", 3), rep("GRP", 3), rep("BAGofT", 3)) )

library(ggplot2)

pd <- position_dodge(0.1) # move them .05 to the left and right

p4 <- ggplot(data = plotDat, aes(x = it, y = rej)) + 
  labs(title=bquote("Setting 1, "~gamma~"="~0.5),
       y="Rejection rate", x="Number of observations") +  
  ylim(0, 1) + 
  theme(legend.position = "top") +
  geom_line(size=0.2,aes(color = legend), show.legend = F) + 
  geom_point(size=psize ,aes(color = legend, shape = legend),alpha = 0.65) + 
  scale_x_discrete(limits=as.character(c(100,200,800)))
# plot(p4)

#=====================Setting 5====================

load(file = 'BAGofT5rej.rda')
BAGofTr <- pRes$rejM
load(file = 'HL5rej.rda')
HLr <- pRes$rejM
load(file = 'CH5rej.rda')
CHr <- pRes$rejM
load(file = 'GRP5rej.rda')
GRPr <- pRes$rejM

plotDat<- data.frame(it = rep(c(1:3), 4 ),
                     rej = c(HLr, CHr, GRPr, BAGofTr),
                     legend = c(rep("HL", 3), rep("CH", 3), rep("GRP", 3), rep("BAGofT", 3)) )

library(ggplot2)

pd <- position_dodge(0.1) # move them .05 to the left and right
p5 <- ggplot(data = plotDat, aes(x = it, y = rej)) + 
  labs(title=bquote("Setting 2, "~gamma~"="~0.5),
       y="Rejection rate", x="Number of observations") + 
  ylim(0, 1) + 
  theme(legend.position = "top") +
  geom_line(size=0.2,aes(color = legend), show.legend = F) + 
  geom_point(size=psize ,aes(color = legend, shape = legend),alpha = 0.65) + 
  scale_x_discrete(limits=as.character(c(100,200,800)))
# plot(p5)

#=====================Setting 6====================

load(file = 'BAGofT6rej.rda')
BAGofTr <- pRes$rejM
load(file = 'HL6rej.rda')
HLr <- pRes$rejM
load(file = 'CH6rej.rda')
CHr <- pRes$rejM
load(file = 'GRP6rej.rda')
GRPr <- pRes$rejM

plotDat<- data.frame(it = rep(c(1:3), 4 ),
                     rej = c(HLr, CHr, GRPr, BAGofTr),
                     legend = c(rep("HL", 3), rep("CH", 3), rep("GRP", 3), rep("BAGofT", 3)) )

library(ggplot2)

pd <- position_dodge(0.1) # move them .05 to the left and right

p6 <- ggplot(data = plotDat, aes(x = it, y = rej)) + 
  labs(title=bquote("Setting 3, "~gamma~"="~0.5),
       y="Rejection rate", x="Number of observations") +  
  ylim(0, 1) + 
  theme(legend.position = "top") +
  geom_line(size=0.2,aes(color = legend), show.legend = F) + 
  geom_point(size=psize ,aes(color = legend, shape = legend),alpha = 0.65) + 
  scale_x_discrete(limits=as.character(c(100,200,800)))
#plot(p6)






library(gridExtra)
library(cowplot)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p1)


p1 <- p1 + theme(legend.position="none")
p2 <- p2 + theme(legend.position="none")
p3 <- p3 + theme(legend.position="none")
p4 <- p4 + theme(legend.position="none")
p5 <- p5 + theme(legend.position="none")
p6 <- p6 + theme(legend.position="none")


blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
# # size: 8.5x4
# grid.arrange(p1_1502, p1_3002,   p1_10002,
#              blankPlot, legend, blankPlot,
#              ncol=3, nrow = 2, widths=c(2.3, 2.3, 2.3), heights = c(2.5, 0.2))


# 5.95 * 4.39
pdf(file = "power2.pdf", width=9, height=5)
grid.arrange(p1,   p2, p3, 
             p4,   p5, p6,
             blankPlot, legend, blankPlot,
             ncol=3, nrow = 3, widths=c(2.3, 2.3,2.3), heights = c(2.5, 2.5, 0.2))
dev.off()

