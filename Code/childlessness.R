### Childlessness country comparison ###########################################

library(tidyverse)
plotheight <- 9
plotwidth <- 9


### Data #######################################################################

# Sweden
Sweden <- data.frame(Country=rep("Sweden",6),
                     Cohort=c("1950-59","1960-69","1970-79",
                              "1950-59","1960-69","1970-79"),
                     Gender=c(rep("Men",3),rep("Women",3)),
                     Childless=c(0.219,0.236,0.247,
                                 0.150,0.162,0.174))  

SwedenCohort <- c("1950-59","1960-69","1970-78")

# Korea
Korea <- data.frame(Country=rep("South Korea",6),
                    Cohort=c("1950-59","1960-69","1970-79",
                             "1950-59","1960-69","1970-79"),
                    Gender=c(rep("Men",3),rep("Women",3)),
                    Childless=c(0.05814,0.13644,0.25531,
                                0.04591,0.07137,0.12881))  

KoreaCohort <- c("1950-59","1960-69","1970-77")

# Combination
Childless <- rbind(Sweden,Korea)


### Figure 1 ###################################################################

childless <- Childless |> ggplot(aes(x=Cohort, 
                                     y=Childless, 
                                     group=Gender,
                                     linetype=Gender)) +
  geom_line(linewidth=1.1)+
  geom_point()+
  scale_linetype_manual(values=c("solid","dashed"))+
  labs(y="Proportion chidless")+
  facet_wrap(~Country)+
  theme_bw(base_size=12) +
  theme(axis.text=element_text(colour="black"), 
        axis.title = element_text(face="bold"), 
        legend.key.width = unit(1,"cm"), 
        strip.text.x = element_text(face="bold"))

ggsave(childless,
       height = plotheight,
       width = plotwidth,
       file="Results/fig_childless.png")
