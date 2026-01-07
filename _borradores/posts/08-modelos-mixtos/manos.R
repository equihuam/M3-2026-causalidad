# manos

manos.lme <- lme(fixed= Length ~ Hand - 1, 
                 random=list(Individual = ~ 1),
                 data=manos)
manos.lme
anova(manos.lme)
