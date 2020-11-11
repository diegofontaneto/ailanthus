#############################
##### Ailanthus effects #####
#############################

# read data
ailanthus <- read.csv('ailanthus data.csv', header=T, as.is=F)

# check that the data is correctly read as factors and as vectors
str(ailanthus)
names(ailanthus)
summary(ailanthus)


########################
##### explore data #####
########################

# plot the coordinates to identify the nine sites (grouped by colour and by shape)
plot(latitude~longitude, data=ailanthus, col=c(1:6,8,10,11)[site], cex=2, pch=c(1,1,2,3)[time])

# plot the pairwise correlation charts between predictors, divided by group of variables
library(PerformanceAnalytics)

biological_forms <- ailanthus[,c(15:19)]
chart.Correlation(biological_forms, histogram=TRUE, pch=30)

chorotypes <- ailanthus[,c(20:29)]
chart.Correlation(chorotypes, histogram=TRUE, pch=30)

Ellenberg_indices <- ailanthus[,c(30:35)]
chart.Correlation(Ellenberg_indices, histogram=TRUE, pch=30)


######################
##### question 1 #####
######################

##### testing the effect of Ailanthus

# subset the data using only the dataset with 17+9 plots
ailanthus1 <- droplevels(subset(ailanthus, habitat!='other'))
str(ailanthus1)
summary(ailanthus1)

##### explore the data

# plot the coordinates to identify the nine sites (grouped by colour and by shape)
plot(latitude~longitude, data=ailanthus1, col=c(1:6,8,10,11)[site], cex=2)

# plot the pairwise correlation charts between predictors, divided by group of variables
library(PerformanceAnalytics)
biological_forms1 <- ailanthus1[,c(15:19)]
chart.Correlation(biological_forms1, histogram=TRUE, pch=30)
chorotypes1 <- ailanthus1[,c(20:29)]
chart.Correlation(chorotypes1, histogram=TRUE, pch=30)
Ellenberg_indices1 <- ailanthus1[,c(30:35)]
chart.Correlation(Ellenberg_indices1, histogram=TRUE, pch=30)


##### A. models for differences with/without ailanthus

##### richness

# for richness use a quasipoisson model for overdispersed count data
library(MASS)
# model_richness <- glm(richness ~ habitat * site, data=ailanthus1, family=quasipoisson) - NOT USED, interaction not significant
model_richness <- glm(richness ~ habitat + site, data=ailanthus1, family=quasipoisson)

# obtain the output
library(car)
Anova(model_richness)

# check that the model makes sense
library(performance)
check_model(model_richness)

# plot the differences between factors
par(mfrow=c(2,1))
plot(richness ~ habitat, data=ailanthus1)
plot(richness ~ site, data=ailanthus1)
dev.off()

# check the effect of Ailanthus height, when present

ailanthus1_h <- droplevels(subset(ailanthus1, habitat=='Ailanthus stand'))
# model_height <- glm(richness ~ ailanthus_height * site, data=ailanthus1_h, family=quasipoisson) - NOT USED, interaction not significant
model_height <- glm(richness ~ ailanthus_height + site, data=ailanthus1_h, family=quasipoisson)
Anova(model_height)

library(ggplot2)
main <- ggplot(ailanthus1,aes(y=richness,x=ailanthus_height, shape=habitat)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_point(size=6) + geom_smooth(method="lm", colour="black") + xlab("Ailanthus height (m)")
ailanthus1$habitat2 <- with(ailanthus1, relevel(habitat,"grassland"))
inset <- ggplot(ailanthus1,aes(y=richness,x=habitat2)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("") + ylab("richness")
library(cowplot)
ggdraw() + draw_plot(main) + draw_plot(inset, x=0.65, y=0.6, width=0.3, height=0.3)

# use of Ailanthus height as a proxy for cover
ailanthus_abundance <- ailanthus1[,c(14,39,40)]
chart.Correlation(ailanthus_abundance, histogram=TRUE, pch=30)
# height, val, and cover are highly correlated and thus we can use of them as a proxy of the others


##### shannon

# the same for shannon, but with a linear model, not count data
# model_shannon <- lm(shannon ~ habitat * site, data=ailanthus1) - NOT USED, interaction not significant
model_shannon <- lm(shannon ~ habitat + site, data=ailanthus1)
Anova(model_shannon)
check_model(model_shannon)

par(mfrow=c(2,1))
plot(shannon ~ habitat, data=ailanthus1)
plot(shannon ~ site, data=ailanthus1)
dev.off()

# model_height <- lm(shannon ~ ailanthus_height * site, data=ailanthus1_h) - NOT USED, interaction not significant
model_height <- lm(shannon ~ ailanthus_height + site, data=ailanthus1_h)
Anova(model_height)

##### biological forms

# manova_biological <- manova(cbind(Ch_chamaephytes,G_geophytes,H_hemicryptophytes,T_therophytes) ~ habitat * site, data=ailanthus1)
manova_biological <- manova(cbind(Ch_chamaephytes,G_geophytes,H_hemicryptophytes,T_therophytes) ~ habitat + site, data=ailanthus1)
summary(manova_biological)
summary.aov(manova_biological)

# manova_biological_height <- manova(cbind(Ch_chamaephytes,G_geophytes,H_hemicryptophytes,T_therophytes) ~ ailanthus_height * site, data=ailanthus1_h)
manova_biological_height <- manova(cbind(Ch_chamaephytes,G_geophytes,H_hemicryptophytes,T_therophytes) ~ ailanthus_height + site, data=ailanthus1_h)
summary(manova_biological_height)
summary.aov(manova_biological_height)

model_ch <- lm(Ch_chamaephytes ~ ailanthus_height + site, data=ailanthus1_h)
Anova(model_ch)
model_chL <- lm(Ch_chamaephytes ~ log(ailanthus_height) + site, data=ailanthus1_h)
Anova(model_chL)
AIC(model_ch,model_chL)

main_ch <- ggplot(ailanthus1,aes(y=Ch_chamaephytes,x=ailanthus_height, shape=habitat)) + theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.position="top") + geom_point(size=6) + geom_smooth(method="lm", formula=y~log(x), colour="black") + xlab("Ailanthus height (m)") + ylab("percentage of Chamaephytes")
ailanthus1$habitat2 <- with(ailanthus1, relevel(habitat,"grassland"))
inset_ch <- ggplot(ailanthus1,aes(y=Ch_chamaephytes,x=habitat2)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("") + ylab("percentage")
ggdraw() + draw_plot(main_ch) + draw_plot(inset_ch, x=0.65, y=0.6, width=0.3, height=0.3)

main_th <- ggplot(ailanthus1,aes(y=T_therophytes,x=ailanthus_height, shape=habitat)) + theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.position="top") + geom_point(size=6) + xlab("Ailanthus height (m)") + ylab("percentage of Therophytes")
ailanthus1$habitat2 <- with(ailanthus1, relevel(habitat,"grassland"))
inset_th <- ggplot(ailanthus1,aes(y=T_therophytes,x=habitat2)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("") + ylab("percentage")
ggdraw() + draw_plot(main_th) + draw_plot(inset_th, x=0.65, y=0.6, width=0.3, height=0.3)



##### chorotypes

# manova_chorotypes <- manova(cbind(BOREAL,ENDEMIC,EUROPEAN,MEDITERRANEAN.ATLANTIC,MEDITERRANEAN.TURANIC,PALAEOTEMPERATE) ~ habitat * site, data=ailanthus1)
manova_chorotypes <- manova(cbind(BOREAL,ENDEMIC,EUROPEAN,MEDITERRANEAN.ATLANTIC,MEDITERRANEAN.TURANIC,PALAEOTEMPERATE) ~ habitat + site, data=ailanthus1)
summary(manova_chorotypes)
summary.aov(manova_chorotypes)

manova_chorotypes_height <- manova(cbind(BOREAL,ENDEMIC,EUROPEAN,MEDITERRANEAN.ATLANTIC,MEDITERRANEAN.TURANIC,PALAEOTEMPERATE) ~ ailanthus_height + site, data=ailanthus1_h)
summary(manova_chorotypes_height)
summary.aov(manova_chorotypes_height)

model_pa <- lm(PALAEOTEMPERATE ~ ailanthus_height + site, data=ailanthus1_h)
Anova(model_pa)

model_en <- lm(ENDEMIC ~ ailanthus_height + site, data=ailanthus1_h)
Anova(model_en)
model_enL <- lm(ENDEMIC ~ log(ailanthus_height) + site, data=ailanthus1_h)
Anova(model_enL)
AIC(model_en,model_enL)

main_pa <- ggplot(ailanthus1,aes(y=PALAEOTEMPERATE,x=ailanthus_height, shape=habitat)) + theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.position="top") + geom_point(size=6) + xlab("Ailanthus height (m)") + ylab("percentage of palaeotemperate species")
ailanthus1$habitat2 <- with(ailanthus1, relevel(habitat,"grassland"))
inset_pa <- ggplot(ailanthus1,aes(y=PALAEOTEMPERATE,x=habitat2)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("") + ylab("percentage")
ggdraw() + draw_plot(main_pa) + draw_plot(inset_pa, x=0.7, y=0.07, width=0.28, height=0.28)

main_en <- ggplot(ailanthus1,aes(y=ENDEMIC,x=ailanthus_height, shape=habitat)) + theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.position="top") + geom_point(size=6) + geom_smooth(method="lm", formula=y~log(x), colour="black") + xlab("Ailanthus height (m)") + ylab("percentage of endemic species")
ailanthus1$habitat2 <- with(ailanthus1, relevel(habitat,"grassland"))
inset_en <- ggplot(ailanthus1,aes(y=ENDEMIC,x=habitat2)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("") + ylab("percentage")
ggdraw() + draw_plot(main_en) + draw_plot(inset_en, x = 0.65, y = .6, width = .3, height = .3)


##### Ellenberg

# Ellenberg <- lm(humidity ~ habitat * site, data=ailanthus1)
Ellenberg <- lm(humidity ~ habitat + site, data=ailanthus1)
Anova(Ellenberg)

# Ellenberg_height <- lm(humidity ~ ailanthus_height * site, data=ailanthus1_h)
Ellenberg_height <- lm(humidity ~ ailanthus_height + site, data=ailanthus1_h)
Anova(Ellenberg_height)

main_hu <- ggplot(ailanthus1,aes(y=humidity,x=ailanthus_height, shape=habitat)) + theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.position="top") + geom_point(size=6) + xlab("Ailanthus height (m)") + ylab("Ellenberg index of humidity")
ailanthus1$habitat2 <- with(ailanthus1, relevel(habitat,"grassland"))
inset_hu <- ggplot(ailanthus1,aes(y=humidity,x=habitat2)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("") + ylab("index")
ggdraw() + draw_plot(main_hu) + draw_plot(inset_hu, x=0.7, y=0.6, width=0.28, height=0.28)


##### community composition

# load the library
library(mvabund)

# step 1: . ~ habitat + site

# read the environmental variables and make them usable
ail_variables <- read.csv('ailanthus data.csv', header=T, as.is=F)
habitat1 <- droplevels(ail_variables$habitat[ail_variables$habitat!="other"])
site1 <- droplevels(ail_variables$site[ail_variables$habitat!="other"])

# read the community dataset and make it usable
ail_species <- read.csv('ailanthus community.csv', header=T, as.is=F)
ail_species1 <- ail_species[ail_variables$habitat!="other",-1]
ail_mva1 <- mvabund(ail_species1)

# fit a model assuming abundances are negative.binomal:
ail_glm1 <- manyglm(ail_mva1 ~ habitat1 * site1, family="negative.binomial")

# check model
plot.manyglm(ail_glm1)

# summary of the model
# summary(ail_glm1, resamp="residual", nBoot=10) - TO CHECK before running it with 1000 iterations
summary(ail_glm1, resamp="residual")

# anova.manyglm(ail_glm1, nBoot = 10)
anova.manyglm(ail_glm1)

# step 2: . ~ ailanthus_height + site

ailanthus_height2 <- ail_variables$ailanthus_height[ail_variables$habitat=="Ailanthus stand"]
site2 <- ail_variables$site[ail_variables$habitat=="Ailanthus stand"]
ail_species2 <- ail_species[ail_variables$habitat=="Ailanthus stand",-1]
ail_mva2 <- mvabund(ail_species2)

# fit a model assuming abundances are negative.binomal:
ail_glm2 <- manyglm(ail_mva2 ~ ailanthus_height2 * site2, family="negative.binomial")

# check model
plot.manyglm(ail_glm2)

# summary of the model
# summary(ail_glm2, resamp="residual", nBoot=10)
summary(ail_glm2, resamp="residual")

# anova.manyglm(ail_glm2, nBoot = 10)
anova.manyglm(ail_glm2)





######################
##### question 2 #####
######################

##### models for differences through time

# subset the data to keep only the six experimental plots
ailanthus_q2 <- droplevels(subset(ailanthus, time!='0'))
summary(ailanthus_q2)


##### richness

# for richness use a quasipoisson model for overdispersed count data
library(MASS)
# model_richness <- glm(richness ~ time * site, data=ailanthus_q2, family=quasipoisson)
model_richness <- glm(richness ~ time + site, data=ailanthus_q2, family=quasipoisson)

# obtain the output
library(car)
Anova(model_richness)

# a Tukey posthoc output on the effect of time
library(emmeans)
pairs(emmeans(model_richness, ~ time))

# check that the model makes sense
library(performance)
check_model(model_richness)

# recovery

ailanthus_R <- ailanthus[complete.cases(ailanthus), ]
# model_richness_recovery <- glm(richness ~ final_groups * site, data=ailanthus_R, family=quasipoisson)
model_richness_recovery <- glm(richness ~ final_groups + site, data=ailanthus_R, family=quasipoisson)
Anova(model_richness_recovery)
pairs(emmeans(model_richness_recovery, ~ final_groups))

# plot

library(ggplot2)
main_rr <- ggplot(ailanthus_q2,aes(y=richness,x=time)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + geom_jitter(aes(size=site, fill=site), shape=21, position=position_jitter(0.1))
inset_rr <- ggplot(ailanthus_R,aes(y=richness,x=final_groups)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("")
library(cowplot)
ggdraw() + draw_plot(main_rr) + draw_plot(inset_rr, x=0.65, y=0.08, width=0.3, height=0.3)


##### shannon

# the same for shannon, but with a linear model
# model_shannon <- lm(shannon ~ time * site, data=ailanthus_q2)
model_shannon <- lm(shannon ~ time + site, data=ailanthus_q2)
Anova(model_shannon)
check_model(model_shannon)
# pairs(emmeans(model_shannon, ~ time))

# plot the differences between factors
# ggplot(ailanthus_q2,aes(y=shannon,x=time)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + geom_jitter(aes(size=site, fill=site), shape=21, position=position_jitter(0.1))


##### biological forms

# manova_biological <- manova(cbind(Ch_chamaephytes,G_geophytes,H_hemicryptophytes,T_therophytes) ~ time * site, data=ailanthus_q2)
manova_biological <- manova(cbind(Ch_chamaephytes,G_geophytes,H_hemicryptophytes,T_therophytes) ~ time + site, data=ailanthus_q2)
summary(manova_biological)
summary.aov(manova_biological)


##### chorotypes

# manova_chorotypes <- manova(cbind(BOREAL,ENDEMIC,EUROPEAN,MEDITERRANEAN.ATLANTIC,MEDITERRANEAN.TURANIC,PALAEOTEMPERATE) ~ time * site, data=ailanthus_q2)
manova_chorotypes <- manova(cbind(BOREAL,ENDEMIC,EUROPEAN,MEDITERRANEAN.ATLANTIC,MEDITERRANEAN.TURANIC,PALAEOTEMPERATE) ~ time + site, data=ailanthus_q2)
summary(manova_chorotypes)
summary.aov(manova_chorotypes)


##### Ellenberg

# Ellenberg <- lm(humidity ~ time * site, data=ailanthus_q2)
Ellenberg <- lm(humidity ~ time + site, data=ailanthus_q2)
Anova(Ellenberg)
pairs(emmeans(Ellenberg, ~ time))

# recovery

# Ellenberg_recovery <- lm(humidity ~ final_groups * site, data=ailanthus_R)
Ellenberg_recovery <- lm(humidity ~ final_groups + site, data=ailanthus_R)
Anova(Ellenberg_recovery)
pairs(emmeans(Ellenberg_recovery, ~ final_groups))

# plot

library(ggplot2)
main_rh <- ggplot(ailanthus_q2,aes(y= humidity,x=time)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + geom_jitter(aes(size=site, fill=site), shape=21, position=position_jitter(0.1))
inset_rh <- ggplot(ailanthus_R,aes(y= humidity,x=final_groups)) + theme(panel.background=element_rect(fill="white", colour="grey50"), legend.position="top") + geom_boxplot(fill="grey") + xlab("")
ggdraw() + draw_plot(main_rh) + draw_plot(inset_rh, x=0.65, y=0.08, width=0.3, height=0.3)


##### community composition

time3 <- droplevels(ail_variables$time[ail_variables$time!="0"])
site3 <- droplevels(ail_variables$site[ail_variables$time!="0"])
ail_species3 <- ail_species[ail_variables$time!="0",-1]
ail_mva3 <- mvabund(ail_species3)

ail_glm3 <- manyglm(ail_mva3 ~ time3 * site3, family="negative.binomial")

plot.manyglm(ail_glm3)

# summary(ail_glm3, resamp="residual", nBoot=10)
summary(ail_glm3, resamp="residual")

# anova.manyglm(ail_glm3, nBoot = 10)
anova.manyglm(ail_glm3)


# recovery

time4 <- droplevels(ail_variables$time[!is.na(ail_variables$final_groups)])
site4 <- droplevels(ail_variables$site[!is.na(ail_variables$final_groups)])
ail_species4 <- ail_species[!is.na(ail_variables$final_groups),-1]
ail_mva4 <- mvabund(ail_species4)

ail_glm4 <- manyglm(ail_mva4 ~ time4 * site4, family="negative.binomial")

plot.manyglm(ail_glm4)

# summary(ail_glm4, resamp="residual", nBoot=10)
summary(ail_glm4, resamp="residual")

# anova.manyglm(ail_glm4, nBoot = 10)
anova.manyglm(ail_glm4)

ail_glm4_recovery <- manyglm(ail_mva4 ~ time4, family="negative.binomial")
# anova.manyglm(ail_glm4_recovery, pairwise.comp = ~ time4, nBoot=10)
anova.manyglm(ail_glm4_recovery, pairwise.comp = ~ time4)
