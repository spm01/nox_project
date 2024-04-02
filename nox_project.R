#loading packages
library(pacman)
p_load(dplyr, tidyverse, haven, ggplot2, magrittr, broom, plm, did, lmtest, miceadds)

#loading data 
nbp_data = read_dta('nbp.dta')
nbp_avg = nbp_data %>%
  group_by(year, summer, nbp) %>%
  summarise(nox_emit = sum(nox_emit, na.rm = T))

#pre-treatment
nbp_pre_data = nbp_data %>%
  filter(post == 0)

#nbp avg treatment = 1
nbp_avg1 = nbp_avg %>%
  filter(nbp == 1)
#nbp avg treatment = 0 
nbp_avg2 = nbp_avg %>%
  filter(nbp == 0)
#nbp raw treatment = 1
nbp_data1 = nbp_data %>%
  filter(nbp == 1)
#nbp raw treatment = 0
nbp_data2 = nbp_data %>%
  filter(nbp == 0)
  
#making graphs to denote NOx in treated states
#Calculate mean NOx emissions for summer and non-summer months
mean_nox = aggregate(nox_emit ~ year + summer, data = nbp_data, FUN = mean)

#Plot mean NOx emissions over time
#both nbp = 1 & nbp = 0 states
plot1 = ggplot(nbp_avg, aes(x = year, y = nox_emit/1000, color = factor(summer))) +
  geom_line() +
  facet_wrap(~ nbp) +
  labs(x = "Year", y = "Mean NOx Emissions") +
  scale_x_continuous(breaks = seq(1997, 2008, by = 1)) + 
  scale_color_manual(values = c("blue", "red"), labels = c("Non-Summer", "Summer")) +
  theme_classic()
plot1
#nbp = 1
plot2 = ggplot(nbp_avg1, aes(x = year, y = nox_emit/1000, color = factor(summer))) +
  geom_line() +
  labs(x = "Year", y = "Mean NOx Emissions") +
  scale_x_continuous(breaks = seq(1997, 2008, by = 1)) + 
  scale_color_manual(values = c("blue", "red"), labels = c("Non-Summer", "Summer")) +
  theme_classic()
plot2
#nbp = 0
plot3 = ggplot(nbp_avg2, aes(x = year, y = nox_emit/1000, color = factor(summer))) +
  geom_line() +
  labs(x = "Year", y = "Mean NOx Emissions") +
  scale_x_continuous(breaks = seq(1997, 2008, by = 1)) + 
  scale_color_manual(values = c("blue", "red"), labels = c("Non-Summer", "Summer")) +
  theme_classic()
plot3

#nbp states first

#calculate statistical test of parallel trends
#Define the DiD model formula
nbp_nox_did_regr1 = lm(nox_emit ~ summer + year + year:summer, 
                       cluster = "state_season_cluster",
                       data = nbp_pre_data)
summary(nbp_nox_did_regr1)

#calculate difference-in-difference regression for panel A
#non-clustered regression 
nbp_nox_did_regr2 = lm(nox_emit ~ summer + post + summer*post, 
                   data = nbp_data1)
summary(nbp_nox_did_regr2)

#creating item to cluster 
#questionable rationale to create this cluster --> more reasonable cluster would be at pure state level
nbp_data1$state_season_cluster = paste(nbp_data1$fips_state, nbp_data1$summer, sep = "_")

#clustered regression
nbp_nox_did_regr_cluster = lm.cluster(nox_emit ~ summer + post + summer*post,
                                   cluster = 'state_season_cluster',
                                   data = nbp_data1)
nox_cluster_df = data.frame(summary(nbp_nox_did_regr_cluster))


#non-nbp states DID model
#create item to cluster
nbp_data2$state_season_cluster = paste(nbp_data2$fips_state, nbp_data2$summer, sep = "_")

#clustered regression
non_nbp_nox_did_regr_cluster = lm.cluster(nox_emit ~ summer + post + summer*post,
                                      cluster = 'state_season_cluster',
                                      data = nbp_data2)
non_nox_cluster_df = data.frame(summary(non_nbp_nox_did_regr_cluster))

#triple diff regression
#create item to cluster
nbp_data$state_season_cluster = paste(nbp_data$fips_state, nbp_data$summer, sep = "_")

#triple-diff regression
full_nbp_nox_did_regr_cluster = lm.cluster(nox_emit ~ summer + post + nbp + summer*post + summer*nbp + post*nbp + summer*post*nbp,
                                           cluster = 'state_season_cluster',
                                           data = nbp_data)
full_nox_cluster_df = data.frame(summary(full_nbp_nox_did_regr_cluster))

#estimate total NOx reduction over time = TE from triple-diff * time duration (2003 - 2007)


#re-estimate model as 2-way fixed effects model
out = lm(data = nbp_data1,
            nox_emit ~ summer*post + factor(fips_state) + factor(year),
                               cluster = ~fips_state)

summary(out)

nbp_data1$residuals = nbp_data1$nox_emit - predict(out, newdata = nbp_data1)
nbp_data1$prediction = predict(out, newdata = nbp_data1)

#new graph demonstrating fixed-effects model based on state, year, and treatment
plot4 = ggplot(data = nbp_data1,
       aes(x = year,
           y = prediction,
           color = factor(fips_state))) +
geom_line () +
facet_wrap(~ summer) +
theme(legend.position = "none")
plot4


