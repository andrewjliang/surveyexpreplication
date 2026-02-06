#################
##### Replication of Berlinski et al. (2023)
##### Andrew J. Liang
##### February 5th, 2026
##### Replication performed on 2022 MacBook Air, M2, 16 G.B. RAM, 512 G.B. SSD
##### macOS Sequoia 15.6
#################


# Loading packages
suppressPackageStartupMessages({
library(tidyverse); library(haven); library(DeclareDesign); library(stargazer)
library(fixest); library(here); library(modelsummary); library(marginaleffects)
library(patchwork); library(biostat3); library(cobalt); library(psych);
library(lavaan); library(texreg); library(marginaleffects)
})
  
# setwd() if needed
here::i_am("code/Replication.R")

# Loading initial dataset and recoding variables
main <- read_dta(here("data/Berlinskietal2023", "survey-data.dta"))
pulse <- read_dta(here("data/Berlinskietal2023", "pulse-data.dta"))

df_combined <- main %>%
  left_join(pulse, by = "caseid") %>%
  mutate(nopulse_pre = if_else(is.na(totalnewsbinary_presurvey), 1, 0),
         female = case_when(gender == 1 ~ 0, # female
                            gender == 2 ~ 1,
                            .default = NA), 
         nonwhite = if_else(race == 1, 0, 1, missing = NA), # race
         college = if_else(educ %in% c(5, 6), 1, 0, missing = NA), # education
         age = 2018 - birthyr, # age
         agecat = case_when(age > 17 & age <= 24 ~ 1, # 18-24
                            age > 24 & age <= 44 ~ 2, # 25-44
                            age > 44 & age <= 64 ~ 3, # 45-64
                            age > 64 ~ 4, # 65+
                            .default = NA),
         agecat1 = if_else(agecat == 1, 1, 0), # age category
         agecat2 = if_else(agecat == 2, 1, 0),
         agecat3 = if_else(agecat == 3, 1, 0), 
         agecat4 = if_else(agecat == 4, 1, 0),
         ideology = ideo, # ideology
         dem = if_else(pid3 == 1, 1, 0, missing = NA), # party
         repub = if_else(pid3 == 2, 1, 0, missing = NA),
         ind3pt = if_else(pid3 %in% c(3, 4, 5), 1, 0, missing = NA),
         dem_leaners = if_else(pid7 %in% c(1, 2, 3), 1, 0, missing = NA),
         repub_leaners = if_else(pid7 %in% c(5, 6, 7), 1, 0, missing = NA),
         independents = if_else(pid7 %in% c(4, 8), 1, 0, missing = NA),
         pid3_lean = case_when(dem_leaners == 1 ~ 1, # lean
                               independents == 1 ~ 2,
                               repub_leaners == 1 ~ 3),
         old_trump_approve = trump_approve,
         trump_approve = case_when(old_trump_approve == 1 ~ 4,
                                   old_trump_approve == 2 ~ 3,
                                   old_trump_approve == 3 ~ 2,
                                   old_trump_approve == 4 ~ 1,
                                   .default = NA),
         trump_app_yn = case_when(old_trump_approve %in% c(1, 2) ~ 1,
                                  old_trump_approve %in% c(3, 4) ~ 0,
                                    .default = NA),
         polint = case_when(pol_interest == 1 ~ 5,
                            pol_interest == 2 ~ 4,
                            pol_interest == 3 ~ 3,
                            pol_interest == 4 ~ 2,
                            pol_interest == 5 ~ 1,
                            .default = NA),
         FT_trump = pol_therm_trump,
         FT_rep = pol_therm_rep,
         FT_dem = pol_therm_dem,
         FT_media = pol_therm_media,
         dem_less_repw1 = FT_dem - FT_rep,
         rep_less_demw1 = FT_rep - FT_dem,
         dem_less_rep_w1 = if_else(repub == 1, 0, dem_less_repw1),
         rep_less_dem_w1 = if_else(dem == 1, 0, rep_less_demw1),
         affect_mergedw1 = dem_less_rep_w1 + rep_less_dem_w1,
         dem_less_rep_w1x = if_else(repub_leaners == 1, 0, dem_less_repw1),
         rep_less_dem_w1x = if_else(dem_leaners == 1, 0, rep_less_demw1),
         affect_merged_leanersw1 = dem_less_rep_w1x + rep_less_dem_w1x,
         polknow = 0,
         polknow = if_else(senator_term == 3, polknow + 1, polknow, missing = polknow),
         polknow = if_else(pres_term_limit == 2, polknow + 1, polknow, missing = polknow),
         polknow = if_else(senator_num == 2, polknow + 1, polknow, missing = polknow),
         polknow = if_else(uk_pm == 4, polknow + 1, polknow, missing = polknow),
         polknow = if_else(rep_term == 1, polknow + 1, polknow, missing = polknow),
         massmedia_trust = case_when(media_trust == 1 ~ 4,
                                     media_trust == 2 ~ 3,
                                     media_trust == 3 ~ 2,
                                     media_trust == 4 ~ 1),
         fbtrust = case_when(fb_trust == 1 ~ 4,
                              fb_trust == 2 ~ 3,
                              fb_trust == 3 ~ 2,
                              fb_trust == 4 ~ 1),
         fb_use = case_when(fb_freq == 1 ~ 9,
                            fb_freq == 2 ~ 8,
                            fb_freq == 3 ~ 7,
                            fb_freq == 4 ~ 6,
                            fb_freq == 5 ~ 5,
                            fb_freq == 6 ~ 4,
                            fb_freq == 7 ~ 3,
                            fb_freq == 8 ~ 2,
                            fb_freq == 9 ~ 1), 
         fb_pol_use = case_when(fb_political_freq == 1 ~ 9,
                                fb_political_freq == 2 ~ 8,
                                fb_political_freq == 3 ~ 7,
                                fb_political_freq == 4 ~ 6,
                                fb_political_freq == 5 ~ 5,
                                fb_political_freq == 6 ~ 4,
                                fb_political_freq == 7 ~ 3,
                                fb_political_freq == 8 ~ 2,
                                fb_political_freq == 9 ~ 1),
         fb_pol_share = case_when(fb_share_freq == 1 ~ 9,
                                  fb_share_freq == 2 ~ 8,
                                  fb_share_freq == 3 ~ 7,
                                  fb_share_freq == 4 ~ 6,
                                  fb_share_freq == 5 ~ 5,
                                  fb_share_freq == 6 ~ 4,
                                  fb_share_freq == 7 ~ 3,
                                  fb_share_freq == 8 ~ 2,
                                  fb_share_freq == 9 ~ 1),
         consp1 = case_when(conspiracy_1 == 1 ~ 5,
                            conspiracy_1 == 2 ~ 4,
                            conspiracy_1 == 3 ~ 3,
                            conspiracy_1 == 4 ~ 2,
                            conspiracy_1 == 5 ~ 1,
                            .default = NA),
         consp2 = case_when(conspiracy_2 == 1 ~ 5,
                            conspiracy_2 == 2 ~ 4,
                            conspiracy_2 == 3 ~ 3,
                            conspiracy_2 == 4 ~ 2,
                            conspiracy_2 == 5 ~ 1,
                            .default = NA),
         consp3 = case_when(conspiracy_3 == 1 ~ 5,
                            conspiracy_3 == 2 ~ 4,
                            conspiracy_3 == 3 ~ 3,
                            conspiracy_3 == 4 ~ 2,
                            conspiracy_3 == 5 ~ 1,
                            .default = NA),
         tweet_treat = tweet_treat_w2,
         tweet4 = if_else(tweet_treat == 1, 1, 0, missing = NA),
         tweet8 = if_else(tweet_treat == 2, 1, 0, missing = NA),
         tweetcorrect = if_else(tweet_treat == 3, 1, 0, missing = NA),
         tweetcontrol = if_else(tweet_treat == 4, 1, 0, missing = NA),
         tweet_news_2018 = case_when(tweet_news_w2 == 3 ~ 1, 
                                     tweet_news_w2 %in% c(1, 2, 4) ~ 0,
                                     .default = NA),
         tweet_news_control = case_when(tweet_accuracy_control_2_w2 == 1 ~ 1,
                                        tweet_accuracy_control_2_w2 %in% c(2, 3, 4) ~ 0,
                                        .default = NA),
         massmedia_trustw2 = case_when(media_trust_w2 == 1 ~ 4,
                                       media_trust_w2 == 2 ~ 3,
                                       media_trust_w2 == 3 ~ 2,
                                       media_trust_w2 == 4 ~ 1),
         fbtrustw2 = case_when(fb_trust_w2 == 1 ~ 4,
                               fb_trust_w2 == 2 ~ 3,
                               fb_trust_w2 == 3 ~ 2,
                               fb_trust_w2 == 4 ~ 1),
         FT_muslim = group_affect_muslim_w2,
         FT_christian = group_affect_christian_w2,
         FT_white = group_affect_white_w2,
         FT_black = group_affect_black_w2,
         FT_labor = group_affect_labor_w2,
         FT_rich = group_affect_rich_w2,
         FT_latino = group_affect_latino_w2,
         FT_white_latino = FT_white-FT_latino,
         FT_christian_muslim = FT_christian-FT_muslim,
         conf1 = case_when(vote_entitled_w2 == 1 ~ 4,
                           vote_entitled_w2 == 2 ~ 3,
                           vote_entitled_w2 == 3 ~ 2,
                           vote_entitled_w2 == 4 ~ 1,
                           .default = NA),
         conf2 = case_when(plan_vote_certain_w2 == 1 ~ 4,
                           plan_vote_certain_w2 == 2 ~ 3,
                           plan_vote_certain_w2 == 3 ~ 2,
                           plan_vote_certain_w2 == 4 ~ 1,
                           .default = NA),
         conf3 = case_when(officials_count_w2 == 1 ~ 4,
                           officials_count_w2 == 2 ~ 3,
                           officials_count_w2 == 3 ~ 2,
                           officials_count_w2 == 4 ~ 1,
                           .default = NA),
         conf4 = case_when(system_works_w2 == 1 ~ 4,
                           system_works_w2 == 2 ~ 3,
                           system_works_w2 == 3 ~ 2,
                           system_works_w2 == 4 ~ 1,
                           .default = NA),
         conf4 = case_when(system_works_w2 == 1 ~ 4,
                           system_works_w2 == 2 ~ 3,
                           system_works_w2 == 3 ~ 2,
                           system_works_w2 == 4 ~ 1,
                           .default = NA),
         trustelect1 = case_when(trust_elections_w2 == 1 ~ 1,
                               trust_elections_w2 == 2 ~ 2,
                               trust_elections_w2 == 3 ~ 3,
                               trust_elections_w2 == 4 ~ 4,
                               trust_elections_w2 == 5 ~ 5,
                               trust_elections_w2 == 6 ~ 6,
                               trust_elections_w2 == 7 ~ 7,
                               .default = NA),
         trustelect2 = case_when(secure_ballot_w2 == 1 ~ 5,
                                 secure_ballot_w2 == 2 ~ 4,
                                 secure_ballot_w2 == 3 ~ 3,
                                 secure_ballot_w2 == 4 ~ 2,
                                 secure_ballot_w2 == 5 ~ 1,
                                 .default = NA),
         trustelect3 = case_when(machine_accurate_w2 == 1 ~ 5,
                                 machine_accurate_w2 == 2 ~ 4,
                                 machine_accurate_w2 == 3 ~ 3,
                                 machine_accurate_w2 == 4 ~ 2,
                                 machine_accurate_w2 == 5 ~ 1,
                                 .default = NA),
         democ_imp = importance_democracy_w2,
         polsys1 =  polsystem_w2_1,
         polsys2 =  polsystem_w2_2,
         polsys3 =  polsystem_w2_3,
         polsys4 = case_when(polsystem_w2_4 == 1 ~ 4,
                             polsystem_w2_4 == 2 ~ 3,
                             polsystem_w2_4 == 3 ~ 2,
                             polsystem_w2_4 == 4 ~ 1),
         missing = as.integer(if_all(all_of(c("conf1","conf2","conf3","conf4",
           "trustelect1","trustelect2","trustelect3")), is.na))
         
  )

## manual creation of some variables
df_combined$conspiracy_mean <- rowMeans(dplyr::select(df_combined, consp1, 
                                                      consp2, consp3), 
                                        na.rm = TRUE)
df_combined$conspiracy_mean <- if_else(is.nan(df_combined$conspiracy_mean), NA, 
                                       df_combined$conspiracy_mean)   


# alpha 
alpha(df_combined %>% dplyr::select(consp1, consp2, consp3))
alpha(df_combined %>% dplyr::select(conf1, conf2, conf3, conf4))
alpha(df_combined %>% dplyr::select(trustelect1, trustelect2, trustelect3))

# Factor analysis - Table B1
factors <- df_combined %>% dplyr::select(conf1, conf2, conf3, conf4, 
                                         trustelect1, trustelect2, trustelect3,
                                         democ_imp, polsys1, polsys2, polsys3, 
                                         polsys4)

principal(factors, nfactors = 3, rotate = "varimax")

# Structural Equation Model - Table B2
sem_fit <- sem("Conf_trust =~ conf1 + conf2 + conf3 + conf4 +
                trustelect1 + trustelect2 + trustelect3", 
    data = df_combined, missing = "fiml")

fs <- lavPredict(sem_fit, type = "lv")

df_combined$zconf_trust <- scale(fs)


# Table 1 - Summary Statistics of Measures of Confidence
summary <- df_combined |>
  dplyr::select(starts_with(c("conf", "trustelect", "zconf"))) |>
  dplyr::select(!starts_with("confi"))

sum_stats <- data.frame(
  Question = c("Confidence all entitled allowed to vote",
               "Confidence own vote was counted",
               "Confidence officials manage counting votes",
               "System works despite problems casting and counting votes",
               "Trust elections",
               "Ballots secure from tampering",
               "Voting machines accurate", 
               "Composite Measure"),
  Mean = sapply(summary, FUN = mean, na.rm = T),
  SD = sapply(summary, FUN = sd, na.rm = T),
  Range = sapply(summary, FUN = function(x) 
    paste(range(x, na.rm = T), collapse = ", "))
  
) |> as_tibble()

sum_stats

# Treatment Effects

## Table 2, Column 1
summary(lm_robust(conf1 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf1 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
              "tweetcorrect-tweet4"))

## Table 2, Column 2
summary(lm_robust(conf2 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf2 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Table 2, Column 3
summary(lm_robust(conf3 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf3 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Table 2, Column 4 
summary(lm_robust(conf4 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf4 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Table 2, Column 5
summary(lm_robust(trustelect1 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(trustelect1 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Table 2, Column 6
summary(lm_robust(trustelect2 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(trustelect2 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Table 2, Column 7
summary(lm_robust(trustelect3 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(trustelect3 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Table 2, Column 8 - Main Analysis
summary(lm_robust(zconf_trust ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                    se_type = "HC2"))
lincom(lm_robust(zconf_trust ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))


# Figure 2
plotreg(list(lm_robust(zconf_trust ~ tweet4 + tweet8 + tweetcorrect, 
                       data = df_combined,
                       se_type = "HC2")),
        custom.model.names = "Composite Measure",
        custom.coef.map = c("tweet4" = "Low dose (H1a)",
                               "tweet8" = "High dose (H2a)",
                               "tweetcorrect" = "Low dose + fact-check tweets (RQ1a)"),
        theme = theme_classic()) 


# Figure 3 - unable to plot but results should be similar
fig3a <- lm_robust(zconf_trust ~ (tweet4 + tweet8 + tweetcorrect)*pid3_lean, 
                   df_combined, se_type = "HC2")

fig3b <- lm_robust(zconf_trust ~ (tweet4 + tweet8 + tweetcorrect)*trump_app_yn, 
                   df_combined, se_type = "HC2")

summary(fig3a)

summary(fig3b)
ss
