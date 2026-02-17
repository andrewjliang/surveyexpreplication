#################
##### Replication of Berlinski et al. (2023)
##### Andrew J. Liang
##### February 5th, 2026
##### Replication performed on 2022 MacBook Air, M2, 16 G.B. RAM, 512 G.B. SSD
##### macOS Sequoia 15.6
#################


#####
## Note: the original analysis was performed in Stata; the original code used 
## by the authors can be found in /data/Berlinskietal2023/voter-fraud.do. I do 
## not use the trimmed data in the original replication package for the power 
## analysis, as part of my analysis involves reconstructing the data for the
## replication.
#####


##### Loading packages
suppressPackageStartupMessages({
library(tidyverse); library(haven); library(DeclareDesign); library(stargazer)
library(fixest); library(here); library(modelsummary); library(marginaleffects)
library(patchwork); library(biostat3); library(cobalt); library(psych);
library(lavaan); library(texreg); library(marginaleffects); library(car);
library(multcomp); library(forcats)
})

# setwd() if needed
here::i_am("code/Replication.R")

# Loading initial dataset and recoding variables
main <- read_dta(here("data/Berlinskietal2023", "survey-data.dta"))
pulse <- read_dta(here("data/Berlinskietal2023", "pulse-data.dta"))

df_combined <- main |>
  left_join(pulse, by = "caseid") |>
  mutate(nopulse_pre = if_else(is.na(totalnewsbinary_presurvey), 1, 0),
         female = case_when(gender == 1 ~ 0, # female
                            gender == 2 ~ 1,
                            .default = NA), 
         nonwhite = if_else(race == 1, 0, 1, missing = NA), # race
         college = if_else(educ %in% c(5, 6), 1, 0, missing = NA), # education
         married = case_when(marstat %in% c(1, 6) ~ 1, 
                             marstat %in% c(2, 3, 4, 5) ~ 0,
                             .default = NA),
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
         highpolint = if_else(polint >= 4, 1, 0, missing = NA),
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
         polknow = if_else(senator_term == 3, polknow + 1, polknow, 
                           missing = polknow),
         polknow = if_else(pres_term_limit == 2, polknow + 1, polknow, 
                           missing = polknow),
         polknow = if_else(senator_num == 2, polknow + 1, polknow, 
                           missing = polknow),
         polknow = if_else(uk_pm == 4, polknow + 1, polknow, 
                           missing = polknow),
         polknow = if_else(rep_term == 1, polknow + 1, polknow, 
                           missing = polknow),
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
         tweet_treat = case_when(tweet_treat_w2 == 4 ~ 1, # control
                                 tweet_treat_w2 == 3 ~ 4, # low + fact-check 
                                 tweet_treat_w2 == 2 ~ 3, # high dose
                                 tweet_treat_w2 == 1 ~ 2, # low dose
                                 .default = NA), 
         tweet4 = if_else(tweet_treat_w2 == 1, 1, 0, missing = NA),
         tweet8 = if_else(tweet_treat_w2 == 2, 1, 0, missing = NA),
         tweetcorrect = if_else(tweet_treat_w2 == 3, 1, 0, missing = NA),
         tweetcontrol = if_else(tweet_treat_w2 == 4, 1, 0, missing = NA),
         tweet_news_2018 = case_when(tweet_news_w2 == 3 ~ 1, 
                                     tweet_news_w2 %in% c(1, 2, 4) ~ 0,
                                     .default = NA),
         tweet_news_control = case_when(tweet_accuracy_control_2_w2 == 1 ~ 1,
                                        tweet_accuracy_control_2_w2 %in% 
                                          c(2, 3, 4) ~ 0,
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
                                              "trustelect1","trustelect2",
                                              "trustelect3")), is.na))
         
  )

## manual creation of some variables
df_combined$conspiracy_mean <- rowMeans(dplyr::select(df_combined, consp1, 
                                                      consp2, consp3), 
                                        na.rm = TRUE)
df_combined$conspiracy_mean <- if_else(is.nan(df_combined$conspiracy_mean), NA, 
                                       df_combined$conspiracy_mean)   


# alpha 
psych::alpha(df_combined |> dplyr::select(consp1, consp2, consp3))
psych::alpha(df_combined |> dplyr::select(conf1, conf2, conf3, conf4))
psych::alpha(df_combined |> dplyr::select(trustelect1, trustelect2, 
                                          trustelect3))

# Factor analysis - Table B1
factors <- df_combined |> dplyr::select(conf1, conf2, conf3, conf4, 
                                         trustelect1, trustelect2, trustelect3,
                                         democ_imp, polsys1, polsys2, polsys3, 
                                         polsys4)

principal(factors, nfactors = 3, rotate = "varimax")

# Structural Equation Model for composite outcome - Table B2
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

# Table 2 - Treatment Effects

## Column 1
summary(lm_robust(conf1 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf1 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
              "tweetcorrect-tweet4"))

## Column 2
summary(lm_robust(conf2 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf2 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Column 3
summary(lm_robust(conf3 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf3 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Column 4 
summary(lm_robust(conf4 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                  se_type = "HC2"))
lincom(lm_robust(conf4 ~ tweet4 + tweet8 + tweetcorrect, data = df_combined,
                 se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Column 5 - Replicate
trust <- lm_robust(trustelect1 ~ tweet4 + tweet8 + tweetcorrect, 
                   data = df_combined, se_type = "HC2")
trust_highlow <- lm_robust(trustelect1 ~ factor(tweet_treat), 
                           data = df_combined |> 
                             filter(tweet_treat %in% c(2, 3)), se_type = "HC2")
trust_lowfact <- lm_robust(trustelect1 ~ factor(tweet_treat), 
                           data = df_combined |>
                             filter(tweet_treat %in% c(2, 4)), se_type = "HC2")
summary(lm_robust(trustelect1 ~ tweet4 + tweet8 + tweetcorrect, 
                  data = df_combined, se_type = "HC2"))

## Column 6
summary(lm_robust(trustelect2 ~ tweet4 + tweet8 + tweetcorrect, 
                  data = df_combined, se_type = "HC2"))
lincom(lm_robust(trustelect2 ~ tweet4 + tweet8 + tweetcorrect, 
                 data = df_combined, se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Column 7
summary(lm_robust(trustelect3 ~ tweet4 + tweet8 + tweetcorrect, 
                  data = df_combined, se_type = "HC2"))
lincom(lm_robust(trustelect3 ~ tweet4 + tweet8 + tweetcorrect, 
                 data = df_combined, se_type = "HC2"), c("tweet8-tweet4",
                                     "tweetcorrect-tweet4"))

## Column 8 - Main Analysis
composite <- lm_robust(zconf_trust ~ tweet4 + tweet8 + tweetcorrect, 
                       data = df_combined, se_type = "HC2")
summary(lm_robust(zconf_trust ~ tweet4 + tweet8 + tweetcorrect, 
                  data = df_combined, se_type = "HC2"))
comp_highlow <- lm_robust(zconf_trust ~ factor(tweet_treat), 
                          data = df_combined |> 
                            filter(tweet_treat %in% c(2, 3)), se_type = "HC2")
comp_lowfact <- lm_robust(zconf_trust ~ factor(tweet_treat), 
                          data = df_combined |>
                            filter(tweet_treat %in% c(2, 4)), se_type = "HC2")


# Figure 2
plotreg(list(lm_robust(zconf_trust ~ tweet4 + tweet8 + tweetcorrect, 
                       data = df_combined,
                       se_type = "HC2")),
        custom.model.names = "Composite Measure",
        custom.coef.map = c("tweet4" = "Low dose (H1a)",
                            "tweet8" = "High dose (H2a)",
                            "tweetcorrect" = "Low dose + fact-check tweets (RQ1a)"),
        theme = theme_classic()) 


# Figure 3(a) and 3(b)
fig3a <- lm_robust(zconf_trust ~ (tweet4 + tweet8 + tweetcorrect) * dem_leaners 
                   + (tweet4 + tweet8 + tweetcorrect) * independents, 
                   df_combined, se_type = "HC2")
fig3acoefs <- as.data.frame(lincom(fig3a, c("tweet4 + tweet4:dem_leaners", 
                                "tweet8 + tweet8:dem_leaners",
                                "tweetcorrect + tweetcorrect:dem_leaners",
                                "tweet4 + tweet4:independents", 
                                "tweet8 + tweet8:independents",
                                "tweetcorrect + tweetcorrect:independents",
                                "tweet4", 
                                "tweet8",
                                "tweetcorrect"))) |>
  rownames_to_column(var = "Treatment") |>
  mutate(party = c("Democrats", "Democrats", "Democrats", "Independents", 
                   "Independents", "Independents", "Republicans", "Republicans", 
                   "Republicans"),
         Treatment = replace_values(Treatment, from = Treatment, 
                                    to = c("Low dose", "High dose", 
                                           "Low dose + fact-check tweets", 
                                           "Low dose", "High dose", 
                                           "Low dose + fact-check tweets", 
                                           "Low dose", "High dose", 
                                           "Low dose + fact-check tweets")),
         Estimate = as.numeric(Estimate),
         lower = as.numeric(`2.5 %`),
         upper = as.numeric(`97.5 %`))
  

df_combined$trump_disapprove <- if_else(df_combined$trump_app_yn == 0, 1, 0, 
                                       missing = 1 - abs(df_combined$trump_app_yn))
fig3b <- lm_robust(zconf_trust ~ (tweet4 + tweet8 + tweetcorrect) * trump_disapprove, 
                   df_combined, se_type = "HC2")
fig3bcoefs <- as.data.frame(lincom(fig3b, c("tweet4 + tweet4:trump_disapprove",
                                            "tweet8 + tweet8:trump_disapprove",
                                            "tweetcorrect + tweetcorrect:trump_disapprove",
                                            "tweet4",
                                            "tweet8",
                                            "tweetcorrect"))) |>
  rownames_to_column(var = "Treatment") |>
  mutate(approval = c("Approve", "Approve", "Approve", "Disapprove", 
                      "Disapprove", "Disapprove"),
         Treatment = replace_values(Treatment, from = Treatment, 
                                    to = c("Low dose", "High dose", 
                                           "Low dose + fact-check tweets", 
                                           "Low dose", "High dose", 
                                           "Low dose + fact-check tweets")),
         Estimate = as.numeric(Estimate),
         lower = as.numeric(`2.5 %`),
         upper = as.numeric(`97.5 %`))


hte_party <- ggplot(fig3acoefs, aes(y = factor(Treatment), color = factor(party), 
                       group = factor(party))) +
  geom_point(aes(x = Estimate), position = position_dodge(width = 0.4)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), width = 0.0, 
                 position = position_dodge(width = 0.4)) +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.4) +
  scale_x_continuous(limits = c(-0.6, 0.2)) +
  scale_y_discrete(limits = c("Low dose", "High dose", 
                              "Low dose + fact-check tweets"),
                   expand = c(0, 0)) +
  labs(x = "Estimated Treatment Effect, SDs",
       y = "Treatment Condition",
       color = "Party") +
  scale_color_manual(values = c("Democrats" = "blue", "Republicans" = "red",
                                "Independents" = "forestgreen")) +
  theme_classic()

ggsave(here("docs", "hte_party.png"), plot = hte_party, 
       units = "in", width = 8, height = 8)

ggplot(fig3bcoefs, aes(y = factor(Treatment), color = factor(approval), 
                       group = factor(approval))) +
  geom_point(aes(x = Estimate), position = position_dodge(width = 0.4)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), width = 0.0, 
                 position = position_dodge(width = 0.4)) +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.4) +
  scale_x_continuous(limits = c(-0.6, 0.2)) +
  scale_y_discrete(limits = c("Low dose", "High dose", 
                              "Low dose + fact-check tweets"),
                   expand = c(0, 0)) +
  theme_classic()

# Covariate-Adjusted Estimates
adj <- df_combined |> # manual scaling instead of lm_lin for regression table
  mutate(female = scale(female, scale = F), 
         nonwhite = scale(nonwhite, scale = F),
         college = scale(college, scale = F), 
         age = scale(age, scale = F),
         polknow = scale(polknow, scale = F),
         married = scale(married, scale = F))

## Trust Elections
trust_adj <- lm_lin(trustelect1 ~ factor(tweet_treat), ~ female + 
                      nonwhite + college + age + polknow + married, 
                    data = df_combined, se_type = "HC2")
trust_highlow_adj <- lm_lin(trustelect1 ~ tweet8, ~ female + 
                              nonwhite + college + age + polknow + married, 
                            data = df_combined |> 
                              filter(tweet_treat %in% c(2, 3)), se_type = "HC2")
trust_lowfact_adj <- lm_lin(trustelect1 ~ tweetcorrect, ~ female + nonwhite + 
                              college + age + 
                              polknow + married, 
                            data = adj |> 
                              filter(tweet_treat %in% c(2, 4)), 
                            se_type = "HC2")


## Composite Outcome
comp_adj <- lm_lin(zconf_trust ~ factor(tweet_treat), ~ female + nonwhite + 
                     college + age + polknow + married, data = df_combined, 
                   se_type = "HC2")
comp_highlow_adj <- lm_lin(zconf_trust ~ tweet8, ~ female + nonwhite + college 
                           + age + polknow + married, data = adj |>
                             filter(tweet_treat %in% c(2, 3)), se_type = "HC2")
comp_lowfact_adj <- lm_lin(zconf_trust ~ tweetcorrect, ~ female + nonwhite + 
                             college + age + 
                             polknow + married, 
                           data = adj |> 
                             filter(tweet_treat %in% c(2, 4)), 
                           se_type = "HC2")


# Power Calculations
set.seed(92092)
N <- 4283

### Table 2, Composite Coefficients as ATE per appendix

### Appendix reports using predicted model to estimate SD of residuals
sd_y_c <- sqrt(composite$res_var)

ate_Low_c <- composite$coefficients[2] # low
ate_High_c <- composite$coefficients[3] # high
ate_Fact_c <- composite$coefficients[4] # low + fact check

replicate_c <- declare_model(N = N, ate_Low_c = ate_Low_c, 
                             ate_High_c = ate_High_c, ate_Fact_c = ate_Fact_c, 
              U = rnorm(N, mean = 0, sd = sd_y_c),
              Y_Z_0 = U,
              Y_Z_1 = ate_Low_c + U,
              Y_Z_2 = ate_High_c + U,
              Y_Z_3 = ate_Fact_c + U) +
  declare_inquiry(ate_low_c = mean(Y_Z_1 - Y_Z_0),
                  ate_high_c = mean(Y_Z_2 - Y_Z_0),
                  ate_fact_c = mean(Y_Z_3 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, conditions = 0:3)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ as.factor(Z), .method = lm_robust, 
                    term = c("as.factor(Z)1", "as.factor(Z)2", "as.factor(Z)3"),
                    inquiry = c("ate_low_c", "ate_high_c", "ate_fact_c"))

d <- draw_data(replicate_c)
power <- declare_diagnosands(power = mean(p.value < 0.05))

set.seed(92092)

### low dose
d1_c <- redesign(replicate_c, ate_Low_c = seq(0, -0.25, by = -0.01))

diag_low_c <- diagnose_design(d1_c, 
                diagnosands = power, 
                sims = 300)

low_c <- diag_low_c$diagnosands_df |> filter(inquiry == "ate_low_c")

low_c_rep <- ggplot(low_c, aes(ate_Low_c, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = composite$coefficients[2])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  labs(x = "Average Treatment Effect",
       y = "Power",
       title = "ATE for Low Dose") +
  theme_minimal()


### high dose
d2_c <- redesign(replicate_c, ate_High_c = seq(0, -0.25, by = -0.01))

diag_high_c <- diagnose_design(d2_c, 
                            diagnosands = power, 
                            sims = 300)

high_c <- diag_high_c$diagnosands_df |> filter(inquiry == "ate_high_c")

high_c_rep <- ggplot(high_c, aes(ate_High_c, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = composite$coefficients[3])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  labs(x = "Average Treatment Effect",
       y = "Power",
       title = "ATE for High Dose") +
  theme_minimal()


### fact-check dose
d3_c <- redesign(replicate_c, ate_Fact_c = seq(0, -0.25, by = -0.01))

diag_fact_c <- diagnose_design(d3_c, 
                             diagnosands = power, 
                             sims = 300)

fact_c <- diag_fact_c$diagnosands_df |> filter(inquiry == "ate_fact_c")

fact_c_rep <- ggplot(fact_c, aes(ate_Fact_c, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = composite$coefficients[4])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  labs(x = "Average Treatment Effect",
       y = "Power",
       title = "ATE for Low + Fact-Check") +
  theme_minimal()


## Table 2, Column 5
set.seed(92092)
sd_y_t <- sqrt(trust$res_var)

ate_Low_t <- trust$coefficients[2] # low
ate_High_t <- trust$coefficients[3] # high
ate_Fact_t <- trust$coefficients[4] # low + fact check

replicate_t <- declare_model(N = N, ate_Low_t = ate_Low_t, 
                             ate_High_t = ate_High_t, ate_Fact_t = ate_Fact_t, 
                             U = rnorm(N, mean = 0, sd = sd_y_t),
                             Y_Z_0 = U,
                             Y_Z_1 = ate_Low_t + U,
                             Y_Z_2 = ate_High_t + U,
                             Y_Z_3 = ate_Fact_t + U) +
  declare_inquiry(ate_low_t = mean(Y_Z_1 - Y_Z_0),
                  ate_high_t = mean(Y_Z_2 - Y_Z_0),
                  ate_fact_t = mean(Y_Z_3 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, conditions = 0:3)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ as.factor(Z), .method = lm_robust, 
                    term = c("as.factor(Z)1", "as.factor(Z)2", "as.factor(Z)3"),
                    inquiry = c("ate_low_t", "ate_high_t", "ate_fact_t"))

d_t <- draw_data(replicate_t)

set.seed(92092)

### low dose
d1_t <- redesign(replicate_t, ate_Low_t = seq(0, -0.25, by = -0.01))

diag_low_t <- diagnose_design(d1_t, 
                              diagnosands = power, 
                              sims = 300)

low_t <- diag_low_t$diagnosands_df |> filter(inquiry == "ate_low_t")

ggplot(low_t, aes(ate_Low_t, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = trust$coefficients[2])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed")


### high dose
d2_t <- redesign(replicate_t, ate_High_t = seq(0, -0.25, by = -0.01))

diag_high_t <- diagnose_design(d2_t, 
                               diagnosands = power, 
                               sims = 300)

high_t <- diag_high_t$diagnosands_df |> filter(inquiry == "ate_high_t")

ggplot(high_t, aes(ate_High_t, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = trust$coefficients[3])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed")


### fact-check dose
d3_t <- redesign(replicate_t, ate_Fact_t = seq(0, -0.25, by = -0.01))

diag_fact_t <- diagnose_design(d3_t, 
                               diagnosands = power, 
                               sims = 300)

fact_t <- diag_fact_t$diagnosands_df |> filter(inquiry == "ate_fact_t")

ggplot(fact_t, aes(ate_Fact_t, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = trust$coefficients[4])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed")

# sample sizes for power
ss_c <- replicate_c |>
  redesign(N = seq(4200, 8000, 100)) |>
  diagnose_designs()

ss_t <- replicate_t |>
  redesign(N = seq(3000, 40000, 1000)) |>
  diagnose_designs()

## composite
pwr_low_c <- ss_c$diagnosands_df |> 
  filter(inquiry == "ate_low_c") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 4283)) +
  labs(x = "Sample Size",
       y = "Statistical Power", 
       title = "ATE for Low Dose, Composite") +
  theme_minimal()

pwr_high_c <- ss_c$diagnosands_df |> 
  filter(inquiry == "ate_high_c") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 4283)) +
  labs(x = "Sample Size",
       y = "Statistical Power",
       title = "ATE for High Dose, Composite") +
  theme_minimal()


pwr_fact_c <- ss_c$diagnosands_df |> 
  filter(inquiry == "ate_fact_c") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 4283)) +
  labs(x = "Sample Size",
       y = "Statistical Power",
       title = "ATE for Low Dose + Fact-Check, Composite") +
  theme_minimal()

pwr_c <- (pwr_low_c + pwr_high_c) / pwr_fact_c

ggsave(here("docs", "samplesizepower_composite.png"), plot = pwr_c, 
       units = "in", width = 7, height = 4)


## trust
pwr_low_t <- ss_t$diagnosands_df |> 
  filter(inquiry == "ate_low_t") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 4283)) +
  labs(x = "Sample Size",
       y = "Statistical Power",
       title = "ATE for Low Dose, Trust") +
  theme_minimal()


pwr_high_t <- ss_t$diagnosands_df |> 
  filter(inquiry == "ate_high_t") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  labs(x = "Sample Size",
       y = "Statistical Power",
       title = "ATE for High Dose, Trust") +
  theme_minimal()

  geom_vline(aes(xintercept = 4283))

pwr_fact_t <- ss_t$diagnosands_df |> 
  filter(inquiry == "ate_fact_t") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 4283)) +
  labs(x = "Sample Size",
       y = "Statistical Power",
       title = "ATE for Low Dose + Fact-Check, Trust") +
  theme_minimal()


# Effect of Fact Check Power Analysis
### Composite
set.seed(92092)
sd_y_clf <- sqrt(comp_lowfact$res_var)

ate_Factlow_c <- comp_lowfact$coefficients[2] 

replicate_clf <- declare_model(N = N, ate_Factlow_c = ate_Factlow_c,
                             U = rnorm(N, mean = 0, sd = sd_y_clf),
                             Y_Z_0 = U,
                             Y_Z_1 = ate_Factlow_c + U) +
  declare_inquiry(ate_factlow_c = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, conditions = 0:1)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ as.factor(Z), .method = lm_robust, 
                    term = c("as.factor(Z)1"),
                    inquiry = c("ate_factlow_c"))

d_clf <- draw_data(replicate_clf)

set.seed(92092)

d1_clf <- redesign(replicate_clf, ate_Factlow_c = seq(0, 0.25, by = 0.01))

diag_factlow_c <- diagnose_design(d1_clf, 
                              diagnosands = power, 
                              sims = 300)

factlow_c <- diag_factlow_c$diagnosands_df |> filter(inquiry == "ate_factlow_c")

ggplot(factlow_c, aes(ate_Factlow_c, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = comp_lowfact$coefficients[2])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed")

### sample sizes for power
ss_clf <- replicate_clf |>
  redesign(N = seq(4200, 15000, 100)) |>
  diagnose_designs()

pwr_factlow_c <- ss_clf$diagnosands_df |> 
  filter(inquiry == "ate_factlow_c") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 4283)) +
  labs(x = "Sample Size",
       y = "Statistical Power", 
       title = "ATE for Low + Fact-Check - Low Dose, Composite") +
  theme_minimal()


### Trust
set.seed(92092)
sd_y_tlf <- sqrt(trust_lowfact$res_var)

ate_Factlow_t <- trust_lowfact$coefficients[2] 

replicate_tlf <- declare_model(N = N, ate_Factlow_t = ate_Factlow_t,
                               U = rnorm(N, mean = 0, sd = sd_y_tlf),
                               Y_Z_0 = U,
                               Y_Z_1 = ate_Factlow_t + U) +
  declare_inquiry(ate_factlow_t = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, conditions = 0:1)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ as.factor(Z), .method = lm_robust, 
                    term = c("as.factor(Z)1"),
                    inquiry = c("ate_factlow_t"))

d_tlf <- draw_data(replicate_tlf)

set.seed(92092)

d1_tlf <- redesign(replicate_tlf, ate_Factlow_t = seq(0, 0.25, by = 0.01))

diag_factlow_t <- diagnose_design(d1_tlf, 
                                  diagnosands = power, 
                                  sims = 300)

factlow_t <- diag_factlow_t$diagnosands_df |> filter(inquiry == "ate_factlow_t")

ggplot(factlow_t, aes(ate_Factlow_t, power)) + 
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = trust_lowfact$coefficients[2])) +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed")

### sample sizes for power
ss_tlf <- replicate_tlf |>
  redesign(N = seq(4200, 15000, 100)) |>
  diagnose_designs()

pwr_factlow_t <- ss_tlf$diagnosands_df |> 
  filter(inquiry == "ate_factlow_t") |>
  ggplot(aes(N, power)) +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = 0.8), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 4283)) +
  labs(x = "Sample Size",
       y = "Statistical Power", 
       title = "ATE for Low + Fact-Check - Low Dose, Trust") +
  theme_minimal()


# Graphs

pwr_graphs <- (low_c_rep + pwr_low_c) / (high_c_rep + pwr_high_c) / (fact_c_rep + pwr_fact_c)
  
ggsave(here("docs", "pwr.png"), plot = pwr_graphs, units = "in", 
       width = 8, height = 8)

