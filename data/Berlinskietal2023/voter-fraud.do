clear

//Set your directory here
*cd "/Users/SetYourDirectory/Replication"

//Packages to install  -- we commented out.
//ssc install cibar
//ssc install estout
//ssc install tost
//ssc install tostregress
//ssc install smileplot  
//net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)

//Open and code data
use "survey-data.dta", clear


******************************************************************************
*** Merging and setting up the  data and recoding a few variables.
******************************************************************************

merge 1:1 caseid using "pulse-data.dta"
tab _merge
gen nopulse_pre=(_merge==1)
tab nopulse_pre

// Demographic variables

// female
gen female = gender
recode female 2=1 1=0
tab female gender, missing

// race
gen nonwhite = race
recode nonwhite 2=1 3=1 4=1 5=1 6=1 7=1 8=1 1=0
tab race nonwhite, missing

//education
gen college = educ
recode college 1=0 2=0 3=0 4=0 5=1 6=1
tab college educ, missing

// age
gen age = 2018-birthyr 
gen agecat=age
/* -age groups (18-24, 25-44, 45-64, 65+)*/
replace agecat=1 if age>17
replace agecat=2 if age>24
replace agecat=3 if age>44
replace agecat=4 if age>64
replace agecat=. if age==.
tab agecat age, missing

// age category dummy variables
gen agecat1=agecat
gen agecat2=agecat
gen agecat3=agecat
gen agecat4=agecat

// rescaling
recode agecat1 1=1 2=0 3=0 4=0
recode agecat2 2=1 1=0 3=0 4=0
recode agecat3 3=1 2=0 1=0 4=0
recode agecat4 4=1 2=0 3=0 1=0

// ideology
gen ideology=ideo /*lower values = more left*/

// party
gen dem = pid3
recode dem 1=1 else=0
gen repub = pid3
recode repub 2=1 else=0
gen ind3pt = pid3 
recode ind3pt 3=1 4=1 5=1 else=0 /*ind, other, not sure*/

// lean variables
gen dem_leaners = pid7
recode dem_leaners 1=1 2=1 3=1 else=0
gen repub_leaners = pid7
recode repub_leaners 5=1 6=1 7=1 else=0
gen independents = pid7 
recode independents 4=1 8=1 else=0 /*ind, not sure*/

gen pid3_lean=.
replace pid3_lean=1 if dem_leaners==1
replace pid3_lean=2 if independents==1
replace pid3_lean=3 if repub_leaners==1

rename trump_approve old_trump_approve
recode old_trump_approve 1=4 2=3 3=2 4=1, gen(trump_approve)

gen trump_app_yn = .
replace trump_app_yn = 1 if old_trump_approve == 1 | old_trump_approve == 2
replace trump_app_yn = 0 if old_trump_approve == 3 | old_trump_approve == 4

******************************************************************************
*sample data for Table A1: Sample characteristics (left column)
******************************************************************************
tab educ
tab agecat
tab gender
tab pid3
tab trump_app_yn
******************************************************************************

// political interest
gen polint = pol_interest
recode polint 1=5 2=4 3=3 4=2 5=1 /*recode very interest high*/
tab polint pol_interest

// political feelings towards
gen FT_trump = pol_therm_trump
gen FT_rep = pol_therm_rep
gen FT_dem = pol_therm_dem
gen FT_media = pol_therm_media

// affective polarization w1

gen dem_less_repw1 = FT_dem - FT_rep
gen rep_less_demw1 = FT_rep - FT_dem

gen dem_less_rep_w1 = dem_less_repw1 /*Cross with party*/
gen rep_less_dem_w1 = rep_less_demw1 
replace dem_less_rep_w1 = 0 if repub==1
replace rep_less_dem_w1 = 0 if dem==1

gen affect_mergedw1 = dem_less_rep_w1 + rep_less_dem_w1 

// with leaners 

gen dem_less_rep_w1x = dem_less_repw1 /*Cross with party with leaners*/
gen rep_less_dem_w1x = rep_less_demw1 
replace dem_less_rep_w1x = 0 if repub_leaners==1
replace rep_less_dem_w1x = 0 if dem_leaners==1

gen affect_merged_leanersw1 = dem_less_rep_w1x + rep_less_dem_w1x 

// political knowledge
gen polknow = 0
replace polknow=polknow+1 if senator_term==3
replace polknow=polknow+1 if pres_term_limit ==2
replace polknow=polknow+1 if senator_num ==2
replace polknow=polknow+1 if uk_pm ==4
replace polknow=polknow+1 if rep_term==1

tab polknow, missing

// trust in mass media
gen massmedia_trust = media_trust /* recode high trust high */
recode massmedia_trust 1=4 2=3 3=2 4=1
tab massmedia_trust media_trust, missing

// trust 
gen fbtrust = fb_trust /* recode high trust high */
recode fbtrust 1=4 2=3 3=2 4=1
tab fbtrust fb_trust, missing

// use of facebook
gen fb_use = fb_freq
recode fb_use 1=9 2=8 3=7 4=6 5=5 6=4 7=3 8=2 9=1 /* recode high use high */
tab fb_use fb_freq, missing

// use of facebook for political news
gen fb_pol_use = fb_political_freq
recode fb_pol_use 1=9 2=8 3=7 4=6 5=5 6=4 7=3 8=2 9=1 /* recode high use high */
tab fb_pol_use fb_political_freq, missing

// how often you share news on facebook
gen fb_pol_share = fb_share_freq 
recode fb_pol_share 1=9 2=8 3=7 4=6 5=5 6=4 7=3 8=2 9=1 /* recode high use high */
tab fb_pol_share fb_share_freq, missing

gen consp1=conspiracy_1
gen consp2=conspiracy_2
gen consp3=conspiracy_3
recode consp1 1=5 2=4 3=3 4=2 5=1 /* recode high consp high */
recode consp2 1=5 2=4 3=3 4=2 5=1
recode consp3 1=5 2=4 3=3 4=2 5=1

alpha consp1 consp2 consp3 /* alpha = .74 */, item 

egen conspiracy_mean  = rowmean(consp1  consp2  consp3)

// Setting up treatment variables (better names and consistent coding)

gen tweet_treat = tweet_treat_w2
gen tweet4 = tweet_treat
gen tweet8= tweet_treat
gen tweetcorrect = tweet_treat
gen tweetcontrol = tweet_treat

recode tweet4 1=1 else=0
recode tweet8 2=1 else=0
recode tweetcorrect 3=1 else=0
recode tweetcontrol 4=1 else=0

foreach var of varlist tweet4 tweet8 tweetcorrect tweetcontrol {
replace `var'=. if tweet_treat==.
}

************************************************************************************************************************
*sample data for Table A2: Sample characteristics by treatment (==1 -> low dose, ==2 -> high dose, ==3 -> low dose+factcheck, ==4 -> control)
************************************************************************************************************************
tab educ if tweet_treat_w2 ==1
tab educ if tweet_treat_w2 ==2
tab educ if tweet_treat_w2 ==3
tab educ if tweet_treat_w2 ==4

tab agecat if tweet_treat_w2 ==1
tab agecat if tweet_treat_w2 ==2
tab agecat if tweet_treat_w2 ==3
tab agecat if tweet_treat_w2 ==4

tab gender if tweet_treat_w2 ==1
tab gender if tweet_treat_w2 ==2
tab gender if tweet_treat_w2 ==3
tab gender if tweet_treat_w2 ==4

tab pid3 if tweet_treat_w2 ==1
tab pid3 if tweet_treat_w2 ==2
tab pid3 if tweet_treat_w2 ==3
tab pid3 if tweet_treat_w2 ==4

tab trump_app_yn if tweet_treat_w2 ==1
tab trump_app_yn if tweet_treat_w2 ==2
tab trump_app_yn if tweet_treat_w2 ==3
tab trump_app_yn if tweet_treat_w2 ==4
************************************************************************************************************************

** attention check 

gen tweet_news_2018 = tweet_news_w2
recode tweet_news_2018 3=1 1=0 2=0 4=0 

tab tweet_news_2018 if tweet_treat_w2 ==1 //81.9%
tab tweet_news_2018 if tweet_treat_w2 ==2 // 85.7%
tab tweet_news_2018 if tweet_treat_w2 ==3 //81.9%

gen tweet_news_control = tweet_accuracy_control_2_w2
recode tweet_news_control 1=1 2=0 3=0 4=0

tab tweet_news_control if tweet_treat_w2 ==4  //88.9%

// Trust variables

gen massmedia_trustw2 = media_trust_w2 /* recode high trust high */
recode massmedia_trustw2 1=4 2=3 3=2 4=1
tab massmedia_trustw2 media_trust_w2, missing

gen fbtrustw2 = fb_trust_w2 /* recode high trust high */
recode fbtrustw2 1=4 2=3 3=2 4=1
tab fbtrustw2 fb_trust_w2, missing

// Affect
gen FT_muslim = group_affect_muslim_w2
gen FT_christian = group_affect_christian_w2
gen FT_white = group_affect_white_w2
gen FT_black = group_affect_black_w2
gen FT_labor = group_affect_labor_w2
gen FT_rich = group_affect_rich_w2
gen FT_latino = group_affect_latino_w2
gen FT_white_latino = FT_white-FT_latino
gen FT_christian_muslim = FT_christian-FT_muslim

***************************************************************************************************
**Table A3 part 1: Missing data by treatment - moderators (outcome measures continued at end of do-file)
***************************************************************************************************
/*note: for tweet_treat_w2, ==1 -> low dose, ==2 -> high dose, ==3 -> low dose+factcheck, ==4 -> control, also, if . is not observed in list of unique values, there are no missing values.*/

bysort tweet_treat_w2: tab pid3 if tweet_treat_w2!=., missing 
bysort tweet_treat_w2: tab trump_app_yn if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab FT_trump if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab FT_media if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab massmedia_trustw2 if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab conspiracy_mean if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab polint if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab polknow if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab totalfakebinary18_presurvey if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab totalfcbinary_presurvey if tweet_treat_w2!=., missing
***************************************************************************************************

// Dependent variable construction for analysis -- See Appendix B

gen conf1 = vote_entitled_w2 /*Confidence entitled allowed to vote - PRE-VOTE NOT POST-VOTE*/
gen conf2 = plan_vote_certain_w2 /*Confidence own vote was counted - LOCAL ONLY PLUS MISSING*/
gen conf3 = officials_count_w2 /*Confidence officials manage counting votes*/
gen conf4 = system_works_w2 /*System works despite problems casting and counting votes*/

// Factor analysis
alpha (conf1 conf2 conf3 conf4) // .79

gen trustelect1 =  trust_elections_w2 /*Trust in elections*/
gen trustelect2 = secure_ballot_w2 /*Security of ballots from tampering*/
gen trustelect3 = machine_accurate_w2 /*Frequency voting machines accurate in counting votes*/

alpha (trustelect1 trustelect2  trustelect3) // .80

gen democ_imp = importance_democracy_w2 /*Importance of living in democratically governed country*/

gen polsys1 =  polsystem_w2_1 /*Having a strong leader who does not have to*/
gen polsys2 =  polsystem_w2_2 /*Having experts, not government, make decisi*/
gen polsys3 =  polsystem_w2_3 /*Having the army rule*/
gen polsys4 =  polsystem_w2_4 /*Having a democratic political system*/

**********************************************************************
*Table B1: Preregistered factor analysis of all measured outcomes
**********************************************************************
factor conf1 conf2 conf3 conf4 trustelect1 trustelect2  trustelect3 democ_imp polsys1 polsys2 polsys3 polsys4, pcf
rotate, varimax
**********************************************************************

// Rescaling confidence/trust variables

// trust/confidence = high
recode conf1 4=1 3=2 2=3 1=4
recode conf2 4=1 3=2 2=3 1=4
recode conf3 4=1 3=2 2=3 1=4
recode conf4 4=1 3=2 2=3 1=4
recode polsys4 4=1 3=2 2=3 1=4
recode trustelect2 5=1 4=2 3=3 2=4 1=5
recode trustelect3 5=1 4=2 3=3 2=4 1=5
pwcorr conf1-conf4 trustelec*

**********************************************************************************************************
*Table B2: Structural equation model for latent election confidence measure
**********************************************************************************************************
sem (Conf_trust -> conf1 conf2 conf3 conf4 trustelect1 trustelect2  trustelect3), method(mlmv) standard 
**********************************************************************************************************

estat gof, stats(all)
gen missing=(conf1==. & conf2==. & conf3==. & conf4==. & trustelect1==. & trustelect2==. & trustelect3==.)
predict conf_trust if tweet_treat!=., latent
egen zconf_trust=std(conf_trust)
su conf_trust zconf_trust

************************************************************
*Table 1: Measures of confidence in elections
************************************************************
su conf1 conf2 conf3 conf4 trustelect1 trustelect2 trustelect3 zconf_trust
************************************************************

reg conf1 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A1

reg conf2 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A2

reg conf3 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A3

reg conf4 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A4

reg trustelect1 tweet4 tweet8 tweetcorrect,robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A5

reg trustelect2 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A6

reg trustelect3 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A7

reg zconf_trust tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store A8

******************************************************************************
*Table 2: Effect of exposure to voter fraud allegations on election confidence
******************************************************************************
estout A1 A2 A3 A4 A5 A6 A7 A8, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
******************************************************************************

//excluded variables 

reg polsys1 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store E1

forval i=1/20 {
local i=`i'/100
tostregress polsys1 tweet4 tweet8 tweetcorrect, eqvt(delta) eqvl(`i') rel 
}

reg polsys2 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store E2

forval i=1/20 {
local i=`i'/100
tostregress polsys2 tweet4 tweet8 tweetcorrect, eqvt(delta) eqvl(`i') rel 
}

reg polsys3 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store E3

forval i=1/20 {
local i=`i'/100
tostregress polsys3 tweet4 tweet8 tweetcorrect, eqvt(delta) eqvl(`i') rel 
}

gen polsys4_r = polsys4
recode polsys4_r 4=1 3=2 2=3 1=4 // now support for dem system is high

reg polsys4_r tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store E4

forval i=1/20 {
local i=`i'/100
tostregress polsys4 tweet4 tweet8 tweetcorrect, eqvt(delta) eqvl(`i') rel 
}

gen democ_imp4=democ_imp/4
reg democ_imp4 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store E5

forval i=1/30 {
local i=`i'/100
tostregress democ_imp4 tweet4 tweet8 tweetcorrect, eqvt(delta) eqvl(`i') rel 
}

*composites (additive) 
egen demo_sup1 = rowmean(polsys1 polsys2 polsys3)
egen demo_sup2 = rowmean (polsys4_r  democ_imp4)

reg demo_sup1 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store democ1

forval i=1/40 {
local i=`i'/100
tostregress demo_sup1 tweet4 tweet8 tweetcorrect, eqvt(delta) eqvl(`i') rel 
}

reg demo_sup2 tweet4 tweet8 tweetcorrect, robust
lincom tweet8-tweet4
lincom tweetcorrect-tweet4
est store democ2

forval i=1/20 {
local i=`i'/100
tostregress demo_sup2 tweet4 tweet8 tweetcorrect, eqvt(delta) eqvl(`i') rel 
}

************************************************************
*Table C4: Main treatment effects for support for democracy
************************************************************
estout E1 E2 E3 E4 E5 democ1 democ2, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex) 
************************************************************

reg zconf_trust tweet4 tweet8 tweetcorrect, robust
est store main_fx

estout main_fx, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)

capture drop point ul ll

gen point =.
gen ll=.
gen ul=.

lincom tweet4
replace point = r(estimate) if tweet4 == 1 
replace ll = r(lb) if tweet4 == 1
replace ul = r(ub) if tweet4 == 1 

lincom tweet8
replace point = r(estimate) if tweet8 == 1 
replace ll = r(lb) if tweet8 == 1 
replace ul = r(ub) if tweet8 == 1 

lincom tweet8 - tweet4
lincom tweet4 - tweetcorrect

lincom tweetcorrect
replace point = r(estimate) if tweetcorrect == 1
replace ll = r(lb) if tweetcorrect == 1
replace ul = r(ub) if tweetcorrect == 1

preserve

collapse (mean) point ll ul, by(tweet4 tweet8 tweetcorrect)

gen ordernum=.
replace ordernum=1 if tweetcorrect==1
replace ordernum=2 if tweet8==1
replace ordernum=3 if tweet4==1

********************************************************************************************
*Figure 2: Marginal effect of exposure to claims of voter fraud on confidence in elections
********************************************************************************************
twoway (scatter ordernum point, xlabel(-.3(.1).1,labsize(*.8)) msymbol(s) mcolor(purple)) (rspike ll ul ordernum, horizontal lcolor(purple) scheme(lean1) legend(off) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ylab(1 `" "Low dose +"  "fact-check tweets" "' 2 "High dose" 3 "Low dose", grid angle(0) glcolor(gs3)) ytitle("") xline(0,lcolor(gs10)))
graph export "Figures/zconf_heA2.png", replace
********************************************************************************************

restore 

gen independentsXtweet4 = independents * tweet4
gen independentsXtweet8 = independents * tweet8
gen independentsXtweetcorrect = independents * tweetcorrect

gen repub_leanersXtweet4 = repub_leaners * tweet4
gen repub_leanersXtweet8 = repub_leaners * tweet8
gen repub_leanersXtweetcorrect = repub_leaners * tweetcorrect

gen dem_leanersXtweet4 = dem_leaners * tweet4
gen dem_leanersXtweet8 = dem_leaners * tweet8
gen dem_leanersXtweetcorrect = dem_leaners * tweetcorrect

reg zconf_trust tweet4 tweet8 tweetcorrect dem_leaners independents dem_leanersXtweet4 dem_leanersXtweet8 dem_leanersXtweetcorrect independentsXtweet4 independentsXtweet8 independentsXtweetcorrect, robust
est store model_i
********************************************************************************************
*Table C1: Effect of exposure to voter fraud allegations on election confidence by party
********************************************************************************************
estout model_i, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex) 

lincom (tweet8+dem_leanersXtweet8)-(tweet4+dem_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+dem_leanersXtweet8)-(tweet4+dem_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+dem_leanersXtweetcorrect)-(tweet4+dem_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+dem_leanersXtweetcorrect)-(tweet4+dem_leanersXtweet4))-(tweetcorrect-tweet4)
********************************************************************************************

capture drop point ul ll

gen point = .
gen ul = .
gen ll = .

lincom tweet4
replace point = r(estimate) if tweet4 == 1 & independents == 0 & dem_leaners == 0
replace ul = r(ub) if tweet4 == 1 & independents == 0 & dem_leaners == 0
replace ll = r(lb) if tweet4 == 1 & independents == 0 & dem_leaners == 0

lincom tweet8
replace point = r(estimate) if tweet8 == 1 & independents == 0 & dem_leaners == 0
replace ul = r(ub) if tweet8 == 1 & independents == 0 & dem_leaners == 0
replace ll = r(lb) if tweet8 == 1 & independents == 0 & dem_leaners == 0

lincom tweetcorrect
replace point = r(estimate) if tweetcorrect == 1 & independents == 0 & dem_leaners == 0
replace ul = r(ub) if tweetcorrect == 1 & independents == 0 & dem_leaners == 0
replace ll = r(lb) if tweetcorrect == 1 & independents == 0 & dem_leaners == 0

lincom tweet4 + independentsXtweet4
replace point = r(estimate) if tweet4 == 1 & independents == 1
replace ul = r(ub) if tweet4 == 1 & independents == 1
replace ll = r(lb) if tweet4 == 1 & independents == 1

lincom tweet8 + independentsXtweet8
replace point = r(estimate) if tweet8 == 1 & independents == 1
replace ul = r(ub) if tweet8 == 1 & independents == 1
replace ll = r(lb) if tweet8 == 1 & independents == 1

lincom tweetcorrect + independentsXtweetcorrect
replace point = r(estimate) if tweetcorrect == 1 & independents == 1
replace ul = r(ub) if tweetcorrect == 1 & independents == 1
replace ll = r(lb) if tweetcorrect == 1 & independents == 1

lincom tweet4 + dem_leanersXtweet4
replace point = r(estimate) if tweet4 == 1 & dem_leaners == 1
replace ul = r(ub) if tweet4 == 1 & dem_leaners == 1
replace ll = r(lb) if tweet4 == 1 & dem_leaners == 1

lincom tweet8 + dem_leanersXtweet8
replace point = r(estimate) if tweet8 == 1 & dem_leaners == 1
replace ul = r(ub) if tweet8 == 1 & dem_leaners == 1
replace ll = r(lb) if tweet8 == 1 & dem_leaners == 1

lincom tweetcorrect + dem_leanersXtweetcorrect
replace point = r(estimate) if tweetcorrect == 1 & dem_leaners == 1
replace ul = r(ub) if tweetcorrect == 1 & dem_leaners == 1
replace ll = r(lb) if tweetcorrect == 1 & dem_leaners == 1

gen party_lean = .
replace party_lean = 1 if dem_leaners == 1
replace party_lean = 2 if independents == 1
replace party_lean = 3 if dem_leaners == 0 & independents == 0

preserve

collapse (mean) point ul ll, by(tweet4 tweet8 tweetcorrect party_lean)

gen ordernum = .
replace ordernum = .90 if tweetcorrect == 1 & party_lean == 1
replace ordernum = 1 if tweetcorrect == 1 & party_lean == 2
replace ordernum = 1.1 if tweetcorrect == 1 & party_lean == 3

replace ordernum = 1.90 if tweet8 == 1 & party_lean == 1
replace ordernum = 2 if tweet8 == 1 & party_lean == 2
replace ordernum = 2.1 if tweet8 == 1 & party_lean == 3

replace ordernum = 2.90 if tweet4 == 1 & party_lean == 1
replace ordernum = 3 if tweet4 == 1 & party_lean == 2
replace ordernum = 3.1 if tweet4 == 1 & party_lean == 3

********************************************************************************************
*Figure 3a: Effect of exposure to claims of voter fraud on election confidence by party
********************************************************************************************
twoway (scatter ordernum point if party_lean == 1, xlabel(,labsize(*.8)) msymbol(S) mcolor(blue)) (scatter ordernum point if party_lean == 2, xlabel(,labsize(*.8)) msymbol(O) mcolor(green)) (scatter ordernum point if party_lean == 3, mcolor(red) msymbol(T) legend(order(1 2 3) lab(1 "Democrats") lab(2 "Independents") lab(3 "Republican") row(1) pos(6) region(lpattern(solid) lcolor(black)))) (rspike ll ul ordernum if party_lean == 1, horizontal lcolor(blue) scheme(lean1) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))  ytitle("") xline(0,lcolor(gs10)) xtitle("", size(*.7)) ylab(3 "Low dose" 2 "High dose" 1 `" "Low dose +"  "fact-check tweets" "', grid angle(0) glcolor(gs3))) (rspike ll ul ordernum if party_lean == 2, horizontal lcolor(green)) (rspike ll ul ordernum if party_lean == 3, horizontal lcolor(red))

graph export "Figures/zconf_party_independents.png", replace
********************************************************************************************
restore

reg zconf_trust tweet4 tweet8 tweetcorrect repub_leaners repub_leanersXtweet4 repub_leanersXtweet8 repub_leanersXtweetcorrect, robust
est store model_i

*********************************************************************************************************************************
*Table C3: Effect of exposure to voter fraud allegations on election confidence by party (Republicans vs. Democrats/independents)
*********************************************************************************************************************************
estout model_i, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex) 

lincom (tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4))-(tweetcorrect-tweet4)
*********************************************************************************************************************************

gen trumpdisapprove=abs(1-trump_app_yn) if trump_app_yn!=.

gen tweet4Xtrump_app_yn = tweet4 * trump_app_yn
gen tweet8Xtrump_app_yn = tweet8 * trump_app_yn
gen tweetcorrectXtrump_app_yn = tweetcorrect * trump_app_yn

gen tweet4Xtrumpdisapprove = tweet4 * trumpdisapprove
gen tweet8Xtrumpdisapprove = tweet8 * trumpdisapprove
gen tweetcorrectXtrumpdisapprove = tweetcorrect * trumpdisapprove

reg zconf_trust tweet4 tweet8 tweetcorrect trumpdisapprove tweet4Xtrumpdisapprove tweet8Xtrumpdisapprove tweetcorrectXtrumpdisapprove, robust
est store robustness_check

****************************************************************************************************
*Table C2: Effect of exposure to voter fraud allegations on election confidence by Trump approval
****************************************************************************************************
estout robustness_check, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex) 

lincom (tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweet8-tweet4
return list
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)

lincom (tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweetcorrect-tweet4)
****************************************************************************************************

save "export-for-R.dta", replace 

capture drop point ul ll

gen point = .
gen ul = .
gen ll = .

lincom tweet4 
replace point = r(estimate) if tweet4 == 1 & trumpdisapprove == 0
replace ul = r(ub) if tweet4 == 1 & trumpdisapprove == 0
replace ll = r(lb) if tweet4 == 1 & trumpdisapprove == 0

lincom tweet8 
replace point = r(estimate) if tweet8 == 1 & trumpdisapprove == 0
replace ul = r(ub) if tweet8 == 1 & trumpdisapprove == 0
replace ll = r(lb) if tweet8 == 1 & trumpdisapprove == 0

lincom tweetcorrect 
replace point = r(estimate) if tweetcorrect == 1 & trumpdisapprove == 0
replace ul = r(ub) if tweetcorrect == 1 & trumpdisapprove == 0
replace ll = r(lb) if tweetcorrect == 1 & trumpdisapprove == 0

lincom tweet4 + tweet4Xtrumpdisapprove
replace point = r(estimate) if tweet4 == 1 & trumpdisapprove == 1
replace ul = r(ub) if tweet4 == 1 & trumpdisapprove == 1
replace ll = r(lb) if tweet4 == 1 & trumpdisapprove == 1

lincom tweet8 + tweet8Xtrumpdisapprove
replace point = r(estimate) if tweet8 == 1 & trumpdisapprove == 1
replace ul = r(ub) if tweet8 == 1 & trumpdisapprove == 1
replace ll = r(lb) if tweet8 == 1 & trumpdisapprove == 1

lincom tweetcorrect + tweetcorrectXtrumpdisapprove
replace point = r(estimate) if tweetcorrect == 1 & trumpdisapprove == 1
replace ul = r(ub) if tweetcorrect == 1 & trumpdisapprove == 1
replace ll = r(lb) if tweetcorrect == 1 & trumpdisapprove == 1

preserve

collapse (mean) point ul ll, by(tweet4 tweet8 tweetcorrect trumpdisapprove)

gen ordernum = .

replace ordernum = .95 if tweetcorrect == 1 & trumpdisapprove == 1
replace ordernum = 1.05 if tweetcorrect == 1 & trumpdisapprove == 0

replace ordernum = 1.95 if tweet8 == 1 & trumpdisapprove == 1
replace ordernum = 2.05 if tweet8 == 1 & trumpdisapprove == 0

replace ordernum = 2.95 if tweet4 == 1 & trumpdisapprove == 1
replace ordernum = 3.05 if tweet4 == 1 & trumpdisapprove == 0

****************************************************************************************************
*Figure 3b: Effect of exposure to claims of voter fraud on election confidence by Trump approval
****************************************************************************************************
twoway (scatter ordernum point if trumpdisapprove == 0, xlabel(,labsize(*.8)) msymbol(T) mcolor(red*1)) (scatter ordernum point if trumpdisapprove == 1, mcolor(red*2) msymbol(S) legend(order(1 2) lab(1 "Approve") lab(2 "Disapprove") row(1) pos(6) region(lpattern(solid) lcolor(black)))) (rspike ll ul ordernum if trumpdisapprove == 0, horizontal lcolor(red*1) scheme(lean1) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))  ytitle("") xline(0,lcolor(gs10)) xtitle("", size(*.7)) ylab(3 "Low dose" 2 "High dose" 1 `" "Low dose +"  "fact-check tweets" "', grid angle(0) glcolor(gs3))) (rspike ll ul ordernum if trumpdisapprove == 1, horizontal lcolor(red*2))

graph export "Figures/trump_robustness_check_approval.png", replace
****************************************************************************************************

restore

matrix mat1 = J(30,1,.)

*****************************************************************************************************************
*Table D1: Effect of exposure to voter fraud allegations on election confidence by feelings towards Trump
*****************************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.FT_trump, robust
est store A
matrix mat1[1,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.FT_trump]/_se[1.tweet4#c.FT_trump]))
matrix mat1[2,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.FT_trump]/_se[1.tweet8#c.FT_trump]))
matrix mat1[3,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.FT_trump]/_se[1.tweetcorrect#c.FT_trump]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

***********************************************************************************************************************
*Table D2: Effect of exposure to voter fraud allegations on election confidence by feelings towards Trump (tercile indicators)
***********************************************************************************************************************
xtile trumpterc=FT_trump,nq(3)
reg zconf_trust tweet4##tweet8##tweetcorrect##i.trumpterc, robust
est store A
matrix mat1[4,1]=2*ttail(e(df_r),abs(_b[1.tweet4#2.trumpterc]/_se[1.tweet4#2.trumpterc]))
matrix mat1[5,1]=2*ttail(e(df_r),abs(_b[1.tweet8#2.trumpterc]/_se[1.tweet8#2.trumpterc]))
matrix mat1[6,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#2.trumpterc]/_se[1.tweetcorrect#2.trumpterc]))
matrix mat1[7,1]=2*ttail(e(df_r),abs(_b[1.tweet4#3.trumpterc]/_se[1.tweet4#3.trumpterc]))
matrix mat1[8,1]=2*ttail(e(df_r),abs(_b[1.tweet8#3.trumpterc]/_se[1.tweet8#3.trumpterc]))
matrix mat1[9,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#3.trumpterc]/_se[1.tweetcorrect#3.trumpterc]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

****************************************************************************************************
*Table D3: Effect of exposure to voter fraud allegations on election confidence by media feelings
****************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.FT_media, robust
est store A
matrix mat1[10,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.FT_media]/_se[1.tweet4#c.FT_media]))
matrix mat1[11,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.FT_media]/_se[1.tweet8#c.FT_media]))
matrix mat1[12,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.FT_media]/_se[1.tweetcorrect#c.FT_media]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

****************************************************************************************************
*Table D4: Effect of exposure to voter fraud allegations on election confidence by media trust
****************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.massmedia_trust, robust
est store A
matrix mat1[13,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.massmedia_trust]/_se[1.tweet4#c.massmedia_trust]))
matrix mat1[14,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.massmedia_trust]/_se[1.tweet8#c.massmedia_trust]))
matrix mat1[15,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.massmedia_trust]/_se[1.tweetcorrect#c.massmedia_trust]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

***************************************************************************************************************
*Table D5: Effect of exposure to voter fraud allegations on election confidence by conspiracy predispositions
***************************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.conspiracy_mean, robust
est store A
matrix mat1[16,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.conspiracy_mean]/_se[1.tweet4#c.conspiracy_mean]))
matrix mat1[17,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.conspiracy_mean]/_se[1.tweet8#c.conspiracy_mean]))
matrix mat1[18,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.conspiracy_mean]/_se[1.tweetcorrect#c.conspiracy_mean]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

****************************************************************************************************
*Table D6: Effect of exposure to voter fraud allegations on election confidence by political interest
****************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.polint, robust
est store A
matrix mat1[19,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.polint]/_se[1.tweet4#c.polint]))
matrix mat1[20,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.polint]/_se[1.tweet8#c.polint]))
matrix mat1[21,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.polint]/_se[1.tweetcorrect#c.polint]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

****************************************************************************************************
*Table D7: Effect of exposure to voter fraud allegations on election confidence by political knowledge
****************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.polknow, robust
est store A
matrix mat1[22,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.polknow]/_se[1.tweet4#c.polknow]))
matrix mat1[23,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.polknow]/_se[1.tweet8#c.polknow]))
matrix mat1[24,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.polknow]/_se[1.tweetcorrect#c.polknow]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

**********************************************************************************************************************************
*Table D8: Effect of exposure to voter fraud allegations on election confidence by pre-treatmentexposure to untrustworthy websites
**********************************************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.totalfakebinary18_presurvey , robust
est store A
matrix mat1[25,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.totalfakebinary18_presurvey ]/_se[1.tweet4#c.totalfakebinary18_presurvey ]))
matrix mat1[26,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.totalfakebinary18_presurvey ]/_se[1.tweet8#c.totalfakebinary18_presurvey ]))
matrix mat1[27,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.totalfakebinary18_presurvey ]/_se[1.tweetcorrect#c.totalfakebinary18_presurvey ]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

*****************************************************************************************************************************
*Table D9: Effect of exposure to voter fraud allegations on election confidence by pre-treatmentvisits to fact checking sites
*****************************************************************************************************************************
reg zconf_trust tweet4##tweet8##tweetcorrect##c.totalfcbinary_presurvey  , robust
est store A
matrix mat1[28,1]=2*ttail(e(df_r),abs(_b[1.tweet4#c.totalfcbinary_presurvey  ]/_se[1.tweet4#c.totalfcbinary_presurvey  ]))
matrix mat1[29,1]=2*ttail(e(df_r),abs(_b[1.tweet8#c.totalfcbinary_presurvey  ]/_se[1.tweet8#c.totalfcbinary_presurvey  ]))
matrix mat1[30,1]=2*ttail(e(df_r),abs(_b[1.tweetcorrect#c.totalfcbinary_presurvey  ]/_se[1.tweetcorrect#c.totalfcbinary_presurvey  ]))
estout A, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)
*****************************************************************************************************************

preserve
svmat mat1
drop if mat1==.
keep mat1 
rename mat1 p
multproc, method(simes)
list p
restore

reg demo_sup1 tweet4 tweet8 tweetcorrect dem_leaners independents dem_leanersXtweet4 dem_leanersXtweet8 dem_leanersXtweetcorrect independentsXtweet4 independentsXtweet8 independentsXtweetcorrect, robust
est store A

lincom (tweet8+dem_leanersXtweet8)-(tweet4+dem_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+dem_leanersXtweet8)-(tweet4+dem_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+dem_leanersXtweetcorrect)-(tweet4+dem_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+dem_leanersXtweetcorrect)-(tweet4+dem_leanersXtweet4))-(tweetcorrect-tweet4)

reg demo_sup2 tweet4 tweet8 tweetcorrect dem_leaners independents dem_leanersXtweet4 dem_leanersXtweet8 dem_leanersXtweetcorrect independentsXtweet4 independentsXtweet8 independentsXtweetcorrect, robust
est store B

********************************************************************************************
*Table C5: Effect of exposure to voter fraud allegations on support for democracy by party
********************************************************************************************
estout A B, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)

lincom (tweet8+dem_leanersXtweet8)-(tweet4+dem_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+dem_leanersXtweet8)-(tweet4+dem_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+dem_leanersXtweetcorrect)-(tweet4+dem_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+dem_leanersXtweetcorrect)-(tweet4+dem_leanersXtweet4))-(tweetcorrect-tweet4)
********************************************************************************************

reg demo_sup1 tweet4 tweet8 tweetcorrect trumpdisapprove tweet4Xtrumpdisapprove tweet8Xtrumpdisapprove tweetcorrectXtrumpdisapprove, robust
est store A
lincom (tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweet8-tweet4
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)
return list
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)

lincom (tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweetcorrect-tweet4)

reg demo_sup2 tweet4 tweet8 tweetcorrect trumpdisapprove tweet4Xtrumpdisapprove tweet8Xtrumpdisapprove tweetcorrectXtrumpdisapprove, robust
est store B
lincom (tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweet8-tweet4
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)
return list
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)

lincom (tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweetcorrect-tweet4)

****************************************************************************************************
*Table C6: Effect of exposure to voter fraud allegations on support for democracy by Trump approval
****************************************************************************************************
estout A B, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)

reg demo_sup1 tweet4 tweet8 tweetcorrect trumpdisapprove tweet4Xtrumpdisapprove tweet8Xtrumpdisapprove tweetcorrectXtrumpdisapprove, robust
est store A
lincom (tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweet8-tweet4
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)
return list
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)

lincom (tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweetcorrect-tweet4)

reg demo_sup2 tweet4 tweet8 tweetcorrect trumpdisapprove tweet4Xtrumpdisapprove tweet8Xtrumpdisapprove tweetcorrectXtrumpdisapprove, robust
est store B
lincom (tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweet8-tweet4
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)
return list
lincom ((tweet8+tweet8Xtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweet8-tweet4)

lincom (tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+tweetcorrectXtrumpdisapprove)-(tweet4+tweet4Xtrumpdisapprove))-(tweetcorrect-tweet4)
****************************************************************************************************

reg demo_sup1 tweet4 tweet8 tweetcorrect repub_leaners repub_leanersXtweet4 repub_leanersXtweet8 repub_leanersXtweetcorrect, robust
est store A
lincom (tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4))-(tweetcorrect-tweet4)

reg demo_sup2 tweet4 tweet8 tweetcorrect repub_leaners repub_leanersXtweet4 repub_leanersXtweet8 repub_leanersXtweetcorrect, robust
est store B
lincom (tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4))-(tweetcorrect-tweet4)
****************************************************************************************************

************************************************************************************************************************************
*Table C7: Effect of exposure to voter fraud allegations on support for democracy by party (Republicans vs. Democrats/Independents)
************************************************************************************************************************************
estout A B, label collabels("") cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) stats(N, fmt(%9.0f) labels("N")) starlevels(* 0.05 ** 0.01 *** 0.005) style(tex)

reg demo_sup1 tweet4 tweet8 tweetcorrect repub_leaners repub_leanersXtweet4 repub_leanersXtweet8 repub_leanersXtweetcorrect, robust
est store A
lincom (tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4))-(tweetcorrect-tweet4)

reg demo_sup2 tweet4 tweet8 tweetcorrect repub_leaners repub_leanersXtweet4 repub_leanersXtweet8 repub_leanersXtweetcorrect, robust
est store B
lincom (tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4)
lincom tweet8-tweet4
lincom ((tweet8+repub_leanersXtweet8)-(tweet4+repub_leanersXtweet4))-(tweet8-tweet4)

lincom (tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4)
lincom tweetcorrect-tweet4
lincom ((tweetcorrect+repub_leanersXtweetcorrect)-(tweet4+repub_leanersXtweet4))-(tweetcorrect-tweet4)
************************************************************************************************************************************

************************************************************************************
**Table A3 part 2: Missing data by treatment - outcome measures (continued from above)
************************************************************************************
/*note: for tweet_treat_w2, ==1 -> low dose, ==2 -> high dose, ==3 -> low dose+factcheck, ==4 -> control, also, if . is not observed in list of unique values, there are no missing values.*/
set more on /*otherwise output is too long to fully see*/
bysort tweet_treat_w2: tab zconf_trust if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab demo_sup1 if tweet_treat_w2!=., missing
bysort tweet_treat_w2: tab demo_sup2 if tweet_treat_w2!=., missing
************************************************************************************
set more off
