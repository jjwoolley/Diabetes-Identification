# nhanes dm identification

# dm questionnaire every year from 2000 to present (8 cycles)

# also include demographic information
# age, gender, race, height, weight, bmi, etc

# dm diagnosis (one of below)
#   - told_dm: told by previous doctor
#         - asked to everyone >= 1 years old
#         - cycle 2000 and up
#   - fast_dm: fasting glucose test: blood sugar level >= 126 mg/dl (7mmol/l)
#         - asked to individuals in morning session >= 12 years old
#         - cycle 2012 and up
#   - hba1c_dm: A1c test: >= 6.5%
#         - asked to individuals >= 12 years old
#         - cycle 2000 and up
#   - rx_dm: take antidiabetic medication
#         - asked to everyone
#         - cycle 2014 and up

# hba1c is the gold standard


##### LOADING DATA #####
library("haven")
library("tidyverse")
library("survey")
library("tidymodels")
library("mice")

# load dm questionnaire data
dm_00 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DIQ.XPT")
dm_02 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DIQ_B.XPT")
dm_04 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DIQ_C.XPT")
dm_06 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DIQ_D.XPT")
dm_08 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DIQ_E.XPT")
dm_10 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DIQ_F.XPT")
dm_12 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.XPT")
dm_14 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.XPT")
dm_16 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT")
dm_18 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT")

# load demographic data
demo_00 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT")
demo_02 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT")
demo_04 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT")
demo_06 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT")
demo_08 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT")
demo_10 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT")
demo_12 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT")
demo_14 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT")
demo_16 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT")
demo_18 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT")


# load body measurement data
body_00 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT")
body_02 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT")
body_04 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT")
body_06 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT")
body_08 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT")
body_10 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT")
body_12 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT")
body_14 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT")
body_16 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT")
body_18 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT")


# load fasting glucose data
# only done on individuals 12 and older and not on every nhanes patient
labs_12 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/GLU_G.XPT")
labs_14 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/GLU_H.XPT")
labs_16 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/GLU_I.XPT")
labs_18 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/GLU_J.XPT")


# prescription medication data
rx_14 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/RXQ_RX_H.XPT")
rx_16 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/RXQ_RX_I.XPT")
rx_18 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/RXQ_RX_J.XPT")


# load glycohemoglobin data
# only done on individuals 12 and older
glycohemoglobin_00 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB10.XPT")
glycohemoglobin_02 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L10_B.XPT")
glycohemoglobin_04 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10_C.XPT")
glycohemoglobin_06 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GHB_D.XPT")
glycohemoglobin_08 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/GHB_E.XPT")
glycohemoglobin_10 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/GHB_F.XPT")
glycohemoglobin_12 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/GHB_G.XPT")
glycohemoglobin_14 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/GHB_H.XPT")
glycohemoglobin_16 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/GHB_I.XPT")
glycohemoglobin_18 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/GHB_J.XPT")


dm_all <- full_join(dm_00, dm_02) %>%
  full_join(dm_04) %>%
  full_join(dm_06) %>%
  full_join(dm_08) %>%
  full_join(dm_10) %>%
  full_join(dm_12) %>%
  full_join(dm_14) %>%
  full_join(dm_16) %>%
  full_join(dm_18)

demo_all <- full_join(demo_00, demo_02) %>%
  full_join(demo_04) %>%
  full_join(demo_06) %>%
  full_join(demo_08) %>%
  full_join(demo_10) %>%
  full_join(demo_12) %>%
  full_join(demo_14) %>%
  full_join(demo_16) %>%
  full_join(demo_18)

body_all <- full_join(body_00, body_02) %>%
  full_join(body_04) %>%
  full_join(body_06) %>%
  full_join(body_08) %>%
  full_join(body_10) %>%
  full_join(body_12) %>%
  full_join(body_14) %>%
  full_join(body_16) %>%
  full_join(body_18)

labs_all <- full_join(labs_12, labs_14) %>%
  full_join(labs_16) %>%
  full_join(labs_18)

glycohemoglobin_all <- full_join(glycohemoglobin_00, glycohemoglobin_02) %>%
  full_join(glycohemoglobin_04) %>%
  full_join(glycohemoglobin_06) %>%
  full_join(glycohemoglobin_08) %>%
  full_join(glycohemoglobin_10) %>%
  full_join(glycohemoglobin_12) %>%
  full_join(glycohemoglobin_14) %>%
  full_join(glycohemoglobin_16) %>%
  full_join(glycohemoglobin_18)

rx_all <- full_join(rx_14, rx_16) %>%
  full_join(rx_18)

nhanes_all <- full_join(demo_all, dm_all, by = "SEQN") %>%
  full_join(body_all, by = "SEQN") %>%
  full_join(labs_all, by = "SEQN") %>%
  full_join(rx_all, by = "SEQN") %>%
  full_join(glycohemoglobin_all, by = "SEQN") %>%
  filter(SEQN %in% dm_all$SEQN)
# fasting variable LBXGLU (fasting glucose level)
# only asked in individuals 12 - 150 years who were examined in morning session



# need to create new weights for dm/body weight








##### CLEANING DATA #####

# Create new dm variable where answers that are not yes or no are eliminated
# tot_dm will = 1 if any of them are 1, but will = NA if any of them are NA and none are 1
nhanes_full <- nhanes_all %>%
  rename(fasting_glucose = LBXGLU,
         Age = RIDAGEYR,
         Weight_kg = BMXWT,
         Height_cm = BMXHT) %>%
  mutate(told_dm = ifelse(DIQ010 == 1 , 1,
                                ifelse(DIQ010 == 2 | DIQ010 == 3, 0, NA)),
         Male = ifelse(RIAGENDR == 1, 1, 0),
         Race = ifelse(RIDRETH1 == 1 | RIDRETH1 == 2, "Hispanic",
                       ifelse(RIDRETH1 == 3, "White",
                              ifelse(RIDRETH1 == 4, "Black",
                                     ifelse(RIDRETH1 == 5, "Other Race", NA)))),
         Child = ifelse(Age < 18, "Yes", "No"),
         BMI = Weight_kg / (Height_cm / 100)^2,
         fast_dm = ifelse(fasting_glucose >= 126, 1, 0),
         hba1c_dm = ifelse(LBXGH >= 6.5, 1, 0),
         rx_dm = ifelse(str_detect(RXDRSC1, "^E10") | str_detect(RXDRSC1, "^E11"), 1, 0),
         Cycle = 2000 + (SDDSRVYR - 1) * 2,
         tot_dm = ifelse(told_dm == 1 | hba1c_dm == 1 | rx_dm == 1 | fast_dm == 1, 1, 0)) %>%
  select(SEQN, Cycle, tot_dm, told_dm, fast_dm, fasting_glucose, hba1c_dm, LBXGH, rx_dm, RXDRSC1, 
         Age, Male, Race, Child, BMI, Weight_kg, Height_cm, WTINT2YR, WTMEC2YR, WTSAF2YR)





nhanes_full %>%
  select(told_dm, fast_dm, hba1c_dm, rx_dm, WTINT2YR, WTMEC2YR, WTSAF2YR) %>%
  md.pattern()

# missing data analysis
# we have TINT and TMEC weightings for everyone
# we have all weights for individuals who responded to the fast_dm section
# use MEC


nhanes_labs <- filter(nhanes_full, !is.na(WTMEC2YR), Cycle >= 2014, Age >= 12)

nhanes_labs %>%
    filter(Cycle >= 2014, Age >= 12) %>%
    select(told_dm, fast_dm, hba1c_dm, rx_dm, WTMEC2YR) %>%
  md.pattern()


xx <- nhanes_labs %>%
  filter(Cycle >= 2014, Age >= 12) %>%
  select(told_dm, fast_dm, hba1c_dm, rx_dm, WTMEC2YR)

# summary
# there are 50,000 observations from 2014, 2016, 2018 cycles in individuals 12 and older
# fasting is only done in half of them


##### EXPLORATORY ANALYSIS #####
all_design <- svydesign(ids =~ SEQN, weights =~ WTINT2YR, data = nhanes_full)
labs_design <- svydesign(ids =~ SEQN, weights =~ WTSAF2YR, data = nhanes_labs)


# histogram of all individuals with dm' BMI
# line at Overweight
nhanes_labs %>%
  filter(hba1c_dm == 1, Cycle == 2018) %>%
  ggplot(aes(BMI, weight = WTSAF2YR)) + 
  geom_histogram(binwidth = 3, color = "darkblue", fill = "lightblue") +
  geom_vline(aes(xintercept = 30), color = "red", show.legend = T) + 
  scale_x_continuous(breaks = seq(10, 90, 5)) + 
  scale_y_continuous(breaks = seq(0, 3000000, 1000000), labels = scales::comma) +
  labs(title = "BMI histogram of Individuals with dm", x = "Body Mass Index", y = "Count") +
  theme_bw()


# plot on proportion of adults with dm over time
prevalence_over_time <- nhanes_labs %>%
  group_by(Cycle) %>%
  summarize(told_mean = weighted.mean(told_dm, w = WTMEC2YR, na.rm = T),
            hba1c_mean = weighted.mean(hba1c_dm, w = WTMEC2YR, na.rm = T)) %>%
  pivot_longer(cols = c(told_mean, hba1c_mean), names_to = "stat", values_to = "prevalence")

ggplot(data = prevalence_over_time, aes(x = Cycle, y = prevalence, color = stat)) + 
  geom_line() + 
  labs(title = "dm Prevalence Over Time in US Adults", x = "Cycle", y = "Prevalence") +
  theme_bw()

# table showing individuals told they have dm versus having dm (hba1c)



svymean(~told_dm, all_design, na.rm = T)
mean(nhanes_full$told_dm, na.rm = T)

svymean(~told_dm, labs_design, na.rm = T)
mean(nhanes_labs$told_dm, na.rm = T)

x <- lm(told_dm ~ (BMI > 30) + Age + Race + Male, data = nhanes_full)
summary(x)


y <- lm(hba1c_dm ~ (BMI > 30) + Age + Race + Male + told_dm, data = nhanes_full)
summary(y)

z <- lm(fast_dm ~ (BMI > 30) + Age + Race + Male + told_dm, data = nhanes_full)
summary(z)


# to do
# 1) determine which dm to include in each analysis
# 2) which set of weights to use in each analysis


# Find correlation matrices between all dm
# hba1c is the ultimate diagnoses