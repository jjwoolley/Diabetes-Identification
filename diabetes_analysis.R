# nhanes diabetes identification

# diabetes questionnaire every year from 2000 to present (8 cycles)

# also include demographic information
# age, gender, race, height, weight, bmi, etc

# Diabetes diagnosis (one of below)
#   - fasting glucose test: blood sugar level >= 126 mg/dl (7mmol/l)
#   - random glucose test: blood sugar level >= 200
#   - A1c test: >= 6.5%


##### LOADING DATA #####

library("haven")
library("tidyverse")
library("survey")
library("tidymodels")

# load diabetes questionnaire data
diabetes_00 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DIQ.XPT")
diabetes_02 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DIQ_B.XPT")
diabetes_04 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DIQ_C.XPT")
diabetes_06 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DIQ_D.XPT")
diabetes_08 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DIQ_E.XPT")
diabetes_10 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DIQ_F.XPT")
diabetes_12 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.XPT")
diabetes_14 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.XPT")
diabetes_16 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT")
diabetes_18 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT")

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


diabetes_all <- full_join(diabetes_00, diabetes_02) %>%
  full_join(diabetes_04) %>%
  full_join(diabetes_06) %>%
  full_join(diabetes_08) %>%
  full_join(diabetes_10) %>%
  full_join(diabetes_12) %>%
  full_join(diabetes_14) %>%
  full_join(diabetes_16) %>%
  full_join(diabetes_18)

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


nhanes_all <- full_join(demo_all, diabetes_all, by = "SEQN") %>%
  full_join(body_all, by = "SEQN") %>%
  full_join(labs_all, by = "SEQN") %>%
  filter(SEQN %in% diabetes_all$SEQN)
# fasting variable LBXGLU (fasting glucose level)
# only asked in individuals 12 - 150 years who were examined in morning session


y <- filter(nhanes_all, !is.na(WTSAF2YR))



# need to create new weights for diabetes/body weight








##### CLEANING DATA #####

# Create new diabetes variable where answers that are not yes or no are eliminated
nhanes_full <- nhanes_all %>%
  rename(fasting_glucose = LBXGLU,
         Age = RIDAGEYR,
         Weight_kg = BMXWT,
         Height_cm = BMXHT) %>%
  mutate(told_diabetes = ifelse(DIQ010 == 1 , 1,
                                ifelse(DIQ010 == 2 | DIQ010 == 3, 0, NA)),
         Male = ifelse(RIAGENDR == 1, 1, 0),
         Race = ifelse(RIDRETH1 == 1 | RIDRETH1 == 2, "Hispanic",
                       ifelse(RIDRETH1 == 3, "White",
                              ifelse(RIDRETH1 == 4, "Black",
                                     ifelse(RIDRETH1 == 5, "Other Race", NA)))),
         Child = ifelse(Age < 18, "Yes", "No"),
         BMI = Weight_kg / (Height_cm / 100)^2,
         hba1c_diabetes = ifelse(fasting_glucose >= 126, 1, 0),
         Cycle = 2000 + (SDDSRVYR - 1) * 2)



nhanes_labs <- filter(nhanes_full, !is.na(WTSAF2YR))


##### EXPLORATORY ANALYSIS #####
all_design <- svydesign(ids =~ SEQN, weights =~ WTINT2YR, data = nhanes_full)
labs_design <- svydesign(ids =~ SEQN, weights =~ WTSAF2YR, data = nhanes_labs)


# histogram of all individuals with diabetes' BMI
nhanes_labs %>%
  filter(hba1c_diabetes == 1, Cycle == 2018) %>%
  ggplot(aes(BMI, weight = WTSAF2YR)) + 
  geom_histogram(binwidth = 3, color = "darkblue", fill = "lightblue") +
  geom_vline(aes(xintercept = 30), color = "red", show.legend = T) + 
  scale_x_continuous(breaks = seq(10, 90, 5)) + 
  scale_y_continuous(breaks = seq(0, 3000000, 1000000), labels = scales::comma) +
  labs(title = "BMI histogram of Individuals with Diabetes", x = "Body Mass Index", y = "Count") +
  theme_bw()


# plot on proportion of adults with diabetes over time
prevalence_over_time <- nhanes_labs %>%
  group_by(Cycle) %>%
  summarize(told_mean = weighted.mean(told_diabetes, w = WTSAF2YR, na.rm = T),
            hba1c_mean = weighted.mean(hba1c_diabetes, w = WTSAF2YR, na.rm = T)) %>%
  pivot_longer(cols = c(told_mean, hba1c_mean), names_to = "stat", values_to = "prevalence")

ggplot(data = prevalence_over_time, aes(x = Cycle, y = prevalence, color = stat)) + 
  geom_line() + 
  labs(title = "Diabetes Prevalence Over Time in US Adults", x = "Cycle", y = "Prevalence") +
  theme_bw()

# table showing individuals told they have diabetes versus having diabetes (hba1c)



svymean(~told_diabetes, all_design, na.rm = T)
mean(nhanes_full$told_diabetes, na.rm = T)

svymean(~told_diabetes, labs_design, na.rm = T)
mean(nhanes_labs$told_diabetes, na.rm = T)

x <- lm(told_diabetes ~ (BMI > 30) + Age + Race + Male, data = nhanes_full)
summary(x)


y <- lm(hba1c_diabetes ~ (BMI > 30) + Age + Race + Male + told_diabetes, data = nhanes_full)
summary(y)


