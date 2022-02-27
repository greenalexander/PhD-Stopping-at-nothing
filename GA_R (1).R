# Load necessary libraries #
library(lme4)
library(psych)
library(simr)
library(data.table)
library(rcompanion)
library(Rmisc)
library(ggplot2)


# TRIAL EXCLUSION #
# Remove trials in which exclusion_reason /= 'na' #
GA_Data <- GA_Data[ which(GA_Data$exclusion_reason=='na'), ]

# Remove familiarisation trials (trial_num<6) #
GA_Data <- GA_Data [which(GA_Data$trial_num > 6), ]

# Remove participants who have completed fewer than 2 trials per condition #
GA_Data <- GA_Data[-which(GA_Data$id==1385), ]
GA_Data <- GA_Data[-which(GA_Data$id==1679), ]
GA_Data <- GA_Data[-which(GA_Data$id==1501), ]
GA_Data <- GA_Data[-which(GA_Data$id==2283), ]

# Remove empty columns #
GA_Data <- GA_Data[ , -c(14:21)]


# FORMAT DATA #
GA_Data$condition <- as.factor(GA_Data$condition)
GA_Data$helping_location <- as.factor(GA_Data$helping_location)
GA_Data$helping_type <- as.factor(GA_Data$helping_type)
GA_Data$response_type <- as.factor(GA_Data$response_type)
GA_Data$id <- as.factor(GA_Data$id)
GA_Data$sex <- as.factor(GA_Data$sex)


# DESCRIPTIVE STATISTICS #

# Describe data #
summary(GA_Data) 
# Describe by condition #
describeBy(GA_Data, GA_Data$condition)
# For the abandoned condition, children helped place the toy in E's initial location on 35% of trials #
# For the interrupted condition, children help place the toy in E's initial location on 63% of trials #
# This means that correct helping was at 65% in the abandoned condition and 63% in the interrupted condition #


# MAIN ANALYSIS #
# First, construct the full glmm model and the null model (which is identical, except without the fixed effect 'condition') #
glmmHTC <- glmer(helping_location ~ condition + (1+condition|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
glmmHT <- glmer(helping_location ~  (1+condition|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# The null model faces singularity #
# To address singularity, Singmann & Kellen (2019) suggest iteratively removing the most complex terms #
# We therefore remove the slope of participant from our random effects structure (for both full and null models) #
glmmHTC2 <- glmer(helping_location ~ condition + (1|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
glmmHT2 <- glmer(helping_location ~  (1|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# We now face no singularity in either model #

# To find whether the full model is a better fit for the data than the null model, we compare the models to each other using an anova #
anova(glmmHTC2, glmmHT2)
# Results indicate that the full model is a significantly better fit for the data than the null #

# Now we need the odds ratio (our effect size), which is just the exponent of the full model's fixed effect estimate #
summary(glmmHTC2)
fixef(glmmHTC2)
exp(fixef(glmmHTC2))


# SECONDARY ANALYSIS; DO PARTICIPANTS PERFORM BETTER IN ONE CONDITION COMPARED TO THE OTHER? #
# First, construct the full and null models # 
glmmRTC <- glmer(response_type ~ condition + (1+condition|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
glmmRT <- glmer(response_type ~ (1+condition|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# Again, singularity in the model -> remove slope of participant from our random effects #
glmmRTC2 <- glmer(response_type ~ condition + (1|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
glmmRT2 <- glmer(response_type ~ (1|id), data = GA_Data, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# No singularity in the latest models #

# Compare full and null model using the 'anova' function #
anova(glmmRTC2, glmmRT2)
# Results indicate that the full model is not a better fit than the null model #


# COMPARISON OF PERFORMANCE TO CHANCE #
# We need to turn the 'response_type' variable back into a numeric data format, so that we can calculate a mean per participant #
GA_Data$response_type <- as.numeric(GA_Data$response_type)
# (Turning factor data into numeric data sets the values to '1' and '2', instead of '0' and '1', so subtract 1 from all 'response_type' cells) #
GA_Data$response_type <- GA_Data$response_type-1

# We need to calculate each participant's 'response_type' (correct helping) rate per condition #
# First, form a dataset that is just the abandoned condition #
GA <- GA_Data[which(GA_Data$condition=='abandoned'), ]
# Then  form a new dataset that is just participant id and each participant's 'response_type' rate #
GAmean <- data.table(id=GA$id, response_type = GA$response_type)
GAmean <- GAmean[ , (response_type.Sum=mean(response_type)), by = id]
# Then add the variable names for our new dataset #
names(GAmean) <- c('id', 'response_type')

# Now do all of this for the control condition #
GI <- GA_Data[which(GA_Data$condition=='interrupted'), ]
GImean <- data.table(id=GI$id, response_type = GI$response_type)
GImean <- GImean[ , (response_type.Sum=mean(response_type)), by = id]
names(GImean) <- c('id', 'response_type')

# We can now do our analysis #
# First,the wilcox.test for each condition, where our alternative hypothesis is that the rate of correct helping is above chance #
wilcox.test(GAmean$response_type, mu = 0.5, alternative = "greater", exact = FALSE)
wilcox.test(GImean$response_type, mu = 0.5, alternative = "greater", exact=FALSE)
# We also need to report the median 'same location' rate per condition, as well as effect size 'r' #
median(GAmean$response_type)
median(GImean$response_type)
wilcoxonOneSampleR(GAmean$response_type, mu = 0.33, alternative = "greater")
wilcoxonOneSampleR(GImean$response_type, mu = 0.33, alternative = "greater")


