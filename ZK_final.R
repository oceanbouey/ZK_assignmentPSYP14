library(psych) # for describe
library(tidyverse) # for tidy code
library(lm.beta) # for lm.beta
library(gridExtra)

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
data_sample_1

data_sample_1 %>%summary()
describe(data_sample_1)
view(data_sample_1)
str(data_sample_1)

#test graphs
data_sample_1 %>%ggplot() + aes(x = pain) + geom_histogram()
data_sample_1 %>%ggplot() + aes(x = pain) + geom_bar()
#this graph revealed that there is an outlier in the pain category. ID88 listed 55 for pain, will be removed from the dataset
data_sample_1 %>%ggplot() + aes(x = sex) + geom_bar()
data_sample_1%>%ggplot() + aes(x = pain_cat) + geom_histogram()
data_sample_1%>%ggplot() + aes(x = age) + geom_histogram()
data_sample_1%>%ggplot() + aes(x = STAI_trait) + geom_histogram()
#this graph revealed an outlier, ID34 will be removed from the dataset 
data_sample_1%>%ggplot() + aes(x = cortisol_serum) + geom_histogram()
data_sample_1%>%ggplot() + aes(x = cortisol_saliva) + geom_histogram()
data_sample_1%>%ggplot() + aes(x = mindfulness) + geom_histogram()
data_sample_1%>%ggplot() + aes(x = weight) + geom_histogram()
data_sample_1%>%ggplot() + aes(x = IQ) + geom_histogram()
data_sample_1%>%ggplot() + aes(x = household_income) + geom_histogram()

#removing outliers from data
edited_data <- data_sample_1[-c(34,88),]
view(edited_data)
#rerun graphs
edited_data %>%ggplot() + aes(x = pain) + geom_histogram()
edited_data %>%ggplot() + aes(x = pain) + geom_bar()
edited_data %>%ggplot() + aes(x = sex) + geom_bar()
edited_data%>%ggplot() + aes(x = pain_cat) + geom_histogram()
edited_data%>%ggplot() + aes(x = age) + geom_histogram()
edited_data%>%ggplot() + aes(x = STAI_trait) + geom_histogram()
edited_data%>%ggplot() + aes(x = cortisol_serum) + geom_histogram()
edited_data%>%ggplot() + aes(x = cortisol_saliva) + geom_histogram()
edited_data%>%ggplot() + aes(x = mindfulness) + geom_histogram()
edited_data%>%ggplot() + aes(x = weight) + geom_histogram()
edited_data%>%ggplot() + aes(x = IQ) + geom_histogram()
edited_data%>%ggplot() + aes(x = household_income) + geom_histogram()

#rerun edited data
describe(edited_data) #the skew values now fall within the normality range of -1:1
str(edited_data)

#predictive values
mod1 <- lm(pain ~ age + sex, data = edited_data)
mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data= edited_data)

#model comparisons
anova(mod1)
anova(mod2)
anova(mod1, mod2)
AIC(mod1)
AIC(mod2)
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared
summary(mod1)
summary(mod2)
#linear regression graphs


mod1 %>%
  ggplot() + aes(x = age, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)

mod2 %>%
  ggplot() + aes(x = STAI_trait, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)
mod2 %>%
  ggplot() + aes(x = pain_cat, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)
mod2 %>%
  ggplot() + aes(x = cortisol_serum, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)
mod2 %>%
  ggplot() + aes(x = cortisol_saliva, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)

mod2 %>%
  ggplot() + aes(x = mindfulness, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)

mod2 %>%
  residualPlots()

library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(lmtest)
bptest(mod1)
bptest(mod2)
anova(mod1, mod2)
anova(mod1)
anova(mod2)
summary(mod1)
summary(mod2)
#cook's distance: according to the rule of anything being extreme when it is over 1, these graphs display that there are no extreme values in mod1 and mod2
#however, on the cook's distances graph there are 3 data points that are higher than the rest: 47, 74, and 86. When checking these datapoints, I don't think any of their inputs are extreme enough to remove from the overall data sample
#low income values: ID 1, 56
mod1 %>%
  plot(which = 4)
mod2 %>%
  plot(which = 4)
mod1 %>%
  plot(which = 5)
mod2 %>%
  plot(which = 5)
#normality plot
mod2 %>%
  plot(which = 2)
#Homoscedasticity 
mod2 %>%
  ncvTest() # NCV test
mod1 %>%
  plot(which=3)
mod2 %>%
  plot(which=3)

summary(mod2)
#regression equation 
#pain = -.008 + (-0.02*age) + (0.15*sex) + (0.14*pain_cat) + (0.17*cortisol_serum) + (0.0.49*cortisol_saliva) + (-0.25*mindfulness) + (-0.03*STAI_trait)#
AIC(mod1)
AIC(mod2)


#assignment part 2
mod1.2 <- lm(pain~age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum +weight + IQ + household_income, data= edited_data)

#test regression/graphs
mod1.2 %>%
  ggplot() + aes(x = mindfulness, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)
mod1.2 %>%
  ggplot() + aes(x = weight, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)
mod1.2 %>%
  ggplot() + aes(x = IQ, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)
mod1.2 %>%
  ggplot() + aes(x = household_income, y = pain, color=sex) + geom_point() +
  geom_smooth(method = "lm", se = F)

#model diagnostics
summary(mod1.2)
bptest(mod1.2)
anova(mod1.2)
str(mod1.2)
describe(edited_data)

#packages
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(psych) # for pairs.panels
library(tidyverse) # for tidy code and ggplot\t
library(lattice) # for qqmath
#normality
mod1.2 %>%
  plot(which = 2) #(note 104, 85, 86)


residuals_mod1.2 = enframe(residuals(mod1.2))
#bar graph
residuals_mod1.2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(mod1.2))

mod1.2 %>%      
  residualPlots()


#linearity plot
plot(mod1.2, arg = "pearson")
mod1.2 %>%
  ggplot() + aes(x = age, y = pain) + geom_point()
#Homoscedasticity
plot(mod1.2, arg = "pearson")

mod1.2 %>%
  plot(which = 3) #note 104, 85, 86

mod1.2 %>%
  plot(which = 5) #for cooks distance (note 85, 86, 47)

mod1.2 %>%
  ncvTest() # NCV test

mod1.2 %>%
  bptest() # Breush-Pagan test

mod1.2 %>%
  vif()





#backward regression

backward_code = step(mod1.2, direction = "backward")

backward_code_final <- lm(pain ~ age + mindfulness +  
                           cortisol_serum + pain_cat, data = edited_data)
backward_code_final
summary(backward_code_final)
anova(backward_code_final)
bptest(backward_code_final)



comparison_model <- lm(pain ~ age + sex  + cortisol_serum + cortisol_saliva
                       + mindfulness + pain_cat + STAI_trait, data = edited_data)

model_backward <- lm(pain ~ age + sex + STAI_trait + pain_cat +
                       cortisol_serum + mindfulness + 
                       weight + IQ + household_income, data = edited_data) 
model_backward %>% 	      
  plot(which = 5)	

model_backward %>% 	     
  plot(which = 4)

model_backward %>%  
  plot(which = 2)

edited_data %>%
  slice(c(104,85,86,47))

model_backward %>%      
  residualPlots()

anova(backward_code_final, comparison_model)
summary(backward_code_final)
anova(backward_code_final)
summary(model_backward)
summary(comparison_model)
r.squaredGLMM(backward_code_final)
AIC(model_backward)
AIC(backward_code_final)

theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data= edited_data)

anova(theory_based_model, backward_code_final)
AIC(theory_based_model, backward_code_final)

home_sample_2= read.csv("https://tinyurl.com/87v6emky")
summary(home_sample_2)
str(home_sample_2)
view(home_sample_2)

prediction_1 <- predict(comparison_model, home_sample_2)
view(prediction_1)


structure_1 = cbind(home_sample_2, prediction_1)
view(structure_1)

residual = sum((home_sample_2$pain - predict(comparison_model))^2)
residual

structure_2 <- predict(backward_code_final, home_sample_2)
structure_2

prediction_2 = cbind(home_sample_2, structure_2)
view(prediction_2)


RSS = sum((home_sample_2$pain - predict(backward_code_final))^2)
RSS


summary(backward_code_final)
#regression equation for backward model
#pain = 1.28 + (-.04) *age + (-.27) * mindfulness + .53 * corisol_serum + .11 * pain_cat

#assignment part 3

data3 = read.csv("https://tinyurl.com/b385chpu")
data4= read.csv("https://tinyurl.com/4f8thztv")

summary(data3)
summary(data4)
str(data3)
str(data4)
data3
data3 = data3 %>% 	
  mutate(hospital = factor(hospital))

summary(data3)
str(data3)
data3 %>% 		
  ggplot() +		
  aes(y = pain, x = age) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data3 %>% 		
  ggplot() +		
  aes(y = pain, x = sex) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)	

# ID25 listed sex as woman instead of female, will remove from dataset
data3 <- data3 %>%
  slice(-c(25))
view(data3)

data3 %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_saliva) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

mod3.1 <- lm(pain ~ age + sex  + pain_cat + cortisol_serum +
                cortisol_saliva + STAI_trait + mindfulness, data = data3)
summary(mod3.1)
mod3.1

random_mod = lmer(pain ~ age + sex + pain_cat + cortisol_serum +
                      cortisol_saliva + mindfulness + STAI_trait +
                      (1|hospital), data = data3)

random_4 = lmer(pain ~ age + sex + pain_cat + cortisol_serum +
                    cortisol_saliva + mindfulness + STAI_trait +
                    (1|hospital), data = data4)
random_mod
summary(random_mod)
plot(random_mod)
qqnorm(resid(random_mod))
qqline(resid(random_mod))

confint(random_mod)
confidence_level = confint(random_mod)

#RSS random intercept
sum(residuals(random_mod)^2)
#RSS regular linear regression
sum(residuals(mod3.1)^2)

mixed_model = lmer(pain ~ cortisol_serum +
                       (cortisol_serum|hospital), data = data3)

summary(mixed_model)

sum(residuals(mixed_model)^2)

cAIC(random_mod)$caic 

cAIC(mixed_model)$caic

anova(random_mod, mixed_model)

r.squaredGLMM(random_mod)
r.squaredGLMM(random_4)


r2beta(random_mod, method = "nsj", data = data4)
prediction_random <- predict(random_mod, data4, allow.new.levels=T)

predicted_final = cbind(data4, prediction_random)
view(predicted_final)
summary(prediction_random)

RSS_1 = sum((data4$pain - prediction_random)^2)
RSS_1
mean_mod <- lmer(pain ~ 1 + (1|hospital), data = data4)
TSS_1 = sum((data4$pain - predict(mean_mod))^2)
TSS_1

variance_explaination <- 1-(RSS_1/TSS_1)
variance_explaination

data3 <- data3 %>%
  mutate(pred_int = predict(random_mod), pred_slope = predict(mixed_model))

data3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "purple",
                                                          aes(y = pred_int, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
data3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "purple",
                                                          aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

