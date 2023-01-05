#The Impact of MPOWER Tobacco Control Policies on Tobacco Use Prevalence in African Countries

#SECTION A: DESCRIPTIVE ANALYSIS

#Import Cross-sectional dataset from clipboard (copied from MS excel spreadsheet 3)
csd <- read.table(file = "clipboard", 
                  sep = "\t", header=TRUE)
#1 Tobacco Use
mean(csd$Tobacco_prev_2018)  #Average tobacco use prevalence in 2018
mean(csd$Male_2018)  #Average male tobacco use prevalence in 2018
mean(csd$Female_2018) #Average female tobacco use prevalence in 2018
x<- mean(csd$Tobacco_prev_2018)-mean(csd$Tobacco_prev_2007) #Change in tobacco use prevalence from 2007 to 2018
(x/(mean(csd$Tobacco_prev_2007)))*100 #Percentage change in prevalence between 2007 and 2018

#2 MPOWER
mean(csd$MPOWER_2008) #Average MPOWER composite score in 2008
mean(csd$MPOWER_2018, na.rm = T) #Average MPOWER composite score in 2018
x<- mean(csd$MPOWER_2018, na.rm = T) - mean(csd$MPOWER_2008)
(x/(mean(csd$MPOWER_2008)))*100

#3 Average tobacco use prevalence by African Regions
mean(subset(csd$Tobacco_prev_2018, csd$UN_subregion == "Northern"))
mean(subset(csd$Tobacco_prev_2018, csd$UN_subregion == "Southern"))
mean(subset(csd$Tobacco_prev_2018, csd$UN_subregion == "Eastern"))
mean(subset(csd$Tobacco_prev_2018, csd$UN_subregion == "Western"))
mean(subset(csd$Tobacco_prev_2018, csd$UN_subregion == "Middle"))

#4 Graphical Analysis
library(ggplot2)
ggplot(data=csd) +
  aes(y=MPOWER_2008, x=Change_male) +
  geom_point() +
  geom_smooth(method=lm , color="blue", se=T)+ 
  labs(title = "Males (n=40 countries)",
         x = "Increase in tobacco use prevalence between 2007 and 2018",
         y = "MPOWER score in 2008")+
  theme( plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(data=csd) +
  aes(y=MPOWER_2008, x=Change_female) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=T)+ 
  labs(title = "Females (n=40 countries)",
       x = "Increase in tobacco use prevalence between 2007 and 2018",
       y = "MPOWER score in 2008")+
  theme( plot.title = element_text(hjust = 0.5, face = "bold"))
cor(csd$Change_male, csd$MPOWER_2008)
cor(csd$Change_female, csd$MPOWER_2008)


# SECTION B: MAIN ANALYSIS

#Import lagged panel dataset from clipboard (copied from MS excel sheet 2)
pan <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)
#Convert categorical variables to factors
pan$Country <- as.factor(pan$Country)
pan$Year <- as.factor(pan$Year)

#1 Panel data two-way fixed effects models for tobacco smoking
library(plm)
#All adults
mod <- plm(Smoked_prev ~ MPOWE + Price + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
#Males only
mod <- plm(Smoked_male ~ MPOWE + Price + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
#Females only
mod <- plm(Smoked_female ~ MPOWE + Price + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)

#2 Models for smokeless tobacco use
#All adults
mod <- plm(Smokeless_prev ~ MPOWE + Price + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
#Males only
mod <- plm(Smokeless_male ~ MPOWE + Price + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
#Females only
mod <- plm(Smokeless_female ~ MPOWE + Price + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)


# PART C: SUB-GROUP ANALYSES

#1 Subgroup analysis of African sub-regions
mod <-plm (Smoked_prev ~ MPOWE + Price + GNI, data = subset(pan, pan$UN_subregion=="Western"), index = c("Country", "Year"), model = "within", effect = "time")
summary(model1) #Smoking
mod <-plm (Smoked_prev ~ MPOWE + Price + GNI, data = subset(pan, pan$UN_subregion=="Western"), index = c("Country", "Year"), model = "within", effect = "time")
summary(model1) #Smokeless tobacco use
#Repeat the above for each region by replacing "Western" with "Eastern", "Middle", "Northern" and "Southern".

#2 Subgroup analysis of HM/HP Countries
hm<- subset(pan, Country == "Algeria" | Country == "Botswana" | Country == "Burkina Faso"|
              Country == "Egypt"| Country == "Gambia"| Country == "Madagascar"| 
              Country == "Mauritania"| Country == "Mauritius"| Country == "Morocco"| 
              Country == "Mozambique"| Country == "Seychelles"| Country == "South Africa"| 
              Country == "Tunisia"| Country == "Uganda"| Country == "Tanzania" | 
              Country == "Zambia"| Country == "Zimbabwe")
# Models for tobacco smoking
mod <- plm(Smoked_prev ~ MPOWE + Price + GNI, data = hm, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
mod <- plm(Smoked_male ~ MPOWE + Price + GNI, data = hm, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
mod <- plm(Smoked_female ~ MPOWE + Price + GNI, data = hm, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
# Repeat the above for smokeless tobacco use


#3 Subgroup analysis of Countries with initially high and earnestly improved MPOWER scores
earnest<- subset(pan, Country == "Benin" | Country == "Chad" | Country == "Burkina Faso"|
               Country == "Niger"| Country == "Seychelles"| Country == "South Africa"| 
               Country == "Uganda" | Country == "Cameroon" | Country == "Botswana"| 
               Country == "Senegal"| Country == "Mali")
# Models for tobacco smoking
mod <- plm(Smoked_prev ~ MPOWE + Price + GNI, data = earnest, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
mod <- plm(Smoked_male ~ MPOWE + Price + GNI, data = earnest, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
mod <- plm(Smoked_female ~ MPOWE + Price + GNI, data = earnest, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
# Repeat the above for smokeless tobacco use


# PART D: SENSITIVITY TESTS AND MODEL FIT

#Sensitivity Test

#1 Main analysis repeated with price replaced by the R component
mod <- plm(Smoked_prev ~ MPOWER + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)#All adults
mod <- plm(Smoked_male ~ MPOWER + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)#Males only
mod <- plm(Smoked_female ~ MPOWER + GNI, data = pan, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)#Females only

#2 "Earnest" subgroup analysis with price was replaced by the R component
mod <- plm(Smoked_prev ~ MPOWER + GNI, data = earnest, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
mod <- plm(Smoked_prev ~ M+P+O+W+E+Price + GNI, data = earnest, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)
mod <- plm(Smoked_female ~ MPOWER + GNI, data = earnest, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)

#3 Using Unemployment Rate as a negative outcome
mod <- plm(Unempl_rate ~ MPOWE + Price + GNI, data = earnest, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(mod)

#4 Checking Model Fit
hist(residuals(mod), xlab = 'Residuals') #Normally distributed residuals
qqnorm(residuals(mod), ylab = 'Residuals') #No evidence of heteroscedasticity
qqline(residuals(mod)) #Looks good.

#The End
