#Trying to see which attributes make men more likely to say yes to a woman

library(gdata)
setwd("/Users/pihu_yadav/Downloads/ORIE4741_Project-master")
speeddating=read.csv("data_excel.csv")

#selecting all the participants that are male
mendata<- subset(speeddating, gender==1)
#OR
mendata<-subset(full_data,gender==1)

#seeing all the women they said yes to
menlike<- subset(mendata, dec==1)

#seeing the summaries of some key statistics that might be deciding the decision
#these include: whether both are of the same race, their interests correlate, the woman's levels of attractiveness, sincerity, intelligence, fun, ambition and shared interests as determined by the man at the end of the speed date
menattr<- subset(menlike, select = c(samerace, int_corr, attr, sinc, intel, fun, amb, shar))
summary(menattr)

#seeing the total values of the above features (i.e. including the results of data where the man said no to the woman) in order to provide a comparison between all women and the women they said yes to
mentotalattr <- subset(mendata , select = c(samerace, int_corr, attr, sinc, intel, fun, amb, shar))
summary(mentotalattr)

#Output for what men want (women they said yes to)
#summary(menattr)
#    samerace         int_corr           attr             sinc       
# Min.   :0.0000   Min.   :-0.830   Min.   : 2.000   Min.   : 0.000  
# 1st Qu.:0.0000   1st Qu.:-0.010   1st Qu.: 6.000   1st Qu.: 7.000  
# Median :0.0000   Median : 0.200   Median : 7.000   Median : 8.000  
# Mean   :0.3957   Mean   : 0.195   Mean   : 7.449   Mean   : 7.568  
# 3rd Qu.:1.0000   3rd Qu.: 0.430   3rd Qu.: 8.000   3rd Qu.: 8.000  
# Max.   :1.0000   Max.   : 0.900   Max.   :10.000   Max.   :10.000  
#                                   NA's   :7        NA's   :29      
#     intel             fun              amb              shar       
# Min.   : 3.000   Min.   : 0.000   Min.   : 2.000   Min.   : 0.000  
# 1st Qu.: 7.000   1st Qu.: 6.000   1st Qu.: 6.000   1st Qu.: 5.000  
# Median : 8.000   Median : 7.000   Median : 7.000   Median : 6.000  
# Mean   : 7.621   Mean   : 7.296   Mean   : 6.995   Mean   : 6.372  
# 3rd Qu.: 8.000   3rd Qu.: 8.000   3rd Qu.: 8.000   3rd Qu.: 8.000  
# Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000  
# NA's   :26       NA's   :49       NA's   :134      NA's   :181   


#Output for their general responses (for comparison)
#summary(mentotalattr)
#    samerace         int_corr           attr            sinc       
# Min.   :0.0000   Min.   :-0.830   Min.   : 0.00   Min.   : 0.000  
# 1st Qu.:0.0000   1st Qu.:-0.010   1st Qu.: 5.00   1st Qu.: 6.000  
# Median :0.0000   Median : 0.200   Median : 7.00   Median : 7.000  
# Mean   :0.3953   Mean   : 0.192   Mean   : 6.46   Mean   : 7.251  
# 3rd Qu.:1.0000   3rd Qu.: 0.430   3rd Qu.: 8.00   3rd Qu.: 8.000  
# Max.   :1.0000   Max.   : 0.910   Max.   :10.00   Max.   :10.000  
#                                   NA's   :101     NA's   :147     
#     intel            fun             amb              shar      
# Min.   : 0.00   Min.   : 0.00   Min.   : 0.000   Min.   : 0.00  
# 1st Qu.: 6.00   1st Qu.: 5.00   1st Qu.: 5.000   1st Qu.: 4.00  
# Median : 7.00   Median : 7.00   Median : 7.000   Median : 6.00  
# Mean   : 7.29   Mean   : 6.52   Mean   : 6.603   Mean   : 5.54  
# 3rd Qu.: 8.00   3rd Qu.: 8.00   3rd Qu.: 8.000   3rd Qu.: 7.00  
# Max.   :10.00   Max.   :10.00   Max.   :10.000   Max.   :10.00  
# NA's   :149     NA's   :167     NA's   :350      NA's   :491   

#From the summary statistics we note that there are a lot of NA values in several of the predictors.
#We can either ignore these values or investigate whether or not all the attributes are not filled in
#(i.e. someone did not fill out an entire section of the survey) or they were genuinely not interested 
#in the female which would imply that their value would be the lowest possible (1).  Thus we should 
#consider overwriting all NA values and 0's to be 1's since that was technically the lowest scale 
#that the inputted values were allowed to take.

#[Code to overwrite 0's and NA's are in the Import_data_Script.r file]

#Also want to run a linear regression to see whether or not these factors are significant
#to a linear model with the classification 
men_pref_lm<-lm(dec~samerace+int_corr+attr+sinc+intel+fun+amb+shar,data=mendata)
summary(men_pref_lm)

#Below is the output of the linear model with all the above predictors.
#We can see that the intercept, attr level, sinc level, fun, ambitious, and sharing in the same interests
#are all statistically significant predictors to the decision factor (whether or not a male will like a female)

# Call:
# lm(formula = dec ~ samerace + int_corr + attr + sinc + intel + 
    # fun + amb + shar, data = mendata)

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -1.09270 -0.34890  0.03239  0.35224  1.25873 

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.372314   0.038558  -9.656  < 2e-16 ***
# samerace    -0.008968   0.014233  -0.630    0.529    
# int_corr     0.001285   0.022993   0.056    0.955    
# attr         0.113255   0.004688  24.158  < 2e-16 ***
# sinc        -0.024752   0.005963  -4.151 3.38e-05 ***
# intel       -0.004253   0.007039  -0.604    0.546    
# fun          0.040311   0.005529   7.291 3.76e-13 ***
# amb         -0.024421   0.005438  -4.491 7.33e-06 ***
# shar         0.043795   0.004286  10.217  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.415 on 3574 degrees of freedom
  # (611 observations deleted due to missingness)
# Multiple R-squared:  0.3126,    Adjusted R-squared:  0.3111 
# F-statistic: 203.2 on 8 and 3574 DF,  p-value: < 2.2e-16


#Actual logistic regression using the same parameters as above.
men_pref_logistic<-glm(dec~samerace+int_corr+attr+sinc+intel+fun+amb+shar,data=mendata,family="binomial")

summary(men_pref_logistic)
# Call:
# glm(formula = dec ~ samerace + int_corr + attr + sinc + intel + 
    # fun + amb + shar, family = "binomial", data = mendata)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -2.6827  -0.8095  -0.1530   0.8376   3.0605  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.27812    0.27085 -19.487  < 2e-16 ***
# samerace    -0.05702    0.08371  -0.681    0.496    
# int_corr     0.04329    0.13670   0.317    0.752    
# attr         0.68250    0.03287  20.764  < 2e-16 ***
# sinc        -0.16213    0.03679  -4.407 1.05e-05 ***
# intel       -0.03350    0.04246  -0.789    0.430    
# fun          0.26077    0.03410   7.647 2.06e-14 ***
# amb         -0.15315    0.03381  -4.530 5.89e-06 ***
# shar         0.27113    0.02606  10.403  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 4965  on 3582  degrees of freedom
# Residual deviance: 3599  on 3574  degrees of freedom
  # (611 observations deleted due to missingness)
# AIC: 3617

# Number of Fisher Scoring iterations: 5

#Then we only use the significant predictors and refit
men_pref_logistic_2<-glm(dec~attr+sinc+fun+amb+shar,data=mendata,family="binomial")
summary(men_pref_logistic_2)
# Call:
# glm(formula = dec ~ attr + sinc + fun + amb + shar, family = "binomial", 
    # data = mendata)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -2.6753  -0.8020  -0.1514   0.8340   3.0764  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.35443    0.25844 -20.719  < 2e-16 ***
# attr         0.68138    0.03278  20.788  < 2e-16 ***
# sinc        -0.17519    0.03237  -5.413 6.21e-08 ***
# fun          0.25775    0.03385   7.615 2.64e-14 ***
# amb         -0.16211    0.03166  -5.121 3.05e-07 ***
# shar         0.27114    0.02602  10.422  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 4972.0  on 3587  degrees of freedom
# Residual deviance: 3601.8  on 3582  degrees of freedom
  # (606 observations deleted due to missingness)
# AIC: 3613.8

# Number of Fisher Scoring iterations: 5

