###Social Determinants of Health and Depressive Symptoms among 142,600 Older Adults: 
###Findings from Six Cohort Studies across 33 Countries


##########  The following code is about data analysis of this study, using the example of HRS wave 10-13.
##########  Including generalized estimation equations (GEE), subgroup analysis, meta-analysis, five sets of 
##########  sensitivity analyses, and interaction effect analysis.




###### 1.generalized estimation equations ######
library(haven)    ##open .dta文件
library(geepack)    ##GEE
library(openxlsx)
library(stringr)
library(data.table)

###### independent SDoH and depressive symptoms ###### 
###### Define a table to receive results
gee_total <- read.xlsx(startRow = 1,
                       "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/2. 回归/1. 总样本分析/gee_total.xlsx",
                       sheet="Sheet1")

###### GEE  
data <- read_dta("D:/SCI/Database/1. HRS (Y)/1. harmonized data/HarmonizedHRSvC_STATA/hrs_gee.dta")
dt <- data.frame(data['housefw2'],data['employ1'],
                 data['pension1'],data['edu1'],
                 data['owner1'],data['marriage1'],
                 data['childh'],data['insure1'],
                 data['id'],data['wave'],
                 data['depress1'],data['age'],
                 data['sex'],data['smoke'],
                 data['drink'],data['chronic'],
                 data['badl'],data['sdoh1'])
dt0=as.data.frame(lapply(dt,as.numeric))

i=1
j=1
k=1

for (j in 1:8) {
  dt0$blank <- dt0[,j]
  result <- geeglm(depress1~factor(blank)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
  summary(result)
  result <- summary(result)$coefficients
  irr <- exp(result[2,1])    ##irr
  std <- irr * result[2,2]    ##std
  irr_l <- irr-qnorm((1+0.95)/2)*std    ##irr_low
  irr_u <- irr+qnorm((1+0.95)/2)*std    ##irr_up
  irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
  
  gee_total[j*3,2] <- irr_ci
  j=j+1
}

result.5 <- geeglm(depress1~factor(housefw2)+factor(employ1)+factor(pension1)+
                     factor(edu1)+factor(owner1)+factor(marriage1)+factor(childh)+
                     factor(insure1)+
                     age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
result1 <- summary(result.5)$coefficients
irr_p <- exp(result1[4,1])    ##irr（pension）
irr_p
std_p <- irr_p * result1[4,2]    ##std（pension）
std_p
for(k in 1:8){
  irr1 <- exp(result1[k+1,1])    ##irr
  std1 <- irr1 * result1[k+1,2]    ##std
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  
  gee_total[k*3,3] <- irr_ci1
  k=k+1
}


###### cumulative SDoH and depressive symptoms ###### 
gee_total1 <- read.xlsx(startRow = 1,
                        "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/2. 回归/1. 总样本分析/gee_total.xlsx",
                        sheet="Sheet2")

dt <- read_dta("D:/SCI/Database/1. HRS (Y)/1. harmonized data/HarmonizedHRSvC_STATA/hrs_gee.dta")
dt0=as.data.frame(lapply(dt,as.numeric))

k=1

result.5 <- geeglm(depress1~factor(sdoh1)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
result1 <- summary(result.5)$coefficients
for(k in 1:6){
  irr1 <- exp(result1[k+1,1])    ##irr
  std1 <- irr1 * result1[k+1,2]    ##std
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  
  gee_total1[k+1,2] <- irr_ci1
  k=k+1
}

result1.5 <- geeglm(depress1~sdoh1+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                      factor(badl),id=id,corstr="exchangeable",
                    data=dt0,waves=wave,family=binomial(logit))
result2.5 <- summary(result1.5)$coefficients
p <- result2.5[2,4]
p <- str_c("<", sprintf("%0.4f",p))
p
gee_total1[1,3] <- p



###### 2.subgroup analysis ######
###### independent SDoH and depressive symptoms ###### 
subgroup <- read.xlsx(startRow = 1,
                      "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/2. 回归/亚组分析/sdoh_subgroup.xlsx",
                      sheet="Sheet2")

data <- read_dta("D:/SCI/Database/1. HRS (Y)/1. harmonized data/HarmonizedHRSvC_STATA/hrs_gee.dta")

dt <- data.frame(data['housefw2'],data['employ1'],
                 data['pension1'],data['edu1'],
                 data['owner1'],data['marriage1'],
                 data['childh'],data['insure1'],
                 data['id'],data['wave'],
                 data['depress1'],data['age'],
                 data['sex'],data['smoke'],
                 data['drink'],data['chronic'],
                 data['badl'],data['age1'])

dt0=as.data.frame(lapply(dt,as.numeric))

dt0_m <- as.data.frame(subset(dt0,dt0$sex==1))    ##male
dt0_f <- as.data.frame(subset(dt0,dt0$sex==2))    ##female
dt0_y <- as.data.frame(subset(dt0,dt0$age1==0))    ##younger
dt0_o <- as.data.frame(subset(dt0,dt0$age1==1))    ##older
dt0_i <- list(dt0_m,dt0_f,dt0_y,dt0_o)


i=1
j=1
k=1

while (i <= 4){
  data <- dt0_i[[i]]
  for (j in 1:8) {
    data$blank <- data[,j]
    if(i<=2){
      result <- geeglm(depress1~factor(blank)+age+factor(smoke)+factor(drink)+factor(chronic)+
                         factor(badl),id=id,corstr="exchangeable",
                       data=data,waves=wave,family=binomial(logit))
    } else {
      result <- geeglm(depress1~factor(blank)+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                         factor(badl),id=id,corstr="exchangeable",
                       data=data,waves=wave,family=binomial(logit))
    }
    result <- summary(result)$coefficients
    irr <- exp(result[2,1])    ##irr
    std <- irr * result[2,2]    ##std
    irr_l <- irr-qnorm((1+0.95)/2)*std    ##irr_low
    irr_u <- irr+qnorm((1+0.95)/2)*std    ##irr_up
    irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
    
    subgroup[j*5-4+i,2] <- irr_ci
    
    j <- j+1
  }
  
  if(i<=2){
    result1 <- geeglm(depress1~factor(housefw2)+factor(employ1)+factor(pension1)+
                        factor(edu1)+factor(owner1)+factor(marriage1)+factor(childh)+
                        factor(insure1)+
                        age+factor(smoke)+factor(drink)+factor(chronic)+
                        factor(badl),id=id,corstr="exchangeable",
                      data=data,waves=wave,family=binomial(logit))
  } else {
    result1 <- geeglm(depress1~factor(housefw2)+factor(employ1)+factor(pension1)+
                        factor(edu1)+factor(owner1)+factor(marriage1)+factor(childh)+
                        factor(insure1)+
                        factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                        factor(badl),id=id,corstr="exchangeable",
                      data=data,waves=wave,family=binomial(logit))
  }
  result1 <- summary(result1)$coefficients
  for(k in 1:8){
    irr1 <- exp(result1[k+1,1])    ##irr
    std1 <- irr1 * result1[k+1,2]    ##std
    irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
    irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
    irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
    
    subgroup[k*5-4+i,3] <- irr_ci1
    
    k <- k+1
  }
  k <- 1
  j <- 1
  i <- i+1
}


###### cumulative SDoH and depressive symptoms ###### 
nsubgroup <- read.xlsx(startRow = 1,
                       "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/2. 回归/亚组分析/nsdoh_subgroup.xlsx",
                       sheet="sheet1")

dt <- read_dta("D:/SCI/Database/1. HRS (Y)/1. harmonized data/HarmonizedHRSvC_STATA/hrs_gee.dta")

dt0=as.data.frame(lapply(dt,as.numeric))

dt0_m <- as.data.frame(subset(dt0,dt0$sex==1))    ##male
dt0_f <- as.data.frame(subset(dt0,dt0$sex==2))    ##female
dt0_y <- as.data.frame(subset(dt0,dt0$age1==0))    ##younger
dt0_o <- as.data.frame(subset(dt0,dt0$age1==1))    ##older
dt0_i <- list(dt0_m,dt0_f,dt0_y,dt0_o)

i=1
j=1
k=1

while (i <= 4){
  data <- dt0_i[[i]]
  
  if(i<=2){
    result <- geeglm(depress1~factor(sdoh1)+age+factor(smoke)+factor(drink)+factor(chronic)+
                       factor(badl),id=id,corstr="exchangeable",
                     data=data,waves=wave,family=binomial(logit))
    result1 <- geeglm(depress1~sdoh1+age+factor(smoke)+factor(drink)+factor(chronic)+
                        factor(badl),id=id,corstr="exchangeable",
                      data=data,waves=wave,family=binomial(logit))
  } else {
    result <- geeglm(depress1~factor(sdoh1)+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                       factor(badl),id=id,corstr="exchangeable",
                     data=data,waves=wave,family=binomial(logit))
    result1 <- geeglm(depress1~sdoh1+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                        factor(badl),id=id,corstr="exchangeable",
                      data=data,waves=wave,family=binomial(logit))
  }
  result <- summary(result)$coefficients
  result1 <- summary(result1)$coefficients
  
  for (j in 1:6){
    irr <- exp(result[j+1,1])    ##irr
    std <- irr * result[j+1,2]    ##std
    irr_l <- irr-qnorm((1+0.95)/2)*std    ##irr_low
    irr_u <- irr+qnorm((1+0.95)/2)*std    ##irr_up
    irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
    
    if(i<=2){
      nsubgroup[j+1,i+1] <- irr_ci
    } else {
      nsubgroup[j+1,i+11] <- irr_ci
    }
    j <- j+1
  }
  
  if(i<=2){
    nsubgroup[9,i+1] <- result1[2,4]
  } else {
    nsubgroup[9,i+11] <- result1[2,4]
  }
  
  j <- 1
  i <- i+1
}


###### 3.meta-analysis and sugroup meta-analysis ######
###### independent SDoH and depressive symptoms ###### 
library(openxlsx)
library(meta)
library(stringr)

###### meta-analysis and sugroup meta-analysis
meta_1 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="housefw")
meta_2 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="employ")
meta_3 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="pension")
meta_4 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="edu")
meta_5 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="own")
meta_6 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="marriage")
meta_7 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="child")
meta_8 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="social")
meta_9 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_回归结果.xlsx",
                    sheet="insure")

meta <- list(meta_1,meta_2,meta_3,meta_4,meta_5,meta_6,meta_7,meta_8,meta_9)

meta1 <- read.xlsx(startRow = 1,
                   "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/meta_模板（sdoh）.xlsx",
                   sheet="meta")

i=1
j=1
k=2

while(i <= 9){
  data <- meta[[i]]
  for (j in 1:5) {
    if (j==1){
      irr=data$irr
      std=data$std
      n=data$n
      case=data$case
    } else if(j==2){
      irr=data$irr_m
      std=data$std_m
      n=data$n_m
      case=data$case_m
    } else if(j==3){
      irr=data$irr_f
      std=data$std_f
      n=data$n_f
      case=data$case_f 
    } else if(j==4){
      irr=data$irr_y
      std=data$std_y
      n=data$n_y
      case=data$case_y 
    } else if(j==5){
      irr=data$irr_o
      std=data$std_o
      n=data$n_o
      case=data$case_o 
    }
    dt <- data.frame(irr, std, base=data$base)
    dt$lg_irr <- log(dt$irr)
    dt$lg_std <- (log(dt$irr+qnorm(0.975)*dt$std)-log(dt$irr+1e-30-qnorm(0.975)*dt$std))/(2*qnorm(0.975))
    fit=metagen(lg_irr, #lg(irr)
                lg_std, #standard error
                sm="RR",   
                data=dt,
                fixed = T,
                random = F,
                backtransf=T
    )
    result <- summary(fit)
    result
    N_total <- sum(n)    ##
    N_total
    n_case <- sum(case)    ##
    n_case
    irr_ <- exp(result$fixed$TE)    ##
    irr_
    l_irr <- exp(result$fixed$lower)
    l_irr
    u_irr <- exp(result$fixed$upper)
    u_irr
    irr_ci <- str_c(sprintf("%0.2f",irr_)," (",sprintf("%0.2f",l_irr),", ",sprintf("%0.2f",u_irr),")")
    p <- result$fixed$p    ##
    p <- sprintf("%0.4f",p)
    p
    
    between_i2 <- result$I2*100
    between_i2 <- str_c(sprintf("%0.2f",between_i2),"%")
    
    meta1[k,2] <- N_total
    meta1[k,3] <- n_case
    meta1[k,4] <- irr_ci
    meta1[k,5] <- p
    meta1[k,6] <- between_i2
    meta1[k,7] <- irr_
    meta1[k,8] <- l_irr
    meta1[k,9] <- u_irr
    
    j <- j+1
    k <- k+1
  }
  i <- i+1
  j <- 1
  k <- k+1
}

meta1

list_data1 <- list("meta" = meta1)
write.xlsx(list_data1, file = "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/sdoh_meta结果.xlsx")


###### test between-group differences (output: P value)
meta <- list(meta_1,meta_2,meta_3,meta_4,meta_5,meta_6,meta_7,meta_8,meta_9)

result_p = c(rep(0,18))
i=1
j=1
k=1

while(i <= 9){
  data <- meta[[i]]
  for (j in 1:2) {
    if (j==1){
      irr=data$irr_sex
      std=data$std_sex
      group=data$sex
    } else if(j==2){
      irr=data$irr_age
      std=data$std_age
      group=data$age
    }
    dt <- data.frame(irr,std,group)
    dt$lg_irr <- log(dt$irr)
    dt$lg_std <- (log(dt$irr+qnorm(0.975)*dt$std)-log(dt$irr-qnorm(0.975)*dt$std+1e-6))/(2*qnorm(0.975))
    fit=metagen(lg_irr,
                lg_std,
                sm="RR",
                data=dt,
                fixed = T,
                random = F,
                backtransf=T
    )
    result=update.meta(fit, 
                       subgroup = group, 
                       tau.common = FALSE)
    p <- result$pval.Q.b.common    ##
    p <- sprintf("%0.4f",p)
    result_p[k] <- p
    k=k+1
  }
  j=1
  i=i+1
}

###### cumulative SDoH and depressive symptoms ###### 
library(openxlsx)
library(meta)
library(stringr)

###### meta-analysis and sugroup meta-analysis
meta_1 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/nsdoh_回归结果.xlsx",
                    sheet="≥1")
meta_2 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/nsdoh_回归结果.xlsx",
                    sheet="≥2")
meta_3 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/nsdoh_回归结果.xlsx",
                    sheet="≥3")
meta_4 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/nsdoh_回归结果.xlsx",
                    sheet="≥4")
meta_5 <- read.xlsx(startRow = 1,
                    "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/nsdoh_回归结果.xlsx",
                    sheet="≥5")

meta <- list(meta_1, meta_2, meta_3, meta_4, meta_5)

meta1 <- read.xlsx(startRow = 1,
                   "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/meta_模板（nsdoh）.xlsx",
                   sheet="meta")
i=1
j=1
k=2

while(i <= 5){
  data <- meta[[i]]
  for (j in 1:5) {
    if (j==1){
      irr=data$irr
      std=data$std
      n=data$n
      case=data$case
    } else if(j==2){
      irr=data$irr_m
      std=data$std_m
      n=data$n_m
      case=data$case_m
    } else if(j==3){
      irr=data$irr_f
      std=data$std_f
      n=data$n_f
      case=data$case_f 
    } else if(j==4){
      irr=data$irr_y
      std=data$std_y
      n=data$n_y
      case=data$case_y 
    } else if(j==5){
      irr=data$irr_o
      std=data$std_o
      n=data$n_o
      case=data$case_o 
    }
    dt <- data.frame(irr, std, base=data$base)
    dt$lg_irr <- log(dt$irr)
    dt$lg_std <- (log(dt$irr+qnorm(0.975)*dt$std)-log(dt$irr-qnorm(0.975)*dt$std))/(2*qnorm(0.975))
    fit=metagen(lg_irr, #lg(irr)
                lg_std, #standard
                sm="RR",   
                data=dt,
                fixed = T,
                random = F,
                backtransf=T
    )
    result <- summary(fit)
    result
    N_total <- sum(n)    ##
    N_total
    n_case <- sum(case)    ##
    n_case
    irr_ <- exp(result$fixed$TE)    ##
    irr_
    l_irr <- exp(result$fixed$lower)
    l_irr
    u_irr <- exp(result$fixed$upper)
    u_irr
    irr_ci <- str_c(sprintf("%0.2f",irr_)," (",sprintf("%0.2f",l_irr),", ",sprintf("%0.2f",u_irr),")")
    p <- result$fixed$p    ##
    p <- sprintf("%0.4f",p)
    p
    
    between_i2 <- result$I2*100
    between_i2 <- str_c(sprintf("%0.2f",between_i2),"%")
    
    meta1[k,2] <- N_total
    meta1[k,3] <- n_case
    meta1[k,4] <- irr_ci
    meta1[k,5] <- p
    meta1[k,6] <- between_i2
    meta1[k,7] <- irr_
    meta1[k,8] <- l_irr
    meta1[k,9] <- u_irr
    
    j <- j+1
    k <- k+1
  }
  i <- i+1
  j <- 1
  k <- k+1
}

meta1

list_data1 <- list("meta" = meta1)
write.xlsx(list_data1, file = "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/3. meta/2. 以数据库为单位做meta/nsdoh_meta结果.xlsx")

###### test between-group differences (output: P value)
meta <- list(meta_1, meta_2, meta_3, meta_4, meta_5)

result_p = c(rep(0,10))
i=1
j=1
k=1

while(i <= 5){
  data <- meta[[i]]
  for (j in 1:2) {
    if (j==1){
      irr=data$irr_sex
      std=data$std_sex
      group=data$sex
    } else if(j==2){
      irr=data$irr_age
      std=data$std_age
      group=data$age
    }
    dt <- data.frame(irr,std,group)
    dt$lg_irr <- log(dt$irr)
    dt$lg_std <- (log(dt$irr+qnorm(0.975)*dt$std)-log(dt$irr-qnorm(0.975)*dt$std+1e-6))/(2*qnorm(0.975))
    fit=metagen(lg_irr, 
                lg_std, 
                sm="RR", 
                data=dt,
                fixed = T,
                random = F,
                backtransf=T
    )
    result=update.meta(fit, 
                       subgroup = group, 
                       tau.common = FALSE)
    p <- result$pval.Q.b.common    ##
    p <- sprintf("%0.4f",p)
    result_p[k] <- p
    k=k+1
  }
  j=1
  i=i+1
}


###### 4.five sets of sensitivity analyses ######
###### define tables to receive results
meta1 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="housefw")
meta2 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="employ")
meta3 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="pension")
meta4 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="edu")
meta5 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="own")
meta6 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="marriage")
meta7 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="child")
meta8 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="social")
meta9 <- read.xlsx(startRow = 1,"C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/meta/meta_模板（sdoh）.xlsx",
                   sheet="insure")
meta <- list(meta1,meta2,meta3,meta4,meta5,meta6,meta7,meta8,meta9)


###### 4.1 using contineous depressive symptoms as outcome ###### 
###### independent SDoH and depressive symptoms ###### 
sensitive1 <- read.xlsx(startRow = 1,
                        "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                        sheet="Sheet1")

data <- read_dta("D:/SCI/Database/1. HRS (Y)/1. harmonized data/HarmonizedHRSvC_STATA/hrs_gee.dta")
dt <- data.frame(data['housefw2'],data['employ1'],
                 data['pension1'],data['edu1'],
                 data['owner1'],data['marriage1'],
                 data['childh'],data['insure1'],
                 data['id'],data['wave'],
                 data['depress'],data['age'],
                 data['sex'],data['smoke'],
                 data['drink'],data['chronic'],
                 data['badl'])
dt0=as.data.frame(lapply(dt,as.numeric))

i=1
j=1
k=1
for (j in 1:8) {
  dt0$blank <- dt0[,j]
  result <- geeglm(depress~factor(blank)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=gaussian(identity))
  summary(result)
  result <- summary(result)$coefficients
  irr <- result[2,1]    ##irr值
  std <- result[2,2]
  irr_l <- irr-qnorm((1+0.95)/2)*std    ##irr_low
  irr_u <- irr+qnorm((1+0.95)/2)*std    ##irr_up
  irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
  sensitive1[j*3,2] <- irr_ci
  j=j+1
}

result.5 <- geeglm(depress~factor(housefw2)+factor(employ1)+factor(pension1)+
                     factor(edu1)+factor(owner1)+factor(marriage1)+factor(childh)+
                     factor(insure1)+
                     age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=gaussian(identity))
result1 <- summary(result.5)$coefficients
for(k in 1:8){
  irr1 <- result1[k+1,1]    ##irr值
  std1 <- result1[k+1,2]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  sensitive1[k*3,3] <- irr_ci1
  
  if (k==8){
    k=9
    n <- length(dt0$id)    ##n值
    meta[[k]][1,4] <- n
    meta[[k]][1,6] <- irr1
    meta[[k]][1,7] <- std1
    k=k+1
  }else{
    n <- length(dt0$id)    ##n值
    meta[[k]][1,4] <- n
    meta[[k]][1,6] <- irr1
    meta[[k]][1,7] <- std1
    k=k+1
  }
}


###### cumulative SDoH and depressive symptoms ###### 
sensitive11 <- read.xlsx(startRow = 1,
                         "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                         sheet="Sheet2")

dt <- read_dta("D:/SCI/Database/1. HRS (Y)/1. harmonized data/HarmonizedHRSvC_STATA/hrs_gee.dta")
dt0=as.data.frame(lapply(dt,as.numeric))

k=1

result.5 <- geeglm(depress~factor(sdoh1)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=gaussian(identity))
result1 <- summary(result.5)$coefficients
for(k in 1:6){
  irr1 <- result1[k+1,1]    ##irr值
  std1 <- result1[k+1,2]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  
  sensitive11[k+1,2] <- irr_ci1
  k=k+1
}

result1.5 <- geeglm(depress~sdoh1+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                      factor(badl),id=id,corstr="exchangeable",
                    data=dt0,waves=wave,family=gaussian(identity))
result2.5 <- summary(result1.5)$coefficients
p <- result2.5[2,4]
p <- str_c("<", sprintf("%0.4f",p))
p
sensitive11[1,3] <- p


###### 4.2 using sample without elevated depressive symptoms at baseline ###### 
###### produce the sample without elevated depressive symptoms at baseline
data1 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/1.hrs_sensitive.dta")
data2 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/2.elsa_sensitive.dta")
data3 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/3.share_sensitive.dta")
data4 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/4.charls_sensitive.dta")
data5 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/5.mhas_sensitive.dta")

dt1=as.data.frame(lapply(data1,as.numeric))
dt2=as.data.frame(lapply(data2,as.numeric))
dt3=as.data.frame(lapply(data3,as.numeric))
dt4=as.data.frame(lapply(data4,as.numeric))
dt5=as.data.frame(lapply(data5,as.numeric))
dt <- list(dt1,dt2,dt3,dt4,dt5)


########### find out baseline sample and who had elevated depressive symptoms 
i=1
j=1
for(i in 1:5){
  dt0=dt[[i]]
  length=length(dt0$id)
  dt0$fstwv=c(rep(-1,length))
  dt0$drop_id=c(rep(-1,length))
  dt0$drop=c(rep(-1,length))
  ship=c(rep(-1,length))
  
  for(j in 1:length){
    t=dt0$id[j]
    yon=t %in% ship[]  
    ship[j]=t
    
    if(yon){
      dt0$fstwv[j]=0
    }else{
      dt0$fstwv[j]=1
    }
    
    if(dt0$fstwv[j]==1 & dt0$depress1[j]==1){
      dt0$drop_id[j]=dt0$id[j]
    }else{
      dt0$drop_id[j]=0
    }
    
    yon1=dt0$id[j] %in% dt0$drop_id[] 
    if(yon1){
      dt0$drop[j]=1
    }else{
      dt0$drop[j]=0
    }
    
    j=j+1
  }
  
  dt[[i]]=dt0
  j=1
  i=i+1
}
########### drop out baseline sample with depressisve symptoms and baseline data
i=1
for(i in 1:5){
  dt0=dt[[i]]
  dt0 = dt0[dt0$drop==0 & dt0$fstwv==0, ]
  dt[[i]]=dt0
}

write_dta(dt[[1]], "D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/1.hrs_sensitive_整理.dta")
write_dta(dt[[2]], "D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/2.elsa_sensitive_整理.dta")
write_dta(dt[[3]], "D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/3.share_sensitive_整理.dta")
write_dta(dt[[4]], "D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/4.charls_sensitive_整理.dta")
write_dta(dt[[5]], "D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/5.mhas_sensitive_整理.dta")



###### independent SDoH and depressive symptoms ###### 
sensitive1 <- read.xlsx(startRow = 1,
                        "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                        sheet="Sheet1")

data <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/1.hrs_sensitive_整理.dta")
dt <- data.frame(data['housefw2'],data['employ1'],
                 data['pension1'],data['edu1'],
                 data['owner1'],data['marriage1'],
                 data['childh'],data['insure1'],
                 data['id'],data['wave'],
                 data['depress1'],data['age'],
                 data['sex'],data['smoke'],
                 data['drink'],data['chronic'],
                 data['badl'])
dt0=as.data.frame(lapply(dt,as.numeric))

i=1
j=1
k=1

for (j in 1:8) {
  dt0$blank <- dt0[,j]
  result <- geeglm(depress1~factor(blank)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
  summary(result)
  result <- summary(result)$coefficients
  irr <- exp(result[2,1])    ##irr值
  std <- irr * result[2,2]
  irr_l <- irr-qnorm((1+0.95)/2)*std    ##irr_low
  irr_u <- irr+qnorm((1+0.95)/2)*std    ##irr_up
  irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
  
  sensitive1[j*3,2] <- irr_ci
  j=j+1
}

result.5 <- geeglm(depress1~factor(housefw2)+factor(employ1)+factor(pension1)+
                     factor(edu1)+factor(owner1)+factor(marriage1)+factor(childh)+
                     factor(insure1)+
                     age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
result1 <- summary(result.5)$coefficients
for(k in 1:8){
  irr1 <- exp(result1[k+1,1])    ##irr值
  std1 <- irr1 * result1[k+1,2]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  sensitive1[k*3,3] <- irr_ci1
  
  if (k==8){
    k=9
    n <- length(dt0$id)    ##n值
    meta[[k]][18,4] <- n
    meta[[k]][18,6] <- irr1
    meta[[k]][18,7] <- std1
    k=k+1
  }else{
    n <- length(dt0$id)    ##n值
    meta[[k]][18,4] <- n
    meta[[k]][18,6] <- irr1
    meta[[k]][18,7] <- std1
    k=k+1
  }
}


###### cumulative SDoH and depressive symptoms ###### 
sensitive11 <- read.xlsx(startRow = 1,
                         "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                         sheet="Sheet2")

dt <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/3. 敏感性分析（基线无抑郁）/2. 分析库/1.hrs_sensitive_整理.dta")
dt0=as.data.frame(lapply(dt,as.numeric))

k=1

result.5 <- geeglm(depress1~factor(sdoh1)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
result1 <- summary(result.5)$coefficients
for(k in 1:6){
  irr1 <- exp(result1[k+1,1])    ##irr值
  std1 <- irr1 * result1[k+1,2]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  
  sensitive11[k+1,2] <- irr_ci1
  k=k+1
}

result1.5 <- geeglm(depress1~sdoh1+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                      factor(badl),id=id,corstr="exchangeable",
                    data=dt0,waves=wave,family=binomial(logit))
result2.5 <- summary(result1.5)$coefficients
p <- result2.5[2,4]
p <- str_c("<", sprintf("%0.4f",p))
p
sensitive11[1,3] <- p



###### 4.3 the time-lagged model ###### 
###### produce the sample using independent variable at time 1 and outcome at time 2
data1 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/1.hrs_sensitive.dta")
data2 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/2.elsa_sensitive.dta")
data3 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/3.share_sensitive.dta")
data4 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/4.charls_sensitive.dta")
data5 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/5.mhas_sensitive.dta")
dt1=as.data.frame(lapply(data1,as.numeric))
dt2=as.data.frame(lapply(data2,as.numeric))
dt3=as.data.frame(lapply(data3,as.numeric))
dt4=as.data.frame(lapply(data4,as.numeric))
dt5=as.data.frame(lapply(data5,as.numeric))
dt <- list(dt1,dt2,dt3,dt4,dt5)


for(i in 1:5){
  dt0=dt[[i]]
  length=length(dt0$id)
  dt0$depress2=c(rep(-1,length))
  dt0$index=c(rep(-1,length))
  for(j in 1:length){
    if(dt0$index[j]==1){
      next
    }else{
      m = which(dt0$id[] == dt0$id[j])
      for(t in 1:(dt0$dup[j]+1)){
        dt0$index[m[t]]=1
        if(t<(dt0$dup[j]+1)){
          dt0$depress2[m[t]]=dt0$depress1[m[t+1]]
        }else{
          next
        }
      }
    }
  }
  dt0 = dt0[dt0$depress2 != -1, ]   #去除最后一次follow-up数据
  dt[[i]]=dt0
}

write_dta(dt[[1]], "D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/1.hrs_sensitive_整理.dta")
write_dta(dt[[2]], "D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/2.elsa_sensitive_整理.dta")
write_dta(dt[[3]], "D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/3.share_sensitive_整理.dta")
write_dta(dt[[4]], "D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/4.charls_sensitive_整理.dta")
write_dta(dt[[5]], "D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/5.mhas_sensitive_整理.dta")

###### independent SDoH and depressive symptoms ###### 
sensitive1 <- read.xlsx(startRow = 1,
                        "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                        sheet="Sheet1")

data <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/1.hrs_sensitive_整理.dta")
dt <- data.frame(data['housefw2'],data['employ1'],
                 data['pension1'],data['edu1'],
                 data['owner1'],data['marriage1'],
                 data['childh'],data['insure1'],
                 data['id'],data['wave'],
                 data['depress2'],data['depress1'],data['age'],
                 data['sex'],data['smoke'],
                 data['drink'],data['chronic'],
                 data['badl'])
dt0=as.data.frame(lapply(dt,as.numeric))

i=1
j=1
k=1

for (j in 1:8) {
  dt0$blank <- dt0[,j]
  result <- geeglm(depress2~factor(blank)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl)+factor(depress1),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
  summary(result)
  result <- summary(result)$coefficients
  irr <- exp(result[2,1])    ##irr值
  std <- irr * result[2,2]
  irr_l <- irr-qnorm((1+0.95)/2)*std    ##irr_low
  irr_u <- irr+qnorm((1+0.95)/2)*std    ##irr_up
  irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
  
  sensitive1[j*3,2] <- irr_ci
  j=j+1
}

result.5 <- geeglm(depress2~factor(housefw2)+factor(employ1)+factor(pension1)+
                     factor(edu1)+factor(owner1)+factor(marriage1)+factor(childh)+
                     factor(insure1)+
                     age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl)+factor(depress1),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
result1 <- summary(result.5)$coefficients
for(k in 1:8){
  irr1 <- exp(result1[k+1,1])    ##irr值
  std1 <- irr1 * result1[k+1,2]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  sensitive1[k*3,3] <- irr_ci1
  
  if (k==8){
    k=9
    n <- length(dt0$id)    ##n值
    case <- dt0 %>% summarize(count = sum(depress2 == 1))    ##case值
    meta[[k]][7,4] <- n
    meta[[k]][7,5] <- case
    meta[[k]][7,6] <- irr1
    meta[[k]][7,7] <- std1
    k=k+1
  }else{
    n <- length(dt0$id)    ##n值
    case <- dt0 %>% summarize(count = sum(depress2 == 1))    ##case值
    meta[[k]][7,4] <- n
    meta[[k]][7,5] <- case
    meta[[k]][7,6] <- irr1
    meta[[k]][7,7] <- std1
    k=k+1
  }
}

###### cumulative SDoH and depressive symptoms ###### 
sensitive11 <- read.xlsx(startRow = 1,
                         "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                         sheet="Sheet2")

dt <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/1.hrs_sensitive_整理.dta")
dt0=as.data.frame(lapply(dt,as.numeric))

k=1

result.5 <- geeglm(depress2~factor(sdoh1)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl)+factor(depress1),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
result1 <- summary(result.5)$coefficients
for(k in 1:6){
  irr1 <- exp(result1[k+1,1])    ##irr值
  std1 <- irr1 * result1[k+1,2]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  
  sensitive11[k+1,2] <- irr_ci1
  k=k+1
}

result1.5 <- geeglm(depress2~sdoh1+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                      factor(badl)+factor(depress1),id=id,corstr="exchangeable",
                    data=dt0,waves=wave,family=binomial(logit))
result2.5 <- summary(result1.5)$coefficients
p <- result2.5[2,4]
p <- str_c("<", sprintf("%0.4f",p))
p
sensitive11[1,3] <- p



###### 4.4  fixed effects regression model ###### 
###### cumulative SDoH and depressive symptoms ###### 
sensitive11 <- read.xlsx(startRow = 1,
                         "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                         sheet="Sheet2")

dt <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/5. 敏感性分析（所有期先后）/2. 分析库/1.hrs_sensitive_整理.dta")
dt0=as.data.frame(lapply(dt,as.numeric))

k=1

result.5 <- geeglm(depress2~factor(sdoh1)+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                     factor(badl)+factor(depress1),id=id,corstr="exchangeable",
                   data=dt0,waves=wave,family=binomial(logit))
result1 <- summary(result.5)$coefficients
for(k in 1:6){
  irr1 <- exp(result1[k+1,1])    ##irr值
  std1 <- irr1 * result1[k+1,2]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  
  sensitive11[k+1,2] <- irr_ci1
  k=k+1
}

result1.5 <- geeglm(depress2~sdoh1+age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                      factor(badl)+factor(depress1),id=id,corstr="exchangeable",
                    data=dt0,waves=wave,family=binomial(logit))
result2.5 <- summary(result1.5)$coefficients
p <- result2.5[2,4]
p <- str_c("<", sprintf("%0.4f",p))
p
sensitive11[1,3] <- p



###### 4.5  using multiple imputation sample ###### 
###### perform multiple imputation
sensitive1 <- read.xlsx(startRow = 1,
                        "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                        sheet="Sheet1")
sensitive11 <- read.xlsx(startRow = 1,
                         "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/7. 敏感性分析/sensitive1.xlsx",
                         sheet="Sheet2")

data <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/8. 敏感性分析（多重插补）/1.原始数据/1.HRS_wave10-13(MI).dta")
dt <- data.frame(data['housefw2'],data['employ1'],
                 data['pension1'],data['edu1'],
                 data['owner1'],data['marriage1'],
                 data['childh'],data['insure1'],
                 data['id'],data['wave'],
                 data['depress1'],data['age'],
                 data['sex'],data['smoke'],
                 data['drink'],data['chronic'],
                 data['badl'])
dt0=as.data.frame(lapply(dt,as.numeric))


dt0$housefw2=as.factor(dt0$housefw2)
dt0$employ1=as.factor(dt0$employ1)
dt0$pension1=as.factor(dt0$pension1)
dt0$edu1=as.factor(dt0$edu1)
dt0$owner1=as.factor(dt0$owner1)
dt0$marriage1=as.factor(dt0$marriage1)
dt0$childh=as.factor(dt0$childh)
dt0$insure1=as.factor(dt0$insure1)
dt0$wave=as.factor(dt0$wave)
dt0$depress1=as.factor(dt0$depress1)
dt0$sex=as.factor(dt0$sex)
dt0$smoke=as.factor(dt0$smoke)
dt0$drink=as.factor(dt0$drink)
dt0$chronic=as.factor(dt0$chronic)
dt0$badl=as.factor(dt0$badl)

sum(is.na(dt0['housefw2']))
sum(is.na(dt0['employ1']))
sum(is.na(dt0['pension1']))
sum(is.na(dt0['edu1']))
sum(is.na(dt0['owner1']))
sum(is.na(dt0['marriage1']))
sum(is.na(dt0['childh']))
sum(is.na(dt0['insure1']))
sum(is.na(dt0['id']))
sum(is.na(dt0['wave']))
sum(is.na(dt0['depress1']))
sum(is.na(dt0['age']))
sum(is.na(dt0['sex']))
sum(is.na(dt0['smoke']))
sum(is.na(dt0['drink']))
sum(is.na(dt0['chronic']))
sum(is.na(dt0['badl']))

imp <- mice(dt0, maxit=0) # We run the mice code with 0 iterations 
predM <- imp$predictorMatrix  # Extract predictor Matrix and methods of imputation 
meth <- imp$method
predM[, c("housefw2")] <- 0  # Setting values of variables I'd like to leave out to 0 in the predictor matrix
predM[, c("employ1")] <- 0
predM[, c("childh")] <- 0
predM[, c("id")] <- 0
predM[, c("wave")] <- 0
predM[, c("age")] <- 0
predM[, c("sex")] <- 0
predM[, c("chronic")] <- 0

poly2 <- c("pension1",'edu1','owner1','marriage1','insure1','depress1',
           'smoke','drink','badl')  # Unordered categorical variable
meth[poly2] <- "logreg"  # Turn their methods matrix into the specified imputation models
meth
imp2 <- mice(dt0, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE)
head(imp2$imp$depress1)
anesimp_long <- mice::complete(imp2, action="long", include = TRUE)  # First, turn the datasets into long format
anesimp_long$sdoh <- with(anesimp_long, 
                          as.numeric(housefw2)+as.numeric(employ1)+
                            as.numeric(pension1)+as.numeric(edu1)+
                            as.numeric(owner1)+as.numeric(marriage1)+
                            as.numeric(childh)+as.numeric(insure1)
                          -8)
anesimp_long$sdoh1 <- with(anesimp_long, 
                           recode(sdoh,"7=6; 8=6"))
anesimp_long$depress1 <- with(anesimp_long, 
                              as.numeric(depress1)-1)
anesimp_long_mids<-as.mids(anesimp_long)

#######单个sdoh #######
i=1
for(i in 1:8){
  if(i==1){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~housefw2+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  } else if(i==2){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~employ1+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  } else if(i==3){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~pension1+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  } else if(i==4){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~edu1+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  } else if(i==5){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~owner1+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  } else if(i==6){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~marriage1+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  } else if(i==7){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~childh+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  } else if(i==8){
    result1 <- with(anesimp_long_mids,
                    geeglm(depress1~insure1+age+sex+smoke+drink+chronic+badl,
                           id=id,corstr="exchangeable",
                           waves=wave,family=binomial(logit)))
  }
  result2 <- summary(pool(result1))
  
  irr1 <- exp(result2[2,2])    ##irr值
  std1 <- irr1 * result2[2,3]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  sensitive1[i*3,2] <- irr_ci1
}

#######independent SDoH#######
result3 <- with(anesimp_long_mids,  
                geeglm(depress1~housefw2+employ1+pension1+
                         edu1+owner1+marriage1+childh+
                         insure1+
                         age+sex+smoke+drink+chronic+
                         badl,id=id,corstr="exchangeable",
                       waves=wave,family=binomial(logit)))
result4 <- summary(pool(result3))
k=1
for(k in 1:8){
  irr1 <- exp(result4[k+1,2])    ##irr值
  std1 <- irr1 * result4[k+1,3]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  sensitive1[k*3,3] <- irr_ci1
  
  if (k==8){
    k=9
    n <- length(dt0$id)    ##n值
    meta[[k]][12,4] <- n
    meta[[k]][12,6] <- irr1
    meta[[k]][12,7] <- std1
    k=k+1
  }else{
    n <- length(dt0$id)    ##n值
    meta[[k]][12,4] <- n
    meta[[k]][12,6] <- irr1
    meta[[k]][12,7] <- std1
    k=k+1
  }
}

####### categorical cumulative SDoH #######
result5 <- with(anesimp_long_mids,  
                geeglm(depress1~factor(sdoh1)+age+sex+smoke+drink+chronic+
                         badl,id=id,corstr="exchangeable",
                       waves=wave,family=binomial(logit)))
result6 <- summary(pool(result5))
j=1
for(j in 1:6){
  irr1 <- exp(result6[j+1,2])    ##irr值
  std1 <- irr1 * result6[j+1,3]    ##std值
  irr_l1 <- irr1-qnorm((1+0.95)/2)*std1    ##irr_low
  irr_u1 <- irr1+qnorm((1+0.95)/2)*std1    ##irr_up
  irr_ci1 <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr_l1),", ",sprintf("%0.2f",irr_u1),")")
  
  sensitive11[j+1,2] <- irr_ci1
}

####### contineous cumulative SDoH #######
result7 <- with(anesimp_long_mids,
                geeglm(depress1~sdoh1+age+sex+smoke+drink+chronic+
                         badl,id=id,corstr="exchangeable",
                       waves=wave,family=binomial(logit)))
result8 <- summary(pool(result7))
p <- result8[2,6]
p <- str_c("<", sprintf("%0.4f",p))
p
sensitive11[1,3] <- p



###### 5. interaction effect analysis ###### 
########### 5.1 using the merged sample of six cohort studies ############
library(haven)  
library(openxlsx)
library(stringr)
library(geepack)
library(ggplot2)

interact <- read.xlsx(startRow = 1,
                      "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/6. 交互作用分析/分析结果1.xlsx",
                      sheet="Sheet1")    ##定义接受数据框

data <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/所有期数据框（调节效应分析）/2. 分析库/6个数据库（合并）.dta")
data1 <- data.frame(data)
dt <- as.data.frame(lapply(data1,as.numeric))

dt$hdi1 <- dt$hdi-mean(dt$hdi)
dt$sdi1 <- dt$sdi-mean(dt$sdi)
dt$sudi1 <- log(dt$sudi)
dt$sudi1 <- dt$sudi1-mean(dt$sudi1)
dt$country11 <- dt$country1-mean(dt$country1)
dt$year1 <- dt$year-mean(dt$year)

i=1
j=5
k=1
for (j in 1:5){
  for (i in 1:3){
    if (i==1){
      dt$blank <- dt$hdi1
    } else if (i==2){
      dt$blank <- dt$sdi1
    } else if (i==3){
      dt$blank <- dt$sudi1
    }
    if (j==1){
      fit1 <- geeglm(depress1~factor(employ1)*blank+factor(edu1)+factor(owner1)+factor(marriage1)+
                       factor(childh)+year1+
                       age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                       factor(badl), data=dt, family=binomial(logit),id=id1,wave=wave2,corstr="exchangeable")
    }else if(j==2){
      fit1 <- geeglm(depress1~factor(employ1)+factor(edu1)*blank+factor(owner1)+factor(marriage1)+
                       factor(childh)+year1+
                       age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                       factor(badl), data=dt, family=binomial(logit),id=id1,wave=wave2,corstr="exchangeable")
    }else if(j==3){
      fit1 <- geeglm(depress1~factor(employ1)+factor(edu1)+factor(owner1)*blank+factor(marriage1)+
                       factor(childh)+year1+
                       age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                       factor(badl), data=dt, family=binomial(logit),id=id1,wave=wave2,corstr="exchangeable")
    }else if(j==4){
      fit1 <- geeglm(depress1~factor(employ1)+factor(edu1)+factor(owner1)+factor(marriage1)*blank+
                       factor(childh)+year1+
                       age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                       factor(badl), data=dt, family=binomial(logit),id=id1,wave=wave2,corstr="exchangeable")
    }else if(j==5){
      fit1 <- geeglm(depress1~factor(employ1)+factor(edu1)+factor(owner1)+factor(marriage1)+
                       factor(childh)*blank+year1+
                       age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                       factor(badl), data=dt, family=binomial(logit),id=id1,wave=wave2,corstr="exchangeable")
    }
    result1 <- summary(fit1)
    result1
    result1.1 <- result1$coefficients
    result1.1[15,1]
    
    irr <- exp(result1.1[j+1,1])
    irr
    sd <- irr*result1.1[j+1,2]
    p <- round(result1.1[j+1,4],4)
    irr_l <- irr-qnorm(0.975)*sd
    irr_u <- irr+qnorm(0.975)*sd
    irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
    irr_ci
    p
    
    irr1 <- exp(result1.1[j+2,1])
    irr1
    sd1 <- irr1*result1.1[j+2,2]
    p1 <- round(result1.1[j+2,4],4)
    irr1_l <- irr1-qnorm(0.975)*sd1
    irr1_u <- irr1+qnorm(0.975)*sd1
    irr1_ci <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr1_l),", ",sprintf("%0.2f",irr1_u),")")
    
    irr2 <- exp(result1.1[15,1])
    irr2
    sd2 <- irr2*result1.1[15,2]
    p2 <- round(result1.1[15,4],4)
    irr2_l <- irr2-qnorm(0.975)*sd2
    irr2_u <- irr2+qnorm(0.975)*sd2
    irr2_ci <- str_c(sprintf("%0.2f",irr2)," (",sprintf("%0.2f",irr2_l),", ",sprintf("%0.2f",irr2_u),")")
    
    if (k==2){
      k=k+1
    }else{
      k=k
    }
    interact[4*k-2,2*i] <- irr_ci
    interact[4*k-2,2*i+1] <- p
    interact[4*k-1,2*i] <- irr1_ci
    interact[4*k-1,2*i+1] <- p1
    interact[4*k,2*i] <- irr2_ci
    interact[4*k,2*i+1] <- p2
    i=i+1
  }
  i=1
  j=j+1
  k=k+1
}


########### 5.2 using the merged sample of cohort studies except for SHARE ############
data1 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/所有期数据框（调节效应分析）/2. 分析库/6个数据库（合并-去share）.dta")
data1 <- data.frame(data1)
dt1 <- as.data.frame(lapply(data1,as.numeric))

dt1$hdi1 <- dt1$hdi-mean(dt1$hdi)
dt1$sdi1 <- dt1$sdi-mean(dt1$sdi)
dt1$sudi1 <- log(dt1$sudi)
dt1$sudi1 <- dt1$sudi1-mean(dt1$sudi1)
dt1$year1 <- dt1$year-mean(dt1$year)

i=1
for (i in 1:3){
  if (i==1){
    dt1$blank <- dt1$hdi1
  } else if (i==2){
    dt1$blank <- dt1$sdi1
  } else if (i==3){
    dt1$blank <- dt1$sudi1
  }
  fit2 <- geeglm(depress1~factor(employ1)+factor(edu1)+factor(owner1)+factor(marriage1)+
                   factor(childh)+factor(insure1)*blank+year1+
                   age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                   factor(badl), data=dt1, family=binomial(logit),id=id1,wave=wave2,corstr="exchangeable")
  result1 <- summary(fit2)
  result1
  result1.1 <- result1$coefficients
  result1.1[16,1]
  
  irr <- exp(result1.1[7,1])
  irr
  sd <- irr*result1.1[7,2]
  p <- round(result1.1[7,4],4)
  irr_l <- irr-qnorm(0.975)*sd
  irr_u <- irr+qnorm(0.975)*sd
  irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
  irr_ci
  p
  
  irr1 <- exp(result1.1[8,1])
  irr1
  sd1 <- irr1*result1.1[8,2]
  p1 <- round(result1.1[8,4],4)
  irr1_l <- irr1-qnorm(0.975)*sd1
  irr1_u <- irr1+qnorm(0.975)*sd1
  irr1_ci <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr1_l),", ",sprintf("%0.2f",irr1_u),")")
  
  irr2 <- exp(result1.1[16,1])
  irr2
  sd2 <- irr2*result1.1[16,2]
  p2 <- round(result1.1[16,4],4)
  irr2_l <- irr2-qnorm(0.975)*sd2
  irr2_u <- irr2+qnorm(0.975)*sd2
  irr2_ci <- str_c(sprintf("%0.2f",irr2)," (",sprintf("%0.2f",irr2_l),", ",sprintf("%0.2f",irr2_u),")")
  irr_ci
  p
  irr1_ci
  p1
  irr2_ci
  p2
  
  interact[26,2*i] <- irr_ci
  interact[26,2*i+1] <- p
  interact[27,2*i] <- irr1_ci
  interact[27,2*i+1] <- p1
  interact[28,2*i] <- irr2_ci
  interact[28,2*i+1] <- p2
  i=i+1
}


###########5.3 using the merged sample of cohort studies except for LASI############
data2 <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/所有期数据框（调节效应分析）/2. 分析库/6个数据库（合并-去lasi）.dta")
data2 <- data.frame(data2)
dt2 <- as.data.frame(lapply(data2,as.numeric))

dt2$hdi1 <- dt2$hdi-mean(dt2$hdi)
dt2$sdi1 <- dt2$sdi-mean(dt2$sdi)
dt2$sudi1 <- log(dt2$sudi)
dt2$sudi1 <- dt2$sudi1-mean(dt2$sudi1)
dt2$year1 <- dt2$year-mean(dt2$year)

i=1
for (i in 1:3){
  if (i==1){
    dt2$blank <- dt2$hdi1
  } else if (i==2){
    dt2$blank <- dt2$sdi1
  } else if (i==3){
    dt2$blank <- dt2$sudi1
  }
  fit3 <- geeglm(depress1~factor(employ1)+factor(pension1)*blank+factor(edu1)+factor(owner1)+factor(marriage1)+
                   factor(childh)+year1+
                   age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                   factor(badl), data=dt2, family=binomial(logit),id=id1,wave=wave2,corstr="exchangeable")
  result1 <- summary(fit3)
  result1
  result1.1 <- result1$coefficients
  result1.1[16,1]
  
  irr <- exp(result1.1[3,1])
  irr
  sd <- irr*result1.1[3,2]
  p <- round(result1.1[3,4],4)
  irr_l <- irr-qnorm(0.975)*sd
  irr_u <- irr+qnorm(0.975)*sd
  irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
  irr_ci
  p
  
  irr1 <- exp(result1.1[4,1])
  irr1
  sd1 <- irr1*result1.1[4,2]
  p1 <- round(result1.1[4,4],4)
  irr1_l <- irr1-qnorm(0.975)*sd1
  irr1_u <- irr1+qnorm(0.975)*sd1
  irr1_ci <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr1_l),", ",sprintf("%0.2f",irr1_u),")")
  
  irr2 <- exp(result1.1[16,1])
  irr2
  sd2 <- irr2*result1.1[16,2]
  p2 <- round(result1.1[16,4],4)
  irr2_l <- irr2-qnorm(0.975)*sd2
  irr2_u <- irr2+qnorm(0.975)*sd2
  irr2_ci <- str_c(sprintf("%0.2f",irr2)," (",sprintf("%0.2f",irr2_l),", ",sprintf("%0.2f",irr2_u),")")
  irr_ci
  p
  irr1_ci
  p1
  irr2_ci
  p2
  
  interact[6,2*i] <- irr_ci
  interact[6,2*i+1] <- p
  interact[7,2*i] <- irr1_ci
  interact[7,2*i+1] <- p1
  interact[8,2*i] <- irr2_ci
  interact[8,2*i+1] <- p2
  i=i+1
}

######## restore results ######
list_data <- list("interaction"=interact)
write.xlsx(list_data, file = "C:/Users/Lenovo/Desktop/搞数据库/4. 数据分析/6. 交互作用分析/分析结果2（第二遍）.xlsx")



###### 6. linear mixed-effects model ######
###### (The results of the analysis were not displayed in the manuscript because 
###### this analysis was not identified as the most suitable model, details see method section

###########using the merged sample of six cohort studies############
library(haven)    ##打开.dta文件
library(lme4)
library(stats)
library(stringr)
library(sjstats)
library(jtools)
install.packages('jtools')

dt <- read_dta("D:/SCI/Database/0. 数据汇总 (6 database)/1 期数据库（调节效应分析）/2. 分析库/6个数据库（合并）.dta")
dt$hdi1 <- (dt$hdi-mean(dt$hdi))/sd(dt$hdi)
dt$sdi1 <- (dt$sdi-mean(dt$sdi))/sd(dt$sdi)
dt$sudi1 <- (dt$sudi-mean(dt$sudi))/sd(dt$sudi)
dt$country11 <- dt$country1-mean(dt$country1)
dt$age1 <- (dt$age-mean(dt$age))/sd(dt$age)

fit1_m <- glmer(depress1~factor(employ1)+factor(edu1)+factor(owner1)+factor(marriage1)*hdi1+
                  factor(childh)+
                  age1+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
                  factor(badl)+(1|country11), data=dt, family=binomial,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e9)))
summ(fit1_m, exp = T)
summary(fit1_m)

result1_m <- summary(fit1_m)
result1_m
result1.1_m <- result1_m$coefficients
result1.1_m[14,1]


fit1 <- glm(depress1~factor(employ1)*di1+factor(edu1)+factor(owner1)+factor(marriage1)+
              factor(childh)+
              age+factor(sex)+factor(smoke)+factor(drink)+factor(chronic)+
              factor(badl), data=dt, family=binomial(logit))
result1 <- summary(fit1)
result1
result1.1 <- result1$coefficients
result1.1[14,1]

i=6
irr <- exp(result1.1[i,1])
irr
sd <- irr*result1.1[i,2]
p <- round(result1.1[i,4],4)
irr_l <- irr-qnorm(0.975)*sd
irr_u <- irr+qnorm(0.975)*sd
irr_ci <- str_c(sprintf("%0.2f",irr)," (",sprintf("%0.2f",irr_l),", ",sprintf("%0.2f",irr_u),")")
irr_ci
p

irr1 <- exp(result1.1[i+1,1])
irr1
sd1 <- irr1*result1.1[i+1,2]
p1 <- round(result1.1[i+1,4],4)
irr1_l <- irr1-qnorm(0.975)*sd1
irr1_u <- irr1+qnorm(0.975)*sd1
irr1_ci <- str_c(sprintf("%0.2f",irr1)," (",sprintf("%0.2f",irr1_l),", ",sprintf("%0.2f",irr1_u),")")

irr2 <- exp(result1.1[14,1])
irr2
sd2 <- irr2*result1.1[14,2]
p2 <- round(result1.1[14,4],4)
irr2_l <- irr2-qnorm(0.975)*sd2
irr2_u <- irr2+qnorm(0.975)*sd2
irr2_ci <- str_c(sprintf("%0.2f",irr2)," (",sprintf("%0.2f",irr2_l),", ",sprintf("%0.2f",irr2_u),")")
irr_ci
p
irr1_ci
p1
irr2_ci
p2





















 
