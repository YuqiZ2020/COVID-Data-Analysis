# ---------- 1. the proportion of arthritis that are rheumatoid:
ratio.rheumatoid.arthritis = 0.2120388


# ---------- 1. obesity and diabetes prevalence by age
## (1) prevalence of each obesity sub-category by age group
rs = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/data_created/individual_rs_covariates.rds')
## Age 15-44
rs = rs[rs$age<45,]
# obesity
prev.obesity1 = sum(rs$obesity1==1)/nrow(rs) # 0.1588229
prev.obesity2 = sum(rs$obesity2==1)/nrow(rs) # 0.06904844
prev.obesity3 = sum(rs$obesity3==1)/nrow(rs) # 0.07855262


## Age 45-74
rs = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/data_created/individual_rs_covariates.rds')
rs = rs[(rs$age>=45)&(rs$age<75),]
# obesity
prev.obesity1 = sum(rs$obesity1==1)/nrow(rs) # 0.2053906
prev.obesity2 = sum(rs$obesity2==1)/nrow(rs) # 0.08825948
prev.obesity3 = sum(rs$obesity3==1)/nrow(rs) # 0.08853358


## Age 75+
rs = readRDS('~/Dropbox/NHANES_risk_score/500cities_data/Updated_July_06_2020/data_created/individual_rs_covariates.rds')
rs = rs[rs$age>=75,]
# obesity
prev.obesity1 = sum(rs$obesity1==1)/nrow(rs) # 0.1628959
prev.obesity2 = sum(rs$obesity2==1)/nrow(rs) # 0.04442616
prev.obesity3 = sum(rs$obesity3==1)/nrow(rs) # 0.04524887



## (2) diabetes: use this proportion for all 3 age groups
ratio.unctrl.ctrl = 0.6133612 # 
pr.unctrl = ratio.unctrl.ctrl/(1+ratio.unctrl.ctrl) # this is the proportion AMONG DIABETES that are uncontrolled.
# You can multiply the prevalence of diabetes by this proportion to get the prevalence of uncontrolled diabetes.
