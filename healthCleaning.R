#######################################################
#
#  PART A:   DATA WRANGLING AND DATASET SPLIT
# 
######################################################


#  The dataset, "home_health.csv", obtained from data.gov is cleaned and missing data imputed in this file.

# Two datasets "AgencyDetails.csv" and "CareDetails.csv" are  created out of the 
# one Home Health Care Agency dataset pulled from the data.gov to perform the analysis.




# Installing libraries

library(tidyverse)
library(mice)


#  Loading in the data obtained from data.gov


health <- read.csv("home_health.csv")



# Checking column names
names(health)      # 71 columns

# Structure of health dataset

str(health)

# Renaming columns


names(health)[names(health) == "CMS.Certification.Number..CCN.."] <- "Cert.Num"
names(health)[names(health) == "Type.of.Ownership"]  <- "Owner.Type"
names(health)[names(health) == "Offers.Nursing.Care.Services"]  <- "Nursing.Care"
names(health)[names(health) == "Offers.Physical.Therapy.Services"]  <- "Phys.Therapy"
names(health)[names(health) == "Offers.Occupational.Therapy.Services"]  <- "Occu.Therapy"
names(health)[names(health) == "Offers.Speech.Pathology.Services"]  <- "Speech.Patho"
names(health)[names(health) == "Offers.Medical.Social.Services"]  <- "Med.Social"
names(health)[names(health) == "Offers.Home.Health.Aide.Services"]  <- "Home.Health"
names(health)[names(health) == "Date.Certified"]  <- "Cert.Date"
names(health)[names(health) == "Quality.of.Patient.Care.Star.Rating"]  <- "Care.Stars"
names(health)[names(health) == "Footnote.for.quality.of.patient.care.star.rating"]  <- "Foot.Care.Stars"
names(health)[names(health) == "How.often.the.home.health.team.began.their.patients..care.in.a.timely.manner"]  <- "Timely.Care"
names(health)[names(health) == "Footnote.for.how.often.the.home.health.team.began.their.patients..care.in.a.timely.manner"]  <- "Foot.Timely.Care"
names(health)[names(health) == "How.often.the.home.health.team.taught.patients..or.their.family.caregivers..about.their.drugs"]  <- "Drug.Teach"
names(health)[names(health) == "Footnote.for.how.often.the.home.health.team.taught.patients..or.their.family.caregivers..about.their.drugs"]  <- "Foot.Drug.Teach"

names(health)[names(health) == "How.often.the.home.health.team.checked.patients..risk.of.falling"]  <- "Fall.Risk"
names(health)[names(health) == "Footnote.for.how.often.the.home.health.team.checked.patients..risk.of.falling"]  <- "Foot.Fall.Risk"
names(health)[names(health) == "How.often.the.home.health.team.checked.patients.for.depression"]  <- "Depress.Check"
names(health)[names(health) == "Footnote.for.how.often.the.home.health.team.checked.patients.for.depression"]  <- "Foot.Depress.Check"
names(health)[names(health) == "How.often.the.home.health.team.determined.whether.patients.received.a.flu.shot.for.the.currnet.flu.season"]  <- "Flu.Shot"
names(health)[names(health) == "Footnote.for.how.often.the.home.health.team.determined.whether.patients.received.a.flu.shot.for.the.currnet.flu.season"]  <- "Foot.Flu.shot"
names(health)[names(health) == "How.often.the.home.health.team.made.sure.that.their.patients.have.received.a.pneumococcal.vaccine..pneumonia.shot.."]  <- "Pneumo.Shot"
names(health)[names(health) == "Footnote.as.how.often.the.home.health.team.made.sure.that.their.patients.have.received.a.pneumococcal.vaccine..pneumonia.shot.."]  <- "Foot.Pneumo.Shot"
names(health)[names(health) == "With.diabetes..how.often.the.home.health.team.got.doctor.s.orders..gave.foot.care..and.taught.patients.about.foot.care"]  <- "Diabetic.Care"
names(health)[names(health) == "Footnote.for.how.often.the.home.health.team.got.doctor.s.orders..gave.foot.care..and.taught.patients.about.foot.care"]  <- "Foot.Diabetic.Care"
names(health)[names(health) == "How.often.patients.got.better.at.walking.or.moving.around"]  <- "Move.Care"
names(health)[names(health) == "Footnote.for.how.often.patients.got.better.at.walking.or.moving.around"]  <- "Foot.Move.Care"



names(health)[names(health) == "How.often.patients.got.better.at.getting.in.and.out.of.bed"]  <- "Bed.Move"
names(health)[names(health) == "Footnote.for.how.often.patients.got.better.at.getting.in.and.out.of.bed"]  <- "Foot.Bed.Move"
names(health)[names(health) == "How.often.patients.got.better.at.bathing"]  <- "Bath.Check"
names(health)[names(health) == "Footnote.for.how.often.patients.got.better.at.bathing"]  <- "Foot.Bath.Check"
names(health)[names(health) == "How.often.patients..breathing.improved"]  <- "Breath.Check"
names(health)[names(health) == "Footnote.for.how.often.patients..breathing.improved"]  <- "Foot.Breath.Check"
names(health)[names(health) == "How.often.patients..wounds.improved.or.healed.after.an.operation"]  <- "Wounds.Care"
names(health)[names(health) == "Footnote.for.how.often.patients..wounds.improved.or.healed.after.an.operation"]  <- "Foot.Wounds.Care"
names(health)[names(health) == "How.often.patients.got.better.at.taking.their.drugs.correctly.by.mouth"]  <- "Drugs.Check"
names(health)[names(health) == "Footnote.for.how.often.patients.got.better.at.taking.their.drugs.correctly.by.mouth"]  <- "Foot.Drugs.Check"
names(health)[names(health) == "How.often.home.health.patients.had.to.be.admitted.to.the.hospital"]  <- "Admit.Hosp"
names(health)[names(health) == "Footnote.for.how.often.home.health.patients.had.to.be.admitted.to.the.hospital"]  <- "Foot.Admit.Hosp"



names(health)[names(health) == "How.often.patients.receiving.home.health.care.needed.urgent..unplanned.care.in.the.ER.without.being.admitted"]  <- "ER.Visit"
names(health)[names(health) == "Footnote.for.how.often.patients.receiving.home.health.care.needed.urgent..unplanned.care.in.the.ER.without.being.admitted"]  <- "Foot.ER.Visit"
names(health)[names(health) == "Footnote"]  <- "Foot.Note"
names(health)[names(health) == "How.often.patients.developed.new.or.worsened.pressure.ulcers"]  <- "Ulcers.New"
names(health)[names(health) == "Footnote.for.how.often.patients.developed.new.or.worsened.pressure.ulcers"]  <- "Foot.Ulcers.New"
names(health)[names(health) == "How.often.physician.recommended.actions.to.address.medication.issues.were.completely.timely"]  <- "Doctor.Rec"
names(health)[names(health) == "Footnote.for.how.often.physician.recommended.actions.to.address.medication.issues.were.completely.timely"]  <- "Foot.Doctor.Rec"
names(health)[names(health) == "Footnote.for.DTC.Risk.Standardized.Rate"]  <- "Foot.DTC.Risk"
names(health)[names(health) == "Footnote.for.PPR.Risk.Standardized.Rate"]  <- "Foot.PPR.Risk"
names(health)[names(health) == "How.much.Medicare.spends.on.an.episode.of.care.at.this.agency..compared.to.Medicare.spending.across.all.agencies.nationally"]  <- "MedCare.Rate"
names(health)[names(health) == "Footnote.for.how.much.Medicare.spends.on.an.episode.of.care.at.this.agency..compared.to.all.agencies.nationally"]  <- "Foot.MedCare.Rate"
names(health)[names(health) == "No..of.episodes.to.calc.how.much.Medicare.spends.per.episode.of.care.at.agency..compared.to.spending.at.all.agencies..national."]  <- "MedCare.Epsodes"





# All the columns starting with "Footnote.." were empty and the ones with "DTC" and "PPR" ne=ot needed for analysis

# Using tidyverse library to drop columns containing "Foot", "DTC", and "PPR"

health1 <- health %>% select(-contains("Foot"),-contains("DTC"), -contains("PPR"))  # Now 35 columns




# Subsetting the few columns to represent provider info and dropping them from the dataset

provider_info  <-  health1[c(1:15)]


# Moving the position of the Cert.date

provider.data   <- provider_info[c(1:3,15,4:14)]    # 15 columns


# Checking for na's in the provider data

is.na(provider.data)

nacount <-  colSums(is.na(provider.data))
nacount       #no NA's



# Dropping extra columns from the health1 data and renaming 

health.data  <- health1[-(2:15)]        # 21 columns



# Testing for missing data in health.data

is.na(health.data)



# To find the total missing values in each column


nacount_h <- colSums(is.na(health.data))

nacount_h     # in each column. 2827 in the Care.Stars column an none in the state column

nacount_hsum  <- sum(colSums(is.na(health.data)))    #Total sum of all columns 55835

sum(complete.cases(health.data))  # Complete rows without NA's 4057 out of 11170




# Using mice function to impute missing values excluding the state column

impute <- mice(health.data[ , 2:21], m = 3, seed = 123)

impute

# Saving the first column, state, as there were no missing data 
state <- health.data[ , 1]     


# Checking a row to see which imputation method to use

impute$imp$Timely.Care

health.data[118, ]
summary(health.data$Timely.Care)   #mean 93.71


# Completing the health.data using the 2nd imputation method and saving into new data

newHealth  <-  complete(impute, 2)
health.Data <- cbind(state, newHealth)

# Checking for missing data after imputation

colSums(is.na(health.Data))    # no missing data



#############################################################
# Final two datasets created from the original dataset are: 
# provider.Data and health.Data 

# Saving these two files as two separate csv files

write.csv(provider.data, "AgencyDetails.csv", row.names = FALSE)
write.csv(health.Data, "CareDetails.csv", row.names = FALSE)
