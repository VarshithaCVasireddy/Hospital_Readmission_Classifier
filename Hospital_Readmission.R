rm(list = ls())
library(tidyverse)
library(VIM)
# library(MASS)
library(caret)
library(MLmetrics)

# install.packages('doParallel');
library(doParallel)
registerDoParallel(cores=3)

# Generating the Data Report-------------------------------------------------------------
gen.data.report.num <- function(df) {
  # Function to generate summary of a numeric column
  gen.num.summary <- function(x){
    Q1<-function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[2] }
    Q3<-function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[4] }
    
    c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
      min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
      max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
  }
  
  # Extracting the Numeric columns and generate the Numeric summary
  num.df <- df %>% 
    dplyr::select(where(is.numeric)) %>% 
    dplyr::as_tibble()
  
  if(length(num.df) > 0) {
    cols <- colnames(num.df)
    
    plot(num.df %>%
           pivot_longer(cols = cols) %>%
           as.data.frame() %>%
           ggplot(aes(x=value)) +
           geom_histogram(bins = sqrt(length(df))) +
           facet_wrap(~ name, scales = "free"))
    
    return(num.df %>%
             dplyr::summarise(dplyr::across(.fns = gen.num.summary)) %>%
             cbind(
               stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd")) %>%
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             mutate(missing_pct = 100*missing/n,
                    unique_pct = 100*unique/n) %>%
             select(variable, n, missing, missing_pct, unique, unique_pct, everything()))
  }
  
  return(data.frame(Message="No Num values in data"))
}

gen.data.report.nnum <- function(df) {
  # Function to generate summary of a non-numeric column
  gen.n.num.summary <- function(x){
    getmodes <- function(v,type=1) {
      tbl <- table(v)
      m1<-which.max(tbl)
      if(length(tbl) < type) { return(NA) }
      
      if (type==1) {
        return (names(m1)) #1st mode
      }
      else if (type==2) {
        return (names(which.max(tbl[-m1]))) #2nd mode
      }
      else if (type==-1) {
        return (names(which.min(tbl))) #least common mode
      }
      else {
        stop("Invalid type selected")
      }
    }
    
    getmodesCnt <- function(v,type=1) {
      tbl <- table(v)
      m1<-which.max(tbl)
      if(length(tbl) < type) { return(NA) }
      
      if (type==1) {
        return (max(tbl)) #1st mode freq
      }
      else if (type==2) {
        return (max(tbl[-m1])) #2nd mode freq
      }
      else if (type==-1) {
        return (min(tbl)) #least common freq
      }
      else {
        stop("Invalid type selected")
      }
    }
    
    freRat <- function(x){ getmodesCnt(x, type = 1)/getmodesCnt(x,type = 2) }
    
    c(length(x), n_distinct(x), sum(is.na(x)),round(freRat(x),digits = 2), getmodes(x,type = 1),
      getmodesCnt(x,type= 1), getmodes(x, type = 2), getmodesCnt(x,type= 2),
      getmodes(x,type= -1),getmodesCnt(x,type= -1))
  }
  
  # Extracting the Non-Numeric Columns
  nnum.df <- df %>% 
    dplyr::transmute(across(!where(is.numeric), as.factor)) %>% 
    as_tibble() 
  
  if(length(nnum.df) > 0) {
    cols <- colnames(nnum.df)
    
    return(nnum.df %>%
             dplyr::summarise(dplyr::across(.fns = gen.n.num.summary)) %>%
             cbind(
               stat=c("n","unique","missing","freqRatio","1st mode","1st mode freq","2st mode",
                      "2st mode freq", "least common","least common freq")) %>%
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             dplyr::mutate(dplyr::across(c(missing, unique, n, freqRatio, `1st mode freq`,
                                           `2st mode freq`, `least common freq`),as.numeric)) %>%
             dplyr::mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>%
             select(variable, n, missing, missing_pct, unique, unique_pct, everything()))
  }
  
  return(data.frame(Message="No Num values in data"))
}

train.data <- read.csv("data/Train.csv", stringsAsFactors = F, na.strings = c("", "NA")) %>%
  mutate(admission_type=as.factor(admission_type), discharge_disposition=as.factor(discharge_disposition),
         admission_source=as.factor(admission_source), readmitted=as.logical(readmitted))

test.data <- read.csv("data/Test.csv", stringsAsFactors = F, na.strings = c("", "NA")) %>%
  mutate(admission_type=as.factor(admission_type), discharge_disposition=as.factor(discharge_disposition),
         admission_source=as.factor(admission_source))

gen.data.report.num(train.data) %>% View(title = "Train Data Numeric Data Report")
gen.data.report.nnum(train.data) %>% View(title = "Train Data Non-Numeric Data Report")

# Data Preprocessing -----------------------------------------------------------------------------

## Missing Values imputation =====================================================================
### Numeric Variables Imputation: Using Mean imputation
train.data$indicator_level[is.na(train.data$indicator_level)] <- 
  mean(train.data$indicator_level, na.rm = T)
train.data$time_in_hospital[is.na(train.data$time_in_hospital)] <- 
  mean(train.data$time_in_hospital, na.rm = T)
train.data$num_lab_procedures[is.na(train.data$num_lab_procedures)] <- 
  mean(train.data$num_lab_procedures, na.rm = T)

### Non-Numeric Variables Imputation: Using Mode imputation
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

train.data$race[is.na(train.data$race)] <- getmodes(train.data$race)
train.data$diagnosis[is.na(train.data$diagnosis)] <- getmodes(train.data$diagnosis)
train.data$gender[is.na(train.data$gender)] <- getmodes(train.data$gender)
train.data$age[is.na(train.data$age)] <- getmodes(train.data$age)

### Using nearest neighbor imputation for medical_speciality and payer_code variables 
train.data.imp <- kNN(train.data, variable = c("payer_code", "medical_specialty"), k=6)

### Generating reports of Imputed Training dataset to check the overall missing values
gen.data.report.num(train.data.imp) %>% View(title = "Imputed Train Data Numeric Data Report")
gen.data.report.nnum(train.data.imp) %>% View(title = "Imputed Train Data Non-Numeric Data Report")

# Imputing missing values in Test data
test.data$time_in_hospital[is.na(test.data$time_in_hospital)] <- mean(test.data$time_in_hospital, na.rm = T)
test.data$age[is.na(test.data$age)] <- getmodes(test.data$age)
test.data$indicator_level[is.na(test.data$indicator_level)] <- mean(test.data$indicator_level, na.rm = T)


## Factor Level Collapsing ================================================================
### Combining Imputed Train and Test datasets to perform factor collapsing
trainIds <- unique(train.data.imp$patientID)
testIds <- unique(test.data$patientID)

# Check for any common values in train and test datasets
trainIds[which(trainIds %in% testIds)] # Shows integer(0)

complete.data <- train.data.imp %>% dplyr::select(-readmitted) %>% bind_rows(test.data)

# Function to collapse Factor levels and change missing values to Other level aswell
collapse.factor <- function(x) {
  x <- fct_lump_n(x, n=5)
  x <- fct_explicit_na(x, na_level = "Other")
  return(x)
}

collapse.diagnosis <- function(x) {
  if(is.na(x)) return(-1)
  
  x = as.character(x)
  if(startsWith(x, "V")) return(18)
  if(startsWith(x, "E")) return(19)
  
  x = as.numeric(x)
  if(between(x, 1, 139)) return(1)
  if(between(x, 140, 239)) return(2)
  if(between(x, 240, 279)) return(3)
  if(between(x, 280, 289)) return(4)
  if(between(x, 290, 319)) return(5)
  if(between(x, 320, 389)) return(6)
  if(between(x, 390, 459)) return(7)
  if(between(x, 460, 519)) return(8)
  if(between(x, 520, 579)) return(9)
  if(between(x, 580, 629)) return(10)
  if(between(x, 630, 679)) return(11)
  if(between(x, 680, 709)) return(12)
  if(between(x, 710, 739)) return(13)
  if(between(x, 740, 759)) return(14)
  if(between(x, 760, 779)) return(15)
  if(between(x, 780, 799)) return(16)
  if(between(x, 800, 999)) return(17)
  
  return(-1)
}

# Column wise collapsing medication information
medications = c("metformin","repaglinide","nateglinide","chlorpropamide","glimepiride",
                "acetohexamide","glipizide","glyburide","tolbutamide","pioglitazone",
                "rosiglitazone","acarbose","miglitol","troglitazone","tolazamide","examide",
                "citoglipton","insulin","glyburide.metformin","glipizide.metformin",
                "glimepiride.pioglitazone","metformin.rosiglitazone","metformin.pioglitazone")

complete.data$diagnosis <- lapply(complete.data$diagnosis, collapse.diagnosis)

complete.data.collapsed <- complete.data %>%
  mutate(medication_up_count = rowSums(.[medications] == "Up"),
         medication_down_count = rowSums(.[medications] == "Down"),
         medication_steady_count = rowSums(.[medications] == "Steady")) %>%
  mutate(age=case_when(age == "[0-10)" ~ 1,
                       age == "[10-20)" ~ 1,
                       age == "[20-30)" ~ 1,
                       age == "[30-40)" ~ 1,
                       age == "[40-50)" ~ 2,
                       age == "[50-60)" ~ 3,
                       age == "[60-70)" ~ 4,
                       age == "[70-80)" ~ 5,
                       age == "[80-90)" ~ 6,
                       age == "[90-100)" ~ 6)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), collapse.factor)) %>%
  group_by(patientID) %>%
  summarise(race=first(race),
            gender=first(gender),
            admission_type=first(admission_type),
            discharge_disposition=first(discharge_disposition),
            medical_specialty=first(medical_specialty),
            diagnosis=first(diagnosis),
            diabetesMed=first(diabetesMed),
            admission_source=first(admission_source),
            max_glu_serum=first(max_glu_serum),
            A1Cresult=first(A1Cresult),
            
            age=first(age),
            time_in_hospital=mean(time_in_hospital),
            indicator_level=mean(indicator_level),
            num_lab_procedures=sum(num_lab_procedures),
            num_procedures=sum(num_procedures),
            num_medications=sum(num_medications),
            number_outpatient=sum(number_outpatient),
            number_emergency=sum(number_emergency),
            number_inpatient=sum(number_inpatient),
            number_diagnoses=sum(number_diagnoses),
            medication_up_count=sum(medication_up_count),
            medication_down_count=sum(medication_down_count),
            medication_steady_count=sum(medication_steady_count)
            )

complete.data.collapsed <- complete.data.collapsed %>%
  select(-nearZeroVar(complete.data.collapsed))

# Bar Chat showing the distribution of age in complete.data.collapsed
ggplot(data=complete.data.collapsed) +
  geom_bar(aes(x=age))

# Splitting Train and Test data from the complete data
X.train <- complete.data.collapsed %>%
  filter(patientID %in% trainIds) %>%
  inner_join(train.data.imp[,c("patientID", "readmitted")], by="patientID") %>%
  mutate(readmitted=ifelse(readmitted==1, "Yes", "No"))
X.test <- complete.data.collapsed %>%
  filter(patientID %in% testIds)

# Generating report for train data
gen.data.report.num(X.train) %>% View(title = "Cleaned Train Numeric Data Report")
gen.data.report.nnum(X.train) %>% View(title = "Cleaned Train Non-Numeric Data Report")

# Data Report for test data
gen.data.report.num(X.test) %>% View(title = "Cleaned Test Numeric Data Report")
gen.data.report.nnum(X.test) %>% View(title = "Cleaned Test Non-Numeric Data Report")


# Modelling --------------------------------------------------------------------

## Extreme gradient boosting (XGBoost) ===================================
trControl <- trainControl(method = "repeatedcv", number = 5, repeats= 3)

xgb.grid <- expand.grid(nrounds = 500,
                        max_depth = 5,
                        eta = 0.05,
                        gamma = 0.01,
                        colsample_bytree = 0.5,
                        min_child_weight = 0,
                        subsample = 0.75)

xgb.fit <- train(as.factor(readmitted) ~.-patientID, 
                 data = X.train, method = "xgbTree", trControl=trControl,verbose = F, verbosity = 0,
                 tuneGrid=xgb.grid, preProc= c("center","scale"))

xgb.fit

xgb.train.preds <- predict(xgb.fit, newdata = X.train, type = "prob")
LogLoss(xgb.train.preds$Yes, ifelse(X.train$readmitted=="Yes", 1, 0))

plot(xgb.fit)

xgb.test.preds <- predict(xgb.fit, newdata = X.test, type = "prob")
submission <- data.frame(patientID=X.test$patientID, 
                         predReadmit=xgb.test.preds$Yes)

View(submission)
write.csv(submission, "xg.submission.csv", row.names = F)

## Logistic Regression ==================================================
glm.fit <- train(as.factor(readmitted) ~.-patientID, 
      data = X.train, method = "glm", trControl=trControl)

glm.fit

glm.train.preds <- predict(glm.fit, newdata = X.train, type = "prob")
LogLoss(glm.train.preds$Yes, ifelse(X.train$readmitted=="Yes", 1, 0))

glm.test.preds <- predict(glm.fit, newdata = X.test, type = "prob")
submission <- data.frame(patientID=X.test$patientID, 
                         predReadmit=glm.test.preds$Yes)
View(submission)
write.csv(submission, "glm.submission.csv", row.names = F)

## Random Forest ========================================================
rf.fit <- train(as.factor(readmitted) ~.-patientID,
                data = X.train, method = "rf", trControl=trControl,
                preProc= c("center","scale"))

rf.fit

rf.train.preds <- predict(rf.fit, newdata = X.train, type = "prob")
LogLoss(rf.train.preds$Yes, ifelse(X.train$readmitted=="Yes", 1, 0))

rf.test.preds <- predict(rf.fit, newdata = X.test, type = "prob")
submission <- data.frame(patientID=X.test$patientID, 
                         predReadmit=rf.test.preds$Yes)
View(submission)
write.csv(submission, "rf.submission.csv", row.names = F)

## MARS Model ==========================================================
mars.fit <- train(as.factor(readmitted) ~.-patientID,
                  data = X.train, method = "earth", trControl=trControl,
                  preProc= c("center","scale"))
mars.fit

mars.train.preds <- predict(mars.fit, newdata = X.train, type = "prob")
LogLoss(mars.train.preds$Yes, ifelse(X.train$readmitted=="Yes", 1, 0))

mars.test.preds <- predict(mars.fit, newdata = X.test, type = "prob")
submission <- data.frame(patientID=X.test$patientID, 
                         predReadmit=mars.test.preds$Yes)
View(submission)
write.csv(submission, "mars.submission.csv", row.names = F)

## Descision Tree =====================================================
dt.fit <- train(as.factor(readmitted) ~.-patientID,
                  data = X.train, method = "rpart", trControl=trControl)
dt.fit

dt.train.preds <- predict(dt.fit, newdata = X.train, type = "prob")
LogLoss(dt.train.preds$Yes, ifelse(X.train$readmitted=="Yes", 1, 0))









