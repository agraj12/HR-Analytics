library(shiny)
library(shinyWidgets)
library(VIM)
library(xgboost)
library(ggthemes)

ui <- fluidPage(
  
  h1(id="big-heading", "ANALYTICS FOR WORKFORCE"),
  tags$style(HTML("#big-heading{color: DarkBlue;text-align: center; font: bold 40px Verdana}")),
  
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("CURRENT WORKFORCE ANALYSIS",br(),br(), 
               sidebarLayout(
                 sidebarPanel( 
                   actionButton(inputId="clicks1",style='padding:20px; font-size:120%; height:60px; width:175px', label = "Salary Hike"),br(),br(),
                   actionButton(inputId="clicks2",style='padding:20px; font-size:120%; height:60px; width:175px', label = "Job Satisfaction"),br(),br(),
                   actionButton("clicks3",style='padding:20px; font-size:120%; height:60px; width:175px', "Past Promotion"),br(),br(),
                   actionButton("clicks4",style='padding:20px; font-size:120%; height:60px; width:175px', "Monthly Income"),br(),br(),
                   actionButton("clicks5",style='padding:20px; font-size:120%; height:60px; width:175px', "Education"),br(),br()
                 ),
                 mainPanel(
                   plotOutput("distPlot")
                 )
               )),
      tabPanel("CHECK EMPLOYEE ATTRITION PROBABILITY",br(),br(),
               sidebarLayout(
                 sidebarPanel(width = 4,
                              selectInput("education", "Education Levels", c('Below College','College','Bachelor','Masters','Doctor')),
                              selectInput("job", "Job Involvement Level", c('Low','Medium','High','Very High')),
                              selectInput("performance", "Performance Rating", c('Low','Good','Excellent','Outstanding')),
                              selectInput("balance", "Work-Life Balance", c('Bad','Good','Better','Best')),
                              selectInput("gender", "Gender", c('Female','Male')),
                              selectInput("married", "Marital Status", c('Single','Married','Divorced')),
                              sliderInput("age", "Age:",
                                          min = 20, max = 70, value = 30),
                              sliderInput("salary", "Percentage Salary Hike:",
                                          min = 10, max = 25, value = 15),
                              actionButton("clicks6",style='padding:20px; font-size:120%; height:60px; width:175px', "Predict")
                 ),
                 mainPanel(
                   h1("Results Based on Inputs:"),
                   h1(htmlOutput("result")),br(),br(),
                   h2("Summary of Inputted Parameters:"),
                   h2(htmlOutput("text1"))
                 )
               )
      )
    ),style='width: 800px; height: 800px'
  )
)

server <- function(input, output) {
  
  
  getwd()
  gen <- read.table(file='general_data.csv',
                    header=T, sep=",")
  emp_sur <- read.table(file='employee_survey_data.csv',
                        header=T, sep=",")
  mgr_sur <- read.table(file='manager_survey_data.csv',
                        header=T, sep=",")
  in_time <- read.table(file='in_time.csv',
                        header=T, sep=",")
  out_time <- read.table(file='out_time.csv',
                         header=T, sep=",")
  head(gen)
  
  str(gen)
  
  # De-Duplication is not needed 
  #Missing values 
  sum(is.na(gen))
  
  gen2<-gen
  
  (colSums(is.na(gen))/nrow(gen))*100  # NumCompaniesWorked & TotalWorkingYears have missing values
  
  hist(gen$NumCompaniesWorked,freq=T)
  
  hist(gen$TotalWorkingYears,freq=T)
  
  
  sum(is.na(gen$NumCompaniesWorked))
  sum(is.na(gen$TotalWorkingYears))
  
  #################
  
  summary(emp_sur) 
  
  summary(mgr_sur) 
  
  table(is.na(mgr_sur)) 
  
  table(is.na(emp_sur)) 
  
  
  library(mice) 
  
  md.pattern(emp_sur) 
  
  
  library(VIM) 
  
  aggr_plot <- aggr(emp_sur, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,  
                    
                    labels=names(emp_sur), cex.axis=.5, gap=1, 
                    
                    ylab=c("Histogram of missing data","Pattern")) 
  
  
  temp <- mice(emp_sur, maxit=50,meth='pmm',seed=500 ) 
  
  
  summary(temp) 
  
  imp_emp_sur <- complete(temp,1) 
  
  summary(imp_emp_sur) 
  
  
  combined <- merge(x=gen, y=imp_emp_sur, by="EmployeeID") 
  
  combined <- merge(x=combined, y= mgr_sur,by="EmployeeID") 
  
  
  library(dplyr) 
  
  glimpse(combined) 
  
  
  temp1 <- mice(combined, maxit=50,meth='pmm',seed=500 ) 
  
  
  summary(temp1) 
  
  gen_tot <- complete(temp1,1) 
  
  sum(is.na(gen_tot)) #no missing values
  
  combined<-gen_tot
  
  combined$Attrition <- as.integer(ifelse(combined$Attrition=="Yes",1,0)) 
  
  class(combined$Attrition) 
  
  names(combined)[3] <- "y" 
  
  
  
  class(combined$y) 
  combined$y <- factor(combined$y)
  ##############################################################################
  ##############With ggplot########################################
  
  combined <- subset(combined, select = -c(EmployeeCount,Over18,StandardHours)) 
  
  str(combined)
  
  library(caret) 
  
  dummies <- dummyVars(y ~ ., data = combined)            # create dummies for Xs 
  
  ex <- data.frame(predict(dummies, newdata = combined))  # actually creates the dummies 
  
  names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names 
  
  combined <- cbind(combined$y, ex)                              # combine target var with Xs 
  
  names(combined)[1] <- "y"                               # name target var 'y' 
  
  rm(dummies, ex)     
  
  #################
  
  in2<-in_time
  out2<-out_time
  
  #removing all thoses columns containing all NAs
  in2 <- in2[,colSums(is.na(in2))<nrow(in2)]
  out2 <- out2[,colSums(is.na(out2))<nrow(out2)]
  #number of columns reduced from 262 to 250
  
  #storing the ID seperately and converting data into date time format 
  in2 <- as.data.frame(lapply( in2[, -1], as.POSIXlt))
  out2 <- as.data.frame(lapply( out2[, -1],as.POSIXlt))
  
  #creating a dataframe to store total hours worked
  totaltime <- data.frame(matrix(ncol=ncol(in2),nrow=nrow(out2)))
  colnames(totaltime) <- colnames(in2)
  #checking if both intime and outime for a given date is present
  # and then storing the value in total time.
  totaltime<-out2-in2
  
  # rounding the values
  totaltime<- as.data.frame(lapply(totaltime,round,digits=2))
  
  #calculating the average time worked per day for every employee
  t1<-as.data.frame(lapply(totaltime[,-1],as.numeric))
  t1["AvgWorkTime"]<- rowMeans(t1, na.rm = TRUE)
  t1["AvgWorkTime"]<-round(t1["AvgWorkTime"],digits = 2)
  
  #calculating the total leaves taken by each employee and storing in a new column
  t1["LeavesTaken"]<-rowSums(is.na(t1))
  
  #appending the ID to the dataframe
  t1 <- cbind(in_time$X, t1)
  names(t1)[1] <- "ID"
  #replacing NAs with zeros in every column
  t1[is.na(t1)] <- 0
  
  time<-subset(t1, select = c(ID,AvgWorkTime,LeavesTaken)) 
  
  combin_time<-merge(x=combined, y=time, by.x="EmployeeID",by.y="ID")
  
  
  
  # Identify correlated variables
  
  descrCor <-  cor(combin_time[,3:ncol(combin_time)])              # correlation matrix
  highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80)        # num Xs with cor > t
  summary(descrCor[upper.tri(descrCor)])                           # summarize the cors
  
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
  filteredDescr <- combin_time[,3:ncol(combin_time)][,-highlyCorDescr] # remove those specific columns
  descrCor2 <- cor(filteredDescr)  
  
  summary(descrCor2[upper.tri(descrCor2)])
  combin_time <- cbind(combin_time$y, filteredDescr)
  names(combin_time)[1] <- "y"
  
  rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr) 
  
  # Identify linear dependencies and remove them to reduce the issue of 
  # perfect collinearity using the findLinearCombos() function
  
  y <- combin_time$y
  
  # create a column of 1s. This will help identify all the right linear combos
  combin_time <- cbind(rep(1, nrow(combin_time)), combin_time[2:ncol(combin_time)])
  names(combin_time)[1] <- "ones"
  
  # identify the columns that are linear combos
  comboInfo <- findLinearCombos(combin_time)
  comboInfo
  
  # remove columns identified that led to linear combos
  combin_time <- combin_time[, -comboInfo$remove]
  
  # remove the "ones" column in the first column
  combin_time <- combin_time[, c(2:ncol(combin_time))]
  
  # Add the target variable back to our data.frame
  combin_time <- cbind(y, combin_time)
  
  rm(y, comboInfo)  # clean up
  
  # Remove features with limited variation using nearZeroVar() 
  ################################################################################
  
  nzv <- nearZeroVar(combin_time, saveMetrics = TRUE)
  combin_time <- combin_time[, c(TRUE,!nzv$zeroVar[2:ncol(combin_time)])]
  
  head(nzv)
  
  # PreProcess() using a min-max normalization.
  
  preProcValues <- preProcess(combin_time[,2:ncol(combin_time)], method = c("range"))
  combin_time <- predict(preProcValues, combin_time)
  combined_xg <- combin_time
  
  combin_time$y <- as.factor(combin_time$y)
  levels(combin_time$y) <- make.names(levels(factor(combin_time$y)))
  levels(combin_time$y)
  # levels of a factor are re-ordered so that the level specified is first and 
  # "X1" is what we are predicting. The X before the 1 has nothing to do with the
  # X variables. It's just something weird with R. 'X1' is the same as 1 for the Y 
  # variable and 'X0' is the same as 0 for the Y variable.
  combin_time$y <- relevel(combin_time$y,"X1")
  
  #Create a 80/20 train/test set using createDataPartition()
  
  
  library(xgboost)
  set.seed(1234)
  combined_xg$y <- as.factor(combined_xg$y)
  
  
  levels(combined_xg$y) <- make.names(levels(factor(combined_xg$y)))
  levels(combined_xg$y)
  # levels of a factor are re-ordered so that the level specified is first and 
  # "X1" is what we are predicting. The X before the 1 has nothing to do with the
  # X variables. It's just something weird with R. 'X1' is the same as 1 for the Y 
  # variable and 'X0' is the same as 0 for the Y variable.
  combined_xg$y <- relevel(combined_xg$y,"X1")
  
  
  inTrain_xg <- createDataPartition(y = combined_xg$y,   # outcome variable
                                    p = .70,   # % of training data you want
                                    list = F)
  train_xg <- combined_xg[inTrain_xg,]  # training data set
  test_xg <- combined_xg[-inTrain_xg,]  # test data set
  x_train <- xgb.DMatrix(as.matrix(train_xg %>% select(-y)))
  y_train <- train_xg$y
  x_test <- xgb.DMatrix(as.matrix(test_xg %>% select(-y)))
  y_test <- test_xg$y
  xgb_control <- trainControl(
    method = 'cv',
    number = 5,
    allowParallel = TRUE,
    verboseIter = TRUE,
    returnData = FALSE,
    classProbs = T,  # if you want probabilities
    summaryFunction = twoClassSummary # for classification
  )
  xgbGrid <- expand.grid(nrounds = 200,  # this is n_estimators in the python code above
                         max_depth =  20,
                         colsample_bytree = 0.7,
                         ## The values below are default values in the sklearn-api. 
                         eta = 0.1,
                         gamma=0,
                         min_child_weight = 1,
                         subsample = 1
  )
  
  set.seed(0) 
  xgb_model = train(
    x_train, y_train,  
    trControl = xgb_control,
    tuneGrid = xgbGrid,
    method = "xgbTree"
  )
  xgb_model
  
  predicted = predict(xgb_model, x_test)
  xgb_trp <- predict(xgb_model, newdata=x_train, type='prob')[,1]
  xgb_trc <- predict(xgb_model, newdata=x_train)
  xgb_tep <- predict(xgb_model, newdata=x_test, type='prob')[,1]
  xgb_tec <- predict(xgb_model, newdata=x_test)
  
  (cm <- confusionMatrix(data=xgb_trc, y_train))
  (testCM <- confusionMatrix(data=xgb_tec, y_test))
  
  
  
  observeEvent(input$clicks1, {
    print(as.numeric(input$clicks1))
    output$distPlot <- renderPlot({
      ggplot(data = combined,aes(PercentSalaryHike,fill = y))+geom_bar()+theme_few()+scale_fill_manual( values=c("grey", "light blue"))
    })
  })
  
  
  observeEvent(input$clicks2, {
    print(as.numeric(input$clicks2))
    output$distPlot <- renderPlot({
      ggplot(data = combined,aes(JobSatisfaction,fill = y))+geom_density(alpha = 0.7)+scale_fill_manual( values=c("grey", "light blue")) + xlab("Job Satisfaction Rating") 
    })
  })
  
  
  observeEvent(input$clicks3, {
    print(as.numeric(input$clicks3))
    output$distPlot <- renderPlot({
      ggplot(data = combined,aes(YearsSinceLastPromotion,fill = y))+geom_bar()+theme_few()+scale_fill_manual( values=c("grey", "light blue"))
    })
  })
  
  observeEvent(input$clicks4, {
    print(as.numeric(input$clicks4))
    output$distPlot <- renderPlot({
      ggplot(data=combined, aes(x=MonthlyIncome,y=PercentSalaryHike,color=y))+ geom_point()
    })
  })
  
  
  observeEvent(input$clicks5, {
    print(as.numeric(input$clicks5))
    output$distPlot <- renderPlot({
      ggplot(data = combined,aes(Education,fill = y))+geom_density(alpha = 0.7)+scale_fill_manual( values=c("grey", "light blue"))+xlab("Education Levels: Below College, College, Bachelor, Masters, Doctor")
    })
  })
  
  
  observeEvent(input$clicks6, {
    print(as.numeric(input$clicks6))
    
    
    output$result <- renderUI({
      
      x_validate <-data.frame("Age"  = 20,                        
                              "BusinessTravelNonTravel"  = 0,     "BusinessTravelTravel_Frequently" = 1,
                              "DepartmentHumanResources"  = 1,       "DepartmentSales" = 0,              
                              "DistanceFromHome" = 9,               "Education"  = 0,                   
                              "EducationFieldHumanResources" = 0,   "EducationFieldLifeSciences" = 0,   
                              "EducationFieldMarketing" = 1,        "EducationFieldMedical"   = 0,      
                              "EducationFieldOther" = 0,            "GenderMale"     = 0,               
                              "JobLevel"    = 3,                    "JobRoleHealthcareRepresentative"= 0,
                              "JobRoleHumanResources"  = 1,         "JobRoleLaboratoryTechnician" = 0,  
                              "JobRoleManager"       = 0,           "JobRoleManufacturingDirector" = 0, 
                              "JobRoleResearchDirector"  = 0,       "JobRoleResearchScientist"     = 0, 
                              "JobRoleSalesExecutive"     = 0,      "MaritalStatusDivorced"  = 0,       
                              "MaritalStatusMarried" = 0,           "MonthlyIncome"  = 65000,               
                              "NumCompaniesWorked"    = 3,          "PercentSalaryHike"  = 15,           
                              "StockOptionLevel"       = 0.8,         "TotalWorkingYears"  = 11,           
                              "TrainingTimesLastYear"   = 3,        "YearsAtCompany"    = 7,             
                              "YearsSinceLastPromotion" = 2,        "YearsWithCurrManager"  =4,        
                              "EnvironmentSatisfaction" = 3,        "JobSatisfaction" = 3,              
                              "WorkLifeBalance"         = 0,        "JobInvolvement" = 0,               
                              "PerformanceRating"  = 0,             "AvgWorkTime"  = 8,                 
                              "LeavesTaken"= 5)
      x_validate$Age = isolate(input$age)
      x_validate$PercentSalaryHike = isolate(input$salary)
      if(isolate(input$gender) == 'Male') {x_validate$GenderMale=1}
      if(isolate(input$married) == 'Married') {x_validate$MaritalStatusMarried=1}
      if(isolate(input$married) == 'Divorced') {x_validate$MaritalStatusDivorced=1}
      if(isolate(input$job) == 'Low') {x_validate$JobInvolvement=1}
      if(isolate(input$job) == 'Medium') {x_validate$JobInvolvement=2}
      if(isolate(input$job) == 'High') {x_validate$JobInvolvement=3}
      if(isolate(input$job) == 'Very High') {x_validate$JobInvolvement=4}
      if(isolate(input$performance) == 'Low') {x_validate$PerformanceRating=1}
      if(isolate(input$performance) == 'Good') {x_validate$PerformanceRating=2}
      if(isolate(input$performance) == 'Excellent') {x_validate$PerformanceRating=3}
      if(isolate(input$performance) == 'Outstanding') {x_validate$PerformanceRating=4}
      if(isolate(input$balance) == 'Bad') {x_validate$WorkLifeBalance=1}
      if(isolate(input$balance) == 'Good') {x_validate$WorkLifeBalance=2}
      if(isolate(input$balance) == 'Better') {x_validate$WorkLifeBalance=3}
      if(isolate(input$balance) == 'Best') {x_validate$WorkLifeBalance=4}
      if(isolate(input$education) == 'Below College') {x_validate$Education=1}
      if(isolate(input$education) == 'College') {x_validate$Education=2}
      if(isolate(input$education) == 'Bachelor') {x_validate$Education=3}
      if(isolate(input$education) == 'Masters') {x_validate$Education=4}
      if(isolate(input$education) == 'Doctor') {x_validate$Education=5}
      
      x_validate <- xgb.DMatrix(as.matrix(x_validate))
      y_validate <- predict(xgb_model, newdata = x_validate, type= 'prob')[,1]
      str1<-sprintf("%0.16f", y_validate)
      HTML(paste(str1))
    })
    
    
    output$text1 <- renderUI({
      str1 <- paste("Gender                 :", isolate(input$gender))
      str2 <- paste("Age                    :", isolate(input$age))
      str3 <- paste("Marital Status         :", isolate(input$married))
      str4 <- paste("Education              :", isolate(input$education))
      str5 <- paste("Job Involvement Level  :", isolate(input$job))
      str6 <- paste("Performance Rating     :", isolate(input$performance))
      str7 <- paste("Work-Life Balance      :", isolate(input$balance))
      str8 <- paste("Percentage Salary Hike :", isolate(input$salary))
      HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8,sep = '<br/>'))
      
    })
    
  })
  
}

shinyApp(ui = ui,server = server)

