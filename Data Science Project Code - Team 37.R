#Data Science Project
#Section C - Team 37 - M/Th 10:30 am - 12:45 pm
#Alex Gille, Ana Paula Gonzalez, Arushi Dheer, Monil Soni, Xuchen Tan

#Libraries
{
library(readr)
library(ggplot2)
library(fastDummies)
}
  
#Question 1
{
  #We are trying to see what is the optimal salary that we will pay our employees depending on 
  #their work location, also taking into account the ratio of remote work they do.
  
  data <- read.csv("ai-job_salaries_dsProject.csv")
  #Creating Dummy Variables for Company vs Employee Residence
  EmployeeResidency <-  ifelse(data$company_location == data$employee_residence,1,0)
  data1 = cbind(data, EmployeeResidency)
  dataUSA <- data1$company_location == "US"
  dataUS <- data1[dataUSA,]
  
  #Creating Variables for Company size (j) and Experience Level (k)
  dataUS=transform(dataUS,j=ifelse(company_size=='M'|company_size == 'L',1,0))
  dataUS=transform(dataUS,k=ifelse(experience_level=='SE'|experience_level == 'EX',1,0))
  
  ## COMPUTING THE EFFECT OF THE TREATMENT VARIABLE
  ## The initial approach to compute the effect would be to compute the averages 
  ## over the observations that received the treatment (the employees that have 
  ## their residence and work location as US)
  ## E[Y|d=1]
  ## and the average over the observations that did not received the treatment
  ## E[Y|d=0]
  aggregate(salary_in_usd ~ EmployeeResidency, FUN = mean, data= dataUS) 
  res <- aggregate(salary_in_usd ~ EmployeeResidency, FUN = mean, data= dataUS) 
  ## Taking the difference
  ## E[Y|d=1]-E[Y|d=0]
  res$salary_in_usd[2] - res$salary_in_usd[1]
  ## which leads to a difference of  $67915.61 in salary of working in the same
  ##location as the residence (for the US)
  
  ## We will now run a t-test to obtain p-value for the null hypothesis 
  t.test( salary_in_usd ~ EmployeeResidency, data= dataUS)
  res <- t.test( salary_in_usd  ~ EmployeeResidency, data= dataUS )
  res$estimate[2]-res$estimate[1]
  ##the null hypothesis H0 = no effect of the treatment
  ## so we reject the null since p-value = .0000016 < .001
  ## and therefore cannot conclude that the means of the two groups are the same
  
  hist( (dataUS$salary_in_usd[ dataUS$EmployeeResidency==1 ])/1000, xlab='Salary for Residence= Work Location(000USD)', main = "Distribution of Salary, Company Location = Residence Location" )
  hist( (dataUS$salary_in_usd[ dataUS$EmployeeResidency==0 ])/1000, xlab='Salary for Residence <> Work Location(000USD)', main = "Distribution of Salary, Company Location != Residence Location" )
  
  ## Although we can conclude that the expected salary of a person residing in the
  #US given that they work for  US company is higher than if they work remotely,
  #It might not lead you to the causal impact (of treatment effect because of 
  #a selection bias since employees that live in the US as well also have different characteristics 
  ## in general
  ## d is not randomly assigned. It depends on firm characteristics, such as experience 
  ## or job title.
  
  ##Analysis of the importance of the factors that will be used for control levels
  
  #BOXPLOT for Live and work locations
  with(data = dataUS, boxplot(salary_in_usd/1000 ~ EmployeeResidency,
                              xlab = "Work and Company Location",
                              ylab = "Salaries in USD 1,000"))
  
  cor(dataUS$salary_in_usd, dataUS$EmployeeResidency == 1)
  #0.222
  
  #There is some positive correlation between salary and if the Work and Employee Residence are
  #both US. It seems to go up when the employee lives in the US, however, the correlation is 
  #small and further analysis would need to be done in order to pursue this conclusion.
  
  #Creating factors for experience Level in order to Control for it
  dataUS$experience_level <- factor(dataUS$experience_level,
                                    levels = c("EN","MI", "SE", "EX"),
                                    ordered = TRUE)
  summary(dataUS$experience_level)
  
  #BOXPLOT for Experience Level and remote work
  
  #Converting Experience Level to Numeric type to include it as a Control Factor
  # and to see the correlation between Experience and Employee Residence|Company Location
  str(dataUS$experience_level)
  head(as.numeric(dataUS$experience_level))
  head(dataUS$experience_level)
  
  dataUS$experience_level_numeric <- as.numeric(dataUS$experience_level)
  with(data = dataUS, boxplot(experience_level_numeric ~EmployeeResidency == 1,
                              xlab = "Living and Company location Equal",
                              ylab = "Experience Level", col = c("green", "salmon") ))
  
  
  with(data = dataUS, boxplot(experience_level_numeric ~ EmployeeResidency==1,
                              xlab = "Work and Company Location",
                              ylab = "Experience Level"))
  
  cor(dataUS$experience_level_numeric, dataUS$EmployeeResidency)
  #0.103
  
  # Based on the above analysis, there is little difference in the experience level of 
  #people that work within their country and those that have a difference residence than
  #US. 
  
  #BOXPLOT for  remote work
  with(data = data1, boxplot(remote_ratio ~ EmployeeResidency,
                             xlab = "Work and Company Location",
                             ylab = "remote ratio"))
  
  ## referring the model shown in the project above: 
  options(scipen = 5)
  with(data = dataUS, boxplot(salary_in_usd/100 ~ experience_level, xlab = "Experience Level", ylab = "Salaries in USD 1,000"))
  plot(factor(dataUS$company_size), dataUS$salary_in_usd/1000 ,xlab = "Company Size" ,ylab="Salary in thousands (USD)", col = c("red", "blue") )
  
  ##Modeling with Linear Regressions
  #FIRST LINEAR  regression [US]
  model1 <- lm(salary_in_usd ~ EmployeeResidency, data = dataUS)
  summary(model1)
  
  # Point estimate:67,916  #P-VALUE = 2.24 e -08
  #The expected average treatment effect that living and working in the same location
  #has on an employee's salary is $67,916 holding everything
  #else constant.
  
  model2 <- lm(salary_in_usd ~ EmployeeResidency + experience_level, data = dataUS)
  summary(model2)
  
  #Point estimate: $60, 506  #p-value: 1.13e-07
  #If we control for experience level, the point estimate decreases and the 
  #standard error also decreases. In this case, the expected average effect
  #on the salary of someone having the residence in their work location (US)
  #would be $60,506
  
  #Trying out interaction variables
  model3=lm(salary_in_usd~EmployeeResidency+j+k+EmployeeResidency*j+EmployeeResidency*k,data=dataUS)
  summary(model3)
  model4=lm(salary_in_usd~EmployeeResidency+j+k+EmployeeResidency*j,data=dataUS)
  summary(model4)
}

#Question 2
{
  #Is it cheaper to hire a part-time employee for different job lengths and titles? 

  #Read in data
  d <- read.csv("ai-job_salaries_dsProject.csv")
  d = data.frame(d, D_FT = d$employment_type == "FT")
  
  #Separate data into groups pf part time and full time
  PT = d[d$employment_type == "PT",]
  FT = d[d$employment_type == "FT",]
  unique(FT$job_title)
  
  #plot out the proportion of each group
  jobs = unique(PT$job_title)
  PT_job = list()
  FT_job = list()
  
  #extract data with corresponding job titles
  for(each in jobs){
    PT_job = append(PT_job,list(PT[PT$job_title == each,]))
    FT_job = append(FT_job,list(FT[FT$job_title == each,]))
  }
  summary(PT_job)
  
  job_name = c("Computer Vision Software Engineer","Data Scientist","Data Engineer","Data Analyst","AI Scientist","3D Computer Vision Researcher","ML Engineer","Computer Vision Engineer")
  names(PT_job) = job_name
  names(FT_job) = job_name
  PT_salary = c()
  FT_salary = c()
  salary = c()
  ET = c()
  job_list = c()
  
  # create data frame for plotting 
  for(each in jobs){
    salary = append(salary, mean(PT_job[[each]]$salary_in_usd))
    ET = append(ET, "PT")
    job_list = append(job_list, each)
    salary = append(salary, mean(FT_job[[each]]$salary_in_usd))
    ET = append(ET, "FT")
    job_list = append(job_list, each)
    PT_salary = append(PT_salary,mean(PT_job[[each]]$salary_in_usd))
    FT_salary = append(FT_salary, mean(FT_job[[each]]$salary_in_usd))
  }
  
  d_PT = data.frame(job_name, PT_salary)
  d_FT = data.frame(job_name, FT_salary)
  d_PF = data.frame(salary, job_list, ET)
  summary(d_FT)
  
  sal_data = cbind(d_PT, d_FT)
  ggplot(d_PT,aes(job_name, PT_salary))+geom_bar(stat = "identity")
  ggplot(d_FT, aes(job_name, FT_salary))+geom_bar(stat = "identity")
  ggplot(d_PF, aes(x = job_list,y= salary)) + geom_bar(aes(fill = ET),stat = "identity", position = "dodge")+labs(x = "Job Title", y = "Average Salary", fill = "Employment Type", title = "Salary Distribution Between Part Time and Full Time Across Different Posistion")
  unique(PT$company_location)
  
  #down sampling accross job title, company location, experience level
  exp_level = unique(PT$experience_level)
  country = unique(PT$company_location)
  FT = FT[FT$job_title %in% job_name,]
  FT = FT[FT$experience_level %in% exp_level,]
  FT = FT[FT$company_location %in% country,]
  
  #re-plot after down sampling
  num_FT = length(FT$work_year)
  num_PT = length(PT$work_year)
  n = num_FT+num_PT
  replot = data.frame(E = c("Full Time", "Part Time"), PartTime =c(num_FT/n, num_PT/n))
  ggplot(replot, aes(x = E, y = PartTime, fill = E)) + geom_bar(stat = "identity")+labs(title = "Part Time And Full Time distribution after down sampling",x = "Employment Type", y = "Proportion", fill = "Employment Type")
  
  #Making dummies for experience_level, company_location, employment_type, job_title
  d = rbind(PT, FT)
  d = dummy_cols(d, select_columns = "job_title", remove_first_dummy = TRUE)
  d = dummy_cols(d, select_columns = "company_location", remove_first_dummy = TRUE)
  d = data.frame(d, part_time = ifelse(d$employment_type == "PT", 1, 0))
  d = data.frame(d, entry_level = ifelse(d$experience_level == "EN", 1, 0))
  new_d = data.frame(d[,13:29], salary = d$salary_in_usd)
  
  #Model 1
  m = lm(salary~., data = new_d)
  summary(m)
  
  #Model 2
  m2 = lm(salary~part_time+entry_level+company_location_US+company_location_IN, data = new_d)
  summary(m2)
  
  #Model 3
  m3 = lm(salary~part_time+entry_level+part_time*entry_level+company_location_US+company_location_IN, data = new_d)
  summary(m3)
}

#Question 3
{
  #We want to understand that, for a given workload, how much does an increase in remote ratio change the salary expected by the employee?
  
  # Read in Data
  require(Matching)
  data <- read.csv("ai-job_salaries_dsProject.csv")
  
  # Preparing Experience Variable
  data$experience_level <- factor(data$experience_level,
                                  levels = c("EN", "MI", "SE", "EX"),
                                  ordered = TRUE)
  summary(data$experience_level)
  options(scipen = 5)
  
  # Change the points on x-axis to full names
  with(data = data, boxplot(salary_in_usd/1000 ~ experience_level,
                            xlab = "Experience Level",
                            ylab = "Salaries in USD 1,000"))
  
  
  # Converting Experience Level to Numeric type to include it as a Control Factor
  str(data$experience_level)
  head(as.numeric(data$experience_level))
  head(data$experience_level)
  
  data$experience_level_numeric <- as.numeric(data$experience_level)
  with(data = data, boxplot(salary_in_usd/1000 ~ experience_level_numeric,
                            xlab = "Experience Level",
                            ylab = "Salaries in USD 1,000"))
  
  
  ### Employment Type
  head(data$employment_type)
  unique(data$employment_type)
  
  # While one may classify employment type as an ordinal factor, I'm choosing to
  # go with a simple factor. As such while a contract work is temporary, during 
  # the course of the work, it is equivalent to a full time job. 
  data$employment_type <- factor(data$employment_type)
  with(data = data, boxplot(salary_in_usd/1000 ~ employment_type,
                            xlab = "Employment Type",
                            ylab = "Salaries in USD 1,000" ))
  
  summary(data$employment_type)
  
  ### Job Title
  managerial_titles <- c("Director of Data Science",
                         "Principal Data Scientist",
                         "Head of Data Science",
                         "Lead Data Scientist",
                         "Data Science Manager",
                         "Data Engineering Manager",
                         "Machine Learning Manager",
                         "Data Analytics Manager",
                         "Head of Data",
                         "Director of Data Engineering" )
  data$manager <- data$job_title %in% managerial_titles
  with(data = data, boxplot(salary_in_usd/1000 ~ manager,
                            xlab = "Individual Contributor (False) vs Manager (True)",
                            ylab = "Salaries in USD 1,000" ))
  
  ### Company Size 
  head(data$company_size)
  data$company_size <- factor(data$company_size,
                              levels = c("S", "M", "L"),
                              ordered = TRUE)
  summary(data$company_size)
  data$company_size_numeric <- as.numeric(data$company_size)
  
  with(data = data, boxplot(salary_in_usd/1000 ~ company_size_numeric,
                            xlab = "Company Size",
                            ylab = "Salaries in USD 1,000"))
  
  ### Linear Model
  # Since we want to understand the marginal effect of a variable (remote ratio), 
  # we use linear model for it. This will give us the coefficient for given 
  # variables which we interpret as a marginal effect. 
  
  # Unfortunately, while the remote ratio variable looks like a range, it is, 
  # in essence, a categorical variable and so, we convert it to factor. 
  data$remote_ratio <- factor(data$remote_ratio)
  with(data = data, boxplot(salary_in_usd/1000 ~ remote_ratio,
                            xlab = "Remote Ratio",
                            ylab = "Salaries in USD 1,000"))
  
  # To account for the distribution of salaries vs. remote ratio, we understand 
  # that this distribution looks like $y = x^2$. Thus, we take the square root 
  # of salary_in_usd variable to make the relationship more linear. 
  linear_model <- lm(sqrt(salary_in_usd) ~ remote_ratio + employment_type
                     + experience_level_numeric + manager + company_size_numeric
                     + (experience_level_numeric + manager + company_size_numeric)^2,
                     data = data)
  summary(linear_model)
  
  # While we get coefficients of dummy variables remote_ratio50 and remote_ratio100, 
  # the p-value for remote_ratio100 is 0.95! This is a very high p-value and 
  # indicative that there is a high probability that this coefficient is zero. 
  # 
  # Moreover, data is also distributed unevenly. For example, there are 
  # significantly low observations for part time compared to full time. On a similar
  # note, the distribution for salary against remote ratio is not an accurate representation of salary but of the available data. Thus, "fitting" our model to this distribution is an incorrect way to go. 
  # 
  # We conclude that while linear model might be simple to understand, it might not 
  # be a good choice to elicit causal effects for this case.
  
  # Creating Dummy Variables For Remote Ratio
  data$d_0 <- data$remote_ratio == 0
  data$d_50 <- data$remote_ratio == 50
  data$d_100 <- data$remote_ratio == 100
  
  #############
  ### Observing the impact of making a job 50% remote from completely onsite
  #############
  
  # We only keep jobs with 0% and 50% remote ratio because we essentially want 
  # to isolate the effect of making job 50% remote from 0% remote. 
  data_0_50 <- data[data$d_100 == 0, ]
  
  # Here the treatment is 1 when job is 50% remote. In other cases, we've only 
  # considered cases when job is completely on-site. Thus, the treatment effect 
  # will give us the salary change my remote ratio changes from 0% to 50% remote.
  lm.no_remote_to_50_remote <- glm(d_50 ~ employment_type + experience_level_numeric + manager + company_size_numeric, data = data_0_50, family = "binomial")
  
  rr.no_remote_to_50_remote <- Match(Y = data_0_50$salary_in_usd, 
                                     Tr = data_0_50$d_50, 
                                     X = lm.no_remote_to_50_remote$fitted.values)
  summary(rr.no_remote_to_50_remote)
  
  #############
  ### Observing the impact of making a job 100% remote from 50% remote
  #############
  
  # We only keep jobs with 50% and 100% remote ratio because we essentially want 
  # to isolate the effect of making job 100% remote from 50% remote. 
  data_50_100 <- data[data$d_0 == 0, ]
  
  # Here the treatment is 1 when job is 100% remote. 
  lm.50_remote_to_100_remote <- glm(d_100 ~ employment_type + experience_level_numeric + manager + company_size_numeric, data = data_50_100, family = "binomial")
  
  rr.50_remote_to_100_remote <- Match(Y = data_50_100$salary_in_usd, 
                                      Tr = data_50_100$d_100,
                                      X = lm.50_remote_to_100_remote$fitted.values)
  summary(rr.50_remote_to_100_remote)
  
  #############
  ### Observing the impact of making a job 100% remote from completely onsite
  #############
  # We only keep jobs with 0% and 100% remote ratio because we essentially want 
  # to isolate the effect of making job 100% remote from 0% remote. 
  data_0_100 <- data[data$d_50 == 0, ]
  
  # Here the treatment is 1 when job is 100% remote
  lm.no_remote_to_100_remote <- glm(d_100 ~ employment_type + experience_level_numeric + manager + company_size_numeric, data = data_0_100, family = "binomial")
  
  rr.no_remote_to_100_remote <- Match(Y = data_0_100$salary_in_usd, 
                                      Tr = data_0_100$d_100, 
                                      X = lm.no_remote_to_100_remote$fitted.values)
  summary(rr.no_remote_to_100_remote)
}

#Question 4
{
  #If a job has a managerial title, how does the salary of that position compare to one
  #with a more baseline title for jobs of similar work
  
  #Read in Data
  data = read.csv("ai-job_salaries_dsProject.csv")

  #Potential Titles from data
  #-Applied Data Scientist
  #-Data Science Consultant
  #-Data Science Engineer
  #-Data Science Manager
  #-Data Scientist
  #-Director of Data Science
  #-Head of Data Science
  #-Lead Data Scientist
  #-Principal Data Scientist
  #-Staff Data Scientist
  
  #Titles we viewed as managerial titles
  TitleAnalysisDS = (data[which(data$job_title %in% c("Director of Data Science",
                                                      "Principal Data Scientist",
                                                      "Head of Data Science",
                                                      "Lead Data Scientist",
                                                      "Data Science Manager",
                                                      "Data Scientist")),])
  
  #Analyzed the sample size of each title
  DataScienceManager = (data[TitleAnalysisDS$job_title == "Data Science Manager",])
  length(DataScienceManager)
  #11 obsv
  
  DirectorDS = (TitleAnalysisDS[TitleAnalysisDS$job_title == "Director of Data Science",])
  summary(DirectorDS$salary_in_usd)
  length(DirectorDS$salary_in_usd)
  #8 obsv
  
  PrincipalDS = (TitleAnalysisDS[TitleAnalysisDS$job_title == "Principal Data Scientist",])
  summary(PrincipalDS$salary_in_usd)
  length(PrincipalDS$salary_in_usd)
  #7 obsv
  
  HeadDS = (TitleAnalysisDS[TitleAnalysisDS$job_title == "Head of Data Science",])
  summary(HeadDS$salary_in_usd)
  length(HeadDS$salary_in_usd)
  #4 obsv
  
  LeadDS = (TitleAnalysisDS[TitleAnalysisDS$job_title == "Lead Data Scientist",])
  summary(LeadDS$salary_in_usd)
  length(LeadDS$salary_in_usd)
  #5 obsv
  
  DS = (TitleAnalysisDS[TitleAnalysisDS$job_title == "Data Scientist",])
  summary(DS$salary_in_usd)
  length(DS$salary_in_usd)
  #232 obsv
  
  #Label whether a non-entry level DS job has a special manager-themed title
  TitleAnalysisDS = transform(TitleAnalysisDS,
                              manager_DS_title = ifelse(job_title=="Data Scientist", 0, 1))
  
  summary(factor(TitleAnalysisDS$manager_DS_title))
  #Base Data Science Title = 202 obsv
  #Managerial Data Science Title = 51 obsv
  
  #Focus specifically on Senior level jobs, as we can assume responsibilities should be similar
  #Use experience level as a proxy for similar responsibilities
  SeniorOnly = TitleAnalysisDS[TitleAnalysisDS$experience_level == 'SE',]
  
  boxplot(SeniorOnly$salary_in_usd~SeniorOnly$manager_DS_title, xlab = 'Manager-based Title', ylab = 'Salary in USD')
  
  summary(factor(SeniorOnly$manager_DS_title))
  #Base Data Science Title = 128 obsv
  #Managerial Data Science Title = 29 obsv
  
  #Create a boxplot for exploration
  boxplot(SeniorOnly$salary_in_usd~SeniorOnly$manager_DS_title, xlab = 'Manager-based Title', ylab = 'Salary in USD')
  
  #Linear model exploring title effect of salary for senior level data science roles
  
  summary(lm(salary_in_usd ~ manager_DS_title,
             data = SeniorOnly))
  
  #Reflections
  #What would I do if I had more data?
  #-Look into how specific titles may affect salary
  #-Drill down into specific countries to see how this may vary between countries and work cultures
  #-Look at specific years of experience to get a better equivalent of similar jobs
}