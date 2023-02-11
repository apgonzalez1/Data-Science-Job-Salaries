# DataScience
Data Science Project - Job Salaries
This project was a part of the Data Science course at Duke Fuqua School of Business - MQM Business Analytics Program - Class of 2023

1. Dataset Source and fields :
Data Science Salaries. This dataset is available from the repository (salaries.ai-jobs.net as of 9.26.2022)
The following fields are found in our dataset, along with the number of unique values in each field:
● Work_year - The year the salary was paid (3 unique values: 2020, 2021, 2022)
● Experience_level - The experience level in the job during the year (4 unique values):
  ○ EN - Entry-level / Junior
  ○ MI - Mid-level / Intermediate
  ○ SE - Senior-level / Expert
  ○ EX - Executive-level / Director
● Employment_type - The type of employment for the role (4 unique values):
  ○ PT - Part-time
  ○ FT - Full-time
  ○ CT - Contract
  ○ FL - Freelance
● Job_title - The role worked in during the year (57 unique values)
● Salary - The total gross salary amount paid.
● Salary_currency - The currency of the salary paid (17 unique values)
● Salary_in_usd - The salary in USD (Foreign exchange rate via fxdata.foorilla.com as of 9.26.2022).
● Employee_residence - Employee's primary country of residence during the work year (62 unique
values)
● Remote_ratio - The overall amount of work done remotely (3 unique values):
  ○ 0 - No remote work (less than 20%)
  ○ 50 - Partially remote (between 20% and 80%)
  ○ 100 - Fully remote (more than 80%)
● Company_location - The country of the employer's main office or contracting branch (52 unique
values)
● Company_size - The average number of people that worked for the company during the year (3
unique values):
  ○ S - less than 50 employees (small)
  ○ M - 50 to 250 employees (medium)
  ○ L - more than 250 employees (large)

2. Problem Definition & Key Questions:
Using this data, we want to better understand, as managers, what it may cost to hire dataprofessionals of specific titles and skill levels in various locations. 
  1) As a company local to the US, how much can we change the salary of an employee if they live outside of the US? 
  Furthermore, how does the salary of remote, non-US workers compare to those located inthe US?
  2) Is it cheaper to hire a part-time employee for different job lengths and titles?
  3)We want to understand that, for a given workload, how much does an increase in remote ratio change the salary expected by the employee? 
  This will allow us to make sure we at least offer the baseline
expectation of the employee if we want to retain them.

3. Proposed Data Techniques:
EDA
Regression Modeling
Propensity Score Matching
Downsammpling

4. Files included
Data set: ai-job_salaries_dsProject
List of R-codes: Data Science Project Code - Team 37.Rmd
Report document: Data Science Project - Team 37-1
