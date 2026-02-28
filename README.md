# biostat
Repository for my biostatistics & data management teaching courses.

This repository was created for team teaching collaboration in developing materials, code, and dataset for the biostatistics classes. This is not replacing LIVE UNPAD as LMS but rather a complementary tools for sharing materials, codes, dan datasets.

There would be three courses related with this site: 
1. Basic Biostatistics (Code: BB)
2. Advance Biostatistics (Code: AB)
3. Data Management and Analysis with R (Code: DM)

Data Description
1. PEF (PEF.csv)
   Data peak expiration flow yang didapat dari data IFLS gelombang 5. Didowload dari website IFLS (www.ifls.org) pada tanggal x-x-yyyy.

2. Fhh1new.csv
   Data yang berasal dari buku online: https://bookdown.org/roback/bookdown-BeyondMLR/

   The Philippine Statistics Authority (PSA) spearheads the Family Income and Expenditure Survey (FIES) nationwide. The survey, which is undertaken every three years, is aimed at
   providing data on family income and expenditure, including levels of consumption by item of expenditure. Our data, from the 2015 FIES, is a subset of 1500 of the 40,000 observations
   (Philippine Statistics Authority 2015). Our data set focuses on five regions: Central Luzon, Metro Manila, Ilocos, Davao, and Visayas

   Each line of the data file refers to a household at the time of the survey:
   Description of variable
   location = where the house is located (Central Luzon, Davao Region, Ilocos Region, Metro Manila, or Visayas)
   age = the age of the head of household
   total = the number of people in the household other than the head
   numLT5 = the number in the household under 5 years of age
   roof = the type of roof in the household (either Predominantly Light/Salvaged Material, or Predominantly Strong Material, where stronger material can sometimes be used as a proxy
   for greater wealth)

4. Data pefc2, pefc3, dan pefc4
5. Data smoking (berisikan variabel kebiasaan merokok) berasal dari IFLS gelombang 5
6. Data stroke (berisikan variabel kondisi terjadinya stroke/Variabel H, dengan kode 1= Ya, 3=Tidak)
7. Data wcgs (berisikan data Coronary Heart Disease/ CHD)
   Description
   The WCGS began in 1960 with 3,524 male volunteers who were employed by 11 California companies. Subjects were 39 to 59 years old and free of heart disease as determined by
   electrocardiogram. After the initial screening, the study population dropped to 3,154 and the number of companies to 10 because of various exclusions. 3154 healthy young men aged
   39-59 from the San Francisco area were assessed for their personality type. All were free from coronary heart disease at the start of the research. The cohort comprised both blue-
   and white-collar employees. At baseline the following information was collected: socio-demographic including age, education, marital status, income, occupation; physical and
   physiological including height, weight, bloodpressure, electrocardiogram, and corneal arcus; biochemical including cholesterol and lipoprotein fractions; medical and family history
   and use of medications; behavioral data including Type A interview, smoking, exercise, and alcohol use. Later surveys added data on anthropometry, triglycerides, Jenkins Activity
   Survey, and caffeine use. Eight and a half years later change in this situation was recorded.
   Format
   A data frame with 3154 observations on the following 13 variables.
   age= age in years;
   height= height in inches;
   weight= weight in pounds;
   sdp= systolic blood pressure in mm Hg;
   dbp= diastolic blood pressure in mm Hg;
   chol= Fasting serum cholesterol in mm %;
   behave= behavior type which is a factor with levels A1 A2 B3 B4;
   cigs= number of cigarettes smoked per day;
   dibep= behavior type a factor with levels A (Agressive) B (Passive);
   chd= coronary heat disease developed is a factor with levels no yes;
   typechd= type of coronary heart disease is a factor with levels angina infdeath none silent;
   timechd= Time of CHD event or end of follow-up;
   arcus= arcus senilis is a factor with levels absent present.

8. c_data.csv (Crime in campus data)
   Description
   c_data.csv contains crime information from a post secondary institution, either a college or university. The variables include:
   Enrollment = enrollment at the school
   type = college (C) or university (U)
   nv = the number of violent crimes for that institution for the given year
   nvrate = number of violent crimes per 1000 students
   enroll1000 = enrollment at the school, in thousands
   region = region of the country (C = Central, MW = Midwest, NE = Northeast, SE = Southeast, SW = Southwest, and W = West)


