##Code used to complete AS.110.125 Case Study

##USING YEAR BY YEAR FINANCE YOU CAN REGRESS THE HISTORICAL DATA, normalize 
##budget data to per capita, compared race demographics across boroughs and shootings

#install and read in necessary packages
install.packages("readr")
install.packages("tidyverse")
install.packages("conflicted")
install.packages("RColorBrewer")
install.packages("jtools")
install.packages("officer")
install.packages("flextable")
install.packages("huxtable")
library(readr)
library(tidyverse)
library(conflicted)
library(RColorBrewer)
library(jtools)
library(officer)
library(flextable)
library(huxtable)
#resolve conflicts between base R and dplyr from tidyverse
filter <- dplyr::filter
lag <- dplyr::lag

##Read in original NYPD Shooting Data and City Payroll and Historical Dept Expenditures
NYPD_Shooting_Incident_Data_Historic_ <- 
                       read_csv("/Users/hank/Desktop/Data Analystics JHU/Case Study/Data Sets/NYPD_Shooting_Incident_Data__Historic_.csv")

Citywide_Payroll_Data_Fiscal_Year_ <- 
                        read_csv("/Users/hank/Desktop/Data Analystics JHU/Case Study/Data Sets/Citywide_Payroll_Data__Fiscal_Year_.csv")

AgencyExpenditures <- read_excel("AgencyExpenditures.xlsx", sheet = "In $000's")


#Pull Department's of interest
#Format PSE to a better format, this displays as obs are 1 FY and Variables are the Departments
#This is displayed in thousands
P_S_E <- subset(AgencyExpenditures, grepl("police|social|education", Department, ignore.case = TRUE))
Dept_Funding_Historical <- pivot_longer(P_S_E, c(2:44)) |> pivot_wider(names_from = Department, values_from = value) |> rename("Year" = name)
Dept_Funding_Historical$Year <- as.double(Dept_Funding_Historical$Year)

##Create new data frame with variables of interest

##These variables are: Occurrence Date, Boro, Precinct, Vic_Race, murder, age, sex of victim
##Coerce variables to more useful types
Shooting_Interest_Vars <- NYPD_Shooting_Incident_Data_Historic_[,c(2,4,6,10,14:16)]
x <- unique(Shooting_Interest_Vars$PRECINCT)

#Rename Staten Island to S.I. for later graphics
Shooting_Interest_Vars$BORO <- ifelse(Shooting_Interest_Vars$BORO == 'STATEN ISLAND', 'S.I.', Shooting_Interest_Vars$BORO)
#Maybe make this more elegant tbd
Shooting_Interest_Vars$BORO <- factor(Shooting_Interest_Vars$BORO, levels = c("MANHATTAN","QUEENS", "BRONX","BROOKLYN","S.I."))
Shooting_Interest_Vars$VIC_SEX <- factor(Shooting_Interest_Vars$VIC_SEX, levels = c("F", "M"))
Shooting_Interest_Vars$VIC_RACE <- factor(Shooting_Interest_Vars$VIC_RACE, levels = c("WHITE", "BLACK", "WHITE HISPANIC","BLACK HISPANIC","ASIAN / PACIFIC ISLANDER"))
Shooting_Interest_Vars$VIC_AGE_GROUP <- factor(Shooting_Interest_Vars$VIC_AGE_GROUP, levels = c("<18", "45-64", "18-24","25-44", "65+"))
Shooting_Interest_Vars$PRECINCT <- factor(Shooting_Interest_Vars$PRECINCT, levels = x)

#Remove observations where the victim race, sex, and age are unknown
Unknown_RM <- subset(Shooting_Interest_Vars, Shooting_Interest_Vars$VIC_RACE != 
                       "UNKNOWN" & Shooting_Interest_Vars$VIC_SEX != "U" & Shooting_Interest_Vars$VIC_AGE_GROUP != "UNKNOWN")
Shooting_Clean_Data <- Unknown_RM

#Make Statistical_Murder_Flag a binary value rather than Boolean for later regression
Shooting_Clean_Data$STATISTICAL_MURDER_FLAG[Shooting_Clean_Data$STATISTICAL_MURDER_FLAG == TRUE] <- 1
Shooting_Clean_Data$STATISTICAL_MURDER_FLAG[Shooting_Clean_Data$STATISTICAL_MURDER_FLAG == FALSE] <- 0
Shooting_Clean_Data <- Shooting_Clean_Data |> rename('Murder' = STATISTICAL_MURDER_FLAG)

##Make the Occurrence Date data "Date" Class in R
Shooting_Clean_Data$OCCUR_DATE <- mdy(Shooting_Clean_Data$OCCUR_DATE)
##With the intention of later merging with funding data, this following line 
##Adds a column that represents the NYC FY the shooting Occurrance date
Shooting_FY <- Shooting_Clean_Data |> mutate(Year = ifelse(month(OCCUR_DATE)>6, year(OCCUR_DATE)+1, year(OCCUR_DATE)))
##Merge Shooting and funding data
Demo_Funding_Shooting <- Shooting_FY |> left_join(Dept_Funding_Historical, by ="Year")
#Reduces Number of levels for the later regression, Makes child = 0 and adult = 1
#Race white = 0, POC = 1
Reduced_Data <- Demo_Funding_Shooting
Reduced_Data$VIC_AGE_GROUP <- as.character(Reduced_Data$VIC_AGE_GROUP)
Reduced_Data$VIC_AGE_GROUP[Reduced_Data$VIC_AGE_GROUP == "<18"] <- 0
Reduced_Data$VIC_AGE_GROUP[Reduced_Data$VIC_AGE_GROUP != "0"] <- 1
Reduced_Data$VIC_AGE_GROUP <- as.factor(Reduced_Data$VIC_AGE_GROUP)
Reduced_Data$VIC_RACE <- as.character(Reduced_Data$VIC_RACE)
Reduced_Data$VIC_RACE[Reduced_Data$VIC_RACE != "WHITE"] <- 1
Reduced_Data$VIC_RACE[Reduced_Data$VIC_RACE = "WHITE"] <- 0
Reduced_Data$VIC_RACE[Reduced_Data$VIC_RACE == "WHITE"] <- 0
Reduced_Data$VIC_RACE <- as.factor(Reduced_Data$VIC_RACE)
no_loc <- glm(Murder ~ VIC_AGE_GROUP + VIC_SEX + VIC_RACE + EDU + PD + SOCIAL, family = "binomial", data = Reduced_Data)
##Rename the Variables for Departments" EDU = Dept. of Education, PD = Police Dept, SOCIAL = Social Services
Reduced_Data <- Reduced_Data |> rename("EDU" = `Department of Education`, "PD" = `Police Department`, "SOCIAL" = `Department of Social Services` )
##Separate out all Shootings from NYC FY 2016, 07/01/2015 and 06/30/2016
Shooting_2016 <- Shooting_Clean_Data |> 
  filter(between(Shooting_Clean_Data$OCCUR_DATE,as.Date("2015-07-01"),as.Date("2016-06-30")))

    ##Basic exploration and Statistics for the Shooting Data

##Finds the number of shooting per borough, and returns a table with the values.
By_Boro_Shooting <- Shooting_2016 |> group_by(Shooting_2016$BORO) |> count()
By_Boro_Shooting <- By_Boro_Shooting |> rename('Borough' = `Shooting_2016$BORO`,
                                               "# of Shootings" = "n")
#convert to Data Frame
By_Boro_Shooting <- as.data.frame(By_Boro_Shooting)
##Graph to aid understanding
ggplot(By_Boro_Shooting, aes(x = Borough, y = `# of Shootings`)) +
       geom_bar(stat = "identity", fill = "steelblue") +
       labs(x = "Borough", y = "Number of Shootings") +
      ggtitle("Shootings by Borough in NYC")
#Shootings by Race of Victim
By_Race <- Shooting_2016 |> group_by(VIC_RACE) |> count()
By_Race <- By_Race |> rename('Victim Race' = 'VIC_RACE', "Number of Victims" = "n")
By_Race <- as.data.frame(By_Race)
#By Race and Borough 

By_Race_Borough <- Shooting_2016 |> group_by(BORO, VIC_RACE) |> count() |> 
                  pivot_wider(names_from = VIC_RACE, values_from = n, values_fill = 0)
By_Race_Borough <- as.data.frame(By_Race_Borough)
plot_race_borough <- ggplot(By_Race_Borough, aes(x = BORO)) +
       geom_bar(aes(y = `BLACK`, fill = "Black"), position = "dodge", stat = "identity") +
       geom_bar(aes(y = `WHITE`, fill = "White"), position = "dodge", stat = "identity") +
       geom_bar(aes(y = `WHITE HISPANIC`, fill = "White Hispanic"), position = "dodge", stat = "identity") +
       geom_bar(aes(y = `BLACK HISPANIC`, fill = "Black Hispanic"), position = "dodge", stat = "identity") +
       geom_bar(aes(y = `ASIAN / PACIFIC ISLANDER`, fill = "Asian/Pacific Islander"), position = "dodge", stat = "identity") +
       labs(x = "Borough", y = "Count", fill = "Race") +
       ggtitle("Count of Shootings by Race and Borough") +
       scale_color_brewer(type = "qual", palette = "Set1") +
       theme(legend.title = element_blank())

#Now to clean and extract relevant information from the payroll data
PR_2016 <- subset(Citywide_Payroll_Data_Fiscal_Year_, Citywide_Payroll_Data_Fiscal_Year_$`Fiscal Year` == 2016)
#Make a vector containing the names of the agencies we want
#Use this vector to subset the Payroll data to contain just that of the agencies of interest
relevant_agencies <- c("DEPT OF ED HRLY SUPPORT STAFF", "DEPT OF ED PARA PROFESSIONALS", "DEPT OF ED PEDAGOGICAL","DEPT OF ED PER DIEM TEACHERS", "DEPT OF ED PER SESSION TEACHER",
                       "DEPT OF PARKS & RECREATION","DEPT. OF HOMELESS SERVICES","HRA/DEPT OF SOCIAL SERVICES",
                       "POLICE DEPARTMENT")
PR_2016_Agency <- subset(PR_2016, PR_2016$`Agency Name` %in% relevant_agencies)

#Pull just the boroughs that are in both our shooting data and the payroll data
boro <- unique(PR_2016_Agency$`Work Location Borough`)
same_boro <- subset(boro, boro %in% By_Race_Borough$BORO)
PR_2016_AB <- subset(PR_2016_Agency, PR_2016_Agency$`Work Location Borough` %in% same_boro)
PR_2016_AB <- PR_2016_AB |> rename('Borough' = `Work Location Borough`, "Sector" = `Agency Name`)

#Code them to be either Education, Police, or Social Services
Edu <- relevant_agencies[1:5]
Social_Services <- relevant_agencies[6:8]
Police <- relevant_agencies[9]
PR_2016_AB$Sector[PR_2016_AB$Sector %in% Edu] <- "EDUCATION"
PR_2016_AB$Sector[PR_2016_AB$Sector %in% Social_Services] <- "SOCIAL SERVICES"
PR_2016_AB$Sector[PR_2016_AB$Sector %in% Police] <- "POLICE"

#Add column of pay per employee: Gross + OT = Pay
PR_2016_AB <- PR_2016_AB |> mutate(Pay = rowSums(PR_2016_AB[,c(14,16)]))

#Parse down the data frame to include just variables of interest
PR_2016_AB <- PR_2016_AB[,c(1,3,8,11:18)]

#Add up all the salaries for the sectors, remove education due to data issues
By_SB <- PR_2016_AB |> group_by(PR_2016_AB$Sector) |> summarise(Funding = sum(Pay))
NonEdu <- By_SB[c(2,3),] |> rename("Sector" = PR_2016_AB$Sector)
NonEdu <- NonEdu |> rename("Sector" = `PR_2016_AB$Sector`)
#Cursory Data Fun Stuff
Funding_Diff <- Dept_Funding_Historical |> mutate(Difference = (`Police Department` - `Department of Social Services`))
ggplot(Funding_Diff, aes(Year, Difference)) + geom_line(aes(color = Difference)) + ggtitle("Difference Between Police and Social Services Funding")
#Model: Reference points: Manhattan, <18, Female, White
model <- glm(Murder ~ BORO + VIC_AGE_GROUP + VIC_SEX + VIC_RACE, family = "binomial", data = Shooting_2016)
#Reduced Model takes the race and turns in into 1 for POC and 0 for not
#And age group is 1 if adult 0 if child
#no_loc excludes boroughs
no_loc <- glm(Murder ~ VIC_AGE_GROUP + VIC_SEX + VIC_RACE + EDU + PD + SOCIAL, family = "binomial", data = Reduced_Data)
export_summs(no_loc, scale = TRUE, to.file = "docx", file.name = "No Boro Regression.docx")

##Loc includes the boros and keeps the reduced levels for all other variables
loc <- glm(Murder ~ BORO + VIC_AGE_GROUP + VIC_SEX + VIC_RACE + EDU + PD + SOCIAL, family = "binomial", data = Reduced_Data)
export_summs(loc, to.file = "docx", file.name = "Model3.docx")


##Two line showing diff in funding and the shootings
ggplot() +
       geom_line(data = Funding_Diff, aes(x = Year, y = Difference/1000), color = "cornflowerblue", show.legend = TRUE) +
       geom_line(data = SPY, aes(x = Year, y = Shootings), color = "darkorange", show.legend = TRUE) + 
       xlim(2006,2023) +
       labs(title = "Shootings and Funding Differences", x = "Years", y = "Shootings/Difference of Funding (in $000)")

##Shooting per year scatterplot
ggplot() + geom_point(data = SPY, aes(x = Year, y = Shootings), shape = "cross", color = "darkorange", size = 3) + labs(title = "Shootings per Year")
