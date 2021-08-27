install.packages("rlang")
library(dplyr)
library(tidyverse)
library(stringr)
library(rvest)
library(tidyr)
library(plyr)

# ----------------------------------------------------
# Gathering and Preprocessing of salary data
# # --------------------------------------------------
# HTML web scraping function
# Return dataframe of player pay for the 2018 and 2019 season

team_payroll_2019 <- function(spotrac_url,team_name) {
  team_table <- spotrac_url %>% read_html() %>% html_nodes(xpath='//*[@id="main"]/div/div[4]/table[1]') %>% html_table()
  team_salary <- team_table[[1]]
  team_salary <- data.frame(team_salary)
  team_tableIL <- spotrac_url %>% read_html() %>% html_nodes(xpath='//*[@id="main"]/div/div[4]/table[2]') %>% html_table()
  teamIL <- team_tableIL[[1]]
  teamIL <- data.frame(teamIL)
  colnames(team_salary) <- c("PlayerName","Age","Position","Status","BaseSalary","SigningBonus","Incentives","TotalSalary",
                             "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary")
  colnames(teamIL) <- c("PlayerName","Age","Position","Status","BaseSalary","SigningBonus","Incentives","TotalSalary",
                        "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary")
  combined_salaries <- rbind(team_salary,teamIL)
  combined_salaries <- combined_salaries %>% mutate(Team = team_name)
  return(combined_salaries)
}

# --------------------------------------------------
# Save results of each team payroll as a variable
dbacks2019 <- team_payroll_2019("https://www.spotrac.com/mlb/arizona-diamondbacks/payroll/2019/","Arizona Diamondbacks")
braves2019 <- team_payroll_2019("https://www.spotrac.com/mlb/atlanta-braves/payroll/2019/","Atlanta Braves")
orioles2019 <- team_payroll_2019("https://www.spotrac.com/mlb/baltimore-orioles/payroll/2019/","Baltimore Orioles")
redsox2019 <- team_payroll_2019("https://www.spotrac.com/mlb/boston-red-sox/payroll/2019/","Boston Red Sox")
cubs2019 <- team_payroll_2019("https://www.spotrac.com/mlb/chicago-cubs/payroll/2019/","Chicago Cubs")
whitesox2019 <- team_payroll_2019("https://www.spotrac.com/mlb/chicago-white-sox/payroll/2019/","Chicago White Sox")
reds2019 <- team_payroll_2019("https://www.spotrac.com/mlb/cincinnati-reds/payroll/2019/","Cincinnati Reds")
indians2019 <- team_payroll_2019("https://www.spotrac.com/mlb/cleveland-indians/payroll/2019/","Cleveland Indians")
rockies2019 <- team_payroll_2019("https://www.spotrac.com/mlb/colorado-rockies/payroll/2019/","Colorado Rockies")
tigers2019 <- team_payroll_2019("https://www.spotrac.com/mlb/detroit-tigers/payroll/2019/","Detroit Tigers")
astros2019 <- team_payroll_2019("https://www.spotrac.com/mlb/houston-astros/payroll/2019/","Houston Astros")
royals2019 <- team_payroll_2019("https://www.spotrac.com/mlb/kansas-city-royals/payroll/2019/","Kansas City Royals")
angels2019 <- team_payroll_2019("https://www.spotrac.com/mlb/los-angeles-angels/payroll/2019/","Los Angeles Angels")
dodgers2019 <- team_payroll_2019("https://www.spotrac.com/mlb/los-angeles-dodgers/payroll/2019/","Los Angeles Dodgers")
marlins2019 <- team_payroll_2019("https://www.spotrac.com/mlb/miami-marlins/payroll/2019/","Miami Marlins")
brewers2019 <- team_payroll_2019("https://www.spotrac.com/mlb/milwaukee-brewers/payroll/2019/","Milwaukee Brewers")
twins2019 <- team_payroll_2019("https://www.spotrac.com/mlb/minnesota-twins/payroll/2019/","Minnesota Twins")
mets2019 <- team_payroll_2019("https://www.spotrac.com/mlb/new-york-mets/payroll/2019/","New York Mets")
yankees2019 <- team_payroll_2019("https://www.spotrac.com/mlb/new-york-yankees/payroll/2019/","New York Yankees")
athletics2019 <- team_payroll_2019("https://www.spotrac.com/mlb/oakland-athletics/payroll/2019/","Oakland Athletics")
phillies2019 <- team_payroll_2019("https://www.spotrac.com/mlb/philadelphia-phillies/payroll/2019/","Philadelphia Philles")
pirates2019 <- team_payroll_2019("https://www.spotrac.com/mlb/pittsburgh-pirates/payroll/2019/","Pittsburgh Pirates")
padres2019 <- team_payroll_2019("https://www.spotrac.com/mlb/san-diego-padres/payroll/2019/","San Diego Padres")
giants2019 <- team_payroll_2019("https://www.spotrac.com/mlb/san-francisco-giants/payroll/2019/","San Francisco Giants")
mariners2019 <- team_payroll_2019("https://www.spotrac.com/mlb/seattle-mariners/payroll/2019/","Seattle Mariners")
cardinals2019 <- team_payroll_2019("https://www.spotrac.com/mlb/st-louis-cardinals/payroll/2019/","St. Louis Cardinals")
rays2019 <- team_payroll_2019("https://www.spotrac.com/mlb/tampa-bay-rays/payroll/2019/","Tampa Bay Rays")
rangers2019 <- team_payroll_2019("https://www.spotrac.com/mlb/texas-rangers/payroll/2019/","Texas Rangers")
bluejays2019 <- team_payroll_2019("https://www.spotrac.com/mlb/toronto-blue-jays/payroll/2019/","Toronto Blue Jays")
nationals2019 <- team_payroll_2019("https://www.spotrac.com/mlb/washington-nationals/payroll/2019/","Washington Nationals")

# --------------------------------------------------
# Combine all 2019 teams payroll data
salary_master_2019 <- rbind(dbacks2019,braves2019,orioles2019,redsox2019,cubs2019,whitesox2019,reds2019,
                            indians2019,rockies2019,tigers2019,astros2019,royals2019,angels2019,dodgers2019,marlins2019,
                            brewers2019,twins2019,mets2019,yankees2019,athletics2019,phillies2019,pirates2019,padres2019,
                            giants2019,mariners2019,cardinals2019,rays2019,rangers2019,bluejays2019,nationals2019)

# --------------------------------------------------
# Apply function to clean names pulled from html table
name_clean <- function(masterDF) {
  clean <- masterDF %>% separate(PlayerName,c("Delete","First","Last"),extra="merge",fill="left")
  clean <- subset(clean,select=-c(Delete))
  clean$PlayerName <- paste(clean$First,clean$Last)
  clean <- subset(clean,select=-c(First,Last))
  remove_punctuation <- str_replace(clean$PlayerName," \\s*\\([^\\)]+\\)", "")
  clean <- cbind(remove_punctuation,clean)
  clean <- subset(clean,select=-c(PlayerName))
  colnames(clean) <- c("PlayerName","Age","Position","Status","BaseSalary","SigningBonus","Incentives","TotalSalary",
                       "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary","Team")
  return(clean)
}

salary_master_2019 <- name_clean(salary_master_2019)


# --------------------------------------------------
# Hardcode specific problem names

player_rename <- function(salary_master,name_wrong,name_correct) {
  cleanDF <- salary_master %>% mutate_at("PlayerName",str_replace,name_wrong,name_correct)
  return(cleanDF)
}

salary_master_2019 <- player_rename(salary_master_2019,"Falefa Isiah Kiner-Falefa","Isiah Kiner-Falefa")
salary_master_2019 <- player_rename(salary_master_2019,"Shin Soo Choo","Shin-Soo Choo")
salary_master_2019 <- player_rename(salary_master_2019,"Nelson Cruz","Nelson Cruz Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Manuel Pina","Manny Pina")
salary_master_2019 <- player_rename(salary_master_2019,"Michael Brantley","Michael Brantley Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"J D. Martinez","J.D. Martinez")
salary_master_2019 <- player_rename(salary_master_2019,"D J. LeMahieu","DJ LeMahieu")
salary_master_2019 <- player_rename(salary_master_2019,"J A. Happ","J.A. Happ")
salary_master_2019 <- player_rename(salary_master_2019,"Yulieski Gurriel","Yuli Gurriel")
salary_master_2019 <- player_rename(salary_master_2019,"Arnaud Travis d'Arnaud","Travis d'Arnaud")
salary_master_2019 <- player_rename(salary_master_2019,"C J. Cron","C.J. Cron")
salary_master_2019 <- player_rename(salary_master_2019,"Giovanny Urshela","Gio Urshela")
salary_master_2019 <- player_rename(salary_master_2019,"A J. Pollock","AJ Pollock")
salary_master_2019 <- player_rename(salary_master_2019,"Michael Taylor","Michael A. Taylor")
salary_master_2019 <- player_rename(salary_master_2019,"J T. Realmuto","J.T. Realmuto")
salary_master_2019 <- player_rename(salary_master_2019,"Ji Man Choi","Ji-Man Choi")
salary_master_2019 <- player_rename(salary_master_2019,"Dee Gordon","Dee Strange-Gordon")
salary_master_2019 <- player_rename(salary_master_2019,"Albert Almora","Albert Almora Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Dan Vogelbach","Daniel Vogelbach")
salary_master_2019 <- player_rename(salary_master_2019,"Daniel Santana","Danny Santana")
salary_master_2019 <- player_rename(salary_master_2019,"J D. Davis","J.D. Davis")
salary_master_2019 <- player_rename(salary_master_2019,"Stella Tommy La Stella","Tommy La Stella")
salary_master_2019 <- player_rename(salary_master_2019,"Timmy Lopes","Tim Lopes")
salary_master_2019 <- player_rename(salary_master_2019,"Jr Richie Martin Jr.","Richie Martin Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"D J. Stewart","DJ Stewart")
salary_master_2019 <- player_rename(salary_master_2019,"J P. Crawford","J.P. Crawford")
salary_master_2019 <- player_rename(salary_master_2019,"Joe Wendle","Joey Wendle")
salary_master_2019 <- player_rename(salary_master_2019,"Neill Tyler O'Neill","Tyler O'Neill")
salary_master_2019 <- player_rename(salary_master_2019,"Shed Long","Shed Long Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Jr Vladimir Guerrero Jr.","Vladimir Guerrero Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Jr Ronald Acuna Jr.","Ronald Acuna Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Hearn Ryan O'Hearn","Ryan O'Hearn")
salary_master_2019 <- player_rename(salary_master_2019,"Jr Fernando Tatis Jr.","Fernando Tatis Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Nate Lowe","Nathaniel Lowe")
salary_master_2019 <- player_rename(salary_master_2019,"Nicholas Lopez","Nicky Lopez")
salary_master_2019 <- player_rename(salary_master_2019,"Cameron Gallagher","Cam Gallagher")
salary_master_2019 <- player_rename(salary_master_2019,"Jr Jackie Bradley Jr.","Jackie Bradley Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Jr Dwight Smith Jr.","Dwight Smith Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Jr Lourdes Gurriel Jr.","Lourdes Gurriel Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Howie Kendrick","Howie Kendrick III")
salary_master_2019 <- player_rename(salary_master_2019,"J T. Riddle","JT Riddle")



# --------------------------------------------------
# Data collected from https://baseballsavant.mlb.com/, Baseball Savant
# Position Player Data from the 2019 season

baseball_savant_2019 <- read_csv("F:\\MLB2021Analysis\\baseball_savant_2019.csv")
baseball_savant_2019 <- subset(baseball_savant_2019,select=-c(X28))

# Combine first and last name columns into one
baseball_savant_2019$PlayerName <- paste(baseball_savant_2019$first_name,baseball_savant_2019$last_name)
baseball_savant_2019 <- subset(baseball_savant_2019,select=-c(first_name,last_name))

baseball_savant_2019 <- player_rename(baseball_savant_2019,"Howie Kendrick III III","Howie Kendrick")
baseball_savant_2019 <- player_rename(baseball_savant_2019,"Buster Posey III","Buster Posey")
baseball_savant_2019 <- player_rename(baseball_savant_2019,"George Springer III","George Springer")
baseball_savant_2019 <- player_rename(baseball_savant_2019,"Trey Mancini III","Trey Mancini")
baseball_savant_2019 <- player_rename(baseball_savant_2019,"Cedric Mullins II","Cedric Mullins")
baseball_savant_2019 <- player_rename(baseball_savant_2019,"Luke Voit III","Luke Voit")

# --------------------------------------------------
# Merge baseball_savant data with salary data

hitters_combined_2019 <- merge(baseball_savant_2019,salary_master_2019)
colnames(hitters_combined_2019) <- c("Name","ID","Year","Age","AB","Hits","Doubles","Triples","HomeRuns","K","BB","RBI","SB","G","GDP","HBP","SACBunt",
                                     "SACFly","Runs","XBA","XSLG","XOBP","ExitVeloAVG","LaunchAngleAVG","BarrelPercent",
                                     "SprintSpeed","Age1","Position","Status","BaseSalary","SigningBonus","Incentives","TotalSalary",
                                     "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary","Team")
hitters_combined_2019 <- subset(hitters_combined_2019,select=-c(Age1))

# --------------------------------------------------
# Find which players are left out of the new merged data frame
hitters_difference_2019 <- setdiff(baseball_savant_2019$PlayerName,hitters_combined_2019$Name)
View(hitters_difference_2019)

# --------------------------------------------------
# Convert Position to numeric
hitters_combined_2019 <- hitters_combined_2019 %>% mutate(PositionNum=case_when(Position=="SP"~1,
                                                                                Position=="C"~2,
                                                                                Position=="1B"~3,
                                                                                Position=="2B"~4,
                                                                                Position=="3B"~5,
                                                                                Position=="SS"~6,
                                                                                Position=="LF"~7,
                                                                                Position=="CF"~8,
                                                                                Position=="RF"~9,
                                                                                Position=="OF"~10,
                                                                                Position=="DH"~11,
                                                                                Position=="RP"~12,
                                                                                Position=="RP/CL"~13))

# Statistics Calculations
# --------------------------------------------------
# Singles
hitters_combined_2019$Singles <- hitters_combined_2019$Hits-(hitters_combined_2019$Doubles+hitters_combined_2019$Triples+hitters_combined_2019$HomeRuns) 

# AVG = Hits/AB
hitters_combined_2019$AVG <- (hitters_combined_2019$Hits/hitters_combined_2019$AB)

# --------------------------------------------------
# OBP = (Hits+BB+HBP)/(AB+BB+HBP+SAC)
hitters_combined_2019$OBP <- (hitters_combined_2019$Hits+hitters_combined_2019$BB+hitters_combined_2019$HBP)/
  (hitters_combined_2019$AB+hitters_combined_2019$BB+hitters_combined_2019$HBP+hitters_combined_2019$SACBunt+hitters_combined_2019$SACFly)


# --------------------------------------------------
# SLG = (1B+(2*2B)+(3*3B)+(4*HR))/AB
hitters_combined_2019$SLG <- (hitters_combined_2019$Singles+(2*hitters_combined_2019$Doubles)+(3*hitters_combined_2019$Triples)+
                                (4*hitters_combined_2019$HomeRuns))/hitters_combined_2019$AB

# --------------------------------------------------
# OPS = OBP+SLG
hitters_combined_2019$OPS <- hitters_combined_2019$OBP + hitters_combined_2019$SLG
# View(hitters_combined)

# --------------------------------------------------
# Average Salary by Position
# Convert player Adjusted Salary from string to numeric
TotalSalary1 <- gsub("\\s","",c(hitters_combined_2019$TotalSalary))
TotalSalary1 <- gsub(",","",TotalSalary1)
TotalSalary1 <- gsub("\\$","",TotalSalary1)
TotalSalary1 <- as.numeric(TotalSalary1)
hitters_combined_2019 <- cbind(hitters_combined_2019,TotalSalary1)

# --------------------------------------------------
# Create an all numeric dataframe and standardize the variables
numeric_hitters_2019DF <- subset(hitters_combined_2019,select=-c(Name,ID,Position,Status,BaseSalary,SigningBonus,
                                                                 Incentives,TotalSalary,AdjustedSalary,LuxuryTaxSalary,
                                                                 Team))

# --------------------------------------------------
# Correlation Matrix of 2019 hitter statistics
library(corrr)

hitter_correlations_2019 <- correlate(numeric_hitters_2019DF)
salaryCorr_2019 <- hitter_correlations_2019 %>% focus(TotalSalary1) %>% gather(-rowname,key="colname",value="cor") %>% 
  filter(abs(cor)<1.000)
salaryCorr_2019 <- salaryCorr_2019 %>% arrange(desc(abs(cor)))
View(salaryCorr_2019)

# --------------------------------------------------
# Create separate dataframe for predictors of TotalSalary

hitters_select_vars <- data.frame(hitters_combined_2019$Age,hitters_combined_2019$GDP,hitters_combined_2019$AB,
                                  hitters_combined_2019$RBI,hitters_combined_2019$BB,hitters_combined_2019$Hits,hitters_combined_2019$Runs,
                                  hitters_combined_2019$G,hitters_combined_2019$HomeRuns,hitters_combined_2019$SACFly,
                                  hitters_combined_2019$TotalSalary1)
colnames(hitters_select_vars) <- c("Age","GDP","AB","RBI","BB","Hits","Runs","G","HomeRuns","SACFly","TotalSalary1")

# Standardize all columns except for TotalSalary
standardized_hitters_2019 <- hitters_select_vars %>% mutate_at(c("Age","GDP","AB","RBI","BB","Hits",
                                                                 "Runs","G","HomeRuns","SACFly"),~(scale(.) %>% as.vector))

# --------------------------------------------------
# Linear model for predicting position player salary
hitters_model_2019 <- lm(TotalSalary1~.,data=standardized_hitters_2019)
summary(hitters_model_2019)

predictions_2019 <- predict(hitters_model_2019,hitters_combined_2019)

# --------------------------------------------------
# Data collected from https://baseballsavant.mlb.com/, Baseball Savant
# Pitcher Data from the 2019 season

baseball_savant_pitchers_2019 <- read_csv("F:\\MLB2021Analysis\\baseball_savant_pitchers2019.csv")
baseball_savant_pitchers_2019 <- subset(baseball_savant_pitchers_2019,select=-c(X36))

# Combine first and last name columns into one
baseball_savant_pitchers_2019$PlayerName <- paste(baseball_savant_pitchers_2019$first_name,baseball_savant_pitchers_2019$last_name)
baseball_savant_pitchers_2019 <- subset(baseball_savant_pitchers_2019,select=-c(first_name,last_name))

# --------------------------------------------------
# Hardcode specific name errors in salary_master_2019 DF and baseball_savant_pitchers_2019 DF

salary_master_2019 <- player_rename(salary_master_2019,"C C. Sabathia","CC Sabathia")
salary_master_2019 <- player_rename(salary_master_2019,"T J. McFarland","T.J. McFarland")
salary_master_2019 <- player_rename(salary_master_2019,"Tom Milone","Tommy Milone")
salary_master_2019 <- player_rename(salary_master_2019,"Matt Boyd","Matthew Boyd")
salary_master_2019 <- player_rename(salary_master_2019,"Alexander Claudio","Alex Claudio")
salary_master_2019 <- player_rename(salary_master_2019,"Daniel Poncedeleon","Daniel Ponce de Leon")
salary_master_2019 <- player_rename(salary_master_2019,"R J. Alaniz","R.J. Alaniz")
salary_master_2019 <- player_rename(salary_master_2019,"A J. Cole","A.J. Cole")
salary_master_2019 <- player_rename(salary_master_2019,"Vincent Velasquez","Vince Velasquez")
salary_master_2019 <- player_rename(salary_master_2019,"J A. Happ","J.A. Happ")
salary_master_2019 <- player_rename(salary_master_2019,"Jake Junis","Jakob Junis")
salary_master_2019 <- player_rename(salary_master_2019,"D J. Johnson","DJ Johnson")
salary_master_2019 <- player_rename(salary_master_2019,"J B. Wendelken","J.B. Wendelken")
salary_master_2019 <- player_rename(salary_master_2019,"Jacob Faria","Jake Faria")
salary_master_2019 <- player_rename(salary_master_2019,"J T. Chargois","JT Chargois")
salary_master_2019 <- player_rename(salary_master_2019,"Wei Yin Chen","Wei-Yin Chen")
salary_master_2019 <- player_rename(salary_master_2019,"Duane Underwood","Duane Underwood Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"A J. Minter","A.J. Minter")
salary_master_2019 <- player_rename(salary_master_2019,"Gerardo Reyes","Gerardo Reyes Jr.")
salary_master_2019 <- player_rename(salary_master_2019,"Wei Chung Wang","Wei-Chung Wang")
salary_master_2019 <- player_rename(salary_master_2019,"T J.","T J. Zeuch")
salary_master_2019 <- player_rename(salary_master_2019,"Foley Sean Reid-Foley","Sean Reid-Foley")
salary_master_2019 <- player_rename(salary_master_2019,"Travis Lakins","Travis Lakins Sr.")
salary_master_2019 <- player_rename(salary_master_2019,"Anthony Gonsolin","Tony Gonsolin")
salary_master_2019 <- player_rename(salary_master_2019,"Jimmie Sherfy","James Sherfy")
salary_master_2019 <- player_rename(salary_master_2019,"Day Darren O'Day","Darren O'Day")
salary_master_2019 <- player_rename(salary_master_2019,"Michael Soroka","Mike Soroka")

baseball_savant_pitchers_2019 <- player_rename(baseball_savant_pitchers_2019,"Phil Maton III","Phil Maton")



# --------------------------------------------------
# Merge baseball_savant_pitchers_2019 data with salary dataframe

pitchers_combined_2019 <- merge(baseball_savant_pitchers_2019,salary_master_2019)
colnames(pitchers_combined_2019) <- c("Name","ID","Year","Age","G","IP","H","HR","K","BB","BAA","OPS","ER","SV","BSV",
                                      "W","L","CG","GDP","Holds","XBA","XWOBA","ExitVelo","LaunchAngle","BarrelPercent",
                                      "FastballPercent","FastballMPH","FastballSpin","BreakingPercent","BreakingMPH","BreakingSpin",
                                      "OffspeedPercent","OffspeedMPH","OffspeedSpin","Age1","Position","Status","BaseSalary","SigningBonus","Incentives","TotalSalary",
                                      "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary","Team")
pitchers_combined_2019 <- subset(pitchers_combined_2019,select=-c(Age1))

# --------------------------------------------------
# Find which players are left out of the new merged data frame
pitchers_difference_2019 <- setdiff(baseball_savant_pitchers_2019$PlayerName,pitchers_combined_2019$Name)
View(pitchers_difference_2019)

# --------------------------------------------------
# Convert position to a numeric field
pitchers_combined_2019 <- pitchers_combined_2019 %>% mutate(positionNum=case_when(Position=="SP"~1,
                                                                                  Position=="RP"~2,
                                                                                  Position=="RP/CL"~3,
                                                                                  Position=="P"~4))
# --------------------------------------------------
# Convert player Adjusted Salary from string to numeric
TotalSalaryP_2019 <- gsub("\\s","",c(pitchers_combined_2019$TotalSalary))
TotalSalaryP_2019 <- gsub(",","",TotalSalaryP_2019)
TotalSalaryP_2019 <- gsub("\\$","",TotalSalaryP_2019)
TotalSalaryP_2019 <- as.numeric(TotalSalaryP_2019)
pitchers_combined_2019 <- cbind(pitchers_combined_2019,TotalSalaryP_2019)

# --------------------------------------------------
# ERA Calculation
pitchers_combined_2019$ERA <- (pitchers_combined_2019$ER/pitchers_combined_2019$IP)*9


# --------------------------------------------------
# Create an all numeric dataframe
pitcher_numeric_2019 <- subset(pitchers_combined_2019,select=-c(Name,Position,Status,BaseSalary,SigningBonus,Incentives,
                                                                TotalSalary,AdjustedSalary,LuxuryTaxSalary,Team))

# --------------------------------------------------
# Pitcher Correlation relative to TotalSalary
library(corrr)

pitcher_correlations_2019 <- correlate(pitcher_numeric_2019)
pitcherCorr <- pitcher_correlations_2019 %>% focus(TotalSalaryP_2019) %>% gather(-rowname,key="colname",value="cor") %>% 
  filter(abs(cor)<1.000)
pitcherCorr <- pitcherCorr %>% arrange(desc(abs(cor)))
View(pitcherCorr)

# --------------------------------------------------
# Dataframe containing only specific predictor variables

pitcher_select_2019 <- data.frame(pitchers_combined_2019$Age,pitchers_combined_2019$K,pitchers_combined_2019$IP,
                                  pitchers_combined_2019$W,pitchers_combined_2019$H,pitchers_combined_2019$HR,pitchers_combined_2019$ER,
                                  pitchers_combined_2019$GDP,pitchers_combined_2019$BB,pitchers_combined_2019$TotalSalaryP_2019)

colnames(pitcher_select_2019) <- c("Age","K","IP","W","H","HR","ER","GDP","BB","TotalSalary2")

# Standardize all variables except for TotalSalary
standardized_pitchers_2019 <- pitcher_select_2019 %>% mutate_at(c("Age","K","IP","W","H","HR","ER","GDP",
                                                                  "BB"),~(scale(.) %>% as.vector))
# --------------------------------------------------
# Linear model for predicting pitcher salary
pitchers_model_2019 <- lm(TotalSalary2~.,data=standardized_pitchers_2019)
summary(pitchers_model_2019)

predictions_2019 <- predict(pitchers_model_2019,pitchers_combined_2019)


# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# Gathering and preprocessing of 2021 position players

team_payroll <- function(spotrac_url,team_name) {
  team_table <- spotrac_url %>% read_html() %>% html_nodes(xpath='//*[@id="main"]/div/div[4]/table[1]') %>% html_table()
  team_salary2021 <- team_table[[1]]
  team_salary2021 <- data.frame(team_salary2021)
  team_tableIL <- spotrac_url %>% read_html() %>% html_nodes(xpath='//*[@id="main"]/div/div[4]/table[2]') %>% html_table()
  teamIL <- team_tableIL[[1]]
  teamIL <- data.frame(teamIL)
  colnames(team_salary2021) <- c("PlayerName","Age","Position","Status","WaiverOptions","BaseSalary","SigningBonus","TotalSalary",
                                 "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary")
  colnames(teamIL) <- c("PlayerName","Age","Position","Status","WaiverOptions","BaseSalary","SigningBonus","TotalSalary",
                        "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary")
  combined_salaries <- rbind(team_salary2021,teamIL)
  combined_salaries <- combined_salaries %>% mutate(Team = team_name)
  return(combined_salaries)
}

# --------------------------------------------------
# Save results of each team payroll as a variable
dbacks2021 <- team_payroll("https://www.spotrac.com/mlb/arizona-diamondbacks/payroll/","Arizona Diamondbacks")
braves2021 <- team_payroll("https://www.spotrac.com/mlb/atlanta-braves/payroll/","Atlanta Braves")
orioles2021 <- team_payroll("https://www.spotrac.com/mlb/baltimore-orioles/payroll/","Baltimore Orioles")
redsox2021 <- team_payroll("https://www.spotrac.com/mlb/boston-red-sox/payroll/","Boston Red Sox")
cubs2021 <- team_payroll("https://www.spotrac.com/mlb/chicago-cubs/payroll/","Chicago Cubs")
whitesox2021 <- team_payroll("https://www.spotrac.com/mlb/chicago-white-sox/payroll/","Chicago White Sox")
reds2021 <- team_payroll("https://www.spotrac.com/mlb/cincinnati-reds/payroll/","Cincinnati Reds")
indians2021 <- team_payroll("https://www.spotrac.com/mlb/cleveland-indians/payroll/","Cleveland Indians")
rockies2021 <- team_payroll("https://www.spotrac.com/mlb/colorado-rockies/payroll/","Colorado Rockies")
tigers2021 <- team_payroll("https://www.spotrac.com/mlb/detroit-tigers/payroll/","Detroit Tigers")
astros2021 <- team_payroll("https://www.spotrac.com/mlb/houston-astros/payroll/","Houston Astros")
royals2021 <- team_payroll("https://www.spotrac.com/mlb/kansas-city-royals/payroll/","Kansas City Royals")
angels2021 <- team_payroll("https://www.spotrac.com/mlb/los-angeles-angels/payroll/","Los Angeles Angels")
dodgers2021 <- team_payroll("https://www.spotrac.com/mlb/los-angeles-dodgers/payroll/","Los Angeles Dodgers")
marlins2021 <- team_payroll("https://www.spotrac.com/mlb/miami-marlins/payroll/","Miami Marlins")
brewers2021 <- team_payroll("https://www.spotrac.com/mlb/milwaukee-brewers/payroll/","Milwaukee Brewers")
twins2021 <- team_payroll("https://www.spotrac.com/mlb/minnesota-twins/payroll/","Minnesota Twins")
mets2021 <- team_payroll("https://www.spotrac.com/mlb/new-york-mets/payroll/","New York Mets")
yankees2021 <- team_payroll("https://www.spotrac.com/mlb/new-york-yankees/payroll/","New York Yankees")
athletics2021 <- team_payroll("https://www.spotrac.com/mlb/oakland-athletics/payroll/","Oakland Athletics")
phillies2021 <- team_payroll("https://www.spotrac.com/mlb/philadelphia-phillies/payroll/","Philadelphia Philles")
pirates2021 <- team_payroll("https://www.spotrac.com/mlb/pittsburgh-pirates/payroll/","Pittsburgh Pirates")
padres2021 <- team_payroll("https://www.spotrac.com/mlb/san-diego-padres/payroll/","San Diego Padres")
giants2021 <- team_payroll("https://www.spotrac.com/mlb/san-francisco-giants/payroll/","San Francisco Giants")
mariners2021 <- team_payroll("https://www.spotrac.com/mlb/seattle-mariners/payroll/","Seattle Mariners")
cardinals2021 <- team_payroll("https://www.spotrac.com/mlb/st-louis-cardinals/payroll/","St. Louis Cardinals")
rays2021 <- team_payroll("https://www.spotrac.com/mlb/tampa-bay-rays/payroll/","Tampa Bay Rays")
rangers2021 <- team_payroll("https://www.spotrac.com/mlb/texas-rangers/payroll/","Texas Rangers")
bluejays2021 <- team_payroll("https://www.spotrac.com/mlb/toronto-blue-jays/payroll/","Toronto Blue Jays")
nationals2021 <- team_payroll("https://www.spotrac.com/mlb/washington-nationals/payroll/","Washington Nationals")

# --------------------------------------------------
# Combine all separate dataframes into one large dataframe

salary_master_2021 <- rbind(dbacks2021,braves2021,orioles2021,redsox2021,cubs2021,whitesox2021,reds2021,
                            indians2021,rockies2021,tigers2021,astros2021,royals2021,angels2021,dodgers2021,marlins2021,
                            brewers2021,twins2021,mets2021,yankees2021,athletics2021,phillies2021,pirates2021,padres2021,
                            giants2021,mariners2021,cardinals2021,rays2021,rangers2021,bluejays2021,nationals2021)


# --------------------------------------------------
# --------------------------------------------------
# Clean up individual team data frames
# Remove descriptions of injuries from players on IL 
# remove_punctuation <- str_replace(clean$PlayerName," \\s*\\([^\\)]+\\)", "")
salary_master_2021 <- name_clean(salary_master_2021)

# --------------------------------------------------
# Hardcode specific problem names

salary_master_2021 <- player_rename(salary_master_2021,"Geus Brett de Geus","Brett de Geus")
salary_master_2021 <- player_rename(salary_master_2021,"Arnaud Travis d'Arnaud","Travis d'Arnaud")
salary_master_2021 <- player_rename(salary_master_2021,"Pozo Migeul Del Pozo","Miguel Del Pozo")
salary_master_2021 <- player_rename(salary_master_2021,"Hernandez Abraham Toro-Hernandez","Abraham Toro")
salary_master_2021 <- player_rename(salary_master_2021,"Jr Jackie Bradley Jr.","Jackie Bradley Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Day Darren O'Day","Darren O'Day")
salary_master_2021 <- player_rename(salary_master_2021,"Jr Fernando Tatis Jr.","Fernando Tatis Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Stella Tommy La Stella","Tommy La Stella")
salary_master_2021 <- player_rename(salary_master_2021,"Neill Tyler O'Neill","Tyler O'Neill")
salary_master_2021 <- player_rename(salary_master_2021,"Falefa Isiah Kiner-Falefa","Isiah Kiner-Falefa")
salary_master_2021 <- player_rename(salary_master_2021,"Jr Lourdes Gurriel Jr.","Lourdes Gurriel Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Jr Vladimir Guerrero Jr.","Vladimir Guerrero Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Jr Ronald Acuna Jr.","Ronald Acuna Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Hearn Ryan O'Hearn","Ryan O'Hearn")
salary_master_2021 <- player_rename(salary_master_2021,"Foley Sean Reid-Foley","Sean Reid-Foley")
salary_master_2021 <- player_rename(salary_master_2021,"Jong Chase De Jong","Chase De Jong")
salary_master_2021 <- player_rename(salary_master_2021,"Grady Brian O'Grady","Brian O'Grady")
salary_master_2021 <- player_rename(salary_master_2021,"Michael Brantley","Michael Brantley Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"J D. Martinez","J.D. Martinez")
salary_master_2021 <- player_rename(salary_master_2021,"D J. LeMahieu","DJ LeMahieu")
salary_master_2021 <- player_rename(salary_master_2021,"C J. Cron","C.J. Cron")
salary_master_2021 <- player_rename(salary_master_2021,"Giovanny Urshela","Gio Urshela")
salary_master_2021 <- player_rename(salary_master_2021,"A J. Pollock","AJ Pollock")
salary_master_2021 <- player_rename(salary_master_2021,"Michael Taylor","Michael A. Taylor")
salary_master_2021 <- player_rename(salary_master_2021,"J T. Realmuto","J.T. Realmuto")
salary_master_2021 <- player_rename(salary_master_2021,"Ji Man Choi","Ji-Man Choi")
salary_master_2021 <- player_rename(salary_master_2021,"D J. Stewart","DJ Stewart")
salary_master_2021 <- player_rename(salary_master_2021,"J P. Crawford","J.P. Crawford")
salary_master_2021 <- player_rename(salary_master_2021,"Josh Fuentes","Joshua Fuentes")
salary_master_2021 <- player_rename(salary_master_2021,"Ke Bryan Hayes","Ke'Bryan Hayes")
salary_master_2021 <- player_rename(salary_master_2021,"LaMonte Wade","LaMonte Wade Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Ka Ai Tom","Ka'ai Tom")
salary_master_2021 <- player_rename(salary_master_2021,"Jazz Chisholm","Jazz Chisholm Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Ha seong Kim","Ha-Seong Kim")
salary_master_2021 <- player_rename(salary_master_2021,"J A. Happ","J.A. Happ")
salary_master_2021 <- player_rename(salary_master_2021,"J B. Wendelken","J.B. Wendelken")
salary_master_2021 <- player_rename(salary_master_2021,"J T. Chargois","JT Chargois")
salary_master_2021 <- player_rename(salary_master_2021,"Lance McCullers","Lance McCullers Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Duane Underwood","Duane Underwood Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"A J. Minter","A.J. Minter")
salary_master_2021 <- player_rename(salary_master_2021,"T J.","T J. Zeuch")
salary_master_2021 <- player_rename(salary_master_2021,"J C. Mejia","J.C. Mejia")
salary_master_2021 <- player_rename(salary_master_2021,"J B. Bukauskas","J.B. Bukauskas")
salary_master_2021 <- player_rename(salary_master_2021,"J P. Feyereisen","J.P. Feyereisen")
salary_master_2021 <- player_rename(salary_master_2021,"Travis Lakins","Travis Lakins Sr.")
salary_master_2021 <- player_rename(salary_master_2021,"Nelson Cruz","Nelson Cruz Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Manuel Pina","Manny Pina")
salary_master_2021 <- player_rename(salary_master_2021,"Yulieski Gurriel","Yuli Gurriel")
salary_master_2021 <- player_rename(salary_master_2021,"Philip Gosselin","Phil Gosselin")
salary_master_2021 <- player_rename(salary_master_2021,"Joe Wendle","Joey Wendle")
salary_master_2021 <- player_rename(salary_master_2021,"Anthony Gonsolin","Tony Gonsolin")
salary_master_2021 <- player_rename(salary_master_2021,"Los Santos Enyel De Los Santos","Enyel De Los Santos")
salary_master_2021 <- player_rename(salary_master_2021,"Pozo Miguel Del Pozo","Miguel Del Pozo")
salary_master_2021 <- player_rename(salary_master_2021,"Kwang hyun Kim","Kwang Hyun Kim")
salary_master_2021 <- player_rename(salary_master_2021,"Vincent Velasquez","Vince Velasquez")
salary_master_2021 <- player_rename(salary_master_2021,"Jacob Faria","Jake Faria")
salary_master_2021 <- player_rename(salary_master_2021,"Alexander Reyes","Alex Reyes")
salary_master_2021 <- player_rename(salary_master_2021,"Samuel Clay","Sam Clay")
salary_master_2021 <- player_rename(salary_master_2021,"T J. Zeuch","T.J. Zeuch")
salary_master_2021 <- player_rename(salary_master_2021,"Sam Long","Sammy Long")
salary_master_2021 <- player_rename(salary_master_2021,"Dan Vogelbach","Daniel Vogelbach")
salary_master_2021 <- player_rename(salary_master_2021,"Daniel Santana","Danny Santana")
salary_master_2021 <- player_rename(salary_master_2021,"John Nogowski","John Nogowski Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"J D. Davis","J.D. Davis")
salary_master_2021 <- player_rename(salary_master_2021,"Shed Long","Shed Long Jr.")
salary_master_2021 <- player_rename(salary_master_2021,"Yu Cheng Chang","Yu Chang")
salary_master_2021 <- player_rename(salary_master_2021,"Nate Lowe","Nathaniel Lowe")
salary_master_2021 <- player_rename(salary_master_2021,"Lucas Williams","Luke Williams")
salary_master_2021 <- player_rename(salary_master_2021,"Nicholas Lopez","Nicky Lopez")
salary_master_2021 <- player_rename(salary_master_2021,"Andy Young","Andrew Young")
salary_master_2021 <- player_rename(salary_master_2021,"Kwang hyun Kim","Kwang Hyun Kim")
salary_master_2021 <- player_rename(salary_master_2021,"Cameron Gallagher","Cam Gallagher")
salary_master_2021 <- player_rename(salary_master_2021,"Matt Boyd","Matthew Boyd")

# End of salary data gathering and preprocessing
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# Data collected from https://baseballsavant.mlb.com/, Baseball Savant
# Position Player Data current as of 8/10/2021

baseball_savant_2021 <- read_csv("F:\\MLB2021Analysis\\baseball_savant_8-17-21.csv")
baseball_savant_2021 <- subset(baseball_savant_2021,select=-c(X31))

# Combine first and last name columns into one
baseball_savant_2021$PlayerName <- paste(baseball_savant_2021$first_name,baseball_savant_2021$last_name)
baseball_savant_2021 <- subset(baseball_savant_2021,select=-c(first_name,last_name))

# --------------------------------------------------
# Hardcode specific problem names from baseball savant data
baseball_savant_2021 <- player_rename(baseball_savant_2021,"George Springer III","George Springer")
baseball_savant_2021 <- player_rename(baseball_savant_2021,"Jazz Chisholm Jr. Jr.","Jazz Chisholm Jr.")
baseball_savant_2021 <- player_rename(baseball_savant_2021,"Trey Mancini III","Trey Mancini")
baseball_savant_2021 <- player_rename(baseball_savant_2021,"Cedric Mullins II","Cedric Mullins")
baseball_savant_2021 <- player_rename(baseball_savant_2021,"Luke Voit III","Luke Voit")
baseball_savant_2021 <- player_rename(baseball_savant_2021,"Buster Posey III","Buster Posey")

# --------------------------------------------------
# Merge baseball_savant_2021 data with salary_master_2021 dataset
hitters_combined_2021 <- merge(baseball_savant_2021,salary_master_2021)
colnames(hitters_combined_2021) <- c("Name","ID","Year","Age","AB","PA","Hits","Doubles","Triples","HomeRuns","K","BB","RBI","CS","SB","G","GDP","HBP","SACBunt",
                                     "SACFly","Runs","XBA","XSLG","XOBP","ExitVeloAVG","LaunchAngleAVG","BarrelPercent","HardHitPercent",
                                     "SprintSpeed","Age1","Position","Status","WaiverOptions","BaseSalary","SigningBonus","TotalSalary",
                                     "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary","Team")
hitters_combined_2021 <- subset(hitters_combined_2021,select=-c(Age1))

# --------------------------------------------------
# Convert Position to a numeric value
hitters_combined_2021 <- hitters_combined_2021 %>% mutate(PositionNum=case_when(Position=="SP"~1,
                                                                                Position=="C"~2,
                                                                                Position=="1B"~3,
                                                                                Position=="2B"~4,
                                                                                Position=="3B"~5,
                                                                                Position=="SS"~6,
                                                                                Position=="LF"~7,
                                                                                Position=="CF"~8,
                                                                                Position=="RF"~9,
                                                                                Position=="OF"~10,
                                                                                Position=="DH"~11,
                                                                                Position=="RP"~12,
                                                                                Position=="RP/CL"~13,
                                                                                Position=="SP"~14))

# --------------------------------------------------
# Find which players are left out of the new merged data frame
hitters_difference_2021 <- setdiff(baseball_savant_2021$PlayerName,hitters_combined_2021$Name)
# View(hitters_difference_2021)


# Statistics Calculations
# --------------------------------------------------
# Singles
hitters_combined_2021$Singles <- hitters_combined_2021$Hits-(hitters_combined_2021$Doubles+hitters_combined_2021$Triples+hitters_combined_2021$HomeRuns) 

# AVG = Hits/AB
hitters_combined_2021$AVG <- (hitters_combined_2021$Hits/hitters_combined_2021$AB)

# --------------------------------------------------
# OBP = (Hits+BB+HBP)/(AB+BB+HBP+SAC)
hitters_combined_2021$OBP <- (hitters_combined_2021$Hits+hitters_combined_2021$BB+hitters_combined_2021$HBP)/
  (hitters_combined_2021$AB+hitters_combined_2021$BB+hitters_combined_2021$HBP+hitters_combined_2021$SACBunt+hitters_combined_2021$SACFly)


# --------------------------------------------------
# SLG = (1B+(2*2B)+(3*3B)+(4*HR))/AB
hitters_combined_2021$SLG <- (hitters_combined_2021$Singles+(2*hitters_combined_2021$Doubles)+(3*hitters_combined_2021$Triples)+
                                (4*hitters_combined_2021$HomeRuns))/hitters_combined_2021$AB

# --------------------------------------------------
# OPS = OBP+SLG
hitters_combined_2021$OPS <- hitters_combined_2021$OBP + hitters_combined_2021$SLG
# View(hitters_combined_2021)

# --------------------------------------------------
# Average Salary by Position
# Convert player Adjusted Salary from string to numeric
TotalSalary_2021 <- gsub("\\s","",c(hitters_combined_2021$TotalSalary))
TotalSalary_2021 <- gsub(",","",TotalSalary_2021)
TotalSalary_2021 <- gsub("\\$","",TotalSalary_2021)
TotalSalary_2021 <- as.numeric(TotalSalary_2021)
hitters_combined_2021 <- cbind(hitters_combined_2021,TotalSalary_2021)
colnames(hitters_combined_2021)[colnames(hitters_combined_2021) == 'TotalSalary_2021'] <- 'TotalSalary1'

# --------------------------------------------------
# Standardized 2021 hitters_combined dataframe

standardized_hitters_2021 <- hitters_combined_2021 %>% mutate_at(c("Age","GDP","AB","RBI","BB","Hits",
                                                                   "Runs","G","HomeRuns","SACFly"),~(scale(.) %>% as.vector))


# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# 2021 Pitcher data
# Pitcher Data current as of 8/10/2021

baseball_savant_pitchers_2021 <- read_csv("F:\\MLB2021Analysis\\baseball_savant_pitchers8-10-21.csv")
baseball_savant_pitchers_2021 <- subset(baseball_savant_pitchers_2021,select=-c(X36))

# combine first and last name columns into one
baseball_savant_pitchers_2021$PlayerName <- paste(baseball_savant_pitchers_2021$first_name,baseball_savant_pitchers_2021$last_name)
baseball_savant_pitchers_2021 <- subset(baseball_savant_pitchers_2021,select=-c(first_name,last_name))

# Rename pitchers in the baseball_savant_pitchers_2021 dataframe
baseball_savant_pitchers_2021 <- player_rename(baseball_savant_pitchers_2021,"Phil Maton III","Phil Maton")


# Combine baseball_savant data with salary_master_2021 dataset
pitchers_combined_2021 <- merge(baseball_savant_pitchers_2021,salary_master_2021)
colnames(pitchers_combined_2021) <- c("Name","ID","Year","Age","G","IP","H","HR","K","BB","BAA","OPS","ER","Saves","BlownSaves",
                                      "W","L","GDP","HBP","GS","XBA","XWOBA","ExitVelo","LaunchAngle","BarrelPercent",
                                      "FBPercent","FBMPH","FBSpin","BreakingPercent","BreakingMPH","BreakingSpin","OffspeedPercent",
                                      "OffspeedMPH","OffspeedSpin","Age1","Position","Status","WaiverOptions","BaseSalary","SigningBonus","TotalSalary",
                                      "AdjustedSalary","PayrollPercentage","LuxuryTaxSalary","Team")
pitchers_combined_2021 <- subset(pitchers_combined_2021,select=-c(Age1))

# Convert position to a numeric value
# Starting pitchers = 1; relief pitchers = 2; relief pitchers/closers = 3; pitchers = 4
pitchers_combined_2021 <- pitchers_combined_2021 %>% mutate(positionNum=case_when(Position=="SP"~1,
                                                                                  Position=="RP"~2,
                                                                                  Position=="RP/CL"~3,
                                                                                  Position=="P"~4))

# View which players did not merge when combined
pitcher_difference_2021 <- setdiff(baseball_savant_pitchers_2021$PlayerName,pitchers_combined_2021$Name)
View(pitcher_difference)

# Convert player Adjusted Salary from string to numeric
TotalSalaryP_2021 <- gsub("\\s","",c(pitchers_combined_2021$TotalSalary))
TotalSalaryP_2021 <- gsub(",","",TotalSalaryP_2021)
TotalSalaryP_2021 <- gsub("\\$","",TotalSalaryP_2021)
TotalSalaryP_2021 <- as.numeric(TotalSalaryP_2021)
pitchers_combined_2021 <- cbind(pitchers_combined_2021,TotalSalaryP_2021)
colnames(pitchers_combined_2021)[colnames(pitchers_combined_2021) == 'TotalSalaryP_2021'] <- 'TotalSalary2'


# --------------------------------------------------
# ERA Calculation
pitchers_combined_2021$ERA <- (pitchers_combined_2021$ER/pitchers_combined_2021$IP)*9

# --------------------------------------------------
# Standardized 2021 pitchers_combined dataframe

standardized_pitchers_2021 <- pitchers_combined_2021 %>% mutate_at(c("Age","K","IP","W","H","HR","ER","GDP",
                                                                     "BB"),~(scale(.) %>% as.vector))



# End of processing 2021 position and pitcher data from baseball savant
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------

# Predictions of 2021 MLB player salaries
set.seed(123456789)

# Hitter Predictions
hitter_predictions_2021 <- predict(hitters_model_2019,standardized_hitters_2021)
hitter_predictions_df <- data.frame(standardized_hitters_2021$Name,standardized_hitters_2021$TotalSalary1,hitter_predictions_2021)
hitter_predictions_df$SalaryError <- hitter_predictions_df$hitter_predictions_2021-hitter_predictions_df$standardized_hitters_2021.TotalSalary1
colnames(hitter_predictions_df) <- c("Name","ActualSalary","PredictedSalary","SalaryError")

avg_hitter_salary_error <- (abs(sum(hitter_predictions_df$SalaryError)))/nrow(hitter_predictions_df)
avg_hitter_salary_error


# Pitcher Predictions
pitcher_predictions_2021 <- predict(pitchers_model_2019,standardized_pitchers_2021)
pitcher_predictions_df <- data.frame(standardized_pitchers_2021$Name,standardized_pitchers_2021$TotalSalary2,pitcher_predictions_2021)
pitcher_predictions_df$SalaryError <- pitcher_predictions_df$pitcher_predictions_2021-pitcher_predictions_df$standardized_pitchers_2021.TotalSalary2
colnames(pitcher_predictions_df) <- c("Name","ActualSalary","PredictedSalary","SalaryError")

avg_pitcher_salary_error <- (abs(sum(pitcher_predictions_df$SalaryError)))/nrow(pitcher_predictions_df)
avg_pitcher_salary_error


# --------------------------------------------------
# Prediction Results

hitter_overpaid <- hitter_predictions_df[order(hitter_predictions_df$SalaryError),]
View(hitter_overpaid)
hitter_underpaid <- hitter_predictions_df[order(-hitter_predictions_df$SalaryError),]
View(hitter_underpaid)

pitcher_overpaid <- pitcher_predictions_df[order(pitcher_predictions_df$SalaryError),]
View(pitcher_overpaid)
pitcher_underpaid <- pitcher_predictions_df[order(-pitcher_predictions_df$SalaryError),]
View(pitcher_underpaid)

# --------------------------------------------------
# Other Salary Prediction Models using other variables


# Hitter Models

# Position Player based on AB,AVG,HR,RBI,OPS,R,BB,SprintSpeed,BarrelPercent,LaunchAngleAVG WITH Age
standardized_hitters_2019_Age <- hitters_combined_2019 %>% mutate_at(c("Age","AB","AVG","OPS","Runs","BB","SprintSpeed",
                                                                       "HomeRuns","BarrelPercent","LaunchAngleAVG"),~(scale(.) %>% as.vector))
standardized_hitters_2021_Age <- hitters_combined_2021 %>% mutate_at(c("Age","AB","AVG","OPS","Runs","BB","SprintSpeed",
                                                                       "HomeRuns","BarrelPercent","LaunchAngleAVG"),~(scale(.) %>% as.vector))

# Linear model for predicting position player salary
hitters_model_2021_Age <- lm(TotalSalary1~Age+AB+AVG+OPS+Runs+BB+SprintSpeed+HomeRuns+BarrelPercent+LaunchAngleAVG,data=standardized_hitters_2019_Age)
summary(hitters_model_2021_Age)

new_predictions <- predict(hitters_model_2021_Age,standardized_hitters_2021_Age)
hitter_predictions_df_Age <- data.frame(standardized_hitters_2021_Age$Name,standardized_hitters_2021_Age$TotalSalary1,new_predictions)
colnames(hitter_predictions_df_Age) <- c("Name","ActualSalary","PredictedSalary")
hitter_predictions_df_Age$SalaryError <- hitter_predictions_df_Age$PredictedSalary-hitter_predictions_df_Age$ActualSalary


avg_error_age <- (abs(sum(hitter_predictions_df_Age$SalaryError)))/nrow(hitter_predictions_df_Age)
avg_error_age

hitter_overpaid_age <- hitter_predictions_df_Age[order(hitter_predictions_df_Age$SalaryError),]
View(hitter_overpaid_age)
hitter_underpaid_age <- hitter_predictions_df_Age[order(-hitter_predictions_df_Age$SalaryError),]
View(hitter_underpaid_age)

# Position Player based on AB,AVG,HR,RBI,OPS,R,BB,SprintSpeed,BarrelPercent,LaunchAngleAVG WITHOUT Age
standardized_hitters_2019_NoAge <- hitters_combined_2019 %>% mutate_at(c("AB","AVG","OPS","Runs","BB","SprintSpeed",
                                                                         "HomeRuns","BarrelPercent","LaunchAngleAVG"),~(scale(.) %>% as.vector))
standardized_hitters_2021_NoAge <- hitters_combined_2021 %>% mutate_at(c("AB","AVG","OPS","Runs","BB","SprintSpeed",
                                                                         "HomeRuns","BarrelPercent","LaunchAngleAVG"),~(scale(.) %>% as.vector))

# Linear model for predicting position player salary
hitters_model_2019_NoAge <- lm(TotalSalary1~AB+AVG+OPS+Runs+BB+SprintSpeed+HomeRuns+BarrelPercent+LaunchAngleAVG,data=standardized_hitters_2019_NoAge)
summary(hitters_model_2019_NoAge)

predictions_NoAge <- predict(hitters_model_2019_NoAge,standardized_hitters_2021_NoAge)
hitter_predictions_df_NoAge <- data.frame(standardized_hitters_2021_NoAge$Name,standardized_hitters_2021_NoAge$TotalSalary1,predictions_NoAge)
colnames(hitter_predictions_df_NoAge) <- c("Name","ActualSalary","PredictedSalary")
hitter_predictions_df_NoAge$SalaryError <- hitter_predictions_df_NoAge$PredictedSalary-hitter_predictions_df_NoAge$ActualSalary


avg_error_noage <- (abs(sum(hitter_predictions_df_NoAge$SalaryError)))/nrow(hitter_predictions_df_NoAge)
avg_error_noage

hitter_overpaid_noage <- hitter_predictions_df_NoAge[order(hitter_predictions_df_NoAge$SalaryError),]
View(hitter_overpaid_noage)
hitter_underpaid_noage <- hitter_predictions_df_NoAge[order(-hitter_predictions_df_NoAge$SalaryError),]
View(hitter_underpaid_noage)

View(hitters_combined_2021[hitters_combined_2021$Name=="Shohei Ohtani",])

undervalued_top <- head(hitter_underpaid_noage,25)


undervalued_pivot <- undervalued_top %>% pivot_longer(ActualSalary:PredictedSalary)



# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# Hitters
# Standardize all variables considered a positive measurement (No strikeouts, CS, etc.), total those statistics
# and divide by the players TotalSalary to give each player a score based on their performance
# A higher score means that the player is performing positively relative to their pay
options(scipen=100)

hitters_standardized_df <- hitters_combined_2021 %>% mutate_at(c("G","Singles","Doubles","Triples","HomeRuns","BB","RBI",
                                                                 "SB","HBP","SACBunt","SACFly","Runs","ExitVeloAVG","LaunchAngleAVG",
                                                                 "BarrelPercent","HardHitPercent","SprintSpeed","AVG","OBP","SLG","OPS"),~(scale(.)%>% as.vector))
# Filter players who have too little plate appearances
hitters_standardized_df <- hitters_standardized_df %>% filter(PA >= (0.25*max(PA)))

hitters_standardized_df$ZSum = (hitters_standardized_df$Singles+hitters_standardized_df$Doubles+hitters_standardized_df$Triples+
                                  hitters_standardized_df$HomeRuns+hitters_standardized_df$BB+hitters_standardized_df$SB+hitters_standardized_df$HBP+
                                  hitters_standardized_df$SACBunt+hitters_standardized_df$SACFly+hitters_standardized_df$Runs+hitters_standardized_df$ExitVeloAVG+
                                  hitters_standardized_df$LaunchAngleAVG+hitters_standardized_df$BarrelPercent+hitters_standardized_df$G+hitters_standardized_df$HardHitPercent+
                                  hitters_standardized_df$SprintSpeed+hitters_standardized_df$AVG+hitters_standardized_df$OBP+hitters_standardized_df$SLG+
                                  hitters_standardized_df$OPS)*100
hitters_standardized_df$TotalSalarySqrt = sqrt(hitters_standardized_df$TotalSalary1)
hitters_standardized_df$PerformanceGrade = ((hitters_standardized_df$ZSum/hitters_standardized_df$TotalSalarySqrt)*100)

performance_grade_df <- hitters_standardized_df %>% select(Name,TotalSalary1,ZSum,PerformanceGrade)
outperforming <- performance_grade_df[order(-performance_grade_df$PerformanceGrade),]
underperforming <- performance_grade_df[order(performance_grade_df$PerformanceGrade),]


# Players whose salary is greater than $10 million
overpaid_hitters <- hitters_standardized_df %>% filter(TotalSalary1 >=10000000)
overpaid_df <- overpaid_hitters %>% select(Name,TotalSalary1,ZSum,PerformanceGrade)
overpaid_hitters <- overpaid_df[order(overpaid_df$PerformanceGrade),]
high_paid_performing_hitters <- overpaid_df[order(-overpaid_df$PerformanceGrade),]

# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
# Pitchers
# Sum up positive metrics and subtract by negative metrics to get a players performance score
# A higher score indicates a player is performing better
# Divide that score by the players Total Salary to get that player's performance grade

# filter pitchers who meet a threshold of innings pitched
pitchers_standardized_df <- pitchers_combined_2021 %>% filter(IP >= 0.15*(max(IP)))

pitchers_standardized_df <- pitchers_standardized_df %>% mutate_at(c("G","IP","K","W","H","HR",
                                                                     "BB","BAA","OPS","ERA"),~(scale(.)%>% as.vector))

pitchers_standardized_df$ZSum <- ((pitchers_standardized_df$G+pitchers_standardized_df$IP+pitchers_standardized_df$K+pitchers_standardized_df$W)-(pitchers_standardized_df$H+
                                                                                                                                                    pitchers_standardized_df$HR+pitchers_standardized_df$BB+pitchers_standardized_df$BAA+pitchers_standardized_df$OP+pitchers_standardized_df$ERA))*100
pitchers_standardized_df$TotalSalarySqrt <- sqrt(pitchers_standardized_df$TotalSalary2)
pitchers_standardized_df$PerformanceGrade <- (pitchers_standardized_df$ZSum/pitchers_standardized_df$TotalSalarySqrt)*100

pitcher_performance_df <- pitchers_standardized_df %>% select(Name,TotalSalary2,ZSum,PerformanceGrade)
outperforming_pitchers <- pitcher_performance_df[order(-pitcher_performance_df$PerformanceGrade),]
underperforming_pitchers <- pitcher_performance_df[order(pitcher_performance_df$PerformanceGrade),]

# Filtered by pitchers making over $5 million
overpaid_pitchers <- pitchers_standardized_df %>% filter(TotalSalary2 >= 5000000)
overpaid_pitchers_df <- overpaid_pitchers %>% select(Name,TotalSalary2,ZSum,PerformanceGrade)
overpaid_pitchers <- overpaid_pitchers_df[order(overpaid_pitchers_df$PerformanceGrade),]
high_paid_performing_pitchers <- overpaid_pitchers_df[order(-overpaid_pitchers_df$PerformanceGrade),]

# git push


