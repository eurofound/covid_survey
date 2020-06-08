# This script reads a CSV file in GNU R.
# While reading this file, comments will be created for all variables.
# The comments for values will be stored as attributes (attr) as well.
# This script has been generated by SocSci and modified by Eurofound. 

# API link of the SocSci database of the Eurofound survey. API token is confidential and stored in secrets file.
source("secrets.R")
ds_file = paste0("https://s2survey.net/eurofound/?act=", token1)

#This is different in the dashboard. THe dashboard is pulling data from the API while being on the shinyapps.io server
#The logs showed an issue with the certificates. This is to cicrumvent that issue (an R-studio helpdesk provided me with this)
library(curl)
h <- new_handle()
handle_setopt(h, ssl_verifypeer = 0);

# Reading in the data. 
ds = read.table(
  file=curl(ds_file,handle=h), #ALSO HERE IT IS DIFFERENT, KEEP THIS OTHERWISE YOU WILL GET AN ERROR
  encoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", #row.names = "CASE",
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","LANGUAGE","STARTED","B001","B002",
    "B003_01","C001_01","C002_01","C003_01","C003_02","C003_03","C003_04","C004_01",
    "C005_01","C005_02","C005_03","C005_04","C005_05","C006_01","C006_02","C006_03",
    "C007_01","C007_02","C007_03","C007_04","C007_05","C008","D001","D002","D003",
    "D004_01","D004_02","D004_03","D004_04","D004_05","D005_01","D006_01","D007_01",
    "D008_01","E001_01","E002_01","E002_02","E003_01","E003_02","E003_03","E003_04",
    "E003_05","E003_06","E004","E005","E006","E007_01","E008_01","E008_02",
    "E008_03","E008_04","E008_05","E008_06","F001_01","F001_01a","F002_01",
    "F003_01","F003_01a","F003_02","F003_02a","F004","F005","F006","F007","F008",
    "F009","F010","F011","F012","F013","F014","F015","F016","F017","F018","F019",
    "F020","F021","F022_01","CountryWrong","B204","B202","B203_01","C201_01",
    "C202_01","C203_01","C203_07","C203_03","C203_04","C203_05","C203_06","C204_01",
    "C205_01","C205_02","C205_03","C205_04","C205_05","C206_01","C206_02","C206_03",
    "C207_01","C207_02","C207_05","C207_03","C207_04","C208","C209_01","D235",
    "D201","D203","D204_01","D204_02","D204_03","D204_04","D204_05","D205_01",
    "D206_01","D208_01","D209","D210","D210_01","D210_02","D210_03","D210_04",
    "D211_01","D211_01a","D211_02","D211_02a","D212_02","D212_02a","D212_03",
    "D212_03a","D212_04","D212_04a","D212_05","D212_05a","D212_06","D212_06a",
    "D212_07","D212_07a","D213_01","D213_02","D213_03","D213_04","D213_05",
    "D213_06","D213_07","D214","D215_01","D215_02","D215_03","D215_04","D215_05",
    "D216_01","D217","D218_01","D219_01","D220","D221","D222_01","D222_02",
    "D222_03","D222_04","D223","D224_01","D225","D226","D227_01","D228_01",
    "D228_02","D229_01","D230_01","D230_02","D230_03","D230_04","D230_05","D231_01",
    "D232_01","D232_02","D233_01","D233_02","D234_01","D234_02","D234_03","D234_04",
    "E201_01","E203_01","E203_02","E203_03","E203_04","E203_05","E203_06","E203_07",
    "E204","E205","E206","E207_01","E208","F201_01","F201_01a","F202","F203_01",
    "F203_01a","F203_02","F203_02a","F203_03","F203_03a","F204","F221","F222_01",
    "F223","F224","F225","F226","F227_01","F227_02","F227_03","F227_04","F228_01",
    "F228_02","F228_03","F228_04","F228_05","F228_06","F229_01","F229_02","F229_03",
    "F229_04","F230_01","F230_02","F230_03","F230_04","F230_05","F231_01","F231_02",
    "F231_03","F231_04","F231_05","F232","F233_01","F233_02","F233_03","F233_04",
    "F233_05","F233_06","F233_07","F234_01_CN","F234_01_1","F234_01_2","F234_02_CN",
    "F234_02_1","F234_02_2","F234_03_CN","F234_03_1","F234_03_2","F234_04_CN",
    "F234_04_1","F234_04_2","F234_05_CN","F234_05_1","F234_05_2","F235_01",
    "F235_02","F235_03","F235_04","F235_05","F236","TIME001","TIME002","TIME003",
    "TIME004","TIME005","TIME006","TIME007","TIME008","TIME009","TIME010","TIME011",
    "TIME012","TIME013","TIME014","TIME015","TIME016","TIME017","TIME018","TIME019",
    "TIME020","TIME021","TIME022","TIME023","TIME024","TIME025","TIME026","TIME027",
    "TIME028","TIME_SUM","MAILSENT","LASTDATA","FINISHED","Q_VIEWER","LASTPAGE",
    "MAXPAGE"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
    MODE="character", LANGUAGE="character", STARTED="POSIXct", B001="numeric",
    B002="numeric", B003_01="numeric", C001_01="numeric", C002_01="numeric",
    C003_01="numeric", C003_02="numeric", C003_03="numeric", C003_04="numeric",
    C004_01="numeric", C005_01="numeric", C005_02="numeric", C005_03="numeric",
    C005_04="numeric", C005_05="numeric", C006_01="numeric", C006_02="numeric",
    C006_03="numeric", C007_01="numeric", C007_02="numeric", C007_03="numeric",
    C007_04="numeric", C007_05="numeric", C008="numeric", D001="numeric",
    D002="numeric", D003="numeric", D004_01="numeric", D004_02="numeric",
    D004_03="numeric", D004_04="numeric", D004_05="numeric", D005_01="numeric",
    D006_01="numeric", D007_01="numeric", D008_01="numeric", E001_01="numeric",
    E002_01="numeric", E002_02="numeric", E003_01="numeric", E003_02="numeric",
    E003_03="numeric", E003_04="numeric", E003_05="numeric", E003_06="numeric",
    E004="numeric", E005="numeric", E006="numeric", E007_01="numeric",
    E008_01="numeric", E008_02="numeric", E008_03="numeric", E008_04="numeric",
    E008_05="numeric", E008_06="numeric", F001_01="numeric", F001_01a="logical",
    F002_01="numeric", F003_01="numeric", F003_01a="logical", F003_02="numeric",
    F003_02a="logical", F004="numeric", F005="numeric", F006="numeric",
    F007="numeric", F008="numeric", F009="numeric", F010="numeric",
    F011="numeric", F012="numeric", F013="numeric", F014="numeric",
    F015="numeric", F016="numeric", F017="numeric", F018="numeric",
    F019="numeric", F020="numeric", F021="character", F022_01="numeric",
    CountryWrong="numeric", B204="numeric", B202="numeric", B203_01="numeric",
    C201_01="numeric", C202_01="numeric", C203_01="numeric", C203_07="numeric",
    C203_03="numeric", C203_04="numeric", C203_05="numeric", C203_06="numeric",
    C204_01="numeric", C205_01="numeric", C205_02="numeric", C205_03="numeric",
    C205_04="numeric", C205_05="numeric", C206_01="numeric", C206_02="numeric",
    C206_03="numeric", C207_01="numeric", C207_02="numeric", C207_05="numeric",
    C207_03="numeric", C207_04="numeric", C208="numeric", C209_01="numeric",
    D235="numeric", D201="numeric", D203="numeric", D204_01="numeric",
    D204_02="numeric", D204_03="numeric", D204_04="numeric", D204_05="numeric",
    D205_01="numeric", D206_01="numeric", D208_01="numeric", D209="numeric",
    D210="numeric", D210_01="logical", D210_02="logical", D210_03="logical",
    D210_04="logical", D211_01="numeric", D211_01a="logical", D211_02="numeric",
    D211_02a="logical", D212_02="numeric", D212_02a="logical",
    D212_03="numeric", D212_03a="logical", D212_04="numeric",
    D212_04a="logical", D212_05="numeric", D212_05a="logical",
    D212_06="numeric", D212_06a="logical", D212_07="numeric",
    D212_07a="logical", D213_01="numeric", D213_02="numeric", D213_03="numeric",
    D213_04="numeric", D213_05="numeric", D213_06="numeric", D213_07="numeric",
    D214="numeric", D215_01="numeric", D215_02="numeric", D215_03="numeric",
    D215_04="numeric", D215_05="numeric", D216_01="numeric", D217="numeric",
    D218_01="numeric", D219_01="numeric", D220="numeric", D221="numeric",
    D222_01="numeric", D222_02="numeric", D222_03="numeric", D222_04="numeric",
    D223="numeric", D224_01="numeric", D225="numeric", D226="numeric",
    D227_01="numeric", D228_01="numeric", D228_02="numeric", D229_01="numeric",
    D230_01="numeric", D230_02="numeric", D230_03="numeric", D230_04="numeric",
    D230_05="numeric", D231_01="numeric", D232_01="numeric", D232_02="numeric",
    D233_01="numeric", D233_02="numeric", D234_01="numeric", D234_02="numeric",
    D234_03="numeric", D234_04="numeric", E201_01="numeric", E203_01="numeric",
    E203_02="numeric", E203_03="numeric", E203_04="numeric", E203_05="numeric",
    E203_06="numeric", E203_07="numeric", E204="numeric", E205="numeric",
    E206="numeric", E207_01="numeric", E208="numeric", F201_01="numeric",
    F201_01a="logical", F202="numeric", F203_01="numeric", F203_01a="logical",
    F203_02="numeric", F203_02a="logical", F203_03="numeric",
    F203_03a="logical", F204="numeric", F221="character", F222_01="numeric",
    F223="numeric", F224="numeric", F225="numeric", F226="numeric",
    F227_01="numeric", F227_02="numeric", F227_03="numeric", F227_04="numeric",
    F228_01="numeric", F228_02="numeric", F228_03="numeric", F228_04="numeric",
    F228_05="numeric", F228_06="numeric", F229_01="numeric", F229_02="numeric",
    F229_03="numeric", F229_04="numeric", F230_01="numeric", F230_02="numeric",
    F230_03="numeric", F230_04="numeric", F230_05="numeric", F231_01="numeric",
    F231_02="numeric", F231_03="numeric", F231_04="numeric", F231_05="numeric",
    F232="numeric", F233_01="numeric", F233_02="numeric", F233_03="numeric",
    F233_04="numeric", F233_05="numeric", F233_06="numeric", F233_07="numeric",
    F234_01_CN="numeric", F234_01_1="logical", F234_01_2="logical",
    F234_02_CN="numeric", F234_02_1="logical", F234_02_2="logical",
    F234_03_CN="numeric", F234_03_1="logical", F234_03_2="logical",
    F234_04_CN="numeric", F234_04_1="logical", F234_04_2="logical",
    F234_05_CN="numeric", F234_05_1="logical", F234_05_2="logical",
    F235_01="numeric", F235_02="numeric", F235_03="numeric", F235_04="numeric",
    F235_05="numeric", F236="numeric", TIME001="integer", TIME002="integer",
    TIME003="integer", TIME004="integer", TIME005="integer", TIME006="integer",
    TIME007="integer", TIME008="integer", TIME009="integer", TIME010="integer",
    TIME011="integer", TIME012="integer", TIME013="integer", TIME014="integer",
    TIME015="integer", TIME016="integer", TIME017="integer", TIME018="integer",
    TIME019="integer", TIME020="integer", TIME021="integer", TIME022="integer",
    TIME023="integer", TIME024="integer", TIME025="integer", TIME026="integer",
    TIME027="integer", TIME028="integer", TIME_SUM="integer",
    MAILSENT="POSIXct", LASTDATA="POSIXct", FINISHED="logical",
    Q_VIEWER="logical", LASTPAGE="numeric", MAXPAGE="numeric"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

rm(ds_file)

attr(ds, "project") = "eurofound"
attr(ds, "description") = "Eurofound e-survey Living, working and COVID-19 "
attr(ds, "date") = Sys.time()
attr(ds, "server") = "https://s2survey.net"

#Keep only wave 1 variables by testing that the second character of the variable name is not 2
ds <- ds[,substring(colnames(ds),2,2)!=2]

# Variable und Value Labels
ds$EU27 = ds$B001>0 & ds$B001<28
ds$B001 = factor(ds$B001, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60"), labels=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","Albania","Bosnia and Herzegovina","Brazil","Canada","China","Colombia","Ecuador","Egypt","India","Indonesia","Iran","Japan","Mexico","Montenegro","Morocco","Netherlands Antilles","Nigeria","North Macedonia","Pakistan","Philippines","Russia","Serbia","South Korea","Switzerland","Suriname","Syria","Thailand","Turkey","Ukraine","United Kingdom","United States","Vietnam","Other country"), ordered=FALSE)
levels(ds$B001)[c(28:56,58:60)] <- "Other country"
ds$B002 = factor(ds$B002, levels=c("1","2","3"), labels=c("Male","Female","In another way"), ordered=FALSE)
ds$C008 = factor(ds$C008, levels=c("1","2","3","4"), labels=c("The open countryside","A village/small town","A medium to large town","A city or city suburb"), ordered=FALSE)
ds$D001 = factor(ds$D001, levels=c("1","2","3","4","5","6","7","8"), labels=c("Employee","Self-employed with employees","Self-employed without employees","Unemployed","Unable to work due to long-term illness or disability","Retired","Full-time homemaker/fulfilling domestic tasks","Student"), ordered=FALSE)
ds$D002 = factor(ds$D002, levels=c("1","2","3"), labels=c("Yes, permanently","Yes, temporarily","No"), ordered=FALSE)
ds$D003 = factor(ds$D003, levels=c("1","2","3","4","5"), labels=c("Decreased a lot","Decreased a little","Stayed the same","Increased a little","Increased a lot"), ordered=FALSE)
ds$E004 = factor(ds$E004, levels=c("1","2","3"), labels=c("Better","The same","Worse"), ordered=TRUE)
ds$E005 = factor(ds$E005, levels=c("1","2","3"), labels=c("Better","The same","Worse"), ordered=TRUE)
ds$E006 = factor(ds$E006, levels=c("1","2","3","4","5"), labels=c("Less than 3 months","From 3 up to 6 months","From 6 up to 12 months","12 or more months","No savings"), ordered=FALSE)
ds$F004 = factor(ds$F004, levels=c("1","2","3"), labels=c("Primary","Secondary","Tertiary"), ordered=TRUE)
ds$F005 = factor(ds$F005, levels=c("1","2","3"), labels=c("East Austria","South Austria","West Austria"), ordered=FALSE)
ds$F006 = factor(ds$F006, levels=c("1","2","3"), labels=c("Brussels Capital Region","Flemish Region","Walloon Region"), ordered=FALSE)
ds$F007 = factor(ds$F007, levels=c("1","2"), labels=c("Northern and Eastern Bulgaria","South-Western and South-Central Bulgaria"), ordered=FALSE)
ds$F008 = factor(ds$F008, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), labels=c("Baden-Württemberg","Bavaria","Berlin","Brandenburg","Bremen","Hamburg","Hessen","Mecklenburg-Vorpommern","Lower Saxony","North Rhine-Westphalia","Rhineland-Palatinate","Saarland","Saxony","Saxony-Anhalt","Schleswig-Holstein","Thuringia"), ordered=FALSE)
ds$F009 = factor(ds$F009, levels=c("1","2","3","4"), labels=c("Attica","Nisia Aigaiou, Kriti","Voreia Ellada","Kentriki Ellada"), ordered=FALSE)
ds$F010 = factor(ds$F010, levels=c("1","2","3","4","5","6","7"), labels=c("North West","North East","Community of Madrid","Centre","East","South","Canary Islands"), ordered=FALSE)
ds$F011 = factor(ds$F011, levels=c("1","2"), labels=c("Mainland Finland","Åland"), ordered=FALSE)
ds$F012 = factor(ds$F012, levels=c("1","2","3","4","5","6","7","8","9"), labels=c("Région parisienne","Bassin parisien","Nord","Est","Ouest","Sud-Ouest","Centre-Est (Auvergne-Rhône-Alpes)","Méditerranée","Départements d\'Outre-Mer"), ordered=FALSE)
ds$F013 = factor(ds$F013, levels=c("1","2","3"), labels=c("Central Hungary","Transdanubia","Great Plain and North"), ordered=FALSE)
ds$F014 = factor(ds$F014, levels=c("1","2","3","4","5"), labels=c("North West","North East","Centre","South","Islands"), ordered=FALSE)
ds$F015 = factor(ds$F015, levels=c("1","2","3","4"), labels=c("North Netherlands","East Netherlands","West Netherlands","South Netherlands"), ordered=FALSE)
ds$F016 = factor(ds$F016, levels=c("1","2","3","4","5","6"), labels=c("Central Region","South Region","East Region","Northwest Region","Southwest Region","North Region"), ordered=FALSE)
ds$F017 = factor(ds$F017, levels=c("1","2","3"), labels=c("Mainland Portugal","Azores","Madeira"), ordered=FALSE)
ds$F018 = factor(ds$F018, levels=c("1","2","3","4"), labels=c("Nord-Vest, Centru","Nord-Est, Sud-Est","Sud – Muntenia, Bucuresti – Ilfov","Sud-Vest Oltenia, Vest"), ordered=FALSE)
ds$F019 = factor(ds$F019, levels=c("1","2","3"), labels=c("East Sweden","South Sweden","North Sweden"), ordered=FALSE)
ds$F020 = factor(ds$F020, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"), labels=c("North East","North West","Yorkshire and the Humber","East Midlands","West Midlands","East of England","Greater London","South East","South West","Wales","Scotland","Northern Ireland"), ordered=FALSE)

attr(ds$C001_01,"1") = "1 Very dissatisfied"
attr(ds$C001_01,"2") = "2"
attr(ds$C001_01,"3") = "3"
attr(ds$C001_01,"4") = "4"
attr(ds$C001_01,"5") = "5"
attr(ds$C001_01,"6") = "6"
attr(ds$C001_01,"7") = "7"
attr(ds$C001_01,"8") = "8"
attr(ds$C001_01,"9") = "9"
attr(ds$C001_01,"10") = "10 Very satisfied"
attr(ds$C001_01,"-1") = "Don\'t know/ Prefer not to answer"

attr(ds$C002_01,"1") = "1 Very unhappy"
attr(ds$C002_01,"2") = "2"
attr(ds$C002_01,"3") = "3"
attr(ds$C002_01,"4") = "4"
attr(ds$C002_01,"5") = "5"
attr(ds$C002_01,"6") = "6"
attr(ds$C002_01,"7") = "7"
attr(ds$C002_01,"8") = "8"
attr(ds$C002_01,"9") = "9"
attr(ds$C002_01,"10") = "10 Very happy"
attr(ds$C002_01,"-1") = "Don\'t know/ Prefer not to answer"

for (var in c("C003_01", "C003_02", "C003_03", "C003_04")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5), 
                      labels=c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
                      ordered=TRUE)
  
}

ds$C004_01 <- factor(ds$C004_01, levels=c(1,2,3,4,5),
                     labels=c("Very good","Good","Fair","Bad","Very bad"))

for (var in c("C005_01","C005_02","C005_03","C005_04","C005_05")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5,6), 
                      labels=c("At no time", "Some of the time",  "Less than half of the time","More than half of the time", "Most of the time","All of the time"),
                      ordered=TRUE)
  
}

for (var in c("C006_01","C006_02","C006_03")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5,6), 
                      labels=c("All of the time","Most of the time","More than half of the time","Less than half of the time","Some of the time", "At no time"),
                      ordered=TRUE)
  
}

attr(ds$C007_01,"1") = "1 Do not trust at all"
attr(ds$C007_01,"2") = "2"
attr(ds$C007_01,"3") = "3"
attr(ds$C007_01,"4") = "4"
attr(ds$C007_01,"5") = "5"
attr(ds$C007_01,"6") = "6"
attr(ds$C007_01,"7") = "7"
attr(ds$C007_01,"8") = "8"
attr(ds$C007_01,"9") = "9"
attr(ds$C007_01,"10") = "10 Trust completely"
attr(ds$C007_01,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_02,"1") = "1 Do not trust at all"
attr(ds$C007_02,"2") = "2"
attr(ds$C007_02,"3") = "3"
attr(ds$C007_02,"4") = "4"
attr(ds$C007_02,"5") = "5"
attr(ds$C007_02,"6") = "6"
attr(ds$C007_02,"7") = "7"
attr(ds$C007_02,"8") = "8"
attr(ds$C007_02,"9") = "9"
attr(ds$C007_02,"10") = "10 Trust completely"
attr(ds$C007_02,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_03,"1") = "1 Do not trust at all"
attr(ds$C007_03,"2") = "2"
attr(ds$C007_03,"3") = "3"
attr(ds$C007_03,"4") = "4"
attr(ds$C007_03,"5") = "5"
attr(ds$C007_03,"6") = "6"
attr(ds$C007_03,"7") = "7"
attr(ds$C007_03,"8") = "8"
attr(ds$C007_03,"9") = "9"
attr(ds$C007_03,"10") = "10 Trust completely"
attr(ds$C007_03,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_04,"1") = "1 Do not trust at all"
attr(ds$C007_04,"2") = "2"
attr(ds$C007_04,"3") = "3"
attr(ds$C007_04,"4") = "4"
attr(ds$C007_04,"5") = "5"
attr(ds$C007_04,"6") = "6"
attr(ds$C007_04,"7") = "7"
attr(ds$C007_04,"8") = "8"
attr(ds$C007_04,"9") = "9"
attr(ds$C007_04,"10") = "10 Trust completely"
attr(ds$C007_04,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_05,"1") = "1 Do not trust at all"
attr(ds$C007_05,"2") = "2"
attr(ds$C007_05,"3") = "3"
attr(ds$C007_05,"4") = "4"
attr(ds$C007_05,"5") = "5"
attr(ds$C007_05,"6") = "6"
attr(ds$C007_05,"7") = "7"
attr(ds$C007_05,"8") = "8"
attr(ds$C007_05,"9") = "9"
attr(ds$C007_05,"10") = "10 Trust completely"
attr(ds$C007_05,"-1") = "Don\'t know/ Prefer not to answer"

for (var in c("D004_01","D004_02","D004_03","D004_04","D004_05")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5), 
                      labels=c("Always","Most of the time","Sometimes","Rarely","Never"),
                      ordered=TRUE)
  
}

ds$D005_01 <- factor(ds$D005_01, levels=c(1,2,3,4,5),
                     labels=c("Every day","Every other day","Once or twice a week","Less often","Never"),
                     ordered=TRUE)

ds$D006_01 <- factor(ds$D006_01, levels=c(1,2,3,4,5),
                     labels=c("Daily","Several times a week","Several times a month","Less often","Never"),
                     ordered=TRUE)

ds$D007_01 <- factor(ds$C007_01, levels=c(1,2), labels=c("No","Yes"))

for (var in c("D008_01","E007_01")) {
  
  ds[[var]] <- factor(ds[[var]], levels=c(1,2,3,4,5), 
                      labels=c("Very likely","Rather likely","Neither likely nor unlikely","Rather unlikely","Very unlikely"),
                      ordered=TRUE)
  
}

ds$E001_01 <- factor(ds$E001_01, levels=c(1,2,3,4,5,6), 
                     labels=c("With great difficulty","With difficulty","With some difficulty","Fairly easily","Easily","Very easily"),
                     ordered=TRUE)

for (var in c("E002_01","E002_02","E003_01","E003_02","E003_03","E003_04","E003_05","E003_06","F002_01","F022_01")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2), 
                      labels=c("Yes","No"))
  
}

for (var in c("E008_01","E008_02","E008_03","E008_04","E008_05","E008_06")) {
  
  ds[[var]] <- factor(ds[[var]], levels=c(1,2,3,4), 
                      labels=c("A member of your family/relative",
                               "A friend, neighbour, or someone else",
                               "A service provider, institution or organisation",
                               "Nobody"))
}


attr(ds$F001_01a,"F") = "Not checked"
attr(ds$F001_01a,"T") = "Checked"
attr(ds$F003_01a,"F") = "Not checked"
attr(ds$F003_01a,"T") = "Checked"
attr(ds$F003_02a,"F") = "Not checked"
attr(ds$F003_02a,"T") = "Checked"

attr(ds$FINISHED,"F") = "Canceled"
attr(ds$FINISHED,"T") = "Finished"
attr(ds$Q_VIEWER,"F") = "Respondent"
attr(ds$Q_VIEWER,"T") = "Spectator"

comment(ds$CASE) = "Case number"
comment(ds$SERIAL) = "Serial number (if provided)"
comment(ds$REF) = "Reference (if provided in link)"
comment(ds$QUESTNNR) = "Questionnaire that has been used in the interview"
comment(ds$MODE) = "Interview mode"
comment(ds$LANGUAGE) = "Language"
comment(ds$STARTED) = "Time the interview has started (Europe/Berlin)"
comment(ds$B001) = "Country"
comment(ds$B002) = "Gender"
comment(ds$B003_01) = "Age"
comment(ds$C001_01) = "Life satisfaction"
comment(ds$C002_01) = "Happiness"
comment(ds$C003_01) = "Optimism and resilience: I am optimistic about my future"
comment(ds$C003_02) = "Optimism and resilience: I am optimistic about my children's or grandchildren's future"
comment(ds$C003_03) = "Optimism and resilience: I find it difficult to deal with important problems that come up in my life"
comment(ds$C003_04) = "Optimism and resilience: When things go wrong in my life, it generally takes me a long time to get back to normal"
comment(ds$C004_01) = "Health"
comment(ds$C005_01) = "WHO-5: I have felt cheerful and in good spirits"
comment(ds$C005_02) = "WHO-5: I have felt calm and relaxed"
comment(ds$C005_03) = "WHO-5: I have felt active and vigorous"
comment(ds$C005_04) = "WHO-5: I woke up feeling fresh and rested"
comment(ds$C005_05) = "WHO-5: My daily life has been filled with things that interest me"
comment(ds$C006_01) = "Negative affect: I have felt particularly tense"
comment(ds$C006_02) = "Negative affect: I have felt lonely"
comment(ds$C006_03) = "Negative affect: I have felt downhearted and depressed"
comment(ds$C007_01) = "Trust: The news media"
comment(ds$C007_02) = "Trust: The police"
comment(ds$C007_03) = "Trust: Your country’s government"
comment(ds$C007_04) = "Trust: The European Union"
comment(ds$C007_05) = "Trust: The healthcare system"
comment(ds$C008) = "Urbanisation"
comment(ds$D001) = "Employment status"
comment(ds$D002) = "During the Covid-19 pandemic have you lost your job(s) or contract(s)?"
comment(ds$D003) = "During the Covid-19 pandemic have your working hours...?"
comment(ds$D004_01) = "Work-life balance: Kept worrying about work when you were not working"
comment(ds$D004_02) = "Work-life balance: Felt too tired after work to do some of the household jobs which need to be done"
comment(ds$D004_03) = "Work-life balance: Found that your job prevented you from giving the time you wanted to your family"
comment(ds$D004_04) = "Work-life balance: Found it difficult to concentrate on your job because of your family responsibilities"
comment(ds$D004_05) = "Work-life balance: Found that your family responsibilities prevented you from giving the time you should to your job"
comment(ds$D005_01) = "Over the last 2 weeks, how often have you worked in your free time to meet work demands?"
comment(ds$D006_01) = "How frequently did you work from home before the outbreak of Covid-19?"
comment(ds$D007_01) = "Have you started to work from home as a result of the COVID-19 situation?"
comment(ds$D008_01) = "Do you think you might lose your job in the next 3 months?"
comment(ds$E001_01) = "Is your household able to make ends meet?"
comment(ds$E002_01) = "Economising: Gone without fresh fruit and vegetables"
comment(ds$E002_02) = "Economising: Bought cheaper cuts of meat or bought less than wanted"
comment(ds$E003_01) = "Arrears: Rent or mortgage payments for accommodation"
comment(ds$E003_02) = "Arrears: Utility bills, such as electricity, water, gas"
comment(ds$E003_03) = "Arrears: Payments related to consumer loans, including credit card overdrafts (to buy electrical appliances, a car, furniture, etc.)"
comment(ds$E003_04) = "Arrears: Telephone, mobile or internet connection bills"
comment(ds$E003_05) = "Arrears: Payments related to informal loans from friends or relatives not living in your household"
comment(ds$E003_06) = "Arrears: Payments for healthcare or health insurance"
comment(ds$E004) = "Financial situation of household now compared to 3 months ago"
comment(ds$E005) = "Expected financial situation of household in 3 months"
comment(ds$E006) = "If your household would not receive any income, how long would your household be able to maintain the same standard of living using savings?"
comment(ds$E007_01) = "Do you think you will need to leave your accommodation within the next 6 months because you can no longer afford it?"
comment(ds$E008_01) = "Support: If you needed help around the house when ill"
comment(ds$E008_02) = "Support: If you needed advice about a serious personal or family matter"
comment(ds$E008_03) = "Support: If you needed help when looking for a job"
comment(ds$E008_04) = "Support: If you were feeling a bit depressed and wanting someone to talk to"
comment(ds$E008_05) = "Support: If you needed help in looking after your children"
comment(ds$E008_06) = "Support: If you needed help with shopping"
comment(ds$F001_01) = "Household size"
comment(ds$F001_01a) = "Household size: Don\'t know/Prefer not to answer"
comment(ds$F002_01) = "Partner"
comment(ds$F003_01) = "Number of children: Age 0-11"
comment(ds$F003_01a) = "Number of children: Age 0-11: Don\'t know/Prefer not to answer"
comment(ds$F003_02) = "Number of children: Age 12-17"
comment(ds$F003_02a) = "Number of children: Age 12-17: Don\'t know/Prefer not to answer"
comment(ds$F004) = "Education"
comment(ds$F005) = "NUTS 1 AT"
comment(ds$F006) = "NUTS 1 BE"
comment(ds$F007) = "NUTS 1 BG"
comment(ds$F008) = "NUTS 1 DE"
comment(ds$F009) = "NUTS 1 EL"
comment(ds$F010) = "NUTS 1 ES"
comment(ds$F011) = "NUTS 1 FI"
comment(ds$F012) = "NUTS 1 FR"
comment(ds$F013) = "NUTS 1 HU"
comment(ds$F014) = "NUTS 1 IT"
comment(ds$F015) = "NUTS 1 NL"
comment(ds$F016) = "NUTS 1 PL"
comment(ds$F017) = "NUTS 1 PT"
comment(ds$F018) = "NUTS 1 RO"
comment(ds$F019) = "NUTS 1 SE"
comment(ds$F020) = "NUTS 1 UK"
comment(ds$F021) = "Person ID (SERIAL)"
comment(ds$F022_01) = "Reporting: [No Description] 01"
comment(ds$TIME001) = "Time spent on page 1"
comment(ds$TIME002) = "Time spent on page 2"
comment(ds$TIME003) = "Time spent on page 3"
comment(ds$TIME004) = "Time spent on page 4"
comment(ds$TIME005) = "Time spent on page 5"
comment(ds$TIME006) = "Time spent on page 6"
comment(ds$TIME007) = "Time spent on page 7"
comment(ds$TIME008) = "Time spent on page 8"
comment(ds$TIME009) = "Time spent on page 9"
comment(ds$TIME010) = "Time spent on page 10"
comment(ds$TIME011) = "Time spent on page 11"
comment(ds$TIME012) = "Time spent on page 12"
comment(ds$TIME013) = "Time spent on page 13"
comment(ds$TIME014) = "Time spent on page 14"
comment(ds$TIME015) = "Time spent on page 15"
comment(ds$TIME016) = "Time spent on page 16"
comment(ds$TIME017) = "Time spent on page 17"
comment(ds$TIME018) = "Time spent on page 18"
comment(ds$TIME019) = "Time spent on page 19"
comment(ds$TIME020) = "Time spent on page 20"
comment(ds$TIME021) = "Time spent on page 21"
comment(ds$TIME022) = "Time spent on page 22"
comment(ds$TIME023) = "Time spent on page 23"
comment(ds$TIME024) = "Time spent on page 24"
comment(ds$TIME025) = "Time spent on page 25"
comment(ds$TIME026) = "Time spent on page 26"
comment(ds$TIME027) = "Time spent on page 27"
comment(ds$TIME028) = "Time spent on page 28"
comment(ds$TIME_SUM) = "Time spent overall (except outliers)"
comment(ds$MAILSENT) = "Time when the invitation mailing was sent (personally identifiable recipients, only)"
comment(ds$LASTDATA) = "Time when the data was most recently updated"
comment(ds$FINISHED) = "Has the interview been finished (reached last page)?"
comment(ds$Q_VIEWER) = "Did the respondent only view the questionnaire, omitting mandatory questions?"
comment(ds$LASTPAGE) = "Last page that the participant has handled in the questionnaire"
comment(ds$MAXPAGE) = "Hindmost page handled by the participant"

#Getting list of all numeric variables
nums <- unlist(lapply(ds, is.numeric))  
num_vars <- names(nums[nums==TRUE])

#Replacing all -1 and -9 values of numeric variables with NA
for (var in num_vars) {
  
  ds[var][ds[var]==-1 | ds[var]==-9] <- NA
  
}

ds$age_group[ds$B003_01<35] <- "Under 35"
ds$age_group[ds$B003_01>=35 & ds$B003_01<50] <- "35 - 49"
ds$age_group[ds$B003_01>50] <- "50 and over"
ds$age_group <- factor(ds$age_group, levels=c("Under 35","35 - 49","50 and over"), ordered=TRUE)

source("Cleaning_simple.R", local=TRUE)

ds <- ds %>%
  left_join(ds_clean[c("CASE","clean")], by="CASE")
ds$clean[is.na(ds$clean)] <- FALSE