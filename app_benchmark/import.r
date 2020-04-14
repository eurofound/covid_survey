# This script reads a CSV file in GNU R.
# While reading this file, comments will be created for all variables.
# The comments for values will be stored as attributes (attr) as well.
# This script has been generated by SocSci and modified by Eurofound. 

#setwd(paste0(getwd(),"/app_benchmark"))

# API link of the SocSci database of the Eurofound survey. API token is confidential and stored in secrets file.
source("secrets.R")
ds_file = paste0("https://s2survey.net/eurofound/?act=", token)

# Reading in the data. 
ds = read.table(
  file=ds_file, encoding="UTF-8",
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
    "F020","F021","F022_01","TIME001","TIME002","TIME003","TIME004","TIME005",
    "TIME006","TIME007","TIME008","TIME009","TIME010","TIME011","TIME012","TIME013",
    "TIME014","TIME015","TIME016","TIME017","TIME018","TIME019","TIME020","TIME021",
    "TIME022","TIME023","TIME024","TIME025","TIME026","TIME027","TIME028",
    "TIME_SUM","MAILSENT","LASTDATA","FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE"
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
    TIME001="integer", TIME002="integer", TIME003="integer", TIME004="integer",
    TIME005="integer", TIME006="integer", TIME007="integer", TIME008="integer",
    TIME009="integer", TIME010="integer", TIME011="integer", TIME012="integer",
    TIME013="integer", TIME014="integer", TIME015="integer", TIME016="integer",
    TIME017="integer", TIME018="integer", TIME019="integer", TIME020="integer",
    TIME021="integer", TIME022="integer", TIME023="integer", TIME024="integer",
    TIME025="integer", TIME026="integer", TIME027="integer", TIME028="integer",
    TIME_SUM="integer", MAILSENT="POSIXct", LASTDATA="POSIXct",
    FINISHED="logical", Q_VIEWER="logical", LASTPAGE="numeric",
    MAXPAGE="numeric"
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

# Variable und Value Labels
ds$B001 = factor(ds$B001, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60"), labels=c("Austria","Belgium","Bulgaria","Croatia","Republic of Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","Albania","Bosnia and Herzegovina","Brazil","Canada","China","Colombia","Ecuador","Egypt","India","Indonesia","Iran","Japan","Mexico","Montenegro","Morocco","Netherlands Antilles","Nigeria","North Macedonia","Pakistan","Philippines","Russia","Republic of Serbia","South Korea","Switzerland","Suriname","Syria","Thailand","Turkey","Ukraine","United Kingdom","United States","Vietnam","Other country"), ordered=FALSE)
ds$B002 = factor(ds$B002, levels=c("1","2","3"), labels=c("Male","Female","In another way"), ordered=FALSE)
ds$C008 = factor(ds$C008, levels=c("1","2","3","4"), labels=c("The open countryside","A village/small town","A medium to large town","A city or city suburb"), ordered=FALSE)
ds$D001 = factor(ds$D001, levels=c("1","2","3","4","5","6","7","8"), labels=c("Employee","Self-employed with employees","Self-employed without employees","Unemployed","Unable to work due to long-term illness or disability","Retired","Full-time homemaker/fulfilling domestic tasks","Student"), ordered=FALSE)
ds$D002 = factor(ds$D002, levels=c("1","2","3"), labels=c("Yes, permanently","Yes, temporarily","No"), ordered=FALSE)
ds$D003 = factor(ds$D003, levels=c("1","2","3","4","5"), labels=c("Decreased a lot","Decreased a little","Stayed the same","Increased a little","Increased a lot"), ordered=FALSE)
ds$E004 = factor(ds$E004, levels=c("1","2","3"), labels=c("Better","The same","Worse"), ordered=TRUE)
ds$E005 = factor(ds$E005, levels=c("1","2","3"), labels=c("Better","The same","Worse"), ordered=TRUE)
ds$E006 = factor(ds$E006, levels=c("1","2","3","4","5"), labels=c("Less than 3 months","From 3 up to 6 months","From 6 up to 12 months","12 or more months","No savings"), ordered=FALSE)
ds$F004 = factor(ds$F004, levels=c("1","2","3"), labels=c("Primary education","Secondary education","Tertiary education"), ordered=TRUE)
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
                               "A friend, neighbour, or someone else, who does not belong to your family/relatives",
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
comment(ds$D002) = "Lost job"
comment(ds$D003) = "Working hours"
comment(ds$D004_01) = "Work-life balance: Kept worrying about work when you were not working"
comment(ds$D004_02) = "Work-life balance: Felt too tired after work to do some of the household jobs which need to be done"
comment(ds$D004_03) = "Work-life balance: Found that your job prevented you from giving the time you wanted to your family"
comment(ds$D004_04) = "Work-life balance: Found it difficult to concentrate on your job because of your family responsibilities"
comment(ds$D004_05) = "Work-life balance: Found that your family responsibilities prevented you from giving the time you should to your job"
comment(ds$D005_01) = "Over the last 2 weeks, how often have you worked in your free time to meet work demands?"
comment(ds$D006_01) = "How frequently did you work from home before the outbreak of Covid-19?"
comment(ds$D007_01) = "Have you started to work from home as a result of the COVID-19 situation?"
comment(ds$D008_01) = "how likely or unlikely do you think it is that you might lose your job in the next 3 months?"
comment(ds$E001_01) = "Is your household able to make ends meet?"
comment(ds$E002_01) = "Economising: Gone without fresh fruit and vegetables"
comment(ds$E002_02) = "Economising: Bought cheaper cuts of meat or bought less than wanted"
comment(ds$E003_01) = "Arrears: Rent or mortgage payments for accommodation"
comment(ds$E003_02) = "Arrears: Utility bills, such as electricity, water, gas"
comment(ds$E003_03) = "Arrears: Payments related to consumer loans, including credit card overdrafts (to buy electrical appliances, a car, furniture, etc.)"
comment(ds$E003_04) = "Arrears: Telephone, mobile or internet connection bills"
comment(ds$E003_05) = "Arrears: Payments related to informal loans from friends or relatives not living in your household"
comment(ds$E003_06) = "Arrears: Payments for healthcare or health insurance"
comment(ds$E004) = "Financial situation of household 3 months ago"
comment(ds$E005) = "Financial situation of household in 3 months"
comment(ds$E006) = "If your household would not receive any income, how long would your household be able to maintain the same standard of living using savings?"
comment(ds$E007_01) = "How likely or unlikely do you think it is that you will need to leave your accommodation within the next 6 months because you can no longer afford it?"
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

make_varnames_list <- function(section) {

  varnames <- list()
  
  for (var in colnames(ds)) {
    
    if (sum(startsWith(var,c(section)))>0) {
      
      varnames[comment(ds[[var]])] <- var
      
    }
    
  }
  
  return(varnames)

}

varnames <- lapply(c("C0","D0","E0"),make_varnames_list)
names(varnames) <- c("Quality of life","Work and teleworking","Financial situation")

save(varnames, file="data/varnames.rda")

save(ds, file="data/ds.Rda")
