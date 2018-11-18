# Group Name:PRSV-Pune-2018
# Pradnya Paithankar
# Dr. Rajendra Warke
# Sameer Sinha
# Vaibhav Palkar

# Installing packages and loading libraries needed

install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
library(tidyr)
library(dplyr)
library(stringr)
# Setting work directory. commenting it later.
#setwd("F:\\PGDDS\\Investments group case study")

# Importing companies data from a text file, replacing blank values with NA

## Checkpoint 1

companies<-read.delim("companies.txt",header=TRUE, sep="\t", na.strings = "",stringsAsFactors = FALSE,strip.white=TRUE)
str(companies)

# Importing rounds data from a csv file,replacing blank values with NA
rounds2<-read.csv("rounds2.csv",header=TRUE, sep=",", na.strings="",fill = TRUE,stringsAsFactors = FALSE)
str(rounds2)

#Answers to Table1.1

#How many unique companies are present in rounds2?
# finding distinct count of companies using unique ID permalink
# rounds2 contains same company_permalink values in different cases so converting them all to lower case
rounds2$company_permalink<-tolower(rounds2$company_permalink)
unique_companies_rounds2 <-n_distinct(rounds2$company_permalink,na.rm=TRUE)
unique_companies_rounds2

#How many unique companies are present in companies?
#converting companies permalink too in lower case to match during merge
companies$permalink<-tolower(companies$permalink)
unique_companies_companies<-n_distinct(companies$permalink,na.rm=TRUE)
unique_companies_companies

#Are there any companies in the rounds2 file which are not present in companies? 
sum(!unique(rounds2$company_permalink) %in% companies$permalink)
# no extra company found in rounds2 that are not there in companies

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame
#merging two dataframes
master_frame<-merge(rounds2,companies,by.x="company_permalink", by.y = "permalink", all=TRUE)

#How many observations are present in master_frame?
master_frame_count<-nrow(master_frame)
master_frame_count
#114949 observations
#Exporting for plotting in tablue
write.csv(master_frame,"master_frame.csv")
## Checkpoint 2

#Calculate the average investment amount for each of the funding types 

summary_funding_type<-summarise(group_by(master_frame,funding_round_type),mean(raised_amount_usd,na.rm=TRUE))

# selecting records for the four preferred funding types (venture, angel, seed, and private equity)
summary_funding_type_preferred<- setNames(subset(summary_funding_type, funding_round_type=="angel"|funding_round_type=="venture"|funding_round_type =="seed" | funding_round_type=="private_equity"),c("preferred_funding_round","average_investment_amount"))
summary_funding_type_preferred[order(-summary_funding_type_preferred$average_investment_amount),]
#Exporting for plotting in tablue
write.csv(summary_funding_type_preferred,"summary_funding_type_preferred.csv")
## For Spark funds, desired average investment amount is between 5M to 15M USD
#finding the funding type that fits in this range
summary_funding_type_selected<-subset(summary_funding_type_preferred,average_investment_amount>=5000000 & average_investment_amount<=15000000 )

# "Venture is the selected preferred funding type

## Checkpoint 3
# Grouping and summarizing based on country for venture funding type
master_frame_venture<-master_frame[which(master_frame$funding_round_type=="venture" & !is.na(master_frame$country_code)),]
summary_country_code<-summarise(group_by(master_frame_venture,country_code),Total_funding_venture=sum(raised_amount_usd,na.rm=TRUE))

#selecting top 9 countries based on total funding in venture category
top9<-slice((summary_country_code[sort.list(summary_country_code$Total_funding_venture, na.last= NA,decreasing = TRUE),]),1:9)
#Exporting for plotting in tablue
write.csv(top9,"top9.csv")
# Top 4 countries are US, Chn, GBR and India. China does not have english as official language as per provided pdf
# So top 3 preferred countries are USA, GBR and IND

#Creating new dataframe for top 3 ocuntries with funding type venture
master_frame_venture_country<-subset(master_frame_venture,country_code %in% c("USA","GBR","IND"))
nrow(master_frame_venture_country)

## Checkpoint 4
## Extracting the primary sector of each category list from the category_list column
master_frame_venture_country<-separate(master_frame_venture_country,category_list, c("primary_sector"),sep = "[|]",remove=FALSE)

 #Importing mapping data
mapping<-read.csv("mapping.csv",header=TRUE, na.strings="",stringsAsFactors = FALSE,check.names = FALSE)

# Some category_list values contain 0 in string in place of characters na
# Replacing all 0 with na string
mapping$category_list<-gsub("0","na", mapping$category_list)
# One exception to this was value Enterprise 2.0 It also got changed. So reverting it back
mapping$category_list<-gsub("\\.na",".0", mapping$category_list)

#Converting mapping dataframe from wide to long 
#Creating column main_sector to hold main_sector values
mapping_long<-gather(mapping,main_sector,sector_value,2:10)
#Removing sector values that are na and removing entries with zero values. 
mapping_long <- mapping_long[which(!(mapping_long$sector_value == 0) & !is.na(mapping_long$category_list)),]


#Changing case of primary sector / category list to lower in both the data frames
master_frame_venture_country$primary_sector<-tolower(master_frame_venture_country$primary_sector)
mapping_long$category_list<-tolower(mapping_long$category_list)
#Removing extra column of sector_value from mapping_long
mapping_long<-mapping_long[,-3]

#Merging master_frame with mapping data to add main sector to master_frame
master_frame_venture_country<-merge(master_frame_venture_country,mapping_long,by.x="primary_sector",by.y="category_list",all.x=TRUE,sort=FALSE)
#Checking if there are na values for main sectors 
summary(is.na(master_frame_venture_country$main_sector))
# 395 na found
# Omitting rows corresponding na values of main_sector from master frame
master_frame_venture_country<-master_frame_venture_country[which(!is.na(master_frame_venture_country$main_sector)),]
nrow(master_frame_venture_country)

  #Checkpoint 5
# Subsettting data frame for funding within 5 to 15 million USD
master_frame_venture_country_flimit<-subset(master_frame_venture_country,raised_amount_usd>=5000000 & raised_amount_usd<=15000000)

#Grouping by country code and main sector, finding the count of investment and sum of investment for each group
count_total_investment<-summarise(group_by(master_frame_venture_country_flimit,country_code,main_sector),n(),sum(raised_amount_usd))
#Naming columns with proper headings
colnames(count_total_investment)[3:4]<-c("sector_investment_count","sector_investment_total")



#Merging count_total_investment with master frame with two columns country_code and main_sector
master_frame_venture_country_flimit<-merge(master_frame_venture_country_flimit,count_total_investment,by=c("country_code","main_sector"))

write.csv(master_frame_venture_country_flimit,"master_frame_venture_country_flimit.csv")
#Finally splitting the dataframe based on country
D1<-subset(master_frame_venture_country_flimit,country_code=="USA")
D2<-subset(master_frame_venture_country_flimit,country_code=="GBR")
D3<-subset(master_frame_venture_country_flimit,country_code=="IND")

#Counting countrywise investment count and total
count_total_investment_country<-summarise(group_by(master_frame_venture_country_flimit,country_code),n(),sum(raised_amount_usd))

#Finding top3 sectors by count for each country
Summary_USA<-subset(count_total_investment,country_code=="USA")
top3_bycount_USA<-head(Summary_USA[order(-Summary_USA$sector_investment_count),],3)


Summary_GBR<-subset(count_total_investment,country_code=="GBR")
top3_bycount_GBR<-head(Summary_GBR[order(-Summary_GBR$sector_investment_count),],3)

Summary_IND<-subset(count_total_investment,country_code=="IND")
top3_bycount_IND<-head(Summary_IND[order(-Summary_IND$sector_investment_count),],3)

# Finding company that received highest investment in top sector
D1_max_inv_company <-subset(D1,main_sector %in% c("Others","Social, Finance, Analytics, Advertising"))
D1_max_inv_company <-group_by(D1_max_inv_company,company_permalink,name,main_sector)
D1_max_inv_company<-summarise(D1_max_inv_company,sum(raised_amount_usd))
colnames(D1_max_inv_company)[4]<-"sum_of_amout"
D1_max_inv_company<-D1_max_inv_company[order(-D1_max_inv_company$sum_of_amout),]

#
D2_max_inv_company  <-subset(D2,main_sector %in% c("Others","Social, Finance, Analytics, Advertising"))
D2_max_inv_company <-group_by(D2_max_inv_company,company_permalink,name,main_sector)
D2_max_inv_company<-summarise(D2_max_inv_company,sum(raised_amount_usd))
colnames(D2_max_inv_company)[4]<-"sum_of_amout"
D2_max_inv_company<-D2_max_inv_company[order(-D2_max_inv_company$sum_of_amout),]

# 
D3_max_inv_company <-subset(D3,main_sector %in% c("Others","Social, Finance, Analytics, Advertising"))
D3_max_inv_company <-group_by(D3_max_inv_company,company_permalink,name,main_sector)
D3_max_inv_company<-summarise(D3_max_inv_company,sum(raised_amount_usd))
colnames(D3_max_inv_company)[4]<-"sum_of_amout"
D3_max_inv_company<-D3_max_inv_company[order(-D3_max_inv_company$sum_of_amout),]

