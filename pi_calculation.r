#set the working directory and load the required packages into the script
setwd("C:/Users/Gourab.Chanda/Desktop/Analytics_Support")
pckgs_req <- c("xlsx","dplyr")
lapply(pckgs_req,library,character.only=T)
#read the input file into the data frame and convert it into tibble
input_file <- read.xlsx(file.choose(),sheetIndex = 1,header = T)
idn_rsp.data <- tibble::as_tibble(input_file)
#copy the data from rsp.data to pricing.data variable
idn_pricing.data <- idn_rsp.data
#format the start date and the end date as per the default format
idn_pricing.data$start_date <- as.Date(idn_pricing.data$start_date,"%m%d%Y")
idn_pricing.data$end_date <- as.Date(idn_pricing.data$end_date,"%m%d%Y")
#do the data manipulation steps and rsp is specific for the specific product set attributes so we have to create the previous rsp based on that

idn_pricing.data <- idn_pricing.data%>%
  arrange(channel,subchannel,region,barcode,sku,start_date)%>%
  group_by(channel,subchannel,region,barcode,sku)%>%
  mutate(prev.rsp=lag(rsp))

#calculate the price Index pi for a given observation using the rsp and the previous rsp
idn_pricing.data$pi <- ((idn_pricing.data$rsp-idn_pricing.data$prev.rsp)/idn_pricing.data$prev.rsp)
#deselect the columns {rsp,prev.rsp,end_date}
idn_pricing.data <- idn_pricing.data%>%
  select(-rsp,-prev.rsp,-end_date)
#extract the subset where pi is not equal to zero
idn_pricing.data <- idn_pricing.data%>%
  filter(pi!=0)
#change the column name for the column named start date and pi into price change
idn_pricing.data <- idn_pricing.data%>%
  rename(price_change=pi,date=start_date)
idn_pricing.data$manufacturer <-"Unilever"
#convert the NA to blank as per the business requirement
#idn_pricing.data <-sapply(idn_pricing.data,as.character)
#idn_pricing.data[is.na(idn_pricing.data)] <- ""
#store the category value in a category variable and system date into a date variable to write into the CSV file
category <- as.vector(unique(idn_rsp.data$category,na.rm=T))
category <- category[!is.na(category)]
date <- Sys.Date()
date <- gsub("-","",date)
write.csv(idn_pricing.data,paste0("pi_",category,"_",date,".csv"),row.names = F,na="")

