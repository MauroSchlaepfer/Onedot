setwd("C:/Users/mschl/OneDrive/Documents/Dokumente/Data Task") # Set working directory to your files!
options(warn=-1)

### Load Packages and Data
packages <- c("rjson","readxl","stringr","rvest","tibble","openxlsx")
lapply(packages, require, character.only = TRUE)

supplierDatNormal<-supplierDat <- as.tibble(fromJSON(file="supplier_car.json",simplify = T))
Target_Data <- read_excel("Target Data.xlsx")

### Recode (Pre-processing)
## Only one data point in supplier data, leave it unchanged

### Normalization
supplierDatNormal$MakeText<-str_to_title(tolower(supplierDat$MakeText))
supplierDatNormal$TypeName<-str_to_title(tolower(supplierDat$TypeName))
supplierDatNormal$TypeNameFull<-str_to_title(tolower(supplierDat$TypeNameFull))
# normalization required for target data (zip)

### Integration
n<-length(supplierDatNormal$ID)
supplierDatTarget<-names(Target_Data) %>% rlang::rep_named(list(as.character())) %>% as_tibble() # Create empty target dataframe
supplierDatTarget[1:n,"carType"] <- ifelse(supplierDatNormal$`Attribute Names`=="Seats"&supplierDatNormal$`Attribute Values`==2,"Coupé",NA) # Can be extended if wished
supplierDatTarget[1:n,"type"] <- rep("car",n) # seems like only cars allowed in target schema, hence no need to test if supplier information is a car
supplierDatTarget$model <- supplierDatNormal$ModelTypeText 
supplierDatTarget$make <- supplierDatNormal$MakeText 

### Get additional information from web (since not a lot of information are given in supplier data)
## Wikipedia
ExtractInformationFromWiki <- function(Car){
  Make<-gsub(" ", "_", Car$MakeText)
  Model<-gsub(" ", "_", Car$ModelTypeText)
  
  url = paste0("https://en.wikipedia.org/wiki/",Make,"_",Model,"#") # can also be done through 'FromJSON' - depends on input data
temp <- url %>% 
  read_html %>%
  html_nodes("table") # get the legend table, easier search

Wiki <- html_table(temp[1])[[1]]

list(Engine=Wiki[grep("Engine",Wiki[,1]),2]) # can be extended
}
supplierDatTarget$model_variant<-ExtractInformationFromWiki(supplierDatNormal)$Engine # multiple entries in supplier would require apply function

## classic.com (some running time - not used for excel sheet)

GetPrice4Year<-function(Year=2010){ # Price from a semi-structered page
i=1
Year<-cbind(Year,rep(NA,length(Year)))

for(i in 1:length(Year[,1])){
  urls<-paste0("https://www.classic.com/m/mercedes-benz/slr-mclaren/year-",Year[i,1],"/")
  # direct link, classic.com unknown webstructure and not sure how to (legally) workaround google search block 
  
  temp <- urls %>% 
  read_html %>%
  html_nodes("table")
  price<-html_table(temp,fill = T)[[1]]$Price

if(is.null(price)){
  Year[i,2] <- "Not Sold"
  next
  } else{
    priceRangeNumeric <- suppressWarnings(range(na.omit(as.numeric(sapply(price,parse_number)))))
  }
  if(priceRangeNumeric[1]=="Inf"){
    Year[i,2] <- "Not Sold"
    next
  }
Year[i,2] <- paste(priceRangeNumeric[1], "$ -",priceRangeNumeric[2], "$")
}
Year
}
GetPrice4Year(2005:2010)

### Write into an excel sheet
list_of_datasets <- list("pre-processing" = supplierDat, "normalisation" = supplierDatNormal,"integration"=supplierDatTarget)
write.xlsx(list_of_datasets, file = "Onedot_datatask.xlsx")
# not entirely sure what belongs to the pre-processing step

options(warn=0)

