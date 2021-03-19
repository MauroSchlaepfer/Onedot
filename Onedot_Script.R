setwd("C:/Users/mschl/OneDrive/Documents/Dokumente/Data Task") # Set working directory to your files!
options(warn=-1)

##### Load Packages and Data #####
packages <- c("rjson","readxl","stringr","rvest","tibble","openxlsx","translateR")
lapply(packages, require, character.only = TRUE)
Target_Data <- read_excel("Target Data.xlsx")
url="supplier_car2.json"
JSON_into_Tibble <- function(url){
  
con <- file(url, open="r") # connenctinon to file

jsonlist <- list() # load as lists
while (length(line <- readLines(con, n=1, warn = FALSE)) > 0){
  jsonlist <- append(jsonlist, list(jsonlite::fromJSON(line)))
}
close(con) # close connection

## Transform from JSON to tibble
# load as lists
lists <- do.call(rbind, lapply(jsonlist, rbind))
rm(jsonlist)

# into regular dataframe
dataf<-do.call(rbind.data.frame, Map('c', lists[,1],lists[,2],lists[,3],lists[,4],lists[,5],lists[,6],lists[,7],lists[,8],lists[,9] ))
colnames(dataf)<- colnames(lists)

dataf<-sapply(dataf, as.character)


# into tibble
TibbleDat<-as.tibble(dataf)
TibbleDat
}
supplier<-JSON_into_Tibble("supplier_car2.json")

##### Recode (Pre-processing) #####
## Only one data point in supplier data, leave it unchanged
UTF_Fix <- function(x) { # from http://www.i18nqa.com/debug/utf8-debug.html
  x <- gsub("Ã¼","ü",x)
  x <- gsub("ÃY","ß",x)                            
  x <- gsub("Ã¶r","ö",x)
  x <- gsub("Ã¤","ä",x)
  x <- gsub("Ã©","é",x)
  
  x
} # Fix UTF 

Var_With_UTF<-(sapply(supplier,function(x)sum(Encoding(x)=="UTF-8"))>0) # Mark variables with UTF-8 coding
supplier[Var_With_UTF]<-sapply(supplier[Var_With_UTF],UTF_Fix) # Fix these variables

TestFunction<-function(x) x[which(Encoding(x)=="UTF-8")] 
print(sapply(supplier,TestFunction),max=2000) # check if everything worked

Var_With_ASCII_Error<-(sapply(supplier,function(x)sum(grepl("\"",x)))>0) # Mark ASCII variables with \" 
supplier[Var_With_ASCII_Error]<-sapply(supplier[Var_With_ASCII_Error],function(x)gsub("\"","",x)) # Fix these variables



## fixed messed up row entries
MessedUpOnes<-which((supplier$entity_id==supplier$ID)) # find those with entity = id which have the attributes one to the left in modeltypetext
supplier$`Attribute Values`[MessedUpOnes]  <- supplier$`Attribute Names`[MessedUpOnes]
supplier$`Attribute Names`[MessedUpOnes]  <- supplier$ModelTypeText[MessedUpOnes]
supplier$ModelTypeText[MessedUpOnes]  <- supplier$ModelText[MessedUpOnes]

## to wide format
supplierDat<-tidyr::pivot_wider(supplier[,1:8], names_from = "Attribute Names", values_from = "Attribute Values")

##### Normalization #####
supplierDatNormal<-supplierDat
Normalization <- function(x){
  x<-str_to_title(tolower(x))
  x<-gsub("Bmw","BMW",x)   # fix specific values
  x<-gsub("Vw","VW",x)   # fix specific values
  x<-gsub("Usa","USA",x)   # fix specific values
  x<-gsub("Nsu","NSU",x)   # fix specific values
  
  x
}

supplierDatNormal$MakeText<-Normalization(supplierDatNormal$MakeText)
supplierDatNormal$TypeName<-Normalization(supplierDatNormal$TypeName)
supplierDatNormal$TypeNameFull<-Normalization(supplierDatNormal$TypeNameFull)
supplierDatNormal$ModelText<-Normalization(supplierDatNormal$ModelText)


### Color
supplierDatNormal$BodyColorText<-sub("(\\w+).*", "\\1", supplierDatNormal$BodyColorText) # take only first word (drop "mét")
Farben<-names(table(supplierDatNormal$BodyColorText)) # write out colors

# Get translation schema
Colors<- str_to_title(translate(content.vec = Farben, # Google Translator
                                google.api.key = "AIzaSyDCS83kSmqBNzVLLBmbgVl2GU6Yqg6SY9U",
                                source.lang = 'de',
                                target.lang = 'en'))
# Search for colors
Position<-grep(paste(Farben,collapse="|"),supplierDatNormal$BodyColorText, value=F)

# Translate to English
supplierDatNormal$BodyColorText[Position]  <- Colors[match(supplierDatNormal$BodyColorText[Position] ,Farben)]


## condition / ConditionType
supplierDatNormal$ConditionTypeText<-gsub("Neu","New",supplierDatNormal$ConditionTypeText)   
supplierDatNormal$ConditionTypeText<-gsub("Occasion","Used",supplierDatNormal$ConditionTypeText)   
supplierDatNormal$ConditionTypeText<-gsub("Vorführmodell","Original Condition",supplierDatNormal$ConditionTypeText)   
supplierDatNormal$ConditionTypeText<-gsub("Oldtimer ","Used with guarantee",supplierDatNormal$ConditionTypeText)   

## cartype / BodyType 
supplierDatNormal$BodyTypeText[which(na.omit(supplierDatNormal$Seats=="1"))] <- "Single seater"
supplierDatNormal$BodyTypeText<-gsub("Cabriolet","Convertible",supplierDatNormal$BodyTypeText) 
supplierDatNormal$BodyTypeText<-gsub("SUV / Geländewagen","SUV",supplierDatNormal$BodyTypeText)   
supplierDatNormal$BodyTypeText<-gsub("Kleinwagen","SUV",supplierDatNormal$BodyTypeText)   
supplierDatNormal$BodyTypeText<-gsub("Limousine","Saloon",supplierDatNormal$BodyTypeText)   
supplierDatNormal$BodyTypeText<-gsub("Kombi","Station Wagon",supplierDatNormal$BodyTypeText)   
supplierDatNormal$BodyTypeText<-gsub("Sattelschlepper","Other",supplierDatNormal$BodyTypeText)   
supplierDatNormal$BodyTypeText<-gsub("Kompaktvan / Minivan","Other",supplierDatNormal$BodyTypeText)   
# etc...




##### Integration #####
n<-length(supplierDatNormal$ID)
supplierDatTarget<-names(Target_Data) %>% rlang::rep_named(list(as.character())) %>% as_tibble() # Create empty target dataframe

supplierDatTarget[1:n,"type"] <- rep("car",n) # seems like only cars allowed in target schema, hence no need to test if supplier information is a car
(names(supplierDatTarget))
# here search algorithm leads to naming issues (ex. multiple color), hence manuell since its simpler
supplierDatTarget$color<-supplierDatNormal$BodyColorText 
supplierDatTarget$city<-supplierDatNormal$City
supplierDatTarget$model <- supplierDatNormal$ModelText 
supplierDatTarget$model_variant <- supplierDatNormal$ModelTypeText
supplierDatTarget$make <- supplierDatNormal$MakeText 
supplierDatTarget$manufacture_month <- supplierDatNormal$FirstRegMonth 
supplierDatTarget$manufacture_year <- supplierDatNormal$FirstRegYear
supplierDatTarget$condition<-supplierDatNormal$ConditionTypeText
supplierDatTarget$mileage<-supplierDatNormal$Km
supplierDatTarget$mileage_unit<-ifelse(!is.na(supplierDatNormal$Km),"Km",NA)
supplierDatTarget$carType<-supplierDatNormal$BodyTypeText

# lock for country in target schema
Cit_Cou<-sapply(names(table(supplierDatTarget$city)),function(x) Target_Data$country[which(Target_Data$city==x)[1]]) # could also be called from web
supplierDatTarget$country<-as.character(Cit_Cou)[match(supplierDatTarget$city,names(Cit_Cou))]


## Currency and drive could be matched to city/country 

### Get information from web for empty entries
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
ExtractInformationFromWiki(supplierDatNormal[1,])$Engine # multiple entries in supplier would require apply function

## Same can be done for cities to get the country 

## classic.com (some running time - not used for excel sheet)
require(readr)

GetPrice4Year<-function(Year=2010){ # Price from a semi-structered page

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
GetPrice4Year(2005:2007) 

### Write into an excel sheet
list_of_datasets <- list("pre-processing" = supplierDat, "normalisation" = supplierDatNormal,"integration"=supplierDatTarget)
write.xlsx(list_of_datasets, file = "Onedot_datatask.xlsx")
# not entirely sure what belongs to the pre-processing step

options(warn=0)

