refine <- read.csv("~/Section-3-Data-Wrangling/refine_original.csv")

#1
#Separating the 4 different companies
phillips_hits<-grep("ps",refine$company)
akzo_hits<-grep("ak",refine$company)
van_houten_hits<-grep("van",refine$company)
unilever_hits<-grep("uni",refine$company)

#Standardizing the separate company names 
refine$company[phillips_hits] <- "philips"
refine$company[akzo_hits] <- "akzo"
refine$company[van_houten_hits] <- "van houten"
refine$company[unilever_hits] <- "unilever"

#2
#use tidyr function separate() to split the column product.code...number into columns:
#product_code and product_number
library(tidyr)
refine <- separate(refine, Product.code...number, c("product_code","product_number"))

#3
#create new column to represent the product_category
refine$product_category <- NA
#set product category to represent each product code
#p = Smartphone
#v = TV
#x = Laptop
#q = Tablet
for (i in 1:length(refine$product_code))
{
  if (refine$product_code[i] == "p")
  {
    refine$product_category[i] <- "Smartphone"
  }
  
  else if(refine$product_code[i] == "v")
  {
    refine$product_category[i] <- "TV"
  }
  else if(refine$product_code[i] == "x")
  {
    refine$product_category[i] <- "Laptop"
  }
  else if (refine$product_code[i] == "q")
  {
    refine$product_category[i] <- "Tablet"
  }
}

#4
#create new column for full address
refine$full_address <- NA

#Fill full address column with address,city,country
for(i in 1:length(refine$address))
{
  refine$full_address[i] <- paste(refine$address[i],refine$city[i],refine$country[i], sep = ",")
}

#create binary columns for companies
refine$company_philips <- NA
refine$company_akzo <- NA
refine$company_van_houten  <- NA
refine$company_unilever <- NA

for (i in 1:length(refine$company))
{
  if(refine$company[i] == "philips")
  {
    refine$company_philips[i] <- 1
    refine$company_akzo[i] <- 0
    refine$company_van_houten[i] <- 0
    refine$company_unilever[i] <- 0
  }
  
  else if (refine$company[i] == "akzo")
  {
    refine$company_philips[i] <- 0
    refine$company_akzo[i] <- 1
    refine$company_van_houten[i] <- 0
    refine$company_unilever[i] <- 0
  }
  
  else if (refine$company[i] == "van houten")
  {
    refine$company_philips[i] <- 0
    refine$company_akzo[i] <- 0
    refine$company_van_houten[i] <- 1
    refine$company_unilever[i] <- 0
  }
  
  else if (refine$company[i] == "unilever")
  {
    refine$company_philips[i] <- 0
    refine$company_akzo[i] <- 0
    refine$company_van_houten[i] <- 0
    refine$company_unilever[i] <- 1
  }
}

#create binary columns for product_category
refine$product_smartphone <- NA
refine$product_tv <- NA
refine$product_laptop <- NA
refine$product_tablet <- NA


for (i in 1:length(refine$product_category))
{
  if(refine$product_category[i] == "Smartphone")
  {
    refine$product_smartphone[i] <- 1
    refine$product_tv[i] <- 0
    refine$product_laptop[i] <- 0
    refine$product_tablet[i] <- 0
  }
  
  else if (refine$product_category[i] == "Laptop")
  {
    refine$product_smartphone[i] <- 0
    refine$product_tv[i] <- 0
    refine$product_laptop[i] <- 1
    refine$product_tablet[i] <- 0
  }
  
  else if (refine$product_category[i] == "TV")
  {
    refine$product_smartphone[i] <- 0
    refine$product_tv[i] <- 1
    refine$product_laptop[i] <- 0
    refine$product_tablet[i] <- 0
  }
  
  else if (refine$product_category[i] == "Tablet")
  {
    refine$product_smartphone[i] <- 0
    refine$product_tv[i] <- 0
    refine$product_laptop[i] <- 0
    refine$product_tablet[i] <- 1
  }
}

write.csv(refine, file = "refine_clean.csv")
#Hello World
