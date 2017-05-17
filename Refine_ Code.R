
#Clean up brand names
 #1st way
refine$company <- as.character(refine$company)

refine <- refine %>% mutate(company=ifelse(grepl("^ph|^fil|^phl", refine$company, ignore.case=TRUE), "philips", refine$company))%>% 
  mutate(company=ifelse(grepl("^van", company, ignore.case=TRUE), "van houten", company)) %>% 
  mutate(company=ifelse(grepl("^uni", company, ignore.case=TRUE), "unilever", company)) %>% 
  mutate(company=ifelse(grepl("^ak", company, ignore.case=TRUE), "akzo", company))
  

#2nd way

#refine$company[grep("^ph|^fil|^phl", refine$company, ignore.case=TRUE)]="philips"
#refine$company[grep("^van", refine$company, ignore.case=TRUE)]="van houten"
#refine$company[grep("^uni", refine$company, ignore.case=TRUE)]="unilever"
#refine$company[grep("^ak", refine$company, ignore.case=TRUE)]="akzo"


#Separate product code and number

refine <- refine %>% separate(Product.code...number, c("product", "number"), sep="-")



#Add product categories

#1st way

#refine$product <- as.factor(refine$product)
#levels(refine$product) <- c("Smartphone","Tablet", "TV", "Laptop")

#2nd way

refine <- refine %>% mutate("product_category"= ifelse(product=="p", "smartphone","")) %>% 
  mutate("product_category"=ifelse(product=="x", "Laptop", product_category)) %>% 
  mutate("product_category"=ifelse(product=="v", "TV", product_category)) %>% 
  mutate("product_category"=ifelse(product=="q", "Tablet", product_category))
  

#Add full address for geocoding

refine <- refine %>% unite("full address", address, city, country, sep=", ") 



      
#Create dummy variables for company and product category

#Company: company_philips, company_akzo, company_van_houten and company_unilever.
refine <- refine %>% mutate(company_philips = ifelse(company =="philips", 1, 0)) %>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = ifelse(company == "van houten", 1, 0)) %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))

#product category: product_smartphone, product_tv, product_laptop and product_tablet.
refine <- refine %>% mutate(product_smartphone = ifelse(product_category== "smartphone", 1, 0)) %>% 
  mutate(product_TV = ifelse(product_category == "TV", 1, 0)) %>% 
  mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>% 
  mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0))

