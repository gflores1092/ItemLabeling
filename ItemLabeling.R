#Load packages
library(stringi)
library(stringr)
library(dplyr)
library(tidytext)
library(readr)
#Run query https://glovoapp.eu.looker.com/sql/tzdd5cx4235vhh in SQL Runner and save file as "productsdata.csv"
#---------------------------------------------------------------------------------------------------
#Read data
products <- read.table('C:/Users/Gerardo Flores/Documents/RGroceries/RProductsClassification/productsdata.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE, allowEscapes = TRUE, quote = "\"", fill = TRUE)
brands <- read.table('C:/Users/Gerardo Flores/Documents/RGroceries/RProductsClassification/brandsgroceries.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE) %>% 
          rename("brand" = "ï..brand") %>%
          mutate(brand = str_to_lower(brand), 
                 provider = str_to_lower(provider)
                 )
#---------------------------------------------------------------------------------------------------
#Homologation
producth <- products %>% 
            mutate(product_name = str_squish(product_name)) %>%
            ##Lower case
            mutate(homo_name = str_to_lower(product_name)) %>%
            ##Weird symbols
            mutate(homo_name = str_replace_all(homo_name, 'Â¢|Ã¢|Â®|â¬|â¢|Â°|Âº|Ã¬\u0081|"', " ")) %>%
            mutate(homo_name = str_replace_all(homo_name, "Ã¬Æ|-", "")) %>%
            mutate(homo_name = str_replace_all(homo_name, " & ", "&")) %>%
            ##Apostrophes
            mutate(homo_name = str_replace_all(homo_name, " Â´", "'")) %>% #convert to apostrophe
            mutate(homo_name = str_replace_all(homo_name, "'", "")) %>% #delete apostrophes (WIP)
            ##Special characters
            mutate(homo_name = str_replace_all(homo_name, "\\(|\\)|\\*|\\<|\\:|\\=", " ")) %>%
            ##External codes
            mutate(homo_name = str_replace_all(homo_name, "\\b[0-9]{5,9}\\b", " ")) %>% #delete external codes 
            ##Supercollection name
            #mutate(homo_name = case_when(supercollection_name == "Pack|Packs" ~ str_replace_all(homo_name, " y |\\+", " + "), TRUE ~ homo_name)) %>% #replaces to unnest: "+"/" y " with " + " 
            ##Homologation
            mutate(homo_name = str_replace_all(homo_name, "\\.\\B|(\\.)(?![0-9]{1,1})", " ")) %>% #erase points boundary and between letters
            mutate(homo_name = str_replace_all(homo_name, " x ", " ")) %>% #deletes character: " x "
            ##Quantity
            ##Un (CHECK Y/O C/S C/U)
            mutate(homo_name = str_replace_all(homo_name, c("y/o" = "o", "c/s" = "con o sin", "s/g" = "sin gas", "c/g" = "con gas", "c/u" = "", "s/s" = "semiseco"))) %>%
            mutate(homo_name = str_replace_all(homo_name, "\\/", " ")) %>%
            mutate(homo_name = str_replace_all(homo_name, regex("(?<=[0-9]{1,4})s"), "un")) %>% #replace 20s, 30s
            mutate(homo_name = str_replace_all(homo_name, regex("(?<=[0-9])x"), "un")) %>% #replace 1x,2x,3x
            mutate(homo_name = str_replace_all(homo_name, "(x)(?=[0-9]{1,1})", " ")) %>% #replace x1,x2,x3
            mutate(homo_name = str_replace_all(homo_name, "^0", "")) %>% #start with zero
            mutate(homo_name = str_replace_all(homo_name, "(\\,)(?=[0-9]{1,1})", ".")) %>% #replace numbers with comma  
            mutate(homo_name = str_replace_all(homo_name, regex("(?:un(?=[id])[a-z]{0,7})"), "un")) %>% #change un.. to "un" (CHECK!!)
            mutate(homo_name = str_replace_all(homo_name, regex("(?<=[0-9]{1,2})pack"), "un")) %>% #change 1pack 2pack to 1un 2un
            mutate(homo_name = str_replace_all(homo_name, ",", " ")) %>% #delete commas and separators
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(-[a-z]{0,1}\\b)"), " ")) %>% #delete separators between words
            mutate(homo_name = str_squish(homo_name)) %>%
            ##Units (se comen palabras revisar)
            ##Join number and units
            mutate(homo_name = str_replace_all(homo_name, c("two pack" = "2un", "four pack" = "4un", "six pack" = "6un", "twelve pack" = "12un"))) %>% #numbers in english
            mutate(homo_name = str_replace_all(homo_name, c("two" = "2", "six" = "6", "twelve" = "12"))) %>% #numbers in english
            mutate(homo_name = str_replace_all(homo_name, regex("(?<=[0-9]{1,4})(\\s)(?=[a-z]{1,2}\\b)", perl = TRUE), "")) %>% # join number and units
            mutate(homo_name = str_replace_all(homo_name, regex("(?<=^[0-9])(.)\\b"), "un ")) %>% #strings that start with number have un attached
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(lt[a-z]{0,1}\\b)|(?<=[0-9]{1,2})(lt[a-z]{0,1}\\b)", perl = TRUE), "l")) %>% #liters
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(cl[a-z]{0,1}\\b)|(?<=[0-9]{1,2})(cl[a-z]{0,1}\\b)", perl = TRUE), "cl")) %>% #centiliters
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(ml[a-z]{0,1}\\b)|(?<=[0-9]{1,2})(m\\b)", perl = TRUE), "ml")) %>% #kilograms
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(kg[a-z]{0,1}\\b)|(?<=[0-9]{1,2})(k[a-z]{0,1}\\b)", perl = TRUE), "kg")) %>% #kilograms
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(gr[a-z]{0,1}\\b)|(?<=[0-9]{1,2})(gr[a-z]{0,1}\\b)", perl = TRUE), "g")) %>% #grams
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(onz[a-z]{0,1}\\b)|(?<=[0-9]{1,2})(onz[a-z]{0,1}\\b)", perl = TRUE), "oz")) %>% #ounces          
            mutate(homo_name = str_replace_all(homo_name, regex("\\b(tableta[a-z]{0,1}\\b)|(?<=[0-9]{1,2})(tableta[a-z]{0,1}\\b)", perl = TRUE), "un")) %>% #tablets
            #Uniform units
            #mutate(homo_name = str_replace_all(homo_name, c("cc$" = "ml", "cl$" = "0ml", "botella" = "un", "six pack" = "6un"))) %>%
            #mutate(homo_name = str_replace_all(homo_name, c("\\bpack\\b" = "un", "latas de" = ""))) %>%
            mutate(homo_name = str_replace_all(homo_name, c("\\buds\\b" = "un", "pzs" = "un", "tab[a-z]{0,1}$" = "un"))) %>% #Uniform units (units)
            mutate(homo_name = str_replace_all(homo_name, c("sob$" = "un","sobres$" = "un"))) %>% #Uniform units (sobres)
            mutate(homo_name = str_replace_all(homo_name, c("cap[a-z]{0,1}$" = "un", "capsulas$" = "un"))) %>% #Uniform units (capsulas)
            mutate(homo_name = str_replace_all(homo_name, c("comp$" = "un", "comprimidos$" = "un"))) %>% #Uniform units (comprimidos)
            mutate(homo_name = str_replace_all(homo_name, c("\\blitros\\b" = "l"))) %>% #Uniform units (litres)          
            mutate(homo_name = str_replace_all(homo_name, c("cc$" = "ml", "cl$" = "0ml", "cm3$" = "ml", "\\buds\\b" = "un"))) %>% #Uniform units (mililitres)
            mutate(homo_name = str_replace_all(homo_name, c("cja" = "", "cj"="", "hjts"="un", "\\gramos\\b"="g"))) %>%
            mutate(homo_name = str_replace_all(homo_name, regex("(?<=[0-9]{1,4})(\\s)(?=[a-z]{1,2}\\b)", perl = TRUE), "")) %>% # join number and units
            ##Fix names
            mutate(homo_name = str_replace_all(homo_name, c("act ii" = "actii", "baby sec" = "babysec", "bell s" = "bells", "bonli" = "bonle", "4 gallos" = "cuatro gallos", "cocacola" = "coca cola", "costeoo" = "costeno", "fuze te "="fuze tea ", "gillete" = "gillette", "inglis" = "ingles", "lucky" = "lucky strike"))) %>%
            mutate(homo_name = str_replace_all(homo_name, c("navarro correas" = "navarro correa","nestli " = "nestle ", "pezziduri" = "peziduri", "play doh" = "playdoh", "scotch brite" = "scotchbrite", "vallealto" = "valle alto", "vitta fresh" = "vittafresh"))) %>%
            ##Final cleaning
            mutate(homo_name = str_replace_all(homo_name, regex("(?<=[a-z]{1,2})(\\.)(?=[0-9]{1,4})"), " ")) %>%
            ##Squish
            mutate(homo_name = str_squish(homo_name)) #%>%
            ##Proper names
            #mutate(homo_name = stri_trans_totitle(homo_name))
#---------------------------------------------------------------------------------------------------
#Brands
pattern <- paste(paste0(paste0("\\b",brands %>% distinct(brand) %>% pull()),"\\b"), collapse = '|')
prodbrand <- producth %>%
             mutate(brand = str_match(homo_name, pattern))
#---------------------------------------------------------------------------------------------------
#Providers
prodprovi <- prodbrand %>%
             left_join(brands %>% select(brand, provider, type, classification), by = c("brand" = "brand"))
#---------------------------------------------------------------------------------------------------
#Final data frame
prodfinal <- prodprovi %>% 
             #Filtrar o unnest todos los que tienen "+"
             mutate(weight = str_extract(homo_name, regex("[0-9]{1,4}(?:(?=[^aehipuv])[a-z]){1,2}|([0-9]{1,4})\\.[0-9]{1,2}(?:(?=[^aehipuv])[a-z]){1,2}"))) %>% #Ignore a,e,h,i,p,u,v
             mutate(units = str_extract(weight,regex("[a-z]{1,2}"))) %>% 
             mutate(weight = str_remove(weight,regex("[a-z]{1,2}"))) %>%
             mutate(quantity = str_remove(str_extract(homo_name, regex("[0-9]{1,4}un")),"un")) %>%
             mutate(quantity = case_when(is.na(quantity) == TRUE ~ 1, TRUE ~ as.numeric(quantity))) %>% #replace NA with 1
             mutate(final_name = str_remove(homo_name, regex("[0-9]{1,4}(?:(?=[^aehipuv])[a-z]){1,2}|[0-9]{1,4}\\.[0-9]{1,2}(?:(?=[^aehipuv])[a-z]){1,2}"))) %>%
             mutate(final_name = str_remove(final_name, regex("[0-9]{1,4}un"))) %>%
             mutate(final_name = str_squish(final_name)) %>%
             mutate(final_name = str_to_title(final_name)) %>%
             mutate(brand = str_to_title(brand)) %>%
             mutate(provider = str_to_title(provider)) #%>%
             #select(supercollection_name:product_name, final_name, quantity, weight, units, brand:classification)
prodcsv <- prodfinal %>%
           select(city_code:product_name, final_name, quantity, weight, units, brand:classification, product_price)
#Export CSV
write_csv(prodfinal, "final_products.csv")
write_csv(prodcsv, "final_products.csv")
