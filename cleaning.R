library(rvest) #web-scraping package
library(dplyr) 
library(janitor)
library(webchem) #cheminformatics package

#extracting 'table' nodes/tags
data <- read_html("flavis_list.html")
table <- data %>%
    html_nodes("table") %>%
    html_table()

table <- select(table[[1]], -1) #html_table returns a list, extracting the data frame from list

table_cleaned <- table %>%
    head(-1) %>% #excluding the last row
    row_to_names(row_number = 1) %>% #janitor package
    clean_names(case = "snake") %>% #janitor package
    mutate_all(na_if, "") %>% #recode blank cells as NAs
    mutate(ce_no = as.numeric(ce_no), jecfa_no = as.numeric(jecfa_no),
           fema_no = as.numeric(fema_no),
           eu_chemical_group = as.numeric(eu_chemical_group))

smiles_table <- table_cleaned %>%
    #following line queries the cheminformatics for smiles, uncomment to run the code
    #mutate(smiles = unlist(cir_query(flavis_name, match = "first"))) %>%
    select(flavis_name, smiles)

#the function queries the cheminformatics to find the synonyms of the compounds
#specify max_syn to get desired number of synonyms
find_synonyms <- function(flavis_list, max_syn = 25) {
    result <- data.frame()
    
    for (name in flavis_list) {
        synonyms <- pc_synonyms(name, match = "all")[[1]]
        
        if (length(synonyms) > max_syn)
            synonyms <- head(synonyms, max_syn)
        
        #following line removes the CAS nos. returned by the query
        synonyms <- subset(synonyms, !grepl("^[0-9]+-[0-9]+-[0-9]+$", synonyms))
        
        #if no synonyms are returned, add the compound name as the synonym
        if (is.na(synonyms))
            synonyms <- name
        
        frame <- data.frame(name, synonyms)
        names(frame) <- c("flavis_name", "synonym")
        result <- rbind(result, frame)
        Sys.sleep(2)
    }
    return(result)
}

synonyms_list <- find_synonyms(table_cleaned$flavis_name, 25)

