
print("==========================ACCESSION DETAILS==================================")
library(dplyr)
library(lubridate)
library(data.table)
library(magrittr)
library(lubridate)
library(stringr)
library(purrr) # For map_df function
library(brapi)

print("2. Set working dir ---------------------------")
setwd("/srv/shiny-server/btract/btract")

source("app/data_prep/fcts.R", local = T)

con = brapi::ba_db()$musabase
banana <- readRDS(paste0("app/data/raw_banana.rds"))

germplasmList = c(unique(as.character(unique(banana$Female_Genotype)), as.character(unique(banana$Male_Genotype))))
glist = c(grep(" ", germplasmList), grep("/",germplasmList))
germplasmList = germplasmList[-glist]
germplasmList = germplasmList[germplasmList>0]
acclist = list()

for (g in 1:length(germplasmList)){
  germplasm <- germplasmList[g]
  tryCatch({
    print(paste(g, " of ", length(germplasmList)));
    acc = ba_germplasm_search(con, germplasmName = germplasm)[,c('germplasmDbId','germplasmName','germplasmPUI')]
    d <- ba_germplasm_attributes(con, germplasmDbId = as.character(acc$germplasmDbId), rclass = "data.frame")
    #print(d)
    p <- d[d$attributeName == "ploidy_level",]$value
    acc$ploidy_level <- ifelse(length(p)>0, p, NA)
    acclist[[germplasm]] <- acc
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df = do.call(plyr::rbind.fill,acclist)
df$germplasmPUI = ifelse(df$germplasmPUI == "", paste0("https://musabase.org/stock/",df$germplasmDbId,"/view"), df$germplasmPUI)
df$url = df$germplasmPUI
df$germplasmPUI = NULL

# manually add links for accessions with spaces
df2 = data.frame(germplasmName = c("ITC1455-Mchare Mlelembo","Mchare Laini", "ITC0249-Calcutta 4" ,"Kisukari Mchare", "02145/1320"),
           url = c("https://musabase.org/stock/65887/view",
                    "https://musabase.org/stock/65045/view",
                    "https://musabase.org/stock/123380/view",
                    "https://musabase.org/stock/65048/view",
                    "https://musabase.org/stock/65419/view"
                    )
)

accession_links = plyr::rbind.fill(df[,c(2,4,3)], df2)
saveRDS(accession_links, file = 'app/data/accession_details.rds')


