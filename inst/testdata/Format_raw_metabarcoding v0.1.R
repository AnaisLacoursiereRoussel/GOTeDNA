################################################################################
##### Date Created  ## 05 Oct 22 ###############################################
##### Date Modified ## 07 May 25 ###############################################
################################################################################

# This code is built to transform raw metabarcoding output data from wide to long
# format for a single project and subsequently obtain species taxonomic information.

# Ensure that the .csv has the columns "Genus" and "Species" at minimum.

#install.packages("tidyverse")
#install.packages("taxize")
#install.packages("readxl")

library(taxize); library(tidyverse); library(readxl)

# Set working directory to the folder which this script and raw metabarcoding data
# are in. Otherwise, bypass this line and enter the pathway to the data below.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Confirm file pathway and file name patterns.
# Ensure all files from the same project are in the same folder with the naming
# pattern: [primer]_[....]_[species_table].csv. The code is contingent on the primer
# e.g., 18s2_run7_lobster_species_table.csv

files = dplyr::tibble(filename = list.files(path = ".",
                                            pattern = "species_table",
                                            full.names = FALSE)) %>%
  dplyr::mutate(filepath = paste0("./",
                                  filename))

files

# Read in completed metadata in Sample Template in order to create eventID and
# occurrenceID in the final exported file.
metadata = readxl::read_excel(path="Sample_template_metabarcoding.xlsx", sheet = 2)

# Loads multiple files as dataframes into a list.
# Double check delimiter in .csv file prior to next step.
# Default is "," but may be ";". sep = "\t" for tab-delimited

df = lapply(files$filepath, read.csv, sep = ",", check.names = FALSE)

# Name the dataframes in the list by extracting target_subfragment from filename
# Location of the last argument has to match where the word is in the filename
names(df) = stringr::str_split(files$filename, "_", simplify = T)[,1]

# This function is to create a new column in each dataframe (e.g., COI1)
f <- function(df, nm) cbind(df, target_subfragment = nm)

df = Map(f, df, names(df))

# Create empty lists for the following loops.
taxon.names = list()
taxons = list()
stacked = list()
multipleHits = list()

# Create dataframe containing multiple hits first by extracting rows containing
# "multiple hits" in Genus column.

multipleHits = lapply(df, function(x)
  dplyr::filter(x, stringr::str_detect(Genus, "Hits")))

# Create binomial "scientificName" column and remove extraneous ones. This
# nomenclature follows that of Darwin Core naming.
for (i in 1:length(df)) {
    df[[i]]$scientificName = paste(df[[i]]$Genus, df[[i]]$Species)

    # These columns will not be used, so can be removed.
    df[[i]] = subset(df[[i]], select=-c(Group, Genus, Species, TaxonName))
}

# Create a new list of taxon names to obtain classifications.
taxon.names = sapply(df, function(x)
  filter(x, !str_detect(scientificName, "Hits")))

#taxon.names = sapply(df, function(x)
#  x$Taxon)

# run this when prior command results in a matrix
#taxon.names = taxon.names[c("scientificName"),]

# run when above command results in a dataframe or list.
#taxon.names = lapply(taxon.names, "[", "scientificName")

taxon.names=unique(taxon.names)
taxon.names=taxon.names[!is.na(taxon.names)]
taxa.id = taxize::get_wormsid(taxon.names, rows=1, marine_only = FALSE)

highertaxa = taxize::classification(taxa.id[!is.na(taxa.id)], db="worms")

new_taxa = lapply(highertaxa, function(x)
  reshape2::melt(x,
                 id.vars=c("name"), # removed ID for now because it was more complicated. retrieve after when have the final taxon name
                 measure.vars="rank",
                 value.name = "rank"))

new_taxa = lapply(new_taxa, function(x)
  tidyr::spread(x, rank, name))

new_taxa = dplyr::bind_rows(new_taxa)
new_taxa=new_taxa[,c("Kingdom","Phylum","Class","Order","Family","Genus","Species")] %>%
  dplyr::distinct()

# when there are gaps in the classification ----
species_df = new_taxa[!is.na(new_taxa$Species),]
genus_df = new_taxa[!is.na(new_taxa$Genus),]
genus_df = genus_df[is.na(genus_df$Species),]
family_df = new_taxa[is.na(new_taxa$Genus),]
family_df = family_df[!is.na(family_df$Family),]
order_df = new_taxa[is.na(new_taxa$Family),]
order_df = order_df[!is.na(order_df$Order),]
class_df = new_taxa[is.na(new_taxa$Order),]
class_df = class_df[!is.na(class_df$Class),]
phylum_df = new_taxa[is.na(new_taxa$Class),]
phylum_df = phylum_df[c(2:3),]

spp.df = df %>%
  right_join(species_df,by=c("Taxon"="Species")) %>%
  rename("scientificName"="Taxon",
         "phylum"="Phylum",
         "kingdom"="Kingdom",
         "class"="Class",
         "order"="Order",
         "family"="Family",
         "genus"="Genus",
         "materialSampleID" = "Numero_unique_echantillon",
         "target_subfragment"="Locus_marker",
         "organismQuantity"="count_by_taxon")

gen.df = df %>%
  right_join(genus_df, by=c("Taxon"="Genus")) %>%
  rename("genus"="Taxon",
         "scientificName"="Species",
         "phylum"="Phylum",
         "kingdom"="Kingdom",
         "class"="Class",
         "order"="Order",
         "family"="Family",
         "materialSampleID" = "Numero_unique_echantillon",
         "target_subfragment"="Locus_marker",
         "organismQuantity"="count_by_taxon")
gen.df$scientificName = gen.df$genus

fam.df = df %>%
  right_join(family_df, by =c("Taxon"="Family")) %>%
  rename("family"="Taxon",
         "scientificName"="Species",
         "phylum"="Phylum",
         "kingdom"="Kingdom",
         "class"="Class",
         "order"="Order",
         "genus"="Genus",
         "materialSampleID" = "Numero_unique_echantillon",
         "target_subfragment"="Locus_marker",
         "organismQuantity"="count_by_taxon")
fam.df$scientificName = fam.df$family

ord.df = df %>%
  right_join(order_df, by=c("Taxon"="Order")) %>%
  rename("order"="Taxon",
         "scientificName"="Species",
         "phylum"="Phylum",
         "kingdom"="Kingdom",
         "class"="Class",
         "family"="Family",
         "genus"="Genus",
         "materialSampleID" = "Numero_unique_echantillon",
         "target_subfragment"="Locus_marker",
         "organismQuantity"="count_by_taxon")
ord.df$scientificName = ord.df$order

cl.df = df %>%
  right_join(class_df, by=c("Taxon"="Class")) %>%
  rename("class"="Taxon",
         "scientificName"="Species",
         "phylum"="Phylum",
         "kingdom"="Kingdom",
         "order"="Order",
         "family"="Family",
         "genus"="Genus",
         "materialSampleID" = "Numero_unique_echantillon",
         "target_subfragment"="Locus_marker",
         "organismQuantity"="count_by_taxon")
cl.df$scientificName=cl.df$class

ph.df = df %>%
  right_join(phylum_df, by =c("Taxon"="Phylum")) %>%
  rename("phylum"="Taxon",
         "scientificName"="Species",
         "kingdom"="Kingdom",
         "class"="Class",
         "order"="Order",
         "family"="Family",
         "genus"="Genus",
         "materialSampleID" = "Numero_unique_echantillon",
         "target_subfragment"="Locus_marker",
         "organismQuantity"="count_by_taxon")
ph.df$scientificName=ph.df$phylum

new_df = bind_rows(spp.df,gen.df,fam.df,ord.df,cl.df,ph.df)


taxa.id = bind_rows(highertaxa)
taxa.id = taxa.id[,-2] %>%
  distinct()

new_taxa = taxa.id %>%
  right_join(new_taxa, by =c("name"="species"))

#test2 = test %>%
#  distinct() %>%
# drop_na(target_subfragment)

# get taxa ids ----

taxa.id=dplyr::tibble(taxonID=paste0("urn:lsid:marinespecies.org:taxname:",names(highertaxa)),
                   scientificName=lapply(highertaxa, function(x) x$name[x$rank=="Species"]))

# remove `Total` column so to add back in as `sampleSizeValue` for each species
sampleSizeValue = lapply(df, function(x) dplyr::select(x, Total, scientificName))

# Joins taxonomic information to metabarcoding data, trims extraneous columns, ----
# and transforms data to long-form
for (k in 1:length(df)){

  df[[k]] = try(df[[k]][!colnames(df[[k]])=="Total"])

  stacked[[k]] = df[[k]] %>%
    dplyr::full_join(new_taxa, by=c('scientificName'='Species')) %>%
   # dplyr::left_join(taxa.id[[k]], by='scientificName') %>% # this is where I need to resume
    dplyr::relocate(c(target_subfragment,scientificName,Kingdom,Phylum,Class,Order,Family,Genus))  %>%
    dplyr::filter(!stringr::str_detect(scientificName, "Hits"), !stringr::str_detect(scientificName, "reads by")) # remove multiple hits

  stacked[[k]] = cbind(stacked[[k]][1:8], stack(stacked[[k]][-(1:8)])) %>%
    dplyr::rename("organismQuantity"="values",
           #"sampleSizeValue"="Total" ,
           "kingdom"="Kingdom",
           "phylum"="Phylum",
           "class"="Class",
           "order"="Order",
           "family"="Family",
           "genus"="Genus",
           "materialSampleID" ="ind")

  stacked[[k]]$taxonID = taxa.id$taxonID[match(stacked[[k]]$scientificName, taxa.id$scientificName)]
  stacked[[k]]$GOTeDNA_ID = metadata$GOTeDNA_ID[1]

  stacked[[k]] = stacked[[k]] %>%
    dplyr::mutate(eventID = paste0(GOTeDNA_ID,"-",materialSampleID),
                  occurrenceID = paste0(eventID,"-",
                                 target_subfragment,"-",
                                 substr(scientificName, 1,1),
                                 substr(stringr::word(scientificName,-1),1,3)), ind = NULL) %>%
    rstatix::drop_na(scientificName)  %>%
    dplyr::mutate(organismQuantityType = "DNA sequence reads")

   stacked[[k]]$sampleSizeValue = sampleSizeValue[[k]]$Total[match(stacked[[k]]$scientificName, sampleSizeValue[[k]]$scientificName)]
   stacked[[k]]$sampleSizeUnit = "DNA sequence reads"
   stacked[[k]] <- stacked[[k]][c("GOTeDNA_ID","materialSampleID","eventID","target_subfragment","scientificName","taxonID",
                                  "kingdom","phylum","class","order","family","genus","occurrenceID","organismQuantity",
                                  "organismQuantityType","sampleSizeValue","sampleSizeUnit")]

   names(stacked) = stacked[[k]]$GOTeDNA_ID[1]

   }


purrr::iwalk(stacked,
       function (x,y) write.csv(x, file=paste0("opt-eDNA-",y,"_metabar_stacked.csv"), row.names = FALSE))




# write multiple hits to file ----

maxlen <- max(lengths(multipleHits))
multipleHits <- lapply(multipleHits, function(lst) c(lst, rep(NA, maxlen - length(lst))))

multHits_df = multipleHits %>%
  map_dfr(.f = bind_rows)


multHits_df= data.frame(multipleHits, check.names = F) %>%
  select(-c(1,2,4,5))

multHits_stacked=cbind(multHits_df[c(1,289)], stack(multHits_df[-c(1,289)])) %>%
  filter(values != 0)


write.csv(multHits_stacked,
          file="C:/Users/morrisonme/OneDrive - DFO-MPO/!Projects/MultipleHits/8-multipleHits-COI.csv",
          row.names = FALSE)


# For Erin's datasets ----

library(phyloseq)

ASVtable = read.csv(files$filepath)

colnames(ASVtable)[colnames(ASVtable) == "group5"] ="genus"

ASVtable = subset(ASVtable, mismatches == "0") # remove all rows with mismatches
ASVtable_messy = filter(ASVtable_clean, str_detect(genus, "[_-]"))

ASVtable_clean_genus=anti_join(ASVtable_clean, ASVtable_messy, by ="genus")

ASVtable_messy_group4 = filter(ASVtable_messy, str_detect(group4, "[_-]"))
ASVtable_clean_group4 = anti_join(ASVtable_messy, ASVtable_messy_group4, by = "group4")

ASVtable_messy_group3 = filter(ASVtable_messy_group4, str_detect(group3, "[_-]"))
ASVtable_clean_group3 = anti_join(ASVtable_messy_group4, ASVtable_messy_group3, by = "group3")

ASVtable_messy_phylum = filter(ASVtable_messy_group3, str_detect(phylum, "[_-]"))
ASVtable_clean_phylum = anti_join(ASVtable_messy_group3, ASVtable_messy_phylum, by = "phylum")

ASVtable_clean_group1 = filter(ASVtable_messy_phylum, str_detect(kingdom, "[_-]"))
ASVtable_clean_kingdom = anti_join(ASVtable_messy_phylum, ASVtable_clean_group1, by = "kingdom")

rm(ASVtable_messy,ASVtable_messy_group3,ASVtable_messy_group4,ASVtable_messy_kingdom,ASVtable_messy_phylum)

species_table = ASVtable_clean[,c(2:40,51,55)] # only keep genus and sample names

taxon.namesG=subset(wormsbynames(ASVtable_clean_genus$genus, marine_only = F),
                          select=c(scientificname,kingdom,phylum,class,order,family,genus))

# Stramenopiles does not exist in WoRMS database > also it's not in Animalia so I don't care
taxons_group1=ASVtable_clean_group1[,46]
taxon.names2=wormsbynames("Stramenopiles", marine_only = F)



taxon.names3=subset(wormsbynames(ASVtable_clean_group3$group3[1:25], marine_only = F, match=T),
                          select=c(scientificname,kingdom,phylum,class,order,family,genus))
taxon.names3.2=subset(wormsbynames(ASVtable_clean_group3$group3[26:37], marine_only = F, match=T),
                    select=c(scientificname,kingdom,phylum,class,order,family,genus))
taxon.names3.3=subset(wormsbynames(ASVtable_clean_group3$group3[42:48], marine_only = F, match=T),
                      select=c(scientificname,kingdom,phylum,class,order,family,genus))


taxon.names4=subset(wormsbynames(ASVtable_clean_group4$group4, marine_only = F),
                          select=c(scientificname,kingdom,phylum,class,order,family,genus))
taxon.namesK=subset(wormsbynames(ASVtable_clean_kingdom$kingdom, marine_only = F),
                          select=c(scientificname,kingdom,phylum,class,order,family,genus))
taxon.namesP=subset(wormsbynames(ASVtable_clean_phylum$phylum, marine_only = F),
                          select=c(scientificname,kingdom,phylum,class,order,family,genus))

taxon.names3 = rbind(taxon.names3,taxon.names3.2, taxon.names3.3)


taxon.namesK = subset(taxon.namesK, kingdom == "Animalia")
taxon.namesP = subset(taxon.namesP, kingdom == "Animalia")
taxon.namesG = subset(taxon.namesG, kingdom == "Animalia")
taxon.names4 = subset(taxon.names4, kingdom == "Animalia")
taxon.names3 = subset(taxon.names3, kingdom == "Animalia")

# Joins taxonomic information to metabarcoding data, trims extraneous columns,
# and transforms dataframes to long-form (i.e., one column per sample).

taxonomicSamples_byGenus = full_join(ASVtable, taxon.namesG, by = "genus")

ASVsGenus = ASVtable_clean_genus[,c(1,51)]
ASVsGroup4 = ASVtable_clean_group4[,c(1,50)]
ASVsGroup3 = ASVtable_clean_group3[,c(1,49)] # doesn't have any animalia
ASVsKingdom = ASVtable_clean_kingdom[,c(1,47)]
ASVsPhylum = ASVtable_clean_phylum[,c(1,48)]

ASVsGenusInfo = left_join(taxon.namesG,ASVsGenus, by = "genus")
ASVsGroup4Info = left_join(taxon.names4, ASVsGroup4, by = c("class"="group4"))
ASVsPhylumInfo = left_join(taxon.namesP, ASVsPhylum, by = "phylum")

# this is because BLAST classified craniata wrong and so it was missed in joining the ASVs
ASVsPhylumInfo$ASV = ASVsPhylumInfo$ASV %>%
replace_na("880ad0eb12d18cc351f9d198f647781b")


ASVsTaxa = rbind(ASVsGenusInfo,ASVsGroup4Info,ASVsPhylumInfo)
ASVsTaxa = unique(ASVsTaxa)

ASVsamples = ASVtable[,c(1:40)]
colnames(ASVsTaxa)[colnames(ASVsTaxa) == "scientificname"] ="scientificName"

stacked = ASVsTaxa %>%
    full_join(ASVsamples, by="ASV") %>%
    relocate(c(scientificName), .after = genus) %>%

   drop_na(kingdom) %>%
  select(-c(ASV))
    #filter(!str_detect(scientificName, "Hits")) # remove multiple hits

stacked = cbind(stacked[1:7], stack(stacked[-(1:7)]))
names(stacked)[names(stacked) == "values"] = 'value' # these lines to the end are to format to the template
stacked$eventID = metadata$eventID[match(stacked$ind, metadata$sampleID)]

# need to find the target subfragment. 18S1 or 2??
stacked = stacked %>%
    mutate(target_subfragment = "18S1", # just put this for now. not sure, will have to get it from Erin on Mar 15.
      occurrenceID = paste0(eventID,"-",
                                 target_subfragment,"-",
                                # substr(scientificName, 1,1),
                                 substr(word(scientificName,-1),1,3)), ind = NULL) %>%
    relocate(eventID, target_subfragment, occurrenceID, .before = kingdom)


write.csv(stacked,
          file="C:/Users/morrisonme/OneDrive - DFO-MPO/!Projects/22 NSF Arctic/opt-eDNA-12_metabar_stacked.csv",
          row.names = FALSE)

test = full_join(stacked,taxa.id, by=c("scientificName"="name"))
test = test %>%
  drop_na(eventID)




# for Nick's dataset----

df = read_excel(path="/Users/morrisonme/OneDrive - DFO-MPO/!Projects/opt-eDNA-13_metabar_data_MM.xlsx", sheet = 4)


taxons = subset(wormsbynames(df$scientificName, marine_only = FALSE),
       select=c(scientificname,kingdom,phylum,class,order,family,genus))

write.csv(taxons,
          file="C:/Users/morrisonme/OneDrive - DFO-MPO/!Projects/13/opt-eDNA-13_metabar_taxa.csv",
          row.names = FALSE)


# for Genevieve's datasets ----
library(purrr)

df[[1]] = df[[1]][,c(5,19:21)]
df[[2]] = df[[2]][,c(13,16:18)]
df[[3]] = df[[3]][,c(5,19:21)]

new_colnames = c("sampleID","scientificName","numberReads","target_subfragment")
df= lapply(df,setNames,new_colnames)


full_df= tibble(bind_rows(taxons))

write.csv(full_df,
          file="C:/Users/morrisonme/OneDrive - DFO-MPO/!Projects/Genevieve/opt-eDNA-06_metabar_taxa.csv",
          row.names = FALSE)

# get taxon id's for all datasets ----

D <- read.csv("data/single_hits_full.csv")
head(D)

taxon_df = tibble()
taxon_df = as_tibble(unique(singleHits_df$scientificName))%>%
  rename("name"="value")

sapply(taxon_df$name, function(x)
  stringr::str_locate(x, ("spp.")))

taxon_df$name[taxon_df$name=="Acartia spp."] = "Acartia"
taxon_df$name[taxon_df$name=="Ciona spp."] = "Ciona"
taxon_df$name[taxon_df$name=="Salmo sp"] = "Salmo"
taxon_df$name[taxon_df$name=="Obelia spp."] = "Obelia"
taxon_df$name[taxon_df$name=="Centropages spp."] = "Centropages"
taxon_df$name[taxon_df$name=="Mytilus spp."] = "Mytilus"
taxon_df$name[taxon_df$name=="Ravinia spp."] = "Ravinia"
taxon_df$name[taxon_df$name=="Canis spp."] = "Canis"
taxon_df$name[taxon_df$name=="Aporrectodea spp."] = "Aporrectodea"
taxon_df$name[taxon_df$name=="Cylindroiulus spp."] = "Cylindroiulus"
taxon_df$name[taxon_df$name=="Ovis spp."] = "Ovis"
taxon_df$name[taxon_df$name=="Ectopleura spp."] = "Ectopleura"
taxon_df$name[taxon_df$name=="Evadne spp."] = "Evadne"
taxon_df$name[taxon_df$name=="Bos spp."] = "Bos"
taxon_df$name[taxon_df$name=="Larus spp."] = "Larus"
taxon_df$name[taxon_df$name=="Oecetis spp."] = "Oecetis"
taxon_df$name[taxon_df$name=="Littorina spp."] = "Littorina"
taxon_df$name[taxon_df$name=="Chrosomus spp."] = "Chrosomus"
taxon_df$name[taxon_df$name=="Catostomus spp."] = "Catostomus"
taxon_df$name[taxon_df$name=="Hymeniacidon spp."] = "Hymeniacidon"
taxon_df$name[taxon_df$name=="Ephydatia spp."] = "Ephydatia"
taxon_df$name[taxon_df$name=="Myoxocephalus spp."] = "Myoxocephalus"
taxon_df$name[taxon_df$name=="Gasterosteus spp."] = "Gasterosteus"
taxon_df$name[taxon_df$name=="Ischnura spp."] = "Ischnura"
taxon_df$name[taxon_df$name=="Anas spp."] = "Anas"

