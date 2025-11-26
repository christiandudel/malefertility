### TFR ratios: proof of concept ###############################################

  # Uses UN Population Prospects results/data to bound TFR ratios


### Packages & Settings ########################################################

  library(httr)
  library(data.table)
  library(dtplyr)
  library(tidyverse)
  library(maps)
  library(ggthemes)
  library(readxl)

  mapheight <- 9
  mapwidth <- 9


### Links ######################################################################
  
  urlfer <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz"
  urlpop1 <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationExposureBySingleAgeSex_Medium_1950-2023.csv.gz"
  urlpop2 <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationExposureBySingleAgeSex_Medium_2024-2100.csv.gz"
  urldiff <- "https://perso.uclouvain.be/bruno.schoumaker/data/A.%20Estimates%20of%20male%20and%20female%20fertility.xlsx"

    
### Download ###################################################################  
  
  # Where to save it?
  zipfer <- "Download/fert.csv.gz"
  zippop1 <- "Download/pop1.csv.gz"
  zippop2 <- "Download/pop2.csv.gz"
  dirdiff <- "Download/agediff.xlsx"
  
  # Download
  if(!file.exists(zipfer)) GET(urlfer, write_disk(zipfer, overwrite = TRUE), progress() )
  if(!file.exists(zippop1)) GET(urlpop1, write_disk(zippop1, overwrite = TRUE), progress() )
  if(!file.exists(zippop2)) GET(urlpop2, write_disk(zippop2, overwrite = TRUE), progress() )
  if(!file.exists(dirdiff)) GET(urldiff, write_disk(dirdiff, overwrite = TRUE), progress() )
  
  
### Load data ##################################################################
  
  # Load
  fer <- fread(zipfer)
  pop1 <- fread(zippop1)
  pop2 <- fread(zippop2)
  diff <- read_excel(dirdiff)
  
  
### Edit data ##################################################################  
  
  # Combine past and future population
  pop <- rbind(pop1,pop2)
  
  # Country and variant
  fer <- fer %>% filter(Variant=="Medium")
  pop <- pop %>% filter(Variant=="Medium")
  
  # Restrict variables
  fer <- fer %>% select(Time,LocID,Location,AgeGrp,ASFR,Births)
  pop <- pop %>% select(Time,LocID,Location,AgeGrp,PopMale,PopFemale)
  
  # Make age integer (causes a warning)
  pop <- pop %>% mutate(AgeGrp=as.numeric(AgeGrp)) %>% na.omit
  
  # Get counts etc. right
  fer <- fer %>% mutate(ASFR=ASFR/1000,
                        Births=Births*1000)
  
  pop <- pop %>% mutate(PopMale=PopMale * 1000,
                        PopFemale=PopFemale * 1000)

  # Merge pop and fer (warning can be ignored)
  all <- left_join(pop,fer)

  # Set missing to zero
  all <- all %>% mutate(ASFR=ifelse(is.na(ASFR),0,ASFR),
                        Births=ifelse(is.na(Births),0,Births))
  
  
### TFR for women ##############################################################
  
  tfr_women <- all %>% group_by(Location,LocID,Time) %>% summarize(TFR=sum(ASFR))
  age_women <- all %>% group_by(Location,LocID,Time) %>% summarize(MAC=sum(AgeGrp*ASFR/sum(ASFR)))

  
### Fixed age difference #######################################################
  
  # What age differences to consider
  agediff <- -2:20
  
  for(ad in agediff) {

    # Age for merging
    pop <- pop %>% mutate(MergeAge=AgeGrp-ad)
    fer <- fer %>% mutate(MergeAge=AgeGrp)
    
    # Merge pop and fer
    shiftall <- left_join(pop,fer,by=c("Time","Location","LocID","MergeAge"))
    
    # Set missing to zero
    shiftall <- shiftall %>% mutate(ASFR=ifelse(is.na(ASFR),0,ASFR),
                          Births=ifelse(is.na(Births),0,Births))
    
    # Get ASFRs for men
    shiftall <- shiftall %>% mutate(ASFRm=ifelse(PopMale==0|Births==0,
                                                 0,
                                                 Births/PopMale))
    
    # TFR
    tfr_men <- shiftall %>% 
               # Group by country and year
               group_by(Location,LocID,Time) %>%
               # Get sum of ASFRs, Total population
               summarize(mTFR=sum(ASFRm),
                         pop=sum(PopMale)+sum(PopFemale)) |> 
               # Add age difference to the data
               mutate(TFRtype=paste0(ad)) 
               
    
    # Merge (warnings can be ignored)
    fixdiff <- left_join(tfr_men,tfr_women)

    # Combine
    if(ad==agediff[1]) results <- fixdiff else results <- rbind(results,fixdiff)

  }
  
  
### Select relevant scenarios ##################################################
  
  # Age difference between men and women
  ageref <- diff |> 
            select("Country name",
                   "UN Country Code",
                   "Mean age at childbearing (shown on Figures)",
                   "Mean age at fatherhood (shown on Figures)") |> 
            rename("Location" = "Country name",
                   "LocID"="UN Country Code",
                   "MAC"="Mean age at childbearing (shown on Figures)",
                   "PAC"="Mean age at fatherhood (shown on Figures)") |> 
            mutate(PAC=as.numeric(PAC),
                   MAC=as.numeric(MAC),
                   diff=round(PAC-MAC))
  
  # Merge with results 
  results <- left_join(results,ageref,by="LocID") |> 
             rename("Location"="Location.x")
  
  # Replace missing with global mean
  globalmean <- round(mean(ageref$diff,na.rm=T))
  results <- results |> mutate(diff=ifelse(is.na(diff),
                                           globalmean,
                                           diff))
  
  # Difference to reference age difference
  results <- results |> mutate(scenario=diff-as.numeric(TFRtype))
  
  
### TFR ratio & classification #################################################
  
  # Calculate ratio
  results <- results |> mutate(TFRratio=mTFR/TFR)
  
  # Classification of ratio
  results$group[results$TFRratio<0.9] <- 1
  results$group[results$TFRratio>=0.9 & results$TFRratio<1.1] <- 2
  results$group[results$TFRratio>=1] <- 3
  results$Type <- factor(results$group,
                         levels=1:3,labels=c("Birth squeeze (men)",
                                             "Balanced",
                                             "Birth squeeze (women)"))

  
### Show #######################################################################

  # Recode some country names
  results <- results |> mutate(Location=case_match(Location,
                                   "China, Taiwan Province of China"~"Taiwan",
                                   "United States of America"~"USA",
                                   "Republic of Korea"~"South Korea",
                                   .default=Location))
  
  # Select countries 
  countrylist <- c("China","India","Nepal","South Korea",
                   "Taiwan","Norway","Sweden","Germany",
                   "France","Italy","Spain","Colombia",
                   "Brazil","Mexico","United Kingdom","USA")

  # Plot
  fig1 <- results |> filter(Time>=2023 & scenario %in% -2:2 & 
                              Location %in% countrylist) |> 
    ggplot(aes(x=Time,y=TFRratio,group=scenario,col=Type)) + 
    facet_wrap(~Location) + 
    geom_line()+
    scale_colour_manual(values = c("darkred", "yellow2","blue")) +
    labs(x="Year",
         y="Predicted TFR ratio",
         title="Scenarios for trends in global TFR ratios, 2023-2100",
         subtitle="Selected countries",
         caption="Source: Own calculations based on UN data")+
    theme(axis.text.x = element_text(size=8, angle=45))

  
### Get map, fix country names #################################################

  # Get map
  world <- map_data("world")
  
  # Fix country mismatch ('world' often has shorter names compared to UN, etc)
  results <- results |> mutate(Location=recode(Location,
                                               "China, Taiwan Province of China"="Taiwan",
                                               "United States of America"="USA",
                                               "Republic of Korea"="South Korea",
                                               "Viet Nam"="Vietnam",
                                               "Venezuela (Bolivarian Republic of)"="Venezuela",
                                               "Russian Federation"="Russia",
                                               "Türkiye"="Turkey",
                                               "United Kingdom"="UK",
                                               "Antigua and Barbuda"="Antigua",
                                               "Bolivia (Plurinational State of)"="Bolivia",
                                               "Wallis and Futuna Islands"="Wallis and Futuna",
                                               "United States Virgin Islands"="Virgin Islands",
                                               "United Republic of Tanzania"="Tanzania",
                                               "Syrian Arab Republic"="Syria",
                                               "Republic of Moldova"="Moldova",
                                               "Iran (Islamic Republic of)"="Iran",
                                               "Czechia"="Czech Republic",
                                               "Dem. People's Republic of Korea"="North Korea",
                                               "State of Palestine" ="Palestine",
                                               "Eswatini"="Swaziland",
                                               "Côte d'Ivoire"="Ivory Coast",
                                               "Congo"="Republic of Congo",
                                               "Bonaire, Sint Eustatius and Saba"="Saba",
                                               "Falkland Islands (Malvinas)"="Falkland Islands",
                                               "Réunion"="Reunion",
                                               "Brunei Darussalam"="Brunei",
                                               "Cabo Verde"= "Cape Verde",
                                               "Curaçao"="Curacao",
                                               "Saint Barthélemy"="Saint Barthelemy",
                                               "Kosovo (under UNSC res. 1244)"="Kosovo",
                                               "Lao People's Democratic Republic"="Laos",
                                               "Saint Martin (French part)"="Saint Martin",
                                               "Sint Maarten (Dutch part)"="Sint Maarten"
                                               ) )
  
  # 'world' splits a few places which politcally belong together
  world <- world |> mutate(region=recode(region,
                                         "Trinidad"="Trinidad and Tobago",
                                         "Tobago"="Trinidad and Tobago",
                                         "Grenadines"="Saint Vincent and the Grenadines",
                                         "Saint Vincent"="Saint Vincent and the Grenadines",
                                         "Saint Kitts"="Saint Kitts and Nevis",
                                         "Nevis"="Saint Kitts and Nevis",
                                         "Canary Islands"="Spain"))
  
  # For assessing country mismatch 
  # names1 <- unique(world$region)
  # names2 <- unique(results$Location)
  # names2[!names2%in%names1]
  # names1[!names1%in%names2]
  # => should be mostly complete, except very few, very small places
  
  
### Plot map ###################################################################
  
  # Get categories
  ever <- results |> 
          filter(Time%in%c(2023:2060) & scenario %in% -1:1 & !is.na(TFRratio)) |> 
          group_by(Location) |> 
          summarise(squeeze=ifelse(any(TFRratio<0.9) & !any(TFRratio>1.1),
                                   "Men",
                                   ifelse(!any(TFRratio<0.9) & any(TFRratio>1.1),
                                          "Women",
                                          ifelse(any(TFRratio<0.9) & any(TFRratio>1.1),"Both","None"))))
  
  # Count population experiencing the categories
  who <- results |> 
            filter(Time==2023 & Location %in% world$region & TFRtype=="1") |> 
            left_join(ever) |> 
            select(Location,pop,squeeze) |> 
            group_by(squeeze) |> 
            summarize(tot=sum(pop)) |> 
            mutate(prop=tot/sum(tot))
  
  # Merge with map data
  ever <- ever |> rename('region'="Location")
  everworld <- inner_join(world,ever,by="region")
  
  # Plot map 
  figmap <- ggplot(data = everworld, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(col="white",aes(fill=squeeze)) +
    #scale_fill_manual(values = wesanderson::wes_palette("Zissou1", n=4, type = "discrete"))+
    scale_fill_manual(values = c("#fe9929","#beaed4","#1f78b4","#33a02c"))+
    guides(fill=guide_legend(title="Predicted birth squeeze"))+
    ggtitle("Will any birth squeeze occur in 2024-2060?") +
    theme_map(base_size=12)
  
  # Save
  ggsave(figmap,
         height = mapheight,
         width = mapwidth,
         file="Results/fig_map.png")

