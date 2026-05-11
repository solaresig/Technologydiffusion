library(tidyverse)
library(rworldmap)
library(igraph)
library(stringi)
library(geodist)
library(stargazer)
library(car)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # requires RStudio interactive session; replace with setwd("path/to/script") for command-line use
#### Functions ####
coords2country <- function(df){  
  ll<-subset(df, select=c("lon", "lat"))
  ll[is.na(ll)]<-FALSE
  countriesSP <- getMap(resolution='high')
  pointsSP <- SpatialPoints(ll, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP, use="complete.obs")
  indices$ADMIN<-tolower(indices$ADMIN)
  indices$ADMIN
}
coords2countiso <- function(df){  
  ll<-subset(df, select=c("lon", "lat"))
  ll[is.na(ll)]<-FALSE
  countriesSP <- getMap(resolution='high')
  pointsSP <- SpatialPoints(ll, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP, use="complete.obs")
  return(indices$ISO_A3)
}
extract_chr <- function(word) {
  characters <- strsplit(word, split = "")[[1]]
  return(characters)
}
comparewords<-function(word1, word2, limit){
  if(is.na(word1)|is.na(word2)){
    return(TRUE)
  }else{
    w1<- strsplit(word1, split = "")[[1]]
    w2<- strsplit(word2, split = "")[[1]]
    shortest<-min(length(unique(w1)), length(unique(w2)))
    inter<-intersect(w1, w2) %>% length()    
    if(((inter/shortest)>=limit)){
      return(TRUE)
      }
    else{
      return(FALSE)
      }
  }
}
comparecolumns<-function(column1, column2, limit=0.51){
  valid<-c()
  for(i in 1:length(column1)){
    valid[i]<-comparewords(column1[i], column2[i], limit)
  }
  return(valid)
}
llenararith<-function(x){
  for(i in 1:length(x)){
    k=1
    if(!is.na(x[i])|(i==length(x))|(i==1)){
    }
    else{
      while((is.na(x[i+k]))&(i+k<length(x))){
        k=k+1
      }
      x[i]<-x[i-1]+((x[i+k]-x[i-1])/(k+1))
    }
  }
  return(x)
}
#### 0. Preparing general data ####
crp0<-read_csv("crp3.csv")
#name standardization#
crp0$technology<-str_replace_all(crp0$technology, "[^[:graph:]]|[:punct:]", " ")
crp0$technology<-str_replace_all(crp0$technology, "(\\s+)", " ")
crp0$company<-str_replace_all(crp0$company,"[^[:graph:]]", " ")
crp0$company<-str_replace_all(crp0$company,"(\\s+)", " ")
crp0$text<-str_replace_all(crp0$text,"[^[:graph:]]", " ")
crp0$text<-str_replace_all(crp0$text,"(\\s+)", " ")
crp0$text2<-str_replace_all(crp0$text2,"[^[:graph:]]", " ")
crp0$text2<-str_replace_all(crp0$text2,"(\\s+)", " ")
crp0$firstname<-stri_trans_general(crp0$firstname, "latin-ascii")
crp0$simpname<-stri_trans_general(crp0$simpname, "latin-ascii")
nombres<-crp0 %>% subset(!is.na(simpname)) %>% distinct(.keep_all = T)%>%
  arrange(simpname, year)
maxfn<- nombres %>%mutate(fn=nchar(firstname)) %>% 
  group_by(simpname) %>% summarize(maxfn=max(fn))
nombres<-nombres %>% left_join(maxfn)
nombres <- nombres %>% mutate(firstname=ifelse(nchar(firstname)<maxfn, NA, firstname)) %>% 
  group_by(simpname) %>% fill(firstname, .direction="downup") %>% subset(select=-maxfn) %>%
  select(simpname, firstname, year) %>% distinct()
nombres2<-count(nombres, simpname, firstname) %>% group_by(simpname) %>% 
  filter(n==max(n)) %>% select(simpname, firstname) %>% arrange(simpname)
nombres2$dum<-ifelse(nombres2$simpname==lag(nombres2$simpname)|nombres2$simpname==lead(nombres2$simpname), 1, 0) 
nombres2<-nombres2 %>% subset(dum==0, select=-dum) %>%
  rename(firstname2=firstname)
nombres3<-nombres %>% left_join(nombres2) %>% subset(!is.na(firstname2), select=-firstname) %>% distinct()
crp1<-crp0 %>% left_join(nombres3)
crp1$firstname<-ifelse(is.na(crp1$firstname2), crp1$firstname, crp1$firstname2)
crp1$firstname2<-NULL
write_csv(crp1, "crpt.csv")
rm(crp0, crp1)
combo0<-read_csv("combo5_3.csv")
addendum<-read_csv("combo6_00.csv") %>% #from extended dataset
  mutate(lastname=tolower(str_replace_all(lastname, "[^[:alnum:]]", "")), 
         firstname=tolower(str_replace_all(firstname, "[^[:alnum:]]", "")), company=org) %>%
  subset((type=="professional")&(source!="taime")&(nchar(lastname)>3)&(nchar(firstname)>0)&!is.na(simpname)&!is.na(firstname))%>%
  select(-name)
#merge datasets and name standardization#
combo0<-rbind(combo0, addendum)
combo0<-combo0 %>% subset(!is.na(simpname)&!is.na(firstname))
combo0$org<-str_replace_all(combo0$org,"[^[:graph:]]", " ")
combo0$org<-str_replace_all(combo0$org,"(\\s+)", " ")
combo0$firstname<-stri_trans_general(combo0$firstname, "latin-ascii")
combo0$simpname<-stri_trans_general(combo0$simpname, "latin-ascii")
nombres<-combo0 %>% subset(!is.na(simpname)) %>% distinct(.keep_all = T)%>%
  arrange(simpname, year)
maxfn<- nombres %>%mutate(fn=nchar(firstname)) %>% 
  group_by(simpname) %>% summarize(maxfn=max(fn))
nombres<-nombres %>% left_join(maxfn)
nombres <- nombres %>% mutate(firstname=ifelse(nchar(firstname)<maxfn, NA, firstname)) %>% 
  group_by(simpname) %>% fill(firstname, .direction="downup") %>% subset(select=-maxfn) %>%
  select(simpname, firstname, year) %>% distinct()
nombres2<-count(nombres, simpname, firstname) %>% group_by(simpname) %>% 
  filter(n==max(n)) %>% select(simpname, firstname) %>% arrange(simpname)
nombres2$dum<-ifelse(nombres2$simpname==lag(nombres2$simpname)|nombres2$simpname==lead(nombres2$simpname), 1, 0) 
nombres2<-nombres2 %>% subset(dum==0, select=-dum) %>%
  rename(firstname2=firstname)
nombres3<-nombres %>% left_join(nombres2) %>% subset(!is.na(firstname2), select=-firstname) %>% distinct()
combo1<-combo0 %>% left_join(nombres3)
combo1$firstname<-ifelse(is.na(combo1$firstname2), combo1$firstname, combo1$firstname2)
combo1$firstname2<-NULL
write_csv(combo1, "combot.csv")
rm(combo0, combo1)
together0<-read_csv("together.csv")
together0$simpname<-stri_trans_general(together0$simpname, "latin-ascii")
together0$firstname<-stri_trans_general(together0$firstname, "latin-ascii")
together0$company<-str_replace_all(together0$company, "[^[:graph:]]", " ")
together0$company<-str_replace_all(together0$company,"(\\s+)", " ")
nombres<-together0 %>% subset(!is.na(simpname)) %>% distinct(.keep_all = T)%>%
  arrange(simpname, year)
maxfn<- nombres %>%mutate(fn=nchar(firstname)) %>% 
  group_by(simpname) %>% summarize(maxfn=max(fn))
nombres<-nombres %>% left_join(maxfn)
nombres <- nombres %>% mutate(firstname=ifelse(nchar(firstname)<maxfn, NA, firstname)) %>% 
  group_by(simpname) %>% fill(firstname, .direction="downup") %>% subset(select=-maxfn) %>%
  select(simpname, firstname, year) %>% distinct()
nombres2<-count(nombres, simpname, firstname) %>% group_by(simpname) %>% 
  filter(n==max(n)) %>% select(simpname, firstname) %>% arrange(simpname)
nombres2$dum<-ifelse(nombres2$simpname==lag(nombres2$simpname)|nombres2$simpname==lead(nombres2$simpname), 1, 0) 
nombres2<-nombres2 %>% subset(dum==0, select=-dum) %>%
  rename(firstname2=firstname)
nombres3<-nombres %>% left_join(nombres2) %>% subset(!is.na(firstname2), select=-firstname) %>% distinct()
together1<-together0 %>% left_join(nombres3)
together1$firstname<-ifelse(is.na(together1$firstname2), together1$firstname, together1$firstname2)
together1$firstname2<-NULL
write_csv(together1, "togethert.csv")
rm(together0, together1)
#### 1. Getting sample ####
combo<-read_csv("combot.csv")
crp0<-read_csv("crpt.csv")
crp1<-read_csv("togethert.csv")
crp<-left_join(crp1, 
                subset(crp0, select=c("id","technology", "text2")))
temp<-distinct(crp,  year, org, country)
temp<-subset(temp, !is.na(temp$country)&!is.na(temp$org))
temp<-temp[order(temp$year, temp$org),]
#identify multiple countries per org per year#
temp$mult<-ifelse((lead(temp$org)==temp$org)&(lead(temp$country)!=temp$country),
                  1, 0)
temp<-subset(temp, temp$mult==1, select=-c(country))
crp<-left_join(crp, temp)
crpa<-crp %>% subset(!is.na(technology))
crpb<-crp %>% subset(is.na(technology))

#identify technology#
tech1<-c("cyanid", "arthur.forrest", 
        "amalgamation", "mercury", "quicksilver", "patio(\\s)")%>%paste(collapse="|")
tech2<-c("flotatio", "thickening","concentratio", "jig.table")%>%paste(collapse="|")
crpa$techno1<-str_extract(tolower(crpa$technology), tech1) 
crpb$techno1<-str_extract(tolower(crpb$text2), tech1) 
crpa$techno2<-str_extract(tolower(crpa$technology), tech2) 
crpb$techno2<-str_extract(tolower(crpb$text2), tech2) 

crpa$techno3<-ifelse(str_detect(crpa$technology, "placer")&
                       str_detect(crpa$technology, "gold|silver")&
                       !str_detect(crpa$technology, "cyanid"), "placer", NA)
crpb$techno3<-ifelse(str_detect(crpb$text2, "placer")&
                       str_detect(crpb$text2, "gold")&
                       !str_detect(crpb$text2, "cyanid"), "placer", NA)

crp2<-rbind(crpa, crpb)

crp2$techtemp<-ifelse(str_detect(crp2$techno1, "cyanid|arthur"), 1, NA)

crp2<- crp2 %>% group_by(year, company) %>% fill(techtemp, .direction = "downup")
crp2$tech3<-ifelse(!is.na(crp2$techtemp)&!is.na(crp2$techno3), NA, "placer")
crp2<-pivot_longer(crp2, 
                    cols=c("techno1", "techno2", "techno3"), names_to=NULL, 
                    values_to = "techno")



#define falses positives#
falses<-c("amalgamation[s]*(\\s)[o|w|a|f|h|b|r|t|m|s]", "liquidate", "above", "purchased(\\s)by(\\s)amalg",
          "scheme(\\s)of(\\s)amalga", "mation(\\s)scheme", "to(\\s)acquire", "series(\\s)of(\\s)amalg",
          "amalgamation(\\s)*$", "acquired[,]*(\\s)by(\\s)amalgamation","before(\\s)amalg", "amalgamated",
          "amalgamation(\\s)expen", "since(\\s)amalgamation","this(\\s)amalgama", "general(\\s)amalgama",
          "time(\\s)of(\\s)amalg","after(\\s)amalg", "terms(\\s)of(\\s)amalg", "amalgamation(\\s)into",
          "was(\\s)discarded", "or(\\s)amalgama", "mercury(\\s)syndi")%>% paste(collapse="|")
crp2$false<-ifelse(str_detect(tolower(crp2$text2), falses), 1, 0)
crp2$false<-ifelse((crp2$false==0)&
                      str_detect(tolower(crp2$company), "quicksilver|diamond|coal|quarr|mercur"), 
                    1, crp2$false)
crp2$technology<-ifelse(crp2$false==0, crp2$techno, NA)
crp2$false<-NULL
crp2<-crp2 %>% subset(!is.na(techno))
crp2<-distinct(crp2)
crp2<-crp2[which(!is.na(crp2$technology)),]
crp2<-crp2[order(crp2$year, crp2$company, crp2$technology),]
crp2<-distinct(crp2)
crp2<-subset(crp2, 
             (crp2$company!="Company")&(crp2$company!="Mining Company")&(crp2$company!="Mines Company"))
crp2$technology<-ifelse(str_detect(crp2$technology, "amalg|mercur|quicksilv|patio|placer"), "amalgama",
                        ifelse(str_detect(crp2$technology, "cyan|forrest"), "cyanid", 
                               ifelse(str_detect(crp2$technology, "flot|thick"), "flotation", "gravity")))
crp2<-crp2%>% distinct(year, company, loc, technology, .keep_all = T)
write_csv(crp2, "crptechno.csv")
#### 2. Getting smoothed adoption ####
##### 2.1 Catalogues #####
correct<-read_csv("crptechno2.csv") %>% select(id, technology, techno, dum) #manually corrected the dataset
colnames(correct)<-c("id", "technology","technoc", "dum")
crp<-read_csv("crptechnob.csv")
crp<-left_join(crp, correct) %>% subset(!is.na(dum), select=-dum) %>%
  mutate(techno=ifelse(is.na(technoc), techno, technoc)) %>% select(-technoc)
#cyanidation vs amalgamation#
crp2<-subset(crp, str_detect(crp$technology, "cyanid|amalgama"))
techno<-crp2 %>% 
  count(year, technology)
techno2<-pivot_wider(techno, id_cols=year, names_from=technology, values_from=n)
techno2$cyanid[which(is.na(techno2$cyanid))]<-0
techno2$amalgama[which(is.na(techno2$amalgama))]<-0
techno2$total<-techno2$amalgama+techno2$cyanid
techno2$pera<-100*techno2$amalgama/techno2$total
techno2$perc<-100*techno2$cyanid/techno2$total
techno3<-subset(techno2, select=c("year", "pera", "perc"))
techno3<-pivot_longer(techno3, cols=c("pera", "perc"), names_to="technology", values_to = "percentage")
adoption<-techno3 %>% subset(technology=="perc", select=-c(technology))
adoption$proportion<-adoption$percentage/100
adoption$percentage<-NULL
write_csv(adoption, "adoptionc.csv")
#flotation vs gravity#
crp2<-subset(crp, !str_detect(crp$technology, "cyanid|amalgama"))
techno<-crp2 %>% 
  count(year, technology)
techno2<-pivot_wider(techno, id_cols=year, names_from=technology, values_from=n)
techno2$flotation[which(is.na(techno2$flotation))]<-0
techno2$gravity[which(is.na(techno2$gravity))]<-0
techno2$total<-techno2$gravity+techno2$flotation
techno2$pera<-100*techno2$gravity/techno2$total
techno2$perc<-100*techno2$flotation/techno2$total
techno3<-subset(techno2, select=c("year", "pera", "perc"))
techno3<-pivot_longer(techno3, cols=c("pera", "perc"), names_to="technology", values_to = "percentage")
adoption<-techno3 %>% subset(technology=="perc", select=-c(technology))
adoption$proportion<-adoption$percentage/100
adoption$percentage<-NULL
write_csv(crp, "crptechno3.csv")
write_csv(adoption, "adoptionf.csv")

##### 2.2 EMJ #####
emj2<-read_csv("emjdbase0b.csv")
emj2$text<-tolower(emj2$text)
terms<-c("cyanid", "(\\s)forrest","[-]forrest", "(\\s)patio", "amalgamation") %>% paste(collapse="|")
emj<-emj2 %>% subset(str_detect(text,terms))
emj$type<-ifelse(str_detect(emj$text, "cyanida"), "cyanidation",
                 ifelse(str_detect(emj$text, "forrest")&str_detect(emj$text, "silver|gold"), "cyanidation",
                        ifelse(str_detect(emj$text, "patio|amalgamation")&str_detect(emj$text, "silver|gold")&str_detect(emj$text, "quicksilver|mercury"), "amalgamation",  
                               NA)
                 )
)
emjt<-emj %>% subset(!is.na(type)) 
emjt<-emjt %>% count(year, type)
emjt<-emjt %>% group_by(year) %>% mutate(proportion=n/sum(n))
emjt2<-emjt %>% subset(str_detect(type, "cyani"))
write_csv(emjt2, "adoptioncemj.csv")
terms<-c("flotation", "thickener","gravity") %>% paste(collapse="|")
emj<-emj2 %>% subset(str_detect(text,terms))
emj$type<-ifelse(str_detect(emj$text, "flot|thickener")&str_detect(emj$text, "silver|gold")&!str_detect(emj$text, "flotation expe"), "flotation",
                 ifelse(str_detect(emj$text, "gravit")&str_detect(emj$text, "silver|gold"), "gravity", NA)
                 )
emjt<-emj %>% subset(!is.na(type)) %>% count(year, type)
emjt<-emjt %>% group_by(year) %>% mutate(proportion=n/sum(n))
emjt2<-emjt %>% subset(str_detect(type, "flot"))
write_csv(emjt2, "adoptionfemj.csv")

##### 2.3 Smoothed cyanide #####
ad_cyan_cat<-read_csv("adoptionc.csv")
ad_cyan_emj<-read_csv("adoptioncemj.csv") %>% select(year, proportion)
ad_cyan<-rbind(ad_cyan_cat, ad_cyan_emj)
year<-unique(ad_cyan$year) %>% sort()
cyan_cat<-data.frame(year=year) %>% left_join(ad_cyan_cat) %>%
  mutate(proportion=llenararith(proportion)) %>% 
  fill(proportion, .direction="down") %>%
  mutate(proportion=ifelse(is.na(proportion), 0,proportion))
cyan_emj<-data.frame(year=year) %>% left_join(ad_cyan_emj) %>%
  mutate(proportion=llenararith(proportion)) %>% 
  fill(proportion, .direction="down") %>%
  mutate(proportion=ifelse(is.na(proportion), 0,proportion))
cyan_tot<-rbind(cyan_emj, cyan_cat) %>% arrange(year)
loess_cat<-loess(proportion~year, data=cyan_cat, span=1/3)
loess_emj<-loess(proportion~year, data=cyan_emj, span=1/3)
loess_ad<-loess(proportion~year, data=ad_cyan, span=1/3)
smoothed_cy<-data.frame(year=year, loess_cat=predict(loess_cat, data.frame(year=year)), 
                           loess_emj=predict(loess_emj, data.frame(year=year)), 
                           loess_tot=predict(loess_ad, data.frame(year=year))) %>%
  fill(loess_cat, loess_emj,loess_tot,.direction="downup") %>% 
  mutate(loess_cat=ifelse(loess_cat<0, 0,ifelse(loess_cat>1, 1, loess_cat)), 
         loess_emj=ifelse(loess_emj<0, 0,ifelse(loess_emj>1, 1, loess_emj)),
         loess_tot=ifelse(loess_tot<0, 0,ifelse(loess_tot>1, 1, loess_tot)))

jpeg("cyanidation_adopt.jpg", width=600, height=400)
smoothed_cy %>% rename(Firms=loess_cat, Journal=loess_emj, Average=loess_tot) %>% 
  pivot_longer(cols=c("Firms", "Journal", "Average"), names_to="Source", values_to="Adoption") %>% 
  ggplot(aes(x=year, y=Adoption, linetype=Source))+geom_line()+
  labs(title="Adoption of cyanidation", y="Proportion", x="Year")+
  theme_minimal()
dev.off()
write_csv(smoothed_cy, "smoothed_cy.csv")
##### 2.4 Smoothed flotation #####
ad_flot_cat<-read_csv("adoptionf.csv")
ad_flot_emj<-read_csv("adoptionfemj.csv") %>% select(year, proportion)
ad_flot<-rbind(ad_flot_cat, ad_flot_emj)
year<-unique(ad_flot$year) %>% sort()
flot_cat<-data.frame(year=year) %>% left_join(ad_flot_cat) %>%
  mutate(proportion=llenararith(proportion)) %>% 
  fill(proportion, .direction="down") %>%
  mutate(proportion=ifelse(is.na(proportion), 0,proportion))
flot_emj<-data.frame(year=year) %>% left_join(ad_flot_emj) %>%
  mutate(proportion=llenararith(proportion)) %>% 
  fill(proportion, .direction="down") %>%
  mutate(proportion=ifelse(is.na(proportion), 0,proportion))
flot_tot<-rbind(flot_emj, flot_cat) %>% arrange(year)
loess_cat<-loess(proportion~year, data=flot_cat, span=1/3)
loess_emj<-loess(proportion~year, data=flot_emj, span=1/3)
loess_ad<-loess(proportion~year, data=ad_flot, span=1/3)
smoothed_fl<-data.frame(year=year, loess_cat=predict(loess_cat, data.frame(year=year)), 
                        loess_emj=predict(loess_emj, data.frame(year=year)), 
                        loess_tot=predict(loess_ad, data.frame(year=year))) %>%
  fill(loess_cat, loess_emj,loess_tot,.direction="downup") %>% 
  mutate(loess_cat=ifelse(loess_cat<0, 0,ifelse(loess_cat>1, 1, loess_cat)), 
         loess_emj=ifelse(loess_emj<0, 0,ifelse(loess_emj>1, 1, loess_emj)),
         loess_tot=ifelse(loess_tot<0, 0,ifelse(loess_tot>1, 1, loess_tot)))

jpeg("flotation_adopt.jpg", width=600, height=400)
smoothed_fl %>% rename(Firms=loess_cat, Journal=loess_emj, Average=loess_tot) %>% 
  pivot_longer(cols=c("Firms", "Journal", "Average"), names_to="Source", values_to="Adoption") %>% 
  ggplot(aes(x=year, y=Adoption, linetype=Source))+geom_line()+
  labs(title="Adoption of flotation", y="Proportion", x="Year")+
  theme_minimal()
dev.off()
write_csv(smoothed_fl, "smoothed_fl.csv")
#### 3. Forming the panel ####
##### 3.1 General data #####
###### 3.1.1 Firms ######
firms<-read_csv("crptechno3.csv")
firms<-firms %>% 
  mutate(technology=ifelse(str_detect(tolower(text2), "flotation")&technology=="gravity", "flotation", technology))
firms<-firms %>% arrange(company, year)
firms<- firms %>% group_by(company) %>% nest() #inputing capital
for(i in 1:nrow(firms)){
  if(length(firms$data[[i]])>1){
    firms$data[[i]]<-fill(firms$data[[i]],capital, .direction="downup")
  }
}
firms<-firms %>% unnest(cols=data)
combo<-read_csv("combot.csv")
#total number of people working #
personnel<-combo %>% subset(!is.na(org)&!is.na(year)) %>% count(year, org)
firms<-left_join(firms, subset(personnel), by=c("year","company"= "org"))
firms<-firms %>% group_by(company) %>% nest() #inputing personnel
for(i in 1:nrow(firms)){
  if(length(firms$data[[i]])>1){
    firms$data[[i]]<-fill(firms$data[[i]],n, .direction="downup")
  }
}
firms<-firms %>% unnest(cols=data)
###### 3.1.2 People ######
together<-read_csv("togethert.csv")
together<- together %>% subset(!is.na(together$simpname))%>%
  arrange(simpname, year)
profs<-combo %>% subset(str_detect(type, "alumni|prof")&year>1850) %>%
  arrange(simpname, year)
profs$edu<-ifelse(is.na(profs$edu), "mining eng", profs$edu)
engpatt<-c("eng","chemical","sanitary","chemistry","physics","geolo","science","geogr","highw", 
           "draw", "mechanic", "topogr", "hydrolo") %>% paste(collapse = "|")
profs<-profs %>% 
  subset(str_detect(edu, engpatt)&!str_detect(edu, "english"), 
         select=c(year, simpname, firstname, source, type,edu))
universities<-profs %>% subset(type=="alumni"&!is.na(simpname), select=-c(year, type)) %>% distinct()
colnames(universities)<-c("simpname", "firstname0","source", "education")
associations<-profs %>% subset(type=="professional"&!is.na(simpname), select=-c(year, type)) %>% distinct()
colnames(associations)<-c("simpname", "firstname0","source", "education")
# match employment records with university affiliations
univ<-together %>% left_join(universities, by=c("simpname"), relationship = "many-to-many") %>% 
  subset(!is.na(source)) %>% distinct() %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-dum)
# match employment records with professional associations
assoc<-together %>% left_join(associations, by=c("simpname"), relationship = "many-to-many")%>% 
  subset(!is.na(source))%>% distinct() %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-dum)
# creating professional and alumni dummies
profst<-profs %>% select(-source)%>%rename(firstname0=firstname) %>% 
  distinct() %>% mutate(dum=1) %>%
  pivot_wider(names_from=type, values_from = dum) %>% 
  arrange(simpname, year) %>% group_by(simpname) %>% fill(everything(), .direction="downup") %>% 
  select(-year)%>% distinct() 
profst<-together %>% left_join(profst, by=c("simpname"), relationship = "many-to-many") %>%
  subset(!is.na(professional)|!is.na(alumni)) %>% distinct() %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=c(simpname, firstname0, professional, alumni)) %>% distinct() %>%
  mutate(profalum=ifelse(professional==1&alumni==1, 1, 0))
##### 3.2 Cyanide case #####
###### 3.2.1 Distance and country ######
cyan<-firms %>% 
  subset(technology=="cyanid"|technology=="amalgama", select=c(year, company, technology)) %>% 
  distinct()  
firmscy<-firms %>% subset((technology=="cyanid"|technology=="amalgama")&!is.na(lon)&!is.na(capital), 
                          select=c(year, company, loc, lon, lat, technology, capital, mult, n))
firmscy$dist<-NA
for(i in 1:nrow(firmscy)){
  if(firmscy$year[i]>1893){
    temp1<-firmscy[which((firmscy$year<firmscy$year[i])&(firmscy$technology=="cyanid")),]
    tempi<-data.frame(lon=firmscy$lon[i], lat=firmscy$lat[i])
    vector1<-geodist(tempi, temp1, measure="geodesic")
    firmscy$dist[i]<-min(vector1, na.rm=T)
  } 
}

###### 3.2.2 Experience ######
cyanexp<-together %>% 
  left_join(cyan, relationship = "many-to-many") %>% 
  subset(!is.na(technology)&str_detect(technology, "cyan"), 
         select=c(year, company, simpname, firstname)) %>% distinct() %>%
  mutate(exp=1) #
acumulado<-data.frame()
years<-unique(cyanexp$year) %>% sort()
years<-years[2:length(years)]
for(y in years){
  # get people with experience before year y
  temp<-cyanexp %>% subset(year<y, select=-c(year, company)) %>% rename(firstname0=firstname)
  tempi<-subset(together, year==y) 
  # match current employees with experienced workers
  tempo<-left_join(tempi, temp, by=c("simpname"), relationship = "many-to-many")  %>%
    mutate(dum=comparecolumns(firstname, firstname0)) %>%
    subset(dum==TRUE, select=-c(dum, firstname0)) %>% 
    distinct() 
  acumulado<-rbind(acumulado, tempo)
}
acumulado<-acumulado %>% left_join(profst, by=c("simpname"), relationship = "many-to-many") %>% 
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-firstname0)
acumulado<-acumulado %>%
  mutate(expuni=ifelse(alumni==1&exp==1, 1, 0), #creatred these crossed variables but did not find any significance and did not use them
         expassoc=ifelse(professional==1&exp==1, 1, 0),
         expprof=ifelse(profalum==1&exp==1, 1, 0),
         nexpuni=ifelse(alumni==1&is.na(exp), 1, 0), 
         nexpassoc=ifelse(professional==1&is.na(exp), 1, 0), 
         nexpprof=ifelse(profalum==1&is.na(exp), 1, 0))
experience<-acumulado %>% group_by(year, company) %>% 
  summarize(exp=sum(exp,na.rm=T), expuni=sum(expuni,na.rm=T), expassoc=sum(expassoc,na.rm=T), expprof=sum(expprof,na.rm=T),
            nexpuni=sum(nexpuni,na.rm=T), nexpassoc=sum(nexpassoc,na.rm=T), nexpprof=sum(nexpprof,na.rm=T),
            .groups='keep')
firmscy<-firmscy %>%  left_join(experience)

###### 3.2.3 Networks ######
#all the networks created until that year#
#ssociation and professional networkscreated comprehensively to avoid false negatives#
cyanunis<-univ %>% left_join(cyan, relationship = "many-to-many") %>% subset(!is.na(technology))
years<-unique(cyanunis$year) %>% sort()
univ_centralidad<-data.frame()
for(y in years){
  companies<-cyanunis %>% subset(year==y&!is.na(source), select=c(source, company))%>% distinct()
  colnames(companies)<-c("ori", "dest")
  if(nrow(companies)>0){
  netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
  cent<-as.data.frame(degree(netc, normalized=F))
  cent$univ_eigen<-eigen_centrality(netc)$vector
  cent$company<-rownames(cent)
  cent$univ_ndegree<-degree(netc, normalized = T)
  cent$univ_degree<-cent$`degree(netc, normalized = F)`
  cent$`degree(netc, normalized = F)`<-NULL
  cent$univ_close<-closeness(netc, normalized=T)
  cent$univ_harm<-harmonic_centrality(netc, normalized=T)
  cent$univ_between<-betweenness(netc, normalized=T)
  cent$year<-y
  rownames(cent)<-NULL
  univ_centralidad<-rbind(univ_centralidad, cent)
  }
}

cyanassocs<-assoc %>% left_join(cyan, relationship = "many-to-many") %>% subset(!is.na(technology))
years<-unique(cyanassocs$year) %>% sort()
assoc_centralidad<-data.frame()
for(y in years){
  companies<-cyanassocs %>% subset(year==y&!is.na(source), select=c(source, company))%>% distinct()
  colnames(companies)<-c("ori", "dest")
  if(nrow(companies)>0){
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$assoc_eigen<-eigen_centrality(netc)$vector
    cent$company<-rownames(cent)
    cent$assoc_ndegree<-degree(netc, normalized = T)
    cent$assoc_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$assoc_close<-closeness(netc, normalized=T)
    cent$assoc_harm<-harmonic_centrality(netc, normalized=T)
    cent$assoc_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    assoc_centralidad<-rbind(assoc_centralidad, cent)
  }
}

cyanprofs<-rbind(cyanunis, cyanassocs)
years<-unique(cyanprofs$year) %>% sort()
prof_centralidad<-data.frame()
for(y in years){
  companies<-cyanprofs %>% subset(year==y&!is.na(source), select=c(source, company))%>% distinct()
  colnames(companies)<-c("ori", "dest")
  if(nrow(companies)>0){
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$prof_eigen<-eigen_centrality(netc)$vector
    cent$company<-rownames(cent)
    cent$prof_ndegree<-degree(netc, normalized = T)
    cent$prof_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$prof_close<-closeness(netc, normalized=T)
    cent$prof_harm<-harmonic_centrality(netc, normalized=T)
    cent$prof_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    prof_centralidad<-rbind(prof_centralidad, cent)
  }
}
# technology networks created until that year to avoid false positives#
cyanemploy<-together %>% left_join(cyan, relationship = "many-to-many") %>% subset(!is.na(technology))
years<-unique(cyanemploy$year) %>% sort()
tech_centralidad<-data.frame()
for(y in years){
  previous<-cyanemploy %>% subset((year<y)&(technology=="cyanid"), select=c(simpname, firstname, company)) %>% distinct()
  colnames(previous)<-c("simpname", "firstname0","ori")
  companies<-cyanemploy %>% subset(year==y, select=c(year, simpname, firstname,company))%>% distinct()
  companies<-left_join(companies, previous, relationship = "many-to-many") %>% 
    subset(!is.na(ori)&(ori!=company))
  if(nrow(companies)>0){
    companies$dum<-comparecolumns(companies$firstname, companies$firstname0)
    companies <- companies %>% subset(dum==TRUE, select=c(ori, company))
    colnames(companies)<-c("ori", "dest")
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$tech_eigen<-eigen_centrality(netc)$vector  
    cent$company<-rownames(cent)
    cent$tech_ndegree<-degree(netc, normalized = T)
    cent$tech_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$tech_close<-closeness(netc, normalized=T)
    cent$tech_harm<-harmonic_centrality(netc, normalized=T)
    cent$tech_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    tech_centralidad<-rbind(tech_centralidad, cent)
    }
}

firmscy<-firmscy %>% left_join(univ_centralidad) %>% left_join(assoc_centralidad) %>% 
  left_join(prof_centralidad) %>% left_join(tech_centralidad) 
firmscy[is.na(firmscy)]<-0
##### 3.3 Flotation case #####
#all processing the same as in cyanide case#
###### 3.3.1 Distance and country ######
flot<-firms %>% 
  subset(technology=="gravity"|technology=="flotation", select=c(year, company, technology)) %>% 
  distinct()
firmsfl<-firms %>% subset((technology=="gravity"|technology=="flotation")&!is.na(lon)&!is.na(capital), 
                          select=c(year, company, loc, lon, lat, country, capital, technology, mult, n))
firmsfl$dist<-NA
for(i in 1:nrow(firmsfl)){
  if(firmsfl$year[i]>1893){
    temp1<-firmsfl[which((firmsfl$year<firmsfl$year[i])&(firmsfl$technology=="flotation")),]
    tempi<-data.frame(lon=firmsfl$lon[i], lat=firmsfl$lat[i])
    vector1<-geodist(tempi, temp1, measure="geodesic")
    firmsfl$dist[i]<-min(vector1, na.rm=T)
  } 
}

firmsfl$country<-ifelse(is.na(firmsfl$country)&str_detect(firmsfl$loc, "zealand"), "new zealand", 
                        ifelse(is.na(firmsfl$country)&str_detect(firmsfl$company, "[M|m]exic"), "mexico",
                               firmsfl$country)
)

###### 3.3.2 Experience ######
flotexp<-together %>% 
  left_join(flot, relationship = "many-to-many") %>% 
  subset(!is.na(technology)&str_detect(technology, "flot"), 
         select=c(year, company, simpname, firstname)) %>% distinct() %>%
  mutate(exp=1) %>% left_join(profst, relationship = "many-to-many") %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>%
  subset(dum==TRUE, select=-c(dum, firstname, company)) %>% distinct()
acumulado<-data.frame()
years<-unique(flotexp$year) %>% sort()
years<-years[2:length(years)]
for(y in years){
  temp<-flotexp %>% subset(year<y, select=-c(year)) 
  tempi<-subset(together, year==y) 
  tempo<-left_join(tempi, temp, by=c("simpname"), relationship = "many-to-many")  %>%
    mutate(dum=comparecolumns(firstname, firstname0)) %>%
    subset(dum==TRUE, select=-c(dum, firstname0)) %>% 
    distinct() 
  acumulado<-rbind(acumulado, tempo)
}
acumulado<-acumulado %>% left_join(profst, by=c("simpname"), relationship = "many-to-many") %>% 
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-firstname0)
experience<-acumulado %>% group_by(year, company) %>% 
  summarize(exp=sum(exp,na.rm=T),
            .groups='keep')
firmsfl<-firmsfl %>%  left_join(experience)

###### 3.3.3 Networks ######
flotassocs<-assoc %>% left_join(flot, relationship = "many-to-many") %>% subset(!is.na(technology))
flotunis<-univ %>% left_join(flot, relationship = "many-to-many") %>% subset(!is.na(technology))
flotprofs<-rbind(flotunis, flotassocs)
years<-unique(flotprofs$year) %>% sort()
prof_centralidad<-data.frame()
for(y in years){
  companies<-flotprofs %>% subset(year==y&!is.na(source), select=c(source, company))%>% distinct()
  colnames(companies)<-c("ori", "dest")
  if(nrow(companies)>0){
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$prof_eigen<-eigen_centrality(netc)$vector
    cent$company<-rownames(cent)
    cent$prof_ndegree<-degree(netc, normalized = T)
    cent$prof_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$prof_close<-closeness(netc, normalized=T)
    cent$prof_harm<-harmonic_centrality(netc, normalized=T)
    cent$prof_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    prof_centralidad<-rbind(prof_centralidad, cent)
  }
}

flotemploy<-together %>% left_join(flot, relationship = "many-to-many") %>% subset(!is.na(technology))
years<-unique(flotemploy$year) %>% sort()
tech_centralidad<-data.frame()
for(y in years){
  previous<-flotemploy %>% subset((year<y)&(technology=="flotation"), select=c(simpname, firstname, company)) %>% distinct()
  colnames(previous)<-c("simpname", "firstname0","ori")
  companies<-flotemploy %>% subset(year==y, select=c(year, simpname, firstname,company))%>% distinct()
  companies<-left_join(companies, previous, relationship = "many-to-many") %>% 
    subset(!is.na(ori)&(ori!=company))
  if(nrow(companies)>0){
    companies$dum<-comparecolumns(companies$firstname, companies$firstname0)
    companies <- companies %>% subset(dum==TRUE, select=c(ori, company))
    colnames(companies)<-c("ori", "dest")
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$tech_eigen<-eigen_centrality(netc)$vector  
    cent$company<-rownames(cent)
    cent$tech_ndegree<-degree(netc, normalized = T)
    cent$tech_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$tech_close<-closeness(netc, normalized=T)
    cent$tech_harm<-harmonic_centrality(netc, normalized=T)
    cent$tech_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    tech_centralidad<-rbind(tech_centralidad, cent)
  }
}
firmsfl<-firmsfl %>% left_join(univ_centralidad) %>% left_join(assoc_centralidad) %>% 
  left_join(prof_centralidad) %>% left_join(tech_centralidad) 

firmsfl[is.na(firmsfl)]<-0
##### 3.4 Final dataset #####
firms<-rbind(firmscy, firmsfl)
firms$tipo<-ifelse(str_detect(firms$technology, "cyanid|amalg"), "extraction", "concentration")
#country, capital and distance#
firms$country<-coords2country(firms)
firms$code<-coords2countiso(firms)
firms$capital<-firms$capital/1000000
firms$dist<-firms$dist/1000
firms$country<-ifelse(is.na(firms$country)&str_detect(firms$loc, "zealand"), "new zealand", 
                      ifelse(is.na(firms$country)&str_detect(firms$company, "[M|m]exic"), "mexico",
                             firms$country)
)
firms$empire<-ifelse(str_detect(tolower(firms$country), "trinidad|kingdom|india|africa"), 1, 0)
firms$commonw<-ifelse(str_detect(tolower(firms$country), "trinidad|kingdom|india|africa|australia|canada"), 1, 0)
stcap<-read_csv("statecap.csv")
vdem<-read_csv("vdem.csv")
vdem$country_name<-NULL
vdem$v2x_rule<-NULL
stcap$country_name<-NULL
colnames(vdem)[1]<-"code"
colnames(stcap)[1]<-"code"
vdem<-vdem %>% left_join(stcap)
firmt<-left_join(firms, vdem) 
write_csv(firmt, "firms.csv")

#### 4. Version without capital ####
##### 4.1 Forming the panel #####
###### 4.1.1 General data ######
###### 4.1.2 Firms ######
firms<-read_csv("crptechno3.csv")
firms<-firms %>% 
  mutate(technology=ifelse(str_detect(tolower(text2), "flotation")&technology=="gravity", "flotation", technology))
firms<-firms %>% arrange(company, year)
firms<- firms %>% group_by(company) %>% nest() #inputing capital
for(i in 1:nrow(firms)){
  if(length(firms$data[[i]])>1){
    firms$data[[i]]<-fill(firms$data[[i]],capital, .direction="downup")
  }
}
firms<-firms %>% unnest(cols=data)
combo<-read_csv("combot.csv")
#total number of people working #
personnel<-combo %>% subset(!is.na(org)&!is.na(year)) %>% count(year, org)
firms<-left_join(firms, subset(personnel), by=c("year","company"= "org"))
firms<-firms %>% group_by(company) %>% nest() #inputing personnel
for(i in 1:nrow(firms)){
  if(length(firms$data[[i]])>1){
    firms$data[[i]]<-fill(firms$data[[i]],n, .direction="downup")
  }
}
firms<-firms %>% unnest(cols=data)
###### 4.1.3 People ######
together<-read_csv("togethert.csv")
together<- together %>% subset(!is.na(together$simpname))%>%
  arrange(simpname, year)
profs<-combo %>% subset(str_detect(type, "alumni|prof")&year>1850) %>%
  arrange(simpname, year)
profs$edu<-ifelse(is.na(profs$edu), "mining eng", profs$edu)
engpatt<-c("eng","chemical","sanitary","chemistry","physics","geolo","science","geogr","highw", 
           "draw", "mechanic", "topogr", "hydrolo") %>% paste(collapse = "|")
profs<-profs %>% 
  subset(str_detect(edu, engpatt)&!str_detect(edu, "english"), 
         select=c(year, simpname, firstname, source, type,edu))
universities<-profs %>% subset(type=="alumni"&!is.na(simpname), select=-c(year, type)) %>% distinct()
colnames(universities)<-c("simpname", "firstname0","source", "edu")
associations<-profs %>% subset(type=="professional"&!is.na(simpname), select=-c(year, type)) %>% distinct()
colnames(associations)<-c("simpname", "firstname0","source", "edu")
# match employment records with university affiliations
univ<-together %>% left_join(universities, by=c("simpname"), relationship = "many-to-many") %>% 
  subset(!is.na(source)) %>% distinct() %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-dum)
# match employment records with professional associations
assoc<-together %>% left_join(associations, by=c("simpname"), relationship = "many-to-many")%>% 
  subset(!is.na(source))%>% distinct() %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-dum)
# creating professional and alumni dummies
profst<-profs %>% select(-source)%>%rename(firstname0=firstname) %>% 
  distinct() %>% mutate(dum=1) %>%
  pivot_wider(names_from=type, values_from = dum) %>% 
  arrange(simpname, year) %>% group_by(simpname) %>% fill(everything(), .direction="downup") %>% 
  select(-year)%>% distinct() 
profst<-together %>% left_join(profst, by=c("simpname"), relationship = "many-to-many") %>%
  subset(!is.na(professional)|!is.na(alumni)) %>% distinct() %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=c(simpname, firstname0, professional, alumni)) %>% distinct() %>%
  mutate(profalum=ifelse(professional==1&alumni==1, 1, 0))
##### 4.2 Cyanide case #####
###### 4.2.1 Distance and country ######
cyan<-firms %>% 
  subset(technology=="cyanid"|technology=="amalgama", select=c(year, company, technology)) %>% 
  distinct()  
firmscy<-firms %>% subset((technology=="cyanid"|technology=="amalgama")&!is.na(lon), 
                          select=c(year, company, loc, lon, lat, technology, mult, n))
firmscy$dist<-NA
for(i in 1:nrow(firmscy)){
  if(firmscy$year[i]>1893){
    temp1<-firmscy[which((firmscy$year<firmscy$year[i])&(firmscy$technology=="cyanid")),]
    tempi<-data.frame(lon=firmscy$lon[i], lat=firmscy$lat[i])
    vector1<-geodist(tempi, temp1, measure="geodesic")
    firmscy$dist[i]<-min(vector1, na.rm=T)
  } 
}

###### 4.2.2 Experience ######
cyanexp<-together %>% 
  left_join(cyan, relationship = "many-to-many") %>% 
  subset(!is.na(technology)&str_detect(technology, "cyan"), 
         select=c(year, company, simpname, firstname)) %>% distinct() %>%
  mutate(exp=1) #
acumulado<-data.frame()
years<-unique(cyanexp$year) %>% sort()
years<-years[2:length(years)]
for(y in years){
  # get people with experience before year y
  temp<-cyanexp %>% subset(year<y, select=-c(year, company)) %>% rename(firstname0=firstname)
  tempi<-subset(together, year==y) 
  # match current employees with experienced workers
  tempo<-left_join(tempi, temp, by=c("simpname"), relationship = "many-to-many")  %>%
    mutate(dum=comparecolumns(firstname, firstname0)) %>%
    subset(dum==TRUE, select=-c(dum, firstname0)) %>% 
    distinct() 
  acumulado<-rbind(acumulado, tempo)
}
acumulado<-acumulado %>% left_join(profst, by=c("simpname"), relationship = "many-to-many") %>% 
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-firstname0)
acumulado<-acumulado %>%
  mutate(expuni=ifelse(alumni==1&exp==1, 1, 0), #creatred these crossed variables but did not find any significance and did not use them
         expassoc=ifelse(professional==1&exp==1, 1, 0),
         expprof=ifelse(profalum==1&exp==1, 1, 0),
         nexpuni=ifelse(alumni==1&is.na(exp), 1, 0), 
         nexpassoc=ifelse(professional==1&is.na(exp), 1, 0), 
         nexpprof=ifelse(profalum==1&is.na(exp), 1, 0))
experience<-acumulado %>% group_by(year, company) %>% 
  summarize(exp=sum(exp,na.rm=T), expuni=sum(expuni,na.rm=T), expassoc=sum(expassoc,na.rm=T), expprof=sum(expprof,na.rm=T),
            nexpuni=sum(nexpuni,na.rm=T), nexpassoc=sum(nexpassoc,na.rm=T), nexpprof=sum(nexpprof,na.rm=T),
            .groups='keep')
firmscy<-firmscy %>%  left_join(experience)

###### 4.2.3 Networks ######
#all the networks created until that year#
#association and professional networkscreated comprehensively to avoid false negatives#
cyanunis<-univ %>% left_join(cyan, relationship = "many-to-many") %>% subset(!is.na(technology))
cyanassocs<-assoc %>% left_join(cyan, relationship = "many-to-many") %>% subset(!is.na(technology))
cyanprofs<-rbind(cyanunis, cyanassocs)
years<-unique(cyanprofs$year) %>% sort()
prof_centralidad<-data.frame()
for(y in years){
  companies<-cyanprofs %>% subset(year==y&!is.na(source), select=c(source, company))%>% distinct()
  colnames(companies)<-c("ori", "dest")
  if(nrow(companies)>0){
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$prof_eigen<-eigen_centrality(netc)$vector
    cent$company<-rownames(cent)
    cent$prof_ndegree<-degree(netc, normalized = T)
    cent$prof_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$prof_close<-closeness(netc, normalized=T)
    cent$prof_harm<-harmonic_centrality(netc, normalized=T)
    cent$prof_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    prof_centralidad<-rbind(prof_centralidad, cent)
  }
}
# technology networks created until that year to avoid false positives#
cyanemploy<-together %>% left_join(cyan, relationship = "many-to-many") %>% subset(!is.na(technology))
years<-unique(cyanemploy$year) %>% sort()
tech_centralidad<-data.frame()
for(y in years){
  previous<-cyanemploy %>% subset((year<y)&(technology=="cyanid"), select=c(simpname, firstname, company)) %>% distinct()
  colnames(previous)<-c("simpname", "firstname0","ori")
  companies<-cyanemploy %>% subset(year==y, select=c(year, simpname, firstname,company))%>% distinct()
  companies<-left_join(companies, previous, relationship = "many-to-many") %>% 
    subset(!is.na(ori)&(ori!=company))
  if(nrow(companies)>0){
    companies$dum<-comparecolumns(companies$firstname, companies$firstname0)
    companies <- companies %>% subset(dum==TRUE, select=c(ori, company))
    colnames(companies)<-c("ori", "dest")
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$tech_eigen<-eigen_centrality(netc)$vector  
    cent$company<-rownames(cent)
    cent$tech_ndegree<-degree(netc, normalized = T)
    cent$tech_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$tech_close<-closeness(netc, normalized=T)
    cent$tech_harm<-harmonic_centrality(netc, normalized=T)
    cent$tech_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    tech_centralidad<-rbind(tech_centralidad, cent)
  }
}

firmscy<-firmscy %>% left_join(univ_centralidad) %>% left_join(assoc_centralidad) %>% 
  left_join(prof_centralidad) %>% left_join(tech_centralidad) # %>% left_join(gen_centralidad)
firmscy[is.na(firmscy)]<-0
##### 4.3 Flotation case #####
#all processing the same as in cyanide case#
###### 4.3.1 Distance and country ######
flot<-firms %>% 
  subset(technology=="gravity"|technology=="flotation", select=c(year, company, technology)) %>% 
  distinct()
firmsfl<-firms %>% subset((technology=="gravity"|technology=="flotation")&!is.na(lon), 
                          select=c(year, company, loc, lon, lat, country, technology, mult, n))
firmsfl$dist<-NA
for(i in 1:nrow(firmsfl)){
  if(firmsfl$year[i]>1893){
    temp1<-firmsfl[which((firmsfl$year<firmsfl$year[i])&(firmsfl$technology=="flotation")),]
    tempi<-data.frame(lon=firmsfl$lon[i], lat=firmsfl$lat[i])
    vector1<-geodist(tempi, temp1, measure="geodesic")
    firmsfl$dist[i]<-min(vector1, na.rm=T)
  } 
}

firmsfl$country<-ifelse(is.na(firmsfl$country)&str_detect(firmsfl$loc, "zealand"), "new zealand", 
                        ifelse(is.na(firmsfl$country)&str_detect(firmsfl$company, "[M|m]exic"), "mexico",
                               firmsfl$country)
)

###### 4.3.2 Experience ######
flotexp<-together %>% 
  left_join(flot, relationship = "many-to-many") %>% 
  subset(!is.na(technology)&str_detect(technology, "flot"), 
         select=c(year, company, simpname, firstname)) %>% distinct() %>%
  mutate(exp=1) %>% left_join(profst, relationship = "many-to-many") %>%
  mutate(dum=comparecolumns(firstname, firstname0)) %>%
  subset(dum==TRUE, select=-c(dum, firstname, company)) %>% distinct()
acumulado<-data.frame()
years<-unique(flotexp$year) %>% sort()
years<-years[2:length(years)]
for(y in years){
  temp<-flotexp %>% subset(year<y, select=-c(year)) 
  tempi<-subset(together, year==y) 
  tempo<-left_join(tempi, temp, by=c("simpname"), relationship = "many-to-many")  %>%
    mutate(dum=comparecolumns(firstname, firstname0)) %>%
    subset(dum==TRUE, select=-c(dum, firstname0)) %>% 
    distinct() 
  acumulado<-rbind(acumulado, tempo)
}
acumulado<-acumulado %>% left_join(profst, by=c("simpname"), relationship = "many-to-many") %>% 
  mutate(dum=comparecolumns(firstname, firstname0)) %>% 
  subset(dum==TRUE, select=-firstname0)
experience<-acumulado %>% group_by(year, company) %>% 
  summarize(exp=sum(exp,na.rm=T),
            .groups='keep')
firmsfl<-firmsfl %>%  left_join(experience)

###### 4.3.3 Networks ######
flotunis<-univ %>% left_join(flot, relationship = "many-to-many") %>% subset(!is.na(technology))
flotassocs<-assoc %>% left_join(flot, relationship = "many-to-many") %>% subset(!is.na(technology))
flotprofs<-rbind(flotunis, flotassocs) %>%distinct()
years<-unique(flotprofs$year) %>% sort()
prof_centralidad<-data.frame()
for(y in years){
  companies<-flotprofs %>% subset(year==y&!is.na(source), select=c(source, company))%>% distinct()
  colnames(companies)<-c("ori", "dest")
  if(nrow(companies)>0){
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$prof_eigen<-eigen_centrality(netc)$vector
    cent$company<-rownames(cent)
    cent$prof_ndegree<-degree(netc, normalized = T)
    cent$prof_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$prof_close<-closeness(netc, normalized=T)
    cent$prof_harm<-harmonic_centrality(netc, normalized=T)
    cent$prof_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    prof_centralidad<-rbind(prof_centralidad, cent)
  }
}

flotemploy<-together %>% left_join(flot, relationship = "many-to-many") %>% subset(!is.na(technology))
years<-unique(flotemploy$year) %>% sort()
tech_centralidad<-data.frame()
for(y in years){
  previous<-flotemploy %>% subset((year<y)&(technology=="flotation"), select=c(simpname, firstname, company)) %>% distinct()
  colnames(previous)<-c("simpname", "firstname0","ori")
  companies<-flotemploy %>% subset(year==y, select=c(year, simpname, firstname,company))%>% distinct()
  companies<-left_join(companies, previous, relationship = "many-to-many") %>% 
    subset(!is.na(ori)&(ori!=company))
  if(nrow(companies)>0){
    companies$dum<-comparecolumns(companies$firstname, companies$firstname0)
    companies <- companies %>% subset(dum==TRUE, select=c(ori, company))
    colnames(companies)<-c("ori", "dest")
    netc<-graph_from_data_frame(d=companies, directed=F) # creates graph from igraph library
    cent<-as.data.frame(degree(netc, normalized=F))
    cent$tech_eigen<-eigen_centrality(netc)$vector  
    cent$company<-rownames(cent)
    cent$tech_ndegree<-degree(netc, normalized = T)
    cent$tech_degree<-cent$`degree(netc, normalized = F)`
    cent$`degree(netc, normalized = F)`<-NULL
    cent$tech_close<-closeness(netc, normalized=T)
    cent$tech_harm<-harmonic_centrality(netc, normalized=T)
    cent$tech_between<-betweenness(netc, normalized=T)
    cent$year<-y
    rownames(cent)<-NULL
    tech_centralidad<-rbind(tech_centralidad, cent)
  }
}

firmsfl<-firmsfl %>% left_join(univ_centralidad) %>% left_join(assoc_centralidad) %>% 
  left_join(prof_centralidad) %>% left_join(tech_centralidad) # %>% left_join(gen_centralidad)

firmsfl[is.na(firmsfl)]<-0
##### 4.4 Final dataset #####
firms<-rbind(firmscy, firmsfl)
firms$tipo<-ifelse(str_detect(firms$technology, "cyanid|amalg"), "extraction", "concentration")
#country, capital and distance#
firms$country<-coords2country(firms)
firms$code<-coords2countiso(firms)
firms$dist<-firms$dist/1000
firms$country<-ifelse(is.na(firms$country)&str_detect(firms$loc, "zealand"), "new zealand", 
                      ifelse(is.na(firms$country)&str_detect(firms$company, "[M|m]exic"), "mexico",
                             firms$country)
)
firms$empire<-ifelse(str_detect(tolower(firms$country), "trinidad|kingdom|india|africa"), 1, 0)
firms$commonw<-ifelse(str_detect(tolower(firms$country), "trinidad|kingdom|india|africa|australia|canada"), 1, 0)
stcap<-read_csv("statecap.csv")
vdem<-read_csv("vdem.csv")
vdem$country_name<-NULL
vdem$v2x_rule<-NULL
stcap$country_name<-NULL
colnames(vdem)[1]<-"code"
colnames(stcap)[1]<-"code"
vdem<-vdem %>% left_join(stcap)
firmt<-left_join(firms, vdem) 
write_csv(firmt, "firmsnocapital.csv")


####5.Models####
#####5.0 comparing samples#####
sample<-read_csv("firms.csv") %>% mutate(set="sample") %>% select(-capital)
total<-read_csv("firmsnocapital.csv") %>% mutate(set="total")
tabla<-rbind(sample, total)
tabla$dist<-ifelse(is.infinite(tabla$dist), NA, tabla$dist)
firmscy<-tabla %>% subset(tipo=="extraction", select=-c(loc, lat, lon))
adoptionc<-read_csv("smoothed_cy.csv") %>% rename(proportion=loess_tot) %>%
  select(year, proportion)
adoptionc$adoptionrate<-lag(adoptionc$proportion)
adoptionc$adoptionrate[1]<-0
firmscy<-firmscy %>% left_join(adoptionc)
firmscy$totalp<-firmscy$n
firmscy$prob<-ifelse(firmscy$technology=="cyanid", 1, 0)
firmscy$district<-ifelse(firmscy$dist<65, 1, 0) #defining a mining districtr within 65 km of previous adopter
firmsfl<-tabla %>% subset(tipo=="concentration", select=-c(loc, lat, lon))
adoptionf<-read_csv("smoothed_fl.csv") %>% rename(proportion=loess_tot) %>%
  select(year, proportion)
adoptionf$adoptionrate<-lag(adoptionf$proportion)
adoptionf$adoptionrate[1]<-0
firmsfl<-firmsfl %>% left_join(adoptionf)
firmsfl$totalp<-firmsfl$n
firmsfl$prob<-ifelse(firmsfl$technology=="flotation", 1, 0)
firmsfl$district<-ifelse(firmsfl$dist<65, 1, 0)
tabla<-rbind(firmsfl, firmscy)
length(unique(sample$company))
length(unique(total$company))
set
resumen<-tabla %>% group_by(set, tipo) %>% 
  mutate(companies=length(unique(company)))%>%
  summarize(mult = mean(mult), dist=mean(dist, na.rm=T), personnel=mean(n), 
            empire=mean(empire), adoptionrate=mean(adoptionrate), district=mean(district, na.rm=T), 
            v2x_rule=mean(v2x_rule, na.rm=T), v2xcl_prpty=mean(v2xcl_prpty, na.rm=T), adoptionrate=mean(adoptionrate), 
            prob=mean(prob), n=n(), companies=mean(companies))
write_csv(resumen, "summarycomparison.csv")

##### 5.1 Network models with capital####
firms<-read_csv("firms.csv")
firmscy<-firms %>% subset(tipo=="extraction", select=-c(loc, lat, lon))
adoptionc<-read_csv("smoothed_cy.csv") %>% rename(proportion=loess_tot) %>%
  select(year, proportion)
adoptionc$adoptionrate<-lag(adoptionc$proportion)
adoptionc$adoptionrate[1]<-0
firmscy<-firmscy %>% left_join(adoptionc)
firmscy$totalp<-firmscy$n
firmscy$prob<-ifelse(firmscy$technology=="cyanid", 1, 0)
firmscy$district<-ifelse(firmscy$dist<65, 1, 0) #defining a mining districtr within 65 km of previous adopter
firmsfl<-firms %>% subset(tipo=="concentration", select=-c(loc, lat, lon))%>% subset(year >= 1900)
adoptionf<-read_csv("smoothed_fl.csv") %>% rename(proportion=loess_tot) %>%
  select(year, proportion)
adoptionf$adoptionrate<-lag(adoptionf$proportion)
adoptionf$adoptionrate[1]<-0
firmsfl<-firmsfl %>% left_join(adoptionf)
firmsfl$totalp<-firmsfl$n
firmsfl$prob<-ifelse(firmsfl$technology=="flotation", 1, 0)
firmsfl$district<-ifelse(firmsfl$dist<65, 1, 0)

######5.1.1 Model cyanidation#####
firmscy<- firmscy %>%
  rename(prob_adoption=prob)
nullmodel<-glm(prob_adoption~1, data=firmscy, family=binomial(link="probit"))
modeltot0<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"),
                data=firmscy %>% 
                  mutate(tacit=tech_degree,codified=prof_degree))
summary(modeltot0)
pseudo_modeltot0<-as.numeric(1-logLik(modeltot0)/logLik(update(modeltot0, .~1)))
modeltot1<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmscy %>% 
                  mutate(codified=prof_ndegree,
                         tacit=tech_ndegree))
summary(modeltot1)
pseudo_modeltot1<-as.numeric(1-logLik(modeltot1)/logLik(update(modeltot1, .~1)))
pseudo_modeltot1
modeltot2<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire,
               family=binomial(link="probit"), 
               data=firmscy %>% 
                 mutate(codified=prof_between,
                        tacit=tech_between))
summary(modeltot2)
pseudo_modeltot2<-as.numeric(1-logLik(modeltot2)/logLik(update(modeltot2, .~1)))

modeltot3<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire,
               family=binomial(link="probit"), 
               data=firmscy %>% 
                 mutate(codified=prof_eigen,
                        tacit=tech_eigen))
summary(modeltot3)
pseudo_modeltot3<-as.numeric(1-logLik(modeltot3)/logLik(update(modeltot3, .~1)))


modeltot4<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmscy %>% 
                  mutate(codified=prof_close,
                         tacit=tech_close))
summary(modeltot4)
pseudo_modeltot4<-as.numeric(1-logLik(modeltot4)/logLik(update(modeltot4, .~1)))


vif_modeltot0<-vif(modeltot0) %>% max() %>% round(4)
vif_modeltot1<-vif(modeltot1)%>% max() %>% round(4)
vif_modeltot2<-vif(modeltot2)%>% max() %>% round(4)
vif_modeltot3<-vif(modeltot3)%>% max() %>% round(4)
vif_modeltot4<-vif(modeltot4)%>% max() %>% round(4)

stargazer(modeltot1, modeltot2, modeltot3,modeltot4, type="html", out="probitcyanidation.html", 
          notes=paste("Null: Loglik ", round(logLik(nullmodel), 3), "; AIC ", round(AIC(nullmodel), 3), sep=""), 
          add.lines = list(c("Type of centrality score", "ndegree", "betweenness", "eigenvector","closeness"), 
                           c("Max VIF", vif_modeltot1, vif_modeltot2, vif_modeltot3, vif_modeltot4), 
                           c("McFadden R2", round(pseudo_modeltot1,4), 
                             round(pseudo_modeltot2,4), round(pseudo_modeltot3,4), round(pseudo_modeltot4,4))
                           )
)

######5.1.2 Model flotation#####
firmsfl<- firmsfl %>%
  rename(prob_adoption=prob)
nullmodel_fl<-glm(prob_adoption~1, data=firmsfl, family=binomial(link="probit"))
modeltot0_fl<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"),
                data=firmsfl %>% 
                  mutate(tacit=tech_degree,codified=prof_degree))
summary(modeltot0_fl)
pseudo_modeltot0_fl<-as.numeric(1-logLik(modeltot0_fl)/logLik(update(modeltot0_fl, .~1)))
modeltot1_fl<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmsfl %>% 
                  mutate(codified=prof_ndegree,
                         tacit=tech_ndegree))
summary(modeltot1_fl)
pseudo_modeltot1_fl<-as.numeric(1-logLik(modeltot1_fl)/logLik(update(modeltot1_fl, .~1)))
pseudo_modeltot1_fl
modeltot2_fl<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire,
               family=binomial(link="probit"), 
               data=firmsfl %>% 
                 mutate(codified=prof_between,
                        tacit=tech_between))
summary(modeltot2_fl)
pseudo_modeltot2_fl<-as.numeric(1-logLik(modeltot2_fl)/logLik(update(modeltot2_fl, .~1)))

modeltot3_fl<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire,
               family=binomial(link="probit"), 
               data=firmsfl %>% 
                 mutate(codified=prof_eigen,
                        tacit=tech_eigen))
summary(modeltot3_fl)
pseudo_modeltot3_fl<-as.numeric(1-logLik(modeltot3_fl)/logLik(update(modeltot3_fl, .~1)))


modeltot4_fl<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+capital+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmsfl %>% 
                  mutate(codified=prof_close,
                         tacit=tech_close))
summary(modeltot4_fl)
pseudo_modeltot4_fl<-as.numeric(1-logLik(modeltot4_fl)/logLik(update(modeltot4_fl, .~1)))


vif_modeltot0_fl<-vif(modeltot0_fl) %>% max() %>% round(4)
vif_modeltot1_fl<-vif(modeltot1_fl)%>% max() %>% round(4)
vif_modeltot2_fl<-vif(modeltot2_fl)%>% max() %>% round(4)
vif_modeltot3_fl<-vif(modeltot3_fl)%>% max() %>% round(4)
vif_modeltot4_fl<-vif(modeltot4_fl)%>% max() %>% round(4)

stargazer(modeltot1_fl, modeltot2_fl, modeltot3_fl,modeltot4_fl, type="html", out="probitflotation.html", 
          notes=paste("Null: Loglik ", round(logLik(nullmodel_fl), 3), "; AIC ", round(AIC(nullmodel_fl), 3), sep=""), 
          add.lines = list(c("Type of centrality score", "ndegree", "betweenness", "eigenvector","closeness"), 
                           c("Max VIF", vif_modeltot1_fl, vif_modeltot2_fl, vif_modeltot3_fl, vif_modeltot4_fl), 
                           c("McFadden R2", round(pseudo_modeltot1_fl,4), 
                             round(pseudo_modeltot2_fl,4), round(pseudo_modeltot3_fl,4), round(pseudo_modeltot4_fl,4))
          )
)

firms<-rbind(firmsfl, firmscy)
write_csv(firms, "firmsfinal.csv")

##### 5.2 Network models (no capital) ####
firms<-read_csv("firmsnocapital.csv") 
###### 5.2.1 Model cyanide #####
firmscy_nc<-firms %>% subset(tipo=="extraction", select=-c(loc, lat, lon))
adoptionc<-read_csv("smoothed_cy.csv") %>% rename(proportion=loess_tot) %>%
  select(year, proportion)
adoptionc$adoptionrate<-lag(adoptionc$proportion)
adoptionc$adoptionrate[1]<-0
firmscy_nc<-firmscy_nc %>% left_join(adoptionc)
firmscy_nc$totalp<-firmscy_nc$n
firmscy_nc$prob<-ifelse(firmscy_nc$technology=="cyanid", 1, 0)
firmscy_nc$district<-ifelse(firmscy_nc$dist<65, 1, 0) #defining a mining districtr within 65 km of previous adopter
firmscy_nc<-firmscy_nc %>% 
  rename(prob_adoption=prob)
nullmodel_cy_nc<-glm(prob_adoption~1, data=firmscy_nc, family=binomial(link="probit"))
modeltot1_cy_nc<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmscy_nc %>% 
                  mutate(codified=prof_ndegree,
                         tacit=tech_ndegree))
summary(modeltot1_cy_nc)
pseudo_modeltot1_cy_nc<-as.numeric(1-logLik(modeltot1_cy_nc)/logLik(update(modeltot1_cy_nc, .~1)))

modeltot2_cy_nc<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
               family=binomial(link="probit"), 
               data=firmscy_nc %>% 
                 mutate(codified=prof_between,
                        tacit=tech_between))
summary(modeltot2_cy_nc)
pseudo_modeltot2_cy_nc<-as.numeric(1-logLik(modeltot2_cy_nc)/logLik(update(modeltot2_cy_nc, .~1)))

modeltot3_cy_nc<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
               family=binomial(link="probit"), 
               data=firmscy_nc %>% 
                 mutate(codified=prof_eigen,
                        tacit=tech_eigen))
summary(modeltot3_cy_nc)
pseudo_modeltot3_cy_nc<-as.numeric(1-logLik(modeltot3_cy_nc)/logLik(update(modeltot3_cy_nc, .~1)))


modeltot4_cy_nc<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmscy_nc%>% 
                  mutate(codified=prof_close,
                         tacit=tech_close))
summary(modeltot4_cy_nc)
pseudo_modeltot4_cy_nc<-as.numeric(1-logLik(modeltot4_cy_nc)/logLik(update(modeltot4_cy_nc, .~1)))


vif_modeltot1_cy_nc<-vif(modeltot1_cy_nc)%>% max() %>% round(4)
vif_modeltot2_cy_nc<-vif(modeltot2_cy_nc)%>% max() %>% round(4)
vif_modeltot3_cy_nc<-vif(modeltot3_cy_nc)%>% max() %>% round(4)
vif_modeltot4_cy_nc<-vif(modeltot4_cy_nc)%>% max() %>% round(4)

stargazer(modeltot1_cy_nc, modeltot2_cy_nc, modeltot3_cy_nc,modeltot4_cy_nc, type="html", out="probitcyanidenocapital.html", 
          notes=paste("Null: Loglik ", round(logLik(nullmodel_cy_nc), 3), "; AIC ", round(AIC(nullmodel_cy_nc), 3), sep=""), 
          add.lines = list(c("Type of centrality score", "ndegree", "betweenness", "eigenvector","closeness"), 
                           c("Max VIF", vif_modeltot1_cy_nc, vif_modeltot2_cy_nc, vif_modeltot3_cy_nc, vif_modeltot4_cy_nc), 
                           c("McFadden R2", round(pseudo_modeltot1_cy_nc,4), 
                             round(pseudo_modeltot2_cy_nc,4), round(pseudo_modeltot3_cy_nc,4), round(pseudo_modeltot4_cy_nc,4))
          )
)


###### 5.2.2 Model flotation #####
firmsfl_nc<-firms %>% subset(tipo=="concentration", select=-c(loc, lat, lon)) %>% subset(year >= 1900)
adoptionf<-read_csv("smoothed_fl.csv") %>% rename(proportion=loess_tot) %>%
  select(year, proportion)
adoptionf$adoptionrate<-lag(adoptionf$proportion)
adoptionf$adoptionrate[1]<-0
firmsfl_nc<-firmsfl_nc %>% left_join(adoptionf)
firmsfl_nc$totalp<-firmsfl_nc$n
firmsfl_nc$prob<-ifelse(firmsfl_nc$technology=="flotation", 1, 0)
firmsfl_nc$district<-ifelse(firmsfl_nc$dist<65, 1, 0)
firmsfl_nc<-firmsfl_nc %>% 
  rename(prob_adoption=prob)
nullmodel_fl_nc<-glm(prob_adoption~1, data=firmsfl_nc, family=binomial(link="probit"))
modeltot0_fl_nc<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"),
                data=firmsfl_nc %>% 
                  mutate(tacit=tech_degree,codified=prof_degree))
summary(modeltot0_fl_nc)
pseudo_modeltot0_fl_nc<-as.numeric(1-logLik(modeltot0_fl_nc)/logLik(update(modeltot0_fl_nc, .~1)))

modeltot1_fl_nc<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmsfl_nc %>% 
                  mutate(codified=prof_ndegree,
                         tacit=tech_ndegree))
summary(modeltot1_fl_nc)
pseudo_modeltot1_fl_nc<-as.numeric(1-logLik(modeltot1_fl_nc)/logLik(update(modeltot1_fl_nc, .~1)))

modeltot2_fl_nc<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
               family=binomial(link="probit"), 
               data=firmsfl_nc %>% 
                 mutate(codified=prof_between,
                        tacit=tech_between))
summary(modeltot2_fl_nc)
pseudo_modeltot2_fl_nc<-as.numeric(1-logLik(modeltot2_fl_nc)/logLik(update(modeltot2_fl_nc, .~1)))

modeltot3_fl_nc<-glm(prob_adoption~ codified+tacit+
                 adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
               family=binomial(link="probit"), 
               data=firmsfl_nc %>% 
                 mutate(codified=prof_eigen,
                        tacit=tech_eigen))
summary(modeltot3_fl_nc)
pseudo_modeltot3_fl_nc<-as.numeric(1-logLik(modeltot3_fl_nc)/logLik(update(modeltot3_fl_nc, .~1)))


modeltot4_fl_nc<- glm(prob_adoption~ codified+tacit+
                  adoptionrate+district+mult+v2x_rule+v2xcl_prpty+empire, 
                family=binomial(link="probit"), 
                data=firmsfl_nc %>% 
                  mutate(codified=prof_close,
                         tacit=tech_close))
summary(modeltot4_fl_nc)
pseudo_modeltot4_fl_nc<-as.numeric(1-logLik(modeltot4_fl_nc)/logLik(update(modeltot4_fl_nc, .~1)))


vif_modeltot0_fl_nc<-vif(modeltot0_fl_nc) %>% max() %>% round(4)
vif_modeltot1_fl_nc<-vif(modeltot1_fl_nc)%>% max() %>% round(4)
vif_modeltot2_fl_nc<-vif(modeltot2_fl_nc)%>% max() %>% round(4)
vif_modeltot3_fl_nc<-vif(modeltot3_fl_nc)%>% max() %>% round(4)
vif_modeltot4_fl_nc<-vif(modeltot4_fl_nc)%>% max() %>% round(4)

stargazer(modeltot1_fl_nc, modeltot2_fl_nc, modeltot3_fl_nc,modeltot4_fl_nc, type="html", out="probitflotationnocapital.html", 
          notes=paste("Null: Loglik ", round(logLik(nullmodel_fl_nc), 3), "; AIC ", round(AIC(nullmodel_fl_nc), 3), sep=""), 
          add.lines = list(c("Type of centrality score", "ndegree", "betweenness", "eigenvector","closeness"), 
                           c("Max VIF", vif_modeltot1_fl_nc, vif_modeltot2_fl_nc, vif_modeltot3_fl_nc, vif_modeltot4_fl_nc), 
                           c("McFadden R2", round(pseudo_modeltot1_fl_nc,4), 
                             round(pseudo_modeltot2_fl_nc,4), round(pseudo_modeltot3_fl_nc,4), round(pseudo_modeltot4_fl_nc,4))
          )
)

write_csv(firms, "firmsfinalnocapital.csv")


##### 5.3 Robustness check ####
###### 5.3.1 Checking capacities #####
firms<-read_csv("firmsfinal.csv")
together<-read_csv("togethert.csv")
together<- together %>% subset(!is.na(together$simpname))%>%
  arrange(simpname, year)
profs<-read_csv("combot.csv") %>% subset(str_detect(type, "alumni|prof")&year>1850) %>%
  arrange(simpname, year)
profs$edu<-ifelse(is.na(profs$edu), "mining eng", profs$edu)
profs<-profs %>% subset(select=c(year, simpname, firstname, type)) %>%
  rename(firstname0=firstname)
prof2<-profs %>% subset(select=-type) %>% distinct() %>% mutate(type="engineer")
profs<-rbind(profs, prof2)
toge<-together %>% subset(select=c(year, company, simpname, firstname)) %>% 
  distinct() %>%
  left_join(profs, relationship = "many-to-many") %>% 
  filter(!is.na(type)) %>% distinct() #%>%
toge$dum<-comparecolumns(toge$firstname, toge$firstname0)
toge<- toge %>%  subset(dum==TRUE, select=-c(dum, firstname0)) %>%  distinct()
toge<-toge %>% count(year, company, type) %>% 
  pivot_wider(names_from=type, values_from=n, values_fill=0) 
firms2<-firms %>% 
  filter(n>0) %>%
  select(year, company, technology, capital, mult, exp, expprof, 
         n, district, tipo, empire, commonw, v2x_rule, v2xcl_prpty, prob_adoption , adoptionrate) %>%
  left_join(toge) %>% 
  mutate(eng=ifelse(is.na(engineer), 0, engineer), 
         prof=ifelse(is.na(professional), 0, professional), 
         alumni=ifelse(is.na(alumni), 0, alumni)) 

###### 5.3.2 Model based on capacity #####
firms2fl<-firms2 %>% subset(tipo=="concentration")
firms2cy<-firms2 %>% subset(tipo!="concentration")
nullmodel_cap_fl<-glm(prob_adoption~1, data=firms2fl, family=binomial(link="probit"))
nullaic_cap_fl<-AIC(nullmodel_cap_fl)
model_cap_fl_1<-glm(prob_adoption~ pers_codified+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2fl%>%
              mutate(pers_codified=eng, pers_tacit=exp, expeng=expprof))
pseudo_cap_fl_1<-as.numeric(1-logLik(model_cap_fl_1)/logLik(update(model_cap_fl_1, .~1)))

model_cap_fl_2<-glm(prob_adoption~ associative+educational+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2fl%>%
              mutate(pers_tacit=exp,associative=prof, educational=alumni))
pseudo_cap_fl_2<-as.numeric(1-logLik(model_cap_fl_2)/logLik(update(model_cap_fl_2, .~1)))

model_cap_fl_3<-glm(prob_adoption~ pers_codified+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2fl%>%
              mutate(pers_codified=eng/n, pers_tacit=exp/n, expeng=expprof/n))
pseudo_cap_fl_3<-as.numeric(1-logLik(model_cap_fl_3)/logLik(update(model_cap_fl_3, .~1)))

model_cap_fl_4<-glm(prob_adoption~ associative+educational+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2fl%>%
              mutate(pers_tacit=exp/n,associative=prof/n, educational=alumni/n, exp=exp/n))
pseudo_cap_fl_4<-as.numeric(1-logLik(model_cap_fl_4)/logLik(update(model_cap_fl_4, .~1)))

vif_cap_fl_1<-vif(model_cap_fl_1) %>% max() %>% round(4)
vif_cap_fl_2<-vif(model_cap_fl_2) %>% max() %>% round(4)
vif_cap_fl_3<-vif(model_cap_fl_3) %>% max() %>% round(4)
vif_cap_fl_4<-vif(model_cap_fl_4) %>% max() %>% round(4)

stargazer(model_cap_fl_1, model_cap_fl_3, model_cap_fl_2, model_cap_fl_4, type="html", out="capacityfl.html", 
           notes=paste("Null: Loglik ", round(logLik(nullmodel_cap_fl), 3), "; AIC ", round(AIC(nullmodel_cap_fl), 3), sep=""), 
          add.lines = list(c("Share upper level personnel","absolute", "relative", "absolute", "relative"),
                           c("Max VIF", vif_cap_fl_1, vif_cap_fl_3, vif_cap_fl_2, vif_cap_fl_4),
            c("McFadden R2", round(pseudo_cap_fl_1,4), round(pseudo_cap_fl_3,4), round(pseudo_cap_fl_2,4), round(pseudo_cap_fl_4,4))))

nullmodel_cap_cy<-glm(prob_adoption~1, data=firms2cy, family=binomial(link="probit"))
nullaic_cap_cy<-AIC(nullmodel_cap_cy)
model_cap_cy_1<-glm(prob_adoption~ pers_codified+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2cy%>%
              mutate(pers_codified=eng, pers_tacit=exp, expeng=expprof))
pseudo_cap_cy_1<-as.numeric(1-logLik(model_cap_cy_1)/logLik(update(model_cap_cy_1, .~1)))

model_cap_cy_2<-glm(prob_adoption~ associative+educational+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2cy%>%
              mutate(pers_tacit=exp,associative=prof, educational=alumni))
pseudo_cap_cy_2<-as.numeric(1-logLik(model_cap_cy_2)/logLik(update(model_cap_cy_2, .~1)))

model_cap_cy_3<-glm(prob_adoption~ pers_codified+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2cy%>%
              mutate(pers_codified=eng/n, pers_tacit=exp/n, expeng=expprof/n))
pseudo_cap_cy_3<-as.numeric(1-logLik(model_cap_cy_3)/logLik(update(model_cap_cy_3, .~1)))

model_cap_cy_4<-glm(prob_adoption~ associative+educational+pers_tacit+
              adoptionrate+capital+district+
              mult+v2x_rule+v2xcl_prpty+empire,
            family=binomial(link="probit"), 
            data=firms2cy%>%
              mutate(pers_tacit=exp/n,associative=prof/n, educational=alumni/n, exp=exp/n))
pseudo_cap_cy_4<-as.numeric(1-logLik(model_cap_cy_4)/logLik(update(model_cap_cy_4, .~1)))

vif_cap_cy_1<-vif(model_cap_cy_1) %>% max() %>% round(4)
vif_cap_cy_2<-vif(model_cap_cy_2) %>% max() %>% round(4)
vif_cap_cy_3<-vif(model_cap_cy_3) %>% max() %>% round(4)
vif_cap_cy_4<-vif(model_cap_cy_4) %>% max() %>% round(4)

stargazer(model_cap_cy_1, model_cap_cy_3, model_cap_cy_2, model_cap_cy_4, type="html", out="capacitycyan.html", 
          notes=paste("Null: Loglik ", round(logLik(nullmodel_cap_cy), 3), "; AIC ", round(AIC(nullmodel_cap_cy), 3), sep=""), 
          add.lines = list(c("Share upper level personnel","absolute", "relative", "absolute", "relative"),
                           c("Max VIF", vif_cap_cy_1, vif_cap_cy_3, vif_cap_cy_2, vif_cap_cy_4),
                           c("McFadden R2", round(pseudo_cap_cy_1,4), round(pseudo_cap_cy_3,4), round(pseudo_cap_cy_2,4), round(pseudo_cap_cy_4,4))))




#### 6. Plots ####
base<- getMap()
base<- ggplot() + 
  geom_polygon(data = base, aes(x=long, y = lat, group = group),  fill = "white", color="lightgray")



firmsadopt<-read_csv("crptechno3.csv") %>% subset(str_detect(technology, "flot|cyan"))
antescy<-firmsadopt %>% subset(year<1895)
despuescy <-firmsadopt %>% subset(year>1919)

jpeg("antes.jpg", width=600, height=400, quality=100)
base+
  geom_point(data=antes, aes(x=lon, y=lat, color=technology), alpha=0.5) +
  theme_grey()+
  labs(title="Adoption of flotation and cyanidation before 1895", x="Longitude", y="Latitude")
dev.off()
jpeg("despues.jpg", width=600, height=400, quality=100)
base+
  geom_point(data=despues, aes(x=lon, y=lat, color=technology), alpha=0.5) +
  theme_grey()+
  labs(title="Adoption of flotation and cyanidation after 1919", x="Longitude", y="Latitude")
dev.off()

firmscy<-read_csv("firmsfinal.csv") %>% subset(str_detect(technology, "cyan|amalgama"))
firmscy$adopt<-ifelse(firmscy$technology=="cyanid", 1, 0)
firmscy<- firmscy %>% group_by(company) %>% 
  mutate(min=min(adopt), max=max(adopt)) %>% 
  mutate(yearadopt=ifelse(adopt==1, year, NA)) %>% 
  mutate(time=ifelse(min==max, 0, year-min(yearadopt, na.rm=T))) 
firmsfl<-read_csv("firmsfinal.csv") %>% subset(str_detect(technology, "flot|gravit"))
firmsfl$adopt<-ifelse(firmsfl$technology=="flotation", 1, 0)
firmsfl<- firmsfl %>% group_by(company) %>% 
  mutate(min=min(adopt), max=max(adopt)) %>% 
  mutate(yearadopt=ifelse(adopt==1, year, NA)) %>% 
  mutate(time=ifelse(min==max, 0, year-min(yearadopt, na.rm=T))) 
general<-rbind(firmscy, firmsfl)

between<-general %>%
  subset(time>-4&time<4, select=-c(yearadopt))%>% 
  group_by(time) %>% 
  summarize(experiential_betweenness=mean(tech_between, na.rm=T),
            educational_betweenness=mean(univ_between, na.rm=T),
            associative_betweenness=mean(assoc_between, na.rm=T), 
            professional_betweenness=mean(prof_between, na.rm=T),
            experiential_ndegree=mean(tech_ndegree, na.rm=T),
            educational_ndegree=mean(univ_ndegree, na.rm=T),
            associative_ndegree=mean(assoc_ndegree, na.rm=T), 
            professional_ndegree=mean(prof_ndegree, na.rm=T),
            educational_closeness=mean(univ_close, na.rm=T),
            experiential_closeness=mean(tech_close, na.rm=T),
            associative_closeness=mean(assoc_close, na.rm=T),
            professional_closeness=mean(prof_close, na.rm=T),
            educational_eigenvector=mean(univ_eigen, na.rm=T),
            experiential_eigenvector=mean(tech_eigen, na.rm=T),
            associative_eigenvector=mean(assoc_eigen, na.rm=T), 
            professional_eigenvector=mean(prof_eigen, na.rm=T)
  ) %>%
  pivot_longer(cols=-time, names_to=c("type", "metric"), names_sep = '_', values_to="between")
between$group<-ifelse(between$time>0, 1, 0)

betweency<-firmscy %>%
  subset(time>-5&time<5, select=-c(yearadopt))%>% 
  group_by(time) %>% 
  summarize(experiential_betweenness=mean(tech_between, na.rm=T),
            educational_betweenness=mean(univ_between, na.rm=T),
            associative_betweenness=mean(assoc_between, na.rm=T), 
            professional_betweenness=mean(prof_between, na.rm=T),
            experiential_ndegree=mean(tech_ndegree, na.rm=T),
            educational_ndegree=mean(univ_ndegree, na.rm=T),
            associative_ndegree=mean(assoc_ndegree, na.rm=T), 
            professional_ndegree=mean(prof_ndegree, na.rm=T),
            educational_closeness=mean(univ_close, na.rm=T),
            experiential_closeness=mean(tech_close, na.rm=T),
            associative_closeness=mean(assoc_close, na.rm=T),
            professional_closeness=mean(prof_close, na.rm=T),
            educational_eigenvector=mean(univ_eigen, na.rm=T),
            experiential_eigenvector=mean(tech_eigen, na.rm=T),
            associative_eigenvector=mean(assoc_eigen, na.rm=T), 
            professional_eigenvector=mean(prof_eigen, na.rm=T)
  ) %>%
  pivot_longer(cols=-time, names_to=c("type", "metric"), names_sep = '_', values_to="between")
betweency$group<-ifelse(betweency$time>0, 1, 0)

betweenfl<-firmsfl %>%
  subset(time>-5&time<5, select=-c(yearadopt))%>% 
  group_by(time) %>% 
  summarize(experiential_betweenness=mean(tech_between, na.rm=T),
            educational_betweenness=mean(univ_between, na.rm=T),
            associative_betweenness=mean(assoc_between, na.rm=T), 
            professional_betweenness=mean(prof_between, na.rm=T),
            experiential_ndegree=mean(tech_ndegree, na.rm=T),
            educational_ndegree=mean(univ_ndegree, na.rm=T),
            associative_ndegree=mean(assoc_ndegree, na.rm=T), 
            professional_ndegree=mean(prof_ndegree, na.rm=T),
            educational_closeness=mean(univ_close, na.rm=T),
            experiential_closeness=mean(tech_close, na.rm=T),
            associative_closeness=mean(assoc_close, na.rm=T),
            professional_closeness=mean(prof_close, na.rm=T),
            educational_eigenvector=mean(univ_eigen, na.rm=T),
            experiential_eigenvector=mean(tech_eigen, na.rm=T),
            associative_eigenvector=mean(assoc_eigen, na.rm=T), 
            professional_eigenvector=mean(prof_eigen, na.rm=T)
  ) %>%
  pivot_longer(cols=-time, names_to=c("type", "metric"), names_sep = '_', values_to="between")
  
betweenfl$group<-ifelse(betweenfl$time>0, 1, 0)

betweenfl %>% subset(str_detect(type, "experiential|professional")) %>%
  mutate(type=ifelse(type=="experiential", "Tacit", 
                     ifelse(type=="professional", "Codified",NA))) %>%
  ggplot(aes(x=time, y=between, linetype=type, shape=type))+
  geom_point()+ geom_smooth(method="lm",se=F)+
  facet_wrap(~factor(metric, c("ndegree", "betweenness","closeness", "eigenvector")), scales="free_y")+
  theme_minimal()+
  labs(title="Centrality scores over time", x="Years from adoption", y="Centrality score", 
       shape="Projected network", linetype="Projected network")
betweency %>% subset(str_detect(type, "experiential|professional")) %>%
  mutate(type=ifelse(type=="experiential", "Tacit", 
                     ifelse(type=="professional", "Codified",NA))) %>%
  ggplot(aes(x=time, y=between, linetype=type, shape=type))+
  geom_point()+ geom_smooth(method="lm",se=F)+
  facet_wrap(~factor(metric, c("ndegree", "betweenness","closeness", "eigenvector")), scales="free_y")+
  theme_minimal()+
  labs(title="Centrality scores over time", x="Years from adoption", y="Centrality score", 
       shape="Projected network", linetype="Projected network")
between %>% subset(str_detect(type, "experiential|professional")) %>%
  mutate(type=ifelse(type=="experiential", "Tacit", 
                     ifelse(type=="professional", "Codified",NA))) %>%
  ggplot(aes(x=time, y=between, linetype=type, shape=type))+
  geom_point()+ geom_smooth(se=F, method="lm")+
  facet_wrap(~factor(metric, c("ndegree", "betweenness","closeness", "eigenvector")), scales="free_y")+
  theme_minimal()+
  labs(title="Centrality scores over time", x="Years from adoption", y="Centrality score", 
       shape="Projected network", linetype="Projected network")



jpeg("centralidadfl.jpg", width=600, height=400, quality=300)
betweenfl %>% subset(str_detect(type, "experiential|professional")) %>%
  mutate(type=ifelse(type=="experiential", "Tacit", 
                       ifelse(type=="professional", "Codified",NA))) %>%
  ggplot(aes(x=time, y=between, linetype=type, shape=type))+
  geom_point()+ geom_smooth(method="lm",se=F)+
  facet_wrap(~factor(metric, c("ndegree", "betweenness","closeness", "eigenvector")), scales="free_y")+
  theme_minimal()+
  labs(title="Centrality scores over time", x="Years from adoption", y="Centrality score", 
       shape="Projected network", linetype="Projected network")
dev.off()
jpeg("centralidadcy.jpg", width=600, height=400, quality=300)
betweency %>% subset(str_detect(type, "experiential|professional")) %>%
  mutate(type=ifelse(type=="experiential", "Tacit", 
                     ifelse(type=="professional", "Codified",NA))) %>%
  ggplot(aes(x=time, y=between, linetype=type, shape=type))+
  geom_point()+ geom_smooth(method="lm",se=F)+
  facet_wrap(~factor(metric, c("ndegree", "betweenness","closeness", "eigenvector")), scales="free_y")+
  theme_minimal()+
  labs(title="Centrality scores over time", x="Years from adoption", y="Centrality score", 
       shape="Projected network", linetype="Projected network")
dev.off()
betweenfl %>% subset(str_detect(type, "experiential|professional")) %>%
  mutate(type=ifelse(type=="experiential", "Tacit", 
                     ifelse(type=="professional", "Codified",NA))) %>%
  ggplot(aes(x=time, y=between, linetype=type, shape=type))+
  geom_point()+ geom_smooth(method="lm", se=F)+
  facet_wrap(~factor(metric, c("ndegree", "betweenness","closeness", "eigenvector")), scales="free_y")+
  theme_minimal()+
  labs(title="Centrality scores over time", x="Years from adoption", y="Centrality score", 
       shape="Projected network", linetype="Projected network")
