# -----------------------------------------------------------------------------------------------------------------------
# Codes and data for replicating the study :
# "The impact of COVID-19 vaccines on the Case Fatality Rate:
#  The importance of monitoring breakthrough infections", by Vanessa di Lego, Miguel Sánchez-Romero and
#  Alexia Prskawetz. International Journal of Infectious Diseases.
#  https://doi.org/10.1016/j.ijid.2022.03.059
#
# Note:
# All of the data used in this paper refers to data available until Dec.2021, before
# the Omicron wave. In addition, any changes to format or retrospective corrections to
# data are only valid until 25.04.2022, which means that any changes after this will not be reflected
# in the main paper; please take that into consideration when interpreting results and running
# updated codes or performing new downloads.
# Also, the format of the data in both The Our World in Data and the Austrian website are changing
# continuously. Check for consistencies in the information, columns and others.
# The data used in the paper is the one currently saved in this directory.
#
# All data is available online and retrieved from:
# 1. Our World in Data (Mathieu et al., 2021)
# 2. Federal Ministry for Social Affairs, Health, Care and Consumer Protection (BMSGPK),
#    Österreichisches COVID-19 Open Data Informationsportal 2021.
# 3. Richter, L., Schmid, D., & Stadlober, E. (2020). Methodenbeschreibung für die Schätzung
#   von epidemiologischen Parametern des COVID19 Ausbruchs, Österreich.
# 4. AGES- Österreichische Agentur für Gesundheit und Ernährungssicherheit.
# https://www.ages.at/en/wissen-aktuell/publikationen/epidemiologische-parameter-des-covid19-ausbruchs-oesterreich-2020/
#------------------------------------------------------------------------------------------------------------------------

# Load necessary packages

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(viridis)
library(ggpubr)
library(data.table)
library(here)
library(gdata)
library(httr)
library(readxl)
library(tidyverse)
library(reshape2)
library(magrittr)
library(countrycode)
library(vroom)
library(ggExtra)
library(ggthemes)
library(ggrepel)
library(Rcpp)
library(suffrager)
library(grid)
library(cowplot)


# creating directory folders where outputs are saved

cfr.folder  <- here("Data","CFR")
vax.folder <- here("Data","Vaccination")
sim.folder <- here("Data","Simulation")
figs.folder <- here("Figures")

# make in-out directories

dir.create(cfr.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(vax.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(sim.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------------------------------------
# Bulk download from the site Our World in Data for CFR and vaccination globally. This is the
# data used for Figure 1 in the main paper. We have saved the data used in the paper in the
# directories. For updates, uncomment the following codes that download the current data into
# the folders. All of the data used in this paper refers to data available until Dec.2021, before
# the Omicron wave hit. In addition, any changes to format or retrospective corrections to
# data are only valid until 25.04.2022, which means that any changes after this will not be reflected
# in the main paper; please take that into consideration when interpreting results and running
# updated codes or performing new downloads.
#
# Also, the format of the data in both The Our World in Data and the Austrian website are changing
# continuously. Check for consistencies in the information, columns and others.
# The data used in the paper is the one currently saved in this directory.
# ----------------------------------------------------------------------------------------------------


# CFR data
# download.file(url = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
#              destfile = file.path(cfr.folder, "cfr.csv"), mode="wb")

# Vaccination data
# download.file(url = "https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv",
#              destfile = file.path(vax.folder, "vax.csv"), mode="wb")



# Combine data and later select the first vaccination dates and CFR

cfr<-fread(here("Data","CFR", "cfr.csv")) %>%
  select(1:5,8, 45) %>%
  mutate(CFR=(total_deaths/total_cases)*100,
         date=as.Date(date)) %>%
  drop_na()

countries<-c("Israel","Italy","Austria","Brazil",
             "United States","United Kingdom", "Malta")

cfr_c<-cfr %>%
  filter(location%in% countries) %>%
  arrange(location, date) %>%
  filter(date>"2021-01-31" & date<"2022-01-02" )


# vaccination data

vax<-fread(here("Data","Vaccination", "vax.csv")) %>%
  mutate(date=as.Date(date)) %>%
  filter(location%in%countries) %>%
  arrange(location, date) %>%
  filter(date>"2021-01-31"& date<"2022-01-02")


vax_join<-full_join(cfr_c,vax, by=c("date","location","iso_code")) %>%
  mutate(`%Vaccinated`= people_fully_vaccinated_per_hundred) %>%
  arrange(location, date)


vax_join<-vax_join %>%
  select(1:8,22) %>%
  drop_na()

# plots for cfr and vaccination

# cfr for all the period

cfr_all<-fread(here("Data","CFR", "cfr.csv")) %>%
  select(1:5,8, 37) %>%
  mutate(CFR=(total_deaths/total_cases)*100,
         date=as.Date(date))# %>%


countries<-c("Israel","Italy","Austria","Brazil",
             "United States","United Kingdom", "Malta")


cfr_all_c<-cfr_all %>%
  filter(location%in% countries) %>%
  arrange(location, date) %>%
  filter(date>"2020-04-01" & date<"2022-01-02" ) %>%
  select(-7) %>%
  drop_na()


min.date<-as.Date("2020-12-13")
max.date<-as.Date('2021-12-31')

cfr_all_plot<-ggplot(cfr_all_c %>% group_by(location) %>%
                       filter(date<max(date-20)),
                     aes(date,CFR,group=location,color=location))+
  geom_line(size=2.6)+
  scale_y_continuous(breaks = seq(0, 18, by = 1),limits=c(-0.5, 19))+
  scale_color_manual(values = suf_palette("hanwell", n = 7,type = "continuous"))+
  # the theme used in the main paper is theme_pander. if you run into any issues with this
  # theme, use the current code. If you can run theme_pander in your computer, just uncomment
  # below for reproducing the same style figure.
  # theme_pander(base_size = 16)+
  # comment here if you want to use theme_pander, otherwise this new call would override the previous one
  theme_minimal()+
  scale_x_date(date_minor_breaks = "1 month",
               date_labels = "%b",
               date_breaks = "1 month",
               limits = as.Date(c('2020-04-01','2021-12-31')))+
  theme(legend.position = "none",axis.title.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y = "%CFR\n(Deaths/Reported Infected)")+
  geom_label_repel(aes(x=date, y=CFR,group=location, colour = location,label = location),
                   data=cfr_all_c %>% filter(date=="2020-06-30"),
                   hjust = "right",
                   fontface = "bold",
                   size = 8,
                   nudge_y = 1.5,
                   nudge_x = 4.5,
                   direction= "x")+
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 0, ymax=18.7,
           alpha = .05, fill="red")+
  geom_vline(xintercept = min.date, color="forestgreen", linetype="dashed", size=1.3)+

  annotate(geom="text", x=min.date+120, y=19,size=8, label="After vaccination starts in countries*")+
  annotate(geom="text", x=min.date-150, y=-0.5,size=8, label="2020")+
  annotate(geom="text", x=min.date+200, y=-0.5,size=8, label="2021")


# vaccination plot

vax_g<-ggplot(vax_join %>%
                group_by(location) %>%
                filter(date<max(date-20)),
              aes(date,`%Vaccinated`,group=location,
                  color=location))+
  geom_line(size=2.6)+
  scale_y_continuous(breaks = seq(0, 90, by = 10), limits = c(0,90))+
  scale_color_manual(values = suf_palette("hanwell", n = 7,type = "continuous"))+
  # the theme used in the main paper is theme_pander. if you run into any issues with this
  # theme, use the current code. If you can run theme_pander in your computer, just uncomment
  # below for reproducing the same style figure.
  # theme_pander(base_size=25)+
  # comment here if you want to use theme_pander, otherwise this new call would override the previous one
  theme_minimal()+
  scale_x_date(date_minor_breaks = "1 month", date_labels = "%b",
               date_breaks = "1 month")+
  theme(legend.position = "none",axis.title.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y = "%Vaccinated")


# connecting cfr and vax and ploting one single high resolution .png figure

png(here(figs.folder,"Fig1_25042022.png"),"Fig1_25042022.png", units="in", width=24, height=12, res=400)
ggdraw() +
  draw_plot(cfr_all_plot) +
  draw_plot(vax_g, x = 0.46, y = .28, width = 0.48, height = 0.65)
dev.off()


# ----------------------------------------------------------------------------------------------
# bulk download from  site, combine data and later select the first vaccination dates and CFR
# Richter, L., Schmid, D., & Stadlober, E. (2020). Methodenbeschreibung für die Schätzung
# von epidemiologischen Parametern des COVID19 Ausbruchs, Österreich.
# AGES- Österreichische Agentur für Gesundheit und Ernährungssicherheit.
# https://www.ages.at/en/wissen-aktuell/publikationen/epidemiologische-parameter-des-covid19-ausbruchs-oesterreich-2020/
# ------------------------------------------------------------------------------------------------

#download.file(url="https://www.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff.csv",
#                   destfile = file.path(sim.folder, "Reff.csv"), mode="wb")

Step0<-fread(here("Data","Simulation", "Reff.csv")) %>%
  mutate(Date = ymd(Datum),
         R_eff=gsub("\\,", ".", R_eff),
         R_eff=as.numeric(R_eff)) %>%
  subset(selec=c("Date","R_eff")) %>%
  as.data.frame()

# Times series for the incidence rate

# Cite: COVID-19: Timeline of data on Covid19 cases per province  from: BMSGPK, Österreichisches COVID-19 Open Data Informationsportal (https://www.data.gv.at/covid-19)

#download.file(url="https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv",
#              destfile = file.path(cfr.folder, "I.csv"), mode="wb")

InterStep1<-fread(here("Data","CFR", "I.csv")) %>%
  subset(BundeslandID=="10",select=c("Time","AnzEinwohner","AnzahlFaelle")) %>%
  mutate(Incidence=AnzahlFaelle/AnzEinwohner) %>%
  mutate(Date=dmy(substr(Time, 1, 10))) %>%
  subset(select=c("Date","AnzahlFaelle","Incidence")) %>%
  as.data.frame()

Step1<-merge(Step0,InterStep1,by=c("Date"))

# This code generates a long format database with the proportion of individuals vaccinated in each group
# Cite: COVID-19 Schutzimpfungen - Eingetragene Impfungen im e-Impfpass, BMSGPK, Österreichisches COVID-19 Open Data Informationsportal (https://www.data.gv.at/covid-19)

#download.file(url="https://info.gesundheitsministerium.gv.at/data/timeline-eimpfpass.csv",
#              destfile = file.path(vax.folder, "impfpass.csv"), mode="wb")

InterStep2<-fread(here("Data","Vaccination", "impfpass.csv")) %>%
  subset(BundeslandID=="10") %>%
  #select(contains(c("ï..Datum","BundeslandID","Name","_2"))) %>% # Uncomment this script in case that you have problems with the special characters
  select(contains(c("Datum","BundeslandID","Name","_2"))) %>%
  mutate(
    #Date   =ymd(substr(ï..Datum, 1, 10)), # Uncomment this script in case that you have problems with the special characters
    Date   =ymd(substr(Datum, 1, 10)),
    "24<"  =rowSums(select(.,contains("Gruppe_<15"))),
    "24<"  =rowSums(select(.,contains("Gruppe_15-24"))),
    "25_34"=rowSums(select(.,contains("Gruppe_25-34"))),
    "35_44"=rowSums(select(.,contains("Gruppe_35-44"))),
    "45_54"=rowSums(select(.,contains("Gruppe_45-54"))),
    "55_64"=rowSums(select(.,contains("Gruppe_55-64"))),
    "65_74"=rowSums(select(.,contains("Gruppe_65-74"))),
    "75_84"=rowSums(select(.,contains("Gruppe_75-84"))),
    "84>"  =rowSums(select(.,contains("Gruppe_>84"))),
  ) %>%
  select(c("Date","24<":"84>")) %>%
  gather(Group, Impfungenend, "24<":"84>", factor_key=TRUE) %>%
  subset(Group=="84>") %>%
  group_by(Date) %>%
  as.data.frame()

# Meging Vaccination data with times series for the incidence rate

Step2<-merge(Step1,InterStep2,  by=c("Date"))

#-----------------------------------------------------------------------------------------#
# Downloading time series on deaths and ever infected and detected by age group
#----------------------------------------------------------------------------------------#

#download.file(url="https://covid19-dashboard.ages.at/data/CovidFaelle_Altersgruppe.csv",
#              destfile = file.path(cfr.folder, "deaths_age.csv"), mode="wb")

# These are the codes for the Regions in Austria. We select code 10 because it refers
# to total Austria

Bund<-c("1","2","3","4","5","6","7","8","9","10")
BundN<-c("Burgenland",
         "Karten",
         "Niederoesterreich",
         "Oberosterreich",
         "Salzburg",
         "Steiermark",
         "Tirol",
         "Vorarlberg",
         "Wien",
         "Oesterreich")

M<-fread(here("Data","CFR", "deaths_age.csv")) %>%
  subset(BundeslandID==Bund[10]) %>%
  mutate(
    Date=dmy(substr(Time, 1, 10)),
    Group=ifelse(AltersgruppeID<4,"24<",
                 ifelse(AltersgruppeID<5,"25_34",
                        ifelse(AltersgruppeID<6,"35_44",
                               ifelse(AltersgruppeID<7,"45_54",
                                      ifelse(AltersgruppeID<8,"55_64",
                                             ifelse(AltersgruppeID<9,"65_74",
                                                    ifelse(AltersgruppeID<10,"75_84","84>")))))))
  ) %>%
  filter(Date>dmy("31.12.2020")) %>%
  group_by(Date,Group) %>%
  summarize(
    AnzEinwohner=sum(AnzEinwohner),
    Anzahl       =sum(Anzahl),
    AnzahlGeheilt=sum(AnzahlGeheilt),
    AnzahlTot    =sum(AnzahlTot)) %>%
  as.data.frame()

df1<-M %>%
  subset(Group=="84>") %>%
  data.frame()

# Computing the CFR by date as the total number of reported deaths divided by the
# total number of reported deaths and recovered.

InterStep3<-df1 %>%
  subset(select=c("Date","Anzahl","AnzahlTot","AnzahlGeheilt")) %>%
  mutate(Date=ymd(Date), CFR=AnzahlTot/(AnzahlTot+AnzahlGeheilt)) %>%
  as.data.frame()

# Combining all datasets

Step3<-merge(Step2,InterStep3,  by=c("Date"))

# ------------------------------------------------------------------------------------------
# Ploting Figure 2, with CFRs only for Austria, older age groups
# ------------------------------------------------------------------------------------------

DF<-fread(here("Data","Vaccination", "impfpass.csv")) %>%
  subset(BundeslandID=="10") %>%
  #select(contains(c("ï..Datum","BundeslandID","Name","_2"))) %>% # Uncomment this script in case that you have problems with the special characters
  select(contains(c("Datum","BundeslandID","Name","_2"))) %>%
  mutate(
    #Date   =ymd(substr(ï..Datum, 1, 10)), # Uncomment this script in case that you have problems with the special characters
    Date   =ymd(substr(Datum, 1, 10)),
    "24<"  =rowSums(select(.,contains("Gruppe_<15"))),
    "24<"  =rowSums(select(.,contains("Gruppe_15-24"))),
    "25_34"=rowSums(select(.,contains("Gruppe_25-34"))),
    "35_44"=rowSums(select(.,contains("Gruppe_35-44"))),
    "45_54"=rowSums(select(.,contains("Gruppe_45-54"))),
    "55_64"=rowSums(select(.,contains("Gruppe_55-64"))),
    "65_74"=rowSums(select(.,contains("Gruppe_65-74"))),
    "75_84"=rowSums(select(.,contains("Gruppe_75-84"))),
    "84>"  =rowSums(select(.,contains("Gruppe_>84"))),
  ) %>%
  select(c("Date","24<":"84>")) %>%
  gather(Group, Impfungenend, "24<":"84>", factor_key=TRUE) %>%
  # subset(Group=="84>") %>%
  group_by(Date) %>%
  as.data.frame()

DF.test<-merge(M,DF,  by=c("Date","Group"))

p1<-DF.test %>%
  filter(Group%in%c("65_74","75_84",	"84>")) %>%
  group_by(Date,Group) %>%
  mutate(CFR=AnzahlTot/(AnzahlTot+AnzahlGeheilt)) %>%
  ggplot(aes(x=Date,y=100*CFR,color=Group))+
  geom_line(size=3)+
  scale_x_date(date_minor_breaks = "2 month",
               date_labels = "%b",
               date_breaks = "2 month")+
  scale_color_manual(values=c("#98AAE0","#644C92","#EF122C"), name="Age Group",
                     labels = c("65-74","75-84",	"84+"))+
  scale_y_continuous(breaks = seq(0, 31, by = 5), limits = c(0,31))+
  labs(y="%CFR\n Deaths/(Deaths+Recovered)",tag="(A)")+
  # the theme used in the main paper is theme_pander. if you run into any issues with this
  # theme, use the current code. If you can run theme_pander in your computer, just uncomment
  # below for reproducing the same style figure.
  #  theme_pander(base_size = 25)+
  # comment here if you want to use theme_pander, otherwise this new call would override the previous one
  theme_minimal()+
  theme(axis.title.x = element_blank())+
  theme(plot.tag = element_text(size = 25),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


p2<-DF.test %>%
  filter(Group%in%c("65_74","75_84",	"84>")) %>%
  group_by(Date,Group) %>%
  mutate(Impf_Pct=100*Impfungenend/AnzEinwohner) %>%
  ggplot(aes(x=Date,y=Impf_Pct,color=Group))+
  geom_line(size=3)+
  scale_x_date(date_minor_breaks = "2 month",
               date_labels = "%b",
               date_breaks = "2 month")+
  #scale_color_viridis(discrete=TRUE)+
  scale_color_manual(values=c("#98AAE0","#644C92","#EF122C"), name="Age Group",
                     labels = c("65-74","75-84",	"84+"))+
  #ylim(0,90.0)+
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100))+
  #scale_y_continuous(labels = scales::percent,limits=c(0,1.0))+
  labs(y="%Vaccinated",tag="(B)")+
  #  theme_pander(base_size = 25)+
  theme_minimal()+
  theme(axis.title.x  = element_blank())+
  theme(plot.tag = element_text(size = 25),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


fig2<-ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
png(here(figs.folder,"Fig2_25042022.png"),"Fig2_25042022.png", units="in", width=18, height=8, res=300)
fig2
dev.off()

# ------------------------------------------------------------------------------------------
# Simulation and Figure 3
# ------------------------------------------------------------------------------------------

DATAB<-Step3 %>%
  mutate(InfectedV=R_eff*Incidence*Impfungenend) %>%
  mutate(CumInfV =cumsum(InfectedV)) %>%
  mutate(GammaRaw=CumInfV/Anzahl) %>%
  as.data.frame()

# Scenarios for when the ratio of detection between vaccinated and unvaccinated, called
# Z in the main paper.

Z1<-0.25
Z2<-0.50
Z3<-0.75

# Scenarios for different effectiveness of vaccines in preventing deaths, called
# beta in the main paper

BetaA<-1-0.25
BetaB<-1-0.50
BetaC<-1-0.75

#Based on Hal et al (2021) we assume an effectivity of vaccines preventing infections of 85%
# Ploting the panels A-C in Figure 3

# Panel A

pA<-DATAB %>%
  mutate(
    Gamma1=(1-0.85)*Z1*GammaRaw,
    Gamma2=(1-0.85)*Z2*GammaRaw,
    Gamma3=(1-0.85)*Z3*GammaRaw,
    CFR1  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaA/Z1)),
    CFR2  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaA/Z2)),
    CFR3  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaA/Z3))
  )%>%
  gather(CFR_Case, CFR_Data, c(CFR1:CFR3), factor_key=TRUE) %>%
  data.frame() %>%
  ggplot(aes(x=Date,y=100*CFR_Data,color=CFR_Case))+
  geom_line(aes(linetype=CFR_Case,group=CFR_Case,color=CFR_Case),size=3)+
  scale_x_date(date_minor_breaks = "2 month",
               date_labels = "%b",
               date_breaks = "2 month")+
  scale_color_manual(values=c("grey30",
                              "grey30","grey30"))+
  scale_linetype_manual(name="",values=c("solid","dotted","dashed"),labels = c("Z=0.25","Z=0.50","Z=0.75"))+
  labs(title=expression(paste("Case: ",beta,"=0.25"),size=12),y="%CFR, age group 84+", tag = "(A)")+
  ylim(27,29)+
  theme_minimal()+
  #  theme_pander(base_size = 15)+
  theme(legend.position = c(0.2, 0.25),
        plot.tag = element_text(size = 25),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  guides(color="none",linetype=guide_legend(override.aes=list(color=c("grey30",
                                                                      "grey30","grey30")),
                                            keywidth = 8, keyheight = 1))

# Panel B

pB<-DATAB %>%
  mutate(
    Gamma1=(1-0.85)*Z1*GammaRaw,
    Gamma2=(1-0.85)*Z2*GammaRaw,
    Gamma3=(1-0.85)*Z3*GammaRaw,
    CFR1  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaB/Z1)),
    CFR2  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaB/Z2)),
    CFR3  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaB/Z3))
  )%>%
  gather(CFR_Case, CFR_Data, c(CFR1:CFR3), factor_key=TRUE) %>%
  data.frame() %>%
  ggplot(aes(x=Date,y=100*CFR_Data,group=CFR_Case))+
  geom_line(aes(linetype=CFR_Case,color=CFR_Case),size=3)+
  scale_x_date(date_minor_breaks = "2 month",
               date_labels = "%b",
               date_breaks = "2 month")+
  scale_color_manual(values=c("grey30",
                              "grey30","grey30"))+
  scale_linetype_manual(name="",values=c("solid","dotted","dashed"),labels = c("Z=0.25","Z=0.50","Z=0.75"))+
  labs(title=expression(paste("Case: ",beta,"=0.50"),size=12),y="%CFR, age group 84+", tag = "(B)")+
  ylim(27,29)+
  #  theme_pander(base_size = 25)+
  theme_minimal()+
  theme(legend.position = c(0.2, 0.25),
        plot.tag = element_text(size = 25),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  guides(color="none",linetype=guide_legend(override.aes=list(color=c("grey30",
                                                                      "grey30","grey30"))),
         keywidth = 8, keyheight = 1)


#Panel C
pC<-DATAB %>%
  mutate(
    Gamma1=(1-0.85)*Z1*GammaRaw,
    Gamma2=(1-0.85)*Z2*GammaRaw,
    Gamma3=(1-0.85)*Z3*GammaRaw,
    CFR1  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaC/Z1)),
    CFR2  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaC/Z2)),
    CFR3  =mean(CFR[31:345])*((1-Gamma1)+Gamma1*(BetaC/Z3))
  )%>%
  gather(CFR_Case, CFR_Data, c(CFR1:CFR3), factor_key=TRUE) %>%
  data.frame() %>%
  ggplot(aes(x=Date,y=100*CFR_Data,group=CFR_Case))+
  geom_line(aes(linetype=CFR_Case,color=CFR_Case),size=3)+
  scale_x_date(date_minor_breaks = "2 month",
               date_labels = "%b",
               date_breaks = "2 month")+
  scale_color_manual(values=c("grey30",
                              "grey30","grey30"))+
  scale_linetype_manual(name="",values=c("solid","dotted","dashed"),labels = c("Z=0.25","Z=0.50","Z=0.75"))+
  labs(title=expression(paste("Case: ",beta,"=0.75"),size=12),y="%CFR, age group 84+",tag = "(C)")+
  ylim(27,29)+
  #theme_pander(base_size = 25)+
  theme_minimal()+
  theme(legend.position = c(0.2, 0.25),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        plot.tag = element_text(size = 25))+
  guides(color="none",
         linetype=guide_legend(override.aes=list(color=c("black",
                                                         "black","black"))),
         keywidth = 8, keyheight = 1)


# arranging the panels into one single plot and saving it as a high resolution .png

fig3<-ggarrange(pA, pB+ rremove("ylab"),
                pC+ rremove("ylab"), ncol=3,
                nrow=1, common.legend = TRUE, legend="bottom")

png(here(figs.folder,"Fig3_25042022.png"),"Fig3_25042022.png", units="in", width=20, height=8, res=1000)
fig3
dev.off()



