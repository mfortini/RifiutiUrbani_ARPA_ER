library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(data.table)
require(rgdal)
require(rgeos)
require(leaflet)
require(plotly)

library(scales)

sistemi=read.csv('./download/2017-11-20/Open-data_Sist-Racc_RU_2015-2016 - RU_2015-2016.csv')

sistemi$PRO_COM=sistemi$ISTAT_PROV*1000+sistemi$ISTAT_COM

andamento_sistemi_anno=sistemi %>% group_by(Anno) %>% 
        summarise (PaP=sum(PP, na.rm=TRUE), 
                   "Contenitori Stradali"=sum(CS, na.rm=TRUE), 
                   Altro=sum(altro, na.rm=TRUE),
                   "Chiamata"=sum(C, na.rm=TRUE),
                   Ecomobile=sum(E, na.rm=TRUE),
                   "Centri di Raccolta"=sum(CdR, na.rm=TRUE)
                   )
tot_KG_anno=sistemi %>% group_by(Anno) %>% 
        summarise (tot_KG=sum(TOTALE.Kg.) 
                   )


melted_sistemi_anno=melt(andamento_sistemi_anno,id.vars=c("Anno"))

andamento_sistemi_comune_anno = sistemi %>% group_by(Anno,COMUNE, ISTAT_PROV, ISTAT_COM, PRO_COM) %>%
        summarise (PaP=sum(PP, na.rm=TRUE), 
                   "Contenitori Stradali"=sum(CS, na.rm=TRUE), 
                   Altro=sum(altro, na.rm=TRUE),
                   "Chiamata"=sum(C, na.rm=TRUE),
                   Ecomobile=sum(E, na.rm=TRUE),
                   "Centri di Raccolta"=sum(CdR, na.rm=TRUE),
                   tot_kg=sum(TOTALE.Kg.)
                   )

produzione=read.csv('./download/2017-11-20/Open-data_PROD_RU_2010-2016 - RU.csv')

produzione$PRO_COM=produzione$ISTAT_PROV*1000+produzione$ISTAT_COM

andamento_sistemi_comune_anno_produzione = merge(andamento_sistemi_comune_anno,produzione, all.y=TRUE, by.x=c("Anno","PRO_COM"), by.y=c("anno","PRO_COM"))

andamento_sistemi_comune_anno_produzione_2010 = andamento_sistemi_comune_anno_produzione %>% filter(Anno==2010)
andamento_sistemi_comune_anno_produzione_2016 = andamento_sistemi_comune_anno_produzione %>% filter(Anno==2016)

scostamento_sistemi_comune_anno_produzione = merge(andamento_sistemi_comune_anno_produzione_2010,andamento_sistemi_comune_anno_produzione_2016, all.y=TRUE, by.x=c("PRO_COM"), by.y=c("PRO_COM"))

scostamento_sistemi_comune_anno_produzione$diffRd=scostamento_sistemi_comune_anno_produzione$RD....y-scostamento_sistemi_comune_anno_produzione$RD....x
scostamento_sistemi_comune_anno_produzione$diffRd[is.na(scostamento_sistemi_comune_anno_produzione$diffRd)]=0.

rdAnno=andamento_sistemi_comune_anno_produzione %>% group_by(Anno) %>% summarize(RDtot=sum(as.numeric(RD.kg.)),RUtot=sum(as.numeric(RU.kg.)))
rdAnno$RD=rdAnno$RDtot/rdAnno$RUtot * 100.

# Limiti cartografici comuni e province
limiti16=readOGR('./download/Limiti_2016_WGS84_g/Com2016_WGS84_g/Com2016_WGS84_g.shp')
limiti16pro=readOGR('./download/Limiti_2016_WGS84_g/CMProv2016_WGS84_g/CMprov2016_WGS84_g.shp')

# Filtro solo la Regione Emilia-Romagna (codice ISTAT 8)
limiti16=limiti16[limiti16$COD_REG==8,]
limiti16pro=limiti16pro[limiti16pro$COD_REG==8,]

limiti16 <- spTransform(limiti16, CRS("+proj=longlat +datum=WGS84 +no_defs"))
limiti16pro <- spTransform(limiti16pro, CRS("+proj=longlat +datum=WGS84 +no_defs"))


densPaPComune=andamento_sistemi_comune_anno[c('Anno','PRO_COM','PaP','tot_kg')]
densPaPComune$percPaP=(densPaPComune$PaP/densPaPComune$tot_kg)*100.

densPaPComune$percPaP[is.na(densPaPComune$percPaP)]=0.

limiti16PaPComune2015=merge(limiti16,densPaPComune %>% filter(Anno==2015),by="PRO_COM")
limiti16PaPComune2016=merge(limiti16,densPaPComune %>% filter(Anno==2016),by="PRO_COM")

limiti16PaPComune2015$percPaP[is.na(limiti16PaPComune2015$percPaP)]=0.
limiti16PaPComune2016$percPaP[is.na(limiti16PaPComune2016$percPaP)]=0.

limiti16RDComune=merge(limiti16,scostamento_sistemi_comune_anno_produzione,by="PRO_COM")

