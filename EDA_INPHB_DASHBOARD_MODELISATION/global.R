library(tidyverse)
library(DT)
library(readxl)
library(treemapify)
library(ggVennDiagram)
library(lemon)

#dictionnaire des variables
dico = read_excel("C:/Users/AGRIDEM/Desktop/PROJET_TUTORE/variables.xlsx")

dt = read_excel("C://Users/AGRIDEM/Desktop/PROJET_TUTORE/donneActualise.xlsx")
dt = dt[,-1]
colnames(dt)
#renommage
dt = dt %>% rename(FiliereAcceuil = filiereAccueil)

#structucture du dataset
str(dt)

#resumé statistiques
summary(dt)

#colonnes
colnames(dt)


#item -- data


#item --  viz------------------------------------------------------------------------------------------
#tabPanel -- stat gen
#ValueBox
infoData = function(tab, filt = "tous"){
  if( filt == "tous"){
    return(tab)
  }else{
    return(filter(tab, anneeConcours == filt))
  }
}

#colbox --  change avec l'annee
colbox = function(tab,filt = "tous"){
  if(filt == "tous"){
    return(dim(unique(tab %>% select(etablissementOrigine)))[1])
  }else{
    tab = filter(tab, anneeConcours == filt)
    return(dim(unique(tab %>% select(etablissementOrigine)))[1])
  }
} 



#candidature feminines -- change avec l'annee
candfem = function(tab, filt ="tous"){
  if(filt != "tous"){
    tab = filter(tab, anneeConcours == filt)
  }
  
  var = tab %>%
    group_by(genre)%>%
    tally()%>%
    arrange(desc(n))%>%
    rename(effectif = n) %>%
    mutate(percent = (round(effectif / sum(effectif) * 100, 2))) %>%
    filter(genre == "F")
  return(var$percent)
 
} 


#nombreChoix en moyenne
nombreChoix = function(tab, filt="tous"){
  if(filt != "tous"){
    tab = tab %>% filter(anneeConcours == filt)
  }
  return(round(mean(dt$nombreDeChoix),0))
}


#inter -- dabord statique -- change avec l'annee
inter = dt %>% filter(Nationalite != "IVOIRIENNE") %>% group_by(region) %>% tally() %>% rename(effectif = n) %>% arrange(desc(effectif))
maps = map_data("world")
maps = left_join(maps, inter, by="region")
maps = maps %>% filter(!is.na(maps$effectif))
maps = ggplot(maps, aes(x=long ,y=lat, group=group)) +geom_polygon(aes(fill = effectif), color="green")
maps

#plot1 -- 
plot1 = function(tab,filt = "tous"){
  if(filt != "tous"){
    tab = filter(tab, anneeConcours == filt)
    resume = tab %>%
      select(genre, ADMISSIBLE, ADMIS) %>%
      mutate(admissible.admis =  str_c(ADMISSIBLE, ADMIS, sep=".")) %>%
      select(-c(2:3)) %>%
      group_by(genre, admissible.admis) %>%
      tally() %>%
      rename(effectif = n)
  }else{
    resume = tab %>%
      select(genre, ADMISSIBLE, ADMIS) %>%
      mutate(admissible.admis =  str_c(ADMISSIBLE, ADMIS, sep=".")) %>%
      select(-c(2:3)) %>%
      group_by(genre, admissible.admis) %>%
      tally() %>%
      rename(effectif = n)
  }
  
  return(ggplot(resume, aes(x = genre, y = effectif, fill = admissible.admis)) +
           geom_col(position = "dodge") +
           geom_text(aes(label = effectif),colour = "white", size = 4,vjust = 1, position = position_dodge(.9)))
}

ggplot(dt, aes(x=genre, fill="ADMISSIBLE")) + geom_bar()


#plot2 -- 
plot2 = function(tab, filt ="tous"){
  
  #imputation par la mediane age
  tab$age[is.na(dt$age) ==  T] = 18
  
  #creation de age.quali
  tab$age.quali = cut(dt$age,
                     include.lowest = TRUE,
                     right = TRUE,
                     dig.lab = 4,
                     breaks = c(14, 16, 18, 20,22,24))
  
  
  if(filt != "tous"){
    tab = tab %>% filter(anneeConcours == filt)
  }

  age.H = tab %>%
    select(age.quali, genre) %>%
    filter(genre == "M") %>%
    select(age.quali,genre) %>%
    group_by(age.quali) %>%
    tally() %>%
    mutate(genre = "M")
  
  age.F = tab %>%
    select(age.quali, genre) %>%
    filter(genre == "F") %>%
    select(age.quali, genre) %>%
    group_by(age.quali) %>%
    tally() %>%
    mutate(genre = "F")
  
  pyr = na.omit(rbind(age.H,age.F))
  
  
  return(ggplot(data = pyr,
                mapping = aes(x = ifelse(test = genre == "M", yes = -n, no = n), y = age.quali, fill = genre)) +
                geom_col() +
                scale_x_symmetric(labels = abs) +
                labs(x = "Population"))
  
}


#tabpPanel -- stat admissible/admis ---

plot4 = function(tab,filt="tous"){
  if(filt != "tous"){
    return(ggplot(tab %>% filter(anneeConcours == filt), aes(x=moyenneDossier, fill=ADMISSIBLE, color=ADMISSIBLE)) +
             geom_histogram(position="identity", alpha=0.5))
  }else{
    return(ggplot(tab, aes(x=moyenneDossier, fill=ADMISSIBLE, color=ADMISSIBLE)) +
             geom_histogram(position="identity", alpha=0.5))
  }
}


nuageA = function(tab, filt="tous"){
  tab = tab %>% filter(moyenneDossier != 0 & moyenneTest != 0)
  if(filt != "tous"){
    tab = tab %>% filter(anneeConcours == filt)
  }
  
  return(ggplot(tab) +
           geom_point(aes(x = moyenneDossier, y = moyenneTest, color = mentionAuBac)) +
           scale_color_manual("MentionAuBac",values = c("red", "#FFDD45", rgb(0.1,0.2,0.6), "darkgreen", "grey80")))
}


#tabPanel -- stat admis ------
#top ecole

plot3 = function(tab,filt = "tous",tips="ADMIS"){
  
  if(filt != "tous"){
    if(tips != "ADMIS"){
      tab = tab %>%
        filter(ADMISSIBLE == "OUI" & anneeConcours == filt) %>%
        group_by(etablissementOrigine) %>%
        tally()%>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        slice(1:3)
    }else{
      tab = tab %>%
        filter(ADMIS == "OUI" & anneeConcours == filt) %>%
        group_by(etablissementOrigine) %>%
        tally()%>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        slice(1:3)
    }
  }else{
    if(tips != "ADMIS"){
      tab = tab %>%
        filter(ADMISSIBLE == "OUI") %>%
        group_by(etablissementOrigine) %>%
        tally()%>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        slice(1:3)
    }else{
      tab = tab %>%
        filter(ADMIS == "OUI") %>%
        group_by(etablissementOrigine) %>%
        tally()%>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        slice(1:3)
    }
  }
  return(tab %>%
  ggplot(aes(x=etablissementOrigine, y=effectif)) +
    geom_bar(stat ="identity", width=0.3) +
    geom_text(aes(label=effectif), vjust=1.6, color="white", size=3.5) + 
    scale_fill_brewer(palette="Dark2"))
}


  

#repartition par filiere -- change avec l'année

piechart =  function(tab, filt ="tous", sex = "tous"){
  hsize = 3
  
  if(filt != "tous"){
    if(sex != "tous"){
      pie = tab %>%
        filter(anneeConcours == filt & genre == sex) %>%
        group_by(FiliereAcceuil) %>%
        tally() %>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        mutate(x=hsize)
      pie = na.omit(pie)
    }else{
      pie = tab %>%
        filter(anneeConcours == filt) %>%
        group_by(FiliereAcceuil) %>%
        tally() %>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        mutate(x=hsize)
      pie = na.omit(pie)
    }
  }else{
    if(sex != "tous"){
      pie = tab %>%
        filter(genre == sex) %>%
        group_by(FiliereAcceuil) %>%
        tally() %>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        mutate(x=hsize)
      pie = na.omit(pie)
    }else{
      pie = tab %>%
        group_by(FiliereAcceuil) %>%
        tally() %>%
        arrange(desc(n)) %>%
        rename(effectif = n) %>%
        mutate(x=hsize)
      pie = na.omit(pie)
    }

  }
  #car les individus pour lesquels il y'a na dans filiere d'acceuil ne sont pas admis a la base
  return(ggplot(pie, aes(x = hsize, y = effectif, fill = FiliereAcceuil)) +
    geom_col(color = "black") +
    geom_label(aes(label = effectif),position = position_stack(vjust = 0.5), show.legend = FALSE) +
    coord_polar(theta = "y") +
    xlim(c(0.2, hsize + 0.5)))
}

#tree map - top10 des 
treemap = function(tab, filt="tous",tips="ADMIS"){
  if(filt != "tous"){
    if(tips != "ADMIS"){
      top_school = tab %>%
        filter(ADMISSIBLE == "OUI") %>%
        group_by(serie) %>%
        tally() %>%
        arrange(desc(n)) %>%
        slice(1:10)
    }else{
      top_school = tab %>%
        filter(ADMIS == "OUI") %>%
        group_by(serie) %>%
        tally() %>%
        arrange(desc(n)) %>%
        slice(1:10)
    }
    
  }else{
    if(tips!="ADMIS"){
      top_school = tab %>%
        filter(ADMISSIBLE == "OUI") %>%
        group_by(serie) %>%
        tally() %>%
        arrange(desc(n)) %>%
        slice(1:10)
    }else{
      top_school = tab %>%
        filter(ADMIS == "OUI") %>%
        group_by(serie) %>%
        tally() %>%
        arrange(desc(n)) %>%
        slice(1:10)
    }
  }

  return(ggplot(top_school, aes(area = n, fill = serie, label = paste(serie, paste(n," eleves"), sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(colour = "black", place = "centre", size = 20))
}



#distribution des notes au test -- evaluer le caractere discriminant de la selection
boxtest = function(tab, filt = "tous"){
  if(filt != "tous"){
    tab = tab %>% filter(anneeConcours == filt)
  }
  
  rp = tab %>% select(identifiantCandidat, ANGLAIS, `CULTURE GENERALE`, `CULTURE SCIENTIFIQUE`, ADMIS) %>% 
    rename(cg = `CULTURE GENERALE` ) %>% rename(cs = `CULTURE SCIENTIFIQUE`)
  
  rp = rp %>% pivot_longer(cols=c('ANGLAIS', 'cs','cg'),
                           names_to='note',
                           values_to='moyenne')
  return(
    ggplot(rp) +
      geom_boxplot(aes(x = note, y = moyenne)) +
      geom_jitter(aes(x = note, y = moyenne,  fill=ADMIS, color=ADMIS), alpha = 0.4) +
      theme_dark()
  )

}



#prediction---------------------------------------------------------------------------------
#dataset
Jeu = dt %>%
  select(moyenneDossier,ANGLAIS,`CULTURE GENERALE`,`CULTURE SCIENTIFIQUE`,moyenneTest,moyenneDefinitive, nombreDeChoix, ADMIS, anneeConcours)

#dataset entrainement
Jeu = Jeu %>%
  filter(anneeConcours %in% c("2017","2018"))

#suppression des colonnes unitiles
Jeu = Jeu %>%
  select(moyenneDossier, ANGLAIS, `CULTURE GENERALE`, `CULTURE SCIENTIFIQUE`, ADMIS)

#renommage des colonnes
colnames(Jeu) =  c("moyenneDossier","anglais","cg", "cs","ADMIS")

#recodage de la variables ADMIS
Jeu$ADMIS_rec = 0
Jeu$ADMIS_rec[Jeu$ADMIS == "OUI"] = 1

#remplacement
Jeu = Jeu %>%
  select(-ADMIS) %>%
  rename(ADMIS = ADMIS_rec)


#entrainement du modele
reglog = glm(ADMIS ~ ., family="binomial", data = Jeu)


#reponse
reponse.modele = function(proba){
  if(proba > 0.5){
    print("ADMIS")
  }else{
    print("REFUSÉ")
  }
}










