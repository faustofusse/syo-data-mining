library(data.table)
library(dplyr)
library(tidyr)

verbos <- read.csv("~/Desktop/POSTA_R/datasets/verbs/jehle_verb_database.csv")

verbos <- verbos %>% select("infinitive", "form_1s", "form_2s", "form_3s", "form_1p", "form_2p", "form_3p", "gerund", "pastparticiple")
verbos <- unite(verbos, form_1s, form_2s, form_3s, form_1p, form_2p, form_3p, col = "forms") # gerund, pastparticiple,
verbos$forms <- sapply(verbos$forms, function(x) (strsplit(x, split = "_", fixed=TRUE)))

tiene_verbos_compuestos <- function(x){
  for (v in x){
    temp<-unlist((strsplit(v, split = " ", fixed = TRUE)), use.names=FALSE)
    if(length(temp) > 1) return(TRUE)
  }
  return(FALSE)
}
verbos$forms <- sapply(verbos$forms, function(x){
  if (tiene_verbos_compuestos(x)) return(NA)
  else return(x)
})

verbos <- filter(verbos, !is.na(forms))
verbos <- verbos %>% rowwise() %>% mutate(forms = list(c(forms, gerund, pastparticiple)))
verbos <- verbos %>% select("infinitive", "forms")

verbos <- verbos %>% group_by(infinitive) %>% summarise_all(funs(list(na.omit(.))))
verbos <- verbos %>% rowwise() %>% mutate(forms = list(unlist(forms)))

verbos$forms <- sapply(verbos$forms, function(x) (unique(unlist(x))))
verbos$forms <- vapply(verbos$forms, paste, collapse = ";", character(1L))

write.csv(verbos, "~/Desktop/verbos_clean.csv")
