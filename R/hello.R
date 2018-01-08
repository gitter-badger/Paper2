---
  title: "Dendrogram and Dots: Figuring it Out"
author: "Chase Clark"
date: "January 4, 2018"
output: html_document
---

  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


Load dendrogram cached file for testing with:
  ```{r}
library("tidyverse")
library("dendextend")

dend<-readRDS("Data/Distance-cosineD_Clustering-ward.D2_UsedPresenceAbsence_SNR-4_PercentPresence-70_LowCut-3000_HighCut-15000.rds")

# dend<-dend %>% set("labels_color", "white")

dend<-dend %>% set("labels_cex", .65)

```

Read csv of groups
```{r}
groupFile<-read_csv("data/b.csv")
```

Load colored_Dots.R function
```{r,include=FALSE}
source('~/Documents/GitHub/DendrogramAndDots/R/colored_Dots.R', echo=TRUE)
```


need to simulate input


```{r}
input<-as_tibble(t(names(groupFile)))
names(input)<-names(groupFile)



```

```{r}

dendrogramLabels<-as_tibble(labels(dend))
names(dendrogramLabels)<-'Sample name'
joinedData<-left_join(dendrogramLabels,groupFile,by="Sample name")

naReplaceValues<-as.list(sapply(names(joinedData),function(x)paste0("Missing ",x)))

joinedData<-joinedData %>%  replace_na(replace=naReplaceValues)

colsel<-'Treatment'

small<-bind_cols(joinedData[,1],joinedData[colsel])



#w<-small %>% group_by(.dots=paste0(colsel))

groupedList<-split(small,factor(small[colsel][[1]]))

bigList<-lapply(1:length(groupedList),function(x)left_join(dendrogramLabels,groupedList[[x]],by='Sample name'))

labels(bigList)<-labels(groupedList)



for(x in 1:length(bigList)){
  bigList[[x]][colsel][!is.na(bigList[[x]][colsel])]<-"#000000"
  bigList[[x]][colsel][is.na(bigList[[x]][colsel])]<-"#00000000"
}



```

Now we we have a list. Each list item represents a group/column from excel table.
These are associated to the entire dendrogram, by label and are in the same order as the dendrogram from top to bottom.
0 means a label is not part of the group, 1 means it is part of the group


/#000000  is black
  /#00000000 is black but transparent   in case we want color supprt later


  ```{r}
bigMatrix<-NULL
for (i in 1:length(bigList)){
  bigMatrix<-bind_cols(bigMatrix,bigList[[i]][,2])}

names(bigMatrix)<-names(bigList)

```



The below chunk will only run when the rmd file is run with R (won't work if viewing static html).








                                                               ```{r,fig.height=50}

                                                               par(mar = c(8,8,8,25))

                                                               plot(dend,horiz=T)

                                                               colored_dots(bigMatrix, dend,
                                                               rowLabels = names(bigMatrix),horiz=T)





                                                               ```


