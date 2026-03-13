library("network")

# prepare unis
uni <- read.csv2("uni-DE.csv")
uni_id <- c(79, # FU
            80, # Potsdam
            84, # Hertie
            92, # Viadrina
            94, # HU
            97, # WZB
            98, # TU
            99 # SWP
            )
uni <- uni[uni$id %in% uni_id, ]
uni$label <- c("Free University Berlin",
               "University of Potsdam",
               "Hertie School of Governance",
               "European University Viadrina Frankfurt (Oder)",
               "Humboldt University Berlin",
               "WZB Berlin Social Science Center",
               "Technical University Berlin",
               "German Institute for International and Security Affairs (SWP)")
rownames(uni) <- NULL

# prepare persons
pers <- read.csv2("persons-DE.csv")
colnames(pers)[1] <- "ID"
pers <- pers[pers$uni %in% uni$name, ]
pers$geb[pers$geb == 0] <- NA
pers$phd[pers$phd == 0] <- NA
pers$gender[pers$gender != "weiblich"] <- "male"
pers$gender[pers$gender == "weiblich"] <- "female"
pers$status[pers$status == "Postdoc"] <- "postdoc"
pers$status[pers$status == "Professor"] <- "professor"
colnames(pers)[5] <- "birthyear"
pers <- pers[order(pers$name), ]

# prepare topic similarity
top <- read.csv2("topicSim-DE.csv", check.names = FALSE, row.names = 1, header = TRUE, dec = ".")
top <- top[rownames(top) %in% pers$name, colnames(top) %in% pers$name]
top <- top[order(rownames(top)), order(colnames(top))]
top <- as.matrix(top)

# prepare publications
pub <- read.csv2("publications-DE.csv")
pub <- pub[pub$reportingAuthor %in% pers$name, ]
pub$type[pub$type == "Sammelband"] <- "edited volume"
pub$type[pub$type == "Monographie"] <- "monograph"
pub$type[pub$type == "Buchkapitel"] <- "book chapter"
pub$type[pub$type == "Artikel"] <- "article"
pub$type[pub$type == "Sonstiges"] <- "other"
pub <- pub[pub$reportingAuthor %in% rownames(top), ]
pub <- pub[order(pub$id), ]
rownames(pub) <- NULL

# prepare affiliation matrix
aff <- read.csv2("networkAff-DE.csv", check.names = FALSE, row.names = 1)
aff <- aff[rownames(aff) %in% pers$name, colnames(aff) %in% pub$id]
aff <- aff[order(rownames(aff)), order(colnames(aff))]
aff <- as.matrix(aff)

# filter persons
pers <- pers[pers$name %in% rownames(aff), ]
rownames(pers) <- NULL

# save to disk
persons <- pers
institutions <- uni
authorship <- aff
topic_similarity <- top
publications <- pub
save(persons, institutions, authorship, topic_similarity, publications, file = "berlin.RData")

# proof of concept: network diagram
plot(network(authorship %*% t(authorship), loops = FALSE, directed = FALSE))
