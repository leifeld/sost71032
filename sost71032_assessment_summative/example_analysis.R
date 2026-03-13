library("ergm")
library("texreg")

load("berlin.RData")

set.seed(12345)

persons_unique <- persons[!duplicated(persons$name), ] # de-duplicate
all(persons_unique$name == rownames(authorship)) # verify correct order

coaut <- authorship %*% t(authorship)
nw <- network(coaut, directed = FALSE)
set.vertex.attribute(nw, "status", persons_unique$status)
set.vertex.attribute(nw, "gender", persons_unique$gender)

numpub <- sapply(rownames(coaut), function(x) length(which(publications$reportingAuthor == x)))
all(names(numpub) == rownames(coaut))
set.vertex.attribute(nw, "numpub", numpub)

same_institution <- matrix(0, nrow = nrow(coaut), ncol = nrow(coaut))
rn <- rownames(coaut)
for (i in 1:nrow(same_institution)) {
  for (j in 1:ncol(same_institution)) {
    unis_i <- persons$uni[persons$name == rn[i]]
    unis_j <- persons$uni[persons$name == rn[j]]
    same_institution[i, j] <- length(intersect(unis_i, unis_j))
  }
}

# proof that a basic model with topic similarity converges
model1 <- ergm(nw ~ edges + edgecov(topic_similarity) + gwesp(0.5, fixed = TRUE))
summary(model1)
gof1 <- gof(model1)
plot(gof1)

# proof that a model without topic similarity can also converge -> the data are sufficiently interesting for an assessment
model2 <- ergm(nw ~ edges + edgecov(same_institution) + nodemix("status") + nodemix("gender") + gwesp(0.5, fixed = TRUE) + gwdegree(0.5, fixed = TRUE))
summary(model2)
gof2 <- gof(model2)
plot(gof2)

# a more elaborate model, showing that other effects than topic similarity are significant
model3 <- ergm(nw ~ edges + edgecov(same_institution) + nodemix("status", base = 3) + nodefactor("gender") + gwesp(0.5, fixed = TRUE) + gwdegree(0.2, fixed = TRUE) + edgecov(topic_similarity) + nodecov("numpub"))
summary(model3)
gof3 <- gof(model3)
plot(gof3)

screenreg(list(model1, model2, model3), single.row = TRUE)
