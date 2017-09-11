library(limma)
library(breastCancerNKI)

data(nki)

normalisedValues <- exprs(nki)
features <- fData(nki)

design <- model.matrix(~0+as.factor(pData(nki)$er))
colnames(design) <- c("Positive","Negative")

fit <- lmFit(normalisedValues,design)
cont.matrix <- makeContrasts(Positive - Negative, levels=design)
fit2 <- contrasts.fit(fit,cont.matrix)
fit2$genes <- features[,c("probe","EntrezGene.ID","HUGO.gene.symbol")]
eb <- eBayes(fit2)

toptable(eb)  
write.csv(topTable(eb,number = Inf),"NKI-DE-results.csv",row.names=FALSE)
