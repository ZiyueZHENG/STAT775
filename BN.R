library('bnlearn')
library(BiDAG)

game_df <- read.csv('./games.csv')

# decide how many sample to choose
sample_size = 5000
game_bndf <- game_df[1:sample_size,4:10]
game_bndf <- data.frame(lapply(game_bndf, function(x) as.factor(x)))

test <- game_df[30000:30100,4:10]
test <- data.frame(lapply(test, function(x) as.factor(x)))

# get the structure
res <- c()
res <- hc(game_bndf)
plot(res)

# get the parameters
fittedbn <- bn.fit(res, data = game_bndf)
fittedbn

# missing data imputation
with.missing.data = game_bndf
with.missing.data<- data.frame(lapply(with.missing.data, function(x) as.factor(x)))
with.missing.data[sample(nrow(game_bndf), 500), "firstTower"] = NA
res = hc(game_bndf)
model = modelstring(res)
fittedbn = bn.fit(model2network(model),game_bndf)
imputed = impute(fittedbn,with.missing.data)
r = 1
for(i in 1:5000){
  if(imputed[i,"firstTower"] == game_bndf[i,"firstTower"]){
    r = r+1
  }
}


# classifier
tan = tree.bayes(game_bndf,'winner')
plot(tan)
fitted = bn.fit(tan,game_bndf,method = "bayes")
pred = predict(fitted,game_bndf)
table(pred,game_bndf[,"winner"])


head(game_df)

# MCMC in BN

sample_size = 1000
game_bndf <- game_df[1:sample_size,c(4,5,6,7,8,9,10)]
score <- scoreparameters("bge",game_bndf)


basefit <- orderMCMC(scorepar = score,MAP = TRUE, plus1 = FALSE)
plot(basefit)
basefit$score
basefit$DAG


iterativefit <- iterativeMCMC(scorepar = score, scoreout = TRUE, verbose = FALSE)
plot(iterativefit)
iterativefit$DAG
summary(iterativefit)


