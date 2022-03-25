setwd("/Users/neshcheret/Documents/GitHub/articles/tree-subsets")

k_probabilities <- read.csv("probabilities_K.txt",sep=";")
k_probabilities[k_probabilities$level=="word",]
# word K=4 (highest Ln probability of data, lower variance compared to the other candidate with high Ln Prob and highest mean value of ln likelihood, K5. K6 also has high mean value of ln prob, but higher variance)
k_probabilities[k_probabilities$level=="clause",]
# clause K=4 (highest ln prob of data, relatively low variation compared to other candidate, K5, which has the highest mean ln likelihood: there is a high jump in variance from 4 to 5; better than K3 in terms of ln prob of data and mean ln likelihood, but worse in terms of variance - variance keeps increasing with each K)
k_probabilities[k_probabilities$level=="phonology",]
# phonology K=4 (highest ln prob of data and third-highest mean ln likelihood (after K7 and K8, which have lower ln prob of data and higher variance), there is almost a doubling of variance from K4 to K5)

word_prob <- k_probabilities[k_probabilities$level=="word",]
clause_prob <- k_probabilities[k_probabilities$level=="clause",]


plot(-(clause_prob$Estimated.Ln.Prob.of.Data))
plot(-(clause_prob$Mean.value.of.ln.likelihood))
plot(clause_prob$Mean.value.of.alpha)
