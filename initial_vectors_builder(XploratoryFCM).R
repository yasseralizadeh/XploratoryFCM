# Set the working directory, first one if using My Personal Laptop, second one HP Laptop

 setwd("E:/Partition E on Google Drive/05- Dissertation/0E-Simulations/Xploratory FCM 2.0")
# setwd("C:/Users/alizadeh/Google Drive/05- Dissertation/0E-Simulations/Xploratory FCM 2.0")

# Generate random initial vectors; Variables: how many IV, How many nodes in Graph, acceptbale values for permutations -1, 0 and 1, probability of acquiring value other than zero=0.01

iv_randomized <- t(replicate(200000,sample(c(-1,1),1) * rbinom(256,1,0.015)))

write.csv(iv_randomized, "200000_random_initial_vectors_4of365.csv", row.names = F)

# one improvement idea; receive the number of initial ceoncepts/managerial practices that we want to change and calculate the probability of binomial
