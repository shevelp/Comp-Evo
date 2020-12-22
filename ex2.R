#Exercise 2
#Sergio Lopez Padilla
#Computational Biology Master

##### Study how fitness depends on the size of the population (N) and fitness advantage (f_bent)################
library(ggplot2)
library(ggpubr)

#----------- parameters definition -----------------#
N = c(10, 30, 100, 300, 1000)
f_ben = c(0.005, 0.01 ,0.02 ,0.04 ,0.08)
n_loc = 5
u = 0.05
u_ben = u*1e-3
u_del = u*1e-1
tdays = 25000
replicates = 100
avgfit1 = c()
avgfit2 = c()

#-----------modeling-----------------------#
for(i in N){
  avgs_fitness = c() #temporal container
  for (j in f_ben){
    #cat("Experiments for", j, "f_ben and", i, "genomes are done. \n" )
    avg_fitness = c() #temporal container
    for (rep in 1:replicates){
      pop = matrix(1, n_loc, i) #population matrix 
      fit_vector = matrix(1, 1, i) #container of fitness per genome
      for (days in 1:tdays){
        #reproduction
        offspring = sample(1:i, i/2, replace=TRUE, prob = fit_vector)
        pop = cbind(pop[,offspring], pop[,offspring], deparse.level = 0)
        #beneficial mutation
        mutations = rpois(1, i*u_ben)
        ind = round(runif(mutations, 1, i))
        gen = round(runif(mutations, 1, n_loc))
        coords = cbind(c(gen), c(ind))
        pop[coords] = 1
        #deleterious mutation
        mutations = rpois(1, i*u_del)
        ind = round(runif(mutations, 1, i))
        gen = round(runif(mutations, 1, n_loc))
        coords = cbind(c(gen), c(ind))
        pop[coords] = 0
        #data retrieving
        fit_vector = 1+colSums(pop)*j
        if(days == 25000){  
          #store average fitness 
          avg_fitness = c(avg_fitness, fit_vector) 
        }
      }
    } 
    #store mean average fitness like columns
    avgs_fitness = c(avgs_fitness, mean(avg_fitness))
    avgfit2 = rbind(avgfit2, c(i, j, mean(avg_fitness)))
  }  
  #store mean average fitness like matrix 5*5
  avgfit1 = rbind(avgfit1, avgs_fitness)
}

#----------- data transformation --------------#
colnames(avgfit2) = c("N", "f_ben", "avg_fitness")
df <- as.data.frame(avgfit2)
write.csv(df,"output.csv")
#print(df)


#-----------relationship --------------------#
cor.test(x = df$f_ben, y = df$avg_fitness, method = "pearson")
cor.test(x = df$N, y = df$avg_fitness, method = "pearson")

#--------- plotting--------------------------#
theme_set(theme_light(base_size = 10))

#N plotting
fit_N = ggplot(df, aes(x=N, y=avg_fitness)) +
  geom_point(aes(x=N, y=avg_fitness)) +
  geom_smooth(se = FALSE, method = "loess") + facet_wrap(~ df$f_ben)

plot(fit_N)

fit_B = ggplot(df, aes(x=f_ben, y=avg_fitness)) + 
  geom_point(aes(x=f_ben, y=avg_fitness)) + 
  geom_smooth(se = F, method = "loess") + facet_wrap(~ df$N) 

plot(fit_B)
figure <- ggarrange(fit_N, fit_B,
                         labels = c("A", "B"),
                         ncol = 1, nrow = 2)
figure
