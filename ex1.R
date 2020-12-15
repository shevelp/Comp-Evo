###############Exercise 1: Sergio Lopez Padilla#################################
library(ggplot2)
library(dbplyr)
library(ggpubr)

#--------- parameters definition -------------#
n_loc = 2 
days = 10000 
replicates = 100000
population = c(100, 300, 1000, 3000)
Bottleneck = c(0.25,0.5,1)

#---------- modeling ------------------#
fix = c()
for (N in population){
  for (B in Bottleneck){
    times = c()
    for (replicate in 1:replicates){
      pop=matrix(0, n_loc, N)
      S1=matrix(0, 1, days)
      times = c(times,c(0))
      for (day in 1:days){
        #Reproduction depending of bottleneck
        if(B == 0.25){
          offspring = sample(1:N, N/4, replace=TRUE)
          pop[] = rbind(pop[,offspring], pop[,offspring],
                        pop[,offspring], pop[,offspring], deparse.level = 0)
        } else if(B == 0.5){
          offspring = sample(1:N, N/2, replace = TRUE)
          pop[] = rbind(pop[,offspring], pop[,offspring], deparse.level = 0)
        } else if(B == 1){
          offspring = sample(1:N, N, replace=TRUE)
          pop[] = rbind(pop[,offspring], deparse.level = 0)
        }
        
        
        #setting the only mutation in the first day
        if (day == 1){
          mutation = 1
          ind = round(runif(mutation, 1, N))
          gen = round(runif(mutation, 1, n_loc))
          coords = rbind(cbind(c(gen), c(ind)))
          pop[coords] = pop[coords]+1
        }
        #Save fixation day if occurs, and the replication ends if the mutation
        2
        #is extinguished or fixed
        S1[1,day] = sum(pop[gen,] == 1)
        if (S1[1,day] == 0){
          break
        } else if (S1[1,day]==N){
          times[replicate] = day
          break
        }
      }
    }
    Tfix = mean(times[times != 0]) #Calculate mean of fixation days
    Pfix = sum(times != 0) / replicates #Calculate frequency of fix
    m_fix = c(N, B, Tfix, Pfix)
    fix = rbind(fix, m_fix)
  }
}

#--------- Data transformation -----------------------# 
colnames(fix) =c("N", "Bottleneck", "Tfix", "Pfix") 

fix <- as.data.frame(fix) #converting df
write.csv(fix, "data") #saving data

#--------relationship analysis-------------------#
cor.test(x = fix$Tfix, y = fix$Bottleneck, method = "pearson")
cor.test(x = fix$Pfix, y = fix$Bottleneck, method = "pearson")
cor.test(x = fix$Tfix, y = fix$N, method = "pearson")
cor.test(x = fix$Tfix, y = fix$N, method = "pearson")

#-----------------plotting---------------------#
theme_set(theme_light(base_size = 10))

#Tfix plotting
Tfix_N = ggplot(fix, aes(x=N, y=Tfix)) +
  geom_point(aes(x=N, y=Tfix)) +
  geom_smooth(se = FALSE, method = "lm") 

Tfix_B = ggplot(fix, aes(x = Bottleneck, y = Tfix)) +
  geom_point(aes(x = Bottleneck, y = Tfix)) +
  geom_smooth(se = FALSE, method = "lm") 

figure_Tfix <- ggarrange(Tfix_N, Tfix_B,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure_Tfix


#Pfix plotting
Pfix_N = ggplot(fix, aes(x=N, y=Pfix)) +
  geom_point(aes(x=N, y=Pfix)) +
  geom_smooth(se = FALSE, method = "lm") 

Pfix_B = ggplot(fix, aes(x = Bottleneck, y = Pfix)) +
  geom_point(aes(x = Bottleneck, y = Pfix)) +
  geom_smooth(se = FALSE, method = "lm") 

figure_Pfix <- ggarrange(Pfix_N, Pfix_B,
                         labels = c("A", "B"),
                         ncol = 2, nrow = 1)
figure_Pfix

################################################################################
