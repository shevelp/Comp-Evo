#Exercise 1: Checking COEVO
#Sergio Lopez Padilla

library(ape)
hostdna <- read.dna("Mammals CYTB/sequence.fasta", format = "fasta")
hostdna
class(hostdna)

parasitedna <- read.dna("outputs/Rabies_ALL_SPECIES_N.fasta", format = "fasta")
parasitedna
class(parasitedna)


#dist-tree
treehost <- dist.dna(hostdna, model = "TN93")
dist_matrix_host <- as.data.frame(as.matrix(treehost))

treeparasite <- dist.dna(parasitedna, model = "TN93")
dist_matrix_parasite <- as.data.frame(as.matrix(treeparasite))

write.table(dist_matrix_host, file = "distancematrixes/hostmatrix.txt")
write.table(dist_matrix_parasite, file = "distancematrixes/parasitematrix.txt")

dist_matrix_host <- read.table("hostmatrix.txt")
dist_matrix_parasite <- read.table("parasitematrix.txt")

#coevo in APE
#I need: A rectangular matrix with hosts as rows and parasites as columns. The matrix contains 1's when a host-parasite link has been observed in nature between the host in the row and the parasite in the column, and 0's otherwise.
#rownames and colnames
HP <- matrix(nrow = 8, ncol = 70) 
rownames(HP) <- rownames(dist_matrix_host)
colnames(HP) <- rownames(dist_matrix_parasite)

#values
HP[1,56:65] <- 1 #skunk
HP[4,12:21] <- 1 #Dog
HP[3,66:70] <- 1 #Wolf
HP[5,22:35] <- 1 #Fox
HP[6,46:53] <- 1 #Racoon
HP[7,36:45] <- 1 #Hum
HP[8,8:11] <- 1 #A.lit
HP[2,1:7] <- 1 #Desmo

HP[is.na(HP)] <- 0 #NAs to 0
  
coevo <- parafit(dist_matrix_host, dist_matrix_parasite, HP, 
                 nperm = 999, test.links = TRUE, seed = NULL, 
                 correction = "lingoes", silent = FALSE)


print(coevo)

