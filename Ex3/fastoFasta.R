#Sergio Lopez Padilla
#converting FAS to fasta in R. 


#########################################################
library(seqRFLP)
fas <- readLines("Rabies_ALL_SPECIES_N.FULL.DATED.fas")
fasta <- ConvFas(fas)

write.fasta(sequences = fasta, file = "Rabies_ALL_SPECIES_N.fasta")
########################################################