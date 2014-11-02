#Purpose of this script:
#Reads a file where each line contains a set of related UMLS codes
#Compares the values of these codes (features) for instances in an ARFF dataset, and 
#returns a measure or pairwise similarity of related codes

library(foreign)
#load ARFF dataset and combinations of related codes
output_path = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
                     "ED_flu_dataset/Feature_selection/UMLS_synonyms/",
                     "top51_related_comparisons.txt", sep = "")
dataset_path = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
                  "ED_flu_dataset/Feature_selection/Experiments/training51.arff",
                  sep = "")
related_codes_path = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
                           "ED_flu_dataset/Feature_selection/UMLS_synonyms/",
                           "top_51_relatedCodes.txt", sep = "")
dataset = read.arff(dataset_path)
related_codes = read.table(related_codes_path)
code_groups = list()
for(codes in related_codes$V1){
  code_groups = append(code_groups, strsplit(codes, ","))
}

#Comparing related codes based on their values accross instances of dataset
out_file = file(output_path, open="w")
for (i in 1:length(code_groups)){
  cat(paste("**RELATED CODES**", "\n", toString(code_groups[[i]]), "\n", sep=""),
      file = out_file)
  code_pairs = combn(code_groups[[i]], 2, simplify = F)
  for (pair in code_pairs){
    feature_comparison = sum((dataset[[pair[1]]] == dataset[[pair[2]]]))/length(dataset[[pair[1]]])
    cat(paste("pair: ", toString(pair), " similarity: ", feature_comparison,
              "\n", sep = ""), file = out_file)
  }
  cat("\n", file=out_file)
}
close(out_file)

