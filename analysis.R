source("./methods.R")

#
# Load data
#
X = get_img_data("color")


# mark columns that are pixel variables
pixels = -c(1,2,3)

# populations
smile = X$smile == 1
nosmile = !smile
females = X$gender == "F"
males = !females

#
# take sample of 50 ids to keep smiling and 50 ids 
# to keep non-smiling for each gender
#
set.seed(41599798)

female_ids = unique(X[females,]$id)
male_ids = unique(X[males,]$id)
smile_females = X$id %in% sample(female_ids, size=length(female_ids) / 2)
smile_males = X$id %in% sample(male_ids, size=length(male_ids) / 2)

sample_include = (
  (smile_females & smile) |
  (!smile_females & females & nosmile) |
  (smile_males & smile) |
  (!smile_males & males & nosmile)
)

#
# centered data
#
centered = data.frame(
  X[sample_include,-pixels], 
  scale(X[sample_include,pixels], center=T, scale=F)
)

#
# compute overall principal components,
#
eig = get_eigen(centered[,pixels])
PC = as.matrix(centered[,pixels]) %*% eig$vectors

#
#
# output plots
#
#
make_face_plots = function() {
  
  loo_lda_accuracy_plot(centered$gender == "M", PC[,1:195], method="lda", filename="loo_lda_gender")
  loo_lda_accuracy_plot(centered$smile, PC[,1:195], method="lda", filename="loo_lda_smile")
  loo_lda_accuracy_plot(centered$gender == "M", PC[,1:90], method="qda", filename="loo_qda_gender")
  loo_lda_accuracy_plot(centered$smile, PC[,1:90], method="qda", filename="loo_qda_smile")
  
  #
  # plotting principal components against one another
  #
  pc_plot(PC, (centered$gender == "M") + 1, filename="pc_pairs_gender_1_2")
  pc_pairs(PC[,1:3], (centered$gender == "M") + 1, filename="pc_pairs_gender")
  pc_pairs(PC[,1:3], centered$smile + 1, filename="pc_pairs_smile")
  
  #
  # cumulative sum of eigenvalues plot
  #
  cumulative_variance_plot(eig, filename="eig_cum_variance", height=5, width=6)
  
  #
  # heatmap of eigenvectors
  #
  eigen_heatmap(eig$vectors[,1], filename="eig_1_heatmap")
  eigen_heatmap(eig$vectors[,2], filename="eig_2_heatmap")
  eigen_heatmap(eig$vectors[,3], filename="eig_3_heatmap")
  eigen_heatmap(eig$vectors[,4], filename="eig_4_heatmap")
  
  #
  # chi-squared plot
  #
  chi.plot(PC[,1:20], filename="chi-squared-face")
  
  #
  # Average Faces
  #
  save_img_vec(colMeans(X[females & smile, pixels]), "avg_female_smile")
  save_img_vec(colMeans(X[females & nosmile, pixels]), "avg_female_nosmile")
  save_img_vec(colMeans(X[males & smile, pixels]), "avg_male_smile")
  save_img_vec(colMeans(X[males & nosmile, pixels]), "avg_male_nosmile")
  save_img_vec(colMeans(X[females, pixels]), "avg_female")
  save_img_vec(colMeans(X[males, pixels]), "avg_male")
}


make_face_plots()




