library("jpeg")
library("stringr")
library("plyr")
library("lattice")
library("MASS")
library("biotools")

#
# curried function to create an individual
# record from an image
# 
# string -> string -> vector
#
get_img = function(img_path) function(filename) {
  img = readJPEG(str_c(img_path, filename))
  
  # average channels
  if (length(dim(img))) {
    img = (img[,,1] + img[,,2] + img[,,3]) / 3
  }
  
  # extract id and smiling indicator from filename
  parts = str_split(filename, "\\.")[[1]][1] # <id><emotion>.jpg
  smiling = if (str_sub(parts, str_length(parts)) == "b") 1 else 0
  id = as.numeric(str_sub(parts, 1, str_length(parts) - 1))

  c(id, smiling, img_to_vector(img))
}


#
# convert image back and forth 
# between matrix and vector
#
img_to_vector = function(img) c(t(img))
vector_to_img = function(vec, ncol=360) t(matrix(vec, ncol=ncol))



#
# read raw images, assign gender and expression,
# join with gender information, return dataframe
#
# string -> dataframe
#
get_img_data = function(image_set="normalized") {
  path = "./data/"

  # id-gender mapping
  gender = read.csv(str_c(path, "gender.txt"), sep="", header=F)
  names(gender) = c("id", "gender")
  
  # list of all expression image files
  img_path = str_c(path, image_set, "/")
  files = list.files(img_path, pattern = "\\.jpg$")
  
  # map the image files into vectors
  X = data.frame(t(sapply(files, get_img(img_path))))
  pixels = sapply(1:(dim(X)[2] - 2), function(i) paste("p", i, sep=""))
  names(X) = c("id", "smile", pixels)
  out = join(X, gender, by="id", type='left', match='all')
  out[order(out$id), c("id", "gender", "smile", pixels)]
}


#
# plot an image vector
#
plot_img_vec = function(x, main="", ncol=360) {
  img = vector_to_img(as.numeric(x), ncol=ncol)
  plot(
    1:2, type = 'n', ylab='', xlab='', 
    xaxt = "n", yaxt = "n", bty="n", 
    asp=1, main=main
  )
  rasterImage(img,1,1,2,2)
}

#
# save image to file
#
save_img_vec = function(x, filename, ncol=360) {
  writeJPEG(
    vector_to_img(x, ncol=ncol), 
    target=str_c("./output/", filename, ".jpg")
  )
}

#
# colors
#
orange = "#ff7f00"
blue = "#0080ff"

#
# Helper functions
#
normalize = function(x) (x - min(x)) / (max(x) - min(x))
rotate = function(x) t(apply(x, 2, rev))
ramp = colorRamp(c(blue, "#ffffff", orange))
palette = function(n) rgb(ramp(seq(0,1,length.out=n)), max=255)


#
# wrap function in pdf output
#
to_pdf = function(fn) function(..., filename="", height=6, width=6) {
  full_path = str_c("./output/", filename, ".pdf")
  print(str_c("writing file to ", full_path, "..."))
  pdf(full_path, height=height, width=width)
  fn(...)
  dev.off()
}


#
# heatmap of principal component
#
eigen_heatmap = to_pdf(function(PC, ncol=360) {
  n = 200
  x = rotate(vector_to_img(PC, ncol=ncol))
  max_ext = max(abs(min(PC)), max(PC))
  at = seq(-max_ext, max_ext, length.out=n)
  print(levelplot(
    x, col.regions=palette(n-1), at=at,
    par.settings = list(
      axis.line = list(col = "transparent"),
      axis.lab = F,
      mar=c(0,0,0,0)
    ),
    xlab="", 
    ylab="",
    scales=list(x=list(at=c()), y=list(at=c()))
  ))
})


#
# compute principal components of X using fast method,
#
get_eigen = function(Z) {
  A = as.matrix(t(Z))
  eig = eigen(crossprod(A))
  n = dim(Z)[1]
  list(
    values = (1/(n-1)) * eig$values,
    vectors = sapply(1:ncol(eig$vectors), function(i) {
       v = A %*% eig$vectors[,i]
       v / sqrt(sum(v^2))
    })
  )
}


#
# plot cumulative sum of variance
#
cumulative_variance_plot = to_pdf(function(eig) {
  cum_variance = cumsum(eig$values) / sum(eig$values)
  plot(
    cum_variance, bty="n", pch=21, col=NULL,
    ylab="Cumulative Variance",
    xlab="Eigenvalue Index",
    lwd=4, bg=blue
  )
  abline(h=0.99, lty=2)
  abline(v=sum(cum_variance < 0.99), lty=2)
  abline(h=0.95, lty=3)
  abline(v=sum(cum_variance < 0.95), lty=3)
})

#
# plot first 2 PC's against one another
#
pc_plot = to_pdf(function(PC, group, legend=c("Female", "Male"), lx=-50, ly=-30, ylab="PC2") {
  plot(
    PC[,1:2], pch=21, bg=c(blue,orange)[unlist(group)],
    xlab="PC1", ylab=ylab, bty="n"
  )
  legend(lx, ly, legend=legend,
         col=c(blue,orange), lty=1, lwd=6, cex=0.8)
})


pc_pairs = to_pdf(function(PC, group) {
  pairs(
    PC, bg=c(blue, orange)[unlist(group)], pch=21, col=NULL,
    labels=sapply(1:dim(PC)[2], function(i) str_c("PC", i))
  )
})



loo_lda_accuracy_plot = to_pdf(function(class_var, PC, method) {
  fn = if (method == "qda") qda else lda
  n = dim(PC)[1]
  p = dim(PC)[2]
  d = sapply(1:p, function(i) {
    fit = fn(class_var ~ PC[,1:i], CV = TRUE)
    sum(diag(table(class_var, fit$class))) / n
  })
  plot(
    d, pch=21, bg=blue, col=NULL, bty="n", ylim=c(0,1),
    xlab="Number of Principal Components", ylab="Accuracy Ratio"
  )
})

stat.dist = function(X) {
  mu = colMeans(X)
  S_inv = solve(cov(X))
  
  # compute d_j for each row, returning the resulting vector
  apply(X, 1, function(x) {
    z = x - mu
    t(z) %*% S_inv %*% z
  })
}


chi.plot = to_pdf(function(X) {
  d = stat.dist(X)
  df = dim(X)[2]
  n = length(d)
  theoretical = sapply(1:n, function(j) {
    qchisq( ((j - 0.5)/n), df=df )
  })
  observed = sort(d)
  overall_max = max(max(observed), max(theoretical))
  overall_min = min(min(observed), min(theoretical))
  lims = c(overall_min, overall_max)
  plot(
    observed, theoretical,
    pch=21, bg=blue, col=NULL, bty="n",
    xlim=lims,ylim=lims,
    xlab="Distance", ylab="Quantile",
    bty="n"
  )
  abline(0,1)
})


box.cox.scale = function(X) {		
  apply(X, 2, function(y) {		
    # bc = box.cox(x)		
    # x_lam(x, bc$L.max)		
    p = powerTransform(y, family="yjPower")		
    yjPower(y, p$roundlam)		
  })		
}

