w <- 256L
h <- 256L
set.seed(0)

img_sample <- matrix(sample(c(0,1),size = w*h, replace = T, prob = c(0.5,0.5)),nrow = w, ncol = h) 

m <- 2^11
y <- lcg_prng(w*h, y1=1, m=m, a=1017, b=1)
img_lcg <- matrix(y/m<0.5, nrow = w, ncol = h) 

par(mfrow=c(1,2), mar=c(1,1,1,1))
image(
  img_sample,
  col = c("black", "white"), 
  axes = FALSE,
  useRaster = TRUE, 
  asp=1, # fixes aspect ratio
  main="sample()") 
image(
  img_lcg,
  col = c("black", "white"), 
  axes = FALSE,
  useRaster = TRUE, 
  asp=1,
  main="lcg_prng()") 
