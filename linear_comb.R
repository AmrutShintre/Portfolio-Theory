## script to generate table of return and standard deviation
##  for a linear combination of two assets

mu = c(14.18,563.59)
v_c = matrix(c(206617.78,63987.52,63987.52,6957630.53),2,2)

for (a in seq(0,1,by=0.1)) {
 w = c(a,1-a)
 cat(a,'in asset 1 generate an average of',t(w)%*%mu,
  'standard deviation of',sqrt(t(w)%*%v_c%*%w),'\n')
}


for (a in c(0,0.1,0.3,0.5,0.9,0.95,0.97,0.98,0.99,1)) {
 w = c(a,1-a)
 cat(a,'in asset 1 generate an average of',t(w)%*%mu,
  'standard deviation of',sqrt(t(w)%*%v_c%*%w),'\n')
}

