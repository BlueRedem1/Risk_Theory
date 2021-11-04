#X <- rexp(10000,1/100)
#Y <- rexp(10000,1/200)
#Z = X+Y
#quantile(Z, 0.9)
#quantile(Z, 0.95)

var_90 = 0
var_95=0
k = 1000
for (i in 1:k){
  X <- rexp(10000,1/100)
  Y <- rexp(10000,1/200)
  Z = X+Y
  var_90 = var_90+quantile(Z, 0.9)
  var_95 = var_95+quantile(Z, 0.95)
}
var_90 = var_90/k
var_95 = var_95/k

print(paste0("VaR al 90%: ", var_90))
print(paste0("VaR al 95%: ", var_95))
