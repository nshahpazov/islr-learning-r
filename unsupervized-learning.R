library(TSA)

set.seed(2)

n = 100
k = 4
x = matrix(rnorm(n*2), ncol=2)

x[1:25,1] = x[1:25,1] + 3
x[1:25,2] = x[1:25,2] -4

x[25:50,1] = x[25:50,1] + 14
x[25:50, 2] = x[25:50, 2] -14

x[51:75,1] = x[51:75, 1] + 26
x[51:75, 2] = x[51:75, 2] - 26

x[75:100, 1] = x[75:100, 1] + 25
x[75:100, 1] = x[75:100, 1] - 25

plot(x)

ks = sample(1:k, n, replace=T)

centroid = function (x, k, ks) {
  return(colMeans(x[which(ks == k), ]))
}

centroids = matrix(unlist(Map({function (i) centroid(x, i, ks)}, 1:k)), 2)

whichK = function (el, centroids) {
  dists = Map({function (i) {dist(rbind(el, centroids[, i]))}}, 1:k)

  return(which.min(unlist(dists)))
}

ks.prev = ks
while(TRUE) {
  ks.new = unlist(Map({function (i1) which.k(x[i1,], centroids)}, 1:n))
  # have to update the ks
  centroids = matrix(unlist(Map({function (i) centroid(x, i, ks.new)}, 1:k)), 2)
  if (all(ks.new == ks.prev)) {
    break;
  }
  ks.prev = ks.new
}

km.out = kmeans(x, 2, nstart=20)
#  plot(x, col=(km.out$cluster+1), pch=20, cex=2)
plot(x, col=(ks.new), pch=20, cex=2)


# time series analysis, will move later to another file
e = rnorm(1000)
y = e - 0.5 * zlag(e)
