#p次元空間における一様分布標本と原点間の距離
#データのサイズ
size = 1000
#最大の次元数
P=10
#標本点と原点間の距離
distance = c()

for(p in 1:P){
  distance[p] = mean(runif(n=size, min=0, max=1)^(1/p))
} 

#作図
png("distanceFromO.png", width=500, height=500)
plot(1:P, distance, xlab="次元数", ylab="原点からの距離")
dev.off()