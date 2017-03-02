# посмотреть графики можнос с помощью команды plots[i], где i до 7

# загружаем данные и вычисляем требуемые величины для метода
df <- read.csv('train.csv')
f  <- data.matrix(df[,-1], rownames.force = NA)
y <- data.matrix(df[,1], rownames.force = NA)
f_t <- t(f)
forV <- f_t%*%f
sobs <- eigen(forV)
sobs_val <- sobs$values

# строим график с собственными числами
library(ggplot2)
plots = list()
sobs_gr <- data.frame(Num=1:length(sobs_val),Value=sort(sobs_val,decreasing = TRUE))
plots[[1]] <- ggplot(aes(x=Num,y=Value),data=sobs_gr) +
xlab('Номер') + ylab('Значение') +
geom_point(size=0.8,color='dark blue') + 
ggtitle("Собственные значения")

# вычисляем доли объясненной дисперсии
Z.pc <- f%*%sobs$vectors
d <- 200 # ввиду большой базы и долгих вычислений ставим ограничение
tss <- sum((y - mean(y))^2)
beta <- vector("list", d)
r.sq <- numeric(d)
delta.r <- numeric(d)
for (p in 1:d) {
  Z <- cbind(1, Z.pc[,1:p])
  beta[[p]] <- solve(t(Z)%*%Z)%*%t(Z)%*%y
  y.hat <- Z %*% cbind(beta[[p]])
  rss <- sum((y - y.hat)^2)
  r.sq[p] <- 1 - rss/tss
  delta.r[p] <- ifelse(p == 1, r.sq[p], r.sq[p] - r.sq[p-1])
}

# строим график кумулятивных сумм
dis_gr <- data.frame(Num=1:d,Value=r.sq)
plots[[2]] <- ggplot(aes(x=Num,y=Value),data=dis_gr) + 
xlab('Количество собственных векторов') +
ylab('Доля объясненной дисперсии') +
geom_line(size=0.5,color='green') + ggtitle("Кумулятивные суммы")

# первые 20 векторов объясняют более 50 процентов дисперсии
# далее скорость роста объяясненной доли сильно снижается,
# как видно из графика. взяв их, мы объясним большую часть
# дисперсии за малое количество компонент

# строим изображения первых пяти собственных векторов
x = c()
y = c()
for (i in 1:28)
{
  for (j in 1:28)
  {
    x[i+28*(j-1)]=i
    y[j+28*(i-1)]=i
  }
}
for (i in 1:5)
{
  vec_gr <- data.frame(X=x,Y=y,Val=sobs$vectors[,i])
  plots[[i+2]] <- ggplot(aes(x=X,y=Y,fill=Val),data=vec_gr)+
    scale_fill_continuous(name='')+
    ggtitle(i)+
    geom_raster()
}