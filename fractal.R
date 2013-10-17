require(RColorBrewer)

fractal<-function(){
colorRampPalette(brewer.pal(9,'Spectral'))(25)->paleta
z0 <- complex(real = stats::rnorm(1000000), imaginary = stats::rnorm(1000000))

c<-complex(real=-.7,imaginary=.5)
#png('fractal2.png')
plot(z0,xlim=c(-1,1),ylim=c(-1,1),pch=20)
for(j in 1:length(z0)){

z<-complex(length.out=100)
modz<-NULL
z[1]<-z0[j]
modz[1]<-Mod(z[1])
for (n in 2:100){
z[n]<-z[n-1]*z[n-1]+c
modz[n]<-Mod(z[n])
}

ind_modz10<-modz<=2
modz_10<-modz[ind_modz10]
ind2<-length(modz_10)
print(ind2)
if(ind2<100){
points(z0[j],pch=20,col=paleta[ind2],cex=.1)
}

}
#dev.off()
}
