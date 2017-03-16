

genSampleDataNoNoise <- function(n=100, r1=4, WriteToDisk=FALSE){
  theta = seq(0, 2*pi, length.out = n)
  x1 = r1*cos(theta)
  y1 = r1*sin(theta)
  sample1 = data.frame(x1,y1)#t(rbind(x1, y1))
  if (WriteToDisk==TRUE){
      write.table(sample1, file="sample1.txt", sep = ",", row.names = FALSE, col.names = FALSE)
  }
  return(sample1)
}

#plot(x1,y1, col='red')
#plot(x2,y2, col='blue')
  