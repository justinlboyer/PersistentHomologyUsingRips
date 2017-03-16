
genSampleDataNoise <- function(n=100, r1=4, WriteToDisk=FALSE, stdev=0.5){
  r2 = rnorm(n, r1, stdev)
  theta2 = runif(n, 0, 2*pi)
  x2 = r2*cos(theta2)
  y2 = r2*sin(theta2)
  sample2 = data.frame(x2, y2)
  if (WriteToDisk==TRUE){
    write.table(sample2, file="sample2.txt", sep = ",", row.names = FALSE, col.names = FALSE)
  }
  return(sample2)
}