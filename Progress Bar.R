total<-13800
progressBar <- winProgressBar(title = "Generating Discretized Feature-Set", min = 0, max = 13800, width = 500)
for(i in seq(1, total))
{
     Sys.sleep(0.00001)
     setWinProgressBar(progressBar, i, title=paste("Generating Discretized Features: ", round(i/total*100, 0), "% done", sep = ""))
}
close(progressBar)