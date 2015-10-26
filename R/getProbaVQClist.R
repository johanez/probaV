# get quality mask values from bit code

getProbaVQClist <- function(){
  
  all_bits <- as.data.frame(t(sapply(1:256, FUN=function(x) as.integer(intToBits(x)[1:8]))))
  
  all_bits$value <- 1:256
  all_bits$cloud <- all_bits[,1]==1 & all_bits[,2]==1 & all_bits[,3]==0
  

  all_bits$shadow <- all_bits[,1]==1 & all_bits[,2]==0 & all_bits[,3]==0
  
  all_bits$sea <-all_bits[,4]==0
  
  all_bits$clear_all <- rowSums(all_bits[,1:3])==0 & rowSums(all_bits[,c(4:8)])==5
  
  all_bits$clear_noswir <- rowSums(all_bits[,1:3])==0 & rowSums(all_bits[,c(4,6:8)])==4
  all_bits$clear_r <- rowSums(all_bits[,1:3])==0 & rowSums(all_bits[,c(4,7)])==2
  all_bits$clear_b <- rowSums(all_bits[,1:3])==0 & rowSums(all_bits[,c(4,8)])==2
  all_bits$clear_nir <- rowSums(all_bits[,1:3])==0 & rowSums(all_bits[,c(4,6)])==2
  all_bits$clear_ndvi <- rowSums(all_bits[,1:3])==0 & rowSums(all_bits[,c(4,6,7)])==3
  all_bits$clear_swir <- rowSums(all_bits[,1:3])==0 & rowSums(all_bits[,4:5])==2
  
 
  QC_list <- apply(all_bits[,-c(1:9)], 2, FUN = which)
  
  all_bits$r <- all_bits$clear_noswir * 255
  all_bits$g <- all_bits$sea * 100
  all_bits$b <- all_bits$cloud * 200
  all_bits$b[all_bits$shadow] <-  100
  
  QC_list$all_bits <- all_bits
  QC_list$clear_rad_list <- QC_list[c("clear_r", "clear_nir", "clear_b", "clear_swir")]
  #write.table(all_bits[,c("r","g", "b")],row.names = T, col.names = F, quote=F,
  #            file = file.path(path, "probav/meta/sm_colormap.clr"))
  
  return(QC_list)
}


