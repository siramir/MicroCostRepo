l <-  list.files(path = library.dir)
l <- l[grep("\\.R",l)]
tilde <- grep("\\.R~",l)
if (length(tilde) > 0)
  l <- l[-tilde]
l <-  l[l!= "Readlibrary.dir"]
s <- sapply(l,FUN =  function(x,library.dir){
  source(file.path(library.dir,x))}, library.dir)
rm(s,l,tilde)