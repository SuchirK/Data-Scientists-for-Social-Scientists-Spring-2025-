mirror_find <- function(){
  require(RCurl)
  mirrors <- c("http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/",
             "ftp://ftp.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/",
             "rsync://rsync.mirrorservice.org/gutenberg/",
             "ftp://eremita.di.uminho.pt/pub/gutenberg/",
             "http://eremita.di.uminho.pt/gutenberg/",
             "http://mirror.csclub.uwaterloo.ca/gutenberg/",
             "http://www.gutenberg.org/dirs/",
             "ftp://ftp.ibiblio.org/pub/docs/books/gutenberg/",
             "http://mirrors.xmission.com/gutenberg/",
             "ftp://mirrors.xmission.com/gutenberg/",
             "rsync://gutenberg.pglaf.org/gutenberg",
             "gopher://gutenberg.pglaf.org:70",
             "ftp://aleph.gutenberg.org/",
             "rsync://aleph.gutenberg.org/gutenberg/",
             "https://gutenberg.pglaf.org/",
             "ftp://gutenberg.pglaf.org",
             "http://aleph.gutenberg.org/",
             "ftp://gutenberg.readingroo.ms/gutenberg/",
             "http://gutenberg.readingroo.ms/")
  mirror <- 0
  for(i in mirrors){
    if (url.exists(i)){
      mirror <- i
      break
    }
  }
  if (mirror==0){
    return("No mirrors available")
  }
  return(mirror)
}

mirror_find

