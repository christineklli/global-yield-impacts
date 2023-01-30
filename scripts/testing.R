a <- list(data.frame(x=rnorm(10, 0, 1), crop="maize", model=1, m=1),
          data.frame(x=rnorm(10,0,1), crop="soy", model=1, m=2),
          data.frame(x=rnorm(10,1,2),crop="maize", model=1, m=3),
          data.frame(x=rnorm(10,1,2),crop="soy", model=1, m=4))


b <- a %>% rbindlist()
c <-  split(b, b$crop)
d <- lapply(1:2, function(i){
  split(c[[i]], 
        interaction(c[[i]]$model, c[[i]]$m)
  )      })
