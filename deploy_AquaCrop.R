#deploy

library('roxygen2')
library('devtools')

 # my_description <- list("Maintainer" =
 #                          "'Anyela V Camargo' <anyelavcamargo@gmail.com>",
 #                        "Title" = "Aquacrop for R",
 #                        "Authors@R" = 'person("Anyela", "Camargo", email = "anyelavcamargo@gmail.com",
 #                        role = c("aut", "cre"))',
 #                        "Description" = "Describes and predict crop growth and development as influenced by environmental
 #                        conditions and crop management, which are provided for the model as input data",
 #                        "Version" = '1.1',
 #                        'Imports' = c('XML', 'xml2', 'pracma', 'kulife', 'Rdpack'),
 #                        'RdMacros' = 'Rdpack')
# create('AquaCropR', my_description)
setwd("C:/anyela/repo/AquacropR")

system('rm AquaCropR.pdf')
document()
devtools::build()

install("AquaCropR") #only to deploy
path <- 'c:/anyela/repo/AquaCropR'
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", 
             shQuote(path)))

