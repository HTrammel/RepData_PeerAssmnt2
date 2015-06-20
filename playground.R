# remove summary
require(plyr)
require(dplyr)
require(stringr)
require(qdap)

animals <- c("Dog","Cat","Catfish","Dogfish"
             ,"Mountain Dog","Wildcat","Hound dog"
             ,"Dirty Dog","Cattle Dog", "Dog/Mixed Breed"
             ,"Dog/ Purebred", "Calico Cat", "Cat Stray")

animals <- gsub("Cat","Feline", animals)

print(animals)