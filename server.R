
options(digits.secs = 3)  

shinyServer(
  function(input, output,session){
  #server side management
    callModule(performance_rating, "performance_rating-module", pool)      
    }
)