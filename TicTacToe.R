tictactoegame <- function(move) {
  
  victory_verify = function()  {
    #verificy if we have a winner
    decvit = paste(gamestate,collapse="")   
    #horizontals
    if ((substr(decvit,1,3) %in% c("XXX","BBB"))   | ( substr(decvit,3,6) %in% c("XXX","BBB"))   | (substr(decvit,7,9) %in% c("XXX","BBB")) | 
      (paste(substr(decvit,1,1),substr(decvit,4,4),substr(decvit,7,7),sep="") %in% c("XXX","BBB")) | # verticals
      (paste(substr(decvit,2,2),substr(decvit,5,5),substr(decvit,8,8),sep="") %in% c("XXX","BBB")) |
      (paste(substr(decvit,3,3),substr(decvit,6,6),substr(decvit,9,9),sep="") %in% c("XXX","BBB")) |
      (paste(substr(decvit,1,1),substr(decvit,5,5),substr(decvit,9,9),sep="") %in% c("XXX","BBB")) |  #transversals
      (paste(substr(decvit,7,7),substr(decvit,5,5),substr(decvit,3,3),sep="") %in% c("XXX","BBB")) 
    ){
      return(TRUE)
    }
    return(FALSE)
  }
  
  game_end = FALSE;
  # Restart the environment
  if (move==0){
    # restart the state of the game
    gamestate <<- c(replicate(9,"." ))
    # creating the table
    plot(0,type='n',axes=FALSE,ann=FALSE)
    abline(v=.87)
    abline(v=1.14)
    abline(h=-.34)
    abline(h=.32) 
    #IA first play is random
     pj = sample(9,1)
     #pj = 4  # To specify the firt move of the IA
     gamestate[pj] <<- "X"
   }
   else {
     # human plays
     # verify human move
     if (gamestate[move] != "."  ) {
       stop("Ilegal Move!")
     }
     gamestate[move] <<- "B"
     if(victory_verify() == TRUE && game_end != TRUE){
       game_end = TRUE
       warning("Human Wins")
     }
     
     #IA Plays
     #Consults the model for a sugestion (policy)
     move =  modelottt$Policy[paste(gamestate,collapse="")]
     move = as.integer(substr(move,2,2))
     # Verifying if the move is not empty or if the move is ilegal
     
     if (is.na(move) | gamestate[move] != "." ) {
        #registro da move sem policy
        print(paste0("ilegal move: ", ifelse(is.na(move)," NA ", " Invalid, "), paste(gamestate,collapse="")))
        move = regexpr(".", paste0(gamestate,collapse=""), fixed=T)[1]
     }
     #movement of IA 
     gamestate[move] <<- "X"
     if(victory_verify() == TRUE && game_end != TRUE){
       game_end = TRUE
       warning("IA Wins")
     }
  }
  #plots 
  text(0.735,.8,labels=ifelse(gamestate[1]=="."," ",gamestate[1]),cex=7)
  text(1,.8,labels=ifelse(gamestate[2]=="."," ",gamestate[2]),cex=7)
  text(1.257,.8,labels=ifelse(gamestate[3]=="."," ",gamestate[3]),cex=7)
  text(0.735,0,labels=ifelse(gamestate[4]=="."," ",gamestate[4]),cex=7)
  text(1,0,labels=ifelse(gamestate[5]=="."," ",gamestate[5]),cex=7)
  text(1.257,0,labels=ifelse(gamestate[6]=="."," ",gamestate[6]),cex=7)
  text(0.735,-.73,labels=ifelse(gamestate[7]=="."," ",gamestate[7]),cex=7)
  text(1,-.73,labels=ifelse(gamestate[8]=="."," ",gamestate[8]),cex=7)
  text(1.257,-.73,labels=ifelse(gamestate[9]=="."," ",gamestate[9]),cex=7)
  
  # Detects the end of the game
  end = regexpr(".", paste0(gamestate,collapse=""), fixed=T)[1]
  if (end==-1)  {
    game_end = TRUE
    warning("End Game!")
  }
}
  