library('jsonlite');
library("uuid");

computeSF36 <- function(json) {
  #physical
  a <- as.numeric(json$items$'2'$subquestions$'0'$response_value)+1;
  b <- as.numeric(json$items$'2'$subquestions$'1'$response_value)+1;
  c <- as.numeric(json$items$'2'$subquestions$'2'$response_value)+1;
  d <- as.numeric(json$items$'2'$subquestions$'3'$response_value)+1;
  e <- as.numeric(json$items$'2'$subquestions$'4'$response_value)+1;
  f <- as.numeric(json$items$'2'$subquestions$'5'$response_value)+1;
  g <- as.numeric(json$items$'2'$subquestions$'6'$response_value)+1;
  h <- as.numeric(json$items$'2'$subquestions$'7'$response_value)+1;
  i <- as.numeric(json$items$'2'$subquestions$'8'$response_value)+1;
  j <- as.numeric(json$items$'2'$subquestions$'9'$response_value)+1;
  sum <-a+b+c+d+e+f+g+h+i+j;
  phy_score <- (sum-10)/20*100;
  physical_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Physical Health'), timestamp=unbox(''), value=unbox(phy_score), unit=unbox(''));
  
  #role-physical
  a <- as.numeric(json$items$'3'$subquestions$'0'$response_value)+1;
  b <- as.numeric(json$items$'3'$subquestions$'1'$response_value)+1;
  c <- as.numeric(json$items$'3'$subquestions$'2'$response_value)+1;
  d <- as.numeric(json$items$'3'$subquestions$'3'$response_value)+1;
  sum <- a+b+c+d;
  role_phy_score <- (sum-4)/4*100;
  role_physical_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Role-Physical Health'), timestamp=unbox(''), value=unbox(role_phy_score), unit=unbox(''));
  
  #bodily pain
  a <- as.numeric(json$items$'6'$response_value)+1;
  b <- as.numeric(json$items$'7'$response_value)+1;
  if(b==1){if (a==1) b<-6 else b<-5} else b<-6-b;
  if (a==1) {a <- 6} else if (a==2) {a <- 5.4} else if (a==3) {a <- 4.2} else if (a==4) {a <- 3.1} else if (a==5) {a <- 2.2} else if (a==6) {a <-1};
  sum <- a+b;
  pain_score <- (sum-2)/10*100;
  pain_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Bodily Pain'), timestamp=unbox(''), value=unbox(pain_score), unit=unbox(''));
  
  #general health
  a <- as.numeric(json$items$'0'$response_value)+1;
  b <- as.numeric(json$items$'10'$subquestions$'0'$response_value)+1;
  c <- as.numeric(json$items$'10'$subquestions$'1'$response_value)+1;
  d <- as.numeric(json$items$'10'$subquestions$'2'$response_value)+1;
  e <- as.numeric(json$items$'10'$subquestions$'3'$response_value)+1;
  if (a==1) {a <- 5} else if (a==2) {a <- 4.4} else if (a==3) {a <- 3.4} else if (a==4) {a <- 2.0} else if (a==5) {a <- 1};
  c <- 5-c;
  e <- 5-e;
  sum <- a+b+c+d+e;
  general_health_score <- (sum-5)/20*100;
  general_health_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('General Health'), timestamp=unbox(''), value=unbox(general_health_score), unit=unbox(''));
  
  #vitality
  a <- as.numeric(json$items$'8'$subquestions$'0'$response_value)+1;
  e <- as.numeric(json$items$'8'$subquestions$'4'$response_value)+1;
  g <- as.numeric(json$items$'8'$subquestions$'6'$response_value)+1;
  i <- as.numeric(json$items$'8'$subquestions$'8'$response_value)+1;
  a <- 6-a;
  e <- 6-e;
  sum <- a+e+g+i;
  vitality_score <- (sum-4)/20*100;
  vitality_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Vitality'), timestamp=unbox(''), value=unbox(vitality_score), unit=unbox(''));
  
  #social
  a <- as.numeric(json$items$'5'$response_value)+1;
  b <- as.numeric(json$items$'9'$response_value)+1;
  a <- 5-a;
  sum <- a+b;
  social_score <- (sum-2)/8*100;
  social_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Social'), timestamp=unbox(''), value=unbox(social_score), unit=unbox(''));
  
  #role-emotional
  a <- as.numeric(json$items$'4'$subquestions$'0'$response_value)+1;
  b <- as.numeric(json$items$'4'$subquestions$'1'$response_value)+1;
  c <- as.numeric(json$items$'4'$subquestions$'2'$response_value)+1;
  sum <- a+b+c;
  role_emotional_score <- (sum-3)/3*100;
  role_emotional_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Role-Emotional Health'), timestamp=unbox(''), value=unbox(role_emotional_score), unit=unbox(''));
  
  #mental health
  b <- as.numeric(json$items$'8'$subquestions$'1'$response_value)+1;
  c <- as.numeric(json$items$'8'$subquestions$'2'$response_value)+1;
  d <- as.numeric(json$items$'8'$subquestions$'3'$response_value)+1;
  f <- as.numeric(json$items$'8'$subquestions$'5'$response_value)+1;
  h <- as.numeric(json$items$'8'$subquestions$'7'$response_value)+1;
  d <- 6-d;
  h <- 6-h;
  sum <- b+c+d+f+g;
  mental_health_score <- (sum-5)/25*100;
  mental_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Mental'), timestamp=unbox(''), value=unbox(mental_health_score), unit=unbox(''));
  
  #health transition
  a <- as.numeric(json$items$'1'$response_value)+1;
  health_transition_score <- (a-1)/4*100;
  health_transition_list <-list(type=unbox('New'),source=unbox('OCPU'),id=unbox(UUIDgenerate()), category=unbox('sf36'), measurement=unbox('Health Transition'), timestamp=unbox(''), value=unbox(health_transition_score), unit=unbox(''));
  
  all_results <- list(physical_list, role_physical_list, pain_list, general_health_list, vitality_list, social_list, role_emotional_list, mental_list, health_transition_list);
  
  return(all_results);
}