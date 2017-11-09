Correct <<- 0
Incorrect <<- 0
Total <<- 0


Prop.Find <- function(Schedule){
  for(i in c(1:nrow(Schedule))){
    if(Schedule[i,1] < Sys.Date()){
      if(Schedule[i, 9] > Schedule[i, 10]){
        if(Schedule[i, 6] > 0){
          Correct <<- Correct + 1
          Total <<- Total + 1
        }
        else{
          Incorrect <<- Incorrect + 1
          Total <<- Total + 1
        }
      }
      else if(Schedule[i,9] < Schedule[i, 10]){
        if(Schedule[i, 6] < 0 ){
          Correct <<- Correct + 1
          Total <<- Total + 1
        }
        else{
          Incorrect <<- Incorrect + 1
          Total <<- Total + 1
        }
      }
    }
  }
}

Prop.Find(Schedule191718)
Prop.Find(Schedule1819)
Prop.Find(Schedule1920)

Prop.Find(Schedule2021)
Prop.Find(Schedule2122)
Prop.Find(Schedule2223)
Prop.Find(Schedule2324)
Prop.Find(Schedule2425)
Prop.Find(Schedule2526)
Prop.Find(Schedule2627)
Prop.Find(Schedule2728)
Prop.Find(Schedule2829)
Prop.Find(Schedule2930)

Prop.Find(Schedule3031)
Prop.Find(Schedule3132)
Prop.Find(Schedule3233)
Prop.Find(Schedule3334)
Prop.Find(Schedule3435)
Prop.Find(Schedule3536)
Prop.Find(Schedule3637)
Prop.Find(Schedule3738)
Prop.Find(Schedule3839)
Prop.Find(Schedule3940)

Prop.Find(Schedule4041)
Prop.Find(Schedule4142)
Prop.Find(Schedule4243)
Prop.Find(Schedule4344)
Prop.Find(Schedule4445)
Prop.Find(Schedule4546)
Prop.Find(Schedule4647)
Prop.Find(Schedule4748)
Prop.Find(Schedule4849)
Prop.Find(Schedule4950)

Prop.Find(Schedule5051)
Prop.Find(Schedule5152)
Prop.Find(Schedule5253)
Prop.Find(Schedule5354)
Prop.Find(Schedule5455)
Prop.Find(Schedule5556)
Prop.Find(Schedule5657)
Prop.Find(Schedule5758)
Prop.Find(Schedule5859)
Prop.Find(Schedule5960)

Prop.Find(Schedule6061)
Prop.Find(Schedule6162)
Prop.Find(Schedule6263)
Prop.Find(Schedule6364)
Prop.Find(Schedule6465)
Prop.Find(Schedule6566)
Prop.Find(Schedule6667)
Prop.Find(Schedule6768)
Prop.Find(Schedule6869)
Prop.Find(Schedule6970)

Prop.Find(Schedule7071)
Prop.Find(Schedule7172)
Prop.Find(Schedule7273)
Prop.Find(Schedule7374)
Prop.Find(Schedule7475)
Prop.Find(Schedule7576)
Prop.Find(Schedule7677)
Prop.Find(Schedule7778)
Prop.Find(Schedule7879)
Prop.Find(Schedule7980)

Prop.Find(Schedule8081)
Prop.Find(Schedule8182)
Prop.Find(Schedule8283)
Prop.Find(Schedule8384)
Prop.Find(Schedule8485)
Prop.Find(Schedule8586)
Prop.Find(Schedule8687)
Prop.Find(Schedule8788)
Prop.Find(Schedule8889)
Prop.Find(Schedule8990)

Prop.Find(Schedule9091)
Prop.Find(Schedule9192)
Prop.Find(Schedule9293)
Prop.Find(Schedule9394)
Prop.Find(Schedule9495)
Prop.Find(Schedule9596)
Prop.Find(Schedule9697)
Prop.Find(Schedule9798)
Prop.Find(Schedule9899)
Prop.Find(Schedule9900)

Prop.Find(Schedule0001)
Prop.Find(Schedule0102)
Prop.Find(Schedule0203)
Prop.Find(Schedule0304)
# 2004-2005 Season Cancelled Due to Player Lockout
Prop.Find(Schedule0506)
Prop.Find(Schedule0607)
Prop.Find(Schedule0708)
Prop.Find(Schedule0809)
Prop.Find(Schedule0910)

Prop.Find(Schedule1011)
Prop.Find(Schedule1112)
Prop.Find(Schedule1213)
Prop.Find(Schedule1314)
Prop.Find(Schedule1415)
Prop.Find(Schedule1516)
Prop.Find(Schedule1617)
Prop.Find(Schedule1718)
