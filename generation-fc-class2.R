
fn.initial=function(popsize,allnumber,maxnumber, seed){
  set.seed(seed)
  initial<-data.frame(matrix(0,popsize, allnumber))
  for(temp.t1 in 1:popsize){
    initial[temp.t1,] = sample(seq(0.01, 1, length.out=100 ),allnumber)
  }
  return(initial)
}

fn.cal.newfatures = function(initial,c.p, numtrain, seed){
  
  f.all.all = data.frame(matrix(,numtrain,))
  
  for (temp.t2 in 1:dim(initial)[1]) {
    f.all = data.frame(matrix(,numtrain,))
    a <- initial[rep(temp.t2,each= numtrain),]
    
    for (temp.r2 in 1:(allnumber/maxnumber) ) {
      a1 = a[,((maxnumber*temp.r2-maxnumber+1):(maxnumber*temp.r2))]
      ave.ele.1 = apply(c.p*a1 ,1,sum)
      dis.ele.1 = sqrt(apply(c.p*((1-a1/ave.ele.1)^2),1,sum))
      
      f.all = cbind(f.all,dis.ele.1)
      
    }
    f.all2 = f.all[,-1]
    f.all.all = cbind(f.all.all,f.all2)
  }
  f.all.all = f.all.all[,-1]
  #f.all.all =scale(f.all.all)
  return(f.all.all)
}

fn.lg.fitness.accuracy.multi3 = function(fea.population){
  accuracy.p1=data.frame()
  temp.n = allnumber/maxnumber 
  popsize  = dim(fea.population)[2]/temp.n 
  for(temp.t3 in 1:popsize){
    x.fea3 = data.frame(fea.population[,(temp.n*temp.t3-temp.n+1):(temp.n*temp.t3)])
    data1 = cbind(y,x.fea3)
    
    library(nnet)
    set.seed(1234)
    model.ln <- multinom(data1$y~.,data = data1)
    
    set.seed(1234)
    pred1 = predict(model.ln,x.fea3,type = "class")
    
    accuracy.p1[temp.t3,"all"]<- mean(pred1 == data1$y)
    
  }
  return(accuracy.p1)
}

fn.lg.fitness.accuracy.multi3 = function(fea.population){
  accuracy.p1=data.frame()
  temp.n = allnumber/maxnumber 
  popsize  = dim(fea.population)[2]/temp.n 
  for(temp.t3 in 1:popsize){
    
    x.fea3 = data.frame(fea.population[,(temp.n*temp.t3-temp.n+1):(temp.n*temp.t3)])
    # x= scale(x)
    #colnames(x)  ="x"
    data = cbind(y,x.fea3)
    data1= data
    
    library(nnet)
    set.seed(1234)
    model.ln <- multinom(data1$y~.,data = data1)
    
    set.seed(1234)
    pred1 = predict(model.ln,x.fea3,type = "class")
    
    accuracy.p1[temp.t3,"all"]<- mean(pred1 == data1$y)
    
  }
  return(accuracy.p1)
}


fn.selection.st = function(accuracy, num.bestretain, seed){
  set.seed(seed)
  fit<-accuracy/sum(accuracy)
  sumfit<-cumsum(fit)
  initial.select<-data.frame(matrix(0,popsize, allnumber))
  #######beat retain
  initial.select = initial[order(accuracy,decreasing = T)[1:num.bestretain],]
  temp.random<-matrix()
  # random using other distributions
  for(temp.t4 in (1+num.bestretain):popsize){
    temp.random1<-runif(2, min = 0, max = 1)
    temp.random[1]<-length(which((sumfit-temp.random1[1])<0))+1
    temp.random[2]<-length(which((sumfit-temp.random1[2])<0))+1
    initial.select[temp.t4,]<-initial[temp.random[order(accuracy[temp.random,],decreasing = T)[1]],]
  }
  return(initial.select)
}

fn.crossover.sc.control = function(initial.select,num.bestretain,
                                   pcrossover,seed){
  set.seed(seed)
  V =dim(initial.select)[2]
  initial.best<-initial.select[1:num.bestretain,]
  
  
  
  temp.pcross = sample(popsize,floor(popsize*pcrossover+num.bestretain))
  initial.cross<-initial.select[temp.pcross,]
  initial.crossover<-rbind(initial.best,initial.select[-temp.pcross,])
  length<-floor((popsize*pcrossover)/2)
  
  for(temp.t5 in 1:length){
    # random number using other disctributions
    
    # random number using other disctributions
    
    temp.parents = sample(dim(initial.cross)[1],2) 
    cross.parents = initial.cross[temp.parents,]
    cross.parents
    
    temp.row = sort(sample(V,floor(V/2)))
    a = cross.parents[2,temp.row]
    cross.parents[2,temp.row] = cross.parents[1,temp.row]
    cross.parents[1,temp.row] = a 
    
    cross.parents
    
    # 
    # cross.parents.new = data.frame(matrix(,2,1))
    # 
    # for (temp.r1 in 1:(allnumber/maxnumber)) {
    #   
    #   cross.parents1=cross.parents[,((maxnumber*temp.r1-maxnumber+1):(maxnumber*temp.r1))]
    #   sum.c1 = apply(cross.parents1, 1,sum)
    #   cross.parents11 = round(cross.parents1/sum.c1,3)
    #   cross.parents.new = cbind(cross.parents.new,cross.parents11)
    # }
    # cross.parents.new  = cross.parents.new[,-1]
    # initial.crossover = rbind(initial.crossover,cross.parents.new )
    initial.crossover = rbind(initial.crossover,cross.parents)
    initial.cross = initial.cross[-temp.parents,]  
  }
  
  return(initial.crossover)
}

fn.mutation.spm.control = function(initial.crossover,num.bestretain, pmutation,seed){
  set.seed(seed)
  
  allnumber = dim(initial.crossover)[2]
  for(temp.t6 in 1:ceiling(pmutation*popsize*allnumber)){ 
    
    
    
    mut1= sample(c((num.bestretain+1):popsize),1)
    
    mut2 = sample(allnumber,1) 
    
    a=initial.crossover[mut1,]
    a[,mut2] =  sample(seq(0.01, 1, length.out=100 ),1)
    initial.crossover[mut1,] =a
  }
  
  # initial.crossover.new =  data.frame(matrix(,popsize,1))
  # 
  # for (temp.r1 in 1:(allnumber/maxnumber)) {
  #   
  #   initial.crossover1=initial.crossover[,((maxnumber*temp.r1-maxnumber+1):(maxnumber*temp.r1))]
  #   sum.c1 = apply(initial.crossover1, 1,sum)
  #   initial.crossover11 = round(initial.crossover1/sum.c1,3)
  #   initial.crossover.new = cbind( initial.crossover.new ,initial.crossover11)
  # }
  # initial.crossover.new  = initial.crossover.new[,-1]
  # 
  # return(generation1<- initial.crossover.new)
  return(generation1<- initial.crossover)
}



fn.best=function(populations){
  
  a<-data.frame(data.frame(populations[1])[order(data.frame(populations[1])$all,decreasing = T)[1],])
  
  
  for(i in 2:length(populations)){
    
    b<-data.frame(data.frame(populations[i])[order(data.frame(populations[i])$all,decreasing = T)[1],])
    
    a<-rbind(a,b)
  }
  return(a)
}






#setwd("F:/HEAS-phd/phase-ga/NEW2/3/100-78")


library(e1071)
library(gtools)


seed.all = read.csv(file = "seed.all.csv")[,-1]



i=1

num.ga = 50

seed2= seed.all[,(5*num.ga*i-(5*num.ga-1)):(5*num.ga*i)]





rep.ga = 1

num.iterations= 1000
popsize = 500
best.all =list()
best.pop =list()

repeat{
 
  seed1 = seed2[,(5*rep.ga-4):(5*rep.ga)]
  
  
  c.all = read.csv(file="168c.csv")
  
  y = data.frame(c.all[,1])
  colnames(y) = "y"
  formula<-y~.
  c.all$y<-factor(c.all$y,levels = c(1,2,3))
  c.p = c.all[,-1]

  #parameter
  
  numfea = 2
  maxnumber = dim(c.p)[2]
  allnumber = maxnumber*numfea
  
  
  
  numtrain = dim(c.p)[1]
  
  pcrossover = 0.9
  pmutation = 0.1
  num.bestretain = 3
  seed =seed1
  
  
  ########main 
  
  Sys.time()
  i = 1
  print(Sys.time())
  print(i)
  populations = list(num.iterations)
  initial = fn.initial(popsize,allnumber,maxnumber, seed[i,1])
  # table(apply(initial, 1, sum))
  initial[1:3,]
  
  fea.population = fn.cal.newfatures(initial,c.p,numtrain, seed)
  dim(fea.population)

 
  #### fitness  by lg 
  accuracy = fn.lg.fitness.accuracy.multi3(fea.population)
  max(accuracy)
  
  # print(initial[which.max(accuracy[,1]),])
  # print(accuracy[which.max(accuracy[,1]),])
  populations[i] = list(cbind(initial, accuracy))
  print(max(populations[[i]]$all))
  populations.all = populations[[i]]
  Sys.time()
  i=i+1
  repeat{
    print(i)
    print(Sys.time())
    accuracy.all = data.frame(accuracy$all)
    ###### selection
    initial.select = fn.selection.st(accuracy.all, num.bestretain, seed[i,3])
    
    ###### crossover
    initial.crossover = fn.crossover.sc.control(initial.select,num.bestretain,
                                                pcrossover,seed[i,4])
    
    #####mutation
    initial = fn.mutation.spm.control(initial.crossover,num.bestretain, pmutation,seed[i,5])

    #####next 
    fea.population = fn.cal.newfatures(initial,c.p,numtrain, seed)
    ####fitness
    accuracy = fn.lg.fitness.accuracy.multi3(fea.population)
    max(accuracy[,1])
    
    print(initial[which.max(accuracy[,1]),])
    print(accuracy[which.max(accuracy[,1]),])
    
    populations[i] = list(cbind(initial, accuracy))
    max.a = max(populations[[i]]$all)
    print(max.a)
    i = i + 1
    if(i > num.iterations) break()
    
  }
  ga.ln <-populations
  
  
  Sys.time()
  best<-fn.best(ga.ln)
  #fn.plot.best(ga.ln)
  #fn.plot.3(ga.ln,popsize)
  best[num.iterations,]
  ga.ln[[num.iterations]]
  best.all[[rep.ga]] = best
  best.pop[[rep.ga]] =  ga.ln[[num.iterations]]
  rep.ga = rep.ga +1
  if(rep.ga > num.ga )break()
}
  

best = data.frame()
best.final5 =  data.frame()
for (i in 1:num.ga) {
  best = rbind(best,best.all[[i]][num.iterations,])
  pop1 = best.pop[[i]]
  best.final5 = rbind(best.final5, pop1[order(pop1$all,decreasing = TRUE)[1:5],])
}

write.csv(best,file = "best-class2.csv")
write.csv(best.final5,file = "best.all-class2.csv")