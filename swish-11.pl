husband(jajajaj, none).
wife(blabla, none).
daughter(none, daughter).
sonsonson(none, sonsonson).
sonsonson(none, sonsonson).


sonsondaughter(none, sonsondaughter).


son(none, none).
son(ha, none).
sonson(none, none).
sondaughter(none, none).
father(none, none).
fatherfather(none, none).
fathermother(none, none).
mother(amina, none).
mothermother(none, none).
realsister(realsis, none).
parentalsister(iexist, none).
realbrother(realbro, none).
parentalbrother(none, none).
maternalsister(sis, none).
maternalbrother(hey1, none).


:- use_module(library(clpr)).
/*this function supports up to 4 males and 4 females will help in figuring out shares of males and females 
 * in the knowledge base of table 2*/
sharesTwoTimesNtoOneTimesNDistributer(Share, MalesShare, FemalesShare,Numberofmales,Numberoffemales) :- 
    Numberofmales==1, Numberoffemales==1 -> MalesShare = Share*2/3, FemalesShare = Share*1/3 
    ; 
    (        
     Numberofmales==1, Numberoffemales==2->   MalesShare = Share*2/4, FemalesShare = Share*1/4
     ;   
      (   
      Numberofmales==2, Numberoffemales==2 ->   MalesShare = Share*2/6, FemalesShare = Share*1/6
      ;   
      (   
      Numberofmales==3, Numberoffemales==1 -> MalesShare = Share*2/7, FemalesShare = Share*1/7
      ;   
      (   
       Numberofmales==3, Numberoffemales==2 -> MalesShare = Share*2/8, FemalesShare = Share*1/8
      ;   
      (   
          Numberofmales==3, Numberoffemales==3 -> MalesShare = Share*2/9, FemalesShare = Share*1/9
      ;   
         (    Numberofmales==4, Numberoffemales==1 -> MalesShare = Share*2/9, FemalesShare = Share*1/9
         ;   
         (  
             Numberofmales==4, Numberoffemales==2-> MalesShare = Share*2/10, FemalesShare = Share*1/10
         ;   
           (     Numberofmales==4, Numberoffemales==3 -> MalesShare = Share*2/11, FemalesShare = Share*1/11
           ;   
           (       Numberofmales==4, Numberoffemales==4, MalesShare = Share*2/12, FemalesShare = Share*1/12
           ;   
           (   
                Numberofmales==2, Numberoffemales==1->   MalesShare = Share*2/5, FemalesShare = Share*1/5
           ;    true
           )
        )
		)
      )
      )
      )
      )
      )
      )
      )
    .

	


/*Heuristics from the inheritance Table: this is the knowledge table N°1*/
husbandWithNoChildrenPart(L) :- countNumberOfHusband(H), H==1->   findall((X,Y,1/2),( husband(X,Y)), L), 
    write(L) ;  true.
husbandWithChildrenPart(L) :- countNumberOfHusband(H), H==1-> findall((X,Y,1/4),( husband(X,Y)), L) , write(L);  true.
wifeWithNoChildrenPart(L) :- countNumberOfWife(W), W==1->   findall((X,Y,1/4),( wife(X,Y)), L),
     write(L) ;  true.
wifeWithChildrenPart(L) :- countNumberOfWife(W), W==1->  findall((X,Y,1/8),( wife(X,Y)), L),  write(L); true.
daughterWithoutSon(L) :- countNumberOfDaughters(N), M=2/(3*N),N>=2 ->  
    findall((X,Y,M),( daughter(X,Y)), L),  write(L)
    ; 
    (   countNumberOfDaughters(N), N==1->  
    findall((X,Y,1/2),( daughter(X,Y)), L),  write(L);
    true)
    .


sonDaughterWithoutSDSS(L):- countNumberOfSonDaughters(N), M= 2/(3*N), N>=2 ->    /*SD without SDSS*/
    findall((X,Y,M),( sondaughter(X,Y)), L),  write(L)
    ; 
   (   countNumberOfSonDaughters(N), N==1->  
    findall((X,Y ,1/2),( sondaughter(X,Y)), L),  write(L)
   ;   
   true
   )
    .
sonDaughterWithoutDaughterOnly(L):-  countNumberOfSonDaughters(N), N==1->   /*son daughter with presence of the son sister*/  
    findall((X,Y,1/6),( sondaughter(X,Y)), L),  write(L);
    true
    .

sSDwithoutSDSSSDSSS(L):- 
    countNumberOfSSD(N),C is 3*N ,M = 2/C, N>=2 ->    /*SD without SDSS*/
    findall((X,Y,M), ( sonsondaughter(X,Y)), L),  write(L)
    ; 
    (     countNumberOfSSD(N), N==1 ->  
    findall((X,Y,1/2),( sonsondaughter(X,Y)), L),  write(L) ;
    true 
    )
    .
sSDwithDaughterOrSonDaughter(L):-  countNumberOfSSD(N), N==1 -> 
     findall((X,Y,1/6),( sonsondaughter(X,Y)), L),  write(L)
    ;   (    countNumberOfSSD(N), M=1/(6*N),N>=2 -> 
     findall((X,Y,M),( sonsondaughter(X,Y)), L),  write(L)
        ;   
    true )
    .
fatherWithChildren(L):-countNumberOfFather(F), F==1->    findall((X,Y,1/6),( father(X,Y)), L),  write(L)
    ;    true.
motherWithChildrenOrSiblingsOrHusbandplusFather(L):-countNumberOfMother(M), M==1->  
    findall((X,Y,1/6),( mother(X,Y)), L),  write(L); true.
motherWithWifePlusFather(L):-countNumberOfMother(M), M==1-> 
    findall((X,Y,1/4),( mother(X,Y)), L),  write(L); true.
motherThirdCase(L):-countNumberOfMother(M), M==1->  findall((X,Y,1/3),( mother(X,Y)), L),  write(L) ; true.
fatherfatherPresenceOfChildrenAndNoFather(L):- countNumberOfFatherFather(FF), FF==1 ->  
    		findall((X,Y,1/6),( fatherfather(X,Y)), L),  write(L) ; true.
    									
fathermotherNoFatherNoMother(L):- countNumberOfFatherMother(FM), FM=1 ->  
    findall((X,Y,1/6),( fathermother(X,Y)), L),  write(L) ; true.

mothermotherNoMother(L):- countNumberOfMotherMother(MM), MM=1 ->  
    findall((X,Y,1/6),( fathermother(X,Y)), L),  write(L) ; true.

realsisterNoChNoFNoFFNoRB(L):- countNumberOfRealSisters(RS), C is 3*RS, S=2/C, RS>=2 ->  
    findall((X,Y,S),( realsister(X,Y)), L),  write(L)
    ;   
    (   countNumberOfRealSisters(RS), RS==1->
    findall((X,Y,1/2),( realsister(X,Y)), L),  write(L);
    true
    ).

parentalsisterNoChNoFNoFFNoRBNoRSNoPB(L):- countNumberOfParentalSisters(PS), C is 3*PS, S=2/C, PS>=2 ->  
    findall((X,Y,S),( parentalsister(X,Y)), L),  write(L)
    ;   
    (    countNumberOfParentalSisters(PS), PS==1->
    findall((X,Y,1/2),( parentalsister(X,Y)), L),  write(L);
    true
    ).
parentalsisterWithOneRealSister(L):- countNumberOfParentalSisters(PS), C is 6*PS, S=1/C, PS>=2 ->  
    findall((X,Y,S),( parentalsister(X,Y)), L),  write(L)
    ;   
    (    countNumberOfParentalSisters(PS), PS==1->
    findall((X,Y,1/6),( parentalsister(X,Y)), L),  write(L); true
    
    ).

maternalbrotherNoChNoFNoFF(L):- countNumberOfMaternalSiblings(MS), C is 3*MS, S = 1/C, MS >= 2 ->  
    findall((X,Y,S),( maternalbrother(X,Y)), L),  write(L)
    ;   
    (   countNumberOfMaternalSiblings(MS), MS==1 ->   
    findall((X,Y,1/6),( maternalbrother(X,Y)), L),  write(L)
    ;   
    true
     )
    .
    
maternalsisterNoChNoFNoFF(L):- countNumberOfMaternalSiblings(MS), C is 3*MS, S = 1/C, MS >= 2 ->  
    findall((X,Y,S),( maternalsister(X,Y)), L),  write(L)
    ;   
    (   countNumberOfMaternalSiblings(MS), MS==1 ->   
    findall((X,Y,1/6),( maternalsister(X,Y)), L),  write(L)
    ;   
    true
     )
    .
maternalsisterAndbrotherNoChNoFNoFF(L):- countNumberOfMaternalSiblings(MS), C is 3*MS, S = 1/C, MS >= 2 ->  
    findall((X,Y,S),( maternalsister(X,Y); maternalbrother(X,Y)), L),  write(L)
    ;   
    true
    .
maternalsiblingsNoChNoFNoFF(L):- 
    countNumberOfMaternalBrothers(MB), MB==0 ->
    	maternalsisterNoChNoFNoFF(L)
    ;   
    (  
    countNumberOfMaternalSisters(MS), MS==0 ->  
    	maternalbrotherNoChNoFNoFF(L)
    ;   
    (   
    countNumberOfMaternalBrothers(MB), countNumberOfMaternalSisters(MS), MB>0, MS>0 ->
    maternalsisterAndbrotherNoChNoFNoFF(L)
    )
    )
    .

/* Knowledge Base of the second table what we could do */ 
sonWithoutDaughter(L) :- 
    countNumberOfSons(N), M=2/(3*N),N>=2 ->  
    findall((X,Y,M),( son(X,Y)), L),  write(L)
    ; 
    (   countNumberOfSons(N), N==1->  
    findall((X,Y,1/2),( son(X,Y)), L),  write(L);
    true)
    .
oneOrManySonsAndOneOrManySisters :- 
     countNumberOfSons(N), countNumberOfDaughters(D), N==1, D==1 -> 
    sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, N, D),
    findall((X,Y,M),( son(X,Y)), L),  write(L), findall((X,Y,F),( daughter(X,Y)), LL), 
    write(LL)
    ; 
    (   countNumberOfSons(N), countNumberOfDaughters(D), N>1, D>1 -> 
    sharesTwoTimesNtoOneTimesNDistributer(2/3, M, F, N, D),
    findall((X,Y,M),( son(X,Y)), L),  write(L), findall((X,Y,F),( daughter(X,Y)), LL), 
    write(LL)
    ;   true)
    .
sonsonWithoutSonDaughter(L):- countNumberOfSonSons(N), M= 2/(3*N), N>=2 ->    /*SD without SDSS*/
    findall((X,Y,M),( sonson(X,Y)), L),  write(L)
    ; 
   (   countNumberOfSonSons(N), N==1->  
    findall((X,Y ,1/2),( sonson(X,Y)), L),  write(L)
   ;   
   true
   )
    .
sonsonWithoutSonDaughterWithDaughter(L):- countNumberOfSonSons(N), M= 1/(6*N), N>=2 ->    /*SD without SDSS*/
    findall((X,Y,M),( sonson(X,Y)), L),  write(L)
    ; 
   (   countNumberOfSonSons(N), N==1->  
    findall((X,Y ,1/6),( sonson(X,Y)), L),  write(L)
   ;   
   true
   )
    .
oneOrManySonSonAndOneOrManySonDaughters:-
     countNumberOfSonSons(N), countNumberOfSonDaughters(D),countNumberOfDaughters(DD), N==1, D==1, DD==0 -> 
    sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, N, D),
    findall((X,Y,M),( sonson(X,Y)), L),  write(L), findall((X,Y,F),( sondaughter(X,Y)), LL), 
    write(LL)
    ; (   
      countNumberOfSonSons(N), countNumberOfSonDaughters(D),countNumberOfDaughters(DD), (  N>1 ; D>1), DD==0 -> 
    sharesTwoTimesNtoOneTimesNDistributer(2/3, M, F, N, D),
    findall((X,Y,M),( sonson(X,Y)), L),  write(L), findall((X,Y,F),( sondaughter(X,Y)), LL), 
    write(LL);
    true)
    .
oneOrManySonSonAndOneOrManySonDaughtersWithDaughter:- 
    countNumberOfSonSons(N), countNumberOfSonDaughters(D),countNumberOfDaughters(DD), N>=1, D>=1, DD==1 -> 
    sharesTwoTimesNtoOneTimesNDistributer(1/6, M, F, N, D),
    findall((X,Y,M),( sonson(X,Y)), L),  write(L), findall((X,Y,F),( sondaughter(X,Y)), LL), 
    write(LL);
    true.

sSSwithoutSonSonDaughter(L):- 
    countNumberOfSSS(N),C is 3*N ,M = 2/C, N>=2 ->    /*SD without SDSS*/
    findall((X,Y,M), ( sonsonson(X,Y)), L),  write(L)
    ; 
    (     countNumberOfSSS(N), N==1 ->  
    findall((X,Y,1/2),( sonsonson(X,Y)), L),  write(L) ;
    true 
    )
    .
sSSwithoutSonSonDaughterWithSonDaughterorDaughter(L):- 
    countNumberOfSSS(N),C is 6*N ,M = 1/C, N>=2 ->    /*SD without SDSS*/
    findall((X,Y,M), ( sonsonson(X,Y)), L),  write(L)
    ; 
    (     countNumberOfSSS(N), N==1 ->  
    findall((X,Y,1/6),( sonsonson(X,Y)), L),  write(L) ;
    true 
    )
    .
oneOrManySSSAndOneOrManySSD:- 
     countNumberOfSSS(N), countNumberOfSSD(D),countNumberOfSonDaughters(SonDaughter), countNumberOfDaughters(Daughter)
    , N==1, D==1, Daughter==0, SonDaughter==0 -> 
    sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, N, D),
    findall((X,Y,M),( sonsonson(X,Y)), L),  write(L), findall((X,Y,F),( sonsondaughter(X,Y)), LL), 
    write(LL)
    ; (   countNumberOfSSS(N), countNumberOfSSD(D),countNumberOfSonDaughters(SonDaughter), countNumberOfDaughters(Daughter)
    , (   N>1; D>1), Daughter==0, SonDaughter==0 -> 
    sharesTwoTimesNtoOneTimesNDistributer(2/3, M, F, N, D),
    findall((X,Y,M),( sonsonson(X,Y)), L),  write(L), findall((X,Y,F),( sonsondaughter(X,Y)), LL), 
    write(LL)
   ; true)
    .
oneOrManySSSAndOneOrManySSDWithDaughterOrSonDaughter:- countNumberOfDaughters(D), countNumberOfSonDaughters(SD),
     countNumberOfSSS(N), countNumberOfSSD(SSD), (   D==1; SD==1), N>=1,SSD>=1 ->  
    sharesTwoTimesNtoOneTimesNDistributer(1/6, M, F, N, D),
    findall((X,Y,M),( sonsonson(X,Y)), L),  write(L), findall((X,Y,F),( sonsondaughter(X,Y)), LL), 
    write(LL);
    true.


/*Count Numbers of family members: used to/ Knowledge base number 2 that will be used in 
 * knowledge base 1 and the inference engine functions*/
countNumberOfDaughters(N):- aggregate_all(count, daughter(_, daughter), N).
countNumberOfSons(N):- aggregate_all(count, son(_, son), N).
countNumberOfSonSons(N):- aggregate_all(count, sonson(_, sonson), N).
countNumberOfSonDaughters(N):- aggregate_all(count, sondaughter(_, sondaughter), N).
countNumberOfSSD(N):- aggregate_all(count, sonsondaughter(_, sonsondaughter), N).
countNumberOfSSS(N):- aggregate_all(count, sonsonson(_, sonsonson), N).
countNumberOfChildren(N):- countNumberOfDaughters(A), countNumberOfSons(Z),
    countNumberOfSonSons(E), countNumberOfSonDaughters(R), countNumberOfSSD(T), 
    countNumberOfSSS(Y), N is (A+Z+E+R+T+Y).
countNumberOfRealSisters(N):- aggregate_all(count, realsister(_, realsister), N).
countNumberOfParentalSisters(N):- aggregate_all(count, parentalsister(_, parentalsister), N).
countNumberOfRealBrothers(N):- aggregate_all(count, realbrother(_, realbrother), N).
countNumberOfParentalBrothers(N):- aggregate_all(count, parentalbrother(_, parentalbrother), N).
countNumberOfMaternalBrothers(N):- aggregate_all(count, maternalbrother(_, maternalbrother), N).
countNumberOfMaternalSisters(N):- aggregate_all(count, maternalsister(_, maternalsister), N).
/*These will either give one or zero*/
countNumberOfHusband(N):- aggregate_all(count, husband(_, husband), N).
countNumberOfFather(N):- aggregate_all(count, father(_, father), N).
countNumberOfWife(N):- aggregate_all(count, wife(_, wife), N).
countNumberOfMother(N):- aggregate_all(count, mother(_, mother), N).
countNumberOfFatherFather(N):- aggregate_all(count, fatherfather(_, fatherfather), N).
countNumberOfFatherMother(N):- aggregate_all(count, fathermother(_, fathermother), N).
countNumberOfMotherMother(N):- aggregate_all(count, mothermother(_, mothermother), N).
countNumberOfSiblings(N):- countNumberOfRealSisters(S), countNumberOfParentalSisters(PS), 
    countNumberOfRealBrothers(B), countNumberOfParentalBrothers(PB), N is (   S+PS+B+PB).
countNumberOfMaternalSiblings(N):- countNumberOfMaternalBrothers(MB), countNumberOfMaternalSisters(MS),
    N is MB+MS.

/*Get Family Members as strings used just if the deceased has either a husband or wife */
getHusband(L):- findall(X, husband(X, _), L).
getWife(L):- findall(X, wife(X, _), L).
getSon(L):- findall(X, son(X, _), L).

create_list(Item, List, [Item|List]).
/*the go(0) function will help distribute the inheritance in order*/
table1Distribution(R):-
  R==0 ->
	    husband, 
    wife, table1Distribution(1)
    ;  
    (   R==1 ->  
        (countNumberOfDaughters(N), N>0 -> nl, write("Daughter will get the following shares"), nl; true )
    ,daughter, table1Distribution(2)
         ;   
         R==2 ->  
    				 (countNumberOfSonDaughters(N), N>0 -> nl, write("Daughter will get the following shares"), nl; true )
    				,sonDaughter, table1Distribution(3)
    	;   (   
            R==3->  
                        (countNumberOfSSD(N), N>0 -> nl, write("Daughter will get the following shares:"), nl; true )
                ,sonsonDaughter, table1Distribution(4)
            ;   
            (   
            R==4->  (  write("Father will get the following shares:") ),father, table1Distribution(5)
            ;   
            (   
            R==5-> (  write("Mother will get the following shares")  ), mother, table1Distribution(6) 
            ;   
            (
            R==6-> (   write("The Father's father will get the following shares") ), fatherfather, table1Distribution(7)
            ;   
            (   
            R==7 -> (  write("The Father's mother will get the following shares")  ), fathermother, table1Distribution(8)
            ;   
            (  
            R==8 -> (  write("The Mother's mother will get the following shares")  ),  mothermother, table1Distribution(9)
            ;   
            (   
            R==9 -> (  write("The full sister(s) will get the following shares")  ),realsister, table1Distribution(10)
            ;   
            (  
            R==10 ->(  write("The parental sister(s) will get the following shares")  ),  parentalsister, table1Distribution(11)
            ;   
            (   
            R==11 -> (   write("The maternal sibling(s) will get the follwing shares") ),
                maternalsibling /*Distributes shares here regardless of gender*/
            ))))))))))   
    , 
nl,
undo.

table2Distribution(R) :- 
    R==0 ->  
          doesOneOrMoreSonExist, table2Distribution(1) 
    ; 
    (  R==1 -> doesOneOrMoreSonSonExist, table2Distribution(2)  ;
    (   
     R==2 ->   doesOneOrMoreSonSonSonExist 
    )
    )
    .
/* Main Function*/
distribute:- 
    table1Distribution(0).
              
              
/*Inference Engine where we apply the rules on each family member, if they don"t exist we return nothing to them
 * if they exist we give them their share here the knowledge base will be used*/
/*Inference Engine Of the first table*/
husband :- 
countNumberOfChildren(C), C>=1->
   			husbandWithChildrenPart(_)
    		;   
    		 husbandWithNoChildrenPart(_)
    		.
		
wife :- 
   countNumberOfChildren(C), C>=1->  
   wifeWithChildrenPart(_)
    ; 
    (   countNumberOfChildren(C), C==0->
   wifeWithNoChildrenPart(_) ; true )
    
.

daughter:- 
    countNumberOfSons(N), N==0 ->
    daughterWithoutSon(_)
    ;  true
    .
    
sonDaughter:- 
     countNumberOfSons(N), countNumberOfSonSons(M), countNumberOfDaughters(P), ( N==0 , M==0, P==0) -> 
    		sonDaughterWithoutSDSS(_)
    	;   
    	(    countNumberOfSons(N), countNumberOfSonSons(M), countNumberOfDaughters(P), ( N==0, M==0, P==1) ->  
    			sonDaughterWithoutDaughterOnly(_) ; true
            
        )
    .
sonsonDaughter:- 
    countNumberOfSons(N), countNumberOfSonSons(M), countNumberOfDaughters(P), countNumberOfSSS(W), countNumberOfSonDaughters(K)
    , ( N==0 , M==0, P==0,K==0,W==0) -> 
    		sSDwithoutSDSSSDSSS(_)
    	;   
    	( countNumberOfSons(N), countNumberOfSonSons(M), countNumberOfDaughters(P), countNumberOfSSS(W), countNumberOfSonDaughters(K),
        ( N==0 , M==0, (P==1;K==1),W==0) ->  
    			
            sSDwithDaughterOrSonDaughter(_); true
        )
    .
father:-
    	countNumberOfChildren(N), N>0 ->  
    fatherWithChildren(_)
    ;   
    true
    .
mother :- countNumberOfChildren(C),
    countNumberOfSiblings(S), countNumberOfHusband(H), countNumberOfFather(F),
    (   C>0; S>0; (   H==1, F==1))->  motherWithChildrenOrSiblingsOrHusbandplusFather(_)
    ;   
    (   countNumberOfWife(W), countNumberOfFather(F), W==1, F==1 ->  motherWithWifePlusFather(_)
    ;   
    motherThirdCase(_), true
     )
    .
    				
fatherfather :- countNumberOfChildren(C), countNumberOfFather(F), C>0, F==0 ->  
    fatherfatherPresenceOfChildrenAndNoFather(_) ; true.

fathermother :- countNumberOfFather(F), countNumberOfMother(M), F==0, M==0 ->  
    fathermotherNoFatherNoMother(_);
    true.
mothermother :- countNumberOfMother(M), M==0 ->  
    mothermotherNoMother(_);
    true.

realsister:- countNumberOfFather(F),countNumberOfFatherFather(FF),
    countNumberOfRealBrothers(RB), countNumberOfChildren(C), F==0, FF==0, RB==0, C==0 ->  
    realsisterNoChNoFNoFFNoRB(_) 
    ;
    true.

parentalsister:- countNumberOfFather(F),countNumberOfFatherFather(FF),
     countNumberOfRealBrothers(RB), countNumberOfChildren(C), 
     countNumberOfParentalBrothers(PB), countNumberOfRealSisters(RS),
     F==0, FF==0, RB==0, C==0,PB==0, RS==0 ->  
		parentalsisterNoChNoFNoFFNoRBNoRSNoPB(_)
    	;   
        (   countNumberOfFather(F),countNumberOfFatherFather(FF),
     countNumberOfRealBrothers(RB), countNumberOfChildren(C), 
     countNumberOfParentalBrothers(PB), countNumberOfRealSisters(RS),
     F==0, FF==0, RB==0, C==0,PB==0, RS==1 ->  
        parentalsisterWithOneRealSister(_)
        ;   
        true
        ).

maternalsibling :- countNumberOfFather(F),countNumberOfFatherFather(FF), countNumberOfChildren(C), 
    F==0, FF==0, C==0 -> maternalsiblingsNoChNoFNoFF(_) ; true. 
    
/* Inference Engine of the second table */
doesOneOrMoreSonExist :- countNumberOfSons(S), countNumberOfDaughters(D), S==1, D==0 ->  
    sonWithoutDaughter(_) 
    ;    (   countNumberOfSons(S), countNumberOfDaughters(D), S>0, D>0 ->   
         oneOrManySonsAndOneOrManySisters
         ;    
         true )									
    		.
doesOneOrMoreSonSonExist :-
   countNumberOfSonDaughters(SD), countNumberOfDaughters(D), SD==0, D==0 ->   sonsonWithoutSonDaughter(_)
    ;   
     (  countNumberOfSonDaughters(SD), countNumberOfDaughters(D), SD==0, D==1 -> 
     				sonsonWithoutSonDaughterWithDaughter(_);
     (   countNumberOfSonDaughters(SD), countNumberOfDaughters(D), SD>0, D==0 ->  
     oneOrManySonSonAndOneOrManySonDaughters;
     (   countNumberOfSonDaughters(SD), countNumberOfDaughters(D), SD>0, D==1 ->  
     oneOrManySonSonAndOneOrManySonDaughtersWithDaughter; true
     )
     )
     
     )
    		.

doesOneOrMoreSonSonSonExist :- 
     countNumberOfSSD(SSD),countNumberOfSonDaughters(SD), countNumberOfDaughters(D), SD==0, D==0 , SSD==0->  
    sSSwithoutSonSonDaughter(_)
    ;   
     (   countNumberOfSSD(SSD),countNumberOfSonDaughters(SD), countNumberOfDaughters(D),(    SD==1; D==1), SSD==0 -> 
     				sSSwithoutSonSonDaughterWithSonDaughterorDaughter(_);
     (    countNumberOfSSD(SSD),countNumberOfSonDaughters(SD), countNumberOfDaughters(D), SSD>0, D==0, SD==0 ->  
     oneOrManySSSAndOneOrManySSD;
     (   countNumberOfSSD(SSD),countNumberOfSonDaughters(SD), countNumberOfDaughters(D), SSD>0, (   D==1; SD==1) ->  
     oneOrManySSSAndOneOrManySSDWithDaughterOrSonDaughter; true
     )
     )
     
     )
    .
    
    

/*How to verify something */
verify(S) :-
(yes(S)
->
true ;
(no(S)
->
fail ;
ask(S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

