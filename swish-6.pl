


husband(none, none).
wife(blabla, wife).
/*hushaschildren(mohammed).
wifehaschildren(khadija). not good for being inconsistent with the rest of the program */
daughter(lalala, daughter).
sonsonson(none, none).
sonsondaughter(none, none).
sonsondaughter(none, none).
son(hamid, son).
sonson(none, none).
sondaughter(none, none).
father(none, none).
fatherfather(kingato, fatherfather).
mother(imaginary, mother).
realsister(none, none).
parentalsister(none, none).
realbrother(none, none).
parentalbrother(none, none).

/*Heuristics from the inheritance Table: this is the knowledge table*/
husbandWithNoChildrenPart(L) :- countNumberOfHusband(H), H==1->   findall((X,Y,1/2),( husband(X,Y)), L), write(L); true.
husbandWithChildrenPart(L) :-countNumberOfHusband(H), H==1-> findall((X,Y,1/4),( husband(X,Y)), L) , write(L); true.
wifeWithNoChildrenPart(L) :- countNumberOfWife(W), W==1->   findall((X,Y,1/4),( wife(X,Y)), L),  write(L); true.
wifeWithChildrenPart(L) :-countNumberOfWife(W), W==1->  findall((X,Y,1/8),( wife(X,Y)), L),  write(L); true.
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
    									


/*Count Numbers of family members: used to */
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
/*These will either give one or zero*/
countNumberOfHusband(N):- aggregate_all(count, husband(_, husband), N).
countNumberOfFather(N):- aggregate_all(count, father(_, father), N).
countNumberOfWife(N):- aggregate_all(count, wife(_, wife), N).
countNumberOfMother(N):- aggregate_all(count, mother(_, mother), N).
countNumberOfFatherFather(N):- aggregate_all(count, fatherfather(_, fatherfather), N).




countNumberOfSiblings(N):- countNumberOfRealSisters(S), countNumberOfParentalSisters(PS), 
    countNumberOfRealBrothers(B), countNumberOfParentalBrothers(PB), N is (   S+PS+B+PB).

/*Get Family Members as strings used just if the deceased has either a husband or wife */
getHusband(L):- findall(X, husband(X, _), L).
getWife(L):- findall(X, wife(X, _), L).
getSon(L):- findall(X, son(X, _), L).




test:-  getHusband(O), O\==[none].

append(L1-T1, T1-T2, L1-T2).
/*the go(0) function will help distribute the inheritance*/
go(R):-
    R==0 ->  
	   husband, wife, go(1)
    ;  
    (   R==1 ->   daughter,  go(2)
         ;   
         R==2 ->  sonDaughter, go(3) 
    	;   (   
            R==3->  sonsonDaughter, go(4)
            ;   
            (   
            R==4->  father, go(5)
            ;   
            (   
            R==5->  mother, go(6) 
            ;   
            (
            R==6->  fatherfather
            )
            )
            )
            )
    ) 
    ,
     
   
nl,
undo.



/*Inference Engine where we apply the rules on each family member, if they don"t exist we return nothing to them
 * if they exist we give them their share*/

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
    (   countNumberOfChildren(C), C>=1->
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
