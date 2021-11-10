/*Group Members: Driss Jaidi and Mohammed Chaouni
  Submission Date: 10 November 2021
  Version: 15.0
  In this program we have many functions.
  A big chunk of these function constitues the knowledge bases 1 and 2
  another set of functions constitutes the inference engine
  then we have the two function that run knowledge base 1 and 2 respectivelly table1Distribution(R) and table2Distribution(R):-
  the we have the "distribute" that runs them both
  We also have count functions that count all family members they start with "count"
  each function is in human readable form and is easy to understand just by its name
  Read the read me file if you want to see how to run the online and desktop version or just watch the video we posted in the word doc
*/

:- ensure_loaded(familytree).         /* Very important to the program*/
:- ensure_loaded(countingFunctions). /* Very important to the program*/

:- use_module(library(clpr)).


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
findall((X,Y,M+ "or rest divided with other sons if less than the share"),( son(X,Y)), L),  write(L), nl
;
(   countNumberOfSons(N), N==1->
findall((X,Y,1/2 + "or rest divided with other sons if less than the share"),( son(X,Y)), L),  write(L), nl;
true)
.
oneOrManySonsAndOneOrManySisters :-
countNumberOfSons(N), countNumberOfDaughters(D), N==1, D==1 ->
sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, N, D),
findall((X,Y,M+ "or rest divided with other sons if less than the share"),( son(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other daughters if less than the share"),( daughter(X,Y)), LL),
write(LL), nl
;
(   countNumberOfSons(N), countNumberOfDaughters(D), N>1, D>1 ->
sharesTwoTimesNtoOneTimesNDistributer(2/3, M, F, N, D),
findall((X,Y,M + "or rest divided with other son sons if less than the share"),( son(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other son sons if less than the share"),( daughter(X,Y)), LL),
write(LL), nl
;   true)
.
sonsonWithoutSonDaughter(L):- countNumberOfSonSons(N), M= 2/(3*N), N>=2 ->    /*SD without SDSS*/
findall((X,Y,M+ "or rest divided with other son sons if less than the share"),( sonson(X,Y)), L),  write(L), nl
;
(   countNumberOfSonSons(N), N==1->
findall((X,Y ,1/2+ "or rest divided with other son sons if less than the share"),( sonson(X,Y)), L),  write(L), nl
;
true
)
.
sonsonWithoutSonDaughterWithDaughter(L):- countNumberOfSonSons(N), M= 1/(6*N), N>=2 ->    /*SD without SDSS*/
findall((X,Y,M+ "or rest divided with other son sons if less than the share"),( sonson(X,Y)), L),  write(L), nl
;
(   countNumberOfSonSons(N), N==1->
findall((X,Y ,1/6+ "or rest divided with other son sons if less than the share"),( sonson(X,Y)), L),  write(L), nl
;
true
)
. 


oneOrManySonSonAndOneOrManySonDaughters:-
countNumberOfSonSons(N), countNumberOfSonDaughters(D),countNumberOfDaughters(DD), N==1, D==1, DD==0 ->
sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, N, D),
findall((X,Y,M+ "or rest divided with other son sons if less than the share"),( sonson(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other son daughters if less than the share"),( sondaughter(X,Y)), LL),
write(LL), nl
; (
countNumberOfSonSons(N), countNumberOfSonDaughters(D),countNumberOfDaughters(DD), (  N>1 ; D>1), DD==0 ->
sharesTwoTimesNtoOneTimesNDistributer(2/3, M, F, N, D),
findall((X,Y,M+ "or rest divided with other son sons if less than the share"),( sonson(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other son daughters if less than the share"),( sondaughter(X,Y)), LL),
write(LL), nl;
true)
.
oneOrManySonSonAndOneOrManySonDaughtersWithDaughter:-
countNumberOfSonSons(N), countNumberOfSonDaughters(D),countNumberOfDaughters(DD), N>=1, D>=1, DD==1 ->
sharesTwoTimesNtoOneTimesNDistributer(1/6, M, F, N, D),
findall((X,Y,M+ "or rest divided with other son sons if less than the share"),( sonson(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other son daughters if less than the share"),( sondaughter(X,Y)), LL),
write(LL), nl;
true.

sSSwithoutSonSonDaughter(L):-
countNumberOfSSS(N),C is 3*N ,M = 2/C, N>=2 ->    /*SD without SDSS*/
findall((X,Y,M+ "or rest divided with other son son son if less than the share"), ( sonsonson(X,Y)), L),  write(L), nl
;
(     countNumberOfSSS(N), N==1 ->
findall((X,Y,1/2+ "or rest divided with other son son son if less than the share"),( sonsonson(X,Y)), L),  write(L), nl ;
true
)
.
sSSwithoutSonSonDaughterWithSonDaughterorDaughter(L):-
countNumberOfSSS(N),C is 6*N ,M = 1/C, N>=2 ->    /*SD without SDSS*/
findall((X,Y,M + "or rest divided with other son son son if less than the share"), ( sonsonson(X,Y)), L),  write(L), nl
;
(     countNumberOfSSS(N), N==1 ->
findall((X,Y,1/6 + "or rest divided with other son son son if less than the share"),( sonsonson(X,Y)), L),  write(L), nl ;
true
)
.
oneOrManySSSAndOneOrManySSD:-
countNumberOfSSS(N), countNumberOfSSD(D),countNumberOfSonDaughters(SonDaughter), countNumberOfDaughters(Daughter)
, N==1, D==1, Daughter==0, SonDaughter==0 ->
sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, N, D),
findall((X,Y,M+ "or rest divided with other son son son if less than the share"),( sonsonson(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other son son daughters if less than the share"),( sonsondaughter(X,Y)), LL),
write(LL), nl
; (   countNumberOfSSS(N), countNumberOfSSD(D),countNumberOfSonDaughters(SonDaughter), countNumberOfDaughters(Daughter)
, (   N>1; D>1), Daughter==0, SonDaughter==0 ->
sharesTwoTimesNtoOneTimesNDistributer(2/3, M, F, N, D),
findall((X,Y,M+ "or rest divided with other son son son if less than the share"),( sonsonson(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other son son daughters if less than the share"),( sonsondaughter(X,Y)), LL),
write(LL), nl
; true)
.
oneOrManySSSAndOneOrManySSDWithDaughterOrSonDaughter:- countNumberOfDaughters(D), countNumberOfSonDaughters(SD),
countNumberOfSSS(N), countNumberOfSSD(SSD), (   D==1; SD==1), N>=1,SSD>=1 ->
sharesTwoTimesNtoOneTimesNDistributer(1/6, M, F, N, D),
findall((X,Y,M+ "or rest divided with other son son sons if less than the share"),( sonsonson(X,Y)), L),  write(L), nl,
findall((X,Y,F+ "or rest divided with other son son daughters if less than the share"),( sonsondaughter(X,Y)), LL),
write(LL), nl;
true.

fatherWithNoSon(L):-countNumberOfFather(F), F==1->
findall((X,Y,"rest if any"),( father(X,Y)), L), write(L), nl; true.
fatherfatherWithNoSonOrFather(L):-countNumberOfFatherFather(F), F==1->
findall((X,Y,"rest if any"),( fatherfather(X,Y)), L), write(L), nl; true.

realBrotherWithNosRealSister(L) :-  countNumberOfRealBrothers(RB), countNumberOfRealSisters(RS),C is 3*RB, S=2/C,  RB>=2, RS==0->
findall((X,Y,S  + "or rest divided with other brothers if less than the share"),( realbrother(X,Y)), L),  write(L), nl
;
(    countNumberOfRealBrothers(RB), countNumberOfRealSisters(RS), RB==1, RS==0
->  findall((X,Y,1/2 +" or rest if less than 1/2"),( realbrother(X,Y)), L),  write(L), nl;
true)
.
oneOrManyrealbrothersWithOneOrManySisters:-
countNumberOfRealBrothers(RB), countNumberOfRealSisters(RS), RB==1, RS==1 ->
sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, RB, RS),
findall((X,Y,M  + "or rest divided with other brothers if less than the share"),( realbrother(X,Y)), L),  write(L), nl,
findall((X,Y,F  + "or rest divided with other brothers if less than the share"),( realsister(X,Y)), LL),  write(LL), nl
;
(
countNumberOfRealBrothers(RB), countNumberOfRealSisters(RS), (   RB>1; RS>1) ->
sharesTwoTimesNtoOneTimesNDistributer(1/2, M, F, RB, RS),
findall((X,Y,M  + "or rest divided with other brothers if less than the share"),( realbrother(X,Y)), L), write(L), nl,
findall((X,Y,F  + "or rest divided with other brothers if less than the share"),( realsister(X,Y)), LL),
write(LL), nl;
true
)
.


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

daughter, table1Distribution(2)
;
R==2 ->
sonDaughter, table1Distribution(3)
;   (
R==3->

sonsonDaughter, table1Distribution(4)
;
(
R==4->
father, table1Distribution(5)
;
(
R==5->
mother, table1Distribution(6)
;
(
R==6->
fatherfather, table1Distribution(7)
;
(
R==7 ->
fathermother, table1Distribution(8)
;
(
R==8 ->
mothermother, table1Distribution(9)
;
(
R==9 ->
realsister, table1Distribution(10)
;
(
R==10 ->
parentalsister, table1Distribution(11)
;
(
R==11 ->
maternalsibling /*Distributes shares here regardless of gender*/
))))))))))
.

table2Distribution(R) :-
R==0 ->
doesOneOrMoreSonExist, table2Distribution(1)
;
(  R==1 -> doesOneOrMoreSonSonExist, table2Distribution(2)  ;
(
R==2 ->   doesOneOrMoreSonSonSonExist , table2Distribution(3)
;
(
R==3 ->  doesFatherExistWithNoChildren  ,table2Distribution(4)
;
(
R==4 ->  doesFatherFatherExistWithNoChildrenAndAFather, table2Distribution(5)
;
(
R==5 ->   doesRealBrotherExist
; true
)
)
)
)
)
.
/* Main Function*/
distribute:-
writeln("Table 1 Distribution:"),
table1Distribution(0), nl,
writeln("Table 2 Distribution:"),
table2Distribution(0), nl,
writeln("More Guidelines"),
writeln("Please if the total of shares is 1"),writeln("then ignore the rest of the calculations"),
writeln("Please if the total of shares is less than 1"),
writeln("Then give the rest to the closest relative like the following"),
writeln("Son-> Son Son -> Son Son Son -> Father (F) -> FF, FFF, etc ->"),
writeln("real brother -> real sister -> parental brother -> parenal sister"),
writeln("Residuary (Father brother, father's father's brother son, etc)")

.


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
doesFatherExistWithNoChildren:-
countNumberOfChildren(Ch), Ch==0 -> fatherWithNoSon(_) ; true
.
doesFatherFatherExistWithNoChildrenAndAFather:- countNumberOfChildren(Ch),
countNumberOfFather(F), Ch==0, F==1->  fatherfatherWithNoSonOrFather(_); true.

doesRealBrotherExist:- countNumberOfRealSisters(RS), RS ==0 -> realBrotherWithNosRealSister(_)
;
(   countNumberOfRealSisters(RS) , countNumberOfRealBrothers(RB), RB>0, RS>0 ->
oneOrManyrealbrothersWithOneOrManySisters
;
true
).



