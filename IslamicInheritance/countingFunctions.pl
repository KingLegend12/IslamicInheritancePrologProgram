/*These functions count the numbers of all supported family members*/

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
