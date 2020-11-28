% is_test(test).
% is_test_rule(X):- is_test(X).

% female(bonnie).
% female(olivia).
% male(edward).
% male(james).
% male(rick).
% cat(navi).
% cat(bebe).

% likes(bonnie, navi).
% likes(bonnie, james).
% likes(bonnie, edward).
% likes(edward, bonnie).
% likes(edward, rick).
% likes(olivia, edward).
% likes(olivia, bebe).
% likes(rick, edward).

% couple(A,B):- likes(A,B),likes(B,A),male(A),female(B);likes(A,B),likes(B,A),male(B),female(A).
% friend(A,B):- likes(A,B),likes(B,A),male(A),male(B);likes(A,B),likes(B,A),female(A),female(B).
% person(A):- male(A);female(A).
% pet(A):- cat(A).
% dumbfriend(A,B):- likes(A,B),person(A),pet(B);likes(A,B),person(B),pet(A).

% male(A):- not(female(A)).

% % is_ancestor_of(X,Y):- is_parent_of(X,Y).
% % is_ancestor_of(X,Y):- is_parent_of(Z,Y), is_ancestor_of(X,Z).

% arr(X,Y,X,Y).
% arr([H|T],X,Y):- arr(H,T,X,Y).

% male(tom).
% male(nicole).
% male(nick).
% male(arnold).
% male(mike).
% male(jack).
% female(ann).
% female(kitty).
% female(michael).
% female(julia).
% female(sue).
% parent(tom, arnold).
% parent(tom, michael).
% parent(nicole, kitty).
% parent(nick, julia).
% parent(nick, mike).
% parent(julia, jack).
% parent(julia, sue).

% father(X,Y):- male(X), parent(X,Y).
% mother(X,Y):- female(X), parent(X,Y).

% sibling(X,Y):- parent(Z,X), parent(Z,Y).
% grandparent(X,Y):- parent(Z,Y), parent(X,Z).

% married(X,Y):- parent(X,Z),parent(Y,Z).

% descendent(D,A):- parent(A,D).
% descendent(D,A):- parent(P,D), descendent(P,A).

% fact_top(N):- fact_td(N,1).
% fact_td(0,F):- write(F).
% fact_td(N,F):- N>0, N1 is N-1, F1 is N*F, fact_td(N1,F1).

% fact_bot(N):- fact_bu(0,1,N).
% fact_bu(N1,F,N):- N1 = N, write(F).
% fact_bu(N1,F,N):- N1 < N, N2 is N1 + 1, F2 is N2 * F, fact_bu(N2, F2, N).

% xor(A,B):- A, not(B).
% xor(A,B):- B, not(A).

% group(L, 5, Result):- Result = L.
% group(L, Idx, Result):-
%     (not(member(betty, L)), append(L, [betty], TempL);
%     not(member(ethel, L)), append(L, [ethel], TempL);
%     not(member(joan, L)), append(L, [joan], TempL);
%     not(member(kitty, L)), append(L, [kitty], TempL),
%     not(member(mary,L)), append(L, [mary], TempL)
%     ), Idx2 is Idx + 1, group(TempL, Idx2, Result).

% betty(Second, Third):- xor(Second = kitty, Third = betty).
% ethel(First, Second):- xor(First = ethel, Second = joan).
% joan(Third, Fifth):- xor(Third = joan, Fifth = ethel).
% kitty(Second, Fourth):- xor(Second = kitty, Fourth = mary).
% mary(Fourth, First):- xor(Fourth = mary, First = betty).

% solution([First, Second, Third, Fourth, Fifth]):-
%     group([], 0, [First, Second, Third, Fourth, Fifth]),
%     betty(Second, Third),
%     ethel(First, Second),
%     joan(Third, Fifth),
%     kitty(Second, Fourth),
%     mary(Fourth, First).


% move(1,X,Y,_):- write(' Move top disk from '), write(X), write(' to '), write(Y), nl.
% move(N,X,Y,Z):- N>1, N2 is N - 1, move(N2,X,Z,Y), move(1,X,Y,_), move(N2,Z,Y,X).

% color(green).
% color(yellow).
% color(red).
% color(blue).

% neighbor(Acolor, Bcolor):- color(Acolor), color(Bcolor), Acolor \= Bcolor.
% graph(A,B,C,D,E):- neighbor(A,B), neighbor(A,C), neighbor(C,D), neighbor(B,D), neighbor(C,E).

% member(X,[X|_]).
% member(X,[_|Y]):- member(X,Y).

% nexthouse(L,R,[L,R|_]).
% nexthouse(L,R,[_|Rest]):- nexthouse(L,R,Rest).

% neighbor(X,Y,Houses):- nexthouse(X,Y,Houses); nexthouse(Y,X,Houses).

% einstein(Fishowner):-
%     Houses = [[nor,_,_,_,_],_,[_,_,_,milk,_],_,_],
%     nexthouse([_,green,_,_,_], [_,white,_,_,_], Houses),
%     neighbor([_,_,_,_,blend], [_,_,cat,_,_], Houses),
%     neighbor([_,_,horse,_,_], [_,_,_,_,dunhill], Houses),
%     member([_,_,_,beer,bluemaster], Houses),
%     member([ger,_,_,_,prince], Houses),
%     neighbor([nor,_,_,_,_], [_,blue,_,_,_], Houses),
%     neighbor([_,_,_,_,blend], [_,_,_,water,_], Houses),
%     member([Fishowner,_,fish,_,_], Houses).


% size(game, 9, 9).
% bomb(game, 4, 1).
% bomb(game, 3, 1).
% bomb(game, 2, 1).
% bomb(game, 5, 1).
% bomb(game, 7, 2).
% bomb(game, 3, 6).
% bomb(game, 8, 6).
% bomb(game, 1, 7).
% bomb(game, 3, 7).
% bomb(game, 6, 7).
% bomb(game, 3, 9).
% bomb(game, 8, 8).
% bomb(game, 9, 9).


size(game, 3, 3).
bomb(game, 1, 1).
bomb(game, 1, 2).
bomb(game, 1, 3).
% bomb(game, 2, 1).
% bomb(game, 2, 2).
bomb(game, 2, 3).
% bomb(game, 3, 1).
bomb(game, 3, 2).
bomb(game, 3, 3).

col(Game, N):- size(Game, N, _).
row(Game, M):- size(Game, _, M).

% bombs_in_col(Game, -1, X, L, Result):- Result = L.
% bombs_in_col(Game, N, X, L, Result):- bomb(Game, X, 1), append(L, [[X,1]], TempL), Nnext is N-1, Xnext is X+1, bombs_in_col(Game, Nnext, Xnext, TempL, Result);Nnext is N-1, Xnext is X+1, bombs_in_col(Game, Nnext, Xnext, TempL, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%-1
first(X, [X|_]).
rest(X, [_|X]).

neighbor(X,Y,Arr):- 
    X1 is X-1, Y1 is Y-1,
    X2 is X, Y2 is Y-1,
    X3 is X+1, Y3 is Y-1,
    X4 is X-1, Y4 is Y,
    X5 is X+1, Y5 is Y,
    X6 is X-1, Y6 is Y+1,
    X7 is X, Y7 is Y+1,
    X8 is X+1, Y8 is Y+1,
    Arr = [[X1,Y1], [X2,Y2], [X3,Y3], [X4,Y4], [X5,Y5], [X6,Y6], [X7,Y7], [X8,Y8]].


check(0, [], Status):- Status = empty.
check(Count, [], Status):- Status = Count.
check(Count,L, Status):-
    first(E, L),
    E = [P,Q],
    bomb(_,P,Q),
    Countnext is Count + 1,
    rest(Lnext, L),
    check(Countnext, Lnext, Status);
    first(E, L),
    E = [P,Q],
    not(bomb(_,P,Q)),
    rest(Lnext, L),
    check(Count, Lnext, Status).

open_at(Game, X, Y, S):-
    bomb(Game, X, Y),
    S = bomb;
    not(bomb(Game, X, Y)),
    neighbor(X, Y, L),
    check(0, L, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%-2
to_format(Game, X, Y):-
    open_at(Game, X, Y, S), S = bomb, format("* ");
    open_at(Game, X, Y, S), S = empty, format("_ ");
    open_at(Game, X, Y, S), not(S = bomb), not(S = empty), format("~d ", S).


print_col(Game, N, N, M):- to_format(Game, N, M), nl.

print_col(Game, N, X, M):-
    X < N, to_format(Game, X, M), Xnext is X + 1, print_col(Game, N, Xnext, M).


print_rows(Game, M, M, N):- print_col(Game, N, 1, M).

print_rows(Game, M, Y, N):-
    Y < M, print_col(Game, N, 1, Y), Ynext is Y + 1, print_rows(Game, M, Ynext, N).

show_answer(Game):-
    size(Game, N, M),
    print_rows(Game, M, 1, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%-3
is_survive(Game, []):- size(Game,_,_).
is_survive(Game, [[X,Y]|Rest]):- not(bomb(Game, X, Y)), is_survive(Game, Rest).



%%%%%%%%%%%%%%%%%%%%%%%%%%-4
opened_to_format(Game, L, X, Y):-
    not(member([X,Y], L)), format("? ");
    open_at(Game, X, Y, S), S = bomb, format("* ");
    open_at(Game, X, Y, S), S = empty, format("_ ");
    open_at(Game, X, Y, S), not(S = bomb), not(S = empty), format("~d ", S).

opened_print_col(Game, L, N, N, M):- opened_to_format(Game, L, N, M), nl.

opened_print_col(Game, L, N, X, M):-
    X < N, opened_to_format(Game, L, X, M), Xnext is X + 1, opened_print_col(Game, L, N, Xnext, M).

opened_print_rows(Game, L, M, M, N):- opened_print_col(Game, L, N, 1, M).

opened_print_rows(Game, L, M, Y, N):-
    Y < M, opened_print_col(Game, L, N, 1, Y), Ynext is Y + 1, opened_print_rows(Game, L, M, Ynext, N).

show_status(Game, L):-
    size(Game, N, M),
    opened_print_rows(Game, L, M, 1, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%-5
win_col(Game, N, N, Y, L, Result):-
    not(bomb(Game, N, Y)), append([[N,Y]], L, Result);
    bomb(Game, N, Y), Result = L.

win_col(Game, N, X, Y, L, Result):-
    X < N, not(bomb(Game, X, Y)), append([[X,Y]], L, Lnext), Xnext is X + 1, win_col(Game, N, Xnext, Y, Lnext, Result);
    X < N, bomb(Game, X, Y), Xnext is X + 1, win_col(Game, N, Xnext, Y, L, Result).

win_rows(Game, N, M, M, L, Result):- win_col(Game, N, 1, M, [], R), append(R, L, Result).

win_rows(Game, N, M, Y, L, Result):- 
Y < M, win_col(Game, N, 1, Y, [], R), Ynext is Y + 1, append(R, L, Lnext), win_rows(Game, N, M, Ynext, Lnext, Result).

win_list(Game, WinList):-
    size(Game, N, M),
    win_rows(Game, N, M, 1, [], WinList).

length_list([], Count, Result):-
    Result = Count.

length_list(L, Count, Result):-
    rest(Lnext, L),
    CountNext is Count + 1,
    length_list(Lnext, CountNext, Result).

win_check(Game, [], L_origin):-
    win_list(Game, WinList),
    length_list(WinList, 0, R1),
    length_list(L_origin, 0, R2),
    R1 = R2.

win_check(Game, L, L_origin):-
    win_list(Game, WinList),
    first(E, L),
    member(E, WinList),
    rest(Lnext, L),
    win_check(Game, Lnext, L_origin).

is_win(Game, L):-
    win_check(Game, L, L).
