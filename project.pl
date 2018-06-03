row(6).
col(6).
fxd_cell(1,2,3).
fxd_cell(1,6,1).
fxd_cell(3,1,2).
fxd_cell(3,4,2).
fxd_cell(5,2,1).
fxd_cell(5,5,2).
fxd_cell(6,3,2).
%fxd_cell(7,1,1).
%fxd_cell(7,5,1).
%fxd_cell(7,7,6).

:-dynamic cell/3.
:-dynamic node/1.
:-dynamic sumGreenCell/1.

clear :- retractall(cell(_,_,_)),retractall(sumGreenCell(_)).

generate(X,Y,L):- X<Y ,X1 is X+1 ,generate(X1,Y,L1),L=[X|L1]
                 ;X=Y , L = [X].
get_numR(Num):- row(R),generate(1,R,L), member(Num,L).
get_numC(Num):- col(C),generate(1,C,L), member(Num,L).

loop_white :- get_numR(Row), get_numC(Col) ,
             assert(cell(Row, Col, white)) ,fail.
loop_green:- get_numR(Row), get_numC(Col) ,fxd_cell(Row,Col,_),
             retract(cell(Row,Col,white)) ,
             assert(cell(Row,Col,green)) ,fail.
getSum([],0).
getSum([H|T],Res):- getSum(T,Res1),X is H-1,Res is Res1+X.
sumFxdCell :- findall(Z,fxd_cell(_,_,Z),L),getSum(L,Z),assert(sumGreenCell(Z)).

init :- clear , \+loop_white , \+loop_green,\+print ,sumFxdCell,sumGreenCell(Z),write(Z),nl .

print :- get_numR(Row),get_numC(Col),
        ( fxd_cell(Row,Col,Z),write(Z),write(' ');
          \+fxd_cell(Row,Col,_) , cell(Row,Col,T),
              (   T=green ,write('G'),write(' ');
                  T=blue  ,write('B'),write(' ');
                  T=white ,write('w'),write(' ')) ),
        col(ColNum),Col = ColNum ,nl,fail.


inside(R,C):- row(Row),col(Col),R>0 , R<Row+1 ,C>0, C<Col+1.
adj(c(R,C),L):- X is R+1 , Y is C+1, X1 is R-1 , Y1 is C-1,
                   L1 = [c(X,C),c(X1,C),c(R,Y),c(R,Y1)],
                   cell(R,C,Color),removeOut(L1,Color,L).
removeOut([c(R,C)|H],Color,L):- removeOut(H,Color,L1),
                         (inside(R,C),cell(R,C,Y),
                               ( Y = Color ,L = [c(R,C)|L1];
                                 \+(Y=Color),L = L1)
                        ;\+inside(R,C),L = L1).
removeOut([],_,[]).

%% Dfs starting from a root
dfs(c(R,C),Z) :- retractall(node(_)),
               dfs([c(R,C)],[]),
               findall(c(X,Y), node(c(X,Y)), L),
         %      write(L),
               Z=L.
%% dfs(ToVisit, Visited)
%% Done, all visited
dfs([],_).
%% Skip elements that are already visited
dfs([c(R,C)|T],Visited) :- member(c(R,C),Visited),
                           dfs(T,Visited).
%% Add all neigbors of the head to the toVisit
dfs([c(R,C)|T],Visited) :- not(member(c(R,C),Visited)),
                           assert(node(c(R,C))),
                           adj(c(R,C),Nbs),
                           append(Nbs,T, ToVisit),
                           dfs(ToVisit,[c(R,C)|Visited]).

begin :-  write("to play as user 'u' ,Computer 'c' : "),read(Z),
          begin(1,2,Z).
begin(X,Y,H) :- X=Y ,! ; \+(X=Y),
     ( H='u',write("to insert cell write 'y' ,to check write 'c' : "),read(Z),
              solve(Z,Res),\+print ,(Res = 1 , begin(X,X,H)
                                    ;Res = 0 , begin(X,Y,H)) ,!
     ; H='c',solveCom() ).
solveCom():- write("to Smart Solve 's' ,to Backtracking Solve 'b' : "),read(Z),
             (   Z = 's' , smartSolve()
             ;   Z = 'b' , backtrackingSolve()).
backtrackingSolve():- findall(c(R,C),cell(R,C,white),L) ,\+backtracking(L).
backtracking([]):-checkSolve(Res), Res='T' , write("Computer Win") ,nl , \+print,nl ,fail.
backtracking([c(X,Y)|L]):- sumGreenCell(SumFxdCell),SumFxdCell>0,Temp is SumFxdCell-1,
                           retract(sumGreenCell(SumFxdCell)),assert(sumGreenCell(Temp)),
                           retract(cell(X,Y,_)) ,assert(cell(X,Y,green)), \+backtracking(L),
                           retractall(sumGreenCell(_)),assert(sumGreenCell(SumFxdCell)),fail
                          ; retract(cell(X,Y,_)) ,assert(cell(X,Y,blue)), backtracking(L).
smartSolve():- color_all_neighbor_cell_with_one,color_blue_if_can_not_color_green,\+print,
                findall(c(R,C),cell(R,C,white),L) ,\+backtracking(L).


solve(T,Res):- T='y' ,write("Enter Row Cell : "),read(X),
                      write("Enter Coloum Cell : "),read(Y),
                      write("Enter Color Cell : "),read(Z),
                      write(cell(X,Y,Z)),nl,
                     (Z=green;Z=blue;Z = white),inside(X,Y),\+fxd_cell(X,Y,_),
                      retract(cell(X,Y,_)) ,assert(cell(X,Y,Z)),
                      Res is 0,!
              ;T='c' ,checkSolve(Res1) ,
                     (Res1='T' , write("You Win"),nl,Res is 1 ,!
                     ;Res1='F' , write("Not Correct Solve ") ,nl , Res is 0) ,!
              ;write("Not Correc Input ... "),nl,Res is 0.
checkSolve(Res):- findall(c(R,C),cell(R,C,white),L), changeToBlue(L),
                  (   seaLonely ,\+ exist4Neighbors ,theIslandsHaveSingleFixedCell,
                      numIslandsCellAccept,%changeToWhite(L)
                  Res = 'T',!
                  ; Res = 'F' ,changeToWhite(L)) .
changeToBlue([c(X,Y)|H]):- changeToBlue(H), retract(cell(X,Y,_)) ,assert(cell(X,Y,blue)).
changeToBlue([]).
changeToWhite([c(X,Y)|H]):- changeToWhite(H), retract(cell(X,Y,_)) ,assert(cell(X,Y,white)).
changeToWhite([]).
lengt([],0).
lengt([c(_,_)|H],Len):- lengt(H,L1),Len is L1+1.

% 1
seaLonely :- findall(c(R,C),cell(R,C,blue),[c(X,Y)|_]),
             findall(c(R,C),cell(R,C,green),L),
             dfs(c(X,Y),List),
             lengt(List,LengthSea),lengt(L,Lengthgreen),
             row(Row),col(Col),SizeGrid is Row*Col,
             Temp is SizeGrid-Lengthgreen,
             LengthSea = Temp .

% 2
exist4Neighbors :- findall(c(R,C),cell(R,C,blue),[c(X,Y)|_]),
                   dfs(c(X,Y),List),check(List,List,Z),
                %   write(Z),
                   Z>0.
check([],_,0).
check([c(R,C)|H],List,Z):- check(H,List,Z1),X is R+1 , Y is C+1,
                          (member(c(X,C),List),member(c(X,Y),List),
                           member(c(R,Y),List),Z is Z1+1,!
                          ;Z is Z1).

% 3
theIslandsHaveSingleFixedCell :- findall(c(R,C),cell(R,C,green),L),
                           %      write(L),nl,
                                 allIslandsHaveSingleFixedCell(L).
allIslandsHaveSingleFixedCell([]).
allIslandsHaveSingleFixedCell([c(X,Y)|H]):- allIslandsHaveSingleFixedCell(H) ,
                                            dfs(c(X,Y),List),countFixedCell(List,Num),
                                        %    write("Num = "),write(Num),nl,
                                            Num = 1.
countFixedCell([],0).
countFixedCell([c(X,Y)|H],Z):- countFixedCell(H,Z1), ( fxd_cell(X,Y,_),Z is Z1+1 ,!
                                                     ; Z is Z1).
% 4
numIslandsCellAccept :- findall(c(R,C,X),fxd_cell(R,C,X),L),
                       % write(L),nl,
                        checkAllIslands(L).
checkAllIslands([]).
checkAllIslands([c(X,Y,Num)|H]):- checkAllIslands(H) , dfs(c(X,Y),List),lengt(List,Length),
                                            %      write(Num),write(' '),write(Length),nl,
                                                  Num = Length  .

% ----------------------------------------------------------------------
removeOut2([c(R,C)|H],L):- removeOut2(H,L1),
                         (inside(R,C),L = [c(R,C)|L1]
                        ;\+inside(R,C),L = L1).
removeOut2([],[]).
adj2(c(R,C),L):- X is R+1 , Y is C+1, X1 is R-1 , Y1 is C-1,
                   L1 = [c(X,C),c(X1,C),c(R,Y),c(R,Y1)],
                   removeOut2(L1,L).
colorNeighbor(c(R,C)):- adj2(c(R,C),L),changeToBlue(L).
color_neighbor_to_blue([]).
color_neighbor_to_blue([c(R,C)|T]) :- color_neighbor_to_blue(T),colorNeighbor(c(R,C)).



color_all_neighbor_cell_with_one :- findall(c(R,C),fxd_cell(R,C,1),L) , color_neighbor_to_blue(L).




% ----------------------------------------------------------------------
check_if_I_can_find_more_than_one_fixed_cell([]).
check_if_I_can_find_more_than_one_fixed_cell([c(R,C)|T]):- check_if_I_can_find_more_than_one_fixed_cell(T),
                                                           adj2(c(R,C),L),countFixedCell(L,Z),
                                                           (   Z>1,retract(cell(R,C,_)) ,assert(cell(R,C,blue))
                                                           ;   Z<2).

color_blue_if_can_not_color_green :- findall(c(R,C),cell(C,R,white),List),check_if_I_can_find_more_than_one_fixed_cell(List).



% ------------------------------------------------------------------------


%check_if_I_can_find_more_than_one_fixed_cell_DFS([]).
% check_if_I_can_find_more_than_one_fixed_cell_DFS(([c(R,C)|T]):-
% check_if_I_can_find_more_than_one_fixed_cell_DFS((T),













