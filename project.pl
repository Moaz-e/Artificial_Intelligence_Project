row(2).
col(2).
fxd_cell(1,2,3).
fxd_cell(1,6,1).
fxd_cell(3,1,2).
fxd_cell(3,4,1).
fxd_cell(5,2,1).
fxd_cell(5,5,2).
fxd_cell(6,3,2).
fxd_cell(7,1,1).
fxd_cell(7,5,1).
fxd_cell(7,7,6).


clear :- retractall(cell(_,_,_)).

generate(X,Y,L):- X<Y ,X1 is X+1 ,
                  generate(X1,Y,L1),L=[X|L1]
                 ;X=Y , L = [X].
get_numR(Num):- row(R),generate(1,R,L), member(Num,L).
get_numC(Num):- col(C),generate(1,C,L), member(Num,L).
loop_blue :- get_numR(Row), get_numC(Col) ,assert(cell(Row, Col, blue)) ,fail.
loop_green:- get_numR(Row), get_numC(Col) ,fxd_cell(Row,Col,_)
             ,retract(cell(Row,Col,blue)) ,assert(cell(Row,Col,green)) ,fail.
init :- clear , \+loop_blue , \+loop_green,\+print .

print :- get_numR(Row),get_numC(Col),
        ( fxd_cell(Row,Col,Z),write(Z),write(' ');
          \+fxd_cell(Row,Col,_) , cell(Row,Col,T),
              (   T=green ,write('G'),write(' ');
                  T=blue  ,write('B'),write(' ') ) ),
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
dfs(c(R,C)) :- retractall(node(_)),
               dfs([c(R,C)],[]),
               findall(c(X,Y), node(c(X,Y)), L),
               write(L).
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

begin :- begin(1,2).
begin(X,Y) :- X=Y ,! ; \+(X=Y),write("to insert cell write 'y' ,to check write 'n' : "),read(Z),
              solve(Z,Res),\+print ,(Res = 1 , begin(X,X)
                                    ;Res = 0 , begin(X,Y)).

solve(T,Res):- T='y' ,write("Enter Row Cell : "),read(X),
                      write("Enter Coloum Cell : "),read(Y),
                      write("Enter Color Cell : "),read(Z),write(cell(X,Y,Z)),nl,
                     (Z=green;Z=blue),inside(X,Y),\+fxd_cell(X,Y,_),
                      retract(cell(X,Y,_)) ,assert(cell(X,Y,Z)), Res is 0,!
              ;T='n' ,checkSolve(Res1) , (Res1='T' , write("You Win"),nl,Res is 1
                                         ;Res1='F' , write("Not Correct") , Res is 0) ,!
              ;write("Not Correc Input ... "),nl,Res is 0.
checkSolve('T').
