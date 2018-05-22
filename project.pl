row(7).
col(7).
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

get_num(Num):- L =[1,2,3,4,5,6,7], member(Num,L).
loop_blue :- get_num(Row), get_num(Col) ,assert(cell(Row, Col, blue)) ,fail.
loop_green:- get_num(Row), get_num(Col) ,fxd_cell(Row,Col,_)
             ,retract(cell(Row,Col,blue)) ,assert(cell(Row,Col,green)) ,fail.
init :- clear , \+loop_blue , \+loop_green .

print :- get_num(Row),get_num(Col),
        ( fxd_cell(Row,Col,Z),write(Z),write(' ');
          \+fxd_cell(Row,Col,_) , cell(Row,Col,T),
              (   T=green ,write('G'),write(' ');
                  T=blue  ,write('B'),write(' ') ) ) ,Col = 7 ,nl,fail.
