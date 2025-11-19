% ------------------------------------------------------------
% Nhf2 - Számtekercs végső megoldó
%
% A feladat megoldásához felhasználom a korábbi kis házikban
% elkészített szűkítéseket (Khf5, Khf6, Khf7).
% ------------------------------------------------------------


:- use_module(library(lists)).

:- ensure_loaded('../Khf5/khf5.pl').
:- ensure_loaded('../Khf6/khf6.pl').
:- ensure_loaded('../Khf7/khf7.pl').


% szamtekercs(+Feladvany, -Megoldas)
szamtekercs(FL, Megoldas) :-
    kezdotabla(FL, Matrix0),
    solve_matrix(FL, Matrix0, MatrixSolved),
    matrix_to_solution(MatrixSolved, Megoldas).


% solve_matrix(+FL, +MatrixIn, -MatrixSolved)
solve_matrix(_, [], _) :- !, fail.
solve_matrix(FL, MatrixIn, MatrixSolved) :-
    propagate_all(FL, MatrixIn, Reduced),
    Reduced \== [],
    (   solved_matrix(Reduced)
    ->  MatrixSolved = Reduced
    ;   select_branch_cell(Reduced, R, C, Domain),
        member(Value, Domain),
        set_cell(Reduced, R, C, [Value], NextMatrix),
        solve_matrix(FL, NextMatrix, MatrixSolved)
    ).


% propagate_all(+FL,+MatrixIn,-MatrixOut)
propagate_all(_FL, [], []) :- !.
propagate_all(FL, MatrixIn, MatrixOut) :-
    (   apply_one_restriction(FL, MatrixIn, Temp)
    ->  (   Temp == []
        ->  MatrixOut = []
        ;   propagate_all(FL, Temp, MatrixOut)
        )
    ;   MatrixOut = MatrixIn
    ).


% apply_one_restriction(+FL,+MatrixIn,-MatrixOut)
apply_one_restriction(FL, MatrixIn, MatrixOut) :-
    (   ismert_szukites(FL, MatrixIn, MatrixOut)
    ->  true
    ;   kizarasos_szukites(FL, MatrixIn, Temp, _Info),
        (   Temp == []
        ->  MatrixOut = []
        ;   Temp \== MatrixIn,
            MatrixOut = Temp
        )
    ;   mxtekercs_szukites(FL, MatrixIn, MatrixOut)
    ).


% mxtekercs_szukites(+FL,+MatrixIn,-MatrixOut)
mxtekercs_szukites(FL, MatrixIn, MatrixOut) :-
    MatrixIn \== [],
    length(MatrixIn, N),
    N > 0,
    spiral_positions(N, Positions),
    extract_spiral(MatrixIn, Positions, Spiral),
    (   apply_spiral_passes(FL, Spiral, Narrowed)
    ->  (   Narrowed == Spiral
        ->  fail
        ;   replace_spiral_values(MatrixIn, Positions, Narrowed, MatrixOut)
        )
    ;   MatrixOut = []
    ).


% apply_spiral_passes(+FL,+SpiralIn,-SpiralOut)
apply_spiral_passes(FL, SpiralIn, SpiralOut) :-
    tekercs_szukites(FL, SpiralIn, SpiralForward),
    reverse(SpiralIn, RevIn),
    tekercs_szukites_backward(FL, RevIn, RevNarrowed),
    reverse(RevNarrowed, SpiralBackward),
    intersect_spiral_domains(SpiralForward, SpiralBackward, SpiralOut).

intersect_spiral_domains([], [], []).
intersect_spiral_domains([A|As], [B|Bs], [R|Rs]) :-
    intersect_domains(A, B, R),
    intersect_spiral_domains(As, Bs, Rs).

intersect_domains(A, B, R) :-
    integer(A),
    integer(B),
    !,
    ( A =:= B -> R = A ; fail ).
intersect_domains(A, B, R) :-
    integer(A),
    is_list(B),
    !,
    ( ord_memberchk(A, B) -> R = A ; fail ).
intersect_domains(A, B, R) :-
    is_list(A),
    integer(B),
    !,
    ( ord_memberchk(B, A) -> R = B ; fail ).
intersect_domains(A, B, R) :-
    is_list(A),
    is_list(B),
    ord_intersection(A, B, Intersect),
    Intersect \= [],
    R = Intersect.


% Backward spiral narrowing
tekercs_szukites_backward(szt(_, CycleLength, _), SpiralLine, NarrowedSpiral) :-
    SpiralLine \= [],
    process_spiral_backward(SpiralLine, CycleLength, [1], NarrowedSpiral),
    !.

process_spiral_backward([], _, _, []).
process_spiral_backward([PositionDomain|Rest], CycleLength,
                        NextPositiveSetIn,
                        [NarrowedDomain|RestNarrowed]) :-
    shrink_position_backward(PositionDomain, CycleLength,
                             NextPositiveSetIn,
                             NarrowedDomain,
                             NextPositiveSetOut),
    process_spiral_backward(Rest, CycleLength,
                            NextPositiveSetOut,
                            RestNarrowed).

shrink_position_backward(PositionDomain, CycleLength,
                         NextPositiveSetIn,
                         NarrowedDomain,
                         NextPositiveSetOut) :-
    cyclic_predecessors(CycleLength, NextPositiveSetIn, PredSet),
    ord_add_element(PredSet, 0, AllowedValues),
    intersect_domain(PositionDomain, AllowedValues,
                     NarrowedDomain, ResultSet),
    update_next_positive_set(ResultSet, NextPositiveSetIn,
                             NextPositiveSetOut).

cyclic_predecessors(CycleLength, NextSet, PredSet) :-
    findall(Pred, (member(Value, NextSet), cyclic_predecessor(CycleLength, Value, Pred)), PredList),
    list_to_ord_set(PredList, PredSet).

cyclic_predecessor(_, 0, 0) :- !.
cyclic_predecessor(CycleLength, Value, Pred) :-
    (   Value > 1
    ->  Pred is Value - 1
    ;   Pred is CycleLength
    ).

update_next_positive_set(ResultSet, NextPositiveSetIn, NextPositiveSetOut) :-
    (   \+ ord_memberchk(0, ResultSet)
    ->  NextPositiveSetOut = ResultSet
    ;   remove_zero(ResultSet, PositiveOnly),
        ord_union(PositiveOnly, NextPositiveSetIn, NextPositiveSetOut)
    ).

% extract_spiral(+Matrix,+Positions,-SpiralList)
extract_spiral(Matrix, Positions, Spiral) :-
    maplist(cell_at(Matrix), Positions, Spiral).

cell_at(Matrix, pos(R,C), Cell) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell).


% replace_spiral_values(+MatrixIn,+Positions,+Values,-MatrixOut)
replace_spiral_values(MatrixIn, Positions, Values, MatrixOut) :-
    replace_spiral_values_(Positions, Values, MatrixIn, MatrixOut).

replace_spiral_values_([], [], Matrix, Matrix).
replace_spiral_values_([pos(R,C)|RestPos], [Value|RestVals], MatrixIn, MatrixOut) :-
    set_cell(MatrixIn, R, C, Value, MatrixNext),
    replace_spiral_values_(RestPos, RestVals, MatrixNext, MatrixOut).


% solved_matrix(+Matrix)
solved_matrix(Matrix) :-
    maplist(row_finalized, Matrix).

row_finalized(Row) :-
    maplist(cell_finalized, Row).

cell_finalized(Cell) :- integer(Cell), !.
cell_finalized([_]).


% select_branch_cell(+Matrix,-R,-C,-Domain)
select_branch_cell(Matrix, R, C, Domain) :-
    findall(branch(Len,RowIdx,ColIdx,Dom),
            ( nth1(RowIdx, Matrix, Row),
              nth1(ColIdx, Row, Cell),
              is_list(Cell),
              length(Cell, Len),
              Len > 1,
              Dom = Cell
            ),
            Branches),
    Branches \= [],
    predsort(compare_branch, Branches, [branch(_,R,C,Domain)|_]).

compare_branch(Order, branch(L1,R1,C1,_), branch(L2,R2,C2,_)) :-
    (   L1 =:= L2
    ->  ( R1 =:= R2
        ->  compare(Order, C1, C2)
        ;   compare(Order, R1, R2)
        )
    ;   compare(Order, L1, L2)
    ).


% matrix_to_solution(+Matrix,-Solution)
matrix_to_solution(Matrix, Solution) :-
    maplist(row_to_values, Matrix, Solution).

row_to_values(Row, Values) :-
    maplist(cell_value, Row, Values).

cell_value(Cell, Value) :-
    (   integer(Cell)
    ->  Value = Cell
    ;   Cell = [Value]
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spirál pozíciók kezelése

spiral_positions(N, Positions) :-
    spiral_collect(1, N, 1, N, Positions).

spiral_collect(Top, Bottom, Left, Right, []) :-
    (Top > Bottom ; Left > Right), !.
spiral_collect(Top, Bottom, Left, Right, Positions) :-
    Top =< Bottom,
    Left =< Right,
    top_edge_positions(Top, Left, Right, TopCells),
    Top1 is Top + 1,
    right_edge_positions(Top1, Bottom, Right, RightCells),
    (   Top < Bottom
    ->  StartB is Right - 1,
        bottom_edge_positions(Bottom, StartB, Left, BottomCells)
    ;   BottomCells = []
    ),
    (   Left < Right
    ->  StartL is Bottom - 1,
        EndL is Top + 1,
        left_edge_positions(StartL, EndL, Left, LeftCells)
    ;   LeftCells = []
    ),
    NextTop is Top + 1,
    NextBottom is Bottom - 1,
    NextLeft is Left + 1,
    NextRight is Right - 1,
    spiral_collect(NextTop, NextBottom, NextLeft, NextRight, InnerCells),
    append([TopCells, RightCells, BottomCells, LeftCells, InnerCells], Positions).


top_edge_positions(_Row, Left, Right, []) :-
    Left > Right, !.
top_edge_positions(Row, Left, Right, [pos(Row, Left)|Rest]) :-
    Left =< Right,
    Next is Left + 1,
    top_edge_positions(Row, Next, Right, Rest).


right_edge_positions(Top, Bottom, _Col, []) :-
    Top > Bottom, !.
right_edge_positions(Top, Bottom, Col, [pos(Top, Col)|Rest]) :-
    Top =< Bottom,
    Next is Top + 1,
    right_edge_positions(Next, Bottom, Col, Rest).


bottom_edge_positions(_Row, Start, End, []) :-
    Start < End, !.
bottom_edge_positions(Row, Start, End, [pos(Row, Start)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    bottom_edge_positions(Row, Next, End, Rest).


left_edge_positions(Start, End, _Col, []) :-
    Start < End, !.
left_edge_positions(Start, End, Col, [pos(Start, Col)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    left_edge_positions(Next, End, Col, Rest).
