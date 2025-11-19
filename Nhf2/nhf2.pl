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



% Fő megoldó belépési pont: feladványból kész megoldást állít elő.
% szamtekercs(+Feladvany, -Megoldas)
szamtekercs(FL, Megoldas) :-
    kezdotabla(FL, Matrix0),
    solve_matrix(FL, Matrix0, MatrixSolved),
    matrix_to_solution(MatrixSolved, Megoldas).



% Rekurzív kereső: szűkít, majd visszalépéssel kitölti a táblát.
% solve_matrix(+FL, +MatrixIn, -MatrixSolved)
solve_matrix(_, [], _) :- !, fail.
solve_matrix(FL, MatrixIn, MatrixSolved) :-
    propagate_all(FL, MatrixIn, Reduced),
    Reduced \== [],
    (   solved_matrix(Reduced)
    ->  MatrixSolved = Reduced
    ;   select_branch_cell(Reduced, R, C, Domain),
        domain_choice(Reduced, R, C, Domain, Value),
        set_cell(Reduced, R, C, [Value], NextMatrix),
        solve_matrix(FL, NextMatrix, MatrixSolved)
    ).


% Szűkítések ismételt alkalmazása fixpontig.
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


% Egyetlen (ismert/kizárásos/spirál) szűkítés kipróbálása.
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


% Spirál-specifikus szűkítés a mátrixon.
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


% Spirál előre-hátra szűkítése fixpontig.
% apply_spiral_passes(+FL,+SpiralIn,-SpiralOut)
apply_spiral_passes(FL, SpiralIn, SpiralOut) :-
    spiral_fixpoint(FL, SpiralIn, SpiralOut).


spiral_fixpoint(FL, SpiralIn, SpiralOut) :-
    % Előre + vissza spirál szűkítés és domain metszet ismétlés.
    tekercs_szukites(FL, SpiralIn, SpiralForward),
    reverse(SpiralIn, RevIn),
    tekercs_szukites_backward(FL, RevIn, RevNarrowed),
    reverse(RevNarrowed, SpiralBackward),
    intersect_spiral_domains(SpiralForward, SpiralBackward, Combined),
    (   Combined == SpiralIn
    ->  SpiralOut = Combined
    ;   spiral_fixpoint(FL, Combined, SpiralOut)
    ).


% Spirál elemek domainjeinek páronkénti metszése.
intersect_spiral_domains([], [], []).
intersect_spiral_domains([A|As], [B|Bs], [R|Rs]) :-
    intersect_domains(A, B, R),
    intersect_spiral_domains(As, Bs, Rs).


intersect_domains(A, B, R) :-
% Két teljesen konkretizált (szám) domain összevetése.
    integer(A),
    integer(B),
    !,
    ( A =:= B -> R = A ; fail ).

intersect_domains(A, B, R) :-
% Szám vs lista domain metszése (szám benne van-e listában).
    integer(A),
    is_list(B),
    !,
    ( ord_memberchk(A, B) -> R = A ; fail ).

intersect_domains(A, B, R) :-
% Lista vs szám domain metszése (szám benne van-e listában).
    is_list(A),
    integer(B),
    !,
    ( ord_memberchk(B, A) -> R = B ; fail ).

intersect_domains(A, B, R) :-
% Két lista domain metszése; üres metszet esetén bukás.
    is_list(A),
    is_list(B),
    ord_intersection(A, B, Intersect),
    Intersect \= [],
    R = Intersect.


% Backward spiral narrowing
% Spirál visszafelé szűkítése (hátrafelé propagálás).
tekercs_szukites_backward(szt(_, CycleLength, _), SpiralLine, NarrowedSpiral) :-
    SpiralLine \= [],
    process_spiral_backward(SpiralLine, CycleLength, [1], NarrowedSpiral),
    !.


% Visszafelé spirál feldolgozás vége (alapeset).
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

% Egy spirálpozíció domainjének leszűkítése visszafelé.
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


% Következő értékek ciklikus elődhalmazának meghatározása.
cyclic_predecessors(CycleLength, NextSet, PredSet) :-
    findall(Pred, (member(Value, NextSet), cyclic_predecessor(CycleLength, Value, Pred)), PredList),
    list_to_ord_set(PredList, PredSet).

% 0 speciális eset: önmaga elődje.
cyclic_predecessor(_, 0, 0) :- !.

cyclic_predecessor(CycleLength, Value, Pred) :-
    (   Value > 1
    ->  Pred is Value - 1
    ;   Pred is CycleLength
    ).


% Frissíti a következő pozitív készletet a szűkítés eredménye alapján.
update_next_positive_set(ResultSet, NextPositiveSetIn, NextPositiveSetOut) :-
    (   \+ ord_memberchk(0, ResultSet)
    ->  NextPositiveSetOut = ResultSet
    ;   remove_zero(ResultSet, PositiveOnly),
        ord_union(PositiveOnly, NextPositiveSetIn, NextPositiveSetOut)
    ).


% Spirál pozíciókhoz tartozó cellaértékek kinyerése a mátrixból.
% extract_spiral(+Matrix,+Positions,-SpiralList)
extract_spiral(Matrix, Positions, Spiral) :-
    maplist(cell_at(Matrix), Positions, Spiral).


% Egyetlen pozíció cellájának lekérése.
cell_at(Matrix, pos(R,C), Cell) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell).


% Spirál mentén új értékek visszaírása a mátrixba.
% replace_spiral_values(+MatrixIn,+Positions,+Values,-MatrixOut)
replace_spiral_values(MatrixIn, Positions, Values, MatrixOut) :-
    replace_spiral_values_(Positions, Values, MatrixIn, MatrixOut).

% Belső rekurzív segéd a spirál értékek cseréjéhez.
replace_spiral_values_([], [], Matrix, Matrix).
replace_spiral_values_([pos(R,C)|RestPos], [Value|RestVals], MatrixIn, MatrixOut) :-
    set_cell(MatrixIn, R, C, Value, MatrixNext),
    replace_spiral_values_(RestPos, RestVals, MatrixNext, MatrixOut).


% Igaz, ha a mátrix minden cellája végleges értékre szűkült.
% solved_matrix(+Matrix)
solved_matrix(Matrix) :-
    maplist(row_finalized, Matrix).


% Igaz, ha a sor minden cellája végleges.
row_finalized(Row) :-
    maplist(cell_finalized, Row).


% Végleges cella: konkrét szám vagy egyelemű lista.
cell_finalized(Cell) :- integer(Cell), !.
cell_finalized([_]).


% Heurisztikus választás a cella domainjéből (pozitív előnyben, különben 0).
domain_choice(Matrix, R, C, Domain, Value) :-
    include(pos_value, Domain, Positives),
    Positives \= [],
    score_positive_choices(Matrix, R, C, Positives, Scored),
    member(_Score-Value, Scored).
domain_choice(_Matrix, _R, _C, Domain, 0) :-
    member(0, Domain).


% Pozitív jelölt (0 kizárva).
pos_value(V) :- V > 0.


% Sor/oszlop előfordulásszám szorzata alapján rangsorol.
score_positive_choices(Matrix, R, C, Positives, OrderedPairs) :-
    nth1(R, Matrix, Row),
    get_column_values(Matrix, C, Column),
    findall(Score-Value,
            ( member(Value, Positives),
              row_value_statistics(Row, Value, RowCount, _, _),
              column_value_statistics(Column, Value, ColCount, _, _),
              Score is RowCount * ColCount
            ),
            Pairs),
    keysort(Pairs, OrderedPairs).


% Első többértékű domainű cella kiválasztása spirál sorrendben.
% select_branch_cell(+Matrix,-R,-C,-Domain)
select_branch_cell(Matrix, R, C, Domain) :-
    length(Matrix, N),
    spiral_positions(N, Positions),
    select_spiral_cell(Matrix, Positions, R, C, Domain).


% Spirál listában első nem egyelemű cella kiválasztása.
select_spiral_cell(Matrix, [pos(R,C)|_], R, C, Domain) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell),
    is_list(Cell),
    length(Cell, Len),
    Len > 1,
    Domain = Cell,
    !.

select_spiral_cell(Matrix, [_|Rest], R, C, Domain) :-
    select_spiral_cell(Matrix, Rest, R, C, Domain).


% Megoldott mátrix sorait egyszerű értéklistákká alakítja.
% matrix_to_solution(+Matrix,-Solution)
matrix_to_solution(Matrix, Solution) :-
    maplist(row_to_values, Matrix, Solution).


% Sor celláinak konkretizálása.
row_to_values(Row, Values) :-
    maplist(cell_value, Row, Values).


% Cellából numerikus érték kinyerése.
cell_value(Cell, Value) :-
    (   integer(Cell)
    ->  Value = Cell
    ;   Cell = [Value]
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spirál pozíciók kezelése

% N méretű négyzet táblához spirál sorrendű pozíciók.
spiral_positions(N, Positions) :-
    spiral_collect(1, N, 1, N, Positions).


% Spirál gyűjtés határátlépéskor leáll.
spiral_collect(Top, Bottom, Left, Right, []) :-
    (Top > Bottom ; Left > Right), !.

% Egy réteg szélei, majd rekurzió belső rétegre.
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


% Felső él bejárása balról jobbra.
top_edge_positions(_Row, Left, Right, []) :-
    Left > Right, !.

top_edge_positions(Row, Left, Right, [pos(Row, Left)|Rest]) :-
    Left =< Right,
    Next is Left + 1,
    top_edge_positions(Row, Next, Right, Rest).


% Jobb él bejárása felülről lefelé.
right_edge_positions(Top, Bottom, _Col, []) :-
    Top > Bottom, !.

right_edge_positions(Top, Bottom, Col, [pos(Top, Col)|Rest]) :-
    Top =< Bottom,
    Next is Top + 1,
    right_edge_positions(Next, Bottom, Col, Rest).


% Alsó él bejárása jobbról balra.
bottom_edge_positions(_Row, Start, End, []) :-
    Start < End, !.

bottom_edge_positions(Row, Start, End, [pos(Row, Start)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    bottom_edge_positions(Row, Next, End, Rest).


% Bal él bejárása lentről felfelé.
left_edge_positions(Start, End, _Col, []) :-
    Start < End, !.
    
left_edge_positions(Start, End, Col, [pos(Start, Col)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    left_edge_positions(Next, End, Col, Rest).
