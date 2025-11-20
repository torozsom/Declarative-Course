% ------------------------------------------------------------
% Nhf2 - Számtekercs végső megoldó
%
% A feladat megoldásához felhasználom a korábbi kis házikban
% elkészített szűkítéseket (Khf5, Khf6, Khf7).
% ------------------------------------------------------------



:- use_module(library(lists)).


% Fő megoldó belépési pont: feladványból kész megoldást állít elő.
% szamtekercs(+Feladvany, -Megoldas)
szamtekercs(FL, Megoldas) :-
    kezdotabla(FL, Matrix0),
    FL = szt(_, Cycle, _),
    solve_matrix(FL, Cycle, Matrix0, MatrixSolved),
    matrix_to_solution(MatrixSolved, Megoldas).



% Rekurzív kereső: szűkít, majd visszalépéssel kitölti a táblát.
% solve_matrix(+FL,+Cycle,+MatrixIn,-MatrixSolved)
solve_matrix(_, _, [], _) :- !, fail.
solve_matrix(FL, Cycle, MatrixIn, MatrixSolved) :-
    propagate_all(FL, MatrixIn, Reduced),
    Reduced \== [],
    (   solved_matrix(Reduced)
    ->  MatrixSolved = Reduced
    ;   select_branch_cell_heuristic(Reduced, R, C, Domain),
        domain_choice(Reduced, R, C, Domain, Cycle, Value),
        set_cell(Reduced, R, C, [Value], NextMatrix),
        solve_matrix(FL, Cycle, NextMatrix, MatrixSolved)
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


% Előre + vissza spirál szűkítés és domain metszet ismétlés.
spiral_fixpoint(FL, SpiralIn, SpiralOut) :-
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


% Két teljesen konkretizált (szám) domain összevetése.
intersect_domains(A, B, R) :-
    integer(A),
    integer(B),
    !,
    ( A =:= B -> R = A ; fail ).


% Szám vs lista domain metszése (szám benne van-e listában).
intersect_domains(A, B, R) :-
    integer(A),
    is_list(B),
    !,
    ( ord_memberchk(A, B) -> R = A ; fail ).


% Lista vs szám domain metszése (szám benne van-e listában).
intersect_domains(A, B, R) :-
    is_list(A),
    integer(B),
    !,
    ( ord_memberchk(B, A) -> R = B ; fail ).


% Két lista domain metszése; üres metszet esetén bukás.
intersect_domains(A, B, R) :-
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
domain_choice(Matrix, R, C, Domain, Cycle, Value) :-
    include(pos_value, Domain, Positives),
    (   Positives \= [],
        expected_spiral_value(Matrix, R, C, Cycle, Expected),
        member(Expected, Positives)
    ->  CandidatePositives = [Expected]
    ;   CandidatePositives = Positives
    ),
    CandidatePositives \= [],
    score_positive_choices(Matrix, R, C, CandidatePositives, Scored),
    member(_Score-Value, Scored).
domain_choice(Matrix, R, C, Domain, Cycle, 0) :-
    member(0, Domain),
    zero_allowed(Matrix, R, C, Domain, Cycle).


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


expected_spiral_value(Matrix, R, C, Cycle, Expected) :-
    length(Matrix, N),
    spiral_positions(N, Positions),
    count_positive_before(Positions, Matrix, R, C, 0, Count),
    Rem is Count mod Cycle,
    Expected is Rem + 1.


count_positive_before([], _, _, _, _, _) :-
    fail.
count_positive_before([pos(R,C)|_], _Matrix, R, C, Count, Count) :- !.
count_positive_before([pos(Ri,Ci)|Rest], Matrix, R, C, CountIn, CountOut) :-
    single_value(Matrix, Ri, Ci, Value),
    (   Value > 0
    ->  CountMid is CountIn + 1
    ;   CountMid = CountIn
    ),
    count_positive_before(Rest, Matrix, R, C, CountMid, CountOut).


single_value(Matrix, R, C, Value) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell),
    (   integer(Cell)
    ->  Value = Cell
    ;   Cell = [Value]
    ).


cell_domain_list(Matrix, R, C, Domain) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell),
    (   integer(Cell)
    ->  Domain = [Cell]
    ;   Domain = Cell
    ).


zero_allowed(Matrix, R, C, Domain, Cycle) :-
    include(pos_value, Domain, Positives),
    (   Positives = []
    ->  true
    ;   expected_spiral_value(Matrix, R, C, Cycle, Expected),
        future_can_place(Matrix, R, C, Expected),
        row_positive_slack(Matrix, R, Cycle),
        column_positive_slack(Matrix, C, Cycle)
    ).


future_can_place(Matrix, R, C, Expected) :-
    length(Matrix, N),
    spiral_positions(N, Positions),
    positions_after(Positions, R, C, Rest),
    Rest \= [],
    future_can_place_from(Rest, Matrix, Expected).


positions_after([], _, _, []).
positions_after([pos(R,C)|Rest], R, C, Rest) :- !.
positions_after([_|Rest], R, C, Tail) :-
    positions_after(Rest, R, C, Tail).


future_can_place_from([], _, _) :- fail.
future_can_place_from([pos(R,C)|Rest], Matrix, Expected) :-
    cell_domain_list(Matrix, R, C, Domain),
    include(pos_value, Domain, Positives),
    (   Positives = []
    ->  future_can_place_from(Rest, Matrix, Expected)
    ;   member(Expected, Positives)
    ->  true
    ;   member(0, Domain)
    ->  future_can_place_from(Rest, Matrix, Expected)
    ;   fail
    ).


row_positive_slack(Matrix, R, Cycle) :-
    row_positive_stats(Matrix, R, Cycle, Need, Capacity),
    (   Need =< 0
    ->  true
    ;   Capacity > Need
    ).


column_positive_slack(Matrix, C, Cycle) :-
    column_positive_stats(Matrix, C, Cycle, Need, Capacity),
    (   Need =< 0
    ->  true
    ;   Capacity > Need
    ).


row_positive_stats(Matrix, R, Cycle, Need, Capacity) :-
    nth1(R, Matrix, Row),
    positive_stats_list(Row, Cycle, Need, Capacity).


column_positive_stats(Matrix, C, Cycle, Need, Capacity) :-
    get_column_values(Matrix, C, Column),
    positive_stats_list(Column, Cycle, Need, Capacity).


positive_stats_list(Cells, Cycle, Need, Capacity) :-
    positive_stats_list(Cells, 0, 0, Assigned, Candidates),
    Need is Cycle - Assigned,
    Capacity = Candidates.


positive_stats_list([], Assigned, Candidates, Assigned, Candidates).
positive_stats_list([Cell|Rest], AccAssigned, AccCand, Assigned, Candidates) :-
    (   cell_assigned_positive(Cell)
    ->  NewAssigned is AccAssigned + 1,
        NewCand = AccCand
    ;   cell_can_still_be_positive(Cell)
    ->  NewAssigned = AccAssigned,
        NewCand is AccCand + 1
    ;   NewAssigned = AccAssigned,
        NewCand = AccCand
    ),
    positive_stats_list(Rest, NewAssigned, NewCand, Assigned, Candidates).


cell_assigned_positive(Cell) :-
    (   integer(Cell),
        Cell > 0
    )
    ;   (   is_list(Cell),
            Cell = [Value],
            Value > 0
        ).


cell_can_still_be_positive(Cell) :-
    is_list(Cell),
    \+ (Cell = [Value], Value > 0),
    member(Value, Cell),
    Value > 0.


select_branch_cell_heuristic(Matrix, R, C, Domain) :-
    length(Matrix, N),
    spiral_positions(N, Positions),
    select_min_domain_cell(Matrix, Positions, R, C, Domain).


select_min_domain_cell(Matrix, Positions, R, C, Domain) :-
    Max is 10_000_000,
    select_min_domain_cell(Matrix, Positions, Max, 0, 0, [], R, C, Domain).

select_min_domain_cell(_, [], BestLen, BestR, BestC, BestDomain, BestR, BestC, BestDomain) :-
    BestLen < 10_000_000.

select_min_domain_cell(Matrix, [pos(R0,C0)|Rest], CurBestLen, CurBestR, CurBestC, CurBestDomain,
                       R, C, Domain) :-
    nth1(R0, Matrix, Row),
    nth1(C0, Row, Cell),
    (   is_list(Cell),
        length(Cell, Len),
        Len > 1,
        Len < CurBestLen
    ->  NewBestLen = Len,
        NewBestR = R0,
        NewBestC = C0,
        NewBestDomain = Cell
    ;   NewBestLen = CurBestLen,
        NewBestR = CurBestR,
        NewBestC = CurBestC,
        NewBestDomain = CurBestDomain
    ),
    select_min_domain_cell(Matrix, Rest, NewBestLen, NewBestR, NewBestC, NewBestDomain,
                           R, C, Domain).


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
% ---- Beépített Khf5 kód ----
% ------------------------------------------------------------
% Khf5 – Számtekercs: kezdő tábla és ismert szűkítés
%
% @author "Toronyi Zsombor <toronyizsombor@edu.bme.hu> [S8F7DV]"
% @date   "2025-11-13" 
% ------------------------------------------------------------



:- use_module(library(lists)).  % ensures availability of nth1/4 and related list predicates



% kezdotabla(+FLeiro, -Mx)
% FLeiro = szt(N, M, Givens), ahol Givens listája i(R,C,E) strukturák.
% Az Mx mátrix N x N, minden cella domainje:
%  - ha adott: [E]
%  - különben: [0..M], ha N > M; vagy [1..M], ha N = M

kezdotabla(szt(N, M, Givens), Mx) :-
	base_domain(N, M, Dom),
	make_matrix(N, N, Dom, Mx0),
	apply_givens(Givens, Mx0, Mx).



% ismert_szukites(+FLeiro, +Mx0, -Mx)
% Ismert (egyelemű lista) értékekből induló sor/oszlop-alapú szűkítések
% ismétlése mindaddig, amíg létezik egyelemű lista. Ha nem történik
% szűkítés, az eljárás meghiúsul. Ha ellentmondás adódik, Mx = [].

ismert_szukites(szt(N, M, _), Mx0, Mx) :-
	zero_quota(N, M, Z),
	propagate_until_fixpoint(Mx0, N, M, Z, Mx1, DidChange),
	( Mx1 == [] -> Mx = []
	; DidChange == true -> Mx = Mx1
	; % nem volt egyelemű tartomány, vagy nem történt szűkítés
	  fail
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Segédeljárások – kezdotabla


base_domain(N, M, Dom) :-
	( N > M -> range_list(0, M, Dom)
	; N =:= M -> range_list(1, M, Dom)
	).


range_list(Lo, Hi, L) :-
	Lo =< Hi,
	range_list_(Lo, Hi, L).


range_list_(I, Hi, [I|Rest]) :-
	I < Hi, !,
	I1 is I + 1,
	range_list_(I1, Hi, Rest).
range_list_(Hi, Hi, [Hi]).


make_matrix(0, _Cols, _Elem, []).
make_matrix(Rows, Cols, Elem, [Row|Rest]) :-
	Rows > 0,
	make_row(Cols, Elem, Row),
	R1 is Rows - 1,
	make_matrix(R1, Cols, Elem, Rest).


make_row(0, _Elem, []).
make_row(N, Elem, [Elem|Rest]) :-
	N > 0,
	N1 is N - 1,
	make_row(N1, Elem, Rest).


apply_givens([], Mx, Mx).
apply_givens([i(R,C,E)|Gs], Mx0, Mx) :-
	set_cell(Mx0, R, C, [E], Mx1),
	apply_givens(Gs, Mx1, Mx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Segédeljárások – ismert_szukites


% Z = n - m – a 0-k száma egy sorban/oszlopban
zero_quota(N, M, Z) :- Z is N - M.


% propagate_until_fixpoint(+Mx0, +N, +M, +Z, -Mx, -DidChange)
% Iteratív szűkítés: mindig kiválasztunk egy egyelemű listát és propagálunk.
% Ha nincs több egyelemű lista, visszaadjuk a jelenlegi mátrixot.
propagate_until_fixpoint(Mx0, N, M, Z, Mx, DidChange) :-
	find_singleton_cell(Mx0, R, C, E), !,
	( E > 0 ->
		propagate_fixed_positive(Mx0, R, C, E, Mx1)
	  ; % E == 0
		propagate_fixed_zero(Mx0, N, M, Z, R, C, Mx1)
	),
	( Mx1 == [] ->
		Mx = [], DidChange = true
	;
		propagate_until_fixpoint(Mx1, N, M, Z, Mx, _),
		DidChange = true
	).
propagate_until_fixpoint(Mx, _N, _M, _Z, Mx, false).


% Egyelemű lista keresése (balról-jobbra, fentről-lefelé)
find_singleton_cell(Mx, R, C, E) :-
	get_row_at_index(Mx, 1, R, Row),
	find_singleton_in_row(Row, 1, C, E).
find_singleton_cell([_|Rs], R, C, E) :-
	find_singleton_cell(Rs, R1, C, E),
	R is R1 + 1.


get_row_at_index([Row|_], R, R, Row).
get_row_at_index([_|Rs], I, R, Row) :- I1 is I + 1, get_row_at_index(Rs, I1, R, Row).


find_singleton_in_row([X|_], C, C, E) :- is_singleton_list(X, E), !.
find_singleton_in_row([_|Xs], I, C, E) :- I1 is I + 1, find_singleton_in_row(Xs, I1, C, E).


is_singleton_list([E], E) :- integer(E).


% Nem nulla egyelemű propagáció
propagate_fixed_positive(Mx0, R, C, E, Mx) :-
	% 1) Sor szűkítés (R. sor, kivéve (R,C) cella)
	nth1(R, Mx0, RowR, RestRows),
	remove_value_from_row_except_index(RowR, C, E, NewRowR, FlagR),
	( FlagR = contradiction -> Mx = [] ;
	  % 2) Ideiglenes mátrix összeállítása az új sorral
	  set_matrix_row_at_index(RestRows, R, NewRowR, TempMx),
	  % 3) Oszlop szűkítés (C. oszlop minden sorban, kivéve R)
	  remove_value_from_column_except_row(TempMx, R, C, E, MxCol, FlagC),
	  ( FlagC = contradiction -> Mx = [] ;
		% 4) [E] -> E az (R,C) cellában
		set_cell(MxCol, R, C, E, Mx)
	  )
	).


% 0 egyelemű propagáció
propagate_fixed_zero(Mx0, _N, _M, Z, R, C, Mx) :-
	% 1) Az R. sorban és C. oszlopban minden [0] -> 0
	nth1(R, Mx0, RowR, RestRows1),
	collapse_zero_singleton_in_row(RowR, RowR1),
	collapse_zero_singleton_in_column(RestRows1, C, RestRows2),
	set_matrix_row_at_index(RestRows2, R, RowR1, M1),
	% 2) Sor-ellenőrzés és esetleges 0 elhagyás az R. sorban
	count_zeros_in_list(RowR1, ZRow),
	( ZRow > Z -> Mx = []
	; ZRow =:= Z ->
		remove_zero_from_row_domain_lists(RowR1, RowR2, FlagR),
		( FlagR = contradiction -> Mx = []
		; set_matrix_row_at_index(RestRows2, R, RowR2, M2a)
		)
	; % ZRow < Z, nincs teendő
	  M2a = M1
	),
	( M2a == [] -> Mx = []
	;  % 3) Oszlop-ellenőrzés
	  get_column_at_index(M2a, C, Col0),
	  count_zeros_in_list(Col0, ZCol0),
	  ( ZCol0 > Z -> Mx = []
	  ; ZCol0 =:= Z ->
		  remove_zero_from_column_domain_lists(M2a, C, M2b, FlagC0),
		  ( FlagC0 = contradiction -> Mx = []
		  ; get_column_at_index(M2b, C, Col1),
		    count_zeros_in_list(Col1, ZCol1),
		    ( ZCol1 =:= Z -> % teljes redukció a C oszlopban is
			  Mx = M2b
		      ; Mx = M2b )
		  )
	  ; Mx = M2a
	  )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sor/oszlop műveletek és domain-kezelés


% remove_value_from_row_except_index(+Row0, +SkipC, +E, -Row, -Flag)
% SkipC: annak az oszlopnak az indexe (1-alapú), amelyet nem szűkítünk.
remove_value_from_row_except_index(Row0, SkipC, E, Row, Flag) :-
    remove_value_from_row_except_index_(Row0, 1, SkipC, E, [], Rev, ok, Flag),
    reverse(Rev, Row).

remove_value_from_row_except_index_([], _Idx, _Skip, _E, Acc, Acc, Flag, Flag).
remove_value_from_row_except_index_([Cell|Rest], Idx, Skip, E, Acc, Out, _FlagIn, FlagOut) :-
	( Idx =:= Skip -> % skip this position
		Acc1 = [Cell|Acc], Flag1 = ok
	; remove_value_from_cell_domain(Cell, E, Cell1, Flag1),
	  ( Flag1 = contradiction -> FlagOut = contradiction, Out = [Cell|Rest] % early exit, keep remaining tail as-is
	  ; Acc1 = [Cell1|Acc]
	  )
	),
	( Flag1 = contradiction -> true
	; Idx1 is Idx + 1,
	  remove_value_from_row_except_index_(Rest, Idx1, Skip, E, Acc1, Out, ok, FlagOut)
	).


% remove_value_from_column(+Rows0, +SkipR, +C, +E, -Rows, -Flag)
% SkipR: annak a sornak az indexe (1-alapú), amelyet nem szűkítünk.
% Új oszlop-szűkítés teljes mátrixon: eredeti sorszám alapján kihagyja SkipR-t
remove_value_from_column_except_row(Mx0, SkipR, C, E, Mx, Flag) :-
    remove_value_from_column_except_row_(Mx0, 1, SkipR, C, E, [], Rev, ok, Flag),
    reverse(Rev, Mx).

remove_value_from_column_except_row_([], _Cur, _SkipR, _C, _E, Acc, Acc, Flag, Flag).
remove_value_from_column_except_row_([Row|Rs], Cur, SkipR, C, E, Acc, Out, _Fin, FlagOut) :-
	( Cur =:= SkipR -> % hagyjuk ezt a sort
		Row1 = Row, Flag1 = ok
	; remove_value_in_row_at_column(Row, C, E, Row1, Flag1)
	),
	( Flag1 = contradiction -> FlagOut = contradiction, Out = [Row|Rs]
	; Acc1 = [Row1|Acc], Cur1 is Cur + 1,
	  remove_value_from_column_except_row_(Rs, Cur1, SkipR, C, E, Acc1, Out, ok, FlagOut)
	).


% remove_value_in_row_at_column(+Row0, +C, +E, -Row, -Flag)
remove_value_in_row_at_column(Row0, C, E, Row, Flag) :-
        remove_value_in_row_at_column_(Row0, 1, C, E, [], Rev, ok, Flag),
        reverse(Rev, Row).

remove_value_in_row_at_column_([], _Idx, _C, _E, Acc, Acc, Flag, Flag).
remove_value_in_row_at_column_([Cell|Rest], Idx, C, E, Acc, Out, _FlagIn, FlagOut) :-
		( Idx =:= C ->
				remove_value_from_cell_domain(Cell, E, Cell1, Flag1),
				( Flag1 = contradiction -> FlagOut = contradiction, Out = [Cell|Rest] ; Acc1 = [Cell1|Acc] )
		; Acc1 = [Cell|Acc], Flag1 = ok
		),
		( Flag1 = contradiction -> true
		; Idx1 is Idx + 1,
			remove_value_in_row_at_column_(Rest, Idx1, C, E, Acc1, Out, ok, FlagOut)
		).


% remove_value_from_cell_domain(+Cell0, +E, -Cell, -Flag)
% Cell0 lehet egész vagy lista. Ha egész==E és el kellene hagyni E-t,
% az ellentmondás. Ha lista, E eltávolítása, üressé válás -> ellentmondás.
remove_value_from_cell_domain(Cell0, E, _Cell, contradiction) :- integer(Cell0), Cell0 =:= E, !.
remove_value_from_cell_domain(Cell0, _E, Cell, ok) :- integer(Cell0), !,
	Cell = Cell0.
remove_value_from_cell_domain([X], E, [], contradiction) :- X =:= E, !.
remove_value_from_cell_domain([X], E, [X], ok) :- X =\= E, !.
remove_value_from_cell_domain(List0, E, List, ok) :-
	is_list(List0),
	remove_all_occurrences(List0, E, List),
	List \= [], !.
remove_value_from_cell_domain(_List0, _E, [], contradiction).


remove_all_occurrences([], _E, []).
remove_all_occurrences([E|Xs], E, Ys) :- !, remove_all_occurrences(Xs, E, Ys).
remove_all_occurrences([X|Xs], E, [X|Ys]) :- remove_all_occurrences(Xs, E, Ys).


% [0] -> 0 a sorban
collapse_zero_singleton_in_row([], []).
collapse_zero_singleton_in_row([[0]|Xs], [0|Ys]) :- !, collapse_zero_singleton_in_row(Xs, Ys).
collapse_zero_singleton_in_row([X|Xs], [X|Ys]) :- collapse_zero_singleton_in_row(Xs, Ys).


% [0] -> 0 az oszlopban (minden sor C. eleme)
collapse_zero_singleton_in_column([], _C, []).
collapse_zero_singleton_in_column([Row|Rs], C, [Row1|Rs1]) :-
	nth1(C, Row, Cell),
	( Cell = [0] -> Cell1 = 0 ; Cell1 = Cell ),
	set_nth1(Row, C, Cell1, Row1),
	collapse_zero_singleton_in_column(Rs, C, Rs1).


% 0-k száma egy sorban/oszlopban
count_zeros_in_list(List, Count) :- count_zeros_in_list_(List, 0, Count).
count_zeros_in_list_([], Acc, Acc).
count_zeros_in_list_([0|Xs], Acc, C) :- !, Acc1 is Acc + 1, count_zeros_in_list_(Xs, Acc1, C).
count_zeros_in_list_([X|Xs], Acc, C) :-
	( is_list(X) -> count_zeros_in_list_(Xs, Acc, C)
	; count_zeros_in_list_(Xs, Acc, C)
	).


% Ha a sorban elérte a 0-k száma a Z értéket, a NEM 0 elemekből
% (listákból) elhagyjuk a 0-t. 0 értékű egészeket nem bántjuk.
remove_zero_from_row_domain_lists([], [], ok).
remove_zero_from_row_domain_lists([0|Xs], [0|Ys], Flag) :- !,
	remove_zero_from_row_domain_lists(Xs, Ys, Flag).
remove_zero_from_row_domain_lists([Cell|Xs], [Cell1|Ys], Flag) :-
	( integer(Cell) -> Cell1 = Cell, remove_zero_from_row_domain_lists(Xs, Ys, Flag)
	; remove_value_from_cell_domain(Cell, 0, Cell1, Flag1),
	  ( Flag1 = contradiction -> Flag = contradiction, Ys = Xs
	  ; remove_zero_from_row_domain_lists(Xs, Ys, Flag)
	  )
	).


% Oszlop megfelelője a fenti sorműveletnek
remove_zero_from_column_domain_lists(Mx0, C, Mx, Flag) :-
	remove_zero_from_column_domain_lists_(Mx0, C, [], RevRows, ok, Flag),
	reverse(RevRows, Mx).


remove_zero_from_column_domain_lists_([], _C, Acc, Acc, Flag, Flag).
remove_zero_from_column_domain_lists_([Row|Rs], C, Acc, Out, _FlagIn, FlagOut) :-
	remove_zero_in_row_at_column(Row, C, NewRow, Flag1),
	( Flag1 = contradiction -> FlagOut = contradiction, Out = [Row|Rs]
	; remove_zero_from_column_domain_lists_(Rs, C, [NewRow|Acc], Out, ok, FlagOut)
	).


% remove_zero_in_row_at_column(+Row0,+C,-Row,-Flag) removes 0 from list at column C if present.
remove_zero_in_row_at_column(Row0, C, Row, Flag) :-
    remove_zero_in_row_at_column_(Row0, 1, C, [], Rev, ok, Flag),
    reverse(Rev, Row).

remove_zero_in_row_at_column_([], _Idx, _C, Acc, Acc, Flag, Flag).
remove_zero_in_row_at_column_([Cell|Rest], Idx, C, Acc, Out, _FlagIn, FlagOut) :-
	( Idx =:= C ->
		( Cell = 0 -> Cell1 = 0, Flag1 = ok
		; integer(Cell) -> Cell1 = Cell, Flag1 = ok
		; remove_value_from_cell_domain(Cell, 0, Cell1, Flag1)
		),
		( Flag1 = contradiction -> FlagOut = contradiction, Out = [Cell|Rest]
		; Acc1 = [Cell1|Acc]
		)
	; Acc1 = [Cell|Acc], Flag1 = ok
	),
	( Flag1 = contradiction -> true
	; Idx1 is Idx + 1,
	  remove_zero_in_row_at_column_(Rest, Idx1, C, Acc1, Out, ok, FlagOut)
	).


% Mátrix oszlopának kiolvasása
get_column_at_index([], _C, []).
get_column_at_index([Row|Rs], C, [X|Xs]) :- nth1(C, Row, X), get_column_at_index(Rs, C, Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Általános listakezelő segédek

% nth1/set_nth1 – 1-alapú indexelés beépített predikátumokra támaszkodva, de
% itt biztosítunk set_nth1-nek egy tiszta definíciót.
set_nth1([_|Xs], 1, E, [E|Xs]).
set_nth1([X|Xs], I, E, [X|Ys]) :- I > 1, I1 is I - 1, set_nth1(Xs, I1, E, Ys).


% Mátrix sor „visszahelyezés” (1-alapú beillesztés) egy olyan listába,
% amelyből az adott sor korábban ki lett véve (RestRows hossza N-1).
set_matrix_row_at_index(Rs, 1, Row, [Row|Rs]).
set_matrix_row_at_index([R0|Rs], I, Row, [R0|Rs1]) :- I > 1, I1 is I - 1, set_matrix_row_at_index(Rs, I1, Row, Rs1).


% Mátrix cellabeállítás (1-alapú): (R,C) pozícióra Value
set_cell(Mx0, R, C, Value, Mx) :-
	nth1(R, Mx0, Row, RestRows),
	set_nth1(Row, C, Value, NewRow),
	set_matrix_row_at_index(RestRows, R, NewRow, Mx).
% ---- Beépített Khf6 kód ----
% ------------------------------------------------------------
% Khf6 – Számtekercs: kizárásos szűkítés
%
% @author "Toronyi Zsombor <toronyizsombor@edu.bme.hu> [S8F7DV]"
% @date   "2025-11-19" 
% ------------------------------------------------------------


:- use_module(library(lists)).


% kizarasos_szukites/4: Egy kizárásos szűkítési lépést hajt végre sorokra majd oszlopokra vizsgálva.
kizarasos_szukites(szt(BoardSize, CycleMax, _), MatrixIn, MatrixOut, Restriction) :-
		ZeroQuota is BoardSize - CycleMax,
		% 1) Sorok vizsgálata növekvő sorszám szerint, Value = 0..CycleMax
		( find_row_restriction_choice(MatrixIn, BoardSize, CycleMax, ZeroQuota,
																	RowIndex, Value, ChoiceAction) ->
				apply_row_restriction_choice(MatrixIn, RowIndex, Value, ChoiceAction, MatrixOut, Restriction)
		; % 2) Oszlopok vizsgálata növekvő oszlopszám szerint, Value = 0..CycleMax
			find_column_restriction_choice(MatrixIn, BoardSize, CycleMax, ZeroQuota,
																		 ColumnIndex, Value, ChoiceAction) ->
				apply_column_restriction_choice(MatrixIn, ColumnIndex, Value, ChoiceAction, MatrixOut, Restriction)
		; % 3) Nincs alkalmazható szűkítés
			fail
		).


% ------------------------------


% Sorok: keresés


% between/3:
between(Low, High, Low) :-
	Low =< High.
between(Low, High, Value) :-
	Low < High,
	Next is Low + 1,
	between(Next, High, Value).


% find_row_restriction_choice/7: Kiválasztja az első olyan sort és értéket, amelyre szűkítés vagy ellentmondás adódik.
% ChoiceAction ∈ { contradiction, zeros_exact, unique(ColumnIndex) }
find_row_restriction_choice(Matrix, BoardSize, CycleMax, ZeroQuota,
														RowIndex, Value, ChoiceAction) :-
		between(1, BoardSize, RowIndex),
		nth1(RowIndex, Matrix, RowValues),
		between(0, CycleMax, Value),
		row_value_statistics(RowValues, Value, CandidateCount, HasAnyList, UniqueListIndex),
		( Value =:= 0 ->
				( CandidateCount < ZeroQuota -> ChoiceAction = contradiction
				; CandidateCount =:= ZeroQuota, HasAnyList == true -> ChoiceAction = zeros_exact
				; fail
				)
		; % Value > 0
			( CandidateCount =:= 0 -> ChoiceAction = contradiction
			; CandidateCount =:= 1, HasAnyList == true, integer(UniqueListIndex) -> ChoiceAction = unique(UniqueListIndex)
			; fail
			)
		), !.


% row_value_statistics/5: Megszámolja, hány cella veheti fel az adott értéket a sorban.
% - CandidateCount: hány pozíció veheti fel az értéket
% - HasAnyList: igaz, ha a felvevők között van lista
% - UniqueListIndex: ha CandidateCount=1 és az egyetlen előfordulás lista, annak indexe, különben szabad változó
row_value_statistics(RowValues, Value, CandidateCount, HasAnyList, UniqueListIndex) :-
	row_value_statistics_(RowValues, Value, 1, 0, false, none,
						  CandidateCount, HasAnyList, UniqueListIndex).

row_value_statistics_([], _Value, _Index, AccCount, AccHasList, AccUnique,
					  AccCount, AccHasList, UniqueIdx) :-
	( AccUnique = some(J) -> UniqueIdx = J ; UniqueIdx = _ ).
row_value_statistics_([Cell|Rest], Value, Index, AccCount, AccHasList, AccUnique,
					  Count, HasList, UniqueIdx) :-
	( cell_can_take_value(Cell, Value) ->
		NewCount is AccCount + 1,
		( is_list(Cell) -> NewHasList = true ; NewHasList = AccHasList ),
		( NewCount =:= 1, is_list(Cell) -> NewUnique = some(Index) ; NewUnique = none )
	; NewCount = AccCount, NewHasList = AccHasList, NewUnique = AccUnique
	),
	NextIndex is Index + 1,
	row_value_statistics_(Rest, Value, NextIndex, NewCount, NewHasList, NewUnique,
						  Count, HasList, UniqueIdx).


% ------------------------------


% Oszlopok: keresés

% find_column_restriction_choice/7: Kiválasztja az első olyan oszlopot és értéket, amelyre szűkítés vagy ellentmondás adódik.
% ChoiceAction ∈ { contradiction, zeros_exact, unique(RowIndex) }
find_column_restriction_choice(Matrix, BoardSize, CycleMax, ZeroQuota,
															 ColumnIndex, Value, ChoiceAction) :-
		between(1, BoardSize, ColumnIndex),
		get_column_values(Matrix, ColumnIndex, ColumnValues),
		between(0, CycleMax, Value),
		column_value_statistics(ColumnValues, Value, CandidateCount, HasAnyList, UniqueListIndex),
		( Value =:= 0 ->
				( CandidateCount < ZeroQuota -> ChoiceAction = contradiction
				; CandidateCount =:= ZeroQuota, HasAnyList == true -> ChoiceAction = zeros_exact
				; fail
				)
		; % Value > 0
			( CandidateCount =:= 0 -> ChoiceAction = contradiction
			; CandidateCount =:= 1, HasAnyList == true, integer(UniqueListIndex) -> ChoiceAction = unique(UniqueListIndex)
			; fail
			)
		), !.


% column_value_statistics/5: Megszámolja, hány cella veheti fel az adott értéket az oszlopban, és jelzi az egyedi listás pozíciót.
column_value_statistics(ColumnValues, Value, CandidateCount, HasAnyList, UniqueListIndex) :-
		column_value_statistics_(ColumnValues, Value, 1, 0, false, none,
														 CandidateCount, HasAnyList, UniqueListIndex).

column_value_statistics_([], _Value, _Index, AccCount, AccHasList, AccUnique,
												 AccCount, AccHasList, UniqueIdx) :-
		( AccUnique = some(J) -> UniqueIdx = J ; UniqueIdx = _ ).
column_value_statistics_([Cell|Rest], Value, Index, AccCount, AccHasList, AccUnique,
												 Count, HasList, UniqueIdx) :-
		( cell_can_take_value(Cell, Value) ->
				NewCount is AccCount + 1,
				( is_list(Cell) -> NewHasList = true ; NewHasList = AccHasList ),
				( NewCount =:= 1, is_list(Cell) -> NewUnique = some(Index) ; NewUnique = none )
		; NewCount = AccCount, NewHasList = AccHasList, NewUnique = AccUnique
		),
		NextIndex is Index + 1,
		column_value_statistics_(Rest, Value, NextIndex, NewCount, NewHasList, NewUnique,
														 Count, HasList, UniqueIdx).


% ------------------------------


% Alkalmazás – sor

% apply_row_restriction_choice/6: Alkalmazza a kiválasztott sorra a szűkítést vagy jelzi az ellentmondást.
apply_row_restriction_choice(_MatrixIn, _RowIndex, _Value, contradiction, [], nem) :- !.
apply_row_restriction_choice(MatrixIn, RowIndex, 0, zeros_exact, MatrixOut, sor(RowIndex,0)) :- !,
    nth1(RowIndex, MatrixIn, RowValues),
    narrow_zeros_to_singleton_in_row(RowValues, NewRowValues),
    set_row_in_matrix(MatrixIn, RowIndex, NewRowValues, MatrixOut).
apply_row_restriction_choice(MatrixIn, RowIndex, Value, unique(ColumnIndex), MatrixOut, sor(RowIndex,Value)) :-
    integer(Value), Value > 0,
    nth1(RowIndex, MatrixIn, RowValues),
    set_nth1_list(RowValues, ColumnIndex, [Value], NewRowValues),
    set_row_in_matrix(MatrixIn, RowIndex, NewRowValues, MatrixOut).


% ------------------------------


% Alkalmazás – oszlop

% apply_column_restriction_choice/6: Alkalmazza a kiválasztott oszlopra a szűkítést vagy jelzi az ellentmondást.
apply_column_restriction_choice(_MatrixIn, _ColumnIndex, _Value, contradiction, [], nem) :- !.
apply_column_restriction_choice(MatrixIn, ColumnIndex, 0, zeros_exact, MatrixOut, oszl(ColumnIndex,0)) :- !,
    narrow_zeros_to_singleton_in_column(MatrixIn, ColumnIndex, MatrixOut).
apply_column_restriction_choice(MatrixIn, ColumnIndex, Value, unique(RowIndex), MatrixOut, oszl(ColumnIndex,Value)) :-
    integer(Value), Value > 0,
    set_cell_in_matrix(MatrixIn, RowIndex, ColumnIndex, [Value], MatrixOut).


% ------------------------------


% Segédeljárások

% cell_can_take_value/2: Igaz, ha a cella felveheti a Value értéket.
cell_can_take_value(Cell, Value) :- integer(Cell), Cell =:= Value, !.
cell_can_take_value(Cell, Value) :- is_list(Cell), member(Value, Cell).


% get_column_values/3: Kiolvassa az adott oszlop elemeit a mátrixból.
get_column_values([], _ColumnIndex, []).
get_column_values([Row|RestRows], ColumnIndex, [Cell|RestCells]) :-
	nth1(ColumnIndex, Row, Cell),
	get_column_values(RestRows, ColumnIndex, RestCells).


% set_row_in_matrix/4: Beállítja az adott sor tartalmát a mátrixban.
set_row_in_matrix(MatrixIn, RowIndex, NewRow, MatrixOut) :-
	nth1(RowIndex, MatrixIn, _OldRow, RestRows),
	insert_row_at_index(RestRows, RowIndex, NewRow, MatrixOut).

% insert_row_at_index/4: Beszúrja a sort az adott indexre.
insert_row_at_index(RowsTail, 1, Row, [Row|RowsTail]).
insert_row_at_index([Row0|RowsTail], Index, Row, [Row0|RowsOut]) :-
	Index > 1, Next is Index - 1, insert_row_at_index(RowsTail, Next, Row, RowsOut).


% set_cell_in_matrix/5: Beállítja a (RowIndex,ColumnIndex) cella értékét a mátrixban.
set_cell_in_matrix(MatrixIn, RowIndex, ColumnIndex, Value, MatrixOut) :-
    nth1(RowIndex, MatrixIn, Row, RestRows),
    set_nth1_list(Row, ColumnIndex, Value, NewRow),
    insert_row_at_index(RestRows, RowIndex, NewRow, MatrixOut).


% set_nth1_list/4: Beállítja a lista adott indexén az elemet.
set_nth1_list([_|Xs], 1, E, [E|Xs]).
set_nth1_list([X|Xs], I, E, [X|Ys]) :- I > 1, I1 is I - 1, set_nth1_list(Xs, I1, E, Ys).


% narrow_zeros_to_singleton_in_row/2: Minden 0-t tartalmazó listát [0]-ra szűkít a sorban.
narrow_zeros_to_singleton_in_row([], []).
narrow_zeros_to_singleton_in_row([Cell|Rest], [CellOut|RestOut]) :-
    ( is_list(Cell), member(0, Cell) -> CellOut = [0]
    ; CellOut = Cell
    ),
    narrow_zeros_to_singleton_in_row(Rest, RestOut).


% narrow_zeros_to_singleton_in_column/3: Minden 0-t tartalmazó listát [0]-ra szűkít az oszlopban.
narrow_zeros_to_singleton_in_column([], _ColumnIndex, []).
narrow_zeros_to_singleton_in_column([Row|RestRows], ColumnIndex, [RowOut|RestOut]) :-
    nth1(ColumnIndex, Row, Cell),
    ( is_list(Cell), member(0, Cell) -> set_nth1_list(Row, ColumnIndex, [0], RowOut)
    ; RowOut = Row
    ),
    narrow_zeros_to_singleton_in_column(RestRows, ColumnIndex, RestOut).
% ---- Beépített Khf7 kód ----
% ------------------------------------------------------------
% Khf7 – Számtekercs: számtekercs szűkítése
%
% @author "Toronyi Zsombor <toronyizsombor@edu.bme.hu> [S8F7DV]"
% @date   "2025-11-19" 
% ------------------------------------------------------------



% tekercs_szukites(+PuzzleDescriptor, +SpiralLine, -NarrowedSpiralLine)
tekercs_szukites(szt(_, CycleLength, _), SpiralLine, NarrowedSpiralLine) :-
	SpiralLine \= [],
	process_spiral(SpiralLine, CycleLength, [CycleLength], NarrowedSpiralLine),
	!.



% process_spiral(+SpiralList, +CycleLength, +PrevLastPositiveSet, -ResultList)
% PrevLastPositiveSet: az eddigi prefixum lehetséges utolsó pozitív elemei (ordset).
process_spiral([], _, _, []).
process_spiral([PositionDomain|RestPositions], CycleLength,
	           PrevLastPositiveSet,
	           [NarrowedDomain|RestNarrowed]) :-
	shrink_position(PositionDomain, CycleLength,
	               PrevLastPositiveSet,
	               NarrowedDomain,
	               NextLastPositiveSet),
	process_spiral(RestPositions, CycleLength,
	              NextLastPositiveSet,
	              RestNarrowed).


% shrink_position(+PositionDomain, +CycleLength, +PrevLastPositiveSet,
%                 -NarrowedDomain, -NextLastPositiveSet)
% Elvégzi az aktuális pozíció szűkítését és kiszámítja a következő utolsó pozitív halmazt.
shrink_position(PositionDomain, CycleLength,
	           PrevLastPositiveSet,
	           NarrowedDomain,
	           NextLastPositiveSet) :-
	cyclic_successors(CycleLength, PrevLastPositiveSet, SuccessorSet),
	ord_add_element(SuccessorSet, 0, AllowedValues),
	intersect_domain(PositionDomain, AllowedValues,
	                NarrowedDomain, ResultSet),
	update_last_positive_set(ResultSet, PrevLastPositiveSet,
	                        NextLastPositiveSet).


% cyclic_successors(+CycleLength, +PrevLastPositiveSet, -SuccessorSet)
cyclic_successors(CycleLength, PrevLastPositiveSet, SuccessorSet) :-
	cyclic_successors_list(PrevLastPositiveSet, CycleLength, Successors),
	list_to_ord_set(Successors, SuccessorSet).


% cyclic_successors_list(+Values, +CycleLength, -Successors)
cyclic_successors_list([], _, []).
cyclic_successors_list([Value|Rest], CycleLength, [Succ|SuccRest]) :-
	cyclic_successor(CycleLength, Value, Succ),
	cyclic_successors_list(Rest, CycleLength, SuccRest).


% cyclic_successor(+CycleLength, +Value, -Successor) : ciklikus rákövetkező
cyclic_successor(CycleLength, Value, Successor) :-
	(   Value < CycleLength
	->  Successor is Value + 1
	;   Successor = 1
	).


% intersect_domain(+DomainRep, +AllowedSet, -ResultRep, -ResultSet)
% DomainRep lehet integer vagy lista.
% AllowedSet ordset lista.
% ResultRep: kimeneti elem (integer vagy lista az előírás szerint).
% ResultSet: mindig lista ordset formában (a pozitívak és esetleg 0).
intersect_domain(DomainRep, AllowedSet, ResultRep, ResultSet) :-
	(   integer(DomainRep) ->
		( ord_memberchk(DomainRep, AllowedSet) ->
			ResultRep = DomainRep,
			ResultSet = [DomainRep]
		;   fail )
	;   % lista eset
		DomainRep = [_|_],
		ord_intersection(DomainRep, AllowedSet, Intersection),
		Intersection \= [],
		ResultRep = Intersection,
		ResultSet = Intersection
	).


% update_last_positive_set(+ResultSet, +PrevLastPositiveSet, -NextLastPositiveSet)
% ResultSet ordset (lista). PrevLastPositiveSet ordset.
% Ha 0 nincs ResultSet-ben: NextLastPositiveSet = ResultSet.
% Ha 0 van: NextLastPositiveSet = PrevLastPositiveSet ∪ (ResultSet \ {0}).
update_last_positive_set(ResultSet, PrevLastPositiveSet, NextLastPositiveSet) :-
	(   \+ ord_memberchk(0, ResultSet) ->
		NextLastPositiveSet = ResultSet
	;   remove_zero(ResultSet, PositiveOnlySet),
		ord_union(PrevLastPositiveSet, PositiveOnlySet, NextLastPositiveSet)
	).


% remove_zero(+Set, -WithoutZero)
remove_zero(Set, WithoutZero) :-
	remove_zero_rec(Set, WithoutZero).


remove_zero_rec([], []).
remove_zero_rec([0|T], R) :- !, remove_zero_rec(T, R).
remove_zero_rec([H|T], [H|R]) :- remove_zero_rec(T, R).


% ------------------------
% Ordset segédfüggvények
% ------------------------


% ord_memberchk(+Elem, +Set)
ord_memberchk(_, []) :- fail.
ord_memberchk(E, [H|T]) :-
	( E =:= H -> true
	; E < H   -> fail
	; ord_memberchk(E, T)
	).


% ord_add_element(+Set, +Elem, -Result)
ord_add_element([], E, [E]).
ord_add_element([H|T], E, [E,H|T]) :- E < H, !.
ord_add_element([H|T], E, [H|R])  :- E > H, !, ord_add_element(T, E, R).
ord_add_element([H|T], E, [H|T])  :- E =:= H.


% ord_intersection(+A, +B, -I)
ord_intersection([], _, []).
ord_intersection(_, [], []).
ord_intersection([A|As], [B|Bs], I) :-
	( A =:= B -> I = [A|R], ord_intersection(As, Bs, R)
	; A < B   -> ord_intersection(As, [B|Bs], I)
	;           ord_intersection([A|As], Bs, I)
	).


% ord_union(+A, +B, -U)
ord_union([], B, B).
ord_union(A, [], A).
ord_union([A|As], [B|Bs], U) :-
	( A =:= B -> U = [A|R], ord_union(As, Bs, R)
	; A < B   -> U = [A|R], ord_union(As, [B|Bs], R)
	;           U = [B|R], ord_union([A|As], Bs, R)
	).


% list_to_ord_set(+List, -OrdSet)
list_to_ord_set(List, OrdSet) :- list_to_ord_set_acc(List, [], OrdSet).


% list_to_ord_set_acc(+List, +Acc, -OrdSet)
list_to_ord_set_acc([], Acc, Acc).
list_to_ord_set_acc([H|T], Acc, Res) :-
	ord_add_element(Acc, H, Acc1),
	list_to_ord_set_acc(T, Acc1, Res).
