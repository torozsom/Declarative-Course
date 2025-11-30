% ------------------------------------------------------------
% Nhf2 - Számtekercs feladvány (Number Spiral Puzzle)
%
% A feladvány egy n*n méretű négyzettáblán az 1..m számok elhelyezését
% kéri úgy, hogy minden sorban és oszlopban minden szám pontosan egyszer
% szerepeljen, és a bal felső sarokból induló spirál mentén a számok
% az 1,2,...,m,1,2,...,m,... sorrendben kövessék egymást.
%
% Author: Toronyi Zsombor <toronyizsombor@edu.bme.hu> [S8F7DV]
% Date:   2025-11-30
% ------------------------------------------------------------

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FŐ BELÉPÉSI PONT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% szamtekercs(+PuzzleDescriptor, -Solution)
%
% Fő belépési pont: a feladványleíróból előállítja a megoldást.
% A PuzzleDescriptor formátuma: szt(BoardSize, CycleLength, GivenElements)
% ahol GivenElements az i(Row, Col, Value) elemek listája.
% A Solution egy listák listája, ahol minden belső lista egy sor értékeit tartalmazza.
%
szamtekercs(PuzzleDescriptor, Solution) :-
    kezdotabla(PuzzleDescriptor, InitialMatrix),
    PuzzleDescriptor = szt(BoardSize, CycleLength, GivenElements),
    spiral_positions(BoardSize, SpiralPositions),
    % Kényszerített értékek térképének felépítése
    build_forced_map(GivenElements, SpiralPositions, ForcedValueMap),
    % Spirál-alapú DFS megoldó használata
    solve_spiral_dfs(PuzzleDescriptor, CycleLength, BoardSize, SpiralPositions, 
                     ForcedValueMap, InitialMatrix, SolvedMatrix),
    matrix_to_solution(SolvedMatrix, Solution).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% KÉNYSZERÍTETT ÉRTÉKEK TÉRKÉPE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% build_forced_map(+GivenElements, +SpiralPositions, -ForcedValueMap)
%
% Felépít egy térképet, amely a spirál indexekhez rendeli a kényszerített értékeket.
% Ha egy pozícióra nincs megadott érték, akkor 0-t tárol.
%
build_forced_map(GivenElements, SpiralPositions, ForcedValueMap) :-
    length(SpiralPositions, TotalPositions),
    functor(ForcedValueMap, forced, TotalPositions),
    init_forced_map(1, TotalPositions, ForcedValueMap),
    fill_forced_map(GivenElements, SpiralPositions, ForcedValueMap).

% Inicializálja a kényszerített értékek térképét 0 értékekkel.
init_forced_map(CurrentIndex, TotalPositions, _ForcedValueMap) :- 
    CurrentIndex > TotalPositions, !.
init_forced_map(CurrentIndex, TotalPositions, ForcedValueMap) :-
    arg(CurrentIndex, ForcedValueMap, 0),
    NextIndex is CurrentIndex + 1,
    init_forced_map(NextIndex, TotalPositions, ForcedValueMap).

% Feltölti a térképet az adott elemek értékeivel.
fill_forced_map([], _, _).
fill_forced_map([i(Row, Col, Value)|RestElements], SpiralPositions, ForcedValueMap) :-
    spiral_index(SpiralPositions, Row, Col, 1, SpiralIndex),
    setarg(SpiralIndex, ForcedValueMap, Value),
    fill_forced_map(RestElements, SpiralPositions, ForcedValueMap).

% Megkeresi egy (Row, Col) pozíció spirál-indexét.
spiral_index([pos(Row, Col)|_], Row, Col, CurrentIndex, CurrentIndex) :- !.
spiral_index([_|RestPositions], Row, Col, CurrentIndex, SpiralIndex) :-
    NextIndex is CurrentIndex + 1,
    spiral_index(RestPositions, Row, Col, NextIndex, SpiralIndex).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPIRÁL-ALAPÚ MÉLYSÉGI KERESÉS (DFS)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% solve_spiral_dfs(+PuzzleDescriptor, +CycleLength, +BoardSize, +SpiralPositions,
%%                  +ForcedValueMap, +MatrixIn, -SolvedMatrix)
%
% Spirál-alapú mélységi keresés (DFS) a feladványhoz.
% Először propagációt végez a szűkítésekhez, majd mélységi kereséssel oldja meg.
%
solve_spiral_dfs(PuzzleDescriptor, CycleLength, BoardSize, SpiralPositions, 
                 ForcedValueMap, MatrixIn, SolvedMatrix) :-
    % Első lépés: kezdeti propagáció
    propagate_all(PuzzleDescriptor, SpiralPositions, MatrixIn, ReducedMatrix),
    ReducedMatrix \== [],
    % Közvetlen spirál keresés
    ZerosPerLine is BoardSize - CycleLength,
    length(SpiralPositions, TotalCells),
    TotalPositiveRequired is BoardSize * CycleLength,
    dfs_spiral(1, 0, ReducedMatrix, SpiralPositions, ForcedValueMap, CycleLength, BoardSize, 
               ZerosPerLine, TotalCells, TotalPositiveRequired, SolvedMatrix).


%% dfs_spiral/11
%
% Mélységi keresés a spirál mentén. Visszalépéses algoritmus, amely minden
% pozícióra megpróbálja elhelyezni a soron következő értéket vagy 0-t.
%
% Alap eset: minden pozíciót bejártunk
dfs_spiral(CurrentIndex, PlacedCount, Matrix, _SpiralPositions, _ForcedValueMap, 
           CycleLength, BoardSize, _ZerosPerLine, TotalCells, TotalPositiveRequired, Matrix) :-
    CurrentIndex > TotalCells, !,
    PlacedCount =:= TotalPositiveRequired,
    check_row_col_counts(Matrix, BoardSize, CycleLength).

% Rekurzív eset: feldolgozzuk a következő pozíciót
dfs_spiral(CurrentIndex, PlacedCount, Matrix, SpiralPositions, ForcedValueMap, 
           CycleLength, BoardSize, ZerosPerLine, TotalCells, TotalPositiveRequired, Solution) :-
    CurrentIndex =< TotalCells,
    % Globális kapacitás ellenőrzés - van-e elég hely a fennmaradó számoknak
    RemainingPositions is TotalCells - CurrentIndex + 1,
    RemainingNeeded is TotalPositiveRequired - PlacedCount,
    RemainingPositions >= RemainingNeeded,
    % Aktuális pozíció és cella lekérése
    nth1(CurrentIndex, SpiralPositions, pos(Row, Col)),
    nth1(Row, Matrix, RowValues),
    nth1(Col, RowValues, CellDomain),
    % Kényszerített érték lekérése (ha van)
    arg(CurrentIndex, ForcedValueMap, ForcedValue),
    % A spirálban következő elvárt érték kiszámítása
    ExpectedNextValue is (PlacedCount mod CycleLength) + 1,
    % Érték elhelyezése vagy kihagyás visszalépéssel
    try_place_bt(CellDomain, ForcedValue, ExpectedNextValue, Row, Col, Matrix, CycleLength, 
                 ZerosPerLine, UpdatedMatrix, NewPlacedCount, PlacedCount),
    NextIndex is CurrentIndex + 1,
    dfs_spiral(NextIndex, NewPlacedCount, UpdatedMatrix, SpiralPositions, ForcedValueMap, 
               CycleLength, BoardSize, ZerosPerLine, TotalCells, TotalPositiveRequired, Solution).


%% try_place_bt/11
%
% Megpróbál értéket elhelyezni vagy 0-t, visszalépéssel.
% Különböző esetek: rögzített szám, egyelemű lista, többelemű domain.
%

% Eset 1: Rögzített pozitív egész szám cellában
try_place_bt(CellValue, ForcedValue, ExpectedValue, _Row, _Col, Matrix,
             _CycleLength, _ZerosPerLine, Matrix, NewPlacedCount, CurrentPlacedCount) :-
    integer(CellValue),
    CellValue > 0, !,
    CellValue =:= ExpectedValue,
    (ForcedValue =:= 0 ; ForcedValue =:= CellValue),
    NewPlacedCount is CurrentPlacedCount + 1.

% Eset 2: Rögzített nulla cellában
try_place_bt(CellValue, ForcedValue, _ExpectedValue, _Row, _Col, Matrix,
             _CycleLength, _ZerosPerLine, Matrix, CurrentPlacedCount, CurrentPlacedCount) :-
    integer(CellValue),
    CellValue =:= 0, !,
    ForcedValue =:= 0.

% Eset 3: Egyelemű lista pozitív értékkel
try_place_bt([SingleValue], ForcedValue, ExpectedValue, Row, Col, Matrix,
             _CycleLength, _ZerosPerLine, UpdatedMatrix, NewPlacedCount, CurrentPlacedCount) :-
    SingleValue > 0, !,
    SingleValue =:= ExpectedValue,
    (ForcedValue =:= 0 ; ForcedValue =:= SingleValue),
    set_cell(Matrix, Row, Col, SingleValue, UpdatedMatrix),
    NewPlacedCount is CurrentPlacedCount + 1.

% Eset 4: Egyelemű lista nullával
try_place_bt([0], ForcedValue, _ExpectedValue, Row, Col, Matrix,
             _CycleLength, _ZerosPerLine, UpdatedMatrix, CurrentPlacedCount, CurrentPlacedCount) :- !,
    ForcedValue =:= 0,
    set_cell(Matrix, Row, Col, 0, UpdatedMatrix).

% Eset 5: Többelemű domain - pozitív érték elhelyezése
try_place_bt(CellDomain, ForcedValue, ExpectedValue, Row, Col, Matrix,
             CycleLength, _ZerosPerLine, UpdatedMatrix, NewPlacedCount, CurrentPlacedCount) :-
    is_list(CellDomain),
    length(CellDomain, DomainSize), DomainSize > 1,
    (ForcedValue =:= 0 ; ForcedValue =:= ExpectedValue),
    member(ExpectedValue, CellDomain),
    can_place_value(Matrix, Row, Col, ExpectedValue, CycleLength),
    set_cell(Matrix, Row, Col, ExpectedValue, UpdatedMatrix),
    NewPlacedCount is CurrentPlacedCount + 1.

% Eset 6: Többelemű domain - nulla elhelyezése
try_place_bt(CellDomain, ForcedValue, _ExpectedValue, Row, Col, Matrix,
             _CycleLength, ZerosPerLine, UpdatedMatrix, CurrentPlacedCount, CurrentPlacedCount) :-
    is_list(CellDomain),
    length(CellDomain, DomainSize), DomainSize > 1,
    ForcedValue =:= 0,
    member(0, CellDomain),
    can_place_zero(Matrix, Row, Col, ZerosPerLine),
    set_cell(Matrix, Row, Col, 0, UpdatedMatrix).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ÉRTÉK ELHELYEZHETŐSÉG ELLENŐRZÉSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% can_place_value(+Matrix, +Row, +Col, +Value, +CycleLength)
%
% Ellenőrzi, hogy a pozitív érték elhelyezhető-e az adott pozícióban.
% Nem lehet duplikált érték a sorban/oszlopban, és nem lépheti túl a ciklushosszt.
%
can_place_value(Matrix, Row, Col, Value, CycleLength) :-
    nth1(Row, Matrix, RowValues),
    \+ value_in_line(RowValues, Value),
    get_column_values(Matrix, Col, ColValues),
    \+ value_in_line(ColValues, Value),
    count_positive_in_line(RowValues, RowPositiveCount),
    RowPositiveCount < CycleLength,
    count_positive_in_line(ColValues, ColPositiveCount),
    ColPositiveCount < CycleLength.

% Ellenőrzi, hogy egy érték már szerepel-e a sorban/oszlopban.
value_in_line([], _) :- !, fail.
value_in_line([Cell|_], Value) :- integer(Cell), Cell =:= Value, !.
value_in_line([[SingleValue]|_], Value) :- SingleValue =:= Value, !.
value_in_line([_|RestCells], Value) :- value_in_line(RestCells, Value).

% Megszámolja a pozitív értékeket egy sorban/oszlopban.
count_positive_in_line(Line, Count) :-
    count_positive_in_line_(Line, 0, Count).

count_positive_in_line_([], Accumulator, Accumulator).
count_positive_in_line_([Cell|RestCells], Accumulator, Count) :-
    (   (integer(Cell), Cell > 0 ; Cell = [SingleValue], SingleValue > 0)
    ->  NextAccumulator is Accumulator + 1
    ;   NextAccumulator = Accumulator
    ),
    count_positive_in_line_(RestCells, NextAccumulator, Count).


%% can_place_zero(+Matrix, +Row, +Col, +ZerosPerLine)
%
% Ellenőrzi, hogy nulla elhelyezhető-e az adott pozícióban.
% A sorban/oszlopban nem lehet több nulla, mint a megengedett kvóta.
%
can_place_zero(Matrix, Row, Col, ZerosPerLine) :-
    nth1(Row, Matrix, RowValues),
    count_zeros_in_line(RowValues, RowZeroCount),
    RowZeroCount < ZerosPerLine,
    get_column_values(Matrix, Col, ColValues),
    count_zeros_in_line(ColValues, ColZeroCount),
    ColZeroCount < ZerosPerLine.

% Megszámolja a nullákat egy sorban/oszlopban.
count_zeros_in_line(Line, Count) :-
    count_zeros_in_line_(Line, 0, Count).

count_zeros_in_line_([], Accumulator, Accumulator).
count_zeros_in_line_([Cell|RestCells], Accumulator, Count) :-
    (   (integer(Cell), Cell =:= 0 ; Cell = [0])
    ->  NextAccumulator is Accumulator + 1
    ;   NextAccumulator = Accumulator
    ),
    count_zeros_in_line_(RestCells, NextAccumulator, Count).


%% check_row_col_counts(+Matrix, +BoardSize, +CycleLength)
%
% Ellenőrzi, hogy a végső megoldásban minden sorban és oszlopban
% pontosan CycleLength pozitív szám szerepel.
%
check_row_col_counts(Matrix, BoardSize, CycleLength) :-
    forall(between(1, BoardSize, RowIndex), 
           (nth1(RowIndex, Matrix, Row), count_positive_in_line(Row, CycleLength))),
    forall(between(1, BoardSize, ColIndex), 
           (get_column_values(Matrix, ColIndex, Col), count_positive_in_line(Col, CycleLength))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SZŰKÍTÉSEK PROPAGÁLÁSA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% propagate_all(+PuzzleDescriptor, +SpiralPositions, +MatrixIn, -MatrixOut)
%
% Szűkítések ismételt alkalmazása fixpontig.
% Addig alkalmazza a különböző szűkítési szabályokat, amíg már nem változik a mátrix.
%
propagate_all(_PuzzleDescriptor, _, [], []) :- !.
propagate_all(PuzzleDescriptor, SpiralPositions, MatrixIn, MatrixOut) :-
    (   apply_one_restriction(PuzzleDescriptor, SpiralPositions, MatrixIn, TempMatrix)
    ->  (   TempMatrix == []
        ->  MatrixOut = []
        ;   propagate_all(PuzzleDescriptor, SpiralPositions, TempMatrix, MatrixOut)
        )
    ;   MatrixOut = MatrixIn
    ).


%% apply_one_restriction(+PuzzleDescriptor, +SpiralPositions, +MatrixIn, -MatrixOut)
%
% Egyetlen szűkítési lépés végrehajtása. Sorrendben próbálja:
% 1. Ismert értékek propagálása (ismert_szukites)
% 2. Kizárásos szűkítés (kizarasos_szukites)
% 3. Spirál-specifikus szűkítés (mxtekercs_szukites)
%
apply_one_restriction(PuzzleDescriptor, SpiralPositions, MatrixIn, MatrixOut) :-
    (   ismert_szukites(PuzzleDescriptor, MatrixIn, MatrixOut)
    ->  true
    ;   kizarasos_szukites(PuzzleDescriptor, MatrixIn, TempMatrix, _RestrictionInfo),
        (   TempMatrix == []
        ->  MatrixOut = []
        ;   TempMatrix \== MatrixIn,
            MatrixOut = TempMatrix
        )
    ;   mxtekercs_szukites(PuzzleDescriptor, SpiralPositions, MatrixIn, MatrixOut),
        MatrixOut \== MatrixIn
    ).


%% mxtekercs_szukites(+PuzzleDescriptor, +SpiralPositions, +MatrixIn, -MatrixOut)
%
% Spirál-specifikus szűkítés a mátrixon.
% Kiveszi a spirál mentén a cellaértékeket, szűkíti őket, majd visszaírja.
%
mxtekercs_szukites(PuzzleDescriptor, SpiralPositions, MatrixIn, MatrixOut) :-
    MatrixIn \== [],
    extract_spiral(MatrixIn, SpiralPositions, SpiralValues),
    (   apply_spiral_passes(PuzzleDescriptor, SpiralValues, NarrowedSpiral)
    ->  (   NarrowedSpiral == SpiralValues
        ->  fail
        ;   replace_spiral_values(MatrixIn, SpiralPositions, NarrowedSpiral, MatrixOut)
        )
    ;   MatrixOut = []
    ).


%% apply_spiral_passes(+PuzzleDescriptor, +SpiralIn, -SpiralOut)
%
% Spirál előre-hátra szűkítése fixpontig.
%
apply_spiral_passes(PuzzleDescriptor, SpiralIn, SpiralOut) :-
    spiral_fixpoint(PuzzleDescriptor, SpiralIn, SpiralOut).


%% spiral_fixpoint(+PuzzleDescriptor, +SpiralIn, -SpiralOut)
%
% Előre és visszafelé szűkítés ismétlése, amíg a spirál nem változik.
% Maximum 20 iterációig fut a végtelen ciklusok elkerülésére.
%
spiral_fixpoint(PuzzleDescriptor, SpiralIn, SpiralOut) :-
    spiral_fixpoint_iter(PuzzleDescriptor, SpiralIn, 0, SpiralOut).

spiral_fixpoint_iter(_PuzzleDescriptor, SpiralIn, IterationCount, SpiralIn) :-
    IterationCount >= 20, !.
spiral_fixpoint_iter(PuzzleDescriptor, SpiralIn, IterationCount, SpiralOut) :-
    tekercs_szukites(PuzzleDescriptor, SpiralIn, SpiralForward),
    reverse(SpiralForward, ReversedInput),
    tekercs_szukites_backward(PuzzleDescriptor, ReversedInput, ReversedNarrowed),
    reverse(ReversedNarrowed, CombinedResult),
    (   CombinedResult == SpiralIn
    ->  SpiralOut = CombinedResult
    ;   NextIteration is IterationCount + 1,
        spiral_fixpoint_iter(PuzzleDescriptor, CombinedResult, NextIteration, SpiralOut)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DOMAIN METSZÉS ÉS KEZELÉS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Spirál elemek domainjeinek páronkénti metszése.
intersect_spiral_domains([], [], []).
intersect_spiral_domains([DomainA|RestA], [DomainB|RestB], [ResultDomain|RestResult]) :-
    intersect_domains(DomainA, DomainB, ResultDomain),
    intersect_spiral_domains(RestA, RestB, RestResult).


%% intersect_domains(+DomainA, +DomainB, -ResultDomain)
%
% Két domain (egész szám vagy lista) metszete.
% Ha a metszet üres, sikertelen lesz.
%

% Két egész szám metszete: egyezniük kell.
intersect_domains(IntValueA, IntValueB, Result) :-
    integer(IntValueA),
    integer(IntValueB), !,
    (IntValueA =:= IntValueB -> Result = IntValueA ; fail).

% Egész szám vs lista: az egész szerepel-e a listában.
intersect_domains(IntValue, ListDomain, Result) :-
    integer(IntValue),
    is_list(ListDomain), !,
    (ord_memberchk(IntValue, ListDomain) -> Result = IntValue ; fail).

% Lista vs egész szám: az egész szerepel-e a listában.
intersect_domains(ListDomain, IntValue, Result) :-
    is_list(ListDomain),
    integer(IntValue), !,
    (ord_memberchk(IntValue, ListDomain) -> Result = IntValue ; fail).

% Két lista metszete: nem lehet üres.
intersect_domains(ListA, ListB, Result) :-
    is_list(ListA),
    is_list(ListB),
    ord_intersection(ListA, ListB, Intersection),
    Intersection \= [],
    Result = Intersection.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPIRÁL VISSZAFELÉ SZŰKÍTÉS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tekercs_szukites_backward(+PuzzleDescriptor, +SpiralLine, -NarrowedSpiral)
%
% Spirál visszafelé szűkítése (hátrafelé propagálás).
% A spirál végéről indulva szűkíti az egyes pozíciókat az előző pozíciók alapján.
%
tekercs_szukites_backward(szt(_, CycleLength, _), SpiralLine, NarrowedSpiral) :-
    SpiralLine \= [],
    process_spiral_backward(SpiralLine, CycleLength, [1], NarrowedSpiral), !.

% Visszafelé spirál feldolgozás vége (alapeset).
process_spiral_backward([], _, _, []).
process_spiral_backward([PositionDomain|RestPositions], CycleLength,
                        NextPositiveSetIn, [NarrowedDomain|RestNarrowed]) :-
    shrink_position_backward(PositionDomain, CycleLength, NextPositiveSetIn,
                             NarrowedDomain, NextPositiveSetOut),
    process_spiral_backward(RestPositions, CycleLength, NextPositiveSetOut, RestNarrowed).


%% shrink_position_backward(+PositionDomain, +CycleLength, +NextPositiveSetIn,
%%                          -NarrowedDomain, -NextPositiveSetOut)
%
% Egy spirálpozíció domainjének szűkítése visszafelé haladva.
% A ciklikus előd értékek alapján szűkít.
%
shrink_position_backward(PositionDomain, CycleLength, NextPositiveSetIn,
                         NarrowedDomain, NextPositiveSetOut) :-
    cyclic_predecessors(CycleLength, NextPositiveSetIn, PredecessorSet),
    ord_add_element(PredecessorSet, 0, AllowedValues),
    intersect_domain(PositionDomain, AllowedValues, NarrowedDomain, ResultSet),
    update_next_positive_set(ResultSet, NextPositiveSetIn, NextPositiveSetOut).


%% cyclic_predecessors(+CycleLength, +ValueSet, -PredecessorSet)
%
% Kiszámítja egy értékhalmaz ciklikus elődeinek halmazát.
%
cyclic_predecessors(CycleLength, ValueSet, PredecessorSet) :-
    findall(Predecessor, 
            (member(Value, ValueSet), cyclic_predecessor(CycleLength, Value, Predecessor)), 
            PredecessorList),
    list_to_ord_set(PredecessorList, PredecessorSet).

% 0 speciális eset: önmaga elődje.
cyclic_predecessor(_, 0, 0) :- !.
cyclic_predecessor(CycleLength, Value, Predecessor) :-
    (Value > 1 -> Predecessor is Value - 1 ; Predecessor is CycleLength).


%% update_next_positive_set(+ResultSet, +PreviousSet, -UpdatedSet)
%
% Frissíti a következő pozitív értékek halmazát a szűkítés eredménye alapján.
%
update_next_positive_set(ResultSet, PreviousSet, UpdatedSet) :-
    (   \+ ord_memberchk(0, ResultSet)
    ->  UpdatedSet = ResultSet
    ;   remove_zero(ResultSet, PositiveOnlySet),
        ord_union(PositiveOnlySet, PreviousSet, UpdatedSet)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPIRÁL ÉRTÉKEK KINYERÉSE ÉS VISSZAÍRÁSA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% extract_spiral(+Matrix, +SpiralPositions, -SpiralValues)
%
% Spirál pozíciókhoz tartozó cellaértékek kinyerése a mátrixból.
%
extract_spiral(Matrix, SpiralPositions, SpiralValues) :-
    maplist(cell_at(Matrix), SpiralPositions, SpiralValues).

% Egyetlen pozíció cellájának lekérése.
cell_at(Matrix, pos(Row, Col), CellValue) :-
    nth1(Row, Matrix, RowValues),
    nth1(Col, RowValues, CellValue).


%% replace_spiral_values(+MatrixIn, +SpiralPositions, +NewValues, -MatrixOut)
%
% Spirál mentén új értékek visszaírása a mátrixba.
%
replace_spiral_values(MatrixIn, SpiralPositions, NewValues, MatrixOut) :-
    replace_spiral_values_(SpiralPositions, NewValues, MatrixIn, MatrixOut).

replace_spiral_values_([], [], Matrix, Matrix).
replace_spiral_values_([pos(Row, Col)|RestPositions], [Value|RestValues], MatrixIn, MatrixOut) :-
    set_cell(MatrixIn, Row, Col, Value, TempMatrix),
    replace_spiral_values_(RestPositions, RestValues, TempMatrix, MatrixOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MÁTRIX ÁLLAPOT ELLENŐRZÉS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% solved_matrix(+Matrix)
%
% Igaz, ha a mátrix minden cellája végleges értékre szűkült.
%
solved_matrix(Matrix) :-
    maplist(is_row_finalized, Matrix).

% Igaz, ha a sor minden cellája végleges.
is_row_finalized(Row) :-
    maplist(is_cell_finalized, Row).

% Végleges cella: konkrét szám vagy egyelemű lista.
is_cell_finalized(Cell) :- integer(Cell), !.
is_cell_finalized([_]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MEGOLDÁS KONVERTÁLÁS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% matrix_to_solution(+Matrix, -Solution)
%
% Megoldott mátrix sorait egyszerű értéklistákká alakítja.
% Az egyelemű listákat kibontja, a számokat megtartja.
%
matrix_to_solution(Matrix, Solution) :-
    maplist(convert_row_to_values, Matrix, Solution).

% Sor celláinak konkretizálása.
convert_row_to_values(Row, Values) :-
    maplist(extract_cell_value, Row, Values).

% Cellából numerikus érték kinyerése.
extract_cell_value(Cell, Value) :-
    (integer(Cell) -> Value = Cell ; Cell = [Value]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPIRÁL POZÍCIÓK GENERÁLÁSA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% spiral_positions(+BoardSize, -SpiralPositions)
%
% N méretű négyzet táblához spirál sorrendű pozíciók listáját állítja elő.
% A spirál a bal felső sarokból indul és befelé tekeredik az óramutató járásával.
%
spiral_positions(BoardSize, SpiralPositions) :-
    spiral_collect(1, BoardSize, 1, BoardSize, SpiralPositions).

% Megállási feltétel: ha a határok átfednek, üres lista.
spiral_collect(TopRow, BottomRow, LeftCol, RightCol, []) :-
    (TopRow > BottomRow ; LeftCol > RightCol), !.

% Egy spirálréteg bejárása és rekurzió a belső rétegre.
spiral_collect(TopRow, BottomRow, LeftCol, RightCol, Positions) :-
    TopRow =< BottomRow,
    LeftCol =< RightCol,
    % Felső él: balról jobbra
    collect_top_edge(TopRow, LeftCol, RightCol, TopEdgePositions),
    NextTopRow is TopRow + 1,
    % Jobb él: felülről lefelé
    collect_right_edge(NextTopRow, BottomRow, RightCol, RightEdgePositions),
    % Alsó él: jobbról balra (ha van)
    (   TopRow < BottomRow
    ->  PrevRightCol is RightCol - 1,
        collect_bottom_edge(BottomRow, PrevRightCol, LeftCol, BottomEdgePositions)
    ;   BottomEdgePositions = []
    ),
    % Bal él: lentről felfelé (ha van)
    (   LeftCol < RightCol
    ->  PrevBottomRow is BottomRow - 1,
        NextTopRowForLeft is TopRow + 1,
        collect_left_edge(PrevBottomRow, NextTopRowForLeft, LeftCol, LeftEdgePositions)
    ;   LeftEdgePositions = []
    ),
    % Belső réteg rekurzív bejárása
    InnerTopRow is TopRow + 1,
    InnerBottomRow is BottomRow - 1,
    InnerLeftCol is LeftCol + 1,
    InnerRightCol is RightCol - 1,
    spiral_collect(InnerTopRow, InnerBottomRow, InnerLeftCol, InnerRightCol, InnerPositions),
    append([TopEdgePositions, RightEdgePositions, BottomEdgePositions, LeftEdgePositions, InnerPositions], Positions).

% Felső él pozícióinak gyűjtése balról jobbra.
collect_top_edge(_Row, LeftCol, RightCol, []) :- LeftCol > RightCol, !.
collect_top_edge(Row, LeftCol, RightCol, [pos(Row, LeftCol)|RestPositions]) :-
    LeftCol =< RightCol,
    NextCol is LeftCol + 1,
    collect_top_edge(Row, NextCol, RightCol, RestPositions).

% Jobb él pozícióinak gyűjtése felülről lefelé.
collect_right_edge(TopRow, BottomRow, _Col, []) :- TopRow > BottomRow, !.
collect_right_edge(TopRow, BottomRow, Col, [pos(TopRow, Col)|RestPositions]) :-
    TopRow =< BottomRow,
    NextRow is TopRow + 1,
    collect_right_edge(NextRow, BottomRow, Col, RestPositions).

% Alsó él pozícióinak gyűjtése jobbról balra.
collect_bottom_edge(_Row, StartCol, EndCol, []) :- StartCol < EndCol, !.
collect_bottom_edge(Row, StartCol, EndCol, [pos(Row, StartCol)|RestPositions]) :-
    StartCol >= EndCol,
    NextCol is StartCol - 1,
    collect_bottom_edge(Row, NextCol, EndCol, RestPositions).

% Bal él pozícióinak gyűjtése lentről felfelé.
collect_left_edge(StartRow, EndRow, _Col, []) :- StartRow < EndRow, !.
collect_left_edge(StartRow, EndRow, Col, [pos(StartRow, Col)|RestPositions]) :-
    StartRow >= EndRow,
    NextRow is StartRow - 1,
    collect_left_edge(NextRow, EndRow, Col, RestPositions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% KEZDŐ TÁBLA ÉS ISMERT SZŰKÍTÉS (KHF5 alapján)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% kezdotabla(+PuzzleDescriptor, -Matrix)
%
% Létrehozza a kezdő mátrixot a feladvány alapján.
% PuzzleDescriptor = szt(BoardSize, CycleLength, GivenElements)
% A mátrix minden cellája egy domain listát tartalmaz:
%   - ha adott: [GivenValue]
%   - különben: [0..CycleLength] ha BoardSize > CycleLength; vagy [1..CycleLength] ha egyenlőek
%
kezdotabla(szt(BoardSize, CycleLength, GivenElements), Matrix) :-
    compute_base_domain(BoardSize, CycleLength, BaseDomain),
    create_empty_matrix(BoardSize, BoardSize, BaseDomain, EmptyMatrix),
    apply_given_elements(GivenElements, EmptyMatrix, Matrix).


%% ismert_szukites(+PuzzleDescriptor, +MatrixIn, -MatrixOut)
%
% Ismert (egyelemű lista) értékekből induló sor/oszlop-alapú szűkítések
% ismétlése mindaddig, amíg létezik egyelemű lista. Ha nem történik
% szűkítés, az eljárás meghiúsul. Ha ellentmondás adódik, MatrixOut = [].
%
ismert_szukites(szt(BoardSize, CycleLength, _), MatrixIn, MatrixOut) :-
    compute_zero_quota(BoardSize, CycleLength, ZeroQuota),
    propagate_until_fixpoint(MatrixIn, BoardSize, CycleLength, ZeroQuota, ResultMatrix, DidChange),
    (   ResultMatrix == []
    ->  MatrixOut = []
    ;   DidChange == true
    ->  MatrixOut = ResultMatrix
    ;   fail  % nem volt egyelemű tartomány, vagy nem történt szűkítés
    ).


%% compute_base_domain(+BoardSize, +CycleLength, -BaseDomain)
%
% Kiszámítja az alapértelmezett domain-t.
% Ha BoardSize > CycleLength: [0, 1, ..., CycleLength]
% Ha BoardSize = CycleLength: [1, ..., CycleLength]
%
compute_base_domain(BoardSize, CycleLength, BaseDomain) :-
    (   BoardSize > CycleLength
    ->  generate_range_list(0, CycleLength, BaseDomain)
    ;   BoardSize =:= CycleLength
    ->  generate_range_list(1, CycleLength, BaseDomain)
    ).

% Tartomány lista generálása [Low..High].
generate_range_list(Low, High, RangeList) :-
    Low =< High,
    generate_range_list_helper(Low, High, RangeList).

generate_range_list_helper(Current, High, [Current|Rest]) :-
    Current < High, !,
    Next is Current + 1,
    generate_range_list_helper(Next, High, Rest).
generate_range_list_helper(High, High, [High]).


%% create_empty_matrix(+RowCount, +ColCount, +DefaultValue, -Matrix)
%
% Létrehoz egy RowCount x ColCount méretű mátrixot, minden cellában DefaultValue értékkel.
%
create_empty_matrix(0, _ColCount, _DefaultValue, []).
create_empty_matrix(RowCount, ColCount, DefaultValue, [Row|RestRows]) :-
    RowCount > 0,
    create_matrix_row(ColCount, DefaultValue, Row),
    NextRowCount is RowCount - 1,
    create_empty_matrix(NextRowCount, ColCount, DefaultValue, RestRows).

create_matrix_row(0, _DefaultValue, []).
create_matrix_row(ColCount, DefaultValue, [DefaultValue|RestCells]) :-
    ColCount > 0,
    NextColCount is ColCount - 1,
    create_matrix_row(NextColCount, DefaultValue, RestCells).


%% apply_given_elements(+GivenElements, +MatrixIn, -MatrixOut)
%
% Beírja az adott elemeket a mátrixba egyelemű listaként.
%
apply_given_elements([], Matrix, Matrix).
apply_given_elements([i(Row, Col, Value)|RestElements], MatrixIn, MatrixOut) :-
    set_cell(MatrixIn, Row, Col, [Value], TempMatrix),
    apply_given_elements(RestElements, TempMatrix, MatrixOut).


%% compute_zero_quota(+BoardSize, +CycleLength, -ZeroQuota)
%
% Kiszámítja, hogy hány nulla lehet egy sorban/oszlopban.
% ZeroQuota = BoardSize - CycleLength
%
compute_zero_quota(BoardSize, CycleLength, ZeroQuota) :- 
    ZeroQuota is BoardSize - CycleLength.


%% propagate_until_fixpoint(+MatrixIn, +BoardSize, +CycleLength, +ZeroQuota, 
%%                          -MatrixOut, -DidChange)
%
% Iteratív szűkítés: kiválaszt egy egyelemű listát és propagálja.
% Ha nincs több egyelemű lista, visszaadja a mátrixot.
%
propagate_until_fixpoint(MatrixIn, BoardSize, CycleLength, ZeroQuota, MatrixOut, DidChange) :-
    find_singleton_cell(MatrixIn, Row, Col, Value), !,
    (   Value > 0
    ->  propagate_fixed_positive(MatrixIn, Row, Col, Value, TempMatrix)
    ;   propagate_fixed_zero(MatrixIn, BoardSize, CycleLength, ZeroQuota, Row, Col, TempMatrix)
    ),
    (   TempMatrix == []
    ->  MatrixOut = [], DidChange = true
    ;   propagate_until_fixpoint(TempMatrix, BoardSize, CycleLength, ZeroQuota, MatrixOut, _),
        DidChange = true
    ).
propagate_until_fixpoint(Matrix, _BoardSize, _CycleLength, _ZeroQuota, Matrix, false).


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
