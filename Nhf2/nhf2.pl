% ------------------------------------------------------------
% Nhf2 – Számtekercs
%
% @author "Toronyi Zsombor <toronyizsombor@edu.bme.hu> [S8F7DV]"
% @date   "2025-11-20" 
% ------------------------------------------------------------



:- use_module(library(lists)).



% szamtekercs(+FeladvanyLeiro, -Matrix): kitölti a mátrixot a spirál sorrend feltételeivel
szamtekercs(szt(N, M, Givens), Matrix) :-
    create_matrix(N, Matrix),                      % létrehozzuk az N x N üres mátrixot
    set_givens(Givens, Matrix),                    % megadott értékek beállítása
    numlist(1, M, Numbers),                        % 1..M lista
    make_needs_list(N, Numbers, RowNeeds),         % soronként szükséges értékek
    make_needs_list(N, Numbers, ColNeeds),         % oszloponként szükséges értékek
    repeat_value(N, N, RowSpaces),                 % soronként hátralévő helyek száma
    repeat_value(N, N, ColSpaces),                 % oszloponként hátralévő helyek száma
    spiral_path(N, Path),                          % spirál bejárási útvonal
    fill_path(Path, Matrix, RowNeeds, ColNeeds, RowSpaces, ColSpaces, 0, M).



% create_matrix(+N, -Matrix): N x N méretű mátrix létrehozása (szabad változók)
create_matrix(N, Matrix) :-
    length(Matrix, N),
    maplist(length_n(N), Matrix).


% length_n(+N, -List): N hosszú lista létrehozása
length_n(N, List) :-
    length(List, N).


% set_givens(+AdottLista, +Matrix): megadott i(R,C,V) értékek beírása
set_givens([], _).
set_givens([i(R, C, V)|Rest], Matrix) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell),
    Cell = V,
    set_givens(Rest, Matrix).


% make_needs_list(+Darab, +Numbers, -ListaLista): Darab darab Numbers másolat
make_needs_list(0, _, []).
make_needs_list(N, Numbers, [Copy|Rest]) :-
    N > 0,
    N1 is N - 1,
    copy_list(Numbers, Copy),
    make_needs_list(N1, Numbers, Rest).


% copy_list(+List, -Copy): lista másolása
copy_list(List, Copy) :- append(List, [], Copy).


% repeat_value(+Count,+Value,-Lista): Value ismétlése Count-szor
repeat_value(0, _, []).
repeat_value(Count, Value, [Value|Rest]) :-
    Count > 0,
    Count1 is Count - 1,
    repeat_value(Count1, Value, Rest).


% spiral_path(+N,-Path): spirál bejárási lista (pos/2) generálása
spiral_path(N, Path) :- spiral(1, N, 1, N, Path).


% spiral(+Top,+Bottom,+Left,+Right,-Path): rekurzív keret szűkítéssel
spiral(Top, Bottom, Left, Right, []) :- Top > Bottom ; Left > Right.
spiral(Top, Bottom, Left, Right, Path) :-
    Top =< Bottom,
    Left =< Right,
    top_edge(Top, Left, Right, TopCells),
    T1 is Top + 1,
    right_edge(T1, Bottom, Right, RightCells),
    ( Top < Bottom -> BStart is Right - 1, bottom_edge(Bottom, BStart, Left, BottomCells) ; BottomCells = [] ),
    ( Left < Right -> LStart is Bottom - 1, LEnd is Top + 1, left_edge(LStart, LEnd, Left, LeftCells) ; LeftCells = [] ),
    NextTop   is Top + 1,
    NextBottom is Bottom - 1,
    NextLeft  is Left + 1,
    NextRight is Right - 1,
    spiral(NextTop, NextBottom, NextLeft, NextRight, InnerCells),
    append(TopCells, RightCells, T1Cells),
    append(T1Cells, BottomCells, T2Cells),
    append(T2Cells, LeftCells, T3Cells),
    append(T3Cells, InnerCells, Path).


% top_edge(+Row,+Left,+Right,-Cells): felső él pozíciói
top_edge(_, Left, Right, []) :- Left > Right.
top_edge(Row, Left, Right, [pos(Row, Left)|Rest]) :-
    Left =< Right,
    Next is Left + 1,
    top_edge(Row, Next, Right, Rest).


% right_edge(+Top,+Bottom,+Col,-Cells): jobb él pozíciói
right_edge(Top, Bottom, _, []) :- Top > Bottom.
right_edge(Top, Bottom, Col, [pos(Top, Col)|Rest]) :-
    Top =< Bottom,
    Next is Top + 1,
    right_edge(Next, Bottom, Col, Rest).


% bottom_edge(+Row,+Start,+End,-Cells): alsó él pozíciói
bottom_edge(_, Start, End, []) :- Start < End.
bottom_edge(Row, Start, End, [pos(Row, Start)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    bottom_edge(Row, Next, End, Rest).


% left_edge(+Start,+End,+Col,-Cells): bal él pozíciói
left_edge(Start, End, _, []) :- Start < End.
left_edge(Start, End, Col, [pos(Start, Col)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    left_edge(Next, End, Col, Rest).


% fill_path(+Path,+Matrix,+RowNeeds,+ColNeeds,+RowSpaces,+ColSpaces,+Next,+M): spirál
fill_path([], _, RowNeeds, ColNeeds, _, _, _, _) :-
    all_empty(RowNeeds),
    all_empty(ColNeeds), !.

fill_path([pos(R, C)|Rest], Matrix, RowNeeds, ColNeeds, RowSpaces, ColSpaces, Next, M) :-
    nth1(R, RowSpaces, RowSpaceBefore),
    RowSpaceAfter is RowSpaceBefore - 1,
    RowSpaceAfter >= 0,
    set_nth(RowSpaces, R, RowSpaceAfter, RowSpacesUpdated),
    nth1(C, ColSpaces, ColSpaceBefore),
    ColSpaceAfter is ColSpaceBefore - 1,
    ColSpaceAfter >= 0,
    set_nth(ColSpaces, C, ColSpaceAfter, ColSpacesUpdated),
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell),
    process_cell(Cell, R, C, RowNeeds, ColNeeds,
                 RowSpaceBefore, RowSpaceAfter,
                 ColSpaceBefore, ColSpaceAfter,
                 Next, M,
                 RowNeedsNext, ColNeedsNext, NextValue),
    fill_path(Rest, Matrix, RowNeedsNext, ColNeedsNext, RowSpacesUpdated, ColSpacesUpdated, NextValue, M).


% process_cell(+Cell,+R,+C, ...): aktuális cella kezelése (kényszer / várható / 0)
process_cell(Cell, R, C, RowNeeds, ColNeeds,
             RowSpaceBefore, RowSpaceAfter,
             ColSpaceBefore, ColSpaceAfter,
             Next, M,
             RowNeedsNext, ColNeedsNext, NextOut) :-
    nth1(R, RowNeeds, RowNeedList), length(RowNeedList, RowNeedLen),
    nth1(C, ColNeeds, ColNeedList), length(ColNeedList, ColNeedLen),
    ( var(Cell) ->
        ( forced_number(RowNeedLen, RowSpaceBefore, ColNeedLen, ColSpaceBefore) ->
              place_expected(Cell, R, C, RowNeeds, ColNeeds,
                             RowSpaceAfter, ColSpaceAfter,
                             Next, M,
                             RowNeedsNext, ColNeedsNext, NextOut)
        ;     place_expected(Cell, R, C, RowNeeds, ColNeeds,
                             RowSpaceAfter, ColSpaceAfter,
                             Next, M,
                             RowNeedsMid, ColNeedsMid, Next1),
              RowNeedsNext = RowNeedsMid, ColNeedsNext = ColNeedsMid, NextOut = Next1
        ;     Cell = 0,
              ensure_capacity(RowNeeds, R, RowSpaceAfter),
              ensure_capacity(ColNeeds, C, ColSpaceAfter),
              RowNeedsNext = RowNeeds, ColNeedsNext = ColNeeds, NextOut = Next
        )
    ; Cell = 0 ->
        ensure_capacity(RowNeeds, R, RowSpaceAfter),
        ensure_capacity(ColNeeds, C, ColSpaceAfter),
        \+ forced_number(RowNeedLen, RowSpaceBefore, ColNeedLen, ColSpaceBefore),
        RowNeedsNext = RowNeeds, ColNeedsNext = ColNeeds, NextOut = Next
    ; place_expected(Cell, R, C, RowNeeds, ColNeeds,
                     RowSpaceAfter, ColSpaceAfter,
                     Next, M,
                     RowNeedsNext, ColNeedsNext, NextOut)
    ).


% forced_number(+SorNeedLen,+SorSpaceBefore,+OszNeedLen,+OszSpaceBefore): kényszer helm
forced_number(RowNeedLen, RowSpaceBefore, ColNeedLen, ColSpaceBefore) :-
    RowNeedLen =:= RowSpaceBefore ;
    ColNeedLen =:= ColSpaceBefore.


% place_expected(+Cell,...,+Next,+M,...,-NextOut): várt érték elhelyezése
place_expected(Cell, R, C, RowNeeds, ColNeeds,
               RowSpaceAfter, ColSpaceAfter,
               Next, M,
               RowNeedsNext, ColNeedsNext, NextOut) :-
    expected_value(Next, M, Expected),
    Cell = Expected,
    remove_need(RowNeeds, R, Expected, RowNeedsMid),
    ensure_capacity(RowNeedsMid, R, RowSpaceAfter),
    remove_need(ColNeeds, C, Expected, ColNeedsMid),
    ensure_capacity(ColNeedsMid, C, ColSpaceAfter),
    NextOut is Next + 1,
    RowNeedsNext = RowNeedsMid,
    ColNeedsNext = ColNeedsMid.


% remove_need(+Needs,+Index,+Value,-UpdatedNeeds): eltávolítja az értéket a szükség listából
remove_need(Needs, Index, Value, UpdatedNeeds) :-
    nth1(Index, Needs, NeedList),
    select(Value, NeedList, NewNeedList),
    set_nth(Needs, Index, NewNeedList, UpdatedNeeds).


% ensure_capacity(+Needs,+Index,+RemainingCells): még befér-e a maradék szükség
ensure_capacity(Needs, Index, RemainingCells) :-
    nth1(Index, Needs, NeedList),
    length(NeedList, Len),
    Len =< RemainingCells.


% set_nth(+Lista,+Index,+Elem,-UjLista): Index-edik elem cseréje
set_nth([_|Rest], 1, Elem, [Elem|Rest]).
set_nth([H|Rest], Index, Elem, [H|UpdatedRest]) :-
    Index > 1,
    Index1 is Index - 1,
    set_nth(Rest, Index1, Elem, UpdatedRest).


% expected_value(+Next,+M,-Expected): sorozat következő száma 1..M körkörösen
expected_value(Next, M, Expected) :-
    Rem is Next mod M,
    Expected is Rem + 1.


% all_empty(+ListaLista): minden belső lista üres-e
all_empty([]).
all_empty([[]|Rest]) :- all_empty(Rest).