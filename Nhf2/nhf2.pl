% ------------------------------------------------------------
% Nhf2 – Számtekercs
%
% @author "Toronyi Zsombor <toronyizsombor@edu.bme.hu> [S8F7DV]"
% @date   "2025-11-20" 
% ------------------------------------------------------------



:- use_module(library(lists)).



szamtekercs(szt(N, M, Givens), Matrix) :-
    create_matrix(N, Matrix),
    set_givens(Givens, Matrix),
    % szamtekercs(+Spec, -Matrix) : Builds the spiral matrix from specification szt(N,M,Givens).
    numlist(1, M, Numbers),
    make_needs_list(N, Numbers, RowNeeds),
    make_needs_list(N, Numbers, ColNeeds),
    repeat_value(N, N, RowSpaces),
    repeat_value(N, N, ColSpaces),
    spiral_path(N, Path),
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


copy_list(List, Copy) :-
    append(List, [], Copy).


% repeat_value(+Count,+Value,-Lista): Value ismétlése Count-szor
repeat_value(0, _, []).
repeat_value(Count, Value, [Value|Rest]) :-
    Count > 0,
    Count1 is Count - 1,
    repeat_value(Count1, Value, Rest).


% spiral_path(+N, -Path) : Generates spiral traversal positions for an N x N grid.
spiral_path(N, Path) :-
    spiral(1, N, 1, N, Path).


% spiral(+Top,+Bottom,+Left,+Right,-Path) : Recursive spiral boundary collection.
spiral(Top, Bottom, Left, Right, []) :-
    Top > Bottom ; Left > Right.
spiral(Top, Bottom, Left, Right, Path) :-
    Top =< Bottom,
    Left =< Right,
    top_edge(Top, Left, Right, TopCells),
    T1 is Top + 1,
    right_edge(T1, Bottom, Right, RightCells),
    (   Top < Bottom
    ->  BStart is Right - 1,
        bottom_edge(Bottom, BStart, Left, BottomCells)
    ;   BottomCells = []
    ),
    (   Left < Right
    ->  LStart is Bottom - 1,
        LEnd is Top + 1,
        left_edge(LStart, LEnd, Left, LeftCells)
    ;   LeftCells = []
    ),
    NextTop is Top + 1,
    NextBottom is Bottom - 1,
    NextLeft is Left + 1,
    NextRight is Right - 1,
    spiral(NextTop, NextBottom, NextLeft, NextRight, InnerCells),
    append(TopCells, RightCells, Tmp1),
    append(Tmp1, BottomCells, Tmp2),
    append(Tmp2, LeftCells, Tmp3),
    append(Tmp3, InnerCells, Path).


% top_edge(+Row,+Left,+Right,-Cells) : Collects top edge row cells.
top_edge(_, Left, Right, []) :-
    Left > Right.
top_edge(Row, Left, Right, [pos(Row, Left)|Rest]) :-
    Left =< Right,
    Next is Left + 1,
    top_edge(Row, Next, Right, Rest).


% right_edge(+Top,+Bottom,+Col,-Cells) : Collects right edge column cells.
right_edge(Top, Bottom, _, []) :-
    Top > Bottom.
right_edge(Top, Bottom, Col, [pos(Top, Col)|Rest]) :-
    Top =< Bottom,
    Next is Top + 1,
    right_edge(Next, Bottom, Col, Rest).


% bottom_edge(+Row,+Start,+End,-Cells) : Collects bottom edge row cells descending.
bottom_edge(_, Start, End, []) :-
    Start < End.
bottom_edge(Row, Start, End, [pos(Row, Start)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    bottom_edge(Row, Next, End, Rest).


% left_edge(+Start,+End,+Col,-Cells) : Collects left edge column cells ascending.
left_edge(Start, End, _, []) :-
    Start < End.
left_edge(Start, End, Col, [pos(Start, Col)|Rest]) :-
    Start >= End,
    Next is Start - 1,
    left_edge(Next, End, Col, Rest).


% fill_path(+Positions,+Matrix,+RowNeeds,+ColNeeds,+RowSpaces,+ColSpaces,+Next,+M)
% Fills matrix following Path, placing numbers or zeros respecting constraints.
fill_path([], _, RowNeeds, ColNeeds, _, _, _, _) :-
    all_empty(RowNeeds),
    all_empty(ColNeeds),
    !.

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
    process_cell(Cell, R, C, Matrix, Rest,
                 RowNeeds, ColNeeds,
                 RowSpaceBefore, RowSpaceAfter,
                 ColSpaceBefore, ColSpaceAfter,
                 Next, M,
                 RowNeedsNext, ColNeedsNext, NextValue),
    fill_path(Rest, Matrix, RowNeedsNext, ColNeedsNext, RowSpacesUpdated, ColSpacesUpdated, NextValue, M).


% process_cell(+Cell,+R,+C,+Matrix,+Rest,...,+Next,+M,-RowNeedsNext,-ColNeedsNext,-NextOut)
% Handles logic based on whether cell is variable or filled.
process_cell(Cell, R, C, Matrix, Rest,
             RowNeeds, ColNeeds,
             RowSpaceBefore, RowSpaceAfter,

             ColSpaceBefore, ColSpaceAfter,
             Next, M,
             RowNeedsNext, ColNeedsNext, NextOut) :-
    expected_value(Next, M, Expected),
    nth1(R, RowNeeds, RowNeedList),
    length(RowNeedList, RowNeedLen),
    nth1(C, ColNeeds, ColNeedList),
    length(ColNeedList, ColNeedLen),
    (   var(Cell)
    ->  process_var_cell(Cell, Expected, R, C, Matrix, Rest,
                         RowNeeds, ColNeeds,
                         RowNeedList, RowNeedLen,
                         ColNeedList, ColNeedLen,
                         RowSpaceBefore, RowSpaceAfter,
                         ColSpaceBefore, ColSpaceAfter,
                         Next,
                         RowNeedsNext, ColNeedsNext, NextOut)
    ;   process_filled_cell(Cell, Expected, R, C, Matrix, Rest,
                            RowNeeds, ColNeeds,
                            RowNeedList, RowNeedLen,
                            ColNeedList, ColNeedLen,
                            RowSpaceBefore, RowSpaceAfter,
                            ColSpaceBefore, ColSpaceAfter,
                            Next,
                            RowNeedsNext, ColNeedsNext, NextOut)
    ).


% process_var_cell(+Cell,+Expected,...) : Decides placing Expected or zero into a variable cell.
process_var_cell(Cell, Expected, R, C, Matrix, Rest,
                 RowNeeds, ColNeeds,
                 RowNeedList, RowNeedLen,

                 ColNeedList, ColNeedLen,
                 RowSpaceBefore, RowSpaceAfter,
                 ColSpaceBefore, ColSpaceAfter,
                 Next,
                 RowNeedsNext, ColNeedsNext, NextOut) :-
    (   needs_value(RowNeedList, Expected),
        needs_value(ColNeedList, Expected)
    ->  (   must_place_here(RowNeedLen, RowSpaceBefore,
                            ColNeedLen, ColSpaceBefore,
                            Matrix, Rest, RowNeeds, ColNeeds, Expected)
        ->  place_expected(Cell, Expected, R, C, RowNeeds, ColNeeds,
                           RowSpaceAfter, ColSpaceAfter,
                           Next,
                           RowNeedsNext, ColNeedsNext, NextOut)
        ;   (   place_expected(Cell, Expected, R, C, RowNeeds, ColNeeds,
                               RowSpaceAfter, ColSpaceAfter,
                               Next,
                               RowNeedsNext, ColNeedsNext, NextOut)
            ;   choose_zero(Cell, Matrix, Rest, RowNeeds, ColNeeds,
                            R, C,
                            RowSpaceAfter, ColSpaceAfter,
                            Expected, Next,
                            RowNeedsNext, ColNeedsNext, NextOut)
            )
        )
    ;   choose_zero(Cell, Matrix, Rest, RowNeeds, ColNeeds,
                    R, C,
                    RowSpaceAfter, ColSpaceAfter,
                    Expected, Next,
                    RowNeedsNext, ColNeedsNext, NextOut)
    ).


% process_filled_cell(+Cell,+Expected,...) : Processes a pre-filled cell (0 or number).
process_filled_cell(Cell, Expected, R, C, Matrix, Rest,
                    RowNeeds, ColNeeds,
                    _RowNeedList, _RowNeedLen,
                    _ColNeedList, _ColNeedLen,
                    _RowSpaceBefore, RowSpaceAfter,
                    _ColSpaceBefore, ColSpaceAfter,
                    Next,
                    RowNeedsNext, ColNeedsNext, NextOut) :-
    (   Cell = 0
    ->  choose_zero_fixed(Matrix, Rest, RowNeeds, ColNeeds,
                          R, C,
                          RowSpaceAfter, ColSpaceAfter,
                          Expected, Next,
                          RowNeedsNext, ColNeedsNext, NextOut)
    ;   place_expected(Cell, Expected, R, C, RowNeeds, ColNeeds,
                       RowSpaceAfter, ColSpaceAfter,
                       Next,
                       RowNeedsNext, ColNeedsNext, NextOut)
    ).


% forced_number(+RowNeedLen,+RowSpaceBefore,+ColNeedLen,+ColSpaceBefore) : True if value must be placed.
forced_number(RowNeedLen, RowSpaceBefore, ColNeedLen, ColSpaceBefore) :-
    RowNeedLen =:= RowSpaceBefore ;
    ColNeedLen =:= ColSpaceBefore.
    

% place_expected(+Cell,+Expected,...) : Places Expected and updates needs lists and next counter.
place_expected(Cell, Expected, R, C, RowNeeds, ColNeeds,
               RowSpaceAfter, ColSpaceAfter,
               Next,
               RowNeedsNext, ColNeedsNext, NextOut) :-
    Cell = Expected,
    remove_need(RowNeeds, R, Expected, RowNeedsMid),
    ensure_capacity(RowNeedsMid, R, RowSpaceAfter),
    remove_need(ColNeeds, C, Expected, ColNeedsMid),
    ensure_capacity(ColNeedsMid, C, ColSpaceAfter),
    NextOut is Next + 1,
    RowNeedsNext = RowNeedsMid,
    ColNeedsNext = ColNeedsMid.


% remove_need(+Needs,+Index,+Value,-Updated) : Removes Value from needs list at Index.
remove_need(Needs, Index, Value, UpdatedNeeds) :-
    nth1(Index, Needs, NeedList),
    select(Value, NeedList, NewNeedList),
    set_nth(Needs, Index, NewNeedList, UpdatedNeeds).


% ensure_capacity(+Needs,+Index,+RemainingCells) : Validates capacity vs remaining cells.
ensure_capacity(Needs, Index, RemainingCells) :-
    nth1(Index, Needs, NeedList),
    length(NeedList, Len),
    Len =< RemainingCells.


% needs_value(+List,+Value) : True if Value is needed in List.
needs_value(List, Value) :-
    memberchk(Value, List).


% value_still_needed(+RowNeeds,+Value) : True if Value appears in any row needs.
value_still_needed(RowNeeds, Value) :-
    member(RowNeedList, RowNeeds),
    memberchk(Value, RowNeedList),
    !.


% has_future_slot(+Matrix,+RemainingPositions,+RowNeeds,+ColNeeds,+Value) : Checks if Value can still be placed later.
has_future_slot(_, [], _, _, _) :-
    fail.
has_future_slot(Matrix, [pos(R, C)|Rest], RowNeeds, ColNeeds, Value) :-
    needs_value_in_row(RowNeeds, R, Value),
    needs_value_in_col(ColNeeds, C, Value),
    cell_accepts_value(Matrix, R, C, Value)
    ->  true
    ;   has_future_slot(Matrix, Rest, RowNeeds, ColNeeds, Value).


% needs_value_in_row(+RowNeeds,+R,+Value) : True if row R needs Value.
needs_value_in_row(RowNeeds, R, Value) :-
    nth1(R, RowNeeds, RowNeedList),
    memberchk(Value, RowNeedList).


% needs_value_in_col(+ColNeeds,+C,+Value) : True if column C needs Value.
needs_value_in_col(ColNeeds, C, Value) :-
    nth1(C, ColNeeds, ColNeedList),
    memberchk(Value, ColNeedList).


% cell_accepts_value(+Matrix,+R,+C,+Value) : True if position can take Value.
cell_accepts_value(Matrix, R, C, Value) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, Cell),
    (   var(Cell)
    ->  true
    ;   Cell = Value
    ).


% allow_zero(+RowNeeds,+ColNeeds,+Matrix,+Rest,+Expected) : Determines if zero allowed now.
allow_zero(RowNeeds, ColNeeds, Matrix, Rest, Expected) :-
    (   value_still_needed(RowNeeds, Expected)
    ->  has_future_slot(Matrix, Rest, RowNeeds, ColNeeds, Expected)
    ;   true
    ).


% choose_zero(+Cell,...) : Assigns zero to a variable cell if permissible.
choose_zero(Cell, Matrix, Rest, RowNeeds, ColNeeds,
            R, C,
            RowSpaceAfter, ColSpaceAfter,
            Expected, Next,
            RowNeedsOut, ColNeedsOut, NextOut) :-
    allow_zero(RowNeeds, ColNeeds, Matrix, Rest, Expected),
    Cell = 0,
    ensure_capacity(RowNeeds, R, RowSpaceAfter),
    ensure_capacity(ColNeeds, C, ColSpaceAfter),
    RowNeedsOut = RowNeeds,
    ColNeedsOut = ColNeeds,
    NextOut = Next.


% choose_zero_fixed(+Matrix,...) : Keeps a fixed zero cell; updates counters only.
choose_zero_fixed(Matrix, Rest, RowNeeds, ColNeeds,
                  R, C,
                  RowSpaceAfter, ColSpaceAfter,
                  Expected, Next,
                  RowNeedsOut, ColNeedsOut, NextOut) :-
    allow_zero(RowNeeds, ColNeeds, Matrix, Rest, Expected),
    ensure_capacity(RowNeeds, R, RowSpaceAfter),
    ensure_capacity(ColNeeds, C, ColSpaceAfter),
    RowNeedsOut = RowNeeds,
    ColNeedsOut = ColNeeds,
    NextOut = Next.


% must_place_here(+RowNeedLen,+RowSpaceBefore,+ColNeedLen,+ColSpaceBefore,+Matrix,+Rest,+RowNeeds,+ColNeeds,+Expected)
% True if Expected must be placed at current cell.
must_place_here(RowNeedLen, RowSpaceBefore, ColNeedLen, ColSpaceBefore,
                Matrix, Rest, RowNeeds, ColNeeds, Expected) :-
    forced_number(RowNeedLen, RowSpaceBefore, ColNeedLen, ColSpaceBefore)
    ;   (   value_still_needed(RowNeeds, Expected),
            \+ has_future_slot(Matrix, Rest, RowNeeds, ColNeeds, Expected)
        ).


% set_nth(+List,+Index,+Elem,-Updated) : Replaces element at Index with Elem.
set_nth([_|Rest], 1, Elem, [Elem|Rest]).
set_nth([H|Rest], Index, Elem, [H|UpdatedRest]) :-
    Index > 1,
    Index1 is Index - 1,
    set_nth(Rest, Index1, Elem, UpdatedRest).


% expected_value(+Next,+M,-Expected) : Computes next expected value cycling 1..M.
expected_value(Next, M, Expected) :-
    Rem is Next mod M,
    Expected is Rem + 1.


% all_empty(+NeedsLists) : True if all inner lists are empty.
all_empty([]).
all_empty([[]|Rest]) :-
    all_empty(Rest).