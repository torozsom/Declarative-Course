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
