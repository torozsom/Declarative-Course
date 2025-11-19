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
