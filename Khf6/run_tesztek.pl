% ------------------------------------------------------------
% Khf6 – Teszt futtató
% Betölti a megoldást és a teszteket, majd lefuttatja mindet,
% rövid összegzést ír ki, és 0/1 kóddal lép ki.
% ------------------------------------------------------------

:- initialization(run_all_tests, main).

:- ensure_loaded('khf6.pl').
:- ensure_loaded('tesztek.txt').

run_all_tests :-
	findall(A, teszteset(A, _, _), Ids0),
	sort(Ids0, Ids),
	run_ids(Ids, 0, Fails),
	length(Ids, Total),
	Passed is Total - Fails,
	format('Summary: ~d passed, ~d failed, ~d total.~n', [Passed, Fails, Total]),
	( Fails =:= 0 -> halt(0) ; halt(1) ).

run_ids([], F, F).
run_ids([Id|Rest], AccFail, Fails) :-
	( hibas_teszteset(Id, Exp, Got) ->
		format('FAIL ~w~n  Expected: ~q~n  Got     : ~q~n', [Id, Exp, Got]),
		Acc1 is AccFail + 1
	;   format('ok   ~w~n', [Id]),
		Acc1 = AccFail
	),
	run_ids(Rest, Acc1, Fails).

