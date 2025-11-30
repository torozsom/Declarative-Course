% Nhf2 – tesztfuttató a szamtekercs/2 predikátumhoz

:- ['nhf2.pl'].
:- ['tesztek.txt'].


run_all_tests :-
	findall(I-FL-Expected, teszteset(I, FL, Expected), Tests),
	run_tests(Tests).


run_tests([]) :-
	writeln('osszes teszt lefutott.'), !.
run_tests([I-FL-Expected|Rest]) :-
	writeln('Teszt '), write(I), writeln(' futtatasa...'),
	findall(Mx, tekercs(FL, Mx), Solutions),
	(   same_solutions(Solutions, Expected)
	->  writeln('  OK')
	;   writeln('  HIBA!'),
	    writeln('  Kapott megoldasok:'), writeln(Solutions),
	    writeln('  Varhato megoldasok:'), writeln(Expected)
	),
	run_tests(Rest).


same_solutions(S1, S2) :-
	sort(S1, T1),
	sort(S2, T2),
	T1 == T2.


:- initialization(run_all_tests).
