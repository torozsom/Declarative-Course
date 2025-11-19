% run_tesztek.pl -- tesztfuttató a khf7 feladathoz

:- initialization(main).

main :-
    consult('khf7.pl'),
    consult('tesztek.txt'),
    findall(A-E-K, hibas_teszteset(A,E,K), Hibak),
    (   Hibak = [] ->
        format('OK: minden teszt sikeres.~n'),
        halt(0)
    ;   format('Hibás tesztek:~n'),
        forall(member(A-E-K, Hibak), (
            format('Teszt ~w hibás. Elvárt=~q Kapott=~q~n', [A,E,K])
        )),
        halt(1)
    ).

