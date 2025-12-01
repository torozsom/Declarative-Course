# Számtekercs Feladvány - Dokumentáció (NHF2)

## Tartalomjegyzék

1. [A Feladat Értelmezése](#1-a-feladat-értelmezése)
2. [Részfeladatokra Bontás](#2-részfeladatokra-bontás)
3. [A Megoldás Helyességének Magyarázata](#3-a-megoldás-helyességének-magyarázata)
4. [Implementációs és Optimalizációs Döntések](#4-implementációs-és-optimalizációs-döntések)
5. [Prolog Nyelvi Elemek Elméleti Háttere](#5-prolog-nyelvi-elemek-elméleti-háttere)

---

## 1. A Feladat Értelmezése

### A Számtekercs Feladvány Leírása

A számtekercs feladvány egy **n×n méretű négyzet alakú táblán** játszódik, amelyen **1 és m közötti számokat** kell elhelyezni a következő szabályok szerint:

1. **Sor-oszlop kényszer**: Minden sorban és minden oszlopban az 1..m számok mindegyike **pontosan egyszer** szerepel.

2. **Spirál kényszer**: A bal felső sarokból induló, befelé tekeredő spirál vonal mentén a számok rendre az **1, 2, ..., m, 1, 2, ..., m, ...** ciklikus sorrendben követik egymást.

### A Spirál Vonal Definíciója

A tekeredő vonal bejárási sorrendje:
1. Első sor: balról jobbra
2. Utolsó oszlop: felülről lefelé
3. Utolsó sor: jobbról balra
4. Első oszlop: alulról felfelé (a 2. sor 1. mezőjéig)
5. Ezután rekurzívan folytatódik a belső (n-2)×(n-2) négyzettel

### Bemenet és Kimenet Formátuma

**Bemenet** (`szt(Meret, Ciklus, Adottak)`):
- `Meret`: A tábla mérete (n×n)
- `Ciklus`: A ciklushossz (m), azaz hány különböző szám van
- `Adottak`: Előre kitöltött mezők listája `i(Sor, Oszlop, Érték)` formában

**Kimenet**: Listák listája, ahol minden belső lista egy sor értékeit tartalmazza (0 = üres mező).

### Példa

```prolog
?- tekercs(szt(6,3,[i(1,5,2),i(2,2,1),i(4,6,1)]), Mx).
Mx = [[1,0,0,0,2,3],
      [0,1,2,3,0,0],
      [0,3,1,2,0,0],
      [0,2,3,0,0,1],
      [3,0,0,0,1,2],
      [2,0,0,1,3,0]]
```

---

## 2. Részfeladatokra Bontás

### 2.1. Kezdeti Tábla Létrehozása

**Feladat**: Az üres tábla inicializálása domain listákkal.

**Megoldás a kódban**: `kezdotabla/2` predikátum
- Minden cellához hozzárendelünk egy lehetséges értékek halmazát (domain)
- Ha `Meret > Ciklus`: domain = `[0, 1, ..., Ciklus]`
- Ha `Meret = Ciklus`: domain = `[1, ..., Ciklus]` (nincs 0, mert minden szám kell)
- Az előre megadott értékeket egyelemű listává alakítjuk

**Miért szükséges**: Ez az alap adatstruktúra, amelyen a szűkítések és keresés dolgozik.

### 2.2. Spirál Pozíciók Generálása

**Feladat**: A spirál mentén bejárandó pozíciók listájának előállítása.

**Megoldás a kódban**: `spiral_positions/2` predikátum
- Rekurzívan építi fel a spirál pozíciókat a `spiral_collect/5` segítségével
- Minden rétegben: felső él → jobb él → alsó él → bal él → belső réteg

**Miért szükséges**: A spirál kényszer ellenőrzéséhez és a DFS kereséshez ismernünk kell a bejárási sorrendet.

### 2.3. Kényszer Propagáció (Szűkítés)

A kényszer propagáció célja, hogy **csökkentsük a lehetséges értékek halmazát** anélkül, hogy visszalépéses keresést végeznénk.

#### 2.3.1. Ismert Értékek Propagálása (`ismert_szukites`)

**Feladat**: Ha egy cella egyértelmű (egyelemű domain), propagáljuk ezt a sorba és oszlopba.

**Algoritmus**:
1. Keress egy egyelemű listát tartalmazó cellát
2. Ha pozitív szám: távolítsd el ezt az értéket a sor és oszlop többi cellájából
3. Ha 0: ellenőrizd a 0-kvótát és szükség esetén távolítsd el a 0-t más cellákból
4. Ismételd fixpontig

**Miért működik**: Arc-konzisztencia fenntartása - ha egy érték csak egy helyen lehet, akkor ott biztosan az lesz.

#### 2.3.2. Kizárásos Szűkítés (`kizarasos_szukites`)

**Feladat**: Ha egy értéknek pontosan egy lehetséges helye van egy sorban/oszlopban, ott kell lennie.

**Algoritmus**:
1. Végigmegyünk minden soron és oszlopon
2. Minden lehetséges értékre (0..m) megszámoljuk, hány cella veheti fel
3. Ha pontosan 1 cella: oda tesszük az értéket
4. Ha 0 cella: ellentmondás

**Miért működik**: "Hidden single" technika - implicit kényszerek explicit kielégítése.

#### 2.3.3. Spirál Szűkítés (`mxtekercs_szukites`)

**Feladat**: A spirál kényszert használjuk a domainek szűkítésére.

**Algoritmus**:
1. Kivonjuk a spirál mentén lévő cellákat
2. Előre irányban: minden pozíción csak az előző pozitív érték ciklikus utódja lehet
3. Hátra irányban: minden pozíción csak az következő pozitív érték ciklikus elődje lehet
4. A két irány eredményét metsszük
5. Visszaírjuk a szűkített értékeket

**Miért működik**: A spirál kényszer lokális konzisztenciájának fenntartása.

### 2.4. Mélységi Keresés (DFS) a Spirál Mentén

**Feladat**: Ha a szűkítések nem elégségesek, visszalépéses kereséssel keressük a megoldást.

**Megoldás a kódban**: `dfs_spiral/11` és `try_place_bt/11` predikátumok

**Algoritmus**:
1. A spirál pozíciók mentén haladunk
2. Minden pozíción megpróbáljuk elhelyezni:
   - A spirálban következő elvárt értéket (ciklikus 1..m)
   - Vagy 0-t (ha megengedett)
3. Ellenőrizzük a kényszereket (sor/oszlop duplikáció, kvóták)
4. Ha zsákutcába jutunk, visszalépünk

**Miért működik**: A spirál szerinti sorrend biztosítja, hogy minden lépésnél pontosan tudjuk, melyik számnak kellene következnie.

### 2.5. Megoldás Konvertálása

**Feladat**: A belső reprezentációt (domain listák) átalakítjuk a kimeneti formátumra.

**Megoldás a kódban**: `matrix_to_solution/2` predikátum
- Egyelemű listákat kibontjuk
- Számokat megtartjuk

---

## 3. A Megoldás Helyességének Magyarázata

### 3.1. Teljesség

A megoldás **teljes**, mert:
1. A DFS keresés minden lehetséges értékelrendezést kipróbál
2. A `member/2` predikátum visszalépéskor új értékeket ad
3. Minden megoldást pontosan egyszer talál meg

### 3.2. Helyesség

A megoldás **helyes**, mert:

**Sor-oszlop kényszer betartása**:
- `can_place_value/5`: ellenőrzi, hogy az érték nem szerepel-e már a sorban/oszlopban
- `value_in_line/2`: megkeresi a duplikátumokat
- `check_row_col_counts/3`: végső ellenőrzés, hogy minden sorban/oszlopban pontosan m pozitív szám van

**Spirál kényszer betartása**:
- `ExpectedNextValue is (PlacedCount mod CycleLength) + 1`: kiszámítja a következő elvárt értéket
- `try_place_bt/11`: csak akkor helyez el számot, ha az megegyezik az elvárt értékkel

**Üres mezők kezelése**:
- `can_place_zero/4`: ellenőrzi, hogy a 0-kvóta (BoardSize - CycleLength) nincs-e túllépve
- `count_zeros_in_line/2`: megszámolja a 0-kat egy sorban/oszlopban

### 3.3. Hatékonyság

A szűkítések nagyban csökkentik a keresési teret:
- **Korai meghiúsulás**: Ha ellentmondást találunk, azonnal visszalépünk
- **Domain szűkítés**: Kevesebb lehetőség = kevesebb visszalépés
- **Propagáció**: Az egyértelmű értékek automatikusan tovább szűkítenek

---

## 4. Implementációs és Optimalizációs Döntések

### 4.1. Adatszerkezetek

#### Mátrix Reprezentáció
```prolog
Matrix = [[Cell11, Cell12, ...], [Cell21, Cell22, ...], ...]
```
- **Listák listája**: Természetes Prolog reprezentáció
- **Cellaértékek**: Lehetnek egész számok (végleges) vagy listák (domain)

#### Domain Reprezentáció
```prolog
Cell = [0, 1, 2, 3]  % lehetséges értékek
Cell = [2]           % egyelemű - szűkített
Cell = 2             % végleges érték
```
- **Ordset (rendezett lista)**: Hatékony metszés és unió műveletekhez
- **Egyelemű lista vs. szám**: Megkülönböztetjük a szűkítettet a véglegestől

#### Kényszerített Értékek Térképe (ForcedValueMap)
```prolog
ForcedValueMap = [0, 0, 2, 0, 1, ...]  % spirál index → érték
```
- **Lista-alapú**: O(1) indexelés `nth1/3`-mal
- **0 jelzi**: nincs előre megadott érték

#### Spirál Pozíciók
```prolog
SpiralPositions = [pos(1,1), pos(1,2), ..., pos(Row, Col), ...]
```
- **pos/2 struktúra**: Explicit sor-oszlop párok
- **Lista**: Természetes bejárási sorrend

### 4.2. Algoritmus Választások

#### Miért Spirál Mentén DFS?

1. **Determinisztikus sorrend**: A spirál kényszer miatt pontosan tudjuk, melyik szám következik
2. **Kevesebb döntés**: Csak azt kell eldönteni, hol legyen 0 és hol pozitív szám
3. **Korai vágás**: Ha nincs elég hely a hátralevő számoknak, azonnal visszalépünk

#### Miért Fixpontos Propagáció?

```prolog
propagate_all(PuzzleDescriptor, SpiralPositions, MatrixIn, MatrixOut) :-
    (   apply_one_restriction(PuzzleDescriptor, SpiralPositions, MatrixIn, TempMatrix)
    ->  propagate_all(PuzzleDescriptor, SpiralPositions, TempMatrix, MatrixOut)
    ;   MatrixOut = MatrixIn  % fixpont elérve
    ).
```

- **Maximális szűkítés**: Minden szűkítési lehetőséget kihasználunk keresés előtt
- **Iteráció limit**: Maximum 20 iteráció a végtelen ciklus elkerülésére

#### Miért Kétirányú Spirál Szűkítés?

```prolog
% Előre irány
tekercs_szukites(PuzzleDescriptor, SpiralIn, SpiralForward),
% Hátra irány
reverse(SpiralForward, ReversedInput),
tekercs_szukites_backward(PuzzleDescriptor, ReversedInput, ReversedNarrowed)
```

- **Előre**: Az előző értékek alapján szűkítünk
- **Hátra**: A következő értékek alapján szűkítünk
- **Kombináció**: Mindkét irány információját felhasználjuk

### 4.3. Optimalizációk

#### Korai Kapacitás Ellenőrzés
```prolog
RemainingPositions is TotalCells - CurrentIndex + 1,
RemainingNeeded is TotalPositiveRequired - PlacedCount,
RemainingPositions >= RemainingNeeded
```
- Ha nincs elég hely a hátralevő pozitív számoknak, azonnal visszalépünk

#### Hatékony Domain Műveletek
- `ord_intersection/3`, `ord_union/3`: O(n) műveletek rendezett listákon
- `ord_memberchk/2`: O(n) tagságvizsgálat

#### Egyetlen Lépéses Beállítás
```prolog
set_cell(Matrix, Row, Col, Value, UpdatedMatrix)
```
- Új mátrix létrehozása ahelyett, hogy módosítanánk (funkcionális stílus)
- Biztonságos visszalépéshez

---

## 5. Prolog Nyelvi Elemek Elméleti Háttere

### 5.1. Predikátumok és Klózok

#### Definíció
A Prolog program **predikátumokból** áll, amelyek **klózokkal** vannak definiálva.

```prolog
predikátum_név(Arg1, Arg2) :- Törzs.  % szabály
predikátum_név(Arg1, Arg2).           % tény
```

#### Szintaxis
- **Fej**: `predikátum_név(Argumentumok)`
- **Törzs**: Célok vesszővel elválasztva (ÉS kapcsolat)
- **`:-`**: "ha" jelentésű operátor

#### Példa a kódból
```prolog
% Tény
spiral_collect(TopRow, BottomRow, LeftCol, RightCol, []) :-
    (TopRow > BottomRow ; LeftCol > RightCol), !.

% Szabály
spiral_collect(TopRow, BottomRow, LeftCol, RightCol, Positions) :-
    TopRow =< BottomRow,
    LeftCol =< RightCol,
    collect_top_edge(TopRow, LeftCol, RightCol, TopEdgePositions),
    ...
```

### 5.2. Egyesítés (Unification)

#### Definíció
Az **egyesítés** a Prolog alapvető művelete, amely két termet összeilleszt.

#### Szabályok
1. Konstansok: `atom1 = atom1` sikeres, `atom1 = atom2` sikertelen
2. Változók: Bármivel egyesíthetők, felveszik az értéket
3. Struktúrák: Funktornak és argumentumoknak egyezniük kell

#### Szintaxis
```prolog
X = term       % explicit egyesítés
f(X, Y) = f(1, 2)  % X = 1, Y = 2
```

#### Példa a kódból
```prolog
PuzzleDescriptor = szt(BoardSize, CycleLength, GivenElements)
```
Ez szétbontja a struktúrát komponenseire.

### 5.3. Visszalépés (Backtracking)

#### Definíció
A **visszalépés** a Prolog végrehajtási mechanizmusa, amely automatikusan kipróbálja az alternatív megoldásokat.

#### Működés
1. Ha egy cél sikertelen, a Prolog visszalép az előző választási pontra
2. Kipróbálja a következő alternatívát
3. Folytatja előre, amíg megoldást talál vagy kimerülnek az alternatívák

#### Példa a kódból
```prolog
member(ExpectedValue, CellDomain)  % választási pont
```
Ha a keresés zsákutcába jut, a `member/2` a domain következő értékét adja.

### 5.4. Vágás (Cut, `!`)

#### Definíció
A **vágás** (`!`) megakadályozza a visszalépést a vágás pontja előtti alternatívákra.

#### Szintaxis
```prolog
predikátum(X) :- feltétel, !, többi_cél.
```

#### Típusok
- **Zöld vágás**: Csak optimalizáció, nem változtatja a logikai jelentést
- **Piros vágás**: Megváltoztatja a program viselkedését

#### Példa a kódból
```prolog
spiral_collect(TopRow, BottomRow, LeftCol, RightCol, []) :-
    (TopRow > BottomRow ; LeftCol > RightCol), !.
```
Ez megakadályozza, hogy a másik klóz is lefusson, ha a feltétel teljesül.

### 5.5. Negáció mint Meghiúsulás (`\+`)

#### Definíció
A `\+` operátor igaz, ha az argumentuma **meghiúsul** (nem bizonyítható).

#### Szintaxis
```prolog
\+ Cél
```

#### Viselkedés
- Ha `Cél` sikeres: `\+ Cél` sikertelen
- Ha `Cél` sikertelen: `\+ Cél` sikeres

#### Példa a kódból
```prolog
\+ value_in_line(RowValues, Value)
```
Igaz, ha az érték nem szerepel a sorban.

### 5.6. Feltételes Kifejezés (If-Then-Else)

#### Definíció
A `->` és `;` operátorokkal feltételes elágazást valósíthatunk meg.

#### Szintaxis
```prolog
( Feltétel -> Igaz_ág ; Hamis_ág )
```

#### Viselkedés
1. Ha `Feltétel` sikeres: `Igaz_ág` fut le
2. Ha `Feltétel` sikertelen: `Hamis_ág` fut le

#### Példa a kódból
```prolog
(   Value > 0
->  propagate_fixed_positive(MatrixIn, Row, Col, Value, TempMatrix)
;   propagate_fixed_zero(MatrixIn, BoardSize, CycleLength, ZeroQuota, Row, Col, TempMatrix) )
```

### 5.7. Listák

#### Definíció
A **lista** a Prolog alapvető összetett adatstruktúrája.

#### Szintaxis
```prolog
[]              % üres lista
[Fej|Farok]     % fej-farok bontás
[1, 2, 3]       % konkrét lista
[X|Xs]          % minta illesztés
```

#### Lista Műveletek a Kódban
```prolog
length(List, N)           % lista hossza
nth1(Index, List, Elem)   % elem indexelés (1-től)
append(L1, L2, L3)        % listák összefűzése
reverse(L1, L2)           % lista megfordítása
member(Elem, List)        % elem keresés (visszalépéses)
maplist(Pred, L1, L2)     % transzformáció
```

### 5.8. Aritmetika

#### Kiértékelés
```prolog
X is Kifejezés    % X felveszi a kifejezés értékét
```

#### Összehasonlítás
```prolog
X =:= Y    % aritmetikai egyenlőség
X =\= Y    % aritmetikai különbözőség
X < Y      % kisebb
X > Y      % nagyobb
X =< Y     % kisebb vagy egyenlő
X >= Y     % nagyobb vagy egyenlő
```

#### Példa a kódból
```prolog
ExpectedNextValue is (PlacedCount mod CycleLength) + 1
```

### 5.9. Struktúrák (Compound Terms)

#### Definíció
A **struktúra** egy funktorból és argumentumokból áll.

#### Szintaxis
```prolog
funktor(arg1, arg2, ...)
```

#### Példák a kódból
```prolog
szt(BoardSize, CycleLength, GivenElements)  % feladványleíró
i(Row, Col, Value)                           % előre megadott elem
pos(Row, Col)                                % spirál pozíció
```

### 5.10. Modul Rendszer

#### Definíció
A **modul** névteret biztosít és elrejti a belső predikátumokat.

#### Szintaxis
```prolog
:- use_module(library(lists)).  % könyvtár betöltése
```

#### A Kódban Használt Könyvtár
```prolog
:- use_module(library(lists)).
```
Ez biztosítja: `nth1/3`, `nth1/4`, `append/3`, `reverse/2`, `member/2`, `maplist/2`, `maplist/3`

### 5.11. Magasabb Rendű Predikátumok

#### maplist/2, maplist/3
```prolog
maplist(Pred, List)           % Pred alkalmazása minden elemre
maplist(Pred, List1, List2)   % páronkénti transzformáció
```

#### Példa a kódból
```prolog
maplist(cell_at(Matrix), SpiralPositions, SpiralValues)
```
Ez minden spirál pozícióra lekéri a cella értékét.

### 5.12. Rendezett Halmazok (Ordered Sets)

#### Definíció
A **rendezett halmaz** egy növekvő sorrendű, duplikátummentes lista.

#### Implementált Műveletek a Kódban
```prolog
ord_memberchk(Elem, Set)      % tagságvizsgálat
ord_add_element(Set, E, New)  % elem hozzáadása
ord_intersection(A, B, I)     % metszet
ord_union(A, B, U)            % unió
list_to_ord_set(L, Set)       % lista konvertálása
```

#### Miért Használjuk?
- O(n) műveletek O(n²) helyett, ahol n a halmaz elemeinek száma
- Garantált duplikátummentesség
- Hatékony domain műveletek a kényszer propagáció során

### 5.13. Akkumulátor Technika

#### Definíció
Az **akkumulátor** egy extra argumentum, amely az eredményt építi fel a rekurzió során.

#### Minta
```prolog
pred(List, Result) :-
    pred_acc(List, [], Result).  % inicializálás

pred_acc([], Acc, Acc).          % alap eset: akkumulátor = eredmény
pred_acc([H|T], Acc, Result) :-
    NewAcc = ...,                % akkumulátor frissítése
    pred_acc(T, NewAcc, Result).
```

#### Példa a kódból
```prolog
count_positive_in_line_([], Accumulator, Accumulator).
count_positive_in_line_([Cell|RestCells], Accumulator, Count) :-
    (   ... -> NextAccumulator is Accumulator + 1
    ;   NextAccumulator = Accumulator
    ),
    count_positive_in_line_(RestCells, NextAccumulator, Count).
```

### 5.14. Különbségi Lista (nth1/4)

#### Definíció
Az `nth1/4` a lista egy elemét és a "maradék" listát is visszaadja.

#### Szintaxis
```prolog
nth1(Index, Lista, Elem, Maradek)
```
- `Maradek` = Lista az Elem nélkül

#### Példa a kódból
```prolog
nth1(R, Mx0, RowR, RestRows)  % kivesszük az R. sort
```

### 5.15. Determinisztikus vs. Nemdeterminisztikus Predikátumok

#### Determinisztikus
- Pontosan egy megoldást ad
- Általában vágással (`!`) biztosítva

#### Nemdeterminisztikus
- Több megoldást adhat visszalépéssel
- Választási pontokat hoz létre

#### Példa a kódból
```prolog
% Nemdeterminisztikus - visszalépéssel próbál értékeket
try_place_bt(CellDomain, ForcedValue, ExpectedValue, Row, Col, Matrix,
             CycleLength, _ZerosPerLine, UpdatedMatrix, NewPlacedCount, CurrentPlacedCount) :-
    is_list(CellDomain),
    length(CellDomain, DomainSize), DomainSize > 1,
    ...
    member(ExpectedValue, CellDomain),  % választási pont
    ...
```

---

## Összefoglalás

A számtekercs megoldó három fő komponensből áll:

1. **Kényszer propagáció**: Fixpontig alkalmazza az ismert értékek, kizárásos és spirál szűkítéseket
2. **Mélységi keresés**: A spirál mentén halad, és visszalépéssel keresi a megoldásokat
3. **Kényszer ellenőrzés**: Minden lépésnél és végén ellenőrzi a sor/oszlop/spirál kényszereket

A Prolog nyelv deklaratív természete és beépített visszalépés mechanizmusa természetesen illeszkedik ehhez a kényszerkielégítési problémához.
