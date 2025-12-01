# Nhf1 — Helix (Számtekercs) megoldó dokumentáció

Ez a README nagyon részletesen összefoglalja a `feladat.txt`-ben leírt problémát, bemutatja, hogyan bontja le a `nhf1.ex` kisebb, kezelhető részfeladatokra, miért adnak ezek együtt helyes és teljes megoldást, és milyen implementációs döntések (algoritmusok, adatszerkezetek) állnak mögötte. Részletesen tárgyalja az összes használt Elixir nyelvi elem elméleti hátterét is.

---

## Tartalomjegyzék

1. [A feladat értelmezése](#1-a-feladat-értelmezése)
2. [Részfeladatokra bontás](#2-részfeladatokra-bontás)
3. [A kód helyességének bizonyítása](#3-a-kód-helyességének-bizonyítása)
4. [Implementációs és optimalizációs döntések](#4-implementációs-és-optimalizációs-döntések)
5. [Elixir nyelvi elemek elméleti háttere](#5-elixir-nyelvi-elemek-elméleti-háttere)
6. [Futtatás és benchmark](#6-futtatás-és-benchmark)
7. [További optimalizációs ötletek](#7-további-optimalizációs-ötletek)

---

## 1. A feladat értelmezése

### 1.1. Probléma leírása

A feladat egy **Számtekercs** (Helix) nevű logikai rejtvény megoldását kéri. Adott egy **n×n méretű négyzetes tábla**, amelynek egyes mezőiben már elő vannak írva bizonyos értékek (1 és m közötti számok). A célunk az, hogy a táblát úgy töltsük ki további 1..m közötti számokkal, hogy az alábbi **két fő feltétel** egyszerre teljesüljön:

#### 1.1.1. Latin négyzet feltétel (sor/oszlop egyediség)
- **Minden sorban** az 1, 2, ..., m számok mindegyike **pontosan egyszer** szerepel.
- **Minden oszlopban** az 1, 2, ..., m számok mindegyike **pontosan egyszer** szerepel.
- A fennmaradó (n - m) darab mező **üres marad** (ezeket 0-val jelöljük a kimenetben).

Ez a feltétel egy részleges latin négyzet definíciójának felel meg, ahol nem minden cellát töltünk ki, hanem csak soronként és oszloponként pontosan m darabot.

#### 1.1.2. Helix (spirál) sorrend feltétel
Ha a táblát egy speciális **spirális (tekeredő) vonal** mentén járjuk be, akkor az ezen az úton talált **nem-0 értékeknek** az alábbi ciklikus sorrendben kell követniük egymást:
```
1, 2, 3, ..., m, 1, 2, 3, ..., m, 1, 2, ...
```

A spirális bejárás definíciója:
1. Indulás a **bal felső sarokból** (1. sor, 1. oszlop)
2. Haladás **jobbra** az első sor végéig
3. Haladás **lefelé** az utolsó oszlopban
4. Haladás **balra** az utolsó sorban
5. Haladás **felfelé** az első oszlopban (a 2. sor 1. mezőjéig)
6. Ezután **rekurzívan** folytatjuk a belső (n-2)×(n-2) méretű négyzettel

### 1.2. Bemenet és kimenet specifikáció

#### Bemenet
```elixir
{n, m, constraints}
```
ahol:
- `n` — a tábla mérete (n×n)
- `m` — a ciklushossz (1 ≤ m ≤ n)
- `constraints` — előre rögzített mezők listája `[{{sor, oszlop}, érték}, ...]` formában

#### Kimenet
Az összes lehetséges megoldás listája (tetszőleges sorrendben). Minden megoldás egy n×n-es egész számokból álló lista-lista:
- `0` — üres mező
- `1..m` — kitöltött mező az adott értékkel

### 1.3. Típusdefiníciók (a kódban is)

```elixir
@type size()  :: integer()                              # tábla mérete (0 < n)
@type cycle() :: integer()                              # ciklushossz (0 < m ≤ n)
@type value() :: integer()                              # mező értéke (0 < v ≤ m)
@type row()   :: integer()                              # sor száma (1-től n-ig)
@type col()   :: integer()                              # oszlop száma (1-től n-ig)
@type field() :: {row(), col()}                         # mező koordinátái
@type field_value() :: {field(), value()}               # mező és értéke
@type puzzle_desc() :: {size(), cycle(), [field_value()]} # feladvány
@type retval()    :: integer()                          # eredménymező értéke (0 ≤ rv ≤ m)
@type solution()  :: [[retval()]]                       # egy megoldás
@type solutions() :: [solution()]                       # összes megoldás
```

### 1.4. Példa

**Feladvány** (n=6, m=3):
```
{6, 3, [{{1,5},2}, {{2,2},1}, {{4,6},1}]}
```

Ez azt jelenti:
- 6×6-os tábla
- Minden sorban és oszlopban az 1, 2, 3 számok pontosan egyszer
- Előre megadott értékek:
  - (1,5) pozícióban: 2
  - (2,2) pozícióban: 1
  - (4,6) pozícióban: 1

**Megoldás:**
```elixir
[[1,0,0,0,2,3],
 [0,1,2,3,0,0],
 [0,3,1,2,0,0],
 [0,2,3,0,0,1],
 [3,0,0,0,1,2],
 [2,0,0,1,3,0]]
```

---

## 2. Részfeladatokra bontás

A komplex feladatot az alábbi **hat fő részfeladatra** bontjuk, amelyek együttesen biztosítják a teljes és helyes megoldást:

### 2.1. Részfeladat: Spirális bejárás előállítása

**Mi a cél?**
Előállítani az n×n-es tábla összes cellájának koordinátáit abban a sorrendben, ahogyan a spirális bejárás érinti őket.

**Miért szükséges?**
A helix feltétel a spirális sorrend mentén definiált. Ahhoz, hogy ellenőrizni tudjuk a ciklikus 1,2,...,m,1,2,... sorrendet, először meg kell határoznunk magát a bejárási sorrendet.

**Implementáció a kódban:**
- `build_spiral_positions/1` — fő függvény, amely elindítja a rekurzív építést
- `build_spiral_layers/5` — rekurzívan építi a spirált rétegenként (külső peremtől befelé)

**Miért oldja meg?**
A spirális koordinátalista egy 0-tól (n²-1)-ig tartó lineáris indexelést indukál. Ezzel a globális spirál feltételt lokális, index-alapú döntésekre fordíthatjuk.

### 2.2. Részfeladat: Megszorítások indexre vetítése

**Mi a cél?**
Az előre megadott `{{r,c}, v}` kényszereket leképezni „spirálindex → fix érték" formára.

**Miért szükséges?**
A keresés során spirálindex alapján haladunk. Gyorsan el kell dönteni, hogy egy adott spirálpozícióra van-e kényszer, és ha igen, milyen értékkel.

**Implementáció a kódban:**
- `map_forced_cells_to_spiral_indices/2` — a kényszerek map-pé alakítása
- `build_constraint_arrays/2` — három segédtömb építése:
  - `forced_value_at_index_t`: indexenként 0 (nincs kényszer) vagy v ∈ 1..m
  - `forced_prefix_count_t`: prefix-összegek a lookahead számításokhoz
  - `next_forced_index_at_or_after_t`: a következő kényszer indexe (gyors kereséshez)

**Miért oldja meg?**
O(1) időben lekérdezhető, hogy egy adott pozícióra van-e kényszer, és milyen értékű. Ez kritikus a keresés sebességéhez.

### 2.3. Részfeladat: Sor/oszlop egyediség és kvóta kezelése

**Mi a cél?**
Biztosítani, hogy minden sorban és oszlopban az 1..m értékek pontosan egyszer szerepeljenek.

**Miért szükséges?**
Ez a latin négyzet feltétel ellenőrzése. Minden elhelyezés előtt meg kell győződni, hogy az adott érték még nem szerepel az adott sorban/oszlopban, és hogy a sor/oszlop még nem érte el az m darab nem-0 kvótát.

**Implementáció a kódban:**
- `row_used_value_masks_t`, `col_used_value_masks_t` — bitmaszkók a használt értékek nyilvántartására
- `row_placed_count_t`, `col_placed_count_t` — számlálók az elhelyezett értékek számára
- `can_place_mask?/8` — O(1) ellenőrzés, hogy lerakható-e egy érték
- `apply_value_mask/3` — bit beállítása elhelyezés után
- `counts_meet_quota?/2` — végső ellenőrzés, hogy minden vonal elérte-e az m kvótát

**Miért oldja meg?**
A bitmaszkók és számlálók együttesen biztosítják, hogy soha nem helyezünk el duplikátumot, és pontosan m darab nem-0 értéket teszünk minden sorba/oszlopba.

### 2.4. Részfeladat: Suffix kapacitások előszámítása

**Mi a cél?**
Előre kiszámítani, hogy az i-edik spirálindextől a végéig hány pozíció esik még az egyes sorokra/oszlopokra.

**Miért szükséges?**
Ez egy „pruning" (metszési) technika. Ha egy sor/oszlop még x darab értéket igényel, de a hátralévő pozíciók között már csak y < x pozíció tartozik hozzá, akkor az aktuális ág biztosan nem vezethet megoldáshoz.

**Implementáció a kódban:**
- `compute_suffix_capacities/2` — előállítja a suffix kapacitás tömböket
- `has_sufficient_row_and_column_capacity?/9` — ellenőrzi, hogy a kapacitás elegendő-e

**Miért oldja meg?**
Korán kiszűri a zsákutca ágakat, jelentősen csökkentve a keresési tér méretét.

### 2.5. Részfeladat: Visszalépéses keresés (DFS) a spirál mentén

**Mi a cél?**
Bejárni az összes lehetséges elhelyezési kombinációt, miközben minden lépésben betartjuk a feltételeket.

**Miért szükséges?**
A feladat megoldása NP-teljes probléma általános esetben. A visszalépéses keresés (backtracking) a klasszikus módszer az összes megoldás megtalálására.

**Implementáció a kódban:**
- `dfs_spiral_search/...` — a fő rekurzív kereső függvény
- Minden spirálpozíciónál két lehetőség:
  - **PLACE** (`maybe_place_branch/...`): lerakjuk a következő elvárt értéket
  - **SKIP** (`maybe_skip_branch/...`): üresen hagyjuk a cellát

**Miért oldja meg?**
A DFS szisztematikusan végigpróbálja az összes SKIP/PLACE kombinációt. A metszések (pruning) biztosítják, hogy csak érvényes útvonalakat járunk be, de egyetlen helyes megoldást sem hagyunk ki.

### 2.6. Részfeladat: Megoldás összeállítása

**Mi a cél?**
Ha elértük a spirál végét érvényes állapotban, összeállítani a teljes n×n-es táblát.

**Miért szükséges?**
A keresés során csak az elhelyezéseket (spirálindex, érték) párokként tároljuk a memóriahatékonyság érdekében. A végeredményt teljes táblaként kell visszaadni.

**Implementáció a kódban:**
- `build_solution_board/3` — összefogja a tábla építését
- `build_assignments_from_placements/2` — a placements listát map-pé alakítja
- `assemble_board_from_map/2` — a map-ből n×n listát épít (hiányzó cellák 0)

**Miért oldja meg?**
A hatékony belső reprezentációt a specifikáció szerinti kimeneti formátumra alakítja.

### 2.7. Hogyan működnek együtt a részfeladatok?

```
1. Spirál előállítása     →  Definiálja a bejárási sorrendet
        ↓
2. Kényszerek indexelése  →  Gyors hozzáférés az előírt értékekhez
        ↓
3. Sor/oszlop kezelés     →  Latin négyzet feltétel betartása
        ↓
4. Suffix kapacitások     →  Korai metszés a zsákutcáknál
        ↓
5. DFS keresés            →  Szisztematikus próbálkozás
        ↓
6. Tábla összeállítás     →  Kimeneti formátum előállítása
```

Minden részfeladat egy specifikus aspektusát kezeli a problémának. Együttesen garantálják, hogy:
- Minden megoldást megtalálunk (teljesség)
- Csak helyes megoldásokat adunk vissza (helyesség)
- A keresés elfogadható idő alatt lefut (hatékonyság)

---

## 3. A kód helyességének bizonyítása

### 3.1. Helyesség (nincsenek hamis pozitívok)

A kód **csak és kizárólag helyes megoldásokat** ad vissza. Ezt az alábbi invariánsok biztosítják:

#### 3.1.1. Spirál szabály betartása
```elixir
next_value = rem(placed_values_count, cycle_length) + 1
```

- Az elhelyezett k-adik nem-0 értéknek pontosan `((k-1) mod m) + 1`-nek kell lennie
- A `placed_values_count` változó nyomon követi az eddig elhelyezett nem-0 értékek számát
- Minden PLACE művelet előtt ellenőrizzük, hogy a lerakandó érték megegyezik-e a várt `next_value`-val
- Ha van kényszer (`forced_value != 0`), az csak akkor fogadható el, ha megegyezik a `next_value`-val

#### 3.1.2. Latin négyzet szabály betartása
```elixir
defp can_place_mask?(row_idx0, col_idx0, mask, row_used_value_masks_t, col_used_value_masks_t, 
                     row_placed_count_t, col_placed_count_t, cycle_len) do
  row_ok = (elem(row_used_value_masks_t, row_idx0) &&& mask) == 0 and 
           elem(row_placed_count_t, row_idx0) < cycle_len
  col_ok = (elem(col_used_value_masks_t, col_idx0) &&& mask) == 0 and 
           elem(col_placed_count_t, col_idx0) < cycle_len
  row_ok and col_ok
end
```

- A bitmaszkok biztosítják, hogy egy érték nem ismétlődhet ugyanabban a sorban/oszlopban
- A számlálók biztosítják, hogy soronként/oszloponként pontosan m darab értéket helyezünk el
- A levélellenőrzés (`counts_meet_quota?/2`) garantálja, hogy minden vonal elérte az m kvótát

#### 3.1.3. Kényszerek betartása
```elixir
can_place = (forced_value == 0 or forced_value == next_value) and ...
```

- Ha egy pozícióra van kényszer, az csak akkor fogadható el, ha:
  - Az előírt érték megegyezik a spirál fázis szerinti következő várt értékkel
  - A sor/oszlop ellenőrzés is átmegy
- Ellenkező esetben az ág azonnal levágódik

### 3.2. Teljesség (nincsenek hamis negatívok)

A kód **minden helyes megoldást megtalál**. Ezt az alábbiak garantálják:

#### 3.2.1. Teljes bejárás
- A DFS minden spirálpozíciónál megvizsgálja mindkét lehetőséget (PLACE és SKIP)
- A keresés csak akkor ér véget, ha minden pozíciót bejártunk

#### 3.2.2. Metszések helyessége
A metszések (pruning) **csak** olyan ágakat vágnak le, amelyek **biztosan nem** vezethetnek megoldáshoz:

1. **Globális kapacitásmetszés:**
   ```elixir
   remaining_positions >= remaining_needed
   ```
   Ha a hátralévő pozíciók száma kevesebb, mint a még szükséges értékek száma, az ág biztosan nem teljesíthető.

2. **Suffix kapacitásmetszés:**
   ```elixir
   remaining_row_slots >= row_needed and remaining_col_slots >= col_needed
   ```
   Ha egy sor/oszlop már nem tudja elérni az m kvótát a hátralévő pozíciókon, az ág biztosan nem teljesíthető.

3. **Alignment lookahead:**
   ```elixir
   alignment_feasible?(idx, placed_count, cycle_len, ...)
   ```
   Ellenőrzi, hogy a következő kényszerig van-e olyan lehetséges elhelyezésszám, ami a fázissal konzisztens. Ha nincs, az ág biztosan nem teljesíthető.

### 3.3. Termináció

A keresés **mindig véget ér**:

- A keresési tér véges: n² pozíció, minden pozíción bináris döntés (PLACE/SKIP)
- Maximális mélység: n²
- A metszések csak csökkentik a bejárt csúcsok számát, de nem akadályozzák a terminációt

---

## 4. Implementációs és optimalizációs döntések

### 4.1. Adatszerkezetek megválasztása

#### 4.1.1. Tuple-ök használata listák helyett

**Döntés:** A hot path-on (gyakran használt útvonalakon) tuple-öket használunk listák helyett.

**Indoklás:**
- `elem/2` és `put_elem/3` O(1) időben működik tuple-ökön
- Listák esetén az elem elérése O(n) lenne
- A backtracking során gyakran kell random access a spirálpozíciókhoz, maszkokhoz és számlálókhoz

**Érintett adatszerkezetek:**
```elixir
spiral_positions_t      # spirálpozíciók tuple-je
spiral_row_index_t      # előszámolt sorindexek
spiral_col_index_t      # előszámolt oszlopindexek
row_used_value_masks_t  # sor maszkok
col_used_value_masks_t  # oszlop maszkok
row_placed_count_t      # sor számlálók
col_placed_count_t      # oszlop számlálók
value_bitmask_t         # érték→maszk leképezés
forced_value_at_index_t # kényszer értékek
```

#### 4.1.2. Bitmaszkok az egyediség kezeléséhez

**Döntés:** Az 1..m értékek jelenlétét bitmaszkokkal reprezentáljuk.

**Indoklás:**
- Egy m-bites egész szám tárolja, hogy mely értékek vannak már használatban
- Az i-edik bit 1, ha az i értéket már elhelyeztük
- Ellenőrzés: `(mask &&& (1 <<< (v-1))) == 0` — O(1)
- Beállítás: `mask ||| (1 <<< (v-1))` — O(1)

**Előny a halmazokkal szemben:**
- Konstans idejű műveletek
- Minimális memóriahasználat
- CPU-barát bitműveletek

#### 4.1.3. Map a placements tárolására keresés közben

**Döntés:** A keresés során az elhelyezéseket `[{spirálindex, érték}, ...]` listában tároljuk.

**Indoklás:**
- A lista elejére való beszúrás O(1)
- Backtracking esetén automatikusan „visszavonódik" az utolsó elem (nincs explicit törlés)
- A teljes tábla csak levélszinten materializálódik

### 4.2. Algoritmus megválasztása

#### 4.2.1. DFS (Depth-First Search) a BFS helyett

**Döntés:** Mélységi keresést alkalmazunk a szélességi keresés helyett.

**Indoklás:**
- Memóriahatékony: O(n²) stack mélység vs. exponenciális szélességi sor
- A backtracking természetes módon illeszkedik a DFS-hez
- Minden megoldást megtalál (nem csak a legrövidebbet keressük)

#### 4.2.2. Lokális fázisszabály a globális ellenőrzés helyett

**Döntés:** A spirál feltételt lokálisan ellenőrizzük:
```elixir
next_value = rem(placed_values_count, cycle_length) + 1
```

**Indoklás:**
- Nem kell a teljes eddigi prefix-et cipelni és ellenőrizni
- A `placed_values_count` egyetlen szám elegendő a fázis meghatározásához
- O(1) számítás minden lépésben

### 4.3. Optimalizációs technikák

#### 4.3.1. Többszintű metszés (pruning)

A keresési tér csökkentése érdekében három szintű metszést alkalmazunk:

1. **Globális kapacitásmetszés** — legolcsóbb, minden lépésben
2. **Suffix kapacitásmetszés** — olcsó, lokális ellenőrzés
3. **Alignment lookahead** — drágább, csak ablakon belül

#### 4.3.2. Előszámítások

Több adatszerkezetet előre kiszámítunk a keresés előtt:

```elixir
# Spirálpozíciók és indexek
{spiral_positions, spiral_positions_t, spiral_row_index_t, spiral_col_index_t, index_by_position} =
  prepare_spiral_and_indices(board_size)

# Kényszer tömbök
{forced_value_at_index_t, forced_prefix_count_t, next_forced_index_at_or_after_t} =
  build_constraint_arrays(forced_values_by_index, total_cells)

# Suffix kapacitások
{row_suffix_capacity_t, col_suffix_capacity_t} =
  compute_suffix_capacities(spiral_positions, board_size)

# Érték maszkok
value_bitmask_t = build_value_mask_table(cycle_length)
```

**Előny:** Ezek az előszámítások egyszer futnak le, de a keresés során sokszor használjuk őket.

#### 4.3.3. Ablakolt alignment lookahead

**Döntés:** Az alignment ellenőrzést csak akkor futtatjuk, ha a következő kényszer egy konfigurálható ablakon belül van.

```elixir
@alignment_window 64  # alapértelmezett ablakméret

if fi - next_idx <= align_window_sz do
  alignment_feasible?(...)
else
  true  # távolabbi kényszereket nem ellenőrizzük
end
```

**Indoklás:**
- Az alignment ellenőrzés költséges
- Távoli kényszerek esetén a korai ellenőrzés ritkán talál konfliktust
- Az ablak mérete hangolható a `HELIX_ALIGN_WIN` környezeti változóval

---

## 5. Elixir nyelvi elemek elméleti háttere

### 5.1. Modul és modulattribútumok

#### 5.1.1. `defmodule`

**Definíció:** Az Elixir kód modulokba szerveződik. A `defmodule` makró egy új modult definiál.

**Szintaxis:**
```elixir
defmodule ModulNév do
  # modul törzse
end
```

**A kódban:**
```elixir
defmodule Nhf1 do
  # ...
end
```

**Elméleti háttér:**
- A modulok az Elixir alapvető kódszervezési egységei
- Minden függvény egy modulhoz tartozik
- A modul neve atom típusú (pl. `Nhf1` → `:Elixir.Nhf1`)
- A modulok lefordított formában BEAM fájlokban tárolódnak

#### 5.1.2. `@moduledoc`

**Definíció:** Dokumentációs attribútum, amely a modul egészét dokumentálja.

**Szintaxis:**
```elixir
@moduledoc """
Modul leírása.
"""
```

**A kódban:**
```elixir
@moduledoc """
Számtekercs

@author "Toronyi Zsombor <toronyizsombor@edu.bme.hu> [S8F7DV]"
@date   "2025-10-18"
"""
```

**Elméleti háttér:**
- Az ExDoc eszköz ezekből generál dokumentációt
- Az `h/1` IEx paranccsal megtekinthető
- Fordításkor metaadatként tárolódik

#### 5.1.3. `@type`

**Definíció:** Típusdefiníciós attribútum a Dialyzer statikus elemzőhöz.

**Szintaxis:**
```elixir
@type típusnév() :: típusspecifikáció
```

**A kódban:**
```elixir
@type size()  :: integer()
@type cycle() :: integer()
@type field() :: {row(), col()}
@type puzzle_desc() :: {size(), cycle(), [field_value()]}
```

**Elméleti háttér:**
- Az Elixir dinamikusan típusos, de támogatja az opcionális típusannotációkat
- A Dialyzer statikus elemző használja a típushibák korai felismeréséhez
- Nem befolyásolja a futásidejű viselkedést, csak dokumentációs és ellenőrzési célú

#### 5.1.4. `@spec`

**Definíció:** Függvény-specifikációs attribútum, amely megadja a függvény típusát.

**Szintaxis:**
```elixir
@spec függvénynév(param_típus1, param_típus2) :: visszatérési_típus
```

**A kódban:**
```elixir
@spec helix(sd :: puzzle_desc()) :: ss :: solutions()
@spec validate_constraints(size(), cycle(), [field_value()]) :: :ok | {:error, term()}
@spec build_spiral_positions(size()) :: [field()]
```

**Elméleti háttér:**
- A Dialyzer ezeket használja a típuskonzisztencia ellenőrzéséhez
- A `::` operátor megnevezett paramétereket is megenged (`sd :: puzzle_desc()`)
- Összetett típusok: union (`|`), tuple (`{}`), lista (`[]`), map (`%{}`)

#### 5.1.5. Egyéni modul attribútumok

**Definíció:** A `@` jellel tetszőleges attribútumok definiálhatók.

**Szintaxis:**
```elixir
@attribútum_név érték
```

**A kódban:**
```elixir
@alignment_window 64
```

**Elméleti háttér:**
- Fordítási időben konstansként behelyettesítődnek
- Nem változtathatók futásidőben
- Modulszintű konfigurációhoz ideálisak

### 5.2. Függvénydefiníciók

#### 5.2.1. `def` — publikus függvények

**Definíció:** Publikus (exportált) függvényt definiál.

**Szintaxis:**
```elixir
def függvénynév(param1, param2) do
  # törzs
end
```

**A kódban:**
```elixir
def helix(sd) do
  case sd do
    {board_size, cycle_length, fixed_cells} when ... -> ...
    _ -> []
  end
end
```

**Elméleti háttér:**
- Kívülről meghívható: `Nhf1.helix(puzzle)`
- Több klózt is definiálhatunk pattern matchinggel
- A függvény aritása (paraméterek száma) a név része: `helix/1`

#### 5.2.2. `defp` — privát függvények

**Definíció:** Privát (nem exportált) függvényt definiál.

**Szintaxis:**
```elixir
defp függvénynév(param1, param2) do
  # törzs
end
```

**A kódban:**
```elixir
defp validate_constraints(n, m, constraints) do
  # ...
end

defp build_spiral_positions(n) do
  build_spiral_layers(1, 1, n, n, [])
end
```

**Elméleti háttér:**
- Csak a modulon belülről hívható
- Segédfüggvényekhez használjuk
- Nem jelenik meg a modul publikus API-jában

#### 5.2.3. Pattern matching függvényparaméterekben

**Definíció:** A függvényparaméterek mintaillesztéssel is definiálhatók.

**A kódban:**
```elixir
defp build_spiral_layers(top, left, bottom, right, acc) when top > bottom or left > right, do: acc
defp build_spiral_layers(top, left, bottom, right, acc) do
  # ...
end
```

**Elméleti háttér:**
- A klózok sorrendje számít — az első illeszkedő hajtódik végre
- A `when` őrfeltétel (guard) tovább szűkíti az illeszkedést
- Ez a polimorfizmus funkcionális megvalósítása

#### 5.2.4. Guard-ok (őrfeltételek)

**Definíció:** A `when` kulcsszó után megadott logikai kifejezések, amelyek tovább szűrik a mintaillesztést.

**Szintaxis:**
```elixir
def függvény(param) when feltétel do
  # törzs
end
```

**A kódban:**
```elixir
{board_size, cycle_length, fixed_cells} when is_integer(board_size) and board_size > 0 
  and is_integer(cycle_length) and cycle_length > 0 and cycle_length <= board_size 
  and is_list(fixed_cells) -> ...

defp build_spiral_layers(top, left, bottom, right, acc) when top > bottom or left > right, do: acc
```

**Elméleti háttér:**
- Csak speciális függvények használhatók guard-okban (pl. `is_integer/1`, `length/1`)
- Mellékhatás-mentes kifejezések megengedettek
- Többszörös guard-ok `and`, `or` operátorokkal kombinálhatók

### 5.3. Adatszerkezetek

#### 5.3.1. Lista (`[]`)

**Definíció:** Láncolt lista, az Elixir alapvető kollekció típusa.

**Szintaxis:**
```elixir
[elem1, elem2, elem3]
[fej | farok]  # cons cella
```

**A kódban:**
```elixir
top_row = for c <- left..right, do: {top, c}
acc2 = acc ++ top_row ++ right_col ++ bottom_row ++ left_col
[board | solutions_acc]
```

**Elméleti háttér:**
- Egyszeresen láncolt lista
- Fejelem elérése: O(1)
- Elem hozzáadása az elejéhez: O(1)
- Hozzáfűzés a végéhez (`++`): O(n)
- Immutábilis — módosítás helyett új lista jön létre

#### 5.3.2. Tuple (`{}`)

**Definíció:** Fix méretű, heterogén elemeket tartalmazó gyűjtemény.

**Szintaxis:**
```elixir
{elem1, elem2, elem3}
elem(tuple, index)      # elem lekérése
put_elem(tuple, idx, v) # új tuple módosított elemmel
```

**A kódban:**
```elixir
spiral_positions_t = List.to_tuple(spiral_positions)
row_idx0 = elem(spiral_row_index_t, spiral_index)
new_row = put_elem(prev_row, row_idx0, elem(prev_row, row_idx0) + 1)
```

**Elméleti háttér:**
- Folytonos memóriaterületen tárolódik
- Elem elérése: O(1)
- Módosítás: O(n) — teljes másolat készül
- Ideális fix méretű, gyakran olvasott adatokhoz

#### 5.3.3. Map (`%{}`)

**Definíció:** Kulcs-érték párok asszociatív gyűjteménye.

**Szintaxis:**
```elixir
%{kulcs1 => érték1, kulcs2 => érték2}
Map.get(map, kulcs, alapértelmezett)
Map.put(map, kulcs, érték)
Map.fetch!(map, kulcs)
```

**A kódban:**
```elixir
forced_values_by_index = Enum.reduce(fixed_cells, %{}, fn {{r, c}, v}, acc ->
  Map.put(acc, Map.fetch!(index_by_position, {r, c}), v)
end)

Map.get(assignments, {r, c}, 0)
```

**Elméleti háttér:**
- Hash-alapú implementáció (HAMT - Hash Array Mapped Trie)
- Elérés, beszúrás, törlés: O(log n) amortizált
- Immutábilis — módosítás új map-et hoz létre
- Strukturális megosztás a hatékonyság érdekében

#### 5.3.4. Range (`..`)

**Definíció:** Egész számok sorozatát reprezentáló lusta adatszerkezet.

**Szintaxis:**
```elixir
első..utolsó       # növekvő sorrend
első..utolsó//lépés # lépésköz megadása
```

**A kódban:**
```elixir
for c <- left..right, do: {top, c}
for c <- (right - 1)..left//-1, do: {bottom, c}  # csökkenő
0..(total_cells - 1)
```

**Elméleti háttér:**
- Nem materializálódik listává automatikusan
- Memóriahatékony nagy tartományoknál
- Az Enumerable protokollt implementálja

### 5.4. Vezérlési szerkezetek

#### 5.4.1. `case`

**Definíció:** Mintaillesztéses elágazás.

**Szintaxis:**
```elixir
case kifejezés do
  minta1 -> eredmény1
  minta2 when guard -> eredmény2
  _ -> alapértelmezett
end
```

**A kódban:**
```elixir
case sd do
  {board_size, cycle_length, fixed_cells} when is_integer(board_size) and board_size > 0 ... ->
    # feldolgozás
  _ -> []
end
```

**Elméleti háttér:**
- Mintaillesztés a kifejezés értékén
- Az első illeszkedő ág hajtódik végre
- A `_` bármire illeszkedik (wildcard)

#### 5.4.2. `if` / `else`

**Definíció:** Feltételes elágazás logikai érték alapján.

**Szintaxis:**
```elixir
if feltétel do
  igaz_ág
else
  hamis_ág
end
```

**A kódban:**
```elixir
if ok, do: :ok, else: {:error, :invalid_constraints}

if not can_place do
  solutions_acc
else
  # folytatás
end
```

**Elméleti háttér:**
- Csak `false` és `nil` számít hamisnak
- Minden más érték igaznak minősül
- Rövidített forma: `if felt, do: x, else: y`

#### 5.4.3. `with`

**Definíció:** Feltételek láncolása korai kilépéssel.

**Szintaxis:**
```elixir
with minta1 <- kifejezés1,
     minta2 <- kifejezés2 do
  sikeres_ág
else
  hiba_minta -> hiba_kezelés
end
```

**A kódban:**
```elixir
with :ok <- validate_constraints(board_size, cycle_length, fixed_cells) do
  # sikeres validáció utáni feldolgozás
end
```

**Elméleti háttér:**
- Ha bármelyik minta nem illeszkedik, a `with` azonnal visszatér az értékkel
- Ideális hibakezelési láncolatokhoz
- Kiváltja a mélyen egymásba ágyazott `case`-eket

#### 5.4.4. `for` (list comprehension)

**Definíció:** Lista-generáló kifejezés iterációval és szűréssel.

**Szintaxis:**
```elixir
for minta <- gyűjtemény, szűrő, do: kifejezés
```

**A kódban:**
```elixir
top_row = for c <- left..right, do: {top, c}
right_col = if top < bottom, do: (for r <- (top + 1)..bottom, do: {r, right}), else: []

for r <- 1..n do
  for c <- 1..n do
    Map.get(assignments, {r, c}, 0)
  end
end
```

**Elméleti háttér:**
- Funkcionális iteráció listákon, range-eken, egyéb Enumerable-eken
- Több generátor kombinálható (Descartes-szorzat)
- A szűrők (`when` vagy boolean kifejezések) kiszűrik az elemeket

### 5.5. Operátorok

#### 5.5.1. Pipe operátor (`|>`)

**Definíció:** Az előző kifejezés eredményét az utána álló függvény első paramétereként adja át.

**Szintaxis:**
```elixir
kifejezés |> függvény(további_paraméterek)
# Egyenértékű: függvény(kifejezés, további_paraméterek)
```

**A kódban:**
```elixir
spiral_row_index_t = spiral_positions |> Enum.map(fn {r, _} -> r - 1 end) |> List.to_tuple()
row_suffix = row_acc |> Enum.reverse() |> List.to_tuple()
index_by_position = spiral_positions |> Enum.with_index() |> Map.new()
```

**Elméleti háttér:**
- Balról jobbra olvasható adatfolyam
- Elkerüli a mélyen egymásba ágyazott függvényhívásokat
- Az Elixir egyik legjellemzőbb idiómája

#### 5.5.2. Bitműveletek (`&&&`, `|||`, `<<<`)

**Definíció:** Bitszintű operátorok egész számokon.

**Szintaxis:**
```elixir
a &&& b   # bitwise AND
a ||| b   # bitwise OR
a <<< b   # left shift
```

**A kódban:**
```elixir
import Bitwise

(elem(row_used_value_masks_t, row_idx0) &&& mask) == 0
put_elem(mask_tuple, idx, elem(mask_tuple, idx) ||| mask)
1 <<< (v - 1)
```

**Elméleti háttér:**
- A `Bitwise` modul importálása szükséges
- Az `&&&` és `|||` nem rövidzárak (mindkét operandust kiértékeli)
- Hatékony set-reprezentációhoz használható

#### 5.5.3. Összehasonlító operátorok

**Definíció:** Értékek összehasonlítása.

**Szintaxis:**
```elixir
a == b   # egyenlőség
a != b   # különbözőség
a < b    # kisebb
a <= b   # kisebb-egyenlő
```

**A kódban:**
```elixir
idx == total_cells
placed_values_count == total_required_nonzeros
elem(row_placed_count_t, row_idx0) < cycle_len
remaining_positions >= remaining_needed
```

**Elméleti háttér:**
- Strukturális összehasonlítás (nem referencia-egyenlőség)
- Különböző típusú értékek is összehasonlíthatók (definiált sorrend)

### 5.6. Modulok és függvénykönyvtárak

#### 5.6.1. `import`

**Definíció:** Egy modul függvényeit beemeli az aktuális névtérbe.

**Szintaxis:**
```elixir
import ModulNév
import ModulNév, only: [függvény: aritás]
```

**A kódban:**
```elixir
import Bitwise
```

**Elméleti háttér:**
- Az importált függvények minősítés nélkül hívhatók
- Az `only:` opció szelektíven importál
- A `Bitwise` modul az `&&&`, `|||`, `<<<` operátorokat biztosítja

#### 5.6.2. `Enum` modul

**Definíció:** Gyűjtemények (Enumerable) feldolgozására szolgáló függvénykönyvtár.

**Főbb függvények a kódban:**
```elixir
Enum.all?(collection, predicate)      # minden elem teljesíti-e
Enum.reduce(collection, acc, fun)     # összehajtás
Enum.map(collection, fun)             # transzformáció
Enum.reverse(collection)              # megfordítás
Enum.with_index(collection)           # indexekkel párosít
Enum.to_list(range)                   # listává alakít
```

**A kódban:**
```elixir
Enum.all?(constraints, fn {{r, c}, v} when ... -> ... end)
Enum.reduce(Enum.reverse(positions), {[zero_row], [zero_row]}, fn ... -> ... end)
spiral_positions |> Enum.map(fn {r, _} -> r - 1 end)
```

**Elméleti háttér:**
- Eager (mohó) kiértékelés — azonnal feldolgozza az egész gyűjteményt
- Az Enumerable protokollt implementáló bármely típussal működik
- A `Stream` modul a lusta (lazy) megfelelője

#### 5.6.3. `Map` modul

**Definíció:** Map adatszerkezet kezelésére szolgáló függvénykönyvtár.

**Főbb függvények a kódban:**
```elixir
Map.new(enumerable)           # új map létrehozása
Map.get(map, key, default)    # érték lekérése alapértelmezetttel
Map.put(map, key, value)      # új map módosított kulccsal
Map.fetch!(map, key)          # érték lekérése (hiba ha nincs)
```

**A kódban:**
```elixir
index_by_position = spiral_positions |> Enum.with_index() |> Map.new()
Map.get(forced_map, i, 0)
Map.put(acc, Map.fetch!(index_by_position, {r, c}), v)
```

#### 5.6.4. `List` modul

**Definíció:** Lista-specifikus műveletek.

**Főbb függvények a kódban:**
```elixir
List.to_tuple(list)  # lista → tuple konverzió
```

**A kódban:**
```elixir
spiral_positions_t = List.to_tuple(spiral_positions)
List.to_tuple(forced_values_list)
```

#### 5.6.5. `Integer` modul

**Definíció:** Egész számokkal kapcsolatos műveletek.

**A kódban:**
```elixir
Integer.parse(val)  # string → integer konverzió
```

```elixir
case Integer.parse(val) do
  {num, _} when num >= 0 -> num
  _ -> @alignment_window
end
```

#### 5.6.6. `System` modul

**Definíció:** Rendszerszintű műveletek.

**A kódban:**
```elixir
System.get_env("HELIX_ALIGN_WIN")  # környezeti változó lekérése
```

### 5.7. Anonim függvények

#### 5.7.1. `fn ... end` szintaxis

**Definíció:** Anonim (névtelen) függvény definiálása.

**Szintaxis:**
```elixir
fn param1, param2 -> eredmény end
fn
  minta1 -> eredmény1
  minta2 -> eredmény2
end
```

**A kódban:**
```elixir
Enum.all?(constraints, fn
  {{r, c}, v} when is_integer(r) and is_integer(c) and is_integer(v) ->
    1 <= r and r <= n and 1 <= c and c <= n and 1 <= v and v <= m
  _ -> false
end)

Enum.reduce(fixed_cells, %{}, fn {{r, c}, v}, acc ->
  Map.put(acc, Map.fetch!(index_by_position, {r, c}), v)
end)
```

**Elméleti háttér:**
- Első osztályú értékek — változóban tárolhatók, paraméterként átadhatók
- Closure-ök — bezárják a környező scope változóit
- Több klóz is definiálható mintaillesztéssel

#### 5.7.2. Capture operátor (`&`)

**Definíció:** Rövidített szintaxis névtelen függvények és függvény-referenciák létrehozására.

**Szintaxis:**
```elixir
&Modul.függvény/aritás   # függvény-referencia
&(&1 + &2)               # rövidített anonim függvény
```

**Elméleti háttér:**
- `&1`, `&2`, ... a paraméterekre hivatkoznak
- Átláthatóbb mint a `fn x, y -> x + y end` egysoros esetekben

### 5.8. Rekurzió és akkumulátorok

#### 5.8.1. Tail rekurzió

**Definíció:** Rekurzió, ahol a rekurzív hívás az utolsó művelet a függvényben.

**A kódban:**
```elixir
defp build_spiral_layers(top, left, bottom, right, acc) when top > bottom or left > right, do: acc
defp build_spiral_layers(top, left, bottom, right, acc) do
  # ... számítások ...
  acc2 = acc ++ top_row ++ right_col ++ bottom_row ++ left_col
  build_spiral_layers(top + 1, left + 1, bottom - 1, right - 1, acc2)  # tail call
end
```

**Elméleti háttér:**
- Az BEAM VM optimalizálja: nem növeli a stack-et
- Az akkumulátor (`acc`) hordozza az eddig összegyűjtött eredményt
- Funkcionális programozásban a ciklust helyettesíti

#### 5.8.2. `Enum.reduce/3` mint rekurzió

**Definíció:** Összehajtás (fold) művelet, amely egy akkumulátort épít fel iteratívan.

**Szintaxis:**
```elixir
Enum.reduce(collection, initial_acc, fn elem, acc -> new_acc end)
```

**A kódban:**
```elixir
Enum.reduce(Enum.reverse(positions), {[zero_row], [zero_row]}, fn {r, c}, {rl, cl} ->
  prev_row = hd(rl)
  prev_col = hd(cl)
  # ... számítások ...
  {[new_row | rl], [new_col | cl]}
end)
```

**Elméleti háttér:**
- Ekvivalens a rekurzív akkumulátor-mintával
- Gyakori művelet: összegzés, gyűjtés, transzformáció
- Az `Enum.map_reduce/3` kombinált map+reduce

### 5.9. Speciális értékek és atomok

#### 5.9.1. Atomok

**Definíció:** Konstans értékek, amelyek neve egyben az értékük is.

**Szintaxis:**
```elixir
:atom_nev
:ok
:error
true   # = :true
false  # = :false
nil    # = :nil
```

**A kódban:**
```elixir
:ok
{:error, :invalid_constraints}
```

**Elméleti háttér:**
- Globálisan egyediek — nincs két azonos nevű, különböző atom
- Konstans összehasonlítás: O(1)
- A boolean és nil értékek is atomok
- Gyakran return értékként vagy tag-ként használatosak

#### 5.9.2. Tuple-k mint visszatérési értékek

**Konvenció:**
```elixir
{:ok, érték}       # sikeres művelet
{:error, ok}       # hiba
:ok                # sikeres művelet érték nélkül
```

**A kódban:**
```elixir
if ok, do: :ok, else: {:error, :invalid_constraints}
```

### 5.10. Dokumentáció

#### 5.10.1. `@doc`

**Definíció:** Függvény-dokumentációs attribútum.

**Szintaxis:**
```elixir
@doc """
Függvény leírása.

## Paraméterek
  - param1: leírás

## Visszatérési érték
  Leírás
"""
def függvény(param1) do
  # ...
end
```

**A kódban:**
```elixir
@doc """
A helix/1 a számtekercs feladvány összes megoldását állítja elő.

Bemenet: `{n, m, megszorítások}`, ahol `megszorítások :: [{{r,c}, v}]` ...
Kimenet: a lehetséges táblák listája ...
"""
@spec helix(sd :: puzzle_desc()) :: ss :: solutions()
def helix(sd) do
  # ...
end
```

**Elméleti háttér:**
- Markdown formátumot támogat
- `iex> h Nhf1.helix` paranccsal megtekinthető
- ExDoc HTML dokumentációt generál belőle

---

## 6. Futtatás és benchmark

### 6.1. Alapvető futtatás

A workspace gyökeréből vagy a `Nhf1` mappából futtatható:

```bash
elixir Nhf1/nhf1.ex
```

Ez lefuttatja a fájl végén lévő teszteseteket és kiírja az eredményeket.

### 6.2. Interaktív használat (IEx)

```bash
iex -S mix
# vagy
iex Nhf1/nhf1.ex
```

```elixir
iex> Nhf1.helix({6, 3, [{{1,5},2}, {{2,2},1}, {{4,6},1}]})
[[[1,0,0,0,2,3],[0,1,2,3,0,0],[0,3,1,2,0,0],[0,2,3,0,0,1],[3,0,0,0,1,2],[2,0,0,1,3,0]]]
```

### 6.3. Benchee benchmark

A mellékelt benchmark szkript használata:

```bash
elixir Nhf1/bench.exs
```

**Szűrés** `BENCH_FILTER` környezeti változóval:

```bash
# Linux/macOS
BENCH_FILTER="tc1[01]" elixir Nhf1/bench.exs   # tc10 és tc11
BENCH_FILTER="8x8" elixir Nhf1/bench.exs       # 8×8-as esetek

# PowerShell
$env:BENCH_FILTER = "tc1[01]"; elixir Nhf1/bench.exs
```

### 6.4. Alignment ablak hangolása

```bash
# Linux/macOS
HELIX_ALIGN_WIN=0 elixir Nhf1/bench.exs     # kikapcsolva
HELIX_ALIGN_WIN=256 elixir Nhf1/bench.exs   # nagyobb ablak

# PowerShell
$env:HELIX_ALIGN_WIN = "0"; elixir Nhf1/bench.exs
```

### 6.5. Teljesítmény-pillanatkép

Példamérések a teszteseteken:

| Teszteset | Méret | m | Idő | Memória |
|-----------|-------|---|-----|---------|
| tc10 | 8×8 | 4 | ~0,94 s | ~401 MB |
| tc11 | 9×9 | 3 | ~0,17 s | ~59 MB |
| tc9 | 8×8 | 3 | ~0,056 s | ~23 MB |
| tc5 | 6×6 | 3 | ~1,0–1,1 ms | ~0,38 MB |

A jó teljesítmény forrásai:
- Duplikációmentes spirálgenerálás
- Akkumulátoros DFS; táblák csak levélszinten materializálódnak
- Bitmaszk alapú sor/oszlop ellenőrzés (O(1))
- Többlépcsős metszés

---

## 7. További optimalizációs ötletek

### 7.1. Erősebb alignment lookahead
Több közeljövőbeli kényszerre kiterjeszteni a vizsgálatot, vagy szigorúbb kongruencia-korlátokat fenntartani.

### 7.2. Ágképzési heurisztikák
Preferálni a PLACE ágat, ha egy sor/oszlop közel jár a kvótához — gyorsabban érünk zsákutcába.

### 7.3. Adatszerkezet finomhangolás
Speciális bitkészletek/primitívek kipróbálása; ahol dominál a véletlen hozzáférés, maradjanak a tuple-ök.

### 7.4. Könnyűsúlyú memoizáció
Csak kompakt állapotprojekciókat cache-elni, pl. `(idx, row_mask[row], col_mask[col], placed_count)`.

### 7.5. Koraibb konfliktusdetektálás
Ha egy sor/oszlop elérte az m kvótát, ellenőrizni, hogy nem maradt-e ugyanazon a vonalon ellentétes kényszer.

### 7.6. Párhuzamosítás
A DFS fa felső szintjeinek szétosztása független feladatokra (`Task.async_stream`).

### 7.7. Mikro-optimalizációk
- Hot path maszkok inline-olása
- Ideiglenes allokációk csökkentése
- Gyakori tuple-ök lokális változóban tartása

---

## Megjegyzések

- A kód a `Nhf1/nhf1.ex` fájlban, a benchmark a `Nhf1/bench.exs`-ben van
- A feladatleírás a `Nhf1/feladat.txt` fájlban található
- Az `@alignment_window` modul-attribútum alapértelmezése 64
- A megoldás Elixir 1.18 és Erlang/OTP 28 verzióval készült
