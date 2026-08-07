# STAIR 046 - Constrained Mode

## Ultimo commit

`STAIR 046 - Constrained Mode`

## Stato attuale

### Geometria

- Geometry Engine stabile
- NONE
- SQUARE
- ROUND
- UCS Independent
- Preview corretta
- Accept promuove la preview a geometria finale

### Tread

- ERGONOMIC
- FIXEDTREAD
- CONSTRAINED

Modalità disponibili nel submenu:

`Tread [Value/Ergonomic/Constrained/Accept]`

### Nosing

- NONE
- SQUARE
- ROUND

Menu:

`Nosing [None/Square/Round/Cancel]`

### Report

- Preview report
- Final report
- MTEXT report

Contenuto:

- Height = xx
- Run = xx
- x risers of xxx
- x treads of xxx
- 2R+T = xx
- Angle = xx°

### MTEXT

- Posizione automatica
- UCS aligned
- Height = rise/3
- Ricorda preferenza Yes/No
- Inserimento sopra ultimo gradino
- Sollevato di: rise/2 + 6 * txtHeight + rise

## Menu principale

`[+/-/Tread/Nosing/Accept/Exit] <Accept>`

Initget:

`(initget "+ - Tread Nosing Accept Exit")`

## Constrained

Implementato.

Formula:

`tread = totalRun / (risers - 1)`

dove:

`totalRun = abs(ep.x - bp.x)`

memorizzato in:

`*stair-total-run*`

Nuova modalità:

`CONSTRAINED`

## TODO immediato

### Bug noto

Nel submenu Tread.

Caso:

`Value`

visualizza:

`Tread value <0.30>`

ma se premi ENTER non usa il valore di default.

Logica desiderata:

ENTER = ultimo valore memorizzato.

Correzione prevista:

`(if (null tv) (setq tv *stair-fixed-tread*))`

prima della validazione.

## Changelog

### 042B-Part5A

- State infrastructure
- Recompute
- Refresh preview

### 042B-Part5C

- Nosing submenu
- NONE
- SQUARE
- ROUND

### 043

- UCS Independent

### 044

- Final command-line report
- Tread submenu
- Fixed tread mode

### 045

- MTEXT Final Report
- UCS aligned
- Remember Yes/No preference

### 046

- Constrained Mode
- Run constrained to picked points
- Tread auto-calculated
- Constrained moved into Tread submenu

## Idee future (non prioritarie)

### 047 Hardening

- error handler più robusto
- pulizia debug
- centralizzazione descrizioni tread/nosing
- centralizzazione report

### Verifiche utili

- UCS ruotato 90°
- UCS con asse invertito
- scale molto piccole
- scale molto grandi
- tread count = 1 (protezione)

## Nota per la prossima sessione

Partire da:

> Correggere il default della voce Value nel submenu Tread.

Per il resto STAIR è in una versione utilizzabile in produzione.
