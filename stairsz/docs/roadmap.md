# STAIR 046A - Production Ready

## Ultimo commit

`STAIR 046A - Production Ready`

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

Modalità disponibili:

- ERGONOMIC
- FIXEDTREAD
- FIT

Menu:

`Tread [Value/Ergonomic/Fit/Accept]`

### Nosing

Modalità disponibili:

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
- Height = rise / 3
- Ricorda preferenza Yes/No
- Inserimento sopra ultimo gradino
- Offset automatico:

`rise/2 + 6 * txtHeight + rise`

## Menu principale

`[+/-/Tread/Nosing/Accept/Exit] <Accept>`

Initget:

```lisp
(initget "+ - Tread Nosing Accept Exit")
```

## FIT Mode

Implementato.

Formula:

`tread = totalRun / (risers - 1)`

dove:

`totalRun = abs(ep.x - bp.x)`

memorizzato in:

```lisp
*stair-total-run*
```

La distanza orizzontale tra Base Point e Arrival Point viene considerata vincolante.

Il tread viene ricalcolato automaticamente per fare coincidere l'ultimo gradino con il punto di arrivo.

## Comportamento del comando

### Value

Permette di impostare un valore fisso del tread.

Prompt:

`Tread value <last value>:`

Premendo ENTER viene riutilizzato l'ultimo valore memorizzato.

### Ergonomic

Calcolo automatico secondo la formula:

`2R + T = 63 cm`

(convertita automaticamente nelle unità correnti del disegno).

### Fit

Calcolo automatico del tread in funzione della distanza orizzontale tra il punto di partenza e il punto di arrivo.

Formula:

`tread = totalRun / treadCount`

dove:

`treadCount = risers - 1`

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
- Fixed Tread mode

### 045

- MTEXT Final Report
- UCS aligned
- Remember Yes/No preference

### 046

- Fit Mode
- Run constrained to picked points
- Tread auto-calculated
- Fit option added to Tread submenu

### 046A

- Production cleanup
- Debug output removed
- Preview report updated for FIT mode
- Internal-only helper functions
- STAIRINFO removed
- STAIRCALC removed

## Comandi pubblici

`STAIR`

## Verifiche completate

- UCS World
- UCS ruotato
- Direzione sinistra/destra
- NONE
- SQUARE
- ROUND
- ERGONOMIC
- FIXEDTREAD
- FIT
- Preview update
- Accept
- MTEXT UCS aligned
- INSUNITS conversion

## Roadmap futura

### 047 Hardening

- Error handler più robusto
- Centralizzazione descrizioni tread
- Centralizzazione descrizioni nosing
- Centralizzazione report
- Protezione divisione per zero in FIT
- Variabile globale `*stair-debug*`
- Pulizia commenti storici

### Test consigliati

- UCS ruotato 90°
- UCS con asse invertito
- Scale molto piccole
- Scale molto grandi
- Tread count = 1
- FIT con run molto ridotto
- FIT con run molto elevato

## Nota per la prossima sessione

Verificare e correggere definitivamente il comportamento di default della voce:

`Tread > Value`

per garantire che ENTER utilizzi sempre l'ultimo valore memorizzato.

Per il resto STAIR è attualmente utilizzabile in produzione.