042A
Geometry Prototype

042B Part 1
Geometry Engine

042B Part 2
Nosing Clamp

042B Part 3A
INSUNITS Constants

042B Part 3B
Height
Proposed Risers
Ergonomic Tread

042B Part 4
Base Point
Arrival Point
Preview

042B Part 5
Correction Loop
[E/F/+/-/N/A/C]

042B Part 6
Accept
Report

042C
MTEXT

043
UCS Independent

STAIR 046 - Constrained Mode
Ultimo commit
STAIR 046 - Constrained Mode

Stato attuale
Geometria
✅ Geometry Engine stabile
✅ NONE
✅ SQUARE
✅ ROUND
✅ UCS Independent
✅ Preview corretta
✅ Accept promuove la preview a geometria finale
Tread
✅ ERGONOMIC
✅ FIXEDTREAD
✅ CONSTRAINED
Modalità disponibili nel submenu:
Tread [Value/Ergonomic/Constrained/Accept]

Nosing
✅ NONE
✅ SQUARE
✅ ROUND
Menu:
Nosing [None/Square/Round/Cancel]

Report
✅ Preview report
✅ Final report
✅ MTEXT report
Contenuto:
Height = xx

Run = xx

x risers of xxx

x treads of xxx

2R+T = xx

Angle = xx°

MTEXT
✅ Posizione automatica
✅ UCS aligned
✅ Height = rise/3
✅ Ricorda preferenza Yes/No
✅ Inserimento sopra ultimo gradino
✅ Sollevato di:
rise/2
+ 6 * txtHeight
+ rise

Menu principale
Attuale:
[+/-/Tread/Nosing/Accept/Exit] <Accept>

Initget:
Lisp
1
(initget "+ - Tread Nosing Accept Exit")
2
``
Mostra più linee
Constrained
Implementato.
Formula:
tread =
  totalRun
  /
  (risers - 1)

dove:
totalRun =
  abs(ep.x - bp.x)

memorizzato in:
Lisp
1
*stair-total-run*
Mostra più linee
Nuova modalità:
Lisp
1
"CONSTRAINED"
Mostra più linee
TODO immediato
Bug noto
Nel submenu Tread.
Caso:
Value

visualizza:
Tread value <0.30>

ma se premi:
ENTER

non usa il valore di default.
Da correggere.
Logica desiderata:
Plain Text
1
ENTER
2
=
3
ultimo valore memorizzato
Mostra più linee
quindi:
Lisp
1
(if (null tv)
2
 
3
(setq tv *stair-fixed-tread*)
4
)
Mostra più linee
prima della validazione.
Changelog completo
042B-Part5A
State infrastructure
Recompute
Refresh preview
042B-Part5C
Nosing submenu
NONE
SQUARE
ROUND
043
UCS Independent
044
Final command-line report
Tread submenu
Fixed tread mode
045
MTEXT Final Report
UCS aligned
Remember Yes/No preference
046
Constrained Mode
Run constrained to picked points
Tread auto-calculated
Constrained moved into Tread submenu
Idee future (non prioritarie)
047 Hardening
error handler più robusto
pulizia debug
centralizzazione descrizioni tread/nosing
centralizzazione report
Possibili verifiche
UCS ruotato 90°
UCS con asse invertito
scale molto piccole
scale molto grandi
tread count = 1 (protezione)
Nota personale
La prossima volta partirei da:
Correggere il default della voce "Value" nel submenu Tread
perché è l'unico difetto funzionale ancora noto.
Per il resto STAIR è ormai in una versione che considero utilizzabile in produzione. 🚀
Fornisci il tuo feedback su BizChat