# index
## survey
an autolisp routine that draws a triangle given three sides - to help with survey routines in acad

### IT
routine per semplificare l'immissioni di triangolazioni di rilievo il prodotto finale dovrebbe, date tre misure, verso, punto di inserimento e allineamento, tracciare un triangolo delle misure date nel punto e con l'allineamento dato
si rilascia con licenza LGPL v3.0 <http://www.opensource.org/licenses/lgpl-3.0.html> per cui, fra l'altro, non è possibile ricavarne un prodotto commerciale, e qualsiasi
derivato (modifiche, incorporamenti in altro codice) deve avere la medesima licenza

### EN
draws a triangle given three sides, insertion point and alignement, intended to help in the boring survey procedure.
Relased under the LGPL licence v3.0 <http://www.opensource.org/licenses/lgpl-3.0.html>
***********************************************************************************************

### IT
#### uso:
salvare il lisp in una dell cartelle di supporto di autocad caricare il programma all'interno di autocad con la procedura standard: Tools>AutoLisp>load application, o aggiungendola alla "Startup Syute".
Avviare il programma digitando "srv" (da SuRVey).
Il programma chiede anzitutto tre misure (si può inserirle con mouse), poi punto d'inserimento e direzione, infine chiede se il triangolo vada tracciato in senso antiorario (default, basta dare l'invio) oppure orario (premere C per "clockwise")

### EN
#### use:
start the command with "srv" (as in SuRVey)
The routine will ask you for three sides' measures (you can input them by mouse), then the insertion point, and the direction. Lastly it will ask you if you want to draw it counterclockwise (default, just press "enter") or Clockwise (press "C")

**
si rilascia anche un icona per la toolbar
il prodotto è rilasciato gratuitamente senza garanzie di alcuna sorta
the software is free but use it at your own risk

### to do
I'd like your help to make it **interactive** (visualize the triangle while placing it on the drawing and rotate and orient it). I'm not ready for that.

## acaddoc.lsp

adding this line of toce to your acaddoc.lsp file will show the current _insunits_ in the taskbar (converted to a readable format)
