Questo README si riferisce al programma "quant.pl", un'implementazione
in Prolog delle librerie che contengono alcune operazioni standard per la manipolazione delle
varie quantità ed unità standard del "International System of Units".

--------------------------------------------------------------------------------------------

Documentazione:

*   Pagina Wikipedia : https://en.wikipedia.org/wiki/International_System_of_Units

--------------------------------------------------------------------------------------------

Il programma è composto dai seguenti predicati (commentati all'interno del file "quant.pl"):

//Predicati principali:

* is_siu/1

* is_base_siu/1

* siu_name/2

* siu_symbol/2

* siu_base_expansion/2

* is_dimension/1

* is_quantity/1

* cmp_units/3

* normalize/2

* qplus/3

* qsubtract/3

* qtimes/3

* qdivide/3

* qexpt/3

* q/2

//Predicati secondari e di utilità:

* is_temperature/2 : Utilizzato per il controllo delle unita di tipo
                     "temperatura" ed eventuali conversioni di queste

* loop_submult_to_base/2 : Utilizzato per controllare e convertire
                     sottomultipli in unità base
* check_if_submultiple_rep/1 : Utilizzato come supporto al predicato
                     "loop_submult_to_base/1"

* check_if_submultiple_in_norm/2 :  Utilizzato per controllare e 
                     convertire sottomultipli in unità base
                     durante il procedimento di normalizzazione

* check_if_submultiple/2 : Utilizzato per la conversione di Quantità
                     conteneti sottomultipli in Quantità con unità 
                     base

* indexOf/3 : Utilizzato come supporto ai metodi di conversione da
                     sottomultipli a unità base.

* get_order_of_unit/2 : Utilizzato per ottenere il valore di "ordine"
                     dell'unità passata

* get_list/2 : Utilizzato per convertire una dimensione in lista

* convert_list_to_dimension/2 : Utilizzato per convertire una
                      lista di unità in una dimensione.

* list_to_dim/2 : Utilizzata come supporto a "convert_list_to_dimension/2"

* convert_and_get_list/2 : Converte una dimensione di unità derivate
                      in una lista di unità base.

* remove_exp/2 : Utilizzato per rimuovere l'esponente da un unità

* convert_in_base/2 : Utilizzato per convertire un un unità derivata
                      in una dimensione equivalente di sole unità
                      base

* normalize_dimension/2 : Utilizzato come supporto al procedimento
                      si normalizzazione 

* sort_dim_list/2 : Data una lista di unità, la riordina in base al
                      loro valore di "ordine"

* step_sort/3 : Utilizzato come supporto a "sort_dim_list/2"

* loop_invert_exp/2 : Utilizzato per ottenere gli opposti delle unità
                      elevate ad un numero

