imp-compiler_int.ml
  Compilatore funzionante che gestisce gli interi nella maniera classica 
	(limitata alla dimensione del registro). 
  
imp-compiler.ml
  Compilatore funzionante che gestisce i bigint.
  I bigint vengono rappresentati come una lista linkata.
  

Il programma fibonacci e' presente in entrambi i file, in sintassi astratta.
Abbiamo inoltre realizzato una funzione
	compile f p
Che dato il programma p, genera il codice asm nel file f.
