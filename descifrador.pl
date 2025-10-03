
min(A, B, A):- A =< B,!.
min(A, B, B):- A >= B,!.

max(A, B, A):- A >= B, !.
max(A, B, B):- A =< B, !.

logaritmo(X, Res):- Res is log(X)/log(2).


generarEspacioMuestral(_, 0, []):-!.
generarEspacioMuestral(Colores, Espacios, [ColorSeleccionado|Universo]):- member(ColorSeleccionado, Colores), EspaciosRestantes is Espacios - 1,
                                                        generarEspacioMuestral(Colores, EspaciosRestantes, Universo).  
conjuntoUniverso(Colores, Espacios, Universo):-findall(Combinacion, generarEspacioMuestral(Colores, Espacios, Combinacion), Universo).


cantidadAciertos([],[],0):-!.
cantidadAciertos([GuessH|GuessT], [ElemH|ElemT], Aciertos):- GuessH = ElemH, cantidadAciertos(GuessT, ElemT, AciertosNuevo),
                                                             Aciertos is AciertosNuevo + 1.
cantidadAciertos([GuessH|GuessT], [ElemH|ElemT], Aciertos):- GuessH \= ElemH, cantidadAciertos(GuessT, ElemT, Aciertos).


coincidencias([], _, 0):-!.
coincidencias([H|T], H, Cantidad):- coincidencias(T, H, CantidadNueva), !, Cantidad is CantidadNueva + 1.
coincidencias([_H|T], X, Cantidad):- coincidencias(T, X, Cantidad), !.




minimoIncognitas([], _, 0):-!.
minimoIncognitas([GuessHead|GuessTail], ElementoUniverso, Minimo):- eliminarRepeticiones(GuessHead, GuessTail, GuessRest),
                                                                    minimoIncognitas(GuessRest, ElementoUniverso, NuevoMinimo),
                                                                    coincidencias([GuessHead|GuessTail], GuessHead, A),
                                                                    coincidencias(ElementoUniverso, GuessHead, B), min(A,B, MinimoParcial), Minimo is NuevoMinimo + MinimoParcial. 

eliminarRepeticiones(_E, [], []):-!.
eliminarRepeticiones(E,[E|T],R):- eliminarRepeticiones(E, T, R),!.
eliminarRepeticiones(E, [H|T],[H|R]):- eliminarRepeticiones(E, T, R),!.

feedback(Guess, ElementoUniverso, [Aciertos, Incognitas]):- minimoIncognitas(Guess, ElementoUniverso, Minimo), 
                                                 cantidadAciertos(Guess, ElementoUniverso, Aciertos), 
                                                 Incognitas is Minimo - Aciertos, !.


contarRepetidos(Feedback, Res):- sort(Feedback, Unicos), findall([Par, Cantidad],
                                 (member(Par, Unicos), include(=(Par), Feedback, Filtrado),
                                 length(Filtrado, Cantidad)), Res).


calcularEntropiaAux([], _ , 0):-!.
calcularEntropiaAux([[_, Cantidad]|T], CardinalidadGuesses, Entropia):- calcularEntropiaAux(T, CardinalidadGuesses, EntropiaNueva), 
                                                                       ProbabilidadFeedback is Cantidad / CardinalidadGuesses,
                                                                       logaritmo(ProbabilidadFeedback, Log),
                                                                       Argumento is ProbabilidadFeedback * Log,
                                                                       Entropia is EntropiaNueva + Argumento.

calcularEntropia(Frecuencias, CardinalidadGuesses, Entropia):- calcularEntropiaAux(Frecuencias, CardinalidadGuesses, EntropiaParcial), Entropia is EntropiaParcial * (-1).

feedbackTotal(ElementoUniverso, Guesses, FeedbackTotal):- findall(Feedback, (member(Guess, Guesses), feedback(Guess, ElementoUniverso, Feedback)), FeedbackTotal).


entropiasAux([], _, _, [0, []]):-!.
entropiasAux([ElementoUniverso|R], Guesses, CardinalidadGuesses,[EntropiaMaxima, ElementoMaximo]):- entropiasAux(R, Guesses, CardinalidadGuesses, [EntropiaActual, ElementoActual]), feedbackTotal(ElementoUniverso, Guesses, FeedbackTotal),
                                                                             contarRepetidos(FeedbackTotal, Frecuencias),
                                                                             calcularEntropia(Frecuencias, CardinalidadGuesses, Entropia),
(Entropia >= EntropiaActual -> EntropiaMaxima is Entropia, ElementoMaximo = ElementoUniverso ; EntropiaMaxima is EntropiaActual, ElementoMaximo = ElementoActual).



