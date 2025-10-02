


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


min(A, B, A):- A =< B,!.
min(A, B, B):- A >= B,!.

minimoIncognitas([], _, 0):-!.
minimoIncognitas([GuessHead|GuessTail], ElementoUniverso, Minimo):- minimoIncognitas(GuessTail, ElementoUniverso, NuevoMinimo),
                                                                    coincidencias([GuessHead|GuessTail], GuessHead, A),
                                                                    coincidencias(ElementoUniverso, GuessHead, B), min(A,B, MinimoParcial), Minimo is NuevoMinimo + MinimoParcial. 

%hace falta el quitar en el que se mueve.