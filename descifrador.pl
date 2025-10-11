
min(A, B, A):- A =< B,!.
min(A, B, B):- A >= B,!.

max(A, B, A):- A >= B, !.
max(A, B, B):- A =< B, !.

logaritmo(X, Res):- Res is log(X)/log(2).

random_int(N, R) :-
    random(X),              
    R is floor(X * (N+1)).  
                                                                                            
                                            
myRandom(N, R):- random(X), R is floor(X * (N + 1) + 1).

generarEspacioMuestral(_, 0, []):-!.
generarEspacioMuestral(Colores, Espacios, [ColorSeleccionado|Universo]):- member(ColorSeleccionado, Colores), EspaciosRestantes is Espacios - 1,
                                                        generarEspacioMuestral(Colores, EspaciosRestantes, Universo).  

conjuntoUniverso(Colores, Espacios, Universo):-findall(Combinacion, generarEspacioMuestral(Colores, Espacios, Combinacion), Universo).


cantidadAciertos([],[],0):-!.
cantidadAciertos([],_,0):-!.
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
%Entropia

calcularEntropiaAux([], _ , 0):-!.
calcularEntropiaAux([[_, Cantidad]|T], CardinalidadGuesses, Entropia):- calcularEntropiaAux(T, CardinalidadGuesses, EntropiaNueva), 
                                                                       ProbabilidadFeedback is Cantidad / CardinalidadGuesses,
                                                                       logaritmo(ProbabilidadFeedback, Log),
                                                                       Argumento is ProbabilidadFeedback * Log,
                                                                       Entropia is EntropiaNueva + Argumento.

calcularEntropia(Frecuencias, CardinalidadGuesses, Entropia):- calcularEntropiaAux(Frecuencias, CardinalidadGuesses, EntropiaParcial), Entropia is EntropiaParcial * (-1).

mapFeedback(ElementoUniverso, Guesses, FeedbackTotal):- findall(Feedback, (member(Guess, Guesses), feedback(Guess, ElementoUniverso, Feedback)), FeedbackTotal).

%agrupar([], 1, _,[]):-!.
%agrupar([],0,Elemento,[[Elemento,1]]):-!.
%agrupar([[Elemento, Cantidad]|T], _, Elemento, [[Elemento, CantidadNueva]|Agrupado]):- CantidadNueva is Cantidad + 1, agrupar(T, 1, Elemento, Agrupado),!.
%agrupar([[H, Cantidad]|T], Estado , Elemento, [[H, Cantidad]|Agrupado]):- agrupar(T, Estado, Elemento, Agrupado).


contarRepetidos(_, [], [], Acum, Acum) :- !.
contarRepetidos([A,B], [[A,B]|T], Resto, Acum, Total) :- NuevoAcum is Acum + 1,
                                                          contarRepetidos([A,B], T, Resto, NuevoAcum, Total), !.
contarRepetidos([A,B], [[X,Y]|T], [[X,Y]|T], Acum, Acum) :-
    (A \= X ; B \= Y), !.

agruparConsecutivos([], []) :- !.
agruparConsecutivos([[A,B]|T], [[[A,B],Total]|Agrupado]) :- contarRepetidos([A,B], T, Resto, 1, Total),
                                                             agruparConsecutivos(Resto, Agrupado).

agruparPares(Lista, Agrupada) :-
    msort(Lista, Ordenada),
    agruparConsecutivos(Ordenada, Agrupada).




comparacionFeedback(GuessParcial, Guess, 0, [AciertosGuess, IncognitasGuess]):- !, feedback(GuessParcial, Guess, [AciertosTotales, IncognitasTotales]), AciertosTotales =:= AciertosGuess, IncognitasGuess =:= IncognitasTotales,!.
comparacionFeedback(GuessParcial, Guess, EspaciosRestantes,[AciertosGuess, IncognitasGuess]):- feedback(GuessParcial, Guess, [AciertosParteCod, IncognitasParteCod]),
                                                                            AciertosParteCod =< AciertosGuess, IncognitasParteCod =< IncognitasGuess + EspaciosRestantes.
cumpleFiltros(_, [], _):-!.
cumpleFiltros(GuessParcial, Filtros, EspaciosRestantes) :-
    forall(member([GuessRealizado, GuessFeedback], Filtros),
           comparacionFeedback(GuessParcial, GuessRealizado, EspaciosRestantes, GuessFeedback)).

generarConjuntoSolucion(_, _, 0, GuessParcial, Solucion):- Solucion = GuessParcial,!. 
generarConjuntoSolucion(Filtros, Colores, Espacios, GuessParcial, Solucion):- member(ColorSeleccionado, Colores), 
                                                                            ParcialNuevo = [ColorSeleccionado|GuessParcial],
                                                                            EspaciosRestantes is Espacios - 1,
                                                                            cumpleFiltros(ParcialNuevo, Filtros, EspaciosRestantes),
                                                                            generarConjuntoSolucion(Filtros, Colores, EspaciosRestantes, ParcialNuevo, Solucion).
                                                                        


generarConjuntoAleatorio(0,_,_,_):-!.
generarConjuntoAleatorio(N, Colores, Espacios, Codigo):- between(1,N,_), generarCodigoAleatorio(Colores, Espacios, Codigo). 
                                        

generarCodigoAleatorio(_, 0, []):-!.
generarCodigoAleatorio(Colores, Espacios, [X|Codigo]):- ColoresLimit is Colores - 1, myRandom(ColoresLimit, X), EspaciosRestantes is Espacios - 1, 
                                                        generarCodigoAleatorio(Colores, EspaciosRestantes, Codigo).

listaFeedbacks(_, [], Acc, Feedbacks):- Feedbacks = Acc,!.
listaFeedbacks(ElementoUniverso, [PosibleSolucion| Restantes], _Acc, Feedbacks):-feedback(PosibleSolucion, ElementoUniverso, Feedback), 
                                                                                agruparPares(Feedback, Agrupado),
                                                                                listaFeedbacks(ElementoUniverso, Restantes, Agrupado, Feedbacks).
entropias([], _, _, [EntropiaMaxima, ElementoMaximo], [EntropiaMaxima, ElementoMaximo]):-!.
entropias([ElementoUniverso|R], ConjuntoSolucion, CardinalidadGuesses,[EntropiaMaxima, ElementoMaximo], Res):-  listaFeedbacks(ElementoUniverso, ConjuntoSolucion, [], Frecuencias),
                                                                             calcularEntropia(Frecuencias, CardinalidadGuesses, Entropia),
                                                                             (Entropia >= EntropiaMaxima -> EntropiaNueva is Entropia, ElementoNuevo = ElementoUniverso; EntropiaNueva is EntropiaMaxima, ElementoNuevo = ElementoMaximo),
                                                                            entropias(R, ConjuntoSolucion, CardinalidadGuesses, [EntropiaNueva, ElementoNuevo], Res).

primerosN(N, Filtros, Colores, Espacios, Values) :-
    primerosHelper(N, Colores, Espacios, Filtros,[], Values).

primerosHelper(0,_,_,_, Acc, Acc) :- !.
primerosHelper(N,Colores, Espacios, Filtros, Acc, Values) :-
    N > 0,
    generarConjuntoSolucion(Filtros, Colores, Espacios,[], Template), \+member(Template, Acc),
    N1 is N - 1,
    copy_term(Template, T),   
    primerosHelper(N1, Colores, Espacios, Filtros, [T|Acc], Values).


muestraAleatoriaSolucion(N, Filtros, Colores, Espacios, Muestra):- primerosN(N, Filtros, Colores, Espacios, Muestra).
muestraAleatoriaUniverso(N, Colores, Espacios, Muestra):- findall(Codigo, (generarConjuntoAleatorio(N, Colores, Espacios, Codigo)), Muestra).

conjuntoSolucion(Filtros, Colores, Espacios, Muestra):- findall(Solucion , (generarConjuntoSolucion(Filtros, Colores, Espacios, [], Solucion)), Muestra), length(Muestra, L),write(L).

siguienteGuessGeneral(Colores, Espacios, Filtros, Guess):-conjuntoSolucion(Filtros, Colores, Espacios, MuestraUniverso), length(MuestraUniverso, L),
                                                       entropias(MuestraUniverso, MuestraUniverso, L, [0,[]], Candidato), Candidato = [_E, Guess], write(Guess).

siguienteGuessAcotado(Colores, Espacios, Filtros, Guess):- muestraAleatoriaUniverso(N, Colores, Espacios, MuestraUniverso),
                            (\+muestraAleatoriaSolucion(N, Filtros, Colores, Espacios, MuestraParcial) -> conjuntoSolucion(Filtros, Colores, Espacios, MuestraCompleta), length(MuestraCompleta, L),
                                                                                                 entropias(MuestraUniverso, MuestraCompleta, L, [0,[]], Candidato),
                                                                                                 Candidato = [E, Guess], write(E)
                                                                                               ; length(MuestraParcial, L), entropias(MuestraUniverso, MuestraParcial, L, [0,[]], Candidato),
                                                                                                 Candidato = [E, Guess], write(E)
                            ).




generarGrupos([], _, []) :- !.
generarGrupos(Lista, Tam, [Grupo|Resto]) :- length(Grupo, Tam), append(Grupo, Sobrantes, Lista), !,
                                            generarGrupos(Sobrantes, Tam, Resto).
generarGrupos(Lista, _, [Lista]).  

tomarPrimeros(_, 0, []) :- !.
tomarPrimeros([], _, []) :- !.
tomarPrimeros([H|T], N, [H|T2]) :- N > 0,
                                   N1 is N - 1,
                                   tomarPrimeros(T, N1, T2).

eliminarPrimeros(L, 0, L) :- !.
eliminarPrimeros([], _, []) :- !.
eliminarPrimeros([_|T], N, Resto) :-
    N > 0,
    N1 is N - 1,
    eliminarPrimeros(T, N1, Resto).


completarGuess(Grupo, Todos, Tam, Guess) :- length(Grupo, L),
    ( L < Tam -> Faltan is Tam - L, tomarPrimeros(Todos, Faltan, Resto), append(Grupo, Resto, Guess)
    ; L > Tam -> tomarPrimeros(Grupo, Tam, Guess)
    ; Guess = Grupo ).

generarGuessesIniciales([], _, _, []) :- !.
generarGuessesIniciales(Colores, Todos, Tam, [Guess|Resto]) :- length(Colores, L),
        L =< Tam ->
        completarGuess(Colores, Todos, Tam, Guess),
        Resto = []
        ;
        tomarPrimeros(Colores, Tam, Guess),
        eliminarPrimeros(Colores, Tam, ColoresRestantes),
        generarGuessesIniciales(ColoresRestantes, Todos, Tam, Resto).




% Loop Principal

jugar(Colores, Tam) :- generarGuessesIniciales(Colores, Colores, Tam, GuessesIniciales),
                       write(GuessesIniciales), nl, % usar esto si format no sirve
                       jugarAux(GuessesIniciales, [], Colores, Tam).

jugarAux([], Filtros, Colores, Tam) :-  siguienteGuessGeneral(Colores, Filtros, Tam, GuessFinal),
                                          format('Guess: ~w~n', [GuessFinal]),
                                          pedirFeedbackYContinuar(GuessFinal, Filtros, Colores, Tam).

jugarAux([G|Guesses], Filtros, Colores, Tam) :- format('Guess: ~w~n', [G]),
                                                write('Ingrese feedback: '),nl,
                                                read(Feedback),
                                                NuevosFiltros = [[G, Feedback]|Filtros],
                                                write(NuevosFiltros),nl,
                                                jugarAux(Guesses, NuevosFiltros, Colores, Tam).

pedirFeedbackYContinuar(Guess, Filtros, Colores, Tam) :- write('Ingrese feedback: '),nl,
                                                         read(Feedback),
                                                         NuevosFiltros = [[Guess, Feedback]|Filtros],
                                                         siguienteGuessGeneral(Colores, NuevosFiltros, Tam, Siguiente),
                                                         format('Guess: ~w~n', [Siguiente]),
                                                         write(NuevosFiltros),nl,
                                                         pedirFeedbackYContinuar(Siguiente, NuevosFiltros, Colores, Tam).

