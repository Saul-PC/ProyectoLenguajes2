min(A, B, A):- A =< B,!.
min(A, B, B):- A >= B,!.

max(A, B, A):- A >= B, !.
max(A, B, B):- A =< B, !.

logaritmo(X, Res):- Res is log(X)/log(2).

random_int(N, R) :-
    random(X),              
    R is floor(X * (N+1)).  
                                                                                            

myRandom(N, R):- random(X), R is floor(X * (N + 1) + 1).

cantidadAciertos([],[],Acumulado, Acumulado):-!.
cantidadAciertos([],_, Acumulado, Acumulado):-!.
cantidadAciertos([GuessH|GuessT], [ElemH|ElemT], Acumulado, Aciertos):- GuessH = ElemH, AcumuladoNuevo is Acumulado + 1, cantidadAciertos(GuessT, ElemT, AcumuladoNuevo, Aciertos).
cantidadAciertos([GuessH|GuessT], [ElemH|ElemT], Acumulado, Aciertos):- GuessH \= ElemH, cantidadAciertos(GuessT, ElemT, Acumulado, Aciertos).




coincidencias([],_, Acumulado, Acumulado):-!.
coincidencias([H|T], H, Acumulado, Cantidad):- !,AcumuladoNuevo is Acumulado + 1, coincidencias(T, H, AcumuladoNuevo, Cantidad).
coincidencias([_H|T], X, Acumulado, Cantidad):-!, coincidencias(T, X, Acumulado, Cantidad).



eliminarRepeticiones(_E,[], Acumulado, Acumulado):-!.
eliminarRepeticiones(E,[E|T], Acumulado, Res):- eliminarRepeticiones(E, T, Acumulado, Res),!.
eliminarRepeticiones(E, [H|T], Acumulado, Res):- NuevoAcumulado = [H|Acumulado], eliminarRepeticiones(E, T, NuevoAcumulado, Res),!.


minimoIncognitas([], _, 0):-!.
minimoIncognitas([GuessHead|GuessTail], ElementoUniverso, Minimo):- eliminarRepeticiones(GuessHead, GuessTail, [], Resto),
                                                                    reverse(Resto, GuessRest),
                                                                    minimoIncognitas(GuessRest, ElementoUniverso, NuevoMinimo),
                                                                    coincidencias([GuessHead|GuessTail], GuessHead, 0, A),
                                                                    coincidencias(ElementoUniverso, GuessHead, 0, B), min(A,B, MinimoParcial), Minimo is NuevoMinimo + MinimoParcial. 



feedback(Guess, ElementoUniverso, [Aciertos, Incognitas]):- minimoIncognitas(Guess, ElementoUniverso, Minimo), 
                                                 cantidadAciertos(Guess, ElementoUniverso, 0, Aciertos), 
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
                                                                       

calcularEntropia(Frecuencias, CardinalidadGuesses, Entropia):- calcularEntropiaAux(Frecuencias, CardinalidadGuesses, EntropiaParcial), Entropia is EntropiaParcial * (-1).




comparacionFeedback(GuessParcial, Guess, 0, [AciertosGuess, IncognitasGuess]):- !, feedback(GuessParcial, Guess, [AciertosTotales, IncognitasTotales]), AciertosTotales =:= AciertosGuess, IncognitasGuess =:= IncognitasTotales,!.
comparacionFeedback(GuessParcial, Guess, EspaciosRestantes,[AciertosGuess, IncognitasGuess]):- feedback(GuessParcial, Guess, [AciertosParteCod, IncognitasParteCod]),
                                                                            AciertosParteCod =< AciertosGuess, IncognitasParteCod =< IncognitasGuess + EspaciosRestantes.
cumpleFiltros(_, [], _):-!.
cumpleFiltros(GuessParcial, Filtros, EspaciosRestantes) :-
    forall(member([GuessRealizado, GuessFeedback], Filtros),
           comparacionFeedback(GuessParcial, GuessRealizado, EspaciosRestantes, GuessFeedback)).

generarConjuntoSolucion(_, _, 0, GuessParcial, Solucion):- Solucion = GuessParcial,!. 
generarConjuntoSolucion(Filtros, Colores, Espacios, GuessParcial, Solucion):- member(ColorSeleccionado, Colores), 
                                                                            append(GuessParcial, [ColorSeleccionado], ParcialNuevo),
                                                                            EspaciosRestantes is Espacios - 1,  %optimizable append
                                                                            cumpleFiltros(ParcialNuevo, Filtros, EspaciosRestantes),
                                                                            generarConjuntoSolucion(Filtros, Colores, EspaciosRestantes, ParcialNuevo, Solucion).
                                                                                                           

listaFeedbacks(_, [], Acc, Feedbacks):- Feedbacks = Acc,!.
listaFeedbacks(ElementoUniverso, [PosibleSolucion| Restantes], Acc, Feedbacks):-feedback(PosibleSolucion, ElementoUniverso, Feedback),
                                                                                Acc2 = [Feedback| Acc],
                                                                                listaFeedbacks(ElementoUniverso, Restantes, Acc2, Feedbacks).


entropias([], _, _, [EntropiaMaxima, ElementoMaximo], [EntropiaMaxima, ElementoMaximo]):-!.
entropias([ElementoUniverso|R], ConjuntoSolucion, CardinalidadGuesses,[EntropiaMaxima, ElementoMaximo], Res):-  listaFeedbacks(ElementoUniverso, ConjuntoSolucion, [], Frecuencias),
                                                                            agruparPares(Frecuencias, Agrupado),
                                                                             calcularEntropia(Agrupado, CardinalidadGuesses, Entropia),
                                                                             (Entropia >= EntropiaMaxima -> EntropiaNueva is Entropia, ElementoNuevo = ElementoUniverso; EntropiaNueva is EntropiaMaxima, ElementoNuevo = ElementoMaximo),
                                                                            entropias(R, ConjuntoSolucion, CardinalidadGuesses, [EntropiaNueva, ElementoNuevo], Res),!.


conjuntoSolucion(Filtros, Colores, Espacios, Muestra):- findall(Solucion , (generarConjuntoSolucion(Filtros, Colores, Espacios, [], Solucion)), Muestra),length(Muestra, L),write(L). 

siguienteGuessGeneral(Colores, Espacios, Filtros, Guess):-conjuntoSolucion(Filtros, Colores, Espacios, M2),!, length(M2, L),
                                                       entropias(M2, M2, L, [0,[]], Candidato), Candidato = [_E, Guess],write(Guess),!.
                                                                                                
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


completarGuess(Grupo, Todos, Tam, Guess) :-
    length(Grupo, L),
    Faltan is Tam - L,
    completarConRandom(Todos, Faltan, Aleatorios),
    append(Grupo, Aleatorios, Guess).

completarConRandom(_, 0, []) :- !.
completarConRandom(Todos, N, [X|Resto]) :-
    N > 0,
    length(Todos, Len),
    myRandom(Len,X),
    N1 is N - 1,
    completarConRandom(Todos, N1, Resto).

generarGuessesIniciales([], _, _, []) :- !.
generarGuessesIniciales(Colores, Todos, Tam, [Guess|Resto]) :- length(Colores, L),
        L =< Tam ->
        completarGuess(Colores, Todos, Tam, Guess),
        Resto = []
        ;
        tomarPrimeros(Colores, Tam, Guess),
        eliminarPrimeros(Colores, Tam, ColoresRestantes),
        generarGuessesIniciales(ColoresRestantes, Todos, Tam, Resto).



leerFeedback(Chars, [Negras, Blancas]) :-
    contar_signos(Chars, 0, 0, Negras, Blancas).

contar_signos([], N, B, N, B).
contar_signos(['!'|T], NAcc, BAcc, N, B) :-
    NAcc1 is NAcc + 1,
    contar_signos(T, NAcc1, BAcc, N, B).
contar_signos(['?'|T], NAcc, BAcc, N, B) :-
    BAcc1 is BAcc + 1,
    contar_signos(T, NAcc, BAcc1, N, B).
contar_signos([_|T], NAcc, BAcc, N, B) :-
    contar_signos(T, NAcc, BAcc, N, B). 

guessesNecesarios(C, S, R, Guesses) :-
    N0 is C ** S,
    LogRatio is log(10000 / N0),
    LogR is log(R),
    Raw is LogRatio / LogR,
    Guesses is ceiling(Raw).
    


generarGuessAleatorio(_, 0, _, []):-!.
generarGuessAleatorio(Colores, Espacios, _Historial, [Color|Guess]):- length(Colores, L), myRandom(L, Color),
                                                          Restantes is Espacios - 1,
                                                          generarGuessAleatorio(Colores, Restantes, _, Guess).

generarGuessHeuristico(Colores, Espacios, _Historial, Guess):- generarGuessAleatorio(Colores, Espacios, _, Guess).



% Loop Principal




% Loop Principal


jugar(Colores, Espacios):- length(Colores, L), Combinaciones is L ** Espacios,
                            (Combinaciones > 10000 
                            -> (guessesNecesarios(L, Espacios, 0.36, Necesarios),
                            (Combinaciones < 1000000 -> Necesarios2 is Necesarios+1 ; Necesarios2 is Necesarios),
                               generarGuessesIniciales(Colores, Colores, Espacios, GuessesIniciales),
                               jugarAux(GuessesIniciales, Necesarios2, [], Colores, Espacios))
                            ; jugarAux([], 0, [], Colores, Espacios)).


jugarAux(_, 0, Filtros, Colores, Tam) :-
    %format('Guess: ~w~n', [Filtros]),
    siguienteGuessGeneral(Colores, Tam, Filtros, GuessFinal),
    format('Guess: ~w~n', [GuessFinal]),
    pedirFeedbackYContinuar(GuessFinal, Filtros, Colores, Tam),!.

jugarAux([], N, Filtros, Colores, Tam) :- N > 0,
    generarGuessHeuristico(Colores, Tam, Filtros, Guess),!,
    format('Guess: ~w~n', [Guess]),
    read_line_to_string(user_input, Linea),!,
    string_chars(Linea, Chars),!,
    leerFeedback(Chars, Feedback),!,
    NuevosFiltros = [[Guess, Feedback] | Filtros],
    N2 is N-1,
    jugarAux([], N2, NuevosFiltros, Colores, Tam).


jugarAux([G|Guesses], N, Filtros, Colores, Tam) :-
    format('Guess: ~w~n', [G]),
    read_line_to_string(user_input, Linea),!,
    string_chars(Linea, Chars),!,
    leerFeedback(Chars, Feedback),!,
    NuevosFiltros = [[G, Feedback] | Filtros],
    N2 is N-1,
    jugarAux(Guesses, N2, NuevosFiltros, Colores, Tam).


pedirFeedbackYContinuar(Guess, Filtros, Colores, Tam) :-
    read_line_to_string(user_input, Linea),!,
    string_chars(Linea, Chars),!,
    leerFeedback(Chars, Feedback),!,
    NuevosFiltros = [[Guess, Feedback] | Filtros],
    siguienteGuessGeneral(Colores, Tam, NuevosFiltros, Siguiente),
    format('Guess: ~w~n', [Siguiente]),
    pedirFeedbackYContinuar(Siguiente, NuevosFiltros, Colores, Tam).




