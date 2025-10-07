-module(motor).
-export([feedback/2,generarCodigo/2,iniciarJuego/2]).



contarAciertos([],[])-> 0;
contarAciertos([Acierto|Guess], [Acierto|Codigo])-> 1 + contarAciertos(Guess, Codigo);
contarAciertos([_Diferente|Guess], [_Acierto|Codigo])-> contarAciertos(Guess, Codigo).


contarCoincidencias(Codigo, Elemento)-> length([X || X <- Codigo, X =:= Elemento]).
eliminarCoincidencias(Codigo, Elemento)-> [X || X <- Codigo, X =/= Elemento].


contarIncognitas([], _Codigo)-> 0;
contarIncognitas([Elemento|Guess], Codigo)-> min(contarCoincidencias(Codigo, Elemento), contarCoincidencias([Elemento|Guess],Elemento))
                                          + contarIncognitas(eliminarCoincidencias(Guess, Elemento), Codigo).

feedback(Guess, Codigo)-> A = contarAciertos(Guess, Codigo), [A, contarIncognitas(Guess, Codigo) - A].

generarCodigo(_Colores, 0)-> [];
generarCodigo(Colores, Espacios)-> [rand:uniform(Colores)|generarCodigo(Colores, Espacios -1)].

leerGuess()-> {ok, [Guess]} = io:fread("", "~w"), Guess.

loop(Codigo)-> Guess = leerGuess(), [A, B] = feedback(Guess, Codigo), io:format("[~w, ~w]~n", [A, B]),
               ((A =:= length(Codigo)) andalso ok) orelse loop(Codigo).
            
iniciarJuego(Colores, Espacios)-> Codigo = generarCodigo(Colores, Espacios), loop(Codigo). 
