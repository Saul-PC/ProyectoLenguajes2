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

feedback(Guess, Codigo) -> A = contarAciertos(Guess, Codigo),
                           B = contarIncognitas(Guess, Codigo) - A,
                           lists:duplicate(A, $!) ++ lists:duplicate(B, $?).
generarCodigo(_Colores, 0)-> [];
generarCodigo(Colores, Espacios)-> [rand:uniform(Colores)|generarCodigo(Colores, Espacios -1)].

leerGuess()-> Input = io:get_line("Ingrese su intento: "),
    {ok, Tokens, _} = erl_scan:string(Input ++ "."),
    {ok, Expr} = erl_parse:parse_term(Tokens),
    Expr.

loop(Codigo)-> Guess = leerGuess(), FeedbackStr = feedback(Guess, Codigo),

               io:format("~s~n", [FeedbackStr]),
               ((length([C || C <- FeedbackStr, C =:= $!]) =:= length(Codigo)))
               orelse loop(Codigo).
            
iniciarJuego(Colores, Espacios)-> Codigo = generarCodigo(Colores, Espacios),
                                        io:format("Cod ingresado: ~p~n", [Codigo]),
                                        loop(Codigo). 
