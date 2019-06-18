:- dynamic si/1, no/1.

adivinarPersonaje :- fail.

% agregarPersonaje(+Nombre, +Atributos).
agregarPersonaje(_, _) :- fail.

% mostrarPersonaje(+Nombre).
mostrarPersonaje(_) :- fail.

borraRespuestas :- fail.

% atributos(?Nombre, ?Atributos).

% satisfaceAtributos(+Atributos).
satisfaceAtributos(_) :- fail.

% satisface(+Atributo).
satisface(_) :- fail.

% pregunta(+Atributo).
pregunta(A) :- mostrarPregunta(A), leerRespuesta(R), guardarRespuesta(A,R).

% mostrarPregunta(+Atributo).
mostrarPregunta(_) :- fail.

% leerRespuesta(-Respuesta).
leerRespuesta(_) :- fail.

% guardarRespuesta(+Atributo, +Respuesta).
guardarRespuesta(A, R) :- R == 'si', !, assertz(si(A)).
guardarRespuesta(A, R) :- R == 'no', !, assertz(no(A)).
guardarRespuesta(A, R) :- write('Respuesta inválida. Se pregunta nuevamente.\n'), pregunta(A).


%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%
test(1) :- true.
tests :- forall(between(1,1,N), test(N)). % Hacer mejores tests y cambiar 1 por la cantidad de tests que tengan.

