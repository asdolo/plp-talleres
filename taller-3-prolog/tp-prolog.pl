:- dynamic si/1, no/1.


% mostrarPersonaje(+Nombre).
mostrarPersonaje(P) :- write('Tu personaje es '), write(P), write('.'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 1

% atributos(?Nombre, ?Atributos).
atributos(sonic, [animal, azul, veloz]).
atributos(flash, [humano, rojo, veloz]).
atributos(juan, [lento, humano]).
atributos(pikachu, [pokemon, amarillo]).
atributos(charmander, [pokemon, rojo]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 2
% mostrarPregunta(+Atributo).
mostrarPregunta(A) :- write('(Pregunta) ¿Tiene su personaje el atributo '), write(A), write('?'), nl.

% leerRespuesta(-Respuesta).
leerRespuesta(R) :- read(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 3

% pregunta(+Atributo).
pregunta(A) :- mostrarPregunta(A), leerRespuesta(R), guardarRespuesta(A,R).

% guardarRespuesta(+Atributo, +Respuesta).
guardarRespuesta(A, R) :- R == 'si', !, assertz(si(A)).
guardarRespuesta(A, R) :- R == 'no', !, assertz(no(A)).
guardarRespuesta(A, _) :- write('Respuesta inválida. Se pregunta nuevamente.\n'), pregunta(A).

% satisface(+Atributo).
satisface(A) :- si(A), !.
satisface(A) :- no(A), !, fail.
satisface(A) :- pregunta(A), satisface(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 4

% satisfaceAtributos(+Atributos).
satisfaceAtributos([]).
satisfaceAtributos([H|T]) :- satisface(H), satisfaceAtributos(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 5

borraRespuestas :- retractall(si(X)), retractall(no(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 6

adivinarPersonaje :- atributos(X, L), satisfaceAtributos(L), mostrarPersonaje(X), borraRespuestas, !.
adivinarPersonaje :- borraRespuestas, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 7
% ¿Qué ocurre cuando el juego no conoce el personaje? 
% Devuelve false, porque no hay un personaje que tenga esos atributos.
% ¿Cómo influye el orden de los atributos en el predicado atributos(Personaje, Atributos)?
% Si los atributos en comun estan al principio entonces va a descartar más personajes al responder que no.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 8

mismosElementos([], []).
mismosElementos([H], L2) :- member(H, L2). 
mismosElementos([H|T], L2) :- member(H, L2), mismosElementos(T, L2).

mismos(L1, L2) :- length(L1, L), length(L2, L), mismosElementos(L1, L2).

existeOtroConMismosAtributos(ATTR) :- atributos(N, ATTR), mismos(ATTR, A).
existeOtroConElMismoNombre(N) :- atributos(N, _).

% agregarPersonaje(+Nombre, +Atributos).
agregarPersonaje(N1, A1) :- not(existeOtroConMismosAtributos(A1)), 
							not(existeOtroConElMismoNombre(N1)), assertz(atributos(N1, A1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 9

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(1) :- true.
tests :- forall(between(1,1,N), test(N)). % Hacer mejores tests y cambiar 1 por la cantidad de tests que tengan.

