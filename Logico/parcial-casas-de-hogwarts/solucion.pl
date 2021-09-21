
% Parcial Casas de Hogwarts 2020

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parte 1 - Sombrero Seleccionador

% Para determinar en qué casa queda una persona cuando ingresa a Hogwarts,
% el Sombrero Seleccionador tiene en cuenta el carácter de la persona, lo
% que prefiere y en algunos casos su status de sangre.

% Tenemos que registrar en nuestra base de conocimientos qué característi-
% cas tienen los distintos magos que ingresaron a Hogwarts, el status de 
% sangre que tiene cada mago y en qué casa odiaría quedar. Actualmente sa-
% bemos que:

% Harry es sangre mestiza, y se caracteriza por ser corajudo, amistoso, 
% orgulloso e inteligente. Odiaría que el sombrero lo mande a Slytherin.

% Draco es sangre pura, y se caracteriza por ser inteligente y orgulloso,
% pero no es corajudo ni amistoso. Odiaría que el sombrero lo mande a 
% Hufflepuff.

% Hermione es sangre impura, y se caracteriza por ser inteligente, orgu-
% llosa y responsable. No hay ninguna casa a la que odiaría ir.

mago(Mago):-
    sangre(Mago, _).

% sangre(Mago, TipoDeSangre).
sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).
sangre(ron, mestiza).
sangre(luna, pura).
sangre(neville, mestiza).



% Además nos interesa saber cuáles son las características principales 
% que el sombrero tiene en cuenta para elegir la casa más apropiada:

% Para Gryffindor, lo más importante es tener coraje.
% Para Slytherin, lo más importante es el orgullo y la inteligencia.
% Para Ravenclaw, lo más importante es la inteligencia y la responsabilidad.
% Para Hufflepuff, lo más importante es ser amistoso.

% Se pide:
% Saber si una casa permite entrar a un mago, lo cual se cumple para cual-
% quier mago y cualquier casa excepto en el caso de Slytherin, que no per-
% mite entrar a magos de sangre impura.

casa(gryffindor).
casa(slytherin).
casa(ravenclaw).
casa(hufflepuff).

% permiteEntrar(Casa, Mago)

permiteEntrar(Casa, Mago) :-
    casa(Casa),
    mago(Mago),
    Casa \= slytherin.

permiteEntrar(slytherin, Mago) :-
    sangre(Mago, TipoDeSangre),
    TipoDeSangre \= impura.


% Saber si un mago tiene el carácter apropiado para una casa, lo cual se
% cumple para cualquier mago si sus características incluyen todo lo que 
% se busca para los integrantes de esa casa, independientemente de si la 
% casa le permite la entrada.

tieneCaracterApropiado(Mago, Casa) :-
    casa(Casa),
    mago(Mago),
    forall(caracteristicaBuscada(Casa, Caracteristica), tieneCaracteristica(Mago, Caracteristica)).

    % tieneCaracteristica(Mago, Caracteristica).
tieneCaracteristica(harry, corajude).
tieneCaracteristica(harry, amistose).
tieneCaracteristica(harry, orgullose).
tieneCaracteristica(harry, inteligente).
tieneCaracteristica(draco, inteligente).
tieneCaracteristica(draco, orgullose).
tieneCaracteristica(hermione, inteligente).
tieneCaracteristica(hermione, orgullose).
tieneCaracteristica(hermione, responsable).
tieneCaracteristica(ron, amistose).
tieneCaracteristica(ron, corajude).
tieneCaracteristica(neville, amistose).
tieneCaracteristica(neville, corajude).
tieneCaracteristica(neville, responsable).
tieneCaracteristica(luna, amistose).
tieneCaracteristica(luna, responsable).
tieneCaracteristica(luna, inteligente).

caracteristicaBuscada(gryffindor, corajude).
caracteristicaBuscada(slytherin, inteligente).
caracteristicaBuscada(slytherin, orgullose).
caracteristicaBuscada(ravenclaw, inteligente).
caracteristicaBuscada(ravenclaw, responsable).
caracteristicaBuscada(hufflepuff, amistose).


% Determinar en qué casa podría quedar seleccionado un mago sabiendo que 
% tiene que tener el carácter adecuado para la casa, la casa permite su 
% entrada y además el mago no odiaría que lo manden a esa casa. Además 
% Hermione puede quedar seleccionada en Gryffindor, porque al parecer en-
% contró una forma de hackear al sombrero.

podriaIr(Mago, Casa) :-
    tieneCaracterApropiado(Mago, Casa),
    permiteEntrar(Casa, Mago),
    not(odia(Mago, Casa)).

podriaIr(hermione, gryffindor).

% odia(Mago, Casa)
odia(harry, slytherin).
odia(draco, hufflepuff).


% Definir un predicado cadenaDeAmistades/1 que se cumple para una lista de 
% magos si todos ellos se caracterizan por ser amistosos y cada uno podría
% estar en la misma casa que el siguiente. No hace falta que sea inver-
% sible, se consultará de forma individual.

cadenaDeAmistades(Magos):-
    todesAmistoses(Magos),
    cadenaDeCasas(Magos).

todesAmistoses(Magos) :-
    forall(member(Mago, Magos), amistose(Mago)).

amistose(Mago):-
    tieneCaracteristica(Mago, amistose).

% cadenaDeCasas(Magos)
cadenaDeCasas([Mago1, Mago2 | Resto]) :-
    podriaIr(Mago1, Casa),
    podriaIr(Mago2, Casa),
    cadenaDeCasas([Mago2 | Resto]).

cadenaDeCasas([_]).
cadenaDeCasas([]).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARTE 2
% Se pide incorporar a la base de conocimiento la información sobre las 
% acciones realizadas y agregar la siguiente lógica a nuestro programa:

%accion(Mago, Accion)

hizoAlgunaAccion(Mago) :-
    accion(Mago, _).

% accion(Mago, Accion)
accion(harry, fueraDeCama).
accion(hermione, ir(tercerPiso)).
accion(hermione, ir(seccionRestringidaDeLaBiblioteca)).
accion(harry, ir(bosque)).
accion(harry, ir(tercerPiso)).
accion(draco, ir(mazmorras)).
accion(ron, buenaAccion(ganarUnaPartidaDeAjedrezMagico, 50)).
accion(hermione, buenaAccion(usarSuIntelectoParaSalvarASusAmigosDeUnaMuerteHorrible, 50)).
accion(harry, buenaAccion(ganarleAVoldemort, 60)).

malaAccion(Mago) :-
    accion(Mago, Accion),
    puntajeQueGenera(Accion, Puntaje),
    Puntaje < 0.

puntajeQueGenera(fueraDeCama, -50).
puntajeQueGenera(ir(tercerPiso), -75).
puntajeQueGenera(ir(bosque), -50).
puntajeQueGenera(ir(seccionRestringidaDeLaBiblioteca), -10).


% Saber si un mago es buen alumno, que se cumple si hizo alguna acción y 
% ninguna de las cosas que hizo se considera una mala acción (que son aque-
% llas que provocan un puntaje negativo).

esBuenAlumno(Mago) :-
    accion(Mago, Accion),
    not(malaAccion(Accion)).


% Saber si una acción es recurrente, que se cumple si más de un mago hizo 
% esa misma acción.

recurrente(Accion) :-
    accion(Mago, Accion),
    forall(accion(Mago2, Accion), Mago \= Mago2).

% Saber cuál es el puntaje total de una casa, que es la suma de los puntos
% obtenidos por sus miembros.




% Saber cuál es la casa ganadora de la copa, que se verifica para aquella 
% casa que haya obtenido una cantidad mayor de puntos que todas las otras.

% Queremos agregar la posibilidad de ganar puntos por responder preguntas
% en clase. La información que nos interesa de las respuestas en clase son:
% cuál fue la pregunta, cuál es la dificultad de la pregunta y qué profesor 
% la hizo.

% Por ejemplo, sabemos que Hermione respondió a la pregunta de dónde se en-
% cuentra un Bezoar, de dificultad 20, realizada por el profesor Snape, y 
% cómo hacer levitar una pluma, de dificultad 25, realizada por el profesor
% Flitwick.

% Modificar lo que sea necesario para que este agregado funcione con lo desa-
% rrollado hasta ahora, teniendo en cuenta que los puntos que se otorgan 
% equivalen a la dificultad de la pregunta, a menos que la haya hecho Snape,
% que da la mitad de puntos en relación a la dificultad de la pregunta.