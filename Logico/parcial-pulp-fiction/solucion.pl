
personaje(pumkin,     ladron([licorerias, estacionesDeServicio])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent,    mafioso(maton)).
personaje(jules,      mafioso(maton)).
personaje(marsellus,  mafioso(capo)).
personaje(winston,    mafioso(resuelveProblemas)).
personaje(mia,        actriz([foxForceFive])).
personaje(butch,      boxeador).

pareja(marsellus, mia).
pareja(pumkin,    honeyBunny).

%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).

%etc

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sabiendo eso, resolver los siguientes predicados, los cuales deben ser completamente inversibles:

% 1. esPeligroso/1. Nos dice si un personaje es peligroso. Eso ocurre cuando:
% realiza alguna actividad peligrosa: ser matón, o robar licorerías. 
% tiene empleados peligrosos

% TODO: Implementar distinct

esPeligroso(Personaje):-
    actividadPeligrosa(Personaje).

actividadPeligrosa(Personaje):-
    personaje(Personaje, mafioso(maton)).
actividadPeligrosa(Personaje):-
    personaje(Personaje, ladron(Lugares)),
    member(licorerias, Lugares).
actividadPeligrosa(Personaje):-
    distinct(Personaje, trabajaPara(Personaje, Empleado)),
    esPeligroso(Empleado).


% 2. duoTemible/2 que relaciona dos personajes cuando son peligrosos y además son pareja o amigos. 

duoTemible(Personaje1, Personaje2):-
    ambosPeligrosos(Personaje1, Personaje2),
    sonCercanos(Personaje1, Personaje2).

sonCercanos(Personaje1, Personaje2):-
    amigo(Personaje1, Personaje2).
sonCercanos(Personaje1, Personaje2):-
    pareja(Personaje1, Personaje2).

ambosPeligrosos(Personaje1, Personaje2):-
    esPeligroso(Personaje1),
    esPeligroso(Personaje2).

% Considerar que Tarantino también nos dió los siguientes hechos:
amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor).


% 3.  estaEnProblemas/1: un personaje está en problemas cuando 

% el jefe es peligroso y le encarga que cuide a su pareja
% o bien, tiene que ir a buscar a un boxeador. 
% Además butch siempre está en problemas. 

% encargo(Solicitante, Encargado, Tarea). 
% las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent,   cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).

estaEnProblemas(Personaje):-
    trabajaPara(Jefe, Personaje),
    distinct(Personaje, encargosPeligrosos(Jefe, Personaje)).
estaEnProblemas(butch).

encargosPeligrosos(Jefe, Personaje):-
    esPeligroso(Jefe),
    pareja(Jefe, Pareja),
    encargo(Jefe, Personaje, cuidar(Pareja)).
encargosPeligrosos(_, Personaje):-
    encargo(_, Personaje, buscar(Boxeador, _)),
    esBoxeador(Boxeador).

esBoxeador(Boxeador):-
    personaje(Boxeador, boxeador).



% 4.  sanCayetano/1:  es quien a todos los que tiene cerca les da trabajo (algún encargo). 
% Alguien tiene cerca a otro personaje si es su amigo o empleado. 
sanCayetano(Persona):-
    tieneAlguienCerca(Persona),
    forall(tieneCerca(Persona, Empleado), leDaTrabajo(Persona, Empleado)).

tieneAlguienCerca(Persona):- 
    esPersonaje(Persona),
    tieneCerca(Persona, _).

esPersonaje(Personaje):-
    personaje(Personaje, _).

leDaTrabajo(Persona, Empleado):- encargo(Persona, Empleado, _).

tieneCerca(Persona1, Persona2):- amigo(Persona1, Persona2).
tieneCerca(Persona1, Persona2):- trabajaPara(Persona1, Persona2).

% 5. masAtareado/1. Es el más atareado aquel que tenga más encargos que cualquier otro personaje.

% 6. personajesRespetables/1: genera la lista de todos los personajes respetables. Es respetable cuando su actividad tiene un nivel de respeto mayor a 9. Se sabe que:
% Las actrices tienen un nivel de respeto de la décima parte de su cantidad de peliculas.
% Los mafiosos que resuelven problemas tienen un nivel de 10 de respeto, los matones 1 y los capos 20.
% Al resto no se les debe ningún nivel de respeto. 
