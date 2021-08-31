
% Parcial el kioskito 2020

% dodain atiende lunes, miércoles y viernes de 9 a 15.
atiende(dodain, lunes, 9, 15).
atiende(dodain, miercoles, 9, 15).
atiende(dodain, viernes, 9, 15).

% lucas atiende los martes de 10 a 20
atiende(lucas, martes, 10, 20).

% juanC atiende los sábados y domingos de 18 a 22.
atiende(juanC, sabado, 18, 22).
atiende(juanC, domingo, 18, 22).

% juanFdS atiende los jueves de 10 a 20 y los viernes de 12 a 20.
atiende(juanFdS, jueves, 10, 20).
atiende(juanFdS, viernes, 12, 20).

% leoC atiende los lunes y los miércoles de 14 a 18.
atiende(leoC, lunes, 14, 18).
atiende(leoC, miercoles, 14, 18).

% martu atiende los miércoles de 23 a 24.
atiende(martu, miercoles, 23, 24).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 1: calentando motores (2 puntos)
% Definir la relación para asociar cada persona con el rango horario que 
% cumple, e incorporar las siguientes cláusulas:

% vale atiende los mismos días y horarios que dodain y juanC. ✅
atiende(vale, X, Y, Z) :- atiende(dodain, X, Y, Z).
atiende(vale, X, Y, Z) :- atiende(juanC, X, Y, Z).

% nadie hace el mismo horario que leoC ✅
% Por principio de universo cerrado, no tiene sentido agregarlo.

% maiu está pensando si hace el horario de 0 a 8 los martes y miércoles ✅
% No se modela por principio de universo cerrado. Lo desconocido se considera falso.

% En caso de no ser necesario hacer nada, explique qué concepto teórico está relacionado y justifique su respuesta.







% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 2: quién atiende el kiosko... (2 puntos)
% Definir un predicado que permita relacionar un día y hora con una persona, en la que dicha persona atiende el kiosko. Algunos ejemplos:
atendiendokiosko(Dia, Hora, Quien):-
    atiende(Quien, Dia, HoraInicio, HoraFin),
    between(HoraInicio, HoraFin, Hora).

% si preguntamos quién atiende los lunes a las 14, son dodain, leoC y vale ✅
% atendiendokiosko(lunes, 14, Quien).

% si preguntamos quién atiende los sábados a las 18, son juanC y vale ✅
% atendiendokiosko(sabado, 18, Quien). 

% si preguntamos si juanFdS atiende los jueves a las 11, nos debe decir que sí. ✅
% atendiendokiosko(jueves, 11, juanFdS).

% si preguntamos qué días a las 10 atiende vale, nos debe decir los lunes, miércoles y viernes. ✅
% atendiendokiosko(Dia, 10, vale).

% El predicado debe ser inversible para relacionar personas y días.






% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 3: Forever alone (2 puntos)
% Definir un predicado que permita saber si una persona en un día y horario
% determinado está atendiendo ella sola. En este predicado debe utilizar 
% not/1, y debe ser inversible para relacionar personas. Ejemplos:

foreverAlone(Dia, Hora, Quien):-
    atendiendokiosko(Dia, Hora, Quien),
    not((atendiendokiosko(Dia, Hora, OtroQuien), OtroQuien \= Quien)). 


% forever alone el martes a las 19, lucas satisface ✅
% foreverAlone(martes, 19, Quien).

% forever alone el jueves a las 10, juanFdS satisface ✅
% foreverAlone(jueves, 10, Quien).

% forever alone el miércoles a las 22, martu no satisface ✅
% foreverAlone(miercoles, 22, martu).

% martu sí está forever alone el miércoles a las 23 ✅
% foreverAlone(miercoles, 23, martu).

% el lunes a las 10 dodain no está forever alone, porque vale también está ✅
% foreverAlone(lunes, 10, dodain).





% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 4: posibilidades de atención (3 puntos / 1 punto)
% Dado un día, queremos relacionar qué personas podrían estar atendiendo ✅
% el kiosko en algún momento de ese día. Por ejemplo, si preguntamos por 
% el miércoles, tiene que darnos esta combinatoria:

atendedores(Dia, AtendedoresPosibles):-
    findall(Atendedor, distinct(Atendedor, atendiendokiosko(Dia, _, Atendedor)), Atendedores),
    atendedoresPosibles(Atendedores, AtendedoresPosibles).

atendedoresPosibles([], []).

atendedoresPosibles([Atendedor|Atendedores], [Atendedor|AtendedoresPosibles]):-
    atendedoresPosibles(Atendedores, AtendedoresPosibles).

atendedoresPosibles([_|Atendedores], AtendedoresPosibles):-
    atendedoresPosibles(Atendedores, AtendedoresPosibles).

% nadie
% dodain solo
% dodain y leoC
% dodain, vale, martu y leoC
% vale y martu
% etc.

% Queremos saber todas las posibilidades de atención de ese día. La única
% restricción es que la persona atienda ese día (no puede aparecer lucas,
% por ejemplo, porque no atiende el miércoles). ✅

% Punto extra: indique qué conceptos en conjunto permiten resolver este
% requerimiento, justificando su respuesta.
% REVIEW: Revisar esto en el futuro.
% Inversibilidad, explosión combinatoria.






% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 5: ventas / suertudas (4 puntos)
% En el kiosko tenemos por el momento tres ventas posibles:
% golosinas, en cuyo caso registramos el valor en plata
%golosinas(Valor).

% cigarrillos, de los cuales registramos todas las marcas de cigarrillos que se vendieron (ej: Marlboro y Particulares)
%cigarrillos(Marca).

% bebidas, en cuyo caso registramos si son alcohólicas y la cantidad
%bebidas(Tipo, Cantidad).

% Queremos agregar las siguientes cláusulas:
% dodain hizo las siguientes ventas el lunes 10 de agosto: golosinas por $ 1200, cigarrillos Jockey, golosinas por $ 50
venta(dodain, lunes, [golosinas(1200), cigarrillos(jockey), golosinas(50)]).

% dodain hizo las siguientes ventas el miércoles 12 de agosto: 8 bebidas alcohólicas, 1 bebida no-alcohólica, golosinas por $ 10
venta(dodain, miercoles, [bebidas(alcoholica, 8), bebidas(noAlcoholica, 1), golosinas(10)]).

% martu hizo las siguientes ventas el miercoles 12 de agosto: golosinas por $ 1000, cigarrillos Chesterfield, Colorado y Parisiennes.
venta(martu, miercoles, [golosinas(1000), cigarrillos(chesterfield), cigarrillos(parisiennes), cigarrillos(colombia)]).

% lucas hizo las siguientes ventas el martes 11 de agosto: golosinas por $ 600.
venta(lucas, martes, [golosinas(600)]).

% lucas hizo las siguientes ventas el martes 18 de agosto: 2 bebidas no-alcohólicas y cigarrillos Derby.
venta(lucas, martes, [bebidas(noAlcoholica, 2), cigarrillos(derby)]).

% Queremos saber si una persona vendedora es suertuda, esto ocurre si para todos los días en los que vendió, la primera venta que hizo fue importante. Una venta es importante:
vendedoreSuertude(Vendedore):-
    distinct(Vendedore, venta(Vendedore, _, _)),
    forall(venta(Vendedore, _, [PrimeraVenta, _]), importante(PrimeraVenta)).

% en el caso de las golosinas, si supera los $ 100.
importante(venta(_, _, golosinas(Valor))):-
    Valor > 100.

% en el caso de los cigarrillos, si tiene más de dos marcas.
importante(venta(_, _, cigarrillos(Marcas))):-
    length(Marcas, Cantidad),
    Cantidad > 2.

% en el caso de las bebidas, si son alcohólicas o son más de 5.
importante(venta(_, _, bebidas(Tipo, _))):-
    Tipo == alcoholica.

importante(venta(_, _, bebidas(_, Cantidad))):-
    Cantidad > 5.


:-begin_tests(kioskito).

test(atienden_los_viernes, set(Persona = [vale, dodain, juanFdS])):-
  atiende(Persona, viernes, _, _).

test(personas_que_atienden_un_dia_puntual_y_hora_puntual, set(Persona = [vale, dodain, leoC])):-
  atendiendokiosko(lunes, 14, Persona).

test(dias_que_atiende_una_persona_en_un_horario_puntual, set(Dia = [lunes, miercoles, viernes])):-
  atendiendokiosko(Dia, 10, vale).

test(una_persona_esta_forever_alone_porque_atiende_sola, set(Persona=[lucas])):-
  foreverAlone(martes, 19, Persona).

test(persona_que_no_cumple_un_horario_no_puede_estar_forever_alone, fail):-
  foreverAlone(miercoles, 22, martu).

test(posibilidades_de_atencion_en_un_dia_muestra_todas_las_variantes_posibles, set(Personas=[[],[dodain],[dodain,leoC],[dodain,leoC,martu],[dodain,leoC,martu,vale],[dodain,leoC,vale],[dodain,martu],[dodain,martu,vale],[dodain,vale],[leoC],[leoC,martu],[leoC,martu,vale],[leoC,vale],[martu],[martu,vale],[vale]])):-
  atendedores(miercoles, Personas).

test(personas_suertudas, set(Persona = [martu, dodain])):-
  vendedoreSuertude(Persona).

:-end_tests(kioskito).