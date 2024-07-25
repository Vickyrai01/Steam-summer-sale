juego(accion(callOfDuty),5).
juego(accion(batmanAA),10).
juego(mmorpg(wow,5000000),30).
juego(mmorpg(lineage2,6000000),15).
juego(puzzle(plantsVsZombies,40,media),10).
juego(puzzle(tetris,10,facil),0).

oferta(callOfDuty,10).
oferta(plantsVsZombies,50).

usuario(nico,[batmanAA,plantsVsZombies,tetris],[compra(lineage2)]).
usuario(fede,[],[regalo(callOfDuty,nico),regalo(wow,nico)]).
usuario(rasta,[lineage2],[]).
usuario(agus,[],[]).
usuario(felipe,[plantsVsZombies],[compra(tetris)]).

cuantoSale(Juego, Valor) :-
    nombreJuego(Juego, Nombre),
    precio(Nombre, Valor).

nombreJuego(accion(Nombre), Nombre).
nombreJuego(mmorpg(Nombre, _), Nombre).
nombreJuego(puzzle(Nombre, _, _), Nombre).

precio(Nombre, Valor) :-
    oferta(Nombre, Descuento),
    precioOriginal(Nombre, PrecioOriginal),
    Valor is PrecioOriginal * (1 - Descuento / 100).

precio(Nombre, Valor) :- precioOriginal(Nombre, Valor).

precioOriginal(Nombre, Valor) :- juego(accion(Nombre), Valor).
precioOriginal(Nombre, Valor) :- juego(mmorpg(Nombre, _), Valor).
precioOriginal(Nombre, Valor) :- juego(puzzle(Nombre, _, _), Valor).

juegoPopular(accion(_)).
juegoPopular(mmorpg(_,Usuarios)) :- integer(Usuarios),Usuarios > 1000000.
juegoPopular(puzzle(_,_,facil)).
juegoPopular(puzzle(_, 25, _)).

tieneUnBuenDescuento(Juego) :-
    nombreJuego(Juego, Nombre),
    oferta(Nombre, Descuento),
    Descuento > 50.

adictoALosDescuentos(Usuario):-
    usuario(Usuario,_,Juegos),
    Juegos \= [],
    forall(adquirio(Juego, Juegos), tieneUnBuenDescuento(Juego)).

adquirio(Juego, Juegos):- member(compra(Juego), Juegos).
adquirio(Juego, Juegos):- member(regalo(Juego,_), Juegos).

%------------------ no se si esta bien --------------------
fanaticoDe(Usuario, Genero):-
    usuario(Usuario,Juegos,_),
    tieneDosJuegosDe(Juegos, Genero).

tieneDosJuegosDe(Juegos, Genero):-
    member(Juego1, Juegos),
    member(Juego2, Juegos),
    esDelGenero(Juego1, Genero),
    esDelGenero(Juego2, Genero),
    Juego1 \= Juego2.

%------------------ no se si esta bien --------------------

esDelGenero(accion(_), accion).
esDelGenero(mmorpg(_, _), mmorpg).
esDelGenero(puzzle(_, _, _), puzzle).

monotematico(Usuario, Genero) :-
    usuario(Usuario, Juegos, _),
    Juegos \= [],
    forall(juegoEnBiblioteca(Juego, Juegos), esDelGenero(Juego, Genero)).

juegoEnBiblioteca(Juego, Juegos):- member(accion(Juego), Juegos).
juegoEnBiblioteca(Juego, Juegos):- member(mmorpg(Juego,_), Juegos).
juegoEnBiblioteca(Juego, Juegos):- member(puzzle(Juego,_,_), Juegos).

buenosAmigos(Usuario1, Usuario2):-
    usuario(Usuario1, _, Adquisiciones1),
    usuario(Usuario2, _, Adquisiciones2),
    regalaJuegoPopular(Usuario1, Usuario2, Adquisiciones1),
    regalaJuegoPopular(Usuario2, Usuario1, Adquisiciones2).

regalaJuegoPopular(Usuario1, Usuario2, Adquisiciones):-
    usuario(Usuario1,_, Adquisiciones),
    member(regalo(Juego,Usuario2), Adquisiciones),
    juegoPopular(Juego).

cuantoGastara(Usuario, GastoTotal) :-
    usuario(Usuario, _, Adquisiciones),
    findall(Costo, (member(Adquisicion, Adquisiciones), costoAdquisicion(Adquisicion, Costo)), Costos),
    sumlist(Costos, GastoTotal).
    
costoAdquisicion(compra(Juego), Costo) :- cuantoSale(Juego, Costo).
costoAdquisicion(regalo(Juego,_), Costo) :- cuantoSale(Juego, Costo).
