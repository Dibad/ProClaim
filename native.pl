:- initialization(start).        % Start shell on load
:- dynamic
  known/3,
  multivalued/1,
  top_goal/1.

% Define not operator
:- op(900, fy, not).



% Start program
start :-
  greetings,
  main_loop.


% Prolog user shell - main loop
main_loop :-
  repeat,
    read(X),
    do(X),
    X = 'exit',
  !.

greetings :-
  writeln('Consola de comandos de Prolog para Sistemas Expertos'),
  help.


% -- COMMANDS --
do('help') :- help, !.
do('load') :- open_file, !.
do('solve') :- solve, !.
do('exit').
do(X) :-
  write(X),
  writeln(' no es un comando válido!').



% -- Help command
help :-
  writeln('Comandos: help. solve. load. o exit.'), nl.


% -- Open file for loading kb
open_file :-
  writeln('Escriba el nombre del archivo a cargar entre comillas (''*.kb''): '),
  /* read(File), */
  open_file('birds.ckb').

open_file(File) :-
  exists_file(File),
  load_rules(File),
  write(File), writeln(' ha sido cargado!.').

open_file(File) :-
  write('El archivo '), write(File), writeln(' no existe.').


load_rules(File) :-
  % clean_db
  see(File),
  lod_ruls,
  writeln('Todas las reglas se han leído correctamente.'),
  seen.

lod_ruls :-
  repeat,
    read_sentence(L),
    process(L),
    L = [''],
  !.

process(['']).

process(L) :-
  trans(R, L, []),
  writeln(R),
  assertz(R), % Add rule to the dynamic databae
  !.

process(L) :-
  writeln('Error de traducción en: '),
  writeln(L),
  fail.


% Split and tokenize a sentence
read_sentence(LA) :-
  read_string(current_input, ".", ". \n", _, String),
  split_string(String, '\n, ', '. \n', List_Strings),
  atomic_list_concat(List_Strings,' ', Atom),
  atomic_list_concat(LA,' ', Atom).


% Trans - Translate a list of atoms into an internal rule

trans(top_goal(X)) --> ['goal', X].
trans(rule(N, if(IF), then(THEN))) --> id(N), if(IF), then(THEN).

id(N) --> ['rule', N].

if(IF) --> ['if'], iflist(IF).
iflist([IF]) --> prase(IF).
iflist([Hif | Tif]) --> prase(Hif), (['and'] ; ['']), iflist(Tif).

then(THEN) --> ['then'], prase(THEN).

prase(av(A, V)) --> [A], (['is'] ; ['are']), [V].
prase(av(A, V)) --> [A, V].


% -- Solve problem command
solve :-
  retractall(known(_,_,_)), nl,
  current_predicate(top_goal/1),
  top_goal(Goal),
  rule(_, if(List), then(av(Goal, Ans))),
  askfrom(List),
  write('La respuesta es '), writeln(Ans),
  help.

solve :-
  not current_predicate(top_goal/1),
  writeln('Primero, cargue una base de datos de conocimiento con load.').
solve :-
  writeln('No se ha encontrado una respuesta válida'), nl.


askfrom([]) :- !.
askfrom([H | T]) :-
  ask(H),
  !,
  askfrom(T).


% Asking
ask(av(A, V)) :-
  known(si, A, V),
  !.

ask(av(A, V)) :-
  known(_, A, V),
  !,
  fail.

ask(av(A, _)) :-
  not multivalued(A),
  known(si, A, _),
  !,
  fail.

ask(av(A, V)) :-
  write(A : V),
  writeln('? Escribe si. o no. :'),
  read(Ans),
  asserta(known(Ans, A, V)),
  Ans == 'si'.

menuask(A, V, _) :-
  known(yes, A, V),
  !.

menuask(A, _, _) :-
  known(yes, A, _),
  !,
  fail.

menuask(A, V, MenuOptions) :-
  write('¿Cómo definirias atributo '),
  write(A),
  write('? '),
  write(MenuOptions),
  read(Val),
  check_val(Val, A, V, MenuOptions),
  asserta(known(yes, A, Val)),
  Val == V.

check_val(Val, _, _, MenuOptions) :-
  member(Val, MenuOptions),
  !.

check_val(Val, A, V, MenuOptions) :-
  write(Val), writeln(' no es un valor de la lista.'),
  menuask(A, V, MenuOptions).
