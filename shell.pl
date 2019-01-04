% Prolog shell
:- initialization(start).        % Start shell on load

/* :- dynamic */

% Define not operator
:- op(900, fy, not).


% Prolog user shell - main loop
start :-
  writeln('Prolog Shell para Sistemas Expertos'),
  repeat,
    nl,
    help,
    read(Command),
    do(Command),
    Command = 'exit',
  !.

help :-
  writeln('Comandos: solve. load. o exit.').


% -- COMMANDS --
do('load') :- open_file, !.
do('solve') :- solve, !.
do('exit').
do(X) :-
  write(X),
  writeln(' no es un comando válido!').


% -- Open file for loading kb
open_file :-
  writeln('Escriba el nombre del archivo a cargar entre comillas (''*.kb''): '),
  /* read(File), */
  open_file('car.ckb').

open_file(File) :-
  exists_file(File),
  load_rules(File),
  write(File), writeln(' ha sido cargado!.').

open_file(File) :-
  write('El archivo '), write(File), writeln(' no existe.').

solve :-
  writeln('solve. aún no ha sido implementado!').




% -- Load and Parse Expert System Rules
load_rules(File) :-
  % clean_db,
  see(File),
  lod_ruls,
  writeln('Todas las reglas se han leído correctamente.'),
  seen.
  %!.

lod_ruls :-
  repeat,
    read_sentence(List),
    process(List),
    List == [''],
  !.

process(['']).
process(List) :-
  /* translate(Rule, List, []), */
  /* assertz(Rule), % Add rule to dynamic database */
  !.

process(List) :-
  writeln('Error de traducción en: '),
  writeln(List),
  fail.

% Split and tokenize a sentence
read_sentence(Rule_List) :-
  read_string(current_input, ".", "\n\r\t ", _, String),
  split_string(String, "\n\t, ", "", LStrings), % Remove special characters
  atomic_list_concat(LStrings,' ', Atom),
  atomic_list_concat(Rule_List,' ', Atom),
  print(Rule_List), nl.
