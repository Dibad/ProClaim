:- dynamic
  known/3,
  voice/1.

bird(laysan_albatross) :-
  family(albatross),
  color(white).

bird(black_footed_albatross) :-
  family(albatross),
  color(dark).

bird(whistling_swan) :-
  family(swan),
  voice(muffled_musical_whistle).

bird(trumpeter_swan) :-
  family(swan),
  voice(loud_trumpeting).

bird(canada_goose) :-
  family(goose),
  season(winter),
  country(united_states),
  head(black),
  cheek(white).

bird(canada_goose) :-
  family(goose),
  season(summer),
  country(canada),
  head(black),
  cheek(white).

bird(mallard) :-
  family(duck),
  voice(quack),
  head(green).

bird(mallard) :-
  family(duck),
  voice(quack),
  color(mottled_brown).


order(tubenose) :-
  nostrils(external_tubular),
  live(at_sea),
  bill(hooked).

order(waterfowl) :-
  feet(webbed),
  bill(flat).

family(albatross) :-
  order(tubenose),
  size(large),
  wings(long_narrow).


family(swan) :-
  order(waterfowl),
  neck(long),
  color(white),
  flight(ponderous).


country(united_states) :- region(mid_west).
country(united_states) :- region(south_west).
country(united_states) :- region(north_west).
country(united_states) :- region(mid_atlantic).
country(canada) :- province(ontario).
country(canada) :- province(quebec).

region(new_england) :-
  state(X),
  member(X, [massachussets, vermont]).

region(south_east) :-
  state(X),
  member(X, [florida, mississippi]).

multivalued(voice).
multivalued(feed).

size(X) :- menuask(size, X, [large, plump, medium, small]).
flight(X) :- menuask(flight, X, [ponderous, agile, flap_glide]).

bill(X) :- ask(bill, X).
live(X) :- ask(live, X).
nostrils(X) :- ask(nostrils, X).
eats(X) :- ask(eats, X).
feet(X) :- ask(feet, X).
wings(X) :- ask(wings, X).
neck(X) :- ask(neck, X).
color(X) :- ask(color, X).


ask(A, V) :-
  known(yes, A, V),
  !.

ask(A, V) :-
  known(_, A, V),
  !,
  fail.

ask(A, V) :-
  write(A : V),
  writeln('? '),
  read(Y),
  asserta(known(Y, A, V)),
  Y == yes.

ask(A, V) :-
  \+ multivalued(A),
  known(yes, A, V2),
  V \== V2,
  !,
  fail.

menuask(A, V, MenuList) :-
  write('What is the value for'), write(A), writeln('?'),
  writeln(MenuList),
  read(X),
  check_val(X, A, V, MenuList),
  asserta(known(yes, A, X)),
  X == V.

check_val(X, A, V, MenuList) :-
  member(X, MenuList),
  !.

check_val(X, A, V, MenuList) :-
  write(X), writeln(' is not a legal value, try again.'),
  menuask(A, V, MenuList).

top_goal(X) :- bird(X).

solve :-
  retractall(known/3),
  top_goal(X),
  write('The answer is '), writeln(X).

solve :-
writeln('No answer found.').

go :-
  greetings,
  repeat,
  writeln('> '),
  read(X),
  do(X),
  X == quit.

greetings :-
  writeln('This is the Native Prolog shell'),
  writeln('Enter load, consult, or quit at the prompt.').

do(load) :-
  load_kb,
  !.

do(consult) :-
  solve,
  !.

do(quit).
do(X) :-
  write(X),
  writeln(' is not a legal command'),
  fail.

load_kb :-
  writeln('Enter file name: '),
  read(F),
  reconsult(F).
