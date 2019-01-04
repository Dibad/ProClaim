:- dynamic
  define/1, av/2, known/3.

% Split and tokenize a sentence
read_sentence(LA) :-
  read_string(current_input, ".", ". \n", _, String),
  split_string(String, '\n ', '. \n', List_Strings),
  atomic_list_concat(List_Strings,' ', Atom),
  atomic_list_concat(LA,' ', Atom),
  print(LA).

test :-
  see('birds.ckb'),
  lod_ruls,
  writeln('End'),
  seen,
  !.

lod_ruls :-
  read_sentence(L),
  print(L),
  trans(R, L, []),
  print(R),
  assertz(R).

top_goal(bird).


check(Ans) :-
  top_goal(Goal),
  rule(X, if(Y), then(av(Goal, Ans))),
  check_values(Y).

check_values([]) :- !.
check_values([H | T]) :-
  ask(H),
  check_values(T).


trans(rule(N, if(IF), then(THEN))) --> id(N), ['if'], iflist(IF), ['then'], then(THEN).

id(N) --> ['rule', N].

iflist([IF]) --> prase(IF).
iflist([Hif | Tif]) --> prase(Hif), ['and'], iflist(Tif).

then(THEN) --> prase(THEN).

prase(av(A, V)) --> [A, 'is', V].
prase(av(A, V)) --> [A, V].


% Asking
ask(A, V) :-
  known(yes, A, V),
  !.

ask(A, V) :-
  known(_, A, V),
  !,
  fail.

ask(A, V) :-
  write(A : V),
  writeln('? Escribe si. o no. :'),
  read(Ans),
  asserta(known(Ans, A, V)),
  Ans == 'si'.


