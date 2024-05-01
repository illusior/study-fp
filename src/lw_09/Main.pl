% ЗАДАНИЕ № 1.a
% Создать базу данных для отношений parent,male,female.

parent(bill, joe).
parent(bill, ann).
parent(sue, joe).
parent(sue, ann).
parent(paul, jim).
parent(mary, jim).
parent(ann, bob).
parent(jim, bob).
parent(joe, tammy).

female(ann).
female(mary).
female(sue).
female(tammy).

% ЗАДАНИЕ № 1.b
% Составить вопрос и найти в базе данных бабушку для bob.

grandparent(X, Y) :- parent(Z, Y), parent(X, Z).
grandmother(X, Y) :- grandparent(X, Y), female(X).

% вопрос:
% ?- grandmother(X, bob).

% ЗАДАНИЕ № 1.c
% Составить вопрос и найти в базе данных внука.

grandchild(X, Y) :- grandparent(Y, X).

% вопросы:
% ?- grandchild(X, bill). % выведет всех внуков bill
% ?- grandchild(X, sue). % выведет всех внуков sue
% ?- grandchild(X, paul). % выведет всех внуков paul
% ?- grandchild(X, mary). % выведет всех внуков mary

% ЗАДАНИЕ № 1.d
% Составить вопрос и найти в базе данных сестру для jim

sibling(X, Y) :- parent(Z, X), parent(Z, Y).

different(X, Y) :- X \= Y.
sister(X, Y) :- sibling(X, Y), female(X), different(X, Y).

% вопрос:
% ?- sister(X, jim). - даст false, т.к. у jim нет сестёр
% ?- sister(X, joe). - даст ann

% ЗАДАНИЕ 1.e
% Определите отношение "тётя" - aunt (X, Y).

aunt(X, Y) :- parent(Z, Y), sister(X, Z).

% ЗАДАНИЕ 1.f
% Определите отношение "кузин" - cousin(X, Y).

cousin(X, Y) :- parent(Z1, X), parent(Z2, Y), sibling(Z1, Z2).

% =================================================================

% ЗАДАНИЕ 2.a
% Создать базу данных "Хобби".
% Предикат likes определяет отношение человек - хобби.

likes(Ellen, reading).
likes(John, computers).
likes(John, badminton).
likes(John, photo).
likes(John, reading).
likes(Leonard, badminton).
likes(Eric, swimming).
likes(Eric, reading).
likes(Eric, chess).
likes(Paul, swimming).

% ЗАДАНИЕ 2.b
% Составить вопрос и найти тех, кто имеет четыре хобби.


% ЗАДАНИЕ 2.c
% Составить вопрос и найти тех, у кого одинаковые хобби.

