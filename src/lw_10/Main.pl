% ЗАДАНИЕ № 1

% На рисунке представлены 9 отрезков: четыре горизонтальные и пять вертикальных.
% Опишите рисунок базой данных, состоящей из 9 фактов об отрезках.

% Вопрос ?- horiz(N).
% Дает ответ N=1, N=2, N=3, N=5.

% Вопрос ?- vertical(N).
% Дает ответ N=4, N=6, N=7, N=8, N=9.

seg(1, point(1, 11), point(4, 11)).
seg(2, point(2, 4), point(13, 4)).
seg(3, point(2, 2), point(9, 2)).
seg(4, point(3, 10), point(3, 2)).
seg(5, point(7, 10), point(13, 10)).
seg(6, point(8, 13), point(8, 0)).
seg(7, point(10, 12), point(10, 3)).
seg(8, point(11, 13), point(11, 3)).
seg(9, point(12, 12), point(12, 2)).

horiz(N) :-
    seg(N, point(_, Y), point(_, Y)).

vertical(N) :-
    seg(N, point(X, _), point(X, _)).

% ЗАДАНИЕ № 2
%
% Введите в базу данных правило, определяющее пересекающиеся отрезки.
% Голова правила представлена структурой cross/5,
% два аргумента которой N и M - номера пересекающихся отрезков,
% третий - point(X,Y), описывает точку пересечения,
% а четвертый и пятый - длины пересекающихся отрезков.
% (Если необходимо, для нахождения квадратного корня воспользуйтесь правилом sqrt(N, X), где N - число, X - квадратный корень от числа N.)

cross(N, M, point(X, Y), NL, ML) :-
    seg(N, point(X, Y1), point(X, Y2)),
    seg(M, point(X1, Y), point(X2, Y)),
    X > min(X1, X2),
    X < max(X1, X2),
    Y > min(Y1, Y2),
    Y < max(Y1, Y2),
    NL is abs(Y1-Y2),
    ML is abs(X1-X2).

% ЗАДАНИЕ № 3
%
% Добавьте в базу данных правило определения периметра и площади прямоугольников, образуемых пересекающимися отрезками.
% Голова правила представлена структурой per_sq/6, четыре аргумента которой - номера отрезков, образующих прямоугольник.
% Пятый аргумент - P, периметр прямоугольника. Шестой аргумент - S, площадь периметра.

per_sq(A, B, C, D, P, S) :-
    cross(A, C, point(X1, Y1), _, _),
    cross(A, D, point(X1, Y2), _, _),
    cross(B, C, point(X2, Y1), _, _),
    cross(B, D, point(X2, Y2), _, _),
    P is (abs(Y1 - Y2) + abs(X1 - X2)) * 2,
    S is abs(Y1 - Y2) * abs(X1 - X2),
    A \= B,
    C \= D.