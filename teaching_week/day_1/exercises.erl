-module(exercises).
-export([f2c/1, c2f/1, convert/2]).

% Divide by 5, then multiply by 9, then add 32
c2f(C) ->  (C * 9/5) + 32.


% Divide by 5, then multiply by 9, then add 32
f2c(F) -> (F - 32) * 5/ 9.

convert(f, T) -> f2c(T);
convert(c, T) -> c2f(T).