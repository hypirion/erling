%%%-----------------------------------------------------------------------------
%%% File    : erling_string.erl
%%% Author  : Jean Niklas L'orange <jeannikl@hypirion.com>
%%% Description : Erling's plugin supervisor.
%%%
%%% Created :  4 Aug 2013 by Jean Niklas L'orange <jeannikl@hypirion.com>
%%%-----------------------------------------------------------------------------
-module(erling_string).

%%------------------------------------------------------------------------------
%% External exports
-export([char_to_lower/1, char_to_upper/1]).

%%==============================================================================
%% External functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: char_to_lower/1
%% Description: Converts a character to its lower case equivalent, as defined by
%%   rfc2812.
%% Returns: LowerCase
%%------------------------------------------------------------------------------
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower($[) -> ${;
char_to_lower($]) -> $};
char_to_lower($|) -> $\\;
char_to_lower($^) -> $~;
char_to_lower(Ch) -> Ch.

%%------------------------------------------------------------------------------
%% Function: char_to_upper/1
%% Description: Converts a character to its upper case equivalent, as defined by
%%   rfc2812.
%% Returns: UpperCase
%%------------------------------------------------------------------------------
char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(${) -> $[;
char_to_upper($}) -> $];
char_to_upper($\\) -> $|;
char_to_upper($~) -> $^;
char_to_upper(Ch) -> Ch.
