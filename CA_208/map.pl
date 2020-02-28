*/ FACTS /*

directlyNorth(a,f).
directlyNorth(b,g).
directlyNorth(c,h).
directlyNorth(d,i).
directlyNorth(e,j).
directlyNorth(f,k).
directlyNorth(g,l).
directlyNorth(h,m).
directlyNorth(i,n).
directlyNorth(j,o).
directlyNorth(k,p).
directlyNorth(l,q).
directlyNorth(m,r).
directlyNorth(n,s).
directlyNorth(o,t).

directlyEast(a,b).
directlyEast(b,c).
directlyEast(c,d).
directlyEast(d,e).
directlyEast(f,g).
directlyEast(g,h).
directlyEast(h,i).
directlyEast(i,j).
directlyEast(k,l).
directlyEast(l,m).
directlyEast(m,n).
directlyEast(n,o).
directlyEast(p,q).
directlyEast(q,r).
directlyEast(r,s).
directlyEast(s,t).


*/RULES/*

directlySouth(S,N) :- directlyNorth(N,S).
directlyWest(W, E) :- directlyEast(E, W).

north(A,B) :- directlyNorth(A,B).
north(A,B) :- directlyNorth(A,C), north(C,B).

south(A,B) :- directlySouth(A,B).
south(A,B) :- directlySouth(A,C), south(C,B).

east(A,B) :- directlyEast(A,B).
east(A,B) :- directlyEast(A,C), east(C,B).

west(A,B) :- directlyWest(A,B).
west(A,B) :- directlyWest(A,C), west(C,B).

southwest(A,B) :- south(A,C), west(C,B).

northwest(A,B) :- north(A,C), west(C,B).

southeast(A,B) :- south(A,C), east(C,B).

northeast(A,B) :- north(A,C), east(C,B).

