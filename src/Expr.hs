module Expr where


data Expr =
  Var Name | -- x
  App Expr Expr | -- f x
  Closure Name [(Name,Expr)] Int Expr | -- \x -> y
  Num Double | -- 3
  Ctor Name [Expr] | -- K[x,y]
  Case Expr [(Name,[Name],Expr)]




