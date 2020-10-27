This program, written in Haskell by WANG, Xinyu, converts a Sahlqvist formula to its local corresponding first order formula, with a single free variable named "x0" by default. However, the computation result may not be in the simplest form.
For what a Sahlqvist formula is, c.f. Blackburn, P., de Rijke, M., Venema, Y.: Modal Logic. Cambridge University Press, New York (2001).

Input syntax:
A ::= Top | Bottom | nameP | (And A A) | (Or A A) | (Not A) | (To A A) | ([nameB arityB] A A .. A) | (<nameD arityD> A A .. A)
where nameP, nameB and nameD are names for propositional letters, boxes and diamonds, respectively. Be sure to use the same name for a dual pair of box and diamond. arityB and arityD are non-negative arities for boxes and diamonds, followed by exactly the same amount of As.

Output syntax:
V ::= nameV
A ::= Top | Bottom | {nameP arityP} V V .. V | Equal V V | And (A) (A) | Or (A) (A) | Not (A) | To (A) (A) | Forall V (A) | Exists V (A)
where nameV and nameP are names for variables and predicates, respectively. Predicates will be displayed the same name as input boxes and diamonds. arityP (>= 1) are arities for predicates, obtained by adding one to the arity of corresponding box or diamond, and followed by exactly the same amount of Vs.

For the present version, please follow the above grammar strictly, and do not mistake any blanks or parentheses.
Input sample: (To (<K 1> ([K 1] p)) ([K 1] p))
Output sample: Forall x1 (To ({K 2} x0 x1) (Forall x3 (To ({K 2} x0 x3) ({K 2} x1 x3))))
For source code of the latest version, see https://github.com/SinnuOu/Sahlqvist-Calculator. For further information and bug reports, email s2010404@jaist.ac.jp.