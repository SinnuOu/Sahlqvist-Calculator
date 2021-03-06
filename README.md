This program, written mainly in Haskell by WANG, Xinyu, converts a Sahlqvist formula to its local corresponding first order formula, with a single free variable named "x0" by default. However, the computation result may not be in the simplest form.
For what a Sahlqvist formula is, c.f. Blackburn, P., de Rijke, M., Venema, Y.: Modal Logic. Cambridge University Press, New York (2001).

Input syntax:
A ::= Top | Bottom | nameP | (And A A) | (Or A A) | (Not A) | (To A A) | ([nameB arityB] A A .. A) | (<nameD arityD> A A .. A)
where nameP, nameB and nameD are names for propositional letters, boxes and diamonds, respectively. Be sure to use the same name for a dual pair of box and diamond. arityB and arityD are non-negative arities for boxes and diamonds, followed by exactly the same amount of As.

Output syntax:
V ::= nameV
A ::= Top | Bottom | {nameP arityP} V V .. V | Equal V V | And (A) (A) | Or (A) (A) | Not (A) | To (A) (A) | Forall V (A) | Exists V (A)
where nameV and nameP are names for variables and predicates, respectively. Predicates will be displayed the same name as input boxes and diamonds. arityP (>= 1) are arities for predicates, obtained by adding one to the arity of corresponding box or diamond, and followed by exactly the same amount of Vs.

Input sample: (To (<K 1> ([K 1] p)) ([K 1] p))
Output sample: Forall x1 (To ({K 2} x0 x1) (Forall x3 (To ({K 2} x0 x3) ({K 2} x1 x3))))

This program has been upgraded, such that the restriction on input syntax becomes less strict:
1. Since prefix expression is being used, parentheses can actually be omitted, but except for box or diamond expression.
2. As a compensate, box or diamond expression can now omit its arity, however it remains your own duty to ensure that modalities with the same name are indeed one identical modality and thus they should all conform to an equal arity. This choice leads to faster parsing algorithm.
3. Moreover, the number of blank spaces between any two words now need not be rigid, either.
Hence, the above input sample may also be entered simply as: To (<K> ([K] p)) ([K] p)

For source code of the latest version, see https://github.com/SinnuOu/Sahlqvist-Calculator. For further information and bug reports, email s2010404@jaist.ac.jp.