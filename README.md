# Truth Tables.

Unix truth table generator for propositional logic formulas developed with Haskell.

This program employs syntax analysis algorithms for the propositional logic language, balanced parentheses algorithms for syntax, and propositional logic interpretations for semantics. It is developed using a custom data structure implementation.

Made exclusively for Unix systems.

## Prerequisites.
**_The program makes use of the STACK v2.7.5 or higher packager._**

```sh
   https://docs.haskellstack.org/en/v2.7.5/README/
```

1. Clone the repository.
```sh
   git clone https://github.com/richardfm77/truthtables.git
```
```sh
   cd truthtables
```

2. Config proyect.
```sh
   stack setup
```

3. Compile.
```sh
   stack build
```

4. Run tests.
```sh
   stack test
```

## Start program.

```sh
   stack exec truthtables-exe
```

**Enter the propositional logic formula you want to know its truth table.**


**_It is important that the input has the following format._**

* Atomic formulas of propositional logic must be alphabetic characters, they can be lowercase or uppercase, except for the *'v'* character. 
  For example: **_p,q,r,s,t,..._**

* The propositional logic operators have the following syntax:

  * Not -->  Not (exp) **where *exp* is a formula of propositional logic.**

  * Or -->  Or (exp1) (exp2) **where *exp1* and *exp2* are formulas of propositional logic.**
  
  * And -->  And (exp1) (exp2) **where *exp1* and *exp2* are formulas of propositional logic.**

  * Implies -->  Implies (exp1) (exp2) **where *exp1* and *exp2* are formulas of propositional logic.**

  * Equal -->  Equal (exp1) (exp2) **where *exp1* and *exp2* are formulas of propositional logic.**
