------------------------
Compiler Construction - Lab B
------------------------
- Bas du Pré (......)
- Tom Tervoort (3470784)


------------------------
Type Checker
------------------------
The main module of the type checker is located in TypeChecker.hs. It defines the type checker 
Component by chaining the CCO ATerm parser, the provided ATerm-to-Diag converter, the actual type 
checker, and then outputting the original input. These components are combined with the arrow (>>>) 
operator.

The type checker runs within the Feedback monad. If the give diagram is not well-typed, it will 
raise a fatal error (causing the final step of outputting the input to not be executed) indicating
what is incorrect.

The type check operation is defined as an attribute grammar within TypeChecker/AG.ag. The 
synthesised attribute 'feedback' contains a computation in the feedback monad with the monadic 
sequence (>>) operator as a use-clause combinator. This combines type checking operations in some 
order, which does not really matter since all of these checks will either fail or do nothing.

The other synthesised attributes of the Diag types describe certain properties of the direct 
children in the tree. Unless explicitly specified among the semantics, these do not affect nodes
higher than their parents. This is achieved by, in the use clause, defining a default value of 
Nothing and a default combinator that always results in Nothing.

Besides 'feedback', the following synthesised properties are maintained: 
 - isPlatform: whether the diagram represents a platform declaration.
 - languageOrPlatform: if set, this holds the language a program is written in, or the platform on
   which it can be runned.
 - runs: interpreters set this to the language that can be interpreted.
 - compiles: compilers set this to the language that can be compiled.
 - targetLanguage: the target language of a compiler, if any.
 - stringRep: a string representation of a diagram that can be included in an error message.

With these properties, it is possible to check, within the semantics of Diag and Diag_, whether a 
certain expression is valid. If not, 'feedback' is set to give a fatal error message.


