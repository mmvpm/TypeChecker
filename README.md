# TypeChecker

[![Build Status](https://github.com/IdeaSeeker/TypeChecker/workflows/CI/badge.svg)](https://github.com/IdeaSeeker/TypeChecker/actions)

Type checker for polymorphic lambda calculus.

## Usage

- Open the project with Intellij IDEA
- Run [main](https://github.com/IdeaSeeker/TypeChecker/blob/main/src/main/scala/Main.scala#L3) and enter the lambda term
- Type syntax:
  - Arrow type: `<type> => <type>`
  - Universal type: `forall <type-variable> . <type>`
- Term syntax:
  - Abstraction: `\<variable>: <type> -> <term>`
  - Application: `<term> <term>`
  - Universal abstraction: `forall <type-variable> -> <term>`
  - Universal application: `<term> ~ <type>`

## Example

- Input: `forall alpha -> \f: forall beta . beta => alpha -> \x: Bool -> f ~ Bool x`
- Output: `Type: ∀alpha . (∀beta . beta => alpha) => Bool => alpha`
