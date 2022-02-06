# TypeChecker

[![Build Status](https://github.com/IdeaSeeker/TypeChecker/workflows/CI/badge.svg)](https://github.com/IdeaSeeker/TypeChecker/actions)

Type checker for simply typed lambda calculus.

## Usage

- Open the project with Intellij IDEA and run [main](https://github.com/IdeaSeeker/TypeChecker/blob/simply-typed/src/main/scala/Main.scala#L3) and enter the lambda term
- Type syntax:
  - Arrow type: `<type> => <type>`
- Term syntax:
  - Abstraction: `\<variable>: <type> -> <term>`
  - Application: `<term> <term>`

## Example

- Input: `\f: alpha => alpha -> \a: alpha -> f (f a)`
- Output: `Type: (alpha => alpha) => alpha => alpha`
