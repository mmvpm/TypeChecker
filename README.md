# TypeChecker

[![Build Status](https://github.com/IdeaSeeker/TypeChecker/workflows/CI/badge.svg)](https://github.com/IdeaSeeker/TypeChecker/actions)

Type checker for polymorphic lambda calculus.

## Usage

- run [main](https://github.com/IdeaSeeker/TypeChecker/blob/main/src/main/scala/Main.scala#L4) and enter the lambda term
- input format: `\<variable> : <type> . <term>`

## Example

- input: `\f : alpha -> alpha . \x: alpha . f (f x)`
- output: `Type: (alpha -> alpha) -> alpha -> alpha`
