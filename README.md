# TypeChecker

[![Build Status](https://github.com/IdeaSeeker/TypeChecker/workflows/CI/badge.svg)](https://github.com/IdeaSeeker/TypeChecker/actions)

Simple type checker for lambda calculus

## Usage

- run [main](https://github.com/IdeaSeeker/TypeChecker/blob/a321d7a902658575df877790a4e4ef78849337f9/src/main/scala/Main.scala#L3) and enter the lambda term
- input format: `\<variable> : <type> . <term>`

## Example

- input: `\f : alpha -> alpha . \x: alpha . f (f x)`
- output: `Type: (alpha -> alpha) -> alpha -> alpha`
