# MicroFP - A Functional Programming Project

## Overview
MicroFP is a small functional programming language interpreter implemented in Haskell. This project demonstrates various functional programming concepts including parser combinators, expression evaluation, pattern matching, and property-based testing.

## Features
- Custom syntax parser using combinators
- Expression evaluation with support for:
  - Arithmetic operations (addition, subtraction, multiplication)
  - Function calls with pattern matching
  - Conditional expressions
  - Variable binding
- Pattern matching for function definitions
- Property-based testing using QuickCheck

## Core Components
- **MicroFP.hs**: Main implementation file containing the language definition, parser, evaluator, and testing
- **PComb.hs**: Parser combinator library implementation
- **BasicParsers.hs**: Basic parsing utilities

## Language Examples
The language supports recursive function definitions like Fibonacci:
