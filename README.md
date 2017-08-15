# Functional Programming Exercises

These are the solutions to the programming exercises assigned to me during the course "Functional Programming Applied to Scala", which I took during the month of July, 2017 at Uniandes. I make them available for reference to anyone interested in the subject, with no warranties (see the license file).

## Scheme

- `higher-order.rkt` is a simple exercise demonstrating the use of higher-order procedures.
- `derivatives.rkt` shows how to extend a symbolic differentiation program to also support deriving sines and cosines, by making use of the chain rule.
- `scheme-interpreter.rkt` extends the classic Scheme interpreter shown in SICP to support two additional special forms: `philo` for writing anonymous recursive procedures via a Y combinator, and `dlambda` for implementing dynamically-scoped `lambda` forms.

## Haskell

- `scheme-interpreter.hs` extends a Scheme interpreter written in Haskell to support the `dlambda` special form, for implementing dynamically-scoped `lambda` forms.

## Scala

- `WikiCrawler.scala` is a mini web crawler that reads pages from a list of Wikipedia URLs, it requires that the library "Reactive Extensions for Scala" is installed and correctly configured.
- `FunctionalDatabase.scala` implements an in-memory database stored using immutable data structures, with support for basic CRUD operations: create table, insert, select, update, delete; with SQL-like syntax and `where` conditions. I'm particularly proud of the way I solved this exercise, see the examples at the end of the file to get an idea of its power.
- `transmi-metro` the final assignment for the course, we wrote this one together with @jagrales22. It's a reactive application using the Play framework that simulates a public transportation system with cars moving passengers across stations; the data is obtained from .csv files and processed via reactive streams. At the end of the day, reports are generated detailing the system's behavior during the simulation. The GUI was going to use web sockets to display the information in real-time, but we didn't get to implement it.
