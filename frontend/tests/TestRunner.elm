module Main where

import Signal exposing (Signal)

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)
import Task

import Tests

console : IO ()
console = consoleRunner Tests.tests

port runner : Signal (Task.Task x ())
port runner = run console
