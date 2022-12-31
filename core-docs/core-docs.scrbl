#lang scribble/manual

@title{Core}

@defproc[(list-all-pairs [list-one (Listof T)] [list-two (Listof T)])
    (Listof (List T T))]{
    Takes two lists and returns a pair of each element of the two lists.
}