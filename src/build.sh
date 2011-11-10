#!/bin/sh
OC=ocamlc

INCLUDE="../lib/"
BUILD="graphe.ml buildgraph.ml"

$OC -c ast.mli
$OC -c -I $INCLUDE graphe.ml
$OC graphe.cmo buildgraph.ml  
