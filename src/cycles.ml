open Graphe


let processRegs igraph =
  let process key value (graph,inlist,outlist) =
    let sucks k (graph,inlist,outlist) =
      let v = Graphe.find graph k in
      if (Vertex.getLabel v) =  Noeud.Reg then
	let graph = Graphe.setLabel graph k Noeud.Inreg in
	let graph = Graphe.removeEdge graph key k in
	(graph,(key::outlist),(k::inlist))
      else
	(graph,outlist,inlist)
    in
    Vertex.foldSucc sucks value (graph,inlist,outlist)    
  in
  let g,il,ol = Graphe.foldVertex process igraph.igraph (igraph.igraph,[],[]) in
  { cgraph = g; cinputs = igraph.iinputs; coutputs = igraph.ioutputs; cinregs = il; coutregs = ol}
