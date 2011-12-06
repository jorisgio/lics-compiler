open Graphe


let processRegs igraph = 
  let size = ref (Graphe.size igraph.igraph) in
  let process key value (graph,inlist,outlist) =
    if (Vertex.getLabel value) = Noeud.Reg then 
      begin
	size := !size + 1;
	let graph = Graphe.addVertex graph !size in
	let value = Vertex.setLabel value Noeud.Inreg in
	let graph = Graphe.setVertex graph !size value in
	let value = Vertex.setLabel Vertex.empty Noeud.Outreg in
	((Graphe.setVertex graph key value),(!size::inlist),(key::outlist))
      end
    else 
      (graph,inlist,outlist)
  in
  let g,il,ol = Graphe.foldVertex process igraph.igraph (igraph.igraph,[],[]) in
  { cgraph = g; cinputs = igraph.iinputs; coutputs = igraph.ioutputs; cinregs = il; coutregs = ol}
