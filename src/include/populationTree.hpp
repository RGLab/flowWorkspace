/*
 * tree.hpp
 *
 *  Created on: Mar 17, 2012
 *      Author: mike
 */

#ifndef TREE_HPP_
#define TREE_HPP_

//#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
//#include <boost/graph/visitors.hpp>
#include "nodeProperties.hpp"

struct Edge{
    // nothing, probably. Or a weight, a distance, a direction, ...
};

typedef boost::adjacency_list<  // adjacency_list is a template depending on :
    boost::vecS,               //  The container used for egdes : here, std::list.
    boost::vecS,                //  The container used for vertices: here, std::vector.
    boost::bidirectionalS,           //  directed or undirected edges ?.
    nodeProperties*,                     //  The type that describes a Vertex.
    Edge                        //  The type that describes an Edge
> populationTree;
typedef populationTree::vertex_descriptor VertexID;
typedef populationTree::vertex_iterator VertexIt;
typedef populationTree::edge_descriptor   EdgeID;
typedef populationTree::edge_iterator   EdgeIt;

#endif /* TREE_HPP_ */
