/*
 * tree.hpp
 *
 *  Created on: Mar 17, 2012
 *      Author: mike
 */

#ifndef TREE_HPP_
#define TREE_HPP_

#include "nodeProperties.hpp"
#include <boost/graph/adjacency_list.hpp>


#define ROOTNODE 0

struct Edge{
        template<class Archive>
                                void serialize(Archive &ar, const unsigned int version)
                                {


                                }
    // nothing, probably. Or a weight, a distance, a direction, ...
};
typedef boost::adjacency_list<  // adjacency_list is a template depending on :
    boost::vecS,               //  The container used for egdes : here, std::list.
    boost::vecS,                //  The container used for vertices: here, std::vector.
    boost::bidirectionalS,           //  directed or undirected edges ?.
    nodeProperties *,
    Edge
> populationTreeOld;

/*since we don't use pointer here
 * and has customized copy and assignment constructor defined for nodeProperties class
 * the entire graph adjacency_list is copiable now
 * thus eliminate the need for customized clone member functions
 */
typedef boost::adjacency_list<  // adjacency_list is a template depending on :
    boost::vecS,               //  The container used for egdes : here, std::list.
    boost::vecS,                //  The container used for vertices: here, std::vector.
    boost::bidirectionalS,           //  directed or undirected edges ?.
    nodeProperties,
    boost::no_property
> populationTree;
typedef populationTree::vertex_descriptor VertexID;
typedef populationTree::vertex_iterator VertexIt;
typedef populationTree::edge_descriptor   EdgeID;
typedef populationTree::edge_iterator   EdgeIt;

#endif /* TREE_HPP_ */
