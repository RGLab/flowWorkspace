/*
 * cpConstructorTest.cpp
 *
 *  Created on: Jan 6, 2014
 *      Author: wjiang2
 */
#include "test_header.hpp"
class NP{
	public:
		int a;
		boost::scoped_ptr<int> b;
		NP(){
			a=0;
		}
		NP(const NP &np){
			a = np.a;
			if(np.b.get()!=NULL)
				b.reset(new int(*(np.b)));
		}
		NP & operator=(NP np){
			std::swap(a, np.a);
			if(np.b.get()!=NULL)
				b.reset(new int(*(np.b)));
			return *this;
		}
	};
	typedef boost::adjacency_list<  // adjacency_list is a template depending on :
	    boost::vecS,               //  The container used for egdes : here, std::list.
	    boost::vecS,                //  The container used for vertices: here, std::vector.
	    boost::bidirectionalS,           //  directed or undirected edges ?.
	    NP,                     //  The type that describes a Vertex.
	    boost::no_property                        //  The type that describes an Edge
	> myG;
	typedef boost::adjacency_list<  // adjacency_list is a template depending on :
		    boost::vecS,               //  The container used for egdes : here, std::list.
		    boost::vecS,                //  The container used for vertices: here, std::vector.
		    boost::bidirectionalS,           //  directed or undirected edges ?.
		    NP *,                     //  The type that describes a Vertex.
		    boost::no_property                        //  The type that describes an Edge
		> myG_old;
	typedef myG::vertex_descriptor VertexID;
 void cpConsTest(){
		myG g, g1;
		myG_old g2;
		for(int i =0;i <3;i++)
			boost::add_vertex(g);

		//test copy constructor
		g[0].b.reset(new int(1));
//		cout<< g[0].b.get() << endl;
//		NP &np = g[0];
//		cout<< np.b.get() << endl;
//		*(np.b)=2;
//		cout<< np.b.get() << endl;
//		np.a = -1;
//
//		cout << "a = " << g[0].a << " b = " << *(g[0].b) << endl;

		//test copy graph
//		boost::copy_graph(g, g1);
		g[0].a = 100;
		cout << "a = " << g[0].a << " b = " << *(g[0].b) << endl;
		cout << "a = " << g1[0].a << " b = " << *(g1[0].b) << endl;

		boost::add_vertex(g2);
		g2[0]->a = 50;
		*(g2[0]->b) = 20;
		myG g3(num_vertices(g2));
		cout << "a = " << g2[0]->a << " b = " << *(g2[0]->b) << endl;
//		cout << "a = " << g3[0].a << " b = " << *(g3[0].b) << endl;

		std::exit(-1);
	}


