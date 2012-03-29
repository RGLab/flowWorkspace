/*
 * workspace.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WORKSPACE_HPP_
#define WORKSPACE_HPP_
#include <vector>
#include <string>
#include <libxml/xpath.h>
#include "wsNode.hpp"
using namespace std;

/*TODO: so far I will see the differenc between wind and max workspace in terms of xpath(like xpath of sample node)
 * if this is the case eventually we can try to use one template class (eliminate two derived classes )
 * with T structure that stores different versions of xpaths for win/mac,for example:
 *
 * struct winWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * struct macWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * this may potentially reduce the amount of code
 *
 */
struct xpath{
	string group;
	string sampleRef;
	string sample;
	string sampleNode;
	string popNode;
};

class workspace{
public:
	 xpath nodePath;
//protected:

	 xmlDoc * doc;
public:
	 ~workspace();
	 virtual string xPathSample(string sampleID)=0;
	 virtual void getCompensation()=0;
	 virtual void getTransformation()=0;
	 virtual vector <string> getSampleID(unsigned short)=0;
//	 virtual wsSampleNode getSample(string sampleID)=0;
	 virtual string getSampleName(wsSampleNode &)=0;
	 virtual wsRootNode getRoot(wsSampleNode sampleNode)=0;
	 virtual wsPopNodeSet getSubPop(wsNode *)=0;
	 virtual populationNode to_popNode(wsRootNode const *)=0;
	 virtual populationNode to_popNode(wsPopNode const *)=0;
};


#endif /* WORKSPACE_HPP_ */

