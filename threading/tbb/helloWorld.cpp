//from https://www.archer.ac.uk/training/course-material/2014/05/AdvancedOpenMP_Oxford/Slides/L10-AlternativesToOpenMP.pdf
//
//



#include <iostream>
#include <tbb/parallel_for.h>
using namespace tbb;
class Hello
{
public:
void operator()(int x) const {
std::cout << "Hello world\n";
}
};
int main()
{
// parallelizing:
// for(int i = 0; i < 2; ++i) { ... }
parallel_for(0, 2, 1, Hello());
return 0;
}
