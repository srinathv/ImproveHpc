//from https://www.archer.ac.uk/training/course-material/2014/05/AdvancedOpenMP_Oxford/Slides/L10-AlternativesToOpenMP.pdf
//
//


#include <boost/thread/thread.hpp>
#include <iostream>

void hello()
{
 std::cout << "Hello world" << std::endl;
}

int main(int argc, char* argv[])
{
 boost::thread thrd(&hello);
 thrd.join();
 return 0;
}
