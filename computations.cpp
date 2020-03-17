#include <iostream>
#include <vector>

namespace matrcomp
{
	class Vector
	{
	public:
		std::vector<int> x;

		Vector(int elements[]) {
			x = elements;
		}
	};
}

int main()
{
	matrcomp::Vector v({ 1, 2, 3});
	std::cout << v[0] << std::endl;
}