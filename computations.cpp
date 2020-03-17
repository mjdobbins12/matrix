#include <iostream>

namespace matrcomp
{
	class Vector
	{
	public:
		int x[];

		Vector(int elements[]) {
			x = elements;
		}
	};
}

int main()
{
	matrcomp::Vector v({ 1, 2, 3});
	std::cout << v << std::endl;
}