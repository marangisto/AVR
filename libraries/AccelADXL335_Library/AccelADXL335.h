#ifndef ACCELADXL335_H
#define ACCELADXL335_H

#include <WProgram.h>

class AccelADXL335
{
public:
	AccelADXL335(int n = 10);	// takes readings (avg of n)

	int get_x() const { return m_x; }
	int get_y() const { return m_y; }
	int get_z() const { return m_z; }

	static void begin();

private:
	int	m_x, m_y, m_z;
};

#endif

