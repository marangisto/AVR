#ifndef MOTION_H
#define MOTION_H

#include <Wire.h>
#include "MotorShield.h"

class Motion
{
public:
	Motion(Stepper *m);

	void Calibrate();
	void MoveTo(int16_t pos);

private:
	Stepper *m_m;
	int16_t	m_pos;
};

#endif // MOTION_H

