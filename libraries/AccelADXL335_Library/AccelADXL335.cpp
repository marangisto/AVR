#include <Wire.h>
#include "AccelADXL335.h"

static const int gnd = A0;
static const int xInput = A1;
static const int yInput = A2;
static const int zInput = A3;
static const int vcc = A4;

static int normalize(int x, int x0, int x1)
{
	return constrain(map(x, x0, x1, -99, 99), -99, 99);
}

AccelADXL335::AccelADXL335(int n)
{
	long lx = 0, ly = 0, lz = 0;

	for (int i = 0; i < n; ++i)
	{
    	lx += analogRead(xInput);
    	ly += analogRead(yInput);
    	lz += analogRead(zInput);
		delay(1);
	}

	m_x = normalize(lx / n, 275, 423);
	m_y = normalize(ly / n, 266, 424);
	m_z = normalize(lz / n, 429, 269);
}

void AccelADXL335::begin()
{
  pinMode(gnd, OUTPUT);
  digitalWrite(vcc, LOW);
  pinMode(vcc, OUTPUT);
  digitalWrite(vcc, HIGH);
}

