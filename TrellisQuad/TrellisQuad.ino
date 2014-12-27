#include <Wire.h>
#include "Adafruit_Trellis.h"
#include "TrellisQuad.h"

void setup()
{
	Serial.begin(9600);
	TrellisQuad::begin();
}

void loop()
{
	static int i = 0;

	TrellisQuad::drawNum(1, 0, i);

	if (++i > 99)
		i = 0;

	TrellisQuad::writeDisplay();

	delay(300);
}

