#include <Wire.h>
#include "Adafruit_Trellis.h"
#include "TrellisQuad.h"
#include "AccelADXL335.h"

void setup()
{
	Serial.begin(9600);
	TrellisQuad::begin();
	AccelADXL335::begin();
}

void loop()
{
	static int i = 0;

	AccelADXL335 accel;

	Serial.print(accel.get_x());
	Serial.print(" ");
	Serial.print(accel.get_y());
	Serial.print(" ");
	Serial.print(accel.get_z());
	Serial.println();

//	TrellisQuad::drawNum(1, 0, i);
	TrellisQuad::drawNum(1, 0, abs(accel.get_y()));

	if (++i > 99)
		i = 0;

	TrellisQuad::writeDisplay();

	delay(300);
}

