#include <Wire.h>
//#include "TrellisQuad.h"
// #include "AccelADXL335.h"

static const uint8_t potGnd = 7;
static const uint8_t potVcc = 5;
static const uint8_t potOne = A6;
static const uint8_t potTwo = A7;
static const uint8_t led = 13;

void setup()
{
	Serial.begin(9600);
//	TrellisQuad::begin();
//	AccelADXL335::begin();

	pinMode(potGnd, OUTPUT);
	digitalWrite(potGnd, LOW);
	pinMode(potVcc, OUTPUT);
	digitalWrite(potVcc, HIGH);

	pinMode(led, OUTPUT);
}

void loop()
{
	static int i = 0;
	static bool on = false;

/*
	AccelADXL335 accel;

	Serial.print(accel.get_x());
	Serial.print(" ");
	Serial.print(accel.get_y());
	Serial.print(" ");
	Serial.print(accel.get_z());
	Serial.println();
*/

   	i = map(analogRead(potTwo), 0, 1023, 0, 99);

	Serial.println(i);
//	TrellisQuad::drawNum(1, 0, i);
//	TrellisQuad::drawNum(1, 0, abs(accel.get_y()));

	if (++i > 99)
		i = 0;

//	TrellisQuad::writeDisplay();

	digitalWrite(led, on);
	on = !on;

	delay(i);
}

