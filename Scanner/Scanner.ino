#include <Wire.h>

#include "MotorShield.h"

static Stepper *gM0 = 0;
static Stepper *gM1 = 0;
static HBridge *gHB1 = 0;
static HBridge *gHB2 = 0;
static HBridge *gHB3 = 0;
static HBridge *gHB4 = 0;

void setup()
{
	Serial.begin(9600);

	MotorShieldV2 *motorShield = new MotorShieldV2();

	gM0 = new Stepper(motorShield, Stepper1);
	gM1 = new Stepper(motorShield, Stepper2);
	gHB1 = new HBridge(motorShield, HBridge1);
	gHB2 = new HBridge(motorShield, HBridge2);
	gHB3 = new HBridge(motorShield, HBridge3);
	gHB4 = new HBridge(motorShield, HBridge4);
}

void runPWM(HBridge *hb, HBridgeState s)
{
	hb->setPWM(0);
	hb->set(s);

	for (uint16_t w = 1; w <= 4096; w *= 2)
	{
		hb->setPWM(w);
		delay(50);
	}
}

void runHB(HBridge *hb)
{
	runPWM(hb, Forward);
	runPWM(hb, Reverse);
	hb->set(Off);
	delay(500);
}

void runHB(HBridge *hb, int n)
{
	for (uint8_t i = 0; i < n; ++i)
		runHB(hb);
}

void loop()
{
//	gM0->step();
//	gM1->step();

	runHB(gHB1, 4);
	runHB(gHB2, 4);
	runHB(gHB3, 4);
	runHB(gHB4, 4);
}

