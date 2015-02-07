#include <Wire.h>
#include <LiquidCrystal.h>

#include "LCDKeyPadShield.h"
#include "UserInterface.h"
#include "MotorShield.h"

static const uint8_t ledPin = 13;

static Stepper *gM1 = 0;
static HBridge *gHB3 = 0;

static LCDKeyPad gLCDKP;
static UserInterface gUI;

void setup()
{
	Serial.begin(9600);

	MotorShieldV2 *motorShield = new MotorShieldV2();

	gM1 = new Stepper(motorShield, Stepper1);
	gHB3 = new HBridge(motorShield, HBridge3);

	gUI.setup(gLCDKP);
}
 
void loop()
{
	static bool ledState = false;			// current led state
	static unsigned long ledNext = 0;		// next event for led
	static unsigned long m1Next = 0;		// next event for motors
	unsigned long now = millis();

	if (ledNext < now)
	{
		digitalWrite(ledPin, ledState);
		ledState = !ledState;
		ledNext = now + 1000;
	}

	switch (gUI.processInput(gLCDKP, now))
	{
	case Light:
		gHB3->setPWM(gUI.intensity());
		gHB3->set(gUI.light() ? Forward : Off);
		break;
	default: ;
	}

	if (m1Next < now)
	{
		gM1->step();
		m1Next = now + 500;
	}
}

