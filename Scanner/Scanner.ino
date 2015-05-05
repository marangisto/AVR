#include <Wire.h>
#include <LiquidCrystal.h>

#include "LCDKeyPadShield.h"
#include "UserInterface.h"
#include "MotorShield.h"

static const uint8_t ledPin = 13;

static Stepper *gM1 = 0;
static HBridge *gHB3 = 0;

enum State { Initial, Abort, Position, Focus, Expose, Pause };

static LCDKeyPad gLCDKP;
static UserInterface gUI;
static State gState = Initial;

void setup()
{
	Serial.begin(9600);
	pinMode(ledPin, OUTPUT);

	MotorShieldV2 *motorShield = new MotorShieldV2();

	gM1 = new Stepper(motorShield, Stepper1);
	gHB3 = new HBridge(motorShield, HBridge3);

	gUI.setup(gLCDKP);
	gM1->setPWM(gUI.intensity());
}
 
void loop()
{
	static bool ledState = false;			// current led state
	static unsigned long ledNext = 0;		// next event for led
	static unsigned long m1Next = 0;		// next event for motors
	static unsigned long exposeEnd = 0;		// end of exposure
	static unsigned long pauseEnd = 0;		// end of pause
	static long pos = 0;					// stepper position
	static long inc = 1;					// next position increment

	Command cmd = gUI.processInput(gLCDKP);

	unsigned long now = millis();
	State lastState = gState;

	if (ledNext < now)
	{
		digitalWrite(ledPin, ledState);
		ledState = !ledState;
		ledNext = now + 250;
	}

	switch (cmd)
	{
	case LightSwitch:
		switch (gState)
		{
		case Initial:
			gHB3->setPWM(gUI.intensity());
			gHB3->set(Forward);
			gState = Focus;
			break;
		case Focus:
			gHB3->set(Off);
			gState = Initial;
			break;
		default: ;
		}
		break;
	case IntensityChange:
		if (gState == Focus)
			gHB3->setPWM(gUI.intensity());
		gM1->setPWM(gUI.intensity());		// FIXME: motor power control
		break;
	case Start:
		if (gState == Initial)
		{
			gHB3->setPWM(gUI.intensity());
			gHB3->set(Forward);
			exposeEnd = now + gUI.shutter();
			gState = Expose;
		}
		else
			gState = Abort;
		break;
	default: ;
	}

	switch (gState)
	{
	case Abort:
		if (lastState == Focus || lastState == Expose)
			gHB3->set(Off);
		pauseEnd = now + 500;
		gState = Pause;
		break;
	case Expose:
		if (exposeEnd < now)
		{
			gHB3->set(Off);
			gState = Initial;
		}
		break;
	case Pause:
		if (pauseEnd < now)
			gState = Initial;
		break;
	default: ;
	}

	if (m1Next < now)
	{
		if (pos >= 200 || pos < 0)
			inc = -inc;

		gM1->step(inc > 0 ? Fwd : Rev);
		pos += inc;
		m1Next = now + gUI.shutter();	// FIXME: speed control
	}

	if (gState != lastState)
		Serial.print(gState);
}

