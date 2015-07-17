#include <Wire.h>
#include <LiquidCrystal.h>

#include "LCDKeyPadShield.h"
#include "UserInterface.h"
#include "MotorShield.h"
#include "Motion.h"

static const uint8_t ledPin = 13;

static Motion *gM = 0;
// static Stepper *gM1 = 0;
static HBridge *gHB3 = 0;

enum State { Initial, Abort, Position, Focus, Expose, Pause };

static LCDKeyPad gLCDKP;
static UserInterface gUI;
static State gState = Initial;

void setup()
{
    Serial.begin(9600);
    while(!Serial); // Wait for serial port to connect - used on Leonardo, Teensy and other boards with built-in USB CDC serial connection
    Serial.println("Start");
	pinMode(ledPin, OUTPUT);

	MotorShieldV2 *motorShield = new MotorShieldV2();

	Stepper *m1 = new Stepper(motorShield, Stepper1);
	gM = new Motion(m1);
	gHB3 = new HBridge(motorShield, HBridge3);

	gUI.setup(gLCDKP);
}
 
void loop()
{
	static bool ledState = false;			// current led state
	static unsigned long ledNext = 0;		// next event for led
	static unsigned long m1Next = 0;		// next event for motors
	static unsigned long exposeEnd = 0;		// end of exposure
	static unsigned long pauseEnd = 0;		// end of pause
	static int16_t pos = 0;					// stepper position

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
		break;
	case Start:
		if (gState == Initial)
			gState = Expose;
/*
		else
			gState = Abort;
*/
		break;
	default: ;
	}

	switch (gState)
	{
	case Abort:
		if (lastState == Focus || lastState == Expose)
			gHB3->set(Off);
		pauseEnd = now + 100;
		gState = Pause;
		break;
	case Expose:
		gState = Initial;
		pos = pos ? 0 : 50;
		Serial.println("calling moveto");
		gM->MoveTo(pos);	// blocking!
		Serial.println("returned");
		break;
	case Pause:
		if (pauseEnd < now)
			gState = Initial;
		break;
	default: ;
	}

//	if (gState != lastState)
//		Serial.print(gState);

	delay(1);
}

