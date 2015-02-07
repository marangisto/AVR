#include <Wire.h>
#include <LiquidCrystal.h>

#include "LCDKeyPadShield.h"
#include "MotorShield.h"

static const uint8_t ledPin = 13;

static Stepper *gM1 = 0;
static Stepper *gM2 = 0;
static HBridge *gHB1 = 0;
static HBridge *gHB2 = 0;
static HBridge *gHB3 = 0;
static HBridge *gHB4 = 0;

LCDKeyPad gLCDKP;

void setup()
{
	Serial.begin(9600);

	MotorShieldV2 *motorShield = new MotorShieldV2();

	gM1 = new Stepper(motorShield, Stepper1);
	gM2 = new Stepper(motorShield, Stepper2);
	gHB1 = new HBridge(motorShield, HBridge1);
	gHB2 = new HBridge(motorShield, HBridge2);
	gHB3 = new HBridge(motorShield, HBridge3);
	gHB4 = new HBridge(motorShield, HBridge4);

	LiquidCrystal& lcd = gLCDKP.lcd();

	delay(1000);
	lcd.setCursor(0,0);
	lcd.print("Push the buttons");
}
 
void loop()
{
	static bool ledState = false;					// current led state
	static unsigned long ledNext = 0;				// next event for led
	static unsigned long m1Next = 0, m2Next = 0;	// next event for motors
	unsigned long now = millis();

	if (ledNext < now)
	{
		digitalWrite(ledPin, ledState);
		ledState = !ledState;
		ledNext = now + 1000;
	}

	LiquidCrystal& lcd = gLCDKP.lcd();

	lcd.setCursor(9,1);             // move cursor to second line "1" and 9 spaces over
	lcd.print(millis()/1000);       // display seconds elapsed since power-up
	lcd.setCursor(0,1);             // move to the begining of the second line

	switch (gLCDKP.read())
	{
		case KeyLeft:	lcd.print("Left  "); break;
		case KeyRight:	lcd.print("Right "); break;
		case KeyUp:		lcd.print("Up    "); break;
		case KeyDown:	lcd.print("Down  "); break;
		case KeySelect:	lcd.print("Select"); break;
		case KeyNone:	lcd.print("      "); break;
	}

	if (m1Next < now)
	{
		gM1->step();
		m1Next = now + 500;
	}

	if (m2Next < now)
	{
		gM2->step();
		m2Next = now + 50;
	}
}

